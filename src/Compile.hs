{-# LANGUAGE OverloadedStrings, NondecreasingIndentation, BangPatterns #-}

module Compile (compile) where

import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.FilePath
import System.Exit
import System.IO

import Data.Time.Clock (addUTCTime)

import GHC
import CmdLineParser ( Warn(..), WarnReason(..) )
import DynFlags
import DriverPhases
import DriverPipeline ( compileFile, oneShot )
import HscTypes
import GhcMonad
import Module
import Outputable
import Packages
import Util

logH :: Handle
logH = stderr

compile :: [String] -> Ghc ()
compile flags = do
        -- Parse flags
        dflags <- getSessionDynFlags
        
        -- liftIO . hPutStrLn logH . showSDoc dflags . ppr . packageDBFlags $ dflags
        (dflags2, fileish_args, _warns) <-
          parseDynamicFlags dflags (map noLoc flags)

        -- Normilize paths
        let
          normalise_hyp fp
            | strt_dot_sl && "-" `isPrefixOf` nfp = cur_dir ++ nfp
            | otherwise                           = nfp
            where
-- # if defined(mingw32_HOST_OS) -- TODO: do we need Win support at this point?
--          strt_dot_sl = "./" `isPrefixOf` fp || ".\\" `isPrefixOf` fp
-- # else
              strt_dot_sl = "./" `isPrefixOf` fp
-- # endif
              cur_dir = '.' : [pathSeparator]
              nfp = normalise fp
          normal_fileish_paths = map (normalise_hyp . unLoc) fileish_args
          (srcs, objs)         = partition_args normal_fileish_paths [] []

        -- Update flags with normalized
          dflags3 = dflags2 { ldInputs = map (FileOption "") objs
                                         ++ ldInputs dflags2 }

        loadedPkgs <- setSessionDynFlags dflags3

        liftIO . hPutStrLn logH . show . map (showSDoc dflags3 . ppr) $ loadedPkgs

        doMake srcs


doMake :: [(String,Maybe Phase)] -> Ghc ()
doMake srcs  = do
    let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget srcs

    hsc_env <- GHC.getSession

    -- if we have no haskell sources from which to do a dependency
    -- analysis, then just do one-shot compilation and/or linking.
    -- This means that "ghc Foo.o Bar.o -o baz" links the program as
    -- we expect.
    if (null hs_srcs)
       then liftIO (oneShot hsc_env StopLn srcs)
       else do

    o_files <- mapM (\x -> liftIO $ compileFile hsc_env StopLn x)
                 non_hs_srcs
    dflags <- GHC.getSessionDynFlags
    let dflags' = dflags { ldInputs = map (FileOption "") o_files
                                      ++ ldInputs dflags }
    _ <- GHC.setSessionDynFlags dflags'

    targets <- mapM (uncurry GHC.guessTarget) hs_srcs
    GHC.setTargets targets
    ok_flag <- GHC.load LoadAllTargets

    when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))
    return ()

partition_args :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args ("-x":suff:args) srcs objs
  | "none" <- suff      = partition_args args srcs objs
  | StopLn <- phase     = partition_args args srcs (slurp ++ objs)
  | otherwise           = partition_args rest (these_srcs ++ srcs) objs
        where phase = startPhase suff
              (slurp,rest) = break (== "-x") args
              these_srcs = zip slurp (repeat (Just phase))
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args ((arg,Nothing):srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)

looks_like_an_input :: String -> Bool
looks_like_an_input m =  isSourceFilename m
                      || looksLikeModuleName m
                      || "-" `isPrefixOf` m
                      || not (hasExtension m)

{-

   # Reimplementaion of a part of setSessionDynFlags to not reload package databases without a need

-}

-- literal copy-paste except hooking up into initPackages
setSessionDynFlags' :: GhcMonad m => DynFlags -> m [InstalledUnitId]
setSessionDynFlags' dflags = do
  dflags' <- checkNewDynFlags dflags
  (dflags'', preload) <- liftIO $ initPackages' dflags'
  modifySession $ \h -> h{ hsc_dflags = dflags''
                         , hsc_IC = (hsc_IC h){ ic_dflags = dflags'' } }
  invalidateModSummaryCache
  return preload

-- nearly literal copy-paste & hool up into readPackageConfigs
initPackages' :: DynFlags -> IO (DynFlags, [InstalledUnitId])
initPackages' dflags0 = do
  dflags <- interpretPackageEnv dflags0
  pkg_db <- readPackageConfigs' dflags
  (pkg_state, preload, insts)
        <- mkPackageState dflags pkg_db []
  return (dflags{ pkgDatabase = Just pkg_db,
                  pkgState = pkg_state,
                  thisUnitIdInsts_ = insts },
          preload)

readPackageConfigs' = undefined

{-
   # Below are literal copy-paste's of stuff not exported from GHC
-}
--mkPackageState= undefined

invalidateModSummaryCache :: GhcMonad m => m ()
invalidateModSummaryCache =
  modifySession $ \h -> h { hsc_mod_graph = mapMG inval (hsc_mod_graph h) }
 where
  inval ms = ms { ms_hs_date = addUTCTime (-1) (ms_hs_date ms) }

checkNewDynFlags :: MonadIO m => DynFlags -> m DynFlags
checkNewDynFlags dflags = do
  -- See Note [DynFlags consistency]
  let (dflags', warnings) = makeDynFlagsConsistent dflags
  liftIO $ handleFlagWarnings dflags (map (Warn CmdLineParser.NoReason) warnings)
  return dflags'


-- ***************************************************************************************
mkPackageState dflags dbs preload0 = do
{-
   Plan.

   There are two main steps for making the package state:

    1. We want to build a single, unified package database based
       on all of the input databases, which upholds the invariant that
       there is only one package per any UnitId and there are no
       dangling dependencies.  We'll do this by merging, and
       then successively filtering out bad dependencies.

       a) Merge all the databases together.
          If an input database defines unit ID that is already in
          the unified database, that package SHADOWS the existing
          package in the current unified database.  Note that
          order is important: packages defined later in the list of
          command line arguments shadow those defined earlier.

       b) Remove all packages with missing dependencies, or
          mutually recursive dependencies.

       b) Remove packages selected by -ignore-package from input database

       c) Remove all packages which depended on packages that are now
          shadowed by an ABI-incompatible package

       d) report (with -v) any packages that were removed by steps 1-3

    2. We want to look at the flags controlling package visibility,
       and build a mapping of what module names are in scope and
       where they live.

       a) on the final, unified database, we apply -trust/-distrust
          flags directly, modifying the database so that the 'trusted'
          field has the correct value.

       b) we use the -package/-hide-package flags to compute a
          visibility map, stating what packages are "exposed" for
          the purposes of computing the module map.
          * if any flag refers to a package which was removed by 1-5, then
            we can give an error message explaining why
          * if -hide-all-packages what not specified, this step also
            hides packages which are superseded by later exposed packages
          * this step is done TWICE if -plugin-package/-hide-all-plugin-packages
            are used

       c) based on the visibility map, we pick wired packages and rewrite
          them to have the expected unitId.

       d) finally, using the visibility map and the package database,
          we build a mapping saying what every in scope module name points to.
-}

  -- This, and the other reverse's that you will see, are due to the face that
  -- packageFlags, pluginPackageFlags, etc. are all specified in *reverse* order
  -- than they are on the command line.
  let other_flags = reverse (packageFlags dflags)
  debugTraceMsg dflags 2 $
      text "package flags" <+> ppr other_flags

  -- Merge databases together, without checking validity
  (pkg_map1, prec_map) <- mergeDatabases dflags dbs

  -- Now that we've merged everything together, prune out unusable
  -- packages.
  let (pkg_map2, unusable, sccs) = validateDatabase dflags pkg_map1

  reportCycles dflags sccs
  reportUnusable dflags unusable

  -- Apply trust flags (these flags apply regardless of whether
  -- or not packages are visible or not)
  pkgs1 <- foldM (applyTrustFlag dflags prec_map unusable)
                 (Map.elems pkg_map2) (reverse (trustFlags dflags))
  let prelim_pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs1

  --
  -- Calculate the initial set of packages, prior to any package flags.
  -- This set contains the latest version of all valid (not unusable) packages,
  -- or is empty if we have -hide-all-packages
  --
  let preferLater pkg pkg' =
        case compareByPreference prec_map pkg pkg' of
            GT -> pkg
            _  -> pkg'
      calcInitial m pkg = addToUDFM_C preferLater m (fsPackageName pkg) pkg
      initial = if gopt Opt_HideAllPackages dflags
                    then emptyUDFM
                    else foldl' calcInitial emptyUDFM pkgs1
      vis_map1 = foldUDFM (\p vm ->
                            -- Note: we NEVER expose indefinite packages by
                            -- default, because it's almost assuredly not
                            -- what you want (no mix-in linking has occurred).
                            if exposed p && unitIdIsDefinite (packageConfigId p)
                               then Map.insert (packageConfigId p)
                                               UnitVisibility {
                                                 uv_expose_all = True,
                                                 uv_renamings = [],
                                                 uv_package_name = First (Just (fsPackageName p)),
                                                 uv_requirements = Map.empty,
                                                 uv_explicit = False
                                               }
                                               vm
                               else vm)
                         Map.empty initial

  --
  -- Compute a visibility map according to the command-line flags (-package,
  -- -hide-package).  This needs to know about the unusable packages, since if a
  -- user tries to enable an unusable package, we should let them know.
  --
  vis_map2 <- foldM (applyPackageFlag dflags prec_map prelim_pkg_db unusable
                        (gopt Opt_HideAllPackages dflags) pkgs1)
                            vis_map1 other_flags

  --
  -- Sort out which packages are wired in. This has to be done last, since
  -- it modifies the unit ids of wired in packages, but when we process
  -- package arguments we need to key against the old versions.
  --
  (pkgs2, wired_map) <- findWiredInPackages dflags prec_map pkgs1 vis_map2
  let pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs2

  -- Update the visibility map, so we treat wired packages as visible.
  let vis_map = updateVisibilityMap wired_map vis_map2

  let hide_plugin_pkgs = gopt Opt_HideAllPluginPackages dflags
  plugin_vis_map <-
    case pluginPackageFlags dflags of
        -- common case; try to share the old vis_map
        [] | not hide_plugin_pkgs -> return vis_map
           | otherwise -> return Map.empty
        _ -> do let plugin_vis_map1
                        | hide_plugin_pkgs = Map.empty
                        -- Use the vis_map PRIOR to wired in,
                        -- because otherwise applyPackageFlag
                        -- won't work.
                        | otherwise = vis_map2
                plugin_vis_map2
                    <- foldM (applyPackageFlag dflags prec_map prelim_pkg_db unusable
                                (gopt Opt_HideAllPluginPackages dflags) pkgs1)
                             plugin_vis_map1
                             (reverse (pluginPackageFlags dflags))
                -- Updating based on wired in packages is mostly
                -- good hygiene, because it won't matter: no wired in
                -- package has a compiler plugin.
                -- TODO: If a wired in package had a compiler plugin,
                -- and you tried to pick different wired in packages
                -- with the plugin flags and the normal flags... what
                -- would happen?  I don't know!  But this doesn't seem
                -- likely to actually happen.
                return (updateVisibilityMap wired_map plugin_vis_map2)

  --
  -- Here we build up a set of the packages mentioned in -package
  -- flags on the command line; these are called the "preload"
  -- packages.  we link these packages in eagerly.  The preload set
  -- should contain at least rts & base, which is why we pretend that
  -- the command line contains -package rts & -package base.
  --
  -- NB: preload IS important even for type-checking, because we
  -- need the correct include path to be set.
  --
  let preload1 = Map.keys (Map.filter uv_explicit vis_map)

  let pkgname_map = foldl add Map.empty pkgs2
        where add pn_map p
                = Map.insert (packageName p) (componentId p) pn_map

  -- The explicitPackages accurately reflects the set of packages we have turned
  -- on; as such, it also is the only way one can come up with requirements.
  -- The requirement context is directly based off of this: we simply
  -- look for nested unit IDs that are directly fed holes: the requirements
  -- of those units are precisely the ones we need to track
  let explicit_pkgs = Map.keys vis_map
      req_ctx = Map.map (Set.toList)
              $ Map.unionsWith Set.union (map uv_requirements (Map.elems vis_map))


  let preload2 = preload1

  let
      -- add base & rts to the preload packages
      basicLinkedPackages
       | gopt Opt_AutoLinkPackages dflags
          = filter (flip elemUDFM (unPackageConfigMap pkg_db))
                [baseUnitId, rtsUnitId]
       | otherwise = []
      -- but in any case remove the current package from the set of
      -- preloaded packages so that base/rts does not end up in the
      -- set up preloaded package when we are just building it
      -- (NB: since this is only relevant for base/rts it doesn't matter
      -- that thisUnitIdInsts_ is not wired yet)
      --
      preload3 = nub $ filter (/= thisPackage dflags)
                     $ (basicLinkedPackages ++ preload2)

  -- Close the preload packages with their dependencies
  dep_preload <- closeDeps dflags pkg_db (zip (map toInstalledUnitId preload3) (repeat Nothing))
  let new_dep_preload = filter (`notElem` preload0) dep_preload

  let mod_map1 = mkModuleToPkgConfAll dflags pkg_db vis_map
      mod_map2 = mkUnusableModuleToPkgConfAll unusable
      mod_map = Map.union mod_map1 mod_map2

  when (dopt Opt_D_dump_mod_map dflags) $
      printInfoForUser (dflags { pprCols = 200 })
                       alwaysQualify (pprModuleMap mod_map)

  -- Force pstate to avoid leaking the dflags0 passed to mkPackageState
  let !pstate = PackageState{
    preloadPackages     = dep_preload,
    explicitPackages    = explicit_pkgs,
    pkgIdMap            = pkg_db,
    moduleToPkgConfAll  = mod_map,
    pluginModuleToPkgConfAll = mkModuleToPkgConfAll dflags pkg_db plugin_vis_map,
    packageNameMap          = pkgname_map,
    unwireMap = Map.fromList [ (v,k) | (k,v) <- Map.toList wired_map ],
    requirementContext = req_ctx
    }
  let new_insts = fmap (map (fmap (upd_wired_in_mod wired_map))) (thisUnitIdInsts_ dflags)
  return (pstate, new_dep_preload, new_insts)
