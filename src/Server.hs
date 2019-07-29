{-# LANGUAGE OverloadedStrings, ViewPatterns, NondecreasingIndentation #-}

module Server (server) where

import qualified Data.ByteString as S
import qualified Data.Text as T
import System.IO

import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)

import GHC
import GHC.Paths ( libdir )
import DynFlags ( defaultFatalMessager, defaultFlushOut )

{- ProtoBuf -}
import qualified Proto.Worker as W
import qualified Proto.Worker_Fields as W
import Data.ProtoLens (defMessage, pprintMessage)
import Data.ProtoLens.Encoding
  ( buildMessageDelimited
  , decodeMessageDelimitedH )
import Data.ProtoLens.Encoding.Bytes ( runBuilder )
import Lens.Micro

import Compile (compile)

logH :: Handle
logH = stderr

server :: Handle -> Handle -> [String] -> IO ()
server hIn hOut extra_args = do
    hSetBuffering stderr NoBuffering
    hSetBuffering hIn NoBuffering
    hSetBuffering hOut  NoBuffering
    hSetBinaryMode hIn True
    hSetBinaryMode hOut True
    hPutStrLn logH "Server Starts"
    
    _ <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) (forever loop)

    hPutStrLn logH "Server: Bye"

  where
    loop = do
        req <- getRequest hIn
        
        resp <- processRequest req extra_args

        sendResponse hOut resp

processRequest :: W.WorkRequest -> [String] -> Ghc W.WorkResponse
processRequest req extra_args = do
    let (args, _inputs) = destructRequest req

    compile (args ++ extra_args ++ ["-v3"])

    return sampleResponse
  
destructRequest :: W.WorkRequest -> ([String], [String])
destructRequest req = (flags, inputs)
  where
    flags = map T.unpack $ req ^. W.arguments
    inputs = map (T.unpack . (^. W.path)) $ req ^. W.inputs

sampleResponse :: W.WorkResponse
sampleResponse = defMessage
    & W.exitCode .~ 0
    & W.output   .~ "All good.\n"

getRequest :: MonadIO m => Handle -> m W.WorkRequest
getRequest hIn = liftIO $  do
        hPutStrLn logH $ "Server: Waiting for a message..."
        msg <- decodeMessageDelimitedH hIn
      
        hPutStrLn logH "Server: got a message, decoding..."
        req <- either fail return msg :: IO W.WorkRequest
        hPutStrLn logH $ "Server: msg received: " ++ (show . pprintMessage $ req)
        return req

sendResponse :: MonadIO m => Handle -> W.WorkResponse -> m ()
sendResponse hOut resp = liftIO $ do
        let msgresp = runBuilder . buildMessageDelimited $ resp
        S.hPut hOut msgresp
        hPutStrLn logH $ "Server sent response..."
