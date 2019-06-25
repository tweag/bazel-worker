{-# LANGUAGE MultiWayIf #-}
module Main where

import System.Environment (getArgs)
import Server (server, client)

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C (hPutStrLn)
import System.Exit (exitFailure)
import System.IO
import System.Process (runInteractiveCommand)


main :: IO ()
main = do
    args <- getArgs
    hPutStrLn stderr $ "Args taken: " ++ show args
    if "--persistent-worker" `elem` args
      then server stdin stdout
      else setup
          -- print "Worker should be called with --persistent-worker"
          -- >> exitFailure

setup :: IO () 
setup = do
    (inp, out, err, _) <- 
      runInteractiveCommand "./server.sh"

    -- pipe server's stderr to our stderr
    _ <- forkIO . forever $ do
        s <- S.hGetLine err
        C.hPutStrLn stderr s

    client inp out -- server's input is client's output and vv
