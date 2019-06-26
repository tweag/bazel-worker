module Main where

import Server (server)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO


main :: IO ()
main = do
    args <- getArgs
    hPutStrLn stderr $ "Args taken: " ++ show args
    if "--persistent-worker" `elem` args
      then server stdin stdout
      else
          print "Worker should be called with --persistent-worker"
          >> exitFailure

