{-# LANGUAGE MultiWayIf #-}
module Main where

import System.Environment (getArgs)
--import System.Posix.Process (getProcessID)
import Server (server, client)

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString as S
import System.IO
import System.Process (runInteractiveCommand)


main :: IO ()
main = do
    args <- getArgs
    hPutStrLn stderr $ "Args taken: " ++ show args
    if "--persistent-worker" `elem` args
      then server stdin stdout
          {-let hOut = stdout
          hOut <- openBinaryFile
            "resp.dat"
            WriteMode
          server stdin hOut
          hClose hOut -}
      else setup

setup :: IO () 
setup = do
    (inp, out, err, _) <- 
      runInteractiveCommand "./server.sh"

    -- pipe server's stderr to our stderr
    _ <- forkIO . forever $ do
        c <- S.hGet err 1
        S.hPut stderr c

    client inp out -- server's input is client's output and vv
