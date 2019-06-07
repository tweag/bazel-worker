{-# LANGUAGE MultiWayIf #-}
module Main where

import System.Environment (getArgs)
import System.Posix.Process (getProcessID)
import Server (server, client)

main :: IO ()
main = do
    pid <- getProcessID
    putStrLn $ "Started with pid " ++ show pid
    args <- getArgs
    putStrLn $ "Args taken: " ++ show args
    if | "--persistent-worker" `elem` args
         -> server
       | "client" `elem` args
         -> client (read (args !! 1) :: Int)
       | otherwise
         -> putStrLn $
            "Compiler started in a non-persistent mode!" ++
            "Use --persistent-worker. Exit."
