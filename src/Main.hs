{-# LANGUAGE MultiWayIf #-}
module Main where

import System.Environment

import Server (server, client)

main :: IO ()
main = do
    [mode] <- getArgs
    if | mode == "server" -> server
       | mode == "client" -> client
       | otherwise        -> putStrLn "Specify the mode (server or client)!"
