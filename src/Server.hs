{-# LANGUAGE OverloadedStrings #-}
module Server (server, client) where

{- ProtoBuf -}
import Proto.Worker as W
import Proto.Worker_Fields as W
import Data.ProtoLens (defMessage, showMessage)
import Data.ProtoLens.Encoding (buildMessage, decodeMessage)
import Data.ProtoLens.Encoding.Bytes (runBuilder)
import Lens.Micro

import qualified Data.ByteString as S

import Control.Monad (forever)
import Data.Either (fromRight)
import System.IO

server :: IO ()
server = forever $ do
    msg <- S.hGetContents stdin
    let req = (fromRight undefined $ decodeMessage msg) :: W.WorkRequest
    putStrLn $ "Received: " ++ show req
    let resp = processRequest req
    S.hPut stdout . runBuilder . buildMessage $ resp

processRequest :: W.WorkRequest -> W.WorkResponse
processRequest _ = sampleResponse

sampleResponse :: W.WorkResponse
sampleResponse = defMessage
    & W.exitCode .~ 0
    & W.output   .~ "All good"

serverDesc :: Int -> Int -> String
serverDesc pid fd = 
  "/proc/" ++ show pid ++ "/fd/" ++ show fd

_STDIN :: Int
_STDIN = 0

_STDOUT :: Int
_STDOUT = 1

client :: Int -> IO ()
client server_pid = do
    putStrLn "Client started"
    hSend <- openBinaryFile
        (serverDesc server_pid _STDIN)
        WriteMode
    hRecv <- openBinaryFile
        (serverDesc server_pid _STDOUT)
        ReadMode
    S.hPut hSend . runBuilder . buildMessage $ sampleReq
    msg <- S.hGetContents hRecv
    let resp = (fromRight undefined $ decodeMessage msg) :: W.WorkResponse
    putStrLn $ "Received: " ++ showMessage resp

sampleReq :: W.WorkRequest
sampleReq = defMessage
    & W.arguments .~ [""]
    & W.inputs    .~ [inp]
  where
    inp :: W.Input
    inp = defMessage
        & W.path   .~ "src/Main.hs"
        & W.digest .~ "main"
