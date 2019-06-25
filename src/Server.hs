{-# LANGUAGE OverloadedStrings #-}
module Server (server, client) where

{- ProtoBuf -}
import Proto.Worker as W
import Proto.Worker_Fields as W
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Encoding
  ( buildMessageDelimited
  , decodeMessageDelimitedH )
import Data.ProtoLens.Encoding.Bytes ( runBuilder )
import Lens.Micro

import Control.Monad (forever)
import qualified Data.ByteString as S
import System.IO

server :: Handle -> Handle -> IO ()
server hIn hOut = do
    hSetBuffering stderr NoBuffering
    hSetBuffering hIn NoBuffering
    hSetBuffering hOut  NoBuffering
    hSetBinaryMode hIn True
    hSetBinaryMode hOut True
    hPutStrLn stderr "Server Starts"
    _ <- forever loop
    hPutStrLn stderr "Server: Bye"
  where
    loop = do
      let logH = stderr

      hPutStrLn logH $ "Server: Waiting for a message..."
      msg <- decodeMessageDelimitedH hIn
      
      hPutStrLn logH "Server: got a message, decoding..."
      req <- either fail return msg :: IO W.WorkRequest
      hPutStrLn logH $ "Server: msg received: " ++ show req

      -- Processing a request
      let resp = processRequest req

      let msgresp = runBuilder . buildMessageDelimited $ resp
      S.hPut hOut msgresp
      hPutStrLn logH $ "Server sent response..."

processRequest :: W.WorkRequest -> W.WorkResponse
processRequest _ = sampleResponse

sampleResponse :: W.WorkResponse
sampleResponse = defMessage
    & W.exitCode .~ 0
    & W.output   .~ "All good"

client :: Handle -> Handle -> IO ()
client hOut hIn = do
    hSetBuffering stderr NoBuffering
    hSetBuffering hIn NoBuffering
    hSetBuffering hOut  NoBuffering
    hSetBinaryMode hIn True
    hSetBinaryMode hOut True
    let logH = stderr

    let msgreq = runBuilder . buildMessageDelimited $ sampleReq
    hPutStrLn logH $ "Client started, about to send a request" 
    S.hPut hOut msgreq

    hPutStrLn logH $ "Client receiving reply..."
    msg <- decodeMessageDelimitedH hIn

    resp <- either fail return msg :: IO W.WorkResponse
    hPutStrLn logH $ "Client received: " ++ show resp

sampleReq :: W.WorkRequest
sampleReq = defMessage
    & W.arguments .~ ["-O1"]
    & W.inputs    .~ [inp]
  where
    inp :: W.Input
    inp = defMessage
        & W.path   .~ "src/Main.hs"
        & W.digest .~ "main"
