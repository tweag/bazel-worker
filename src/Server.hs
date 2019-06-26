{-# LANGUAGE OverloadedStrings #-}
module Server (server) where

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
      hPutStr logH $ "Server: msg received: " ++ show req

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
