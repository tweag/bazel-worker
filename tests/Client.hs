{-# LANGUAGE OverloadedStrings #-}
module Main where

{- ProtoBuf -}
import Proto.Worker as W
import Proto.Worker_Fields as W
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Encoding
  ( buildMessageDelimited
  , decodeMessageDelimitedH )
import Data.ProtoLens.Encoding.Bytes ( runBuilder )
import Lens.Micro

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C (hPutStrLn)
import System.Process (runInteractiveCommand)
import System.IO

main :: IO () 
main = do
    (inp, out, err, _) <- 
      runInteractiveCommand
        --"./server.sh"
        "cabal new-run -v0 worker-proto -- --persistent-worker"

    -- pipe server's stderr to our stderr
    _ <- forkIO . forever $ do
        s <- S.hGetLine err
        C.hPutStrLn stderr s

    client inp out -- server's input is client's output and vv

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
        & W.path   .~ "tests/hello.hs"
        & W.digest .~ "test"
