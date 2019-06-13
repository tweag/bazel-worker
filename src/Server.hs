{-# LANGUAGE OverloadedStrings #-}
module Server (server, client) where

{- ProtoBuf -}
import Proto.Worker as W
import Proto.Worker_Fields as W
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Encoding
  ( buildMessageDelimited
  , decodeMessage )
import Data.ProtoLens.Encoding.Bytes
  ( runBuilder
  , runParser
  , getVarInt )
import Lens.Micro

import qualified Data.ByteString as S

--import Control.Monad (forever, guard)
import Data.Bits ((.&.))
import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO

server :: Handle -> Handle -> IO ()
server hIn hOut = do
    hSetBuffering stderr NoBuffering
    hSetBuffering hIn NoBuffering
    hSetBuffering hOut  NoBuffering
    hSetBinaryMode hIn True
    hSetBinaryMode hOut True
    hPutStrLn stderr "Server Starts"
    {-forever-} 
    loop
    hPutStrLn stderr "Server: Bye"
  where
    loop = do
      let logH = stderr
      hPutStrLn logH $ "Server Waiting for Len..."
      len <- getLength hIn

      hPutStrLn logH $ "Server Len: " ++ show len
          ++ "\nServer Receiving Msg..."
      msg <- S.hGet hIn $ fromIntegral len
      
      hPutStrLn logH "Server after get msg, decoding..."
      req <- either fail return (decodeMessage msg) :: IO W.WorkRequest
      hPutStr logH $ "Server Msg Received: " ++ show req
          ++ "\nServer is about to respond, response size: "

      let resp = processRequest req
      let msgresp = runBuilder . buildMessageDelimited $ resp
      hPutStrLn logH . show . S.length $ msgresp
      S.hPut hOut msgresp
      hPutStrLn logH $ "Server sent response..."

getLength :: Handle -> IO Word64
getLength h = do
    buf <- mallocBytes 4
    arr <- loop buf 0
    res <- either fail return (runParser getVarInt . S.pack $ arr)
    free buf
    return res
  where
    loop :: Ptr Word8 -> Int -> IO [Word8]
    loop buf offset = do
        let cur = (buf `plusPtr` offset) :: Ptr Word8
        readCnt <- hGetBuf h cur 1
        _ <- if readCnt /= 1 
          then fail $ "getLength: readCnt /= 1 (= "  ++ show readCnt ++ ")"
          else return ()
        b <- peek cur
        if testMsb b
          then loop buf (offset + 1)
          else peekArray (offset + 1) buf

testMsb :: Word8 -> Bool
testMsb b = (b .&. 128) == 1

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
    hPutStrLn logH $ "Client started, about to send a msg of len: " 
        ++ show (S.length msgreq)
    S.hPut hOut msgreq
    hPutStrLn logH "Client: message sent; waiting for reply's len..."

    len <- getLength hIn
    hPutStrLn logH $ "Client recvd len: " ++ show len

    hPutStrLn logH $ "Client receiving reply..."
    msg <- S.hGet hIn $ fromIntegral len
    hPutStrLn logH "Client received reply, decoding..."

    resp <- either fail return (decodeMessage msg) :: IO W.WorkResponse
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
