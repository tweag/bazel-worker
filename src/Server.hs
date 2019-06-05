{-# LANGUAGE OverloadedStrings #-}
module Server (server, client) where

{- Networking -}
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

{- ProtoBuf -}
import Proto.Worker as W
import Proto.Worker_Fields as W
import Data.ProtoLens (defMessage, showMessage)
import Data.ProtoLens.Encoding (buildMessage, decodeMessage)
import Data.ProtoLens.Encoding.Bytes (runBuilder)
import Lens.Micro

import Data.Either (fromRight)

port :: Int
port = 44444

max_bytes_msg :: Int
max_bytes_msg = 4096

server :: IO ()
server = withSocketsDo $ do
    addr <- resolve port
    E.bracket (open addr) close loop
  where
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just . show $ port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        -- If the prefork technique is not used,
        -- set CloseOnExec for the security reasons.
        withFdSocket sock $ \fd -> do
          setCloseOnExecIfNeeded fd
          bind sock (addrAddress addr)
          listen sock 10
          return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (talk conn) (\_ -> close conn)
    talk conn = do
        msg <- recv conn max_bytes_msg
        unless (S.null msg) $ do
          sendAll conn msg
          talk conn

client :: IO ()
client = withSocketsDo $ do
    addr <- resolve "127.0.0.1" port
    E.bracket (open addr) close talk
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just . show $ port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    talk sock = do
        sendAll sock . runBuilder $ buildMessage sampleReq
        msg <- recv sock max_bytes_msg
        putStr "Received: "
        let resp = (fromRight undefined $ decodeMessage msg) :: W.WorkRequest
        -- TODO: above we say Req instead of Resp as we're still
        --       testing with the Echo server
        putStrLn (show resp)

sampleReq :: W.WorkRequest
sampleReq = defMessage
    & W.arguments .~ [""]
    & W.inputs    .~ [inp]
  where
    inp :: W.Input
    inp = defMessage
        & W.path   .~ "src/Main.hs"
        & W.digest .~ "main"
