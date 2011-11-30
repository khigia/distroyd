-- TODO there is no exception handling at all!!!
-- TODO separate Codec from Api (Net)
-- TODO api could be based on ZMQ + messagePack
module Troyd.MApi (
  server
) where

import Network (
  HostName,
  PortID(..),
  PortNumber,
  Socket,
  accept,
  listenOn,
  withSocketsDo
  )

import System.IO (
  BufferMode(..),
  Handle,
  hGetLine,
  hPutStrLn,
  hSetBuffering,
  hFlush
  )

import System.Log.Logger

import Control.Concurrent (
  forkIO,
  myThreadId
  )

import qualified Troyd.Exch

logger = "Troyd.MApi"

server port register = withSocketsDo $ do
    sock <- listenOn $ PortNumber port
    debugM logger $ "Listening on " ++ (show port)
    acceptor register sock

--acceptor :: Socket -> IO ()
acceptor register sock = do
    debugM logger $ "Accepting..."
    hhp <- accept sock
    let (handle, host, port) = hhp
    debugM logger $ "Accepted " ++ (show host) ++ (show port)
    threadId <- forkIO $ (handler register handle host port)
    acceptor register sock

--handler :: Handle -> HostName -> PortNumber -> IO ()
handler register handle host port = do
    receivingThreadId <- myThreadId
    debugM logger $ "Handler reader processing  connection " ++ (show host) ++ (show port)
    hSetBuffering handle LineBuffering -- or NoBuffering
    receiver <- register (sender handle)
    receivingLoop handle receiver
    where
      receivingLoop hdl receiver = do
        line <- hGetLine hdl -- TODO bytestring maybe ...
        debugM logger $ "Ignore mkt channel input"
        receivingLoop hdl receiver --TODO sequence_ repeat
      sender hdl m = do
        hPutStrLn hdl (serialize m)
        hFlush hdl

serialize Troyd.Exch.MarketOk = "ok"
serialize m@(Troyd.Exch.MarketUpdate{}) = show m
