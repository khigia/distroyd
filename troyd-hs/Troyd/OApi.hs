-- TODO there is no exception handling at all!!!
-- TODO separate Codec from Api (Net)
-- TODO api could be based on ZMQ + messagePack
module Troyd.OApi (
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

logger = "Troyd.OApi"

server port register = withSocketsDo $ do
    sock <- listenOn $ PortNumber port
    debugM logger $ "Api:Listening on " ++ (show port) -- TODO logging
    acceptor register sock

--acceptor :: Socket -> IO ()
acceptor register sock = do
    debugM logger $ "Api:Accepting..."
    hhp <- accept sock
    let (handle, host, port) = hhp
    debugM logger $ "Api:Accepted " ++ (show host) ++ (show port)
    threadId <- forkIO $ (handler register handle host port)
    acceptor register sock

--handler :: Handle -> HostName -> PortNumber -> IO ()
handler register handle host port = do
    receivingThreadId <- myThreadId
    debugM logger $ "Api:Handler reader processing  connection " ++ (show host) ++ (show port)
    hSetBuffering handle LineBuffering -- or NoBuffering
    receiver <- register host port receivingThreadId (sender handle)
    receivingLoop handle receiver
    where
      receivingLoop hdl receiver = do
        line <- hGetLine hdl -- TODO bytestring maybe ...
        let msg = unserialize line
        receiver msg
        receivingLoop hdl receiver --TODO sequence_ repeat
      sender hdl m = do
        hPutStrLn hdl (serialize m)
        hFlush hdl

serialize Troyd.Exch.Ok = "Ok"
serialize (Troyd.Exch.Text s) = s
serialize (Troyd.Exch.OrderUpdate o) = "OrderUpdate: " ++ show o
serialize (Troyd.Exch.Fills fills) = "Fills: " ++ show fills

unserialize line =
  --TODO before using messagePack, can try parsec for the heck of learning
  --also keeping wire format simple help debug/learn
  --TODO ideally wire format / whole api should be plugable!
  unserialize_ $ words line
  where
    unserialize_ ["OrderAdd", inst, side, qty, prx] =
      Troyd.Exch.OrderAdd inst (parseSide side) (parseQty qty) (parseTickPrice prx)
    parseSide "Bid" = Troyd.Exch.Bid
    parseSide "Ask" = Troyd.Exch.Ask
    parseQty :: String -> Int
    parseQty = read
    parseTickPrice :: String -> Int
    parseTickPrice = read

