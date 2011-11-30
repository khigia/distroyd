module Troyd.Exch (
  Exch,
  Side(..),
  BookRequest(..),
  Client,
  ExchMessage(..),
  MarketMessage(..),
  clMake,
  clSend,
  expMake,
  expOrdReq,
  expMktSubscribe 
) where

import System.Log.Logger
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Heap as Heap
import qualified Network (HostName, PortNumber)

import Control.Concurrent (ThreadId, forkIO)

import Control.Concurrent.Chan (Chan(..), newChan, readChan, writeChan)

logger = "Troyd.Exch"

data ClientConnection = ClientConnection {
    ccHost :: Network.HostName,
    ccPort :: Network.PortNumber,
    ccSender :: ExchMessage -> IO ()
  }

instance Show ClientConnection where
  show conn = intercalate ":" $ ($ conn) `fmap` [show . ccHost, show . ccPort]

data Client = Client {
    clConn :: ClientConnection,
    clSend :: ExchMessage -> IO ()
  }

instance Show Client where
  show cli = show $ clConn cli

clMake host port sender = do
  ch <- newChan
  forkIO $ sendingLoop ch
  return $ Client conn (writeChan ch)
  where
    conn = ClientConnection host port sender
    sendingLoop ch = do
      m <- readChan ch
      sender m
      sendingLoop ch

data Side = Bid | Ask deriving Show

sideOther Bid = Ask
sideOther Ask = Bid


data Order = Order {
    ordId :: Int,
    ordSide :: Side,
    ordTotalQty :: Int,
    ordFilledQty :: Int,
    ordTickPrice :: Int,
    ordClient :: Client
  }
  deriving Show

ordRemQty o = ordTotalQty o - ordFilledQty o


data OrderHeap = AskOrderHeap (Heap.MinPrioHeap (Int,Int) Order)
               | BidOrderHeap (Heap.MaxPrioHeap (Int,Int) Order)

orderHeapView (AskOrderHeap h) = let v = Heap.view h
                                 in case v of
                                   Just p -> Just (fst p, AskOrderHeap $ snd p)
                                   Nothing -> Nothing
orderHeapView (BidOrderHeap h) = let v = Heap.view h
                                 in case v of
                                   Just p -> Just (fst p, BidOrderHeap $ snd p)
                                   Nothing -> Nothing

orderHeapInsert item (BidOrderHeap h) = BidOrderHeap $ Heap.insert item h
orderHeapInsert item (AskOrderHeap h) = AskOrderHeap $ Heap.insert item h

orderHeapToDescList (AskOrderHeap h) = Heap.toDescList h
orderHeapToDescList (BidOrderHeap h) = Heap.toDescList h

orderHeapToAscList (AskOrderHeap h) = Heap.toAscList h
orderHeapToAscList (BidOrderHeap h) = Heap.toAscList h

data Book = Book {
    bkInst:: String,
    bkNextId :: Int,
    bkAskOrders :: OrderHeap,
    bkBidOrders :: OrderHeap
  }

bkMake instrument =
  Book {
    bkInst = instrument,
    bkNextId = 0,
    bkAskOrders = AskOrderHeap Heap.empty,
    bkBidOrders = BidOrderHeap Heap.empty
  }

bkGetOrders :: Book -> Side -> OrderHeap
bkGetOrders b Ask = bkAskOrders b
bkGetOrders b Bid = bkBidOrders b

bkSetOrders b Ask o = b { bkAskOrders = o}
bkSetOrders b Bid o = b { bkBidOrders = o}

bkAddOrder book order =
  let side = ordSide order
      qty = ordTotalQty order
      prx = ordTickPrice order
      (remQty, fills, book') = bkCross book (sideOther side) qty prx
      order' = order {ordFilledQty = qty - remQty}
  in if remQty > 0
     then (insert side order' book', order', fills)
     else (book', order', fills)
  where
    insert side o b = 
      let orders = bkGetOrders b side
          orders' = orderHeapInsert ((ordTickPrice o, ordId o), o) orders
      in bkSetOrders b side orders'

bkCross book side q prx =
  let orders = bkGetOrders book side
      (remQty, fills, orders') = crossOrders orders side q [] prx
  in (remQty, fills, bkSetOrders book side orders')
  where
    crossOrders orders side 0 fills prx =
      (0, fills, orders)
    crossOrders orders side q fills prx =
      case orderHeapView orders of
        Nothing ->
          (q, fills, orders)
        Just ((k, o), rest) ->
          if crossPrice side prx (ordTickPrice o)
          then
            if ordRemQty o <= q
            then
              --TODO Fill data type, with exec time and less details
              let fill = (ordRemQty o, o{ordFilledQty = ordFilledQty o + ordRemQty o})
              in crossOrders rest side (q - ordRemQty o) (fill:fills) prx
            else
              let fill = (q, o {ordFilledQty = ordFilledQty o + q})
              in (0, fill:fills, orderHeapInsert (k, snd fill) rest)
          else
            (q, fills, orders)
    crossPrice Ask = (>=)
    crossPrice Bid = (<=)

bkToString book note =
  intercalate "\n" $ concat [
    ["Book " ++ bkInst book ++ ": " ++ note],
    asks,
    bids
  ]
  where
    asks = orderListToStringList "  -" (orderHeapToDescList (bkAskOrders book))
    bids = orderListToStringList "   " (orderHeapToDescList (bkBidOrders book))
    orderListToStringList prefix = map ((prefix ++) . orderToString . snd)
    orderToString o = show (ordRemQty o) ++ "@" ++ show (ordTickPrice o) ++ " oid:" ++ show (ordId o)

data BookRequest = --TODO Snapshot(with subscribe param)
  OrderAdd {
    oaInst :: String,
    oaSide :: Side,
    oaQty :: Int, --TODO unsigned
    oaTick :: Int
  }
  | BookMarketSnapshot {
    bmsHandler :: MarketMessageHandler,
    bmsMktChan :: Chan MarketMessage
  }
  deriving Show

instance Show (Chan MarketMessage) where
  show it = "*chan market message*"


data ExchMessage = Ok
                 | OrderUpdate Order
                 | Fills [(Int,Order)]
                 | Text String


data BookProcessor = BookProcessor {
    bkpReqChan :: Chan (Maybe Client, BookRequest),
    bkpReqProcessThreadId :: ThreadId
  }

bkpMake mktCh book = do
  ch <- newChan
  threadId <- forkIO $ bkpReqProcess mktCh ch book
  return BookProcessor {
    bkpReqChan = ch,
    bkpReqProcessThreadId = threadId
  }

bkpReqProcess mktCh ch bk = do
  --TODO state monad ?
  qry <- readChan ch
  case qry of
    (Just cli, req) -> do
      bk' <- bkpDoOrderAdd mktCh bk cli req
      bkpReqProcess mktCh ch bk'
    (Nothing, req@BookMarketSnapshot {}) -> do
      bkpDoMarketSnapshot bk req
      bkpReqProcess mktCh ch bk

bkpDoMarketSnapshot bk req = do
  debugM logger $ "Book:market snapshot"
  --TODO forward to mkt channel with snapshot data
  let asks = orderHeapToDescList (bkAskOrders bk)
      bids = orderHeapToDescList (bkBidOrders bk)
      updates = map (orderToUpdate . snd) (asks ++ bids)
  writeChan (bmsMktChan req) $ MarketSnapshot {
    msHandler = bmsHandler req,
    msSnapshot = updates
  }
  where
    vers = bkNextId bk
    inst = bkInst bk
    orderToUpdate o = MarketUpdate vers inst (ordSide o) (ordRemQty o) (ordTickPrice o)

bkpDoOrderAdd mktCh bk cli req = do
  debugM logger $ "Book:processing : " ++ show req
  let order = Order {
    ordId = bkNextId bk,
    ordSide = oaSide req,
    ordTotalQty = oaQty req,
    ordTickPrice = oaTick req,
    ordFilledQty = 0,
    ordClient = cli
  }
  sendOrder order
  debugM logger $ "Book:created order " ++ show order
  let (bk', order', fills) = bkAddOrder bk order
  debugM logger $ "Book:added order " ++ show order' ++ " filling " ++ show fills
  pubOrderUpdate order'
  pubFillsUpdate cli fills
  let note = "added " ++ show (ordSide order) ++ " " ++ show (ordTotalQty order) ++ "@" ++ show (ordTickPrice order)
  debugM logger (bkToString bk' note)
  return (bk' { bkNextId = bkNextId bk' + 1})
  where
    pubOrderUpdate order = do
      sendOrder order
      mktUpd (bkNextId bk) (bkInst bk) (ordSide order) (ordRemQty order) (ordTickPrice order)
    pubFillsUpdate cli fills = do
      sendFills cli fills
      sendCounterpartyOrders fills
      mapM_ (\(q,o) -> (mktUpd (bkNextId bk) (bkInst bk) (ordSide o) (-q) (ordTickPrice o))) fills
    sendOrder order = do
      let cli = ordClient order
          upd = OrderUpdate order
      clSend cli upd
    sendCounterpartyOrders = mapM_ (sendOrder . snd)
    sendFills _ [] = return ()
    sendFills cli fills = do
      clSend cli (Fills fills)
    mktUpd vers inst side qty tick = do
      writeChan mktCh $ MarketUpdate vers inst side qty tick

data Exch = Exch {
    exBooks :: Map.Map String (IO BookProcessor)
  }

exMake instruments mktCh =
  Exch { exBooks = Map.fromList bkps }
  where
    bkps = [(p, (bkpMake mktCh . bkMake) p) | p <- instruments]


data ExchProcessor = ExchProcessor {
  --TODO seems reasonable use case for STM
    expReqChan :: Chan (Maybe Client, BookRequest),
    expMktChan :: Chan MarketMessage
  }

expMake instruments = do
  mktCh <- newChan
  forkIO $ expMktProcess [] mktCh
  reqCh <- newChan
  let ex = exMake instruments mktCh
  forkIO $ expReqProcess reqCh ex
  return $ ExchProcessor reqCh mktCh

expOrdReq ex cli req@(OrderAdd {}) = do
  -- TODO pre-filter of instrument?
  writeChan (expReqChan ex) (Just cli, req)
  return Ok
  
expReqProcess ch ex = do
  qry <- readChan ch
  case qry of
    (cli, req@OrderAdd{}) -> do
      case Map.lookup (oaInst req) (exBooks ex) of
        Just iobkp -> do
          bkp <- iobkp 
          writeChan (bkpReqChan bkp) (cli, req)
          --TODO static map ... except the IO thingy :(
          let ex' = ex {exBooks = Map.insert (oaInst req) (return bkp) (exBooks ex)}
          expReqProcess ch ex'
        Nothing ->
          expReqProcess ch ex
    (Nothing, req@BookMarketSnapshot{}) -> do
      mapM_ (\iobkp -> do bkp <- iobkp ; writeChan (bkpReqChan bkp) (Nothing, req)) (Map.elems $ exBooks ex)
      expReqProcess ch ex


data MarketMessageHandler = MktSubCallback (MarketMessage -> IO ()) --TODO handler do not know MarketMessage but MarketData (Snapshot data, update data)

instance Show MarketMessageHandler where
  show it = "*handler*"

--TODO separation of MarketMessage with MaketData
data MarketMessage = MarketOk
                   | MarketUpdate {
                       muVers :: Int,
                       muInst :: String,
                       muSide :: Side,
                       muQty :: Int,
                       muTickPrice :: Int
                     }
                   | MarketSnapshot {
                       msHandler :: MarketMessageHandler,
                       msSnapshot :: [MarketMessage]
                     }
                   | MarketSub {
                       msubHandler :: MarketMessageHandler
                     }
                   deriving Show

expMktProcess st ch = do
  msg <- readChan ch
  st' <- process st msg
  --TODO UDP broadcast for learning purpose?
  expMktProcess st' ch
  where
    process st mu@(MarketUpdate{}) = do
      debugM logger $ "pub mkt upd " ++ show mu
      --TODO if publish fail, ignore and and unregister
      mapM_ (\(MktSubCallback slot) -> slot mu) st
      return st
    process st MarketSub{msubHandler = hdl} = do
      return $ hdl:st
    process st MarketSnapshot{msHandler = MktSubCallback hdl, msSnapshot = updates} = do
      --TODO publish snapshot to new subscriber
      mapM_ hdl updates
      return st

expMktSubscribe exp slot = do
  --TODO do not send a MarketSubscribe ... rather send to book
  --a req to send a MarketMessage::Snapshot; when processing that msg
  --the expMktProcess send the snapshot to client and subscribe it
  --to update.
  let reqReq = BookMarketSnapshot {
    bmsMktChan = expMktChan exp,
    bmsHandler = MktSubCallback slot
  }
  writeChan (expReqChan exp) (Nothing, reqReq)
  let mktReq = MarketSub {
    msubHandler = MktSubCallback slot
  }
  writeChan (expMktChan exp) mktReq
  return MarketOk
