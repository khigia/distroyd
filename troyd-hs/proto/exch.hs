import qualified Data.Heap
import qualified Data.Map as Map
import Data.List (intercalate)

data Side = Bid | Ask deriving Show
otherSide Bid = Ask
otherSide Ask = Bid

data Order = Order {
               oid :: Int,
               ordSide :: Side,
               totalQty :: Int,
               filledQty :: Int,
               tickPrice :: Int
             }
             deriving Show
remQty o = totalQty o - filledQty o

data OrderHeap = AskOrderHeap (Data.Heap.MinPrioHeap (Int,Int) Order)
               | BidOrderHeap (Data.Heap.MaxPrioHeap (Int,Int) Order)
--TODO lifting to simplify/clean code
orderHeapView (AskOrderHeap h) = let v = Data.Heap.view h
                                 in case v of
                                   Just p -> Just (fst p, AskOrderHeap $ snd p)
                                   Nothing -> Nothing
orderHeapView (BidOrderHeap h) = let v = Data.Heap.view h
                                 in case v of
                                   Just p -> Just (fst p, BidOrderHeap $ snd p)
                                   Nothing -> Nothing
orderHeapInsert item (BidOrderHeap h) = BidOrderHeap $ Data.Heap.insert item h
orderHeapInsert item (AskOrderHeap h) = AskOrderHeap $ Data.Heap.insert item h
orderHeapToDescList (AskOrderHeap h) = Data.Heap.toDescList h
orderHeapToDescList (BidOrderHeap h) = Data.Heap.toDescList h
orderHeapToAscList (AskOrderHeap h) = Data.Heap.toAscList h
orderHeapToAscList (BidOrderHeap h) = Data.Heap.toAscList h

data Book = Book {
              security :: String,
              nextId :: Int,
              askOrders :: OrderHeap,
              bidOrders :: OrderHeap
            }

mkbook product = Book {
           security = product,
           nextId = 0,
           bidOrders = BidOrderHeap Data.Heap.empty,
           askOrders = AskOrderHeap Data.Heap.empty
         }

getOrders :: Side -> Book -> OrderHeap
getOrders Ask = askOrders
getOrders Bid = bidOrders

setOrders b Ask o = b { askOrders = o}
setOrders b Bid o = b { bidOrders = o}

data Exch = Exch {
              books :: Map.Map String Book
            }

cross book side q prx =
  let orders = getOrders side book
      (remQty, fills, orders') = crossOrders orders side q [] prx
  in (remQty, fills, setOrders book side orders')
  where
    crossOrders orders side 0 fills prx =
      (0, fills, orders)
    crossOrders orders side q fills prx =
      case orderHeapView orders of
        Nothing ->
          (q, fills, orders)
        Just ((k, o), rest) ->
          if crossPrice side prx (tickPrice o)
          then
            if remQty o <= q
            then
              crossOrders rest side (q - remQty o) ((remQty o, o{filledQty=filledQty o + remQty o}):fills) prx
            else
              (0, (q, o {filledQty=filledQty o + q}):fills, orderHeapInsert (k, o { filledQty = filledQty o + q}) rest)
          else
            (q, fills, orders)
    crossPrice Ask = (>=)
    crossPrice Bid = (<=)

createOrder exch security side qty prx =
  case Map.lookup security (books exch) of
    Just book ->
      let order = Order {oid = nextId book,
                         ordSide = side,
                         totalQty = qty,
                         filledQty = 0,
                         tickPrice = prx}
          --TODO index order by id to be able to delete later
          -- unless oid embed some info like book and side
      in Just (book {nextId = nextId book + 1}, order)
    Nothing -> Nothing

addOrder book order =
  let side = ordSide order
      qty = totalQty order
      prx = tickPrice order
      (remQty, fills, book') = cross book (otherSide side) qty prx
      order' = order {filledQty = qty - remQty}
  in if remQty > 0
     then (insert side order' book', order', fills)
     else (book', order', fills)
  where
    insert side o b = 
      let orders = getOrders side b
          orders' = orderHeapInsert ((tickPrice o, oid o), o) orders
      in setOrders b side orders'

toString book note =
  intercalate "\n" $ concat [
    ["Book " ++ security book ++ ": " ++ note],
    asks,
    bids
  ]
  where
    asks = orderListToStringList "  -" (orderHeapToDescList (askOrders book))
    bids = orderListToStringList "   " (orderHeapToAscList (bidOrders book))
    orderListToStringList prefix = map ((prefix ++) . orderToString . snd)
    orderToString o = show (remQty o) ++ "@" ++ show (tickPrice o) ++ " oid:" ++ show (oid o)

mkexch products =
  Exch { books = Map.fromList books}
  where
    books = map (\p -> (p, mkbook p)) products

main =
  return (mkexch ["S&P", "ZZZ"]) >>=
  showAddOrder "S&P" Bid 1 100 >>=
  showAddOrder "S&P" Bid 2 100 >>=
  showAddOrder "S&P" Bid 3 100 >>=
  showAddOrder "S&P" Bid 3 101 >>=
  showAddOrder "S&P" Ask 3 103 >>=
  showAddOrder "S&P" Ask 2 103 >>=
  showAddOrder "S&P" Ask 4 104 >>=
  showAddOrder "S&P" Ask 5 101 >>=
  showAddOrder "S&P" Bid 5 103 >>=
  showAddOrder "ZZZ" Bid 1 100 >>=
  showAddOrder "ZZZ" Ask 8 101 >>=
  showAddOrder "ZZZ" Bid 3 101 >>=
  (\_ -> return ())
  where
    showAddOrder security side q p exch | q > 0 =
      case createOrder exch security side q p of
        Just (book, order) ->
          let -- TODO ack order creation to client
              (book', o, fills) = addOrder book order
              exch' = exch { books = Map.insert security book' (books exch) }
              --TODO trade order to client (trade channel)
              --TODO publish fills
              --TODO publish prices/volumes changes (market data channel)
              note = "add " ++ show (ordSide order) ++ " " ++ show (totalQty order) ++ "@" ++ show (tickPrice order)
          in putStrLn (toString book' note) >>
             putStrLn (show o) >>
             putStrLn (show fills) >>
             return exch'
        _ -> return exch -- drop order
