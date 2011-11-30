package main

import (
  "fmt"
  "os"
  "json"
)

type Level struct {
  price int
  quantity int
}

func addAsk(asks []Level, tickSize int, bestAskIdx *int, newPrx int, newQtyDiff int) {
  if *bestAskIdx >= 0  {
    offset := (newPrx - asks[*bestAskIdx].price) / tickSize
    //TODO check offset is valid ... might need to resize underlying container
    if offset >= 0 {
      asks[*bestAskIdx + offset].quantity += newQtyDiff
    } else {
      *bestAskIdx += offset
      asks[*bestAskIdx].quantity += newQtyDiff
      asks[*bestAskIdx].price = newPrx //no need
    }
  } else {
    *bestAskIdx = len(asks) / 2
    asks[*bestAskIdx].price = newPrx
    asks[*bestAskIdx].quantity += newQtyDiff
  }
  fmt.Println(asks)
}

const (
  Bid = false
  Ask = true
)

const (
  DirAsc = -1
  DirDesc = +1
)

type HalfBook struct {
  direction int // -1 or +1 ... TODO type for this?
  levels []int
  bestIdx int
  bestPrx int
}

func newHalfBook(direction int, size int) *HalfBook {
  return &HalfBook{direction, make([]int, size), -1, 0}
}

func (book *HalfBook) addOrder(qty int, prx int) {
  if book.bestIdx >= 0  {
    offset := (prx - book.bestPrx) * book.direction
    //TODO check offset is valid ... might need to resize underlying container
    if offset >= 0 {
      book.levels[book.bestIdx + offset] += qty
    } else {
      book.bestIdx += offset
      book.bestPrx = prx
      book.levels[book.bestIdx] += qty
    }
  } else {
    book.bestIdx = len(book.levels) / 2 //TODO might not be optimal guess...
    book.bestPrx = prx
    book.levels[book.bestIdx] += qty
  }
}

func (book *HalfBook) toSeq(maxLen int) [][2]int { //TODO use a real type ...
  res := make([][2]int, maxLen)
  for i := book.bestIdx; i < book.bestIdx + maxLen && i < len(book.levels); i++ {
    resIdx := i - book.bestIdx
    var j int
    if book.direction < 0 {
      j = maxLen - resIdx - 1
    } else {
      j = resIdx
    }
    res[j][0] = book.levels[i]
    res[j][1] = book.bestPrx + resIdx * book.direction
  }
  return res
}

type OrderBook struct {
  tickSize int
  bids *HalfBook
  asks *HalfBook
}

func newOrderBook(tickSize int) *OrderBook {
  return &OrderBook{tickSize, newHalfBook(DirAsc, 10), newHalfBook(DirDesc, 10)}
}

func (book *OrderBook) getHalfBook(side bool) *HalfBook {
  if Ask == side {
    return book.asks
  } else {
    return book.bids
  }
  panic("never reach here")
}

func (book *OrderBook) addOrder(side bool, qty int, prx int) {
  ticks := prx / book.tickSize
  book.getHalfBook(side).addOrder(qty, ticks)
  encoder := json.NewEncoder(os.Stdout)
  encoder.Encode(book.getHalfBook(side).toSeq(5))
}

type Book struct {
  tickSize int
  levels [][2]int
  bestBidIdx int
  bestAskIdx int
  firstPrx int
}

func newBook(tickSize int) *Book {
  return &Book{tickSize, make([][2]int, 10), -1, -1, 0}
}

func min(i,j int) int {
  if i <= j {
    return i
  } else {
    return j;
  }
  panic("never reached")
}

func max(i,j int) int {
  if i <= j {
    return j
  } else {
    return i;
  }
  panic("never reached")
}

func (book *Book) addOrder(side bool, qty int, prx int) {
  ticks := prx / book.tickSize
  var idx *int
  var c int
  if Ask == side {
    idx = &book.bestAskIdx
    c = 0
  } else {
    idx = &book.bestBidIdx
    c = 1
  }
  if *idx >= 0 {
    //TODO check offset is valid ... might need to resize underlying container
    offset := ticks - book.firstPrx
    book.levels[offset][c] += qty
    if Ask == side {
      *idx = min(offset, *idx)
    } else {
      *idx = max(offset, *idx)
    }
  } else {
    if book.firstPrx > 0  {
      *idx = ticks - book.firstPrx
    } else {
      *idx = len(book.levels) / 2 //TODO might not be optimal guess...
      book.firstPrx = ticks - *idx
    }
    book.levels[*idx][c] += qty
  }
}

func (book *Book) toSeq(maxLen int) [][3]int { //TODO use a real type ...
  res := make([][3]int, maxLen)
  bookSpread := book.bestAskIdx - book.bestBidIdx
  //TODO not fair coz 0 based, favor Bid levels
  //TODO check if idx defined
  ibeg := book.bestBidIdx - (maxLen / 2) + (bookSpread / 2) // ok for crossing book, but need to take care of even/odd spread
  if ibeg < 0 {
    ibeg = 0
  }
  iend := book.bestAskIdx + (maxLen / 2) - (bookSpread / 2) // ok for crossing book, but need to take care of even/odd spread
  if iend >= len(book.levels) {
    iend = len(book.levels)
  }
  j := 0
  for i := ibeg; i < iend; i++ {
    res[j][0] = book.levels[i][0]
    res[j][1] = book.firstPrx + i
    res[j][2] = book.levels[i][1]
    j++
  }
  return res
}



func main() {
  asks := make([]Level, 10)
  tickSize := 10
  //bestAskIdx := -1
  fmt.Println(asks)

  //addAsk(asks, tickSize, &bestAskIdx, 1200, 11)
  //addAsk(asks, tickSize, &bestAskIdx, 1200, 11)
  //addAsk(asks, tickSize, &bestAskIdx, 1210, 10)
  //addAsk(asks, tickSize, &bestAskIdx, 1240, 8)
  //addAsk(asks, tickSize, &bestAskIdx, 1180, 7)
  //addAsk(asks, tickSize, &bestAskIdx, 1190, 6)

  askBook := newOrderBook(tickSize)
  askBook.addOrder(Ask, 6, 1180)
  askBook.addOrder(Ask, 7, 1190)
  askBook.addOrder(Ask, 9, 1210)
  askBook.addOrder(Ask, 5, 1160)
  askBook.addOrder(Bid, 1, 1150)
  askBook.addOrder(Bid, 2, 1140)
  
  encoder := json.NewEncoder(os.Stdout)
  book := newBook(tickSize)
  book.addOrder(Ask, 6, 1180)
  encoder.Encode(book.levels)
  book.addOrder(Ask, 7, 1190)
  encoder.Encode(book.levels)
  book.addOrder(Ask, 9, 1210)
  encoder.Encode(book.levels)
  book.addOrder(Ask, 5, 1160)
  encoder.Encode(book.levels)
  book.addOrder(Bid, 1, 1150)
  encoder.Encode(book.levels)
  book.addOrder(Bid, 2, 1140)
  encoder.Encode(book.levels)
  book.addOrder(Bid, 8, 1160) // crossing
  encoder.Encode(book.levels)
  encoder.Encode(book.toSeq(5))
}

