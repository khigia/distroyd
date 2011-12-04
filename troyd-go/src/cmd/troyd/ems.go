package main

import (
	"log"
)

type OrderQtyBookRow struct {
	bid int
	ask int
}

type OrderQtyBook map[int]OrderQtyBookRow

type EmsBookTopSub struct {
	login string
	inst  string
	sub   chan OrderQtyBook
	isSub bool
}
type OrderQtyBookContext struct {
	book        OrderQtyBook
	subscribers map[chan OrderQtyBook]int
}
type booksT map[string](map[string]*OrderQtyBookContext)
type Ems struct {
	evtCh chan IfcEvt
	eng   *CrossEngine
	books booksT
}

func NewEms() *Ems {
	return &Ems{
		evtCh: make(chan IfcEvt),
		books: make(booksT),
	}
}
func (ems *Ems) Connect(eng *CrossEngine) {
	// TODO subscribe only to exec evt
	eng.ifc <- IfcEvtSub{ems.evtCh, true}
	ems.eng = eng // TODO not single engine!
}
func (ems *Ems) Subscribe(login string, inst string, ch chan OrderQtyBook) {
	ems.evtCh <- EmsBookTopSub{login, inst, ch, true}
}
func (ems *Ems) Unsubscribe(login string, inst string, ch chan OrderQtyBook) {
	ems.evtCh <- EmsBookTopSub{login, inst, ch, false}
}

func (ems *Ems) OrderAdd(order *Order) {
	ems.eng.api <- order
}

func (ems *Ems) getOrderQtyBookContext(login string, inst string) *OrderQtyBookContext {
	instMap, okLogin := ems.books[login]
	if !okLogin {
		instMap = make(map[string]*OrderQtyBookContext)
		ems.books[login] = instMap
	}
	ctx, okInst := instMap[inst]
	if !okInst {
		ctx = &OrderQtyBookContext{
			book:        make(OrderQtyBook),
			subscribers: make(map[chan OrderQtyBook]int),
		}
		instMap[inst] = ctx
	}
	return ctx
}

func (ems *Ems) Run() {
	for {
		m := <-ems.evtCh
		switch t := m.(type) {
		case IfcEvtOrderAdd:
			ctx := ems.getOrderQtyBookContext(t.o.Owner, t.o.Inst)
			book := ctx.book
			if t.o.IsBid {
				row := book[t.o.Prx]
				row.bid += t.o.Qty
				book[t.o.Prx] = row
			} else {
				row := book[t.o.Prx]
				row.ask += t.o.Qty
				book[t.o.Prx] = row
			}
			ctx.book = book
			for subscriber, _ := range ctx.subscribers {
				subscriber <- book
			}
			log.Println("ems: recv order add notification: ", book)
		case IfcEvtFill:
			log.Println("ems recv fill from engine")
			ctx := ems.getOrderQtyBookContext(t.Owner, t.Inst)
			book := ctx.book
			if t.IsBid {
				row := book[t.Prx]
				row.bid -= t.FillQty
				book[t.Prx] = row
			} else {
				row := book[t.Prx]
				row.ask -= t.FillQty
				book[t.Prx] = row
			}
			ctx.book = book
			log.Println("ems recv fill book ", book)
			for subscriber, _ := range ctx.subscribers {
				subscriber <- book
			}
		case EmsBookTopSub:
			log.Println("ems evt: sub req: ", t)
			ctx := ems.getOrderQtyBookContext(t.login, t.inst)
			ctx.subscribers[t.sub] = 0, t.isSub
			if t.isSub {
				t.sub <- ctx.book
			}
		default:
			log.Println("ems evt: unknown type: ", m)
		}
	}
}
