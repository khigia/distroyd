package main

import (
        "log"
)

type Book struct {
	ifc *chan IfcEvt
	bids  []*Order //BidOrderHeap
	asks  []*Order //AskOrderHeap
}

func NewBook(ifc *chan IfcEvt) *Book {
	book := &Book{
		ifc: ifc,
		bids:  make([]*Order, 0),
		asks:  make([]*Order, 0),
	}
	return book
}

func (b *Book) AddOrder(o *Order) {
	log.Println("book recv order add")
	if o.IsBid {
		for len(b.asks) > 0 && o.RemQty() > 0 && o.Prx >= b.asks[0].Prx {
			if o.RemQty() >= b.asks[0].RemQty() {
				fillQty := b.asks[0].RemQty()
				o.Filled += fillQty         // publish fill
				b.asks[0].Filled += fillQty // publish fill
				*b.ifc <- &IfcEvtFill{
					Owner:   b.asks[0].Owner,
					Inst:    o.Inst,
					IsBid:   b.asks[0].IsBid,
					Prx:     b.asks[0].Prx,
					FillQty: fillQty,
					Ctpy:    o.Owner,
				}
				*b.ifc <- &IfcEvtFill{
					Owner:   o.Owner,
					Inst:    o.Inst,
					IsBid:   o.IsBid,
					Prx:     o.Prx,
					FillQty: fillQty,
					Ctpy:    b.asks[0].Owner,
				}
				b.asks = b.asks[1:]
			} else {
				fillQty := o.RemQty()
				o.Filled += fillQty         // publish fill
				b.asks[0].Filled += fillQty // publish fill
				*b.ifc <- &IfcEvtFill{
					Owner:   b.asks[0].Owner,
					Inst:    o.Inst,
					IsBid:   b.asks[0].IsBid,
					Prx:     b.asks[0].Prx,
					FillQty: fillQty,
					Ctpy:    o.Owner,
				}
				*b.ifc <- &IfcEvtFill{
					Owner:   o.Owner,
					Inst:    o.Inst,
					IsBid:   o.IsBid,
					Prx:     o.Prx,
					FillQty: fillQty,
					Ctpy:    b.asks[0].Owner,
				}
			}
		}
		if o.RemQty() > 0 {
			done := false
			for i := 0; i < len(b.bids); i++ {
				if o.Prx > b.bids[i].Prx {
					b.bids = append(b.bids[:i], append([]*Order{o}, b.bids[i:]...)...)
					done = true
					break
				}
			}
			if !done {
				b.bids = append(b.bids, o)
				done = true
			}
			log.Println("added bid order to book", len(b.bids))
			*b.ifc <- &IfcEvtMktQtyChange{
				Inst:    o.Inst,
				IsBid:   o.IsBid,
				Prx:     o.Prx,
				QtyDiff: o.RemQty(),
			}
		}
		log.Println("pulished bid book update")
	} else {
		for len(b.bids) > 0 && o.RemQty() > 0 && o.Prx <= b.bids[0].Prx {
			if o.RemQty() >= b.bids[0].RemQty() {
				fillQty := b.bids[0].RemQty()
				o.Filled += fillQty         // publish fill
				b.bids[0].Filled += fillQty // publish fill
				*b.ifc <- &IfcEvtFill{
					Owner:   b.bids[0].Owner,
					Inst:    b.bids[0].Inst,
					IsBid:   b.bids[0].IsBid,
					Prx:     b.bids[0].Prx,
					FillQty: fillQty,
					Ctpy:    o.Owner,
				}
				*b.ifc <- &IfcEvtFill{
					Owner:   o.Owner,
					Inst:    o.Inst,
					IsBid:   o.IsBid,
					Prx:     o.Prx,
					FillQty: fillQty,
					Ctpy:    b.bids[0].Owner,
				}
				b.bids = b.bids[1:]
			} else {
				fillQty := o.RemQty()
				o.Filled += fillQty         // publish fill
				b.bids[0].Filled += fillQty // publish fill
				*b.ifc <- &IfcEvtFill{
					Owner:   b.bids[0].Owner,
					Inst:    b.bids[0].Inst,
					IsBid:   b.bids[0].IsBid,
					Prx:     b.bids[0].Prx,
					FillQty: fillQty,
					Ctpy:    o.Owner,
				}
				*b.ifc <- &IfcEvtFill{
					Owner:   o.Owner,
					Inst:    o.Inst,
					IsBid:   o.IsBid,
					Prx:     o.Prx,
					FillQty: fillQty,
					Ctpy:    b.bids[0].Owner,
				}
			}
		}
		if o.RemQty() > 0 {
			done := false
			for i := 0; i < len(b.asks); i++ {
				if o.Prx < b.asks[i].Prx {
					b.asks = append(b.asks[:i], append([]*Order{o}, b.asks[i:]...)...)
					done = true
					break
				}
			}
			if !done {
				b.asks = append(b.asks, o)
				done = true
			}
			log.Println("added ask order to book", len(b.asks))
			*b.ifc <- &IfcEvtMktQtyChange{
				Inst:    o.Inst,
				IsBid:   o.IsBid,
				Prx:     o.Prx,
				QtyDiff: o.RemQty(),
			}
		}
		log.Println("pulished ask book update")
	}
}

func (b *Book) DelOrder(owner string, prx int, isBid bool) {
	log.Println("book recv order del")
	if isBid {
		log.Println("book recv bid order del", len(b.bids))
		orders := &b.bids
		for i := 0; i < len(*orders); {
			o := (*orders)[i]
			if o.Owner == owner && o.Prx == prx {
				log.Println("book del order", o)
				*b.ifc <- &IfcEvtMktQtyChange{
					Inst:    o.Inst,
					IsBid:   o.IsBid,
					Prx:     o.Prx,
					QtyDiff: -o.RemQty(),
				}
				*orders = append((*orders)[:i], (*orders)[i+1:]...)
			} else {
				i++
			}
		}
	} else {
		orders := &b.asks
		for i := 0; i < len(*orders); {
			o := (*orders)[i]
			if o.Owner == owner && o.Prx == prx {
				log.Println("book del order", o)
				*b.ifc <- &IfcEvtMktQtyChange{
					Inst:    o.Inst,
					IsBid:   o.IsBid,
					Prx:     o.Prx,
					QtyDiff: -o.RemQty(),
				}
				*orders = append((*orders)[:i], (*orders)[i+1:]...)
			} else {
				i++
			}
		}
	}
}

type CrossEngine struct {
	ifc chan IfcEvt
	api   chan Req
	insts map[string]*Book
}

func NewCrossEngine() *CrossEngine {
	return &CrossEngine{
		ifc: make(chan IfcEvt),
		api:   make(chan Req),
		insts: make(map[string]*Book),
	}
}

type Resp interface{}
type Req interface{}
type ReqBase struct {
	ch chan Resp
}
type ReqInstrumentAdd struct {
	*ReqBase
	Key string
}
type ReqInstrumentList struct {
	*ReqBase
}
type ReqOrderDel struct {
	owner string
	inst  string
	isBid bool
	prx   int
}

func (eng *CrossEngine) RunApi() {
	for {
		req := <-eng.api
		switch t := req.(type) {
		case *Order:
			log.Println("eng recev order request: ", t)
			eng.insts[t.Inst].AddOrder(t)
			log.Println("eng fwd order to book: ", t)
		case ReqOrderDel:
			log.Println("eng recev order del request: ", t)
			eng.insts[t.inst].DelOrder(t.owner, t.prx, t.isBid)
			log.Println("eng fwd order del to book: ", t)
		case ReqInstrumentAdd:
			log.Printf("Add instrument %s", t.Key)
			eng.insts[t.Key] = NewBook(&eng.ifc)
			t.ch <- "resp"
		case ReqInstrumentList:
			log.Printf("List instrument")
			keys := make([]string, len(eng.insts))
			i := 0
			for k, _ := range eng.insts {
				keys[i] = k
				i++
			}
			t.ch <- keys
		case ReqBase:
			log.Println("unexpected request type: %s", t)
			t.ch <- "error"
		default:
			log.Println("unknown request type: %s", req)
		}
	}
}

type IfcEvt interface{}
type IfcEvtSub struct {
	sub   chan IfcEvt
	isSub bool
}
type IfcEvtMktQtyChange struct {
	Inst    string
	IsBid   bool
	Prx     int
	QtyDiff int
}
type IfcEvtFill struct {
	Owner   string
	Inst    string
	IsBid   bool
	Prx     int
	FillQty int
	Ctpy    string
	// ref to order id
}

func (eng *CrossEngine) RunIfc() {
	conns := make(map[chan IfcEvt]int)
	for {
		m := <-eng.ifc
		switch t := m.(type) {
		case *IfcEvtFill:
			log.Println("eng ems evt: fill: %s", t)
            ch := IfcEvtMktQtyChange{
                    Inst: t.Inst,
                    IsBid: t.IsBid,
                    Prx: t.Prx,
                    QtyDiff: -t.FillQty,
            }
			for subscriber, _ := range conns {
				subscriber <- *t  // TODO only to owner
				subscriber <- ch
			}
		case *IfcEvtMktQtyChange:
			log.Println("eng ems evt: qty change to book: %s", t)
			for subscriber, _ := range conns {
				subscriber <- *t
			}
		case IfcEvtSub:
			log.Println("eng ems evt: sub req: %s", t)
			conns[t.sub] = 0, t.isSub
		default:
			log.Println("eng ems evt: unknown", m)
		}
	}
}

