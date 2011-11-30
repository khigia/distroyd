package main

import (
	"log"
)

type OrderQtyBookRow struct {
	bid int
	ask int
}
type EmsBookTopSub struct {
	sub   chan map[int]OrderQtyBookRow
	isSub bool
}
type Ems struct {
	evtCh chan IfcEvt
}

func NewEms() *Ems {
	return &Ems{
		evtCh: make(chan IfcEvt),
	}
}
func (ems *Ems) Connect(eng *CrossEngine) {
	// TODO get list of inst
	eng.ifc <- IfcEvtSub{ems.evtCh, true}
}
func (ems *Ems) Subscribe(cli chan map[int]OrderQtyBookRow) {
	ems.evtCh <- EmsBookTopSub{cli, true}
}
func (ems *Ems) Unsubscribe(cli chan map[int]OrderQtyBookRow) {
	ems.evtCh <- EmsBookTopSub{cli, false}
}
func (ems *Ems) Run() {
	conns := make(map[chan map[int]OrderQtyBookRow]int)
	qties := make(map[int]OrderQtyBookRow)
	for {
		// TODO subscription per inst
		// TODO maintain order book, execution book
		// listen to updates from ems and build client top-of-book
		m := <-ems.evtCh
		switch t := m.(type) {
		case IfcEvtMktQtyChange:
			log.Println("ems evt: qty change: ", t)
			row := qties[t.Prx]
			if t.IsBid {
				row.bid += t.QtyDiff
			} else {
				row.ask += t.QtyDiff
			}
			qties[t.Prx] = row
			log.Println("ems evt: qty changed:", qties)
			for subscriber, _ := range conns {
				subscriber <- qties
			}
		case string:
			log.Println("ems evt: string: ", t)
			for subscriber, _ := range conns {
				subscriber <- qties
			}
		case EmsBookTopSub:
			log.Println("ems evt: sub req: ", t)
			conns[t.sub] = 0, t.isSub
			if t.isSub {
				t.sub <- qties
			}
		default:
			log.Println("ems evt: unknown type: ", m)
		}
	}
}
