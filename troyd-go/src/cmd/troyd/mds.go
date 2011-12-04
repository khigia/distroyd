package main

import (
	"log"
)

type mdRow [5]int

type MdEvt interface{}
type MdEvtSub struct {
	login     string
	inst      string
	mdChan    chan []mdRow
	subscribe bool
}

type BookTop struct {
	tickSize    int
	data        []mdRow
	subscribers []chan []mdRow
}

func NewBookTop() *BookTop {
	book := &BookTop{
		tickSize:    10,
		data:        make([]mdRow, 10),
		subscribers: make([]chan []mdRow, 0),
	}
    for i, _ := range book.data {
	    book.data[10 - 1 - i][2] = 1000 + i * book.tickSize
    }
	return book
}
func (b *BookTop) addQty(isBuy bool, prx int, qty int) {
	if len(b.data) == 0 {
		if isBuy {
			b.data = append(b.data, mdRow{0, qty, prx, 0, 0})
		} else {
			b.data = append(b.data, mdRow{0, 0, prx, qty, 0})
		}
	} else {
		frontPrice := b.data[0][2]
		index := (frontPrice - prx) / b.tickSize
		if index > 0 {
			for index >= len(b.data) {
				b.data = append(b.data, mdRow{0, 0, frontPrice - b.tickSize*len(b.data), 0, 0})
			}
		} else if index < 0 {
			for prx > b.data[0][2] {
				b.data = append([]mdRow{mdRow{0, 0, b.data[0][2] + b.tickSize, 0, 0}}, b.data...)
			}
			index = 0
		}
		if isBuy {
			row := b.data[index]
			row[1] += qty
			b.data[index] = row
		} else {
			row := b.data[index]
			row[3] += qty
			b.data[index] = row
		}
		// TODO delete rows all empty
	}
}

type Mkt struct {
	evtch    chan MdEvt
	engch    chan IfcEvt
	dataChan chan []mdRow
	books    map[string]*BookTop
}

func MktNew(eng *CrossEngine) *Mkt {
	mkt := &Mkt{
		evtch:    make(chan MdEvt),
		engch:    make(chan IfcEvt),
		dataChan: make(chan []mdRow),
		books:    make(map[string]*BookTop),
	}
	// TODO subscribe only to mkt evt
	eng.ifc <- IfcEvtSub{mkt.engch, true} // TODO eng to provide a mkt api
	return mkt
}
func (m *Mkt) Subscribe(login string, inst string, cli chan []mdRow) {
	m.evtch <- MdEvtSub{login, inst, cli, true}
}
func (m *Mkt) Unsubscribe(login string, inst string, cli chan []mdRow) {
	m.evtch <- MdEvtSub{login, inst, cli, false}
}

func (m *Mkt) GetBook(inst string) *BookTop {
	book, ok := m.books[inst]
	if !ok {
		book = NewBookTop()
		m.books[inst] = book
	}
	return book
}

func (mkt *Mkt) Run() {
	for {
		// TODO listen to snapshot/updates from eng and build top-of-book
		select {
		case e := <-mkt.engch:
			switch m := e.(type) {
			case IfcEvtMktQtyChange:
				book := mkt.GetBook(m.Inst)
				book.addQty(m.IsBid, m.Prx, m.QtyDiff)
				for _, subscriber := range book.subscribers {
					subscriber <- book.data
				}
                log.Println("mds: mkt qty change: ", book)
			}
		case e := <-mkt.evtch:
			switch m := e.(type) {
			case MdEvtSub:
				// TODO handle per instrument sub req, and request
				// snapshot for new inst, and sub to eng updates
				book := mkt.GetBook(m.inst)
				if m.subscribe {
					book.subscribers = append(book.subscribers, m.mdChan)
					log.Println("mkt add sub for ", m.inst)
					m.mdChan <- book.data
				} else {
					for i, subscriber := range book.subscribers {
						if subscriber == m.mdChan {
							book.subscribers = append(book.subscribers[:i], book.subscribers[i+1:]...)
							break
						}
					}
				}
			}
		case md := <-mkt.dataChan:
			book := mkt.GetBook("ZZZZ")
			for _, subscriber := range book.subscribers {
				subscriber <- md
			}
		}
	}
}
