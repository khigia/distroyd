package main

type mdRow [5]int

type mdHubSub struct {
	mdChan    chan []mdRow
	subscribe bool
}

type Mkt struct {
	subChan  chan mdHubSub
	dataChan chan []mdRow
}

func MktNew() *Mkt {
	return &Mkt{
		subChan:  make(chan mdHubSub),
		dataChan: make(chan []mdRow),
	}
}
func (m *Mkt) Subscribe(cli chan []mdRow) {
	m.subChan <- mdHubSub{cli, true}
}
func (m *Mkt) Unsubscribe(cli chan []mdRow) {
	m.subChan <- mdHubSub{cli, false}
}

func (m *Mkt) Run() {
	conns := make(map[chan []mdRow]int)
	for {
		// TODO listen to snapshot/updates from eng and build top-of-book
		select {
		case subscription := <-m.subChan:
			// TODO handle per instrument sub req, and request
			// snapshot for new inst, and sub to eng updates
			conns[subscription.mdChan] = 0, subscription.subscribe
		case md := <-m.dataChan:
			for subscriber, _ := range conns {
				subscriber <- md
			}
		}
	}
}


