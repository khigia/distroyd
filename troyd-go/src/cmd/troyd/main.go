package main

import (
	"flag"
	"log"
	"rand"
	"time"
)

var webAddr = flag.String("webAddr", ":8080", "http service address")

func main() {
	log.Println("Start")
	flag.Parse()

	// cross engine
	// ems
	// md hub (dispatch top of book only)
	// client handler (aggregate ems feed and md feed to build dashboard view)

	eng := NewCrossEngine()
	go eng.RunApi()
	go eng.RunIfc()

	ems := NewEms()
	go ems.Run()
	ems.Connect(eng)

	mkt := MktNew(eng)
	go mkt.Run() // TODO receive data from engine ... but for now:
	go func() {
		for {
			mkt.dataChan <- []mdRow{
				{0, 0, 1580, rand.Intn(100), 0},
				{0, 0, 1570, rand.Intn(100), 0},
				{0, 0, 1560, rand.Intn(100), 0},
				{0, 0, 1550, rand.Intn(100), 0},
				{0, 0, 1540, rand.Intn(100), 0},
				{0, rand.Intn(100), 1530, 0, 0},
				{0, rand.Intn(100), 1520, 0, 0},
				{0, rand.Intn(100), 1510, 0, 0},
				{0, rand.Intn(100), 1500, 0, 0},
				{0, rand.Intn(100), 1490, 0, 0},
			}
			time.Sleep(rand.Int63n(20) * 50 * 1000000)
		}
	}()

	api := &ClientApi{engine: eng, ems: ems, mkt: mkt} // TODO admin API(add instrument), trade API(add order), monitoring API
	api.InstrumentAdd("BEA")
	api.InstrumentAdd("ZZZZ")

    WebServer(*webAddr, eng, ems, mkt)
	log.Println("Stop")
}
