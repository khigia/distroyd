package main

import (
	"http"
	"json"
	"log"
	"template"
	"websocket"
)

var tmpl *template.Set

func init() {
	tmpl = template.SetMust(template.ParseTemplateGlob("tmpl/*.html"))
}

func WebServer(addr string, eng *CrossEngine, ems *Ems, mkt *Mkt) {
	http.Handle("/static/", http.FileServer(http.Dir(".")))
	http.HandleFunc("/login/", webLogin)
	http.HandleFunc("/logout/", webLogout)
	http.HandleFunc("/", webMakeHandle(webHome, eng, ems, mkt))
	http.HandleFunc("/instrument/", webMakeHandle(webInstrument, eng, ems, mkt))
	http.HandleFunc("/dashboard/", webMakeHandle(webDashboard, eng, ems, mkt))
	http.HandleFunc("/ws/dashboard/", webMakeWsHandle(webWsDashboard, eng, ems, mkt))
	if err := http.ListenAndServe(addr, nil); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}

type ClientApi struct {
	engine *CrossEngine // TODO no direct!
	ems    *Ems
	mkt    *Mkt
}

func (api *ClientApi) InstrumentAdd(key string) {
	rch := make(chan Resp)
	api.engine.api <- ReqInstrumentAdd{&ReqBase{rch}, key}
	<-rch
	log.Printf("Eng api added instrument %s", key)
}

func (api *ClientApi) InstrumentList() []string {
	rch := make(chan Resp)
	api.engine.api <- ReqInstrumentList{&ReqBase{rch}}
	insts := <-rch
	log.Printf("Eng api instrument list: %a", insts)
	return insts.([]string)
}

func (api *ClientApi) OrderAdd(order *Order) {
	log.Println("sending order to engine.api") // TODO to ems
	api.engine.api <- order
}

func (api *ClientApi) OrderDel(owner string, inst string, prx int, isBid bool) {
	log.Println("sending order del to engine.api") // TODO to ems
	api.engine.api <- ReqOrderDel{owner, inst, isBid, prx}
}

func webLogin(c http.ResponseWriter, req *http.Request) {
	if req.Method == "GET" {
		err := tmpl.Execute(c, "login.html", nil)
		if err != nil {
			http.Error(c, err.String(), http.StatusInternalServerError)
			return
		}
	} else {
		session := req.FormValue("login")
		http.SetCookie(c, &http.Cookie{
			Name:   "troyd-session",
			Value:  session,
			Path:   "/",
			Domain: "",
			MaxAge: 86400,
		})
		http.Redirect(c, req, "/", http.StatusFound)
		return
	}
}

func webLogout(c http.ResponseWriter, req *http.Request) {
	http.SetCookie(c, &http.Cookie{
		Name:   "troyd-session",
		Value:  "",
		Path:   "/",
		Domain: "",
		MaxAge: 86400,
	})
	http.Redirect(c, req, "/", http.StatusFound)
	return
}

func webMakeHandle(fn func(http.ResponseWriter, *http.Request, *ClientApi), eng *CrossEngine, ems *Ems, mkt *Mkt) http.HandlerFunc {
	// also take path etc ... can probably provide a type implementing the Handler interface
	return func(c http.ResponseWriter, req *http.Request) {
		if cookie, err := req.Cookie("troyd-session"); err != nil || cookie.Value == "" {
			http.Redirect(c, req, "/login/", http.StatusFound)
			return
		}
		// TODO not a ClientApi, but a EmsApi!!!
		api := &ClientApi{engine: eng, ems: ems, mkt: mkt}
		fn(c, req, api)
	}
}
func webHome(c http.ResponseWriter, req *http.Request, api *ClientApi) {
	cookie, err := req.Cookie("troyd-session")
	if err != nil {
		http.Error(c, err.String(), http.StatusInternalServerError)
		return
	}
	err = tmpl.Execute(c, "index.html", cookie.Value)
	if err != nil {
		http.Error(c, err.String(), http.StatusInternalServerError)
		return
	}
}

func webInstrument(c http.ResponseWriter, req *http.Request, api *ClientApi) {
	type Instrument struct {
		Key          string
		DashboardUrl string
	}
	type tmplargs struct {
		Login       string
		Instruments []Instrument
	}
	cookie, err := req.Cookie("troyd-session")
	if err != nil {
		http.Error(c, err.String(), http.StatusInternalServerError)
		return
	}
	id := req.URL.Path[len("/instrument/"):]
	if id != "" {
		http.Error(c, "empty resource", http.StatusInternalServerError)
		return
	} else {
		keys := api.InstrumentList()
		log.Printf("list: %a", keys)
		args := &tmplargs{
			Login:       cookie.Value,
			Instruments: make([]Instrument, len(keys)),
		}
		for i, k := range keys {
			args.Instruments[i].Key = k
			args.Instruments[i].DashboardUrl = "/dashboard/" + k
		}
		err := tmpl.Execute(c, "instrument-index.html", args)
		if err != nil {
			http.Error(c, err.String(), http.StatusInternalServerError)
			return
		}
	}
}

func webDashboard(c http.ResponseWriter, req *http.Request, api *ClientApi) {
	type tmplargs struct {
		Login      string
		Instrument string
	}
	cookie, err := req.Cookie("troyd-session")
	if err != nil {
		http.Error(c, err.String(), http.StatusInternalServerError)
		return
	}
	id := req.URL.Path[len("/dashboard/"):]
	if id != "" {
		err := tmpl.Execute(c, "dashboard.html", &tmplargs{
			Login:      cookie.Value,
			Instrument: id,
		})
		if err != nil {
			http.Error(c, err.String(), http.StatusInternalServerError)
			return
		}
	} else {
		http.Error(c, "empty resource", http.StatusInternalServerError)
		return
	}
}

func webMakeWsHandle(fn func(ws *websocket.Conn, api *ClientApi), eng *CrossEngine, ems *Ems, mkt *Mkt) http.HandlerFunc {
	return func(c http.ResponseWriter, req *http.Request) {
		// TODO not crossapi but EmsApi
		api := &ClientApi{engine: eng, ems: ems, mkt: mkt}
		fnh := func(ws *websocket.Conn) { fn(ws, api) }
		if _, found := req.Header["Sec-Websocket-Key1"]; found {
			websocket.Handler(fnh).ServeHTTP(c, req)
		} else {
			websocket.Handler(fnh).ServeHTTP(c, req)
			//websocket.Draft75Handler(fnh).ServeHTTP(c, req)
		}
	}
}

func webWsDashboard(ws *websocket.Conn, api *ClientApi) {
	var mdChan = make(chan []mdRow)
	var exChan = make(chan map[int]OrderQtyBookRow) // TODO this should come from mdChan!!!
	//var emsChan = make(chan int) // TODO sub to ems orders
	var ctrlChan = make(chan int)
	type wsMsg struct {
		Tag string
		Dt  []mdRow
		Ex  IfcEvt
	}
	go func() {
		log.Printf("Enter mdHub receiver")
		var md []mdRow
		var ex map[int]OrderQtyBookRow
		ok := true
		for ok {
			select {
			case md = <-mdChan:
				log.Println("ws client updated raw md")
			case ex = <-exChan:
				log.Printf("ws client recv ex evt: %A", ex)
			case <-ctrlChan:
				ok = false
				break
			}
			// publish to client the aggregate mkt/ems top-of-book
			for i, mdrow := range md {
				prx := mdrow[2]
				exrow := ex[prx]
				mdrow[0] = exrow.bid
				mdrow[4] = exrow.ask
				md[i] = mdrow
			}
			b, err := json.Marshal(wsMsg{Tag: "md", Dt: md})
			if err == nil {
				// log.Printf("Sending md: %A", b)
				if _, err := ws.Write(b); err != nil {
					ws.Close()
				}
			} else {
				log.Printf("Error encoding md: %A", md)
			}
		}
		log.Printf("Exit mdHub receiver")
	}()
	defer func() {
		api.mkt.Unsubscribe(mdChan)
		api.ems.Unsubscribe(exChan)
		ctrlChan <- 0
		ws.Close()
	}()
	api.mkt.Subscribe(mdChan)
	api.ems.Subscribe(exChan)
	type dashMsg struct {
		Login string
		Inst  string
		Row   mdRow
		Coln  int
	}
	buf := make([]byte, 256)
	for {
		n, err := ws.Read(buf)
		if err != nil {
			break
		}
		text := buf[:n]
		log.Printf("Received %s", text)
		var m dashMsg
		if err := json.Unmarshal(text, &m); err != nil {
			log.Printf("Error decoding mesage '%s': %s", text, err)
			continue
		}
		switch m.Coln {
		case 0: // del buy
			api.OrderDel(m.Login, m.Inst, m.Row[2], IS_BID)
		case 1: // add buy
			api.OrderAdd(&Order{m.Login, m.Inst, IS_BID, 1, m.Row[2], 0})
		case 2: // prx
		case 3: // add sell
			api.OrderAdd(&Order{m.Login, m.Inst, IS_ASK, 1, m.Row[2], 0})
		case 4: // del sell
			api.OrderDel(m.Login, m.Inst, m.Row[2], IS_ASK)
		}
	}
}
