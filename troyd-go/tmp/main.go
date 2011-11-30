package main

import (
    "flag"
    "http"
    "log"
    "template"
    "websocket"
    "net"
    "fmt"
    "os"
    // "bufio"
)

var addr = flag.String("addr", ":8080", "http service address")

func main() {
	flag.Parse()
	go hub()
	go md()
	http.HandleFunc("/", homeHandler)
	http.HandleFunc("/ws", webSocketProtocolSwitch)
	if err := http.ListenAndServe(*addr, nil); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}

func webSocketProtocolSwitch(c http.ResponseWriter, req *http.Request) {
	// Handle old and new versions of protocol.
	if _, found := req.Header["Sec-Websocket-Key1"]; found {
		websocket.Handler(clientHandler).ServeHTTP(c, req)
	} else {
		websocket.Handler(clientHandler).ServeHTTP(c, req)
		//websocket.Draft75Handler(clientHandler).ServeHTTP(c, req)
	}
}

var messageChan = make(chan []byte)

var mdChan = make(chan []byte)
//var mdChan = make(chan string)

type subscription struct {
	conn      *websocket.Conn
	subscribe bool
}

var subscriptionChan = make(chan subscription)

func hub() {
	conns := make(map[*websocket.Conn]int)
	for {
		select {
		case subscription := <-subscriptionChan:
			conns[subscription.conn] = 0, subscription.subscribe
		case md := <-mdChan:
			for conn, _ := range conns {
				//if _, err := conn.Write([]byte(md)); err != nil {
				if _, err := conn.Write(md); err != nil {
					conn.Close()
				}
			}
		}
	}
}

func md() {
    var (
        host = "127.0.0.1";
        port = "12346";
        remote = host + ":" + port;
    )
    con, error := net.Dial("tcp", remote)
    defer con.Close()
    if error != nil { fmt.Printf("Host not found: %s\n", error ); os.Exit(1); }
    
    // var nl byte = 10
    // reader := bufio.NewReader(con)
	
    for {
	    buf := make([]byte, 256)
		n, err := con.Read(buf)
		if err != nil {
			break
		}
		mdChan <- buf[0:n]
        // s, err := reader.ReadString(nl)
        // if err != nil {
        //     break
        // }
        // mdChan <- s
	}
}

func clientHandler(ws *websocket.Conn) {
	defer func() {
		subscriptionChan <- subscription{ws, false}
		ws.Close()
	}()

	subscriptionChan <- subscription{ws, true}

    var (
        host = "127.0.0.1";
        port = "12345";
        remote = host + ":" + port;
    )
    con, error := net.Dial("tcp", remote)
    defer con.Close()
    if error != nil { fmt.Printf("Host not found: %s\n", error ); os.Exit(1); }
    
    // read from socket and echo to messageChan or mdChan etc
	for {
	    buf := make([]byte, 256)
		n, err := ws.Read(buf)
		if err != nil {
			break
		}
        buf[n] = 10
        fmt.Printf("Forwarding: %s\n", buf[0:n])
		if _, err := con.Write(buf[0:n+1]); err != nil {
			con.Close()
            break
		}
	}
}

// Handle home page requests.
func homeHandler(c http.ResponseWriter, req *http.Request) {
	homeTempl.Execute(c, req.Host)
}

var homeTempl *template.Template

func init() {
	homeTempl = template.New()
	homeTempl.SetDelims("<<", ">>")
	if err := homeTempl.Parse(homeStr); err != nil {
		panic("template error: " + err.String())
	}
}

const homeStr = `
<html>
<head>
<title>Chat Example</title>
<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"></script>
<script type="text/javascript">
    $(function() {

    var conn;
    var msg = $("#msg");
    var log = $("#log");

    function appendLog(msg) {
        var d = log[0]
        var doScroll = d.scrollTop == d.scrollHeight - d.clientHeight;
        msg.appendTo(log)
        if (doScroll) {
            d.scrollTop = d.scrollHeight - d.clientHeight;
        }
    }

    $("#form").submit(function() {
        if (!conn) {
            return false;
        }
        if (!msg.val()) {
            return false;
        }
        conn.send(msg.val());
        msg.val("");
        return false
    });

    if (window["WebSocket"]) {
        conn = new WebSocket("ws://<<@>>/ws");
        conn.onclose = function(evt) {
            appendLog($("<div><b>Connection closed.</b></div>"))
        }
        conn.onmessage = function(evt) {
            appendLog($("<div/>").text(evt.data))
        }
    } else {
        appendLog($("<div><b>Your browser does not support WebSockets.</b></div>"))
    }
    });
</script>
<style type="text/css">
html {
    overflow: hidden;
}

body {
    overflow: hidden;
    padding: 0;
    margin: 0;
    width: 100%;
    height: 100%;
    background: gray;
}

#log {
    background: white;
    margin: 0;
    padding: 0.5em 0.5em 0.5em 0.5em;
    position: absolute;
    top: 0.5em;
    left: 0.5em;
    right: 0.5em;
    bottom: 3em;
    overflow: auto;
}

#form {
    padding: 0 0.5em 0 0.5em;
    margin: 0;
    position: absolute;
    bottom: 1em;
    left: 0px;
    width: 100%;
    overflow: hidden;
}

</style>
</head>
<body>
<div id="log"></div>
<form id="form">
    <input type="submit" value="Send" />
    <input type="text" id="msg" size="64"/>
</form>
</body>
</html> `
