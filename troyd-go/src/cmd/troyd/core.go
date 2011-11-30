package main

const (
	IS_BID = true
	IS_ASK = false
)

type Order struct {
	Owner  string
	Inst   string
	IsBid  bool
	Qty    int
	Prx    int
	Filled int
}

func (o *Order) RemQty() int {
	return o.Qty - o.Filled
}
