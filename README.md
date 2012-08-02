distroyd
========

Distributed trading system simulation toy.


Motivation:
Build a set of components that simulate the main principles of a complete trading system.
  - a simplified instrument definition
  - an exchange simulation with a simple crossing engine, a market data source and order execution channel
  - a market data system simulation
  - an order management system simulation
  - a user click trading frontend
  - an algobox simulation
This is a pure learning exercice without any real world goal.


What's done?
  - crapy haskell code which send "order-add" messages over tcp connection, cross orders and response
  - crapy go code with lot more: cross engine, stupid ems and mkt, a web-based frontend to see market book and send order
