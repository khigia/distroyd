APP=troyd

run: $(APP)
	./$(APP)
 
include $(GOROOT)/src/Make.inc

TARG=$(APP)
GOFILES=\
    core.go\
    cross.go\
    ems.go\
    mds.go\
    web.go\
    main.go\

include $(GOROOT)/src/Make.cmd

