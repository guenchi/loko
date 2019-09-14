SUBDIRS = bga-graphics etherdump hello lspci web-server

default: all

$(SUBDIRS)::
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(SUBDIRS)

all clean: $(SUBDIRS)
