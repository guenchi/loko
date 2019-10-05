SUBDIRS = bga-graphics etherdump hello lspci lsusb web-server

default: all

$(SUBDIRS)::
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(SUBDIRS)

all clean: $(SUBDIRS)
