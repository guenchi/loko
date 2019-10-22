SUBDIRS = \
  bga-graphics \
  etherdump \
  hello \
  lspci \
  lsusb \
  ps2demo \
  web-server

default: all

$(SUBDIRS)::
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(SUBDIRS)

all clean: $(SUBDIRS)
