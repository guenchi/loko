SUBDIRS = \
  etherdump \
  hello \
  lspci \
  lsusb \
  ps2demo \
  video \
  web-server

default: all

$(SUBDIRS)::
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(SUBDIRS)

all clean: $(SUBDIRS)
