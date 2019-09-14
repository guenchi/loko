# SPDX-License-Identifier: AGPL-3.0-or-later
# Loko Scheme - an R6RS Scheme compiler
# Copyright © 2019 Göran Weinholt

LINUX_SOURCE := /lib/modules/$(shell uname -r)/source

DESTDIR=
PREFIX=/usr/local
INSTALL=install

do_subst = sed -e 's,[@]PREFIX[@],$(PREFIX),g'

all: loko scheme-script

config.sls: config.sls.in
	$(do_subst) < config.sls.in > config.sls

.akku/env: config.sls
	akku install

loko: .akku/env
	LOKO_SOURCE=.akku/lib .akku/env scheme --program compile-loko.sps
	chmod +x loko.out
	if [ -f loko ]; then mv -f loko loko.old; fi
	mv -f loko.out loko

selfcompile: .akku/env loko
	LOKO_SOURCE=.akku/lib .akku/env ./loko --program compile-loko.sps
	chmod +x loko.out
	if [ -f loko ]; then mv -f loko loko.old; fi
	mv -f loko.out loko

scheme-script: loko
	if [ -f scheme-script ]; then mv -f scheme-script scheme-script.old; fi
	ln loko scheme-script

header-snarfer.c: tools/header-snarfer.sps
	cat $(LINUX_SOURCE)/include/uapi/asm-generic/errno* | \
	  scheme --program $< loko arch > $@

header-snarfer: header-snarfer.c
	$(CC) -Wall header-snarfer.c -o header-snarfer

arch/amd64/linux-numbers.sls: header-snarfer
	./header-snarfer > arch/amd64/linux-numbers.sls

samples:: loko
	$(MAKE) -C samples

clean:
	rm -f header-snarfer.c header-snarfer
	rm -f loko loko.out
	rm -f scheme-script scheme-script.old
	rm -f config.sls
	$(MAKE) -C samples clean

install: all
	$(INSTALL) -m 0755 -d   $(DESTDIR)$(PREFIX)/bin
	$(INSTALL) -m 0755 loko $(DESTDIR)$(PREFIX)/bin
# Libraries for users
	$(INSTALL) -m 0755 -d             $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64
	$(INSTALL) -m 0644 arch/amd64/linux-numbers.sls  $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64
	$(INSTALL) -m 0644 arch/amd64/linux-syscalls.sls $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64
# Libraries needed when compiling programs
	(cd .akku/lib; find * -type d | \
	  while read fn; do \
	    $(INSTALL) -m 0755 -d $(DESTDIR)$(PREFIX)/lib/loko/$$fn; \
	  done)
# FIXME: Install only those libraries used by (loko compiler static).
	(cd .akku/lib; find * ! -type d -a \
            ! \( -name '*.chezscheme.sls' -o -name '*.ikarus.sls' \) | \
	  while read fn; do \
	    $(INSTALL) -m 0644 $$fn $(DESTDIR)$(PREFIX)/lib/loko/$$fn; \
	  done)

install-all: install
	ln -f $(DESTDIR)$(PREFIX)/bin/loko $(DESTDIR)$(PREFIX)/bin/scheme-script

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/loko
	rm -rf $(DESTDIR)$(PREFIX)/share/loko
	rm -rf $(DESTDIR)$(PREFIX)/lib/loko
