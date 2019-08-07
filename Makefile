# SPDX-License-Identifier: AGPL-3.0-or-later
# Loko Scheme - an R6RS Scheme compiler
# Copyright © 2019 Göran Weinholt

LINUX_SOURCE := /lib/modules/$(shell uname -r)/source

all: loko scheme-script

.akku/env:
	akku install

loko: .akku/env
	.akku/env scheme --program compile-loko.sps
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

clean:
	/bin/rm -f header-snarfer.c header-snarfer \
	  loko loko.out \
	  scheme-script scheme-script.old
