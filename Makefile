# SPDX-License-Identifier: AGPL-3.0-or-later
# Loko Scheme - an R6RS Scheme compiler
# Copyright © 2019 Göran Weinholt

LINUX_SOURCE := /lib/modules/$(shell uname -r)/source

DESTDIR=
PREFIX=/usr/local
INSTALL=install
INSTALLINFO=install-info

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
	LOKO_SOURCE=.akku/lib .akku/env ./loko -feval --compile loko.sps --output loko.out
	if [ -f loko ]; then mv -f loko loko.old; fi
	mv -f loko.out loko

munchausen: .akku/env loko
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

manual:: Documentation/manual/loko.info Documentation/manual/loko.html Documentation/manual/loko.pdf

Documentation/manual/loko.info: Documentation/manual/*.texi Documentation/manual/version.texi
	cd Documentation/manual && makeinfo loko

Documentation/manual/loko.html: Documentation/manual/*.texi Documentation/manual/version.texi
	cd Documentation/manual && makeinfo --no-split --html loko

Documentation/manual/loko.pdf: Documentation/manual/*.texi Documentation/manual/version.texi
	cd Documentation/manual && texi2pdf loko.texi

Documentation/manual/version.texi: Documentation/manual/mkversion.sps loko Akku.manifest
	cd Documentation/manual && ../../.akku/env ../../loko --program mkversion.sps

samples:: loko
	$(MAKE) -C samples

clean:
	rm -f header-snarfer.c header-snarfer
	rm -f loko loko.out
	rm -f scheme-script scheme-script.old
	rm -f config.sls
	rm -f Documentation/manual/version.texi Documentation/manual/loko*.info Documentation/manual/loko.pdf
	$(MAKE) -C samples clean

install: all
	$(INSTALL) -m 0755 -d   $(DESTDIR)$(PREFIX)/bin
	$(INSTALL) -m 0755 loko $(DESTDIR)$(PREFIX)/bin
# Libraries for users
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64
	$(INSTALL) -m 0644 arch/amd64/linux-numbers.sls  $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64
	$(INSTALL) -m 0644 arch/amd64/linux-syscalls.sls $(DESTDIR)$(PREFIX)/share/r6rs/loko/arch/amd64
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/keyboard.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/keymaps.sls           $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/mouse.sls             $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0644 drivers/storage.sls           $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers

	$(INSTALL) -m 0644 drivers/pci.sls               $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/pci
	$(INSTALL) -m 0644 drivers/pci/roms.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/pci

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/atapi.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/core.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/drive.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/ide.sls           $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata
	$(INSTALL) -m 0644 drivers/ata/identify.sls      $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ata

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/scsi
	$(INSTALL) -m 0644 drivers/scsi/core.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/scsi
	$(INSTALL) -m 0644 drivers/scsi/block.sls        $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/scsi

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/video
	$(INSTALL) -m 0644 drivers/video/bga.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/video
	$(INSTALL) -m 0644 drivers/video/vbe.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/video

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/core.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/hid-numbers.sls   $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/hub.sls           $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb
	$(INSTALL) -m 0644 drivers/usb/uhci.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/usb

	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ps2
	$(INSTALL) -m 0644 drivers/ps2/core.sls          $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ps2
	$(INSTALL) -m 0644 drivers/ps2/i8042.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ps2
	$(INSTALL) -m 0644 drivers/ps2/keyboard.sls      $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ps2
	$(INSTALL) -m 0644 drivers/ps2/mouse.sls         $(DESTDIR)$(PREFIX)/share/r6rs/loko/drivers/ps2

	$(INSTALL) -m 0644 lib/match.sls                 $(DESTDIR)$(PREFIX)/share/r6rs/loko
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/srfi
	$(INSTALL) -m 0644 srfi/170.loko.sls             $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a170.loko.sls
	$(INSTALL) -m 0755 -d                            $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a170
	$(INSTALL) -m 0644 srfi/170/posix.loko.sls       $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a170
	$(INSTALL) -m 0644 srfi/170/linux.loko.sls       $(DESTDIR)$(PREFIX)/share/r6rs/srfi/%3a170/compat.loko.sls
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

install-info: Documentation/manual/loko.info
	mkdir -p $(DESTDIR)$(PREFIX)/share/info
	$(INSTALL) -m 0644 Documentation/manual/loko.info* \
	  $(DESTDIR)$(PREFIX)/share/info
	$(INSTALLINFO) --info-dir='$(DESTDIR)$(PREFIX)/share/info' \
	  '$(DESTDIR)$(PREFIX)/share/info/loko.info'

install-all: install install-info
	ln -f $(DESTDIR)$(PREFIX)/bin/loko $(DESTDIR)$(PREFIX)/bin/scheme-script

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/loko
	rm -rf $(DESTDIR)$(PREFIX)/share/loko
	rm -rf $(DESTDIR)$(PREFIX)/lib/loko
	$(INSTALLINFO) --info-dir='$(DESTDIR)$(PREFIX)/share/info' \
	  --delete '$(DESTDIR)$(PREFIX)/share/info/loko.info' || true
	rm -f $(DESTDIR)$(PREFIX)/share/info/loko.info*
