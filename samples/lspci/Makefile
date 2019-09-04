# Loko Scheme sample
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: MIT

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := lspci

all: $(PROGRAM) pci.ids

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO)
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) --compile $< --output $@

pci.ids:
	if [ -f /usr/share/misc/pci.ids ]; then \
	  ln -s /usr/share/misc/pci.ids .; \
	else \
	  curl -O "https://pci-ids.ucw.cz/v2.2/pci.ids"; \
	fi

run: $(PROGRAM) pci.ids
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -usb \
	  -watchdog i6300esb \
	  -device rtl8139 \
	  -device virtio-net-pci \
	  -soundhw hda \
	  -initrd pci.ids

clean:
	rm -f $(PROGRAM) pci.ids
