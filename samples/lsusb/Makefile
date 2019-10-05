# Loko Scheme sample
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: MIT

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := lsusb

all: $(PROGRAM)

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) --compile $< --output $@

run: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -usb \
	  -device usb-kbd -device usb-mouse \
	  -s

clean:
	rm -f $(PROGRAM)
