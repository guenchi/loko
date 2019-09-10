# Loko Scheme sample
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: MIT

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := etherdump

all: $(PROGRAM)

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) -feval --compile $< --output $@

run: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -net nic,model=rtl8139 \
	  -net tap,ifname=tap0,script=no,downscript=no \
	  -append 'LOKO_LIBRARY_PATH=/boot/.akku/lib' \
	  -initrd .akku/lib/text-mode/terminfo/builtins.sls

clean:
	rm -f $(PROGRAM)
