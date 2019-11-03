# Loko Scheme sample
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: MIT

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := bga-graphics

all: $(PROGRAM) vbe-graphics

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) -ftarget=pc --compile $< --output $@

vbe-graphics: vbe-graphics.sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) -ftarget=pc+linux -feval --compile $< --output $@

run: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 1024 -serial stdio -debugcon vc \
	  -initrd 'loko.obj' \
	  -append 'LOKO_LIBRARY_PATH=/boot/.akku/lib CONSOLE=com1'

run-vbe: vbe-graphics
	qemu-system-x86_64 -enable-kvm -kernel vbe-graphics -m 1024 -serial stdio -debugcon vc \
	  -initrd 'loko.obj,.akku/lib/zabavno/cpu/x86.sls,.akku/lib/zabavno/cpu/compat.sls,.akku/lib/zabavno/cpu/x86-utils.sls' \
	  -append 'LOKO_LIBRARY_PATH=/boot/.akku/lib CONSOLE=com1'

clean:
	rm -f $(PROGRAM) vbe-graphics
