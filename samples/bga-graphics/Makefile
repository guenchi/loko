# Loko Scheme sample
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: MIT

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := bga-graphics

all: $(PROGRAM)

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) -ftarget=pc --compile $< --output $@

run: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 1024 -serial stdio -debugcon vc \
	  -initrd 'loko.obj'

clean:
	rm -f $(PROGRAM)
