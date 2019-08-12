# Loko Scheme sample
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: MIT

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := bga-graphics

all: $(PROGRAM)

$(PROGRAM): $(PROGRAM).sps $(LOKO)
	LOKO_SOURCE=$(LOKO_SOURCE) $(LOKO) --compile $< --output $@

run: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 1024 -serial stdio \
	  -initrd 'loko.obj'

clean:
	rm -f $(PROGRAM)
