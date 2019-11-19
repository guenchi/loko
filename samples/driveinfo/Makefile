# Loko Scheme sample
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: MIT

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := driveinfo

all: $(PROGRAM)

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) --compile $< --output $@

run: $(PROGRAM) testdisk.img testdisc.iso
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 512 \
	  -debugcon vc -serial stdio \
	  -hda testdisk.img \
	  -cdrom testdisc.iso

testdisk.img: README.md
	dd if=/dev/zero of=testdisk.img bs=512 count=2880
	mformat -f 1440 -i testdisk.img
	mcopy -i testdisk.img README.md "::"

testdisc.iso: README.md
	xorrisofs -r -J -o ./testdisc.iso ./README.md

clean:
	rm -f $(PROGRAM)
