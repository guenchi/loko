# Loko Scheme sample
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: MIT

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := hello

all: $(PROGRAM)

$(PROGRAM): $(PROGRAM).sps $(LOKO)
	LOKO_SOURCE=$(LOKO_SOURCE) $(LOKO) --compile $< --output $@

run: $(PROGRAM)
	qemu-system-x86_64 -enable-kvm -kernel $(PROGRAM) -m 1024 -serial stdio -debugcon vc

run-native: $(PROGRAM)
	./$(PROGRAM)

run-dynamic:
	qemu-system-x86_64 -enable-kvm -kernel $(LOKO) -m 1024 -serial stdio -debugcon vc \
	  -append 'LOKO_LIBRARY_PATH=/boot -- --program /boot/hello.sps' \
	  -initrd 'hello-lib.sls,hello.sps'

clean:
	rm -f $(PROGRAM)
