# Loko Scheme sample
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: MIT

LOKO := ../../loko
LOKO_SOURCE := ../../.akku/lib

PROGRAM := web-server

all: $(PROGRAM)

.akku/env:
	akku install

$(PROGRAM): $(PROGRAM).sps $(LOKO) .akku/env
	LOKO_SOURCE=$(LOKO_SOURCE) .akku/env $(LOKO) --compile $< --output $@

run: $(PROGRAM)
	./$(PROGRAM)

run-dynamic:
	$(LOKO) --program $(PROGRAM).sps

clean:
	rm -f $(PROGRAM)
