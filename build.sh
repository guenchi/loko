#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# Loko Scheme - an R6RS Scheme compiler
# Copyright © 2019 Göran Weinholt

set -e
#set -o pipefail

# first: akku install

if [ ! -f arch/amd64/linux-numbers.sls ]; then
    if [ -d "/lib/modules/$(uname -r)/source/include/uapi" ]; then
        (cat "/lib/modules/$(uname -r)/source/include/uapi/asm-generic/errno"* | \
             scheme --program tools/header-snarfer.sps loko arch > header-snarfer.c && \
             gcc -Wall header-snarfer.c -o header-snarfer && \
             ./header-snarfer > arch/amd64/linux-numbers.sls && \
             /bin/rm -f header-snarfer.c header-snarfer) || true
    fi
fi

eval $(.akku/env -s)
rm -f loko.out
scheme --program ./compile-loko.sps
chmod +x loko.out
[ -f loko ] && mv -f loko loko.old
mv -f loko.out loko
[ -f scheme-script ] && mv -f scheme-script scheme-script.old
ln loko scheme-script
