#!/bin/sh
# Loko Scheme sample
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: MIT

LOKO=${LOKO:-../../loko}
qemu-system-x86_64 -enable-kvm -kernel "$LOKO" -m 1024 -serial stdio \
  -append 'LOKO_LIBRARY_PATH=/boot -- --program /boot/hello.sps' \
  -initrd 'hello-lib.sls,hello.sps'
