# Hello World for Loko

## Dynamically loaded hello on QEMU: make run-dynamic

This program demonstrates how to use the interpreter to load code
dynamically on a bare machine.

Multiboot modules provided by `-initrd` are made available in `/boot`.
This sample is a minimal library and program that runs directly on a
PC with the help of Loko.

## Compiled hello on KVM: make run

This target compiles the hello program and runs it in QEMU.

## Compiled hello for Linux: make run-native

The same hello program can also be run on Linux with `make
run-native`. It is just a normal ELF binary that also happens to have
a multiboot header.

## Freestanding hello for Linux: make run-just-hello

There is also a freestanding program for Linux called `just-hello`,
which shows a basic program that doesn't use the standard libraries.
It only has access to the R6RS language on a syntactic level, the
primitives that are built in to the code generator and the assembler
library.

There is no stable API defined for freestanding programs. The
intention is just to assist compiler backend authors in adding
features step by step. The only code passed to the code generator is
that which exists in `just-hello.sps`.
