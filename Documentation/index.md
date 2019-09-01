# Loko Scheme documentation

## Index

* [Built-in libraries](builtins.md)
* [Concurrency](concurrency.md)
* [Roadmap](roadmap.md)
* [Tools-support in Loko](tools.md) (debugging, profiling, etc)

Loko internals:

* [Interrupts](interrupts.md)

## Target documentation

Loko supports these targets, as set in `(loko config)`:

* `amd64 linux`. Linux ELF amd64. Regular Linux binaries.
* `amd64 loko`. Multiboot amd64. These are binaries that are loaded
  by boot loaders such as GRUB 2 and SYSLINUX. QEMU's `-kernel` option
  also works.
* `amd64 loko+linux` (default). Dual-personality binaries can run
  on both Linux and from a boot loader.

The `amd64` targets are a bit unusual in that they require that the
system supports alignment checking. This allows Loko to generate fast
code for many common Scheme procedures. Some older emulators do not
support alignment checking.
