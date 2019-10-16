# Loko Scheme documentation

## Index

* [Built-in libraries](builtins.md)
* [Concurrency](concurrency.md)
* [Roadmap](roadmap.md)
* [Tools-support in Loko](tools.md) (debugging, profiling, etc)

Loko internals:

* [Interrupts](interrupts.md)

## Available libraries

The standard [R6RS Scheme](https://r6rs.org) libraries are provided.
Please see the documents on the website for the official versions.
Unofficial versions
of [R6RS updated with errata](https://weinholt.se/scheme/r6rs/)
versions are also available online.

These libraries are documented in [Built-in libraries](builtins.md):

* `(loko)` -- built-in base library
* `(loko system unsafe)` -- unsafe primitives
* `(loko system fibers)` -- lightweight concurrency

These SRFIs are provided with Loko (see the Akku package chez-srfi
for most other SRFIs):

* `(srfi :98 os-environment-variables)` -- read-only access to
  environment variables
* `(srfi :170 posix)` -- access to common POSIX operations (currently
  only available on Linux), see [SRFI 170][srfi-170]

 [srfi-98]: https://srfi.schemers.org/srfi-98/srfi-98.html
 [srfi-170]: https://srfi.schemers.org/srfi-170/srfi-170.html

The following Linux-specific libraries are provided. Their usage
mirrors 1:1 the functionality in the Linux manpages, section 2.

* `(loko arch amd64 linux-numbers)` -- constants used in the Linux
  syscall interface (UAPI)
* `(loko arch amd64 linux-syscalls)` -- thin wrappers around Linux
  syscalls

Loko comes with some drivers for hardware. These are currently
undocumented and should be expected to not have stable APIs for a
while.

## Summary of supported targets

Loko currently supports these targets, as set in `(loko config)`:

* `amd64 linux`. Linux ELF amd64. Regular Linux binaries.
* `amd64 pc`. Multiboot amd64. These are binaries that are loaded
  by boot loaders such as GRUB 2 and SYSLINUX. QEMU's `-kernel` option
  also works.
* `amd64 pc+linux` (default). Dual-personality binaries can run
  on both Linux and from a boot loader.

The `amd64` targets are a bit unusual in that they require that the
system supports alignment checking. This allows Loko to generate fast
code for many common Scheme procedures. Some older emulators do not
support alignment checking.
