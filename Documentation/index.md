# Loko Scheme documentation

## Current target support

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

## Disassembly

Loko can be disassembled using any regular disassembler that supports
ELF and amd64. There is also a built-in disassembler for procedures
(the `disassemble` procedure in the `(loko)` library).

## Debugging

Loko can be debugged with [GNU gdb][gdb] with the help of
`loko-gdb.py`. Stack traces and pretty printing should work. Line
information is currently missing.

There is a bug in the stack tracing support that needs manual
intervention before usage, namely that gdb is not finding the symbol
`bootstrap_unwind_table`. Update `loko-gdb.py` if stack traces don't
seem to make any sense.

 [gdb]: https://www.gnu.org/software/gdb/

## Profiling

Loko can be profiled with Linux [perf][perf].

 [perf]: https://perf.wiki.kernel.org/index.php/Main_Page

## Memory checking

It's possible to run Loko in [Valgrind][valgrind]. Valgrind does not
support alignment checking, so Loko will print a warning about that.
Loko also does not use the *red zone*, so Valgrind will think that a
lot of what Loko is doing uses uninitialized memory. Don't believe it.

 [valgrind]: http://valgrind.org/

## Fuzzing

Loko can be run with [AFL][afl]. This requires an instrumented binary,
which has some overhead. The support needs to be enabled in `(loko
arch amd64 codegen)` first. Change `use-branch-instrumentation` to
`#t` and recompile.

 [afl]: http://lcamtuf.coredump.cx/afl/

## The mandatory wishlist (or overambitious plan for growth)

It is going to depend on what people want to work on, but Loko will
likely be going in two directions: a platform for running programs on
Linux (and other kernels if people add support), and as a platform for
an operating system kernel.

The use cases for Loko can grow forever and become quite variegated.
It's fairly important that the project doesn't become a mess. Some of
the ideas you will be having are going to be good additions to the
core Loko project, some are their own independent projects, and some
can live as separate projects that are pulled in as dependencies.

There's not going to be a very hard line drawn most of the time. A
discussion could go on forever about definitions of, merits of and
demerits of monolithic kernels, microkernels, unikernels and so on.
Classic discussions on these topics have not tended to include the
consideration that the kernel is written in a safe language like R6RS
Scheme (although exceptions do exist). We can have ideas, but let's
see what Loko grows to become.

For the long-term manageability of the project it is likely better if
things that are beasts in their own right are managed under their own
umbrellas. These can and should be packaged
in [Akku.scm](https://akkuscm.org/) so that they are easily pulled
in when building.

It is also conceivable that the project of making a fully featured
kernel around Loko will be another project that pulls in Loko as a
dependency. This would be similar to how Linux uses GCC, but also
different since Loko will be providing the basic architecture support.
And there's nothing to say that only one kernel should be developed in
this way. It really only depends on what people want to do. (Or not
do, as the case may be).

### Desirable core features

One fairly good guideline when adding a feature to the core of Loko is
that it should work more or less the same across all targets.

* Easy compilation of user programs. Currently the only way to compile
  something with Loko is to modify the `compile-loko.sps` script. It
  would be better if normal R6RS programs could be compiled with
  something like `loko --compile script.sps`.

* Separate compilation. As Loko and applications grow it will become
  untenable to compile whole programs in one pass. The psyntax library
  has hooks ready for saving compiled libraries.

* Online compilation. The REPL currently uses a simple tree
  interpreter to run code. The compiler is *almost* hooked up though.
  All the libraries are present, but it needs some TLC to allocate
  memory for new code and rodata pages, to hook up the unwinding
  tables, and so on.

* Loko should be able to run virtual memory processes. These are
  processes with their own page table, like on a traditional OS. This
  is fairly useful since it means that not all software has to be
  rewritten to run on Loko.

* Loko needs a scheduler and inter-process communication.

* Loko needs large improvements to its memory management. Currently
  Loko allocates a statically sized heap and stack to each internal
  process.

* Epoll()-based green threads, isolated processes, fibers or similar
  for the Linux target. Could be based on the same ideas that are in
  Swish. Most Scheme code is written to run single-threaded and it
  would be good to keep this code working and useful. Threads with
  shared data are a big source of bugs. It would be really good if
  threads were not needed or were delegated to only those use cases
  where they are truly necessary.

* Improvements to the compiler are always good. As in Chez Scheme, it
  would be preferable if optimizations "paid their way". Work to do
  here includes a conversion to the nanopass framework, a real
  implementation of multiple values, a better type analysis pass (such
  as _Flow-Sensitive Type Recovery in Linear-Log Time_), and
  improvements to the low-level optimizer (particularly the register
  allocator).

* The `loko` target needs full multicore support. Currently all
  Application Processors (APs) are booted, but then stop just short of
  running a scheduler process. The next steps are basically to
  allocate memory to the APs so that they can each run a scheduler
  process. After that the next step is some way to distribute the
  system's workload between cores and to implement intra-core
  communication.

* The `linux` target needs support for threads in order to utilize all
  cores in a system, and in order to work the same as the `loko`
  target on a multicore system.

* The `loko` target should be able to use an early file system that is
  passed to it from the boot loader. This will contain drivers and
  programs needed to get the full system up and running.
