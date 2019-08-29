# Tools-supports in Loko

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

Scheme objects can get very big. Limit the output with e.g. `set print
elements 5`.

Traps from the processor are translated into conditions with a
&program-counter condition, e.g.:

```scheme
 1. &assertion &violation &serious
 2. &who
     who: apply
 3. &message
     message: "A non-procedure object was called"
 4. &irritants
     irritants: (#f)
 5. &program-counter
     program-counter: 2380563
```

You can translate the program counter value to hex and look up the
trapping instruction with a disassembler.

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
