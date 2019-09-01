# Loko Scheme built-in libraries

The complete list of built-in libraries and their exports can be found
in source code for `(loko compiler expander)`.

## (loko)

### Parameter: library-directories

This parameter is a list of strings that name directories to check
when importing libraries.

Default: `(".")`

### Parameter: library-extensions

This parameter is a list of strings with file extensions to use when
importing libraries.

Default: `(".loko.sls" ".sls" ".ss" ".scm")`

### Procedure: (installed-libraries)

For use in the repl. Returns a list of libraries.

### Procedure: (uninstall-library *name*)

For use in the repl. Uninstalls the named library.

### Procedure: (expand *expr*)

Expands the expression, returning core forms. The format of the
returned forms should not be relied on.

### Procedure: (expand/optimize *expr*)

Expands and optimizes the expression, returning core forms. The
format of the returned forms should not be relied on.

### Parameter: cp0-size-limit

### Parameter: cp0-effort-limit

### Procedure: (disassemble *procedure*)

Print the disassembly of *procedure*. Example:

```scheme
> (disassemble car)
Disassembly for #<procedure car .akku/lib/loko/libs/pairs.loko.sls:3224>

  entry:
   206E00 83F8F8       (cmp eax #xFFFFFFF8)
   206E03 0F8505000000 (jnz L0)
 ; (set! rax (car rdi))
   206E09 488B47FE     (mov rax (mem64+ rdi #x-2))
   206E0D C3           (ret)
  L0:
   206E0E E96DA2FFFF   (jmp (+ rip #x-5D93))
```

### Procedure: (machine-type)

The machine type that Loko is running on. This is a vector where the
first element is the CPU type (`amd64`) and the second is the OS
(`linux` or `loko`).

### Syntax: (time *expr*)

Run the procedure *thunk* once with no arguments and print some
numbers of memory allocation and elapsed time.

### Procedure: (time-it *what thunk*)

This is the procedural version of `time`.

### Procedure: (time-it* *what iterations thunk*)

Run *thunk* repeatedly *iterations* times and print some bogus
statistics. The aim is that this procedure should be the best way to
do micro benchmarks. Example:

```scheme
> (time-it* "fx+" 10000000 (lambda () (fx+ x 1)))
Timing fx+ to find the minimum cycle time:
New minimum is 1819 cycles with 10000000 iterations to go.
...
New minimum is 234 cycles with 6257346 iterations to go.

  The cycle count varied between 234 and 83160784
  (Arithmetic mean)      µ  = 248.75
  (Standard deviation)   σ  = 24.33
  (Population variance)  σ² = 592.08
                    min x_i = µ-.61σ
  Used 9736890 samples (263110 outliers discarded).
234
> (time-it* "+" 10000000 (lambda () (+ x 1)))
Timing + to find the minimum cycle time:
New minimum is 1751 cycles with 10000000 iterations to go.
...
New minimum is 240 cycles with 9968540 iterations to go.

  The cycle count varied between 240 and 84141254
  (Arithmetic mean)      µ  = 252.96
  (Standard deviation)   σ  = 30.46
  (Population variance)  σ² = 927.82
                    min x_i = µ-.43σ
  Used 9979862 samples (20138 outliers discarded).
240
```

Note that cp0 will optimize the thunk before it runs, so you may end
up benchmarking something other than what you thought. Check with
`expand/optimize`. If the code is entered in the REPL then you also
measure the overhead of `eval`.

Modern computers are notoriously difficult to get any consistent
results from. An improvement in cycles could be because the code
slightly moved in memory.

### Procedure: (open-output-string)

Make a new string output port that accumulates characters in memory.
The accumulated string can be extracted with `get-output-string`.

### Procedure: (get-output-string *string-output-port*)

Extract the accumulated string in *string-output-port* and reset it.
Returns the string.

### Procedure: (port-file-descriptor *port*)

Get the file descriptor associated with *port*. Returns `#f` if there
is no associated file descriptor.

### Procedure: (port-file-descriptor-set! *port fd*)

Set the file descriptor associated with *port* to *fd*.

This procedure is primarily intended to allow custom ports to have
file descriptors. It is unspecified whether changing a port's file
descriptor affects the file descriptor used for subsequent operations
on the port.

### Procedure: (gensym)

Generate an uninterned symbol.

### Procedure: (make-parameter *default-value [fender]*)

Create a new parameter object. Parameters are typically used to
implement dynamically scoped variables together with `parameterize`. A
parameter's current value can be queried by calling it with no
arguments and its value can be modified by calling it with one
argument, the new value.

The optional *fender* procedure is applied to the value whenever the
parameter is modified. The return value of *fender* is used in place
of the new value. A typical use of this procedure is to do some type
checks on the new value.

### Syntax: (parameterize ([name value] ...) body ...)

Parameterize rebinds the parameter *name* to *value* for the dynamic
extent of *body*. This means that while *body* is running, *name* will
be set to *value* (possibly filtered by a fender).

Whenever the program leaves the body, either by a normal return or a
non-local exit (such as in a `guard` expression or by calling a
continuation created by `call/cc`), the value is reset to the value it
has outside of the body. If control reenters body, as in a call to a
continuation created inside the body, the parameter will return to the
value established by `parameterize`.

Although it has the same name, this syntax is a faster variant that is
not fully compatible with SRFI-39. This variant is very common in
Scheme implementations and matches the one used in e.g. Chez Scheme.

## (loko system unsafe)

This library provides raw access to kernel services, linear memory and
I/O bus registers.

### Procedure: (syscall *n arg …*)

Calls the kernel's system call number *n* with the arguments *arg …*.
Returns a fixnum.

Example fork on Linux amd64:

```scheme
(when (zero? (syscall 57))     ; __NR_fork
  (display "child process\n")
  (exit))                      ; child become a zombie
```

Scheme programs should generally *not* do syscalls directly any less
than C programs would do the same. There are usually interactions with
the standard library that should be considered, such as flushing of
ports to prevent duplicated output.

### Procedure: (bytevector-address *bytevector*)

Get the linear address of the first byte of *bytevector*, which is
guaranteed to have an alignment of eight bytes. The linear address is
not the same as the physical address.

Note that a moving garbage collector is used for normally allocated
bytevectors (created with `make-bytevector`). There is no way to ensure
that they do not move during GC.

Returns a fixnum.

### Procedure: (get-mem-u8 *addr*)
### Procedure: (get-mem-u16 *addr*)
### Procedure: (get-mem-u32 *addr*)

Read a u8, u16 or u32, respectively, from linear address *addr* and
return it as a fixnum. If *addr* is unaligned then an exception is
raised.

The `get-mem-u32` procedure may return a bignum on targets where
`(<= (fixnum-width) 32)`, but the bus access will be 32 bits wide.

### Procedure: (get-mem-s61 *addr*)

Read and return a fixnum from linear address *addr*. If *addr* is not
evenly divisible by 8 then an exception is raised.

This procedure is only available on targets where
`(>= (fixnum-width) 61)`.

### Procedure: (put-mem-u8 *addr n*)
### Procedure: (put-mem-u16 *addr n*)
### Procedure: (put-mem-u32 *addr n*)

Write *n* as a u8, u16 or u32, respectively, to linear address *addr*.
If *addr* is unaligned then an exception is raised.

Returns unspecified values.

### Procedure: (put-mem-s61 *addr n*)

Write the fixnum *n* to linear address *addr*. If *addr* is not evenly
divisible by 8 then an exception is raised.

Returns unspecified values.

This procedure is only available on targets where
`(>= (fixnum-width) 61)`.

### Procedure: (get-i/o-u8 *busaddr*)
### Procedure: (get-i/o-u16 *busaddr*)
### Procedure: (get-i/o-u32 *busaddr*)

Read a u8, u16 or u32, respectively, from I/O bus address *busaddr*
and return it as a fixnum.

The `get-i/o-u32` procedure may return a bignum on targets where
`(<= (fixnum-width) 32)`, but the bus access will be 32 bits wide.

### Procedure: (put-i/o-u8 *busaddr n*)
### Procedure: (put-i/o-u16 *busaddr n*)
### Procedure: (put-i/o-u32 *busaddr n*)

Write *n* as a u8, u16 or u32, respectively, to I/O bus address *busaddr*.

Returns unspecified values.

### Procedure: (get-i/o-u8-n! *busaddr addr n*)
### Procedure: (get-i/o-u16-n! *busaddr addr n*)
### Procedure: (get-i/o-u32-n! *busaddr addr n*)

Read *n* units of u8, u16 or u32, respectively, from I/O bus address
*busaddr* and write them to memory starting at linear address *addr*.

Returns unspecified values.

## (loko system fibers)

Fibers are a form of lightweight concurrency based on Concurrent ML.
For an overview, see [Concurrency in Loko Scheme][concurrency.md].

### Procedure: (spawn-fiber *thunk*)

Create a new fiber that will start running *thunk*.

### Procedure: (make-channel)

Create a new channel. Channels are places where two fibers can
rendezvous to exchange a message. There is no buffering in a channel.

### Procedure: (channel? *obj*)

True if *obj* is a channel.

### Procedure: (put-message *ch obj*)

Put the message *obj* on the channel *ch*. Blocks until another fiber
picks up the message. Returns unspecified values.

### Procedure: (get-message *ch*)

Get a message from the channel *ch*. Blocks until another fiber
has arrived with a message. Returns the message.

### Procedure: (sleep *t*)

Block the fiber for *t* seconds.

### Procedure: (put-operation *ch obj*)

Returns an operation object that represents putting the message *obj*
on the channel *ch*.

### Procedure: (get-operation *ch*)

Returns an operation object that represents getting a message from the
channel *ch*.

### Procedure: (wrap-operation *op f*)

Returns an operation object that is the same as the operation *op*,
except that the values a wrapped by the procedure *f*.

### Procedure: (sleep-operation *t*)

Returns an operation object that represents waiting until *t* seconds
have passed from the time of the call to this procedure.

### Procedure: (timer-operation *a*)

Return an operation object that represents waiting until absolute time
*a* (in internal time units).

### Procedure: (choice-operation *op …*)

Returns an operation object that represents a choice between the given
operations *op …*. If multiple operations can be performed then one is
selected non-deterministically.

It is not an error to call this procedure with no arguments. It is in
fact a useful construction when gathering operations.

If `wrap-operation` is used on a choice operation then every operation
will be wrapped.

### Procedure: (perform-operation *op*)

Perform the operation *op*, possibly blocking the fiber until the
operation is ready.

With `choice-operation` and `perform-operation` it's possible to write
code that waits for one of several operations. This can be something
simple like waiting for a message with a timeout:

```scheme
(perform-operation (get-operation ch) (sleep-operation 1))
```

This will wait for a message on the channel *ch* for up to one second.
In order to distinguish between a message and a timeout,
`wrap-operation` is used:

```scheme
(perform-operation
 (choice-operation
  (wrap-operation (get-operation ch) (lambda (x) (cons 'msg x)))
  (wrap-operation (sleep-operation 1) (lambda _ 'timeout))))
```

This code will either return `(msg . x)` where *x* is the received
message; but if more than one second passes without a message it
returns `timeout`.

The object returned from `choice-operation` can be returned from a
procedure, stored in a data structure, sent over a channel, etc.

### Procedure: (make-cvar)

Make a new *condition variable* (in Concurrent ML's terminology).

### Procedure: (cvar? *obj*)

True if *obj* is a condition variable.

### Procedure: (signal-cvar! *cvar*)

Signal the condition variable *cvar*, unblocking any fibers that are
waiting for it.

### Procedure: (wait *cvar*)

Wait for the condition variable *cvar* to be signalled, blocking until
it is.

### Procedure: (wait-operation *cvar*)

Return an operation that represents waiting for the condition variable
*cvar* to be signalled.

### Procedure: (yield-current-task)

Yield the current task and and let another fiber run. This is
generally not needed in I/O-bound programs, but is provided to let
CPU-bound programs cooperate and voluntarily let other fibers run.

### Procedure: (exit-current-task)

Stops the running fiber.

### Procedure: (run-fibers *init-thunk*)

Provided for compatibility with Guile. It runs the procedure
*init-thunk* in the fibers scheduler. This procedure can return
earlier in Loko than in does in Guile. Guile provides it because
fibers are not an integrated feature in its runtime, so it needs
an entry point for when to start and stop the fibers facility.
