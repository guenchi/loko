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

Default: `(".loko.sls" ".sls")`

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

Print the disassembly of *procedure*.

### Procedure: (machine-type)

The machine type that Loko is running on. This is a vector where the
first element is the CPU type (`amd64`) and the second is the OS
(`linux` or `loko`).

### Procedure: (time *thunk*)

### Procedure: (time-it *what thunk*)

### Procedure: (time-it* *what iterations thunk*)

### Procedure: (open-output-string)

Make a new string output port that accumulates characters in memory.
The accumulated string can be extracted with `get-output-string`.

### Procedure: (get-output-string *string-output-port*)

Extract the accumulated string in *string-output-port* and reset it.
Returns the string.

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
