# Concurrency in Loko Scheme

The concurrency in Loko exists at two different levels:

* Loko processes, which are preemptible processes running at the same
  privilege level and in the same page table as the Loko runtime.

* Loko fibers, which are lightweight processes that run inside a Loko
  process. They are an implementation of [Concurrent ML][cml].

 [cml]: https://people.cs.uchicago.edu/~jhr/papers/cml.html

When Loko starts it immediately sets up a Loko process for the
scheduler (also called pid 0). The boot loader has already created a
heap and stack for it. The scheduler is responsible for starting pid
1, handling interrupts, managing preemption, message passing between
processes and other maintenance tasks. The first scheduler that starts
is also responsible for booting all other processors in the system.

(Currently all other processors boot up but stop after initialization.
Some work needs to be done to allocate new scheduler processes for
them).

(At least one more type of process is planned: old heavy-weight
processes with their own page tables. These will be where we put all
the FORTRAN programs. For now, this feature is missing and all code on
the system is Scheme code running in a Loko process).

## Fibers

Fibers are a lightweight concurrency system based on Concurrent ML.
The implementation is heavily inspired by a Andy Wingo's
articles [Growing Fibers][growing-fibers]
and [A New Concurrent ML][a-new-concurrent-ml]. Concurrent ML forms a
fundamental principle for concurrency that can be used to implement
higher-level concurrency like Go channels or Erlang processes.

 [growing-fibers]: https://wingolog.org/archives/2017/06/27/growing-fibers
 [a-new-concurrent-ml]: https://wingolog.org/archives/2017/06/29/a-new-concurrent-ml

For details on how to use fibers in your program,
see [Loko Scheme built-in libraries][builtins.md]. You can also
consult the [Guile fibers][fibers] manual to some extent; it has a lot
of background information.

 [fibers]: https://github.com/wingo/fibers/wiki/Manual

The implementation in Loko is different from Guile fibers in these ways:

* Loko fibers need the import `(loko system fibers)` rather than
  `(fibers)`.

* Loko fibers are based on pure `call/cc` to switch between fibers and
  the fiber scheduler. This is mostly because the Loko runtime doesn't
  have delimited continuations, but it made it easier to quickly get
  the correct semantics for parameters.

  However, basing the implementation on `call/cc` brings a space leak.
  It means that there is some extra overhead from lugging around the
  unused parts of the continuation and the dynamic winders. In some
  programs, this state can potentially grow without bound. This could
  be solved without implementing delimited continuations, but extra
  support from the runtime is needed either way.

* Loko fibers are not preemptively scheduled. This is because of
  several reasons, explained in the next section. This means that you
  shouldn't run long-running computations in a fiber that shares a
  Loko process with other fibers that need to be responsive (unless
  you explicitly yield the fiber every now and then).

* Loko fibers in a process do not run in parallel; they are not shared
  between processors. The way memory allocation is done in Loko means
  that two processors can't manage the same heap.

The API is compatible with Guile fibers, so the concurrency parts of a
program written for Guile fibers should work with Loko fibers. The
largest exception is Guile's `(fibers internals)` library which
manages fiber schedulers. Loko only has one of those per Loko process.
Another difference is that I/O is non-blocking by default on Loko.

A large part of what makes fibers attractive is that code can be
written *as if* it were non-concurrent. You're free to read and write
to pipes and network streams without explicitly dealing with polling
for when data is available or when file descriptors are ready for
writing. One of the sample programs is a tiny web-server that spawns a
fiber per connected client.

Loko on Linux takes care to set `O_NONBLOCK` on file descriptors and
suspends the current fiber when syscalls return `EAGAIN` (also called
`EWOULDBLOCK`). Loko uses epoll to find out when the file descriptor
will be ready. But Linux does not implement `EAGAIN` for regular
files, so reading from a file can block. When this happens it prevents
other fibers from running. Standard I/O is non-blocking, but other
programs that use the same terminal can either get confused by that
and/or turn it off. Even memory accesses can be blocking on Linux
because pages can be swapped out to disk. Turning off swap is not a
good idea. The binary for your program was mmap'd by Linux when it got
started and unmodified pages can be read back from disk, so Linux can
freely evict the pages from memory. So having anything like a
guaranteed responsive program on Linux is challenging. If anything
goes wrong with the disk, all your processes can end up in
uninterruptible sleep.

Loko on bare hardware has no operations that block other fibers from
running.

### Why fibers are not preemptible

Here's the excuse. Loko processes can temporarily use registers in
such a way that they contain arbitrary bit patterns. If such a
register were to be saved to a continuation object, the garbage
collector would choke on them. Other fibers in the same Loko process
use the same heap, so a fiber can't simply be suspended and left alone
as Loko processes are when they're preempted.

One common solution to this problem is that the compiler inserts
counters at various points in the code. These counters are incremented
at safe points in the code and are used to a implement software-based
timer interrupts.

This solution brings with it some overhead and needs special care to
not ruin the performance of tight loops. It may be done later unless
another solution is found.

## Loko processes

The use case for processes is pretty slim at this time, but they are
the only way to get preemptive concurrency.

Loko on Linux currently has a very rudimentary scheduler that can't
handle more than one process.
