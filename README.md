# Loko Scheme

Loko Scheme is an implementation of the algorithmic language R6RS
Scheme. It runs on the Linux kernel (amd64) and on bare amd64
hardware.

Loko Scheme is intended to be a platform for application and operating
system development. It is written purely in Scheme and assembler
language.

## Current status

* It works surprisingly well.
* Most of R6RS Scheme is present and accounted for.
* Most SRFIs in [chez-srfi][chez-srfi] are supported.
* Supports non-blocking I/O with lightweight concurrency.
* Memory management is limited. The Linux target makes at most 1 GB
  available (hard coded limit, will be fixed some day).
* There is no test suite.

 [chez-srfi]: https://akkuscm.org/packages/chez-srfi/

The packages in [Akku](https://akkuscm.org) should be working, but
Loko is still very new and needs to be battle tested.

## Getting Loko running

Loko can currently be bootstrapped with Chez Scheme and depends on
some packages from Akku. Follow these steps to compile Loko:

* Install [Chez Scheme](https://cisco.github.io/ChezScheme/), version
  9.5 or later, as a bootstrap compiler.
* Install the package manager [Akku.scm](https://akkuscm.org), version
  1.0.0 or later
* Run `make` to compile Loko
* Install with `make install`

Once Loko is bootstrapped you can optionally build it with itself
using `make selfcompile`.

Alternatively, get the pre-compiled binary from the GitLab CI system.
Go to [tags](https://gitlab.com/weinholt/loko/-/tags), click the
download drop-down on the right, and select the "build" artifact under
*Download artifacts* (not one of the links on the top of the
drop-down).

Another option is the Docker base image [weinholt/loko:base][docker]:
`docker run --rm -it weinholt/loko:base`. The image akkuscm/akku:loko
is also available and comes with Debian GNU/Linux and Akku
pre-installed.

 [docker]: https://cloud.docker.com/u/weinholt/repository/docker/weinholt/loko

## Running on the Linux kernel

Loko uses the environment variable `LOKO_LIBRARY_PATH` to find
libraries. This is a colon-separated list of directories. It uses the
file extensions `.loko.sls` and `.sls`. This is automatically handled
by Akku.

R6RS top-level programs can be run from the command line with `loko
--program program.sps`. If Loko is installed as `scheme-script` then
it will work with programs that use the line `#!/usr/bin/env
scheme-script`.

## Running on KVM

To get a REPL on the serial port (no echo or line editing, but it
works as an inferior Scheme for Emacs):

```sh
qemu-system-x86_64 -enable-kvm -kernel loko -m 1024 -serial stdio
```

If you create a script with this command then you can easily run it as
an "Inferior Scheme" in e.g. Emacs. There are some additional options
you can try:

* Add files to `/boot` using `-initrd filename`.
* Set environment variables with e.g. `-append
  LOKO_LIBRARY_PATH=/boot`.
* Pass command line arguments in `-append` by adding them after `--`,
  e.g. `-append 'VAR=abc -- --program foo.sps'`.

See the [samples](samples) directory for more examples.

## Compiling a program

Loko can compile R6RS top-level programs. Use this command line:

```sh
loko --compile hello.sps --output hello
```

Libraries are looked up from the `LOKO_LIBRARY_PATH` environment
variable (which is automatically set by Akku). The use of `eval` is
disabled by default to speed up builds, but can be enabled with
`-feval`:

```sh
loko -feval --compile hello.sps --output hello
```

**Note**: The command line is very inflexible.

**Note**: Loko integrates its run-time into the resulting binary and
Loko's source code needs to be available for compilation to succeed.
The location is decided by `PREFIX` when compiling Loko, but can be
overridden using the `LOKO_SOURCE` environment variable.

## Contact

* [Loko Scheme on GitLab issues](https://gitlab.com/weinholt/loko/issues)
* The IRC channel `#loko` on Freenode, but `#scheme` also works if the
  subject is about Scheme in general.
* The Usenet group comp.lang.scheme, available through any Usenet
  provider,
  e.g. [Eternal September](http://www.eternal-september.org/). I would
  like for Scheme communities to be less fragmented, so I will wait
  with setting up a separate mailing list.
* The [Loko Scheme](https://scheme.fail/) website.

## License

**Work is pending (https://gitlab.com/weinholt/loko/issues/2) to
formulate an application exception to this license. For now, there is
no exception.**

**The samples/ and srfi/ directories are covered by a different license.**

Copyright © 2019 Göran Weinholt

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
