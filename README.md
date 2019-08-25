# Loko Scheme

Loko Scheme is an implementation of the algorithmic language R6RS
Scheme. It runs on the Linux kernel (amd64) and on bare amd64
hardware.

Loko Scheme is intended to be a platform for application and operating
system development. It is written purely in Scheme and assembler
language.

## Current status

* Most of R6RS Scheme is present and accounted for.
* Most SRFIs in [chez-srfi][chez-srfi] are supported.
* Memory management is limited. The Linux target makes at most 1 GB
  available (hard coded limit, will be fixed some day).
* Very few Linux system calls are implemented. Basic I/O works, but
  may have horrible bugs and is fully blocking.
* There is no test suite.

 [chez-srfi]: https://akkuscm.org/packages/chez-srfi/

## Building on GNU/Linux

Install the package manager [Akku.scm](https://akkuscm.org) and then
run `make` followed by `make install`.

Alternatively get the pre-compiled `loko` binary from the CI system or
use the Docker base image [weinholt/loko:base][docker]: `docker run
--rm -it weinholt/loko:base`. The image akkuscm/akku:loko is also
available and comes with Debian GNU/Linux and Akku pre-installed.

 [docker]: https://cloud.docker.com/u/weinholt/repository/docker/weinholt/loko

## Building with Loko

Loko can compile itself:

```
.akku/env ./loko --program compile-loko.sps
```

## Running on the Linux kernel

Loko uses the environment variable `LOKO_LIBRARY_PATH` to find
libraries. This is a colon-separated list of directories. It uses the
file extensions `.loko.sls` and `.sls`.

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

See the [samples](samples) directory for more examples.

## Compiling a program

Loko can compile R6RS top-level programs. Use this command line:

```sh
loko --compile hello.sps --output hello
```

Libraries are looked up from the `LOKO_LIBRARY_PATH` environment
variable (which is automatically set by Akku).

**Note**: Currently you need to do chmod +x hello yourself and the
command line is very inflexible.

**Note**: Loko integrates its run-time into the resulting binary and
Loko's source code needs to be available for compilation to succeed.
The location is decided by `PREFIX` when compiling Loko, but can be
overridden using the `LOKO_SOURCE` environment variable.

## Contact

* [Loko Scheme on GitLab issues](https://gitlab.com/weinholt/loko/issues)
* The IRC channel `#loko` on Freenode.
* The Usenet group comp.lang.scheme, available through any Usenet
  provider, e.g. [Eternal September](http://www.eternal-september.org/).
* The [Loko Scheme](https://scheme.fail/) website.

## License

**Work is pending to formulate an exception to this license. For now,
there is no exception. See https://gitlab.com/weinholt/loko/issues/2.**

**The samples/ directory is covered by a different license.**

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
