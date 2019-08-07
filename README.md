# Loko Scheme

Loko Scheme is an implementation of the algorithmic language R6RS
Scheme. It runs on the Linux kernel (amd64) and on bare amd64
hardware.

Loko Scheme is intended to be a platform for application and operating
system development. It is written purely in Scheme and assembler
language.

## Current status

* Most of R6RS Scheme is present and accounted for.
* No SRFIs apart from SRFI 98 are implemented. (In most cases SRFIs
  will not be added directly, but the support that is needed to use
  them via [chez-srfi][chez-srfi] will be provided instead).
* Memory management is limited. The Linux target makes at most 1 GB
  available (hard coded limit, will be fixed some day).
* Very few Linux system calls are implemented. Basic I/O works, but
  may have horrible bugs and is fully blocking.
* There is no test suite.

 [chez-srfi]: https://akkuscm.org/packages/chez-srfi/

## Building on GNU/Linux

Install the package manager [Akku.scm](https://akkuscm.org) and run
`akku install` in a checked out copy of the repository, followed by
`./build.sh`.

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

To get a REPL on the serial port:

```sh
qemu-system-x86_64 -enable-kvm -kernel loko -m 1024 -serial stdio
```

Or to get a basic REPL on the VGA text console and on the serial port
(Ctrl+Alt+3):

```sh
qemu-system-x86_64 -enable-kvm -kernel loko -m 1024 -append '-- --ide'
```

See the [samples](samples) directory for more advanced examples.

## Contact

* [Loko Scheme on GitLab issues](https://gitlab.com/weinholt/loko/issues)
* The IRC channel `#loko` on Freenode.
* The Usenet group comp.lang.scheme, available through any Usenet
  provider, e.g. [Eternal September](http://www.eternal-september.org/).

## License

**Work is pending to formulate an exception to this license. For now,
there is no exception. See https://gitlab.com/weinholt/loko/issues/2.**

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
