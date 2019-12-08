# Loko Scheme

Loko Scheme is an implementation of the algorithmic language R6RS
Scheme. It runs on the Linux kernel (amd64) and on bare amd64
hardware.

Loko Scheme is intended to be a platform for application and operating
system development. It is written purely in Scheme and some assembler.

## Current status

It works surprisingly well, failing just a handful of the tests from
Racket's R6RS test suite.

Memory management is limited. Processes can use at most 1 GB of RAM.

The packages in [Akku](https://akkuscm.org) should be working, but
Loko is still very new and needs to be battle tested.

## Documentation

[The Loko Scheme Developer's Manual](https://scheme.fail/manual.html)
is available online. It is also available
in [PDF format](https://scheme.fail/manual.pdf).

The manual can also be build from the Texinfo sources with `make
manual`.

## Building

See the section
[Building Loko](https://scheme.fail/manual/loko.html#Building) in
the manual.

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
