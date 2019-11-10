## Opening issues

No special guidelines out of the ordinary. Nobody will bite you.

## Submitting code

Before working on something large, please reach out and discuss it
first.

You retain ownership of the code you contribute. The code you
contribute is assumed to be contributed under the same license as the
project is already using, as is customary for open source and free
software projects. Other GPL-compatible licenses are also okay.

When you contribute code, use `git commit --signoff` or add the
`Signed-off-by:` line yourself. This has the same meaning as in the
Linux project, and refers to
the
[Developer Certificate of Origin](https://developercertificate.org/).

For practical reasons, your `Signed-off-by` needs to mean one more
thing. If you contribute code under the AGPL or GPL, have a look at
issue https://gitlab.com/weinholt/loko/issues/2. The intention is to
formulate an exception that would allow application developers to use
Loko as a platform, without them being required to release the source
code for their own application. This is similar to how Linux can run
proprietary applications. Your `Signed-off-by` in this project is an
agreement to apply that same exception to your contributions as will
be applied to the rest of Loko as a result of that issue.

## Where to contribute code

Use one of these options:

1. Open a merge request on GitLab (https://gitlab.com/weinholt/loko).
   This requires an account, so there are other options:

2. Use `git format-patch` and post it on comp.lang.scheme. If the
   traffic get too much we'll try to make another newsgroup.

3. Format a patch and use email (see below).

For new files, please write a copyright line and an
`SPDX-License-Identifier` at the top. You can check existing files to
see how it is done.

## Contact by email

If you don't want to open an account on GitLab or mess around
with Usenet then you can use email.

Send your bugs and patches to the `bugs` alias at the domain of the
Loko Scheme website. They will be forwarded to GitLab and will
initially be marked as confidential. You will get a confirmation when
the email has been received.
