# The big plead to contribute to Loko Scheme

~~Ask not what Loko Scheme can do for you. Ask what YOU can do for Loko Scheme!~~

Actually, ask what Loko Scheme can do for YOU and figure out how to
make it do that! Then contribute code that makes Loko work for YOU.

My hope is that Loko will be run as an open community project. Think
something like the Linux kernel project. Not just one person holds the
copyright on Linux; thousands do.

You can ask yourself "would Linus Torvalds accept this change?" If in
doubt, just ask. But in general you can start hacking and make your
mark on Loko.

Let us make Loko into the native homeworld of Scheme!

  -- Göran Weinholt  Sat, 07 Sep 2019 11:48:44 +0200

## What is asked of you

There is no need to assign your copyright when you contribute to the
project. But the code you contribute is assumed to be contributed
under the same license as the project is already using, as is
customary for open source and free software projects. Other
GPL-compatible license are also OK, of course.

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
   This requires an account, so there is another option:

2. Use `git format-patch` and post it on comp.lang.scheme. If the
   traffic get too much we'll try to make another newsgroup.

For new files, please write a copyright line and an
`SPDX-License-Identifier` at the top. You can check existing files to
see how it is done.
