# Credits

Many learned people have indirectly or directly contributed to this
Scheme.

The syntax-case implementation is from _r6rs-libraries_ by Abdulaziz
Ghuloum and R. Kent Dybvig, with bug fixes and improvements from
Llewellyn Pritchard.

The high-level optimizer cp0 is based on the chapter _Fast and
Effective Procedure Integration_ from _Extending the Scope of
Syntactic Abstraction_ by Oscar Waddell (Ph.D. thesis).

The low-level optimizer is based on ideas I learned in a course given
by David Whalley in 2011.

The register allocator, except for the bugs, is from _Register
Allocation via Graph Coloring_ by Preston Briggs (Ph.D. thesis).

The letrec handling is from _Fixing Letrec (reloaded)_ by Abdulaziz
Ghuloum and R. Kent Dybvig.

The bignum algorithms are based on algorithms from _BigNum Math_ by
Tom St Denis.

The list? procedure uses Olin Shiver's version of Robert W. Floyd's
cycle-finding algorithm.

Some intricate parts of the records implementation are from the
reference implementation of SRFI-76 by Michael Sperber.

The list sorting code is from SLIB, was written Richard A. O'Keefe and
is based on Prolog code by David H. D. Warren.

The dynamic-wind code is from SLIB and was written by Aubrey Jaffer.

The division magic, and many other wonderful hacks, is from the
excellent book Hacker's Delight by Henry S. Warren, Jr. (foreword by
one Guy L. Steele, Jr.!)

Thanks also to Abdulaziz Ghuloum for _An Incremental Approach to
Compiler Construction_, which helped me consolidate the Scheme
compiler experience I had accumulated through experimentation.
