# Notes

## 2024-01

Parsing works.  Test generation under way; there is a BNF with more
information on the `file` mirror repository, which helped a lot,
reassuring me that I was on the right path.


## 2024-03

Reworked and refactored. Compilation now is well organized and works
as advertised (almost), thanks to the switch to generic functions
(yeah, PL/I! yeah, Common Lisp!)


## 2024-05

It works!  I mean: the overall structure works.  But "magic(5)" never
stops surprising you.

I tried to test the *C language* specification (file `c-lang` in the
`magic` directory) and discovered a funny thing about the `regex` type
and test, which I believe I reverse engineered by looking at many
other `regex` entries.  The `regex` strings that start with character
that may look like one of the "numeric" operators must be quoted; that
is, the `regex` string `^at-bol` (there are no `"`'s in the `magic(5)`
format) must be written as `\^at-bol` or as `=^at-bol`, effectively
quoting the `^`.  That this is a quoting operation derives from the
discussion about the `=` in front of strings (check whether this comes
from github).

Bottom line: an *operator* character in front of a `regex` is *quoted*
in `magic(5)` file specifications.  Therefore, `regex` "tests" must be
handled specially and "unquoted" before being used.

Oh well!
