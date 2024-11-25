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
discussion about the `=` in front of strings (this is inferred from
the BNF description
[BNF Description](https://github.com/file/file/blob/master/src/BNF "BNF
description") on [GitHub](https://github.com).

Bottom line: an *operator* character in front of a `regex` is *quoted*
in `magic(5)` file specifications.  Therefore, `regex` "tests" must be
handled specially and "unquoted" before being used.

Oh well!


## 2024-05-16

Interesting snag.  A string gotten from a file or buffer must be
transformed into a proper **byte** string.  This is what the Emacs
Lisp reader does, but not `substring-xxx`; i.e., a hex (Emacs Lisp)
"character" represented like, say, `?\x42` (that is, the ASCII
character `'B'`) must be translated in the proper byte `1000010`,
which has length 1 and not 4.

Now, how to do this required a lot of going back and forth the manual
and, at the end, doing a `read-from-string`.  You can see this
acrobatics in `magiciel--parse-test`; I am sure they will be necessary
elsewhere as well.


## 2024-05-23

Two notes.
- The `read-from-string` bit did not work.  The result is a new file
  (cfr., `cstringizing.el`) which contains a first cut function that
  reads characters from a string (a buffer) and transforms them from a
  C-ish form to a Emacs Lisp form; they **are different**!
- The modifiers for the `type` field (and, eventually, for the `test`
  field as well) may have a more complicated shape than just `/mods`;
  hence some extra kludges in the parsign routines and in the
  instruction compilation ones.


## 2024-05-28

Checking all the entries in `/usr/share/file/magic` made me realize
that `name` and `use` entries cross magic file boundaries, cfr.,
`display-coff` names (just "grep" the folder).  This means that I cannot
use the `cl-labels` scheme and must resort to more elaborate
bookkeeping procedures.


## 2024-11-19

Much rewriting was done.  Flow of control is somewhat simpler and the
tests, especially the `regexp` one where checked and tested
(somewhat), especially on the pesky `c-lang` spec.


## 2024-11-24

The current scheme works, mostly.  Reverse-engineering the `magic(5)`
spec is quite a chore, and scouring the Internet for answers is
... not easy.  Plus, I do not want to look into the `file` code, which
may be stupid; there, I admit it.

In any case, the algorithm to run tests seems to be:
1. Run the level 0 tests; hopefully only one works (which is not
   given).
2. Run the substests, all of them, piling up the result

But, `!:mime`, `!:ext` etc., tests *exit* immediately.

Then there is the issue of the note found in the IBM documentation,
which seem to refere to a *different* `magic(5)` format, most likely
from AIX
days. https://www.ibm.com/docs/en/zos/2.4.0?topic=formats-magic-format-etcmagic-file.
So, if a subtext fails, does also the main level-0 fail?

Meanwhile, I found iteresting the Python `PolyFile` comments:
https://blog.trailofbits.com/2022/07/01/libmagic-the-blathering/

Let's see what can be done.


### More fun with `byte` type

At least in the `mach` specification it appears that `byte` must be
considered *unsigned*; that is it must be read in as an *unsigned*
byte. I make some special changes in the `parse-magic.el` file to
accommodate this, but I am pretty sure things will not be go smootly
down the road.


