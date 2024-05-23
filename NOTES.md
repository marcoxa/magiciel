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
