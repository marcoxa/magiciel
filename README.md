# MAGICIEL

**MAGICIEL** is an **Emacs Lisp** implementation of the `file` and
`magic` file content and type guessing machinery available on UN*X
systems.

There are two reasons for its existence.

1. Installing the **C** `dll` on Windows is a P.I.T.A. for my
   standards.
2. An **Emacs Lisp** (and, next, a **Common Lisp**) version is useful
   per se.
3. Why not doing it?  It is just one of the many rabbit holes I fall
   into.
4. You do not want to know which rabbit hole led to this one.
5. Did I say there were *two* reasons?


## Goals

The final version (available "practically now" (tm)) will allow you to
get information about a file (and MIME information) directly from
**Emacs Lisp**.

Also, the library will allow you to load and compile `magic(5)` files
in a modular way.


### Current Status

The code in the library implements a simple check of file MIME type
using an internal "data base" built on the basis of the **Javascript**
`mime-db` package (cf.,
[`mime-db` Github](https://github.com/jshttp/mime-db) and
[`mime-db` **npm**](https://www.npmjs.com/package/mime-db)).  The
internal "data base" (see file `magiciel-mime-db.el`) is built using
`json-read-file` to import the `mime-db` `db.json` file, dumping it in
the `magiciel-mime-data.el` file, and using a manually curated
`magiciel-mime-local-data.el` that contains some extra definitions
that I felt are needed.

The library also implements a full blown (and surely brittle)
`magic(5)` parser and "compiler".  The results of the compilation and
held in the `apothecary` sub-directory; each piece of code checks for
a particular `magic(5)` entry.


# Installing and Using MAGICIEL

To use the package, just install the folder `magiciel` in your Emacs
setup and ensure that the file `magiciel.el` is loaded.  Eventually, it
may get into `elpa` and/or `melpa`.


## Using MAGICIEL

The main command (and function) to use are `magiciel-guess` and
`magiciel-divine`, described below.

```elisp
(magiciel-guess FILENAME &key (DIVINE nil))
```

The "mime type" of *FILENAME* will be looked up using its extension.
If no extension is found and *DIVINE* is `NIL` the function will
return `"application/octet-stream"`; if *DIVINE* is non-`NIL` **and**
checking by the extension is uninformative, the system will try harder
by invoking `magiciel-divine`.



```elisp
(magiciel-divine FILENAME &optional (CONTEXT :normal)) 
```

The type of *FILENAME* will be divined by the call.

*CONTEXT* can be either `:normal` or `!:normal`, in which case the
function will return a description of the file content (as per the
simple use of the UN\*X `file` command; or *CONTEXT* can be `!:mime`
in which case it will return the MIME type of the file (or
`"application/octet-stream"` or `"data"` as last resort).  Finally,
*CONTEXT* can be `!:ext`, in which case the function will return the
most likely extension for *FILENAME*, by querying the magic spells
(i.e., not by looking at the filename extension).

If called interactively, the command will default to `:normal`
*CONTEXT*, unless invoked with a prefix (use the `?d` choice for
`:normal` operation in that case).



# Nice to have...

- Eventually, it'd be nice to have a *sexpy* `magic` format.
- Integration with `dired` et similia, is also a desirable goal.
- leveraging the registry under Windows is also desirable.


# An Overview of the `magic(5)` "Compilation" Flow

The flow is essentially the following.

1. A `magic(5)` file is read line by line.
2. A `line` is parsed in a `magiciel--magic-record` by the function
   `magiciel--parse-magic-record` (in file `parse-magic.el`).
3. A `magiciel--magic-record` is "compiled" in a
   `magiciel--instruction` by the function `magiciel--compile-record`,
   which is actually called by `magiciel-compile-magic`; the last
   function does also other things like organizing the *test tree*
   implied in the `magic(5)` specifications (in file
   `compile-magic.el`). 
4. The function `magiciel--compile-record` is just a wrapper around
   `magiciel--compile-instr` which generates `magiciel--instruction`s.
5. The `magiciel-instruction`s generated are then *emitted* by
   `magiciel--emit` (always in file `compile-magic.el`), which is
   called by `magiciel--group-emit` from `magiciel--magic-emit`.



## A NOTE ON FORKING

Of course you are free to fork the project subject to the current
licensing scheme.  However, before you do so, I ask you to consider
plain old "cooperation" by asking me to become a developer.
It helps keeping the entropy level at an acceptable level.



Enjoy

Marco Antoniotti, Milan, Italy, (c) 2024-2025
