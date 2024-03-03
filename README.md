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

The final version (available "very soon now" (tm)) will allow you to
get information about a file (and MIME information) directly from
**Emacs Lisp**.

Also, the library will allow you to load and compile `magic(5)` files
in a modular way.


# Installing and Using MAGICIEL

To use the package, just install the folder `magiciel` in your Emacs
setup and ensure that the file `magiciel.el` is loaded.  Eventually, it
will get into `melpa`.


# Nice to have...

Eventually, it'd be nice to have a *sexpy* `magic` format.


## A NOTE ON FORKING

Of course you are free to fork the project subject to the current
licensing scheme.  However, before you do so, I ask you to consider
plain old "cooperation" by asking me to become a developer.
It helps keeping the entropy level at an acceptable level.



Enjoy

Marco Antoniotti, Milan, Italy, (c) 2024
