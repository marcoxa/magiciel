;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; try-magic.el


;;; Commentary:
;;
;; Some tests for the `magiciel' library.

;;; Code:

(require 'cl-lib)
(require 'magiciel-parse-magic)
(require 'magiciel-compile-magic)

;; Records.

(defvar magiciel--r1 "0	string	foo	Record 1 parsed.")

(defvar magiciel--r2 "&0	string	foo	Record 2 parsed.")

(defvar magiciel--r3 "&0x2a	string	foo	Record 3 parsed.")

(defvar magiciel--r4 "(0x2a)	string	foo	Record 4 parsed.")

(defvar magiciel--r5 "(&0x2a)	string	foo	Record 5 parsed.")

(defvar magiciel--r6 "(&0x2a.s)	string	foo	Record 6 parsed.")

(defvar magiciel--r7 "(&0x2a.s*512)	string	foo	Record 7 parsed.")

(defvar magiciel--r8 "(&0x2a.s+(-512))	string	foo	Record 8 parsed.")


;; Records from magic(5) man page.

(defvar magiciel--mr1 "0	string	MZ")

(defvar magiciel--mr2 ">0x18	leshort	<0x40	MS-DOS executable")

(defvar magiciel--mr3 ">0x18	leshort	>0x3f	extended PC executable (e.g., MS Windows)")

(defvar magiciel--mr4 ">>(0x3c.l)	string	PE\0\0	PE executable (MS-Windows)")

(defvar magiciel--mr5 ">>(0x3c.l)	string	LX\0\0	LX executable (OS/2)")

(defvar magiciel--mr6 ">>(4.s*512)	leshort	0x014c	COFF executable (MS-DOS, DJGPP)")

(defvar magiciel--mr7 ">>(4.s*512)	leshort	!0x014c	MZ executable (MS-DOS)")

(defvar magiciel--mr8 ">>(0x3c.l)	string	PE\0\0	PE executable (MS-Windows)")

(defvar magiciel--mr9 ">>>&0	leshort	0x14c	for Intel 80386")

(defvar magiciel--mr10 ">>>&0	leshort	0x184	for DEC Alpha")

(defvar magiciel--mr11 ">>(4.s*512)	leshort	!0x014c	MZ executable (MS-DOS)")

(defvar magiciel--mr12 ">>>&(2.s-514)	string	LE	LE executable (MS Windows VxD driver)")


;; Parsed records.

(defvar magiciel--pr1 (magiciel--parse-magic-record magiciel--r1))

(defvar magiciel--pr2 (magiciel--parse-magic-record magiciel--r2))

(defvar magiciel--pr3 (magiciel--parse-magic-record magiciel--r3))

(defvar magiciel--pr4 (magiciel--parse-magic-record magiciel--r4))

(defvar magiciel--pr5 (magiciel--parse-magic-record magiciel--r5))

(defvar magiciel--pr6 (magiciel--parse-magic-record magiciel--r6))

(defvar magiciel--pr7 (magiciel--parse-magic-record magiciel--r7))

(defvar magiciel--pr8 (magiciel--parse-magic-record magiciel--r8))


;; Parsed records from man page.

(defvar magiciel--pmr1 (magiciel--parse-magic-record magiciel--mr1))

(defvar magiciel--pmr2 (magiciel--parse-magic-record magiciel--mr2))

(defvar magiciel--pmr3 (magiciel--parse-magic-record magiciel--mr3))

(defvar magiciel--pmr4 (magiciel--parse-magic-record magiciel--mr4))

(defvar magiciel--pmr5 (magiciel--parse-magic-record magiciel--mr5))

(defvar magiciel--pmr6 (magiciel--parse-magic-record magiciel--mr6))

(defvar magiciel--pmr7 (magiciel--parse-magic-record magiciel--mr7))

(defvar magiciel--pmr8 (magiciel--parse-magic-record magiciel--mr8))

(defvar magiciel--pmr9 (magiciel--parse-magic-record magiciel--mr9))

(defvar magiciel--pmr10 (magiciel--parse-magic-record magiciel--mr10))

(defvar magiciel--pmr11 (magiciel--parse-magic-record magiciel--mr11))

(defvar magiciel--pmr12 (magiciel--parse-magic-record magiciel--mr12))

;;; try-magic.el ends here
