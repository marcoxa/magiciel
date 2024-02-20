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


(defvar magiciel--pr1 (magiciel--parse-magic-record magiciel--r1))

(defvar magiciel--pr2 (magiciel--parse-magic-record magiciel--r2))

(defvar magiciel--pr3 (magiciel--parse-magic-record magiciel--r3))

(defvar magiciel--pr4 (magiciel--parse-magic-record magiciel--r4))

(defvar magiciel--pr5 (magiciel--parse-magic-record magiciel--r5))

(defvar magiciel--pr6 (magiciel--parse-magic-record magiciel--r6))

(defvar magiciel--pr7 (magiciel--parse-magic-record magiciel--r7))

(defvar magiciel--pr8 (magiciel--parse-magic-record magiciel--r8))


;;; try-magic.el ends here
