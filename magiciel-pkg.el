;;; magiciel --- Emacs does magic and divines file contents. 
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; magiciel-pkg.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: 2024-02-14
;;
;; Version: 2024-00-10
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; The package definition of the "MAGICIEL" collection of functions and
;; tools to handle "magic" file type guessing.

;;; Code:

(define-package "MAGICIEL" "2024-05-10"
  "The Magiciel package.

The Magiciel package is an Emacs Lisp (no C! Or -- argh --
Python! Not even Rust!  Sorry, Julia) collection of functions and
tools to handle file content and type guessing in the spirit of
the UN*X facilities 'file' and 'magic'."
  )


(provide 'magiciel-pkg)

;;; magiciel-pkg.el ends here.
