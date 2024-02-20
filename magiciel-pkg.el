;;; magiciel --- EMACS works the Iron on the (IBM) Mainframe.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; magiciel-pkg.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: 2024-02-14
;;
;; Version: 2024-02-14
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; The package definition of the "MAGICIEL" collection of functions and
;; tools to handle "magic" file type guessing.

;;; Code:

(define-package "MAGICIEL" "2024-02-14"
  "The Magiciel package.

The Magiciel package is an Emacs Lisp (no C! Or, argh, Python!)
collection of functions and tools to handle file content and type
guessing in the spirit of the UN*X facilities 'file' and 'magic'."
  )


(provide 'magiciel-pkg)

;;; magiciel-pkg.el ends here.
