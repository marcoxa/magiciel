;;; magiciel.el --- Guessing the content of a file in the guise of `file' `magic'. -*- lexical-binding: t -*-
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Maintainer: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Summary: Determine the content of a file (as per the `file' command).
;;
;; Created: 2024-02-14
;;
;; Version: 2025-10-16
;;
;; Keywords: languages, operating systems, file systems.

;;; Commentary:
;;
;; Get mime types using the "file" command on UN*X and querying the
;; registry on Windows.
;;
;; ... or, get the same by implementing a compile for the "magic(5)"
;; database.


;;; Code:

;; Magiciel setup.

(require 'cl-lib)
(require 'bindat)
;; (require 'bitpack)			; Getting ready for it.
(require 'pcre2el)
(require 'platel)			; Here we go.


(defvar magiciel-path (file-name-directory (or load-file-name "."))
  "The location the MAGICIEL library is loaded from.")


(defvar magiciel--do-recompile-dir nil
  "Controls whether to recompile the source folder or not.")


;; This seems "obsoleted" in Emacs 29.1.
;;
;; (load (expand-file-name "magiciel-pkg" magiciel-path))

;; (load (expand-file-name "magiciel-constants" magiciel-path)) ; This is pleonastic.

(load (expand-file-name "utilities" magiciel-path))
(load (expand-file-name "cstringizing" magiciel-path))

(load (expand-file-name "magic-ingredients" magiciel-path))

(load (expand-file-name "magic-bits" magiciel-path))
(load (expand-file-name "cauldron" magiciel-path))
(load (expand-file-name "parse-magic" magiciel-path))
(load (expand-file-name "bookshelf" magiciel-path))
(load (expand-file-name "compile-magic" magiciel-path))
(load (expand-file-name "magiciel-spells" magiciel-path))
(load (expand-file-name "brew" magiciel-path))


;; Testing, actually.

(load (expand-file-name "try-magic" magiciel-path))


;; Epilogue.

(when magiciel--do-recompile-dir
  (byte-recompile-directory magiciel-path 0))


(provide 'magiciel)

;; Local Variables:
;; mode: Emacs-lisp
;; End:

;;; magiciel.el ends here
