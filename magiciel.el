;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; magiciel.el


;;; Commentary:
;;
;; Get mime types using the "file" command on UN*X and querying the
;; registry on Windows.


;;; Code:

;; Magiciel setup.

(require 'cl-lib)
(require 'bindat)
(require 'bitpack)			; Getting ready for it.


(defvar magiciel-path (file-name-directory (or load-file-name "."))
  "The location the MAGICIEL library is loaded from.")


;; (load (expand-file-name "magiciel-pkg" magiciel-path)) 

;; (load (expand-file-name "magiciel-constants" magiciel-path)) ; This is pleonastic.

(load (expand-file-name "utilities" magiciel-path))

(load (expand-file-name "magic-ingredients" magiciel-path))

(load (expand-file-name "magic-bits" magiciel-path))
(load (expand-file-name "cauldron" magiciel-path))
(load (expand-file-name "magiciel-spells" magiciel-path))
(load (expand-file-name "parse-magic" magiciel-path))
(load (expand-file-name "compile-magic" magiciel-path))
(load (expand-file-name "try-magic" magiciel-path))


(provide 'magiciel)

;;; magiciel.el ends here
