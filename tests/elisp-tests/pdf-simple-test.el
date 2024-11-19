;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; pdf-simple.el

;; PDF Simple.

(magiciel--msg 1 "PDF-simple loading.")

(defvar magiciel--pdf-simple-magic
  (magiciel--load-magic-file
   (expand-file-name "tests/magic/pdf-simple" magiciel-path)))


(magiciel--msg 1 "PDF-simple compiling.")

(cl-multiple-value-setq (pdfsrecs pdfsgrps)
  (magiciel-compile-magic magiciel--pdf-simple-magic 2))

(magiciel--msg 1 "PDF-simple compiled.")
(magiciel--msg 1 "PDF-simple emitting.")

(setq magiciel--pdf-simple-emitted
      (magiciel--magic-emit magiciel--pdf-simple-magic pdfsgrps))

(magiciel--msg 1 "PDF-simple compiling and emitting.")

;;; pdf-simple.el ends here.
