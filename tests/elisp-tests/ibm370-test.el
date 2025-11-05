;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; ibm.el

;; IBM.

(magiciel--msg 1 "loading IBM magic file.")

(defvar magiciel--ibm-magic
  (magiciel--load-magic-file
   (expand-file-name "tests/magic/ibm370" magiciel-path)))

(message "\nMAGICIEL: IBM compiling and emitting.")

(message "MAGICIEL: IBM compiling...")

(cl-multiple-value-setq (ibmrecs ibmgrps)
  (magiciel-compile-magic magiciel--ibm-magic 3))

(message "MAGICIEL: IBM emitting...")

(setq magiciel-clang-emitted
      (magiciel--magic-emit magiciel--ibm-magic ibmgrps))

;;; ibm.el ends here.
