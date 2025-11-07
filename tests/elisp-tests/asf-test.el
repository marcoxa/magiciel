;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; asf.el

;; ASF.

(magiciel--info "\n")
(magiciel--info "loading afs magic file.")

(defvar magiciel--asf-magic
  (magiciel--load-magic-file
   (expand-file-name "tests/magic/asf" magiciel-path)))

(magiciel--info "ASF compiling and emitting.")

(magiciel--info "ASF compiling...")

(cl-multiple-value-setq (asfrecs asfgrps)
  (magiciel-compile-magic magiciel--asf-magic 2))

(magiciel--info "ASF compiled.\n")

;; (magiciel--info "ASF emitting...")

;; (setq magiciel-asf-emitted
;;   (magiciel--magic-emit magiciel--asf-magic asfgrps))


;;; asf.el ends here.
