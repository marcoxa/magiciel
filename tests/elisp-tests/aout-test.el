;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; aout.el

;; a.out.

(magiciel--msg 0 "loading a.out magic file.")

(defvar magiciel--aout-magic
  (magiciel--load-magic-file "tests/magic/aout"))

(magiciel--msg 0 "aout compiling and emitting.")

(cl-multiple-value-setq (aoutrecs aoutgrps)
  (magiciel-compile-magic magiciel--aout-magic 2))

(cl-multiple-value-setq (aoutfname
			 aoutgrpfns
			 aoutgrpnamed
			 aout-magic-fun
			 aout-external-funs)
  (magiciel--magic-emit magiciel--aout-magic aoutgrps))

(magiciel--msg 0 "aout compiled and emitted.")

;; (magiciel--msg 0 "saving code to file.")

;; (magiciel--save-code aout-magic-fun "tests/magic/aout-saved.el")
				  
(magiciel--msg 0 "code to file \"tests/magic/aout-saved.el\".")

(magiciel--msg 0 "aout processing completed.")

;;; aout.el ends here.
