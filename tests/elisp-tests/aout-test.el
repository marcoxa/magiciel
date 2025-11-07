;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; aout.el

;; a.out.

(magiciel--msg 0 "loading a.out magic file.")

(defvar magiciel--aout-magic
  (magiciel--load-magic-file
   (expand-file-name "tests/magic/aout" magiciel-path)))

(magiciel--msg 0 "aout compiling and emitting.")

(cl-multiple-value-setq (aoutrecs aoutgrps)
  (magiciel-compile-magic magiciel--aout-magic 3))

(magiciel--msg 0 "aout compiled.")

(magiciel--msg 0 "aout  emitting.")

(cl-multiple-value-setq (aout-fname
			 aout-grpfns
			 aout-grpnamed
			 aout-magic-fun
			 aout-external-funs)
  (magiciel--magic-emit magiciel--aout-magic aoutgrps))

(magiciel--msg 0 "aout emitted.")

;; (magiciel--msg 0 "saving code to file.")

;; (magiciel--save-code aout-magic-fun "tests/magic/aout-saved.el")

(magiciel--msg 0 "saving aout code to file.")

(magiciel--write-spell-file magiciel--aout-magic
			    aout-fname
			    aout-magic-fun
			    aout-external-funs
			    (expand-file-name "temp/apothecary/"
					      magiciel-path)
			    )

				  
;; (magiciel--msg 0 "code to file \"tests/magic/aout-saved.el\".")

(magiciel--msg 0 "aout processing completed.\n")

;;; aout.el ends here.
