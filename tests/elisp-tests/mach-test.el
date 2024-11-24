;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; mach.el

;; mach.

(magiciel--msg 0 "loading mach magic file.")

(defvar magiciel--mach-magic
  (magiciel--load-magic-file
   (expand-file-name "tests/magic/mach" magiciel-path)))

(magiciel--msg 0 "mach compiling and emitting.")


(cl-multiple-value-setq (machrecs machgrps)
  (magiciel-compile-magic magiciel--mach-magic 2))

(magiciel--msg 0 "mach compiled.")


(magiciel--msg 0 "mach emitting.")

(cl-multiple-value-setq (mach-fname
			 mach-grpfns
			 mach-grpnamed
			 mach-magic-fun
			 mach-external-funs)
  (magiciel--magic-emit magiciel--mach-magic machgrps))

(magiciel--msg 0 "mach emitted.")


;; (magiciel--msg 0 "saving code to file.")

;; (magiciel--save-code mach-magic-fun "tests/magic/mach-saved.el")

(magiciel--msg 0 "saving mach code to file.")

(magiciel--write-spell-file magiciel--mach-magic
			    mach-fname
			    mach-magic-fun
			    mach-external-funs
			    (expand-file-name "temp/apothecary/"
					      magiciel-path)
			    )

				  
(magiciel--msg 0 "code to file \"tests/magic/mach-saved.el\".")

(magiciel--msg 0 "mach processing completed.")

;;; mach.el ends here.
