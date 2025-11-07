;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; clang.el

;; C.
 
(magiciel--msg 0 "loading C-Lang magic file.")

(defvar magiciel--c-lang-magic
  (magiciel--load-magic-file
   (expand-file-name "tests/magic/c-lang" magiciel-path)))

(magiciel--msg 0 "C-lang compiling and emitting.")

(cl-multiple-value-setq (clangrecs clanggrps)
  (magiciel-compile-magic magiciel--c-lang-magic 3))

(magiciel--msg 0 "C-lang compiled.")

(magiciel--msg 0 "C-lang emitting.")

;; (magiciel--set-debug-level magiciel--*debug-mid-level*)

(cl-multiple-value-setq (clang-fname
			 clang-grpfns
			 clang-grpnamed
			 clang-magic-fun
			 clang-external-funs)
  (magiciel--magic-emit magiciel--c-lang-magic clanggrps))

(magiciel--msg 0 "C-lang emitted.")

(magiciel--msg 0 "saving C-lang code to file.")

(magiciel--write-spell-file magiciel--c-lang-magic
			    clang-fname
			    clang-magic-fun
			    clang-external-funs
			    (expand-file-name "temp/apothecary/"
					      magiciel-path)
			    )

;; (magiciel--set-debug-level magiciel--*debug-low-level*)

;; (magiciel--msg 0 "code to file \"tests/magic/clang-saved.el\".")

(magiciel--msg 0 "C-lang processing completed.\n")


;;; clang.el ends here.
