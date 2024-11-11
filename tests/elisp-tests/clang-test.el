;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; clang.el

;; C.
 
(magiciel--msg 1 "loading C-Lang magic file.")

(defvar magiciel--c-lang-magic
  (magiciel--load-magic-file
   (expand-file-name "tests/magic/c-lang" magiciel-path)))

(message "\nMAGICIEL: c-lang compiling and emitting.")

(cl-multiple-value-setq (clangrecs clanggrps)
  (magiciel-compile-magic magiciel--c-lang-magic 2))

(setq magiciel-clang-emitted
      (magiciel--magic-emit magiciel--c-lang-magic clanggrps))

(with-temp-buffer
  (print (cl-fourth magiciel-clang-emitted) (current-buffer))
  (write-file "temp/temp-clang-fun.el"))

;;; clang.el ends here.
