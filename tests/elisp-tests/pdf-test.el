;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; pdf.el

;; PDF.

(magiciel--msg 0 "loading PDF magic file.")

(defvar magiciel--pdf-test-file
  (expand-file-name "tests/eintr.pdf" magiciel-path))

(defvar magiciel--pdf-magic
  (magiciel--load-magic-file
   (expand-file-name "tests/magic/pdf" magiciel-path)))

(magiciel--msg 0 "PDF compiling.")

(cl-multiple-value-setq (pdfrecs pdfgrps)
  (magiciel-compile-magic magiciel--pdf-magic 3))

(magiciel--msg 0 "PDF compiled.")

(magiciel--msg 0 "PDF emitting.")

;; (magiciel--set-debug-level magiciel--*debug-mid-level*)

(cl-multiple-value-setq (pdffname
			 pdfgrpfns
			 pdfgrpnamed
			 pdf-magic-fun
			 pdf-external-funs)
  (magiciel--magic-emit magiciel--pdf-magic pdfgrps))

(magiciel--msg 0 "PDF emitted.")

(magiciel--msg 0 "pdffname %S" pdffname)
(magiciel--msg 0 "pdfgrpfns %S" pdfgrpfns)
(magiciel--msg 0 "pdfgrpnamed %S" pdfgrpnamed)
(magiciel--msg 0 "pdf-magic-fun %S" pdf-magic-fun)
(magiciel--msg 0 "pdf-magic-fun %S" pdf-external-funs)


(magiciel--msg 0 "saving PDF code to file.")

(magiciel--write-spell-file magiciel--pdf-magic
			    pdffname
			    pdf-magic-fun
			    pdf-external-funs
			    (expand-file-name "temp/apothecary/"
					      magiciel-path)
			    )

;; (magiciel--save-code pdf-magic-fun
;; 		     "tests/magic/pdf-saved.el")

;; (magiciel--save-code pdf-external-funs
;; 		     "tests/magic/pdf-saved.el"
;; 		     t)

(magiciel--set-debug-level magiciel--*debug-low-level*)

;; (magiciel--msg 0 "code to file \"tests/magic/pdf-saved.el\".")

(magiciel--msg 0 "code saved in folder %S."
	       (expand-file-name "temp/apothecary/" magiciel-path))

(magiciel--msg 0 "pdf processing completed.\n")


;;; pdf.el ends here.
