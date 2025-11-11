;;; magiciel --- Guessing the content of a file in the guise of `file' `magic'. -*- lexical-binding: t -*-

;;; magiciel-mime-local-data.el
;;;
;;; File containing a manually curated list of useful MIME types.  The
;;; format is the same of 'magiciel-mime-data.el'; note that the
;;; 'source' field is "magiciel".


(
 ;; Application category.

 (application/x-common-lisp (source . "magiciel")
			    (extensions . ["fasl" "fsl"]))
  
 (application/x-emacs-lisp (source . "magiciel") (extensions . ["eln" "elc"]))

 (application/x-xmit (source . "magiciel") (extensions . ["xmi"]))

 
 ;;; Text category.
 (text/x-ada (source . "magiciel") (extensions . ["ada" "adb" "ads"]))
 
 (text/x-c (source . "magiciel") (extensions . ["c" "h"]))
 (text/x-c++ (source . "magiciel") (extensions . ["cc" "hh" "C" "cpp"
						  "hpp"]))

 (text/x-common-lisp (source . "magiciel")
		     (extensions . ["lisp" "lsp" "l" "cl"]))

 (text/x-emacs-lisp (source . "magiciel") (extensions . ["el"]))
 
 ;; (text/x-fortran (source . "apache") (extensions . ["f" "for" "f77" "f90"]))

 ;; (text/x-java-source (source . "apache") (extensions . ["java"]))
 (text/x-java (source . "magiciel") (extensions . ["java"]))

 (text/x-julia (source . "magiciel") (extensions . ["jl"]))

 (text/x-python (source . "magiciel") (extensions . ["py"]))

 ;; (text/x-markdown (compressible . t) (extensions . ["mkd"]))
 (text/x-markdown (source . "magiciel")
		  (compressible . t)
		  (extensions . ["md" "mkd"]))

  (text/x-r (source . "magiciel") (extensions . ["R"]))

 (text/x-rust (source . "magiciel") (extensions . ["rs"]))
 )


;; Local Variables:
;; mode: Emacs-lisp
;; End:

;;; magiciel-mime-local-data.el ends here.
