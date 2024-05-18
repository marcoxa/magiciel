;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; cstringizing.el


;;; Commentary:
;;
;; Utilities to read "C strings" (cfr., the "C standard", search for
;; "escape"), which, bottom line, are also Emacs Lisp ones.
;;
;; It is weird that no such library seems to exists out there.
;;
;; Notes:
;;
;; * In `magiciel' FTTB.
;;
;; * Using `write-char` to a (temp string) obviously does something
;;   about multibyte stuff.  Shifting to collecting byts and then
;;   using `unibyte-string` seems to do TRT.


;;; Code:

(require 'cl-lib)


(cl-defun magiciel--cstringize (c-string
				&optional
				(verbose-level 0)
				&aux
				(cslen (length c-string)))
  "Produces a \\='string\\=' from C-STRING with all escapes replaced by bytes.

The functions scans C-STRING producing results as follows.

Examples:

(magiciel-cstringize \"\\\\x62\") ==> \"b\")

Note that in the example the length of the original string is 4, while
the result has length 1.

The optional VERBOSE-LEVEL can be 0 (non verbose), 1 (medium verbose)
or 2 (verbose)."

  (if (string-empty-p c-string)
      ""

    ;; I have at least one character.
    
    (let ((c (aref c-string 0))
	  (i 0)
	  (bytes ())
	  )
      (cl-block processing
	(cl-labels
	    ((msg (level fmt &rest args)
	       (when (<= 0 level verbose-level)
		 (apply #'message fmt args))
	       )
	       
	     (start ()
	       (msg 1 "\nCSTRINGIZE start...")
	       (msg 1 "\nCSTRINGIZE %d %S" cslen c-string)
		 
	       ;; (when (end-processing) (finish)) ; Too paranoid.

	       ;; Differently paranoid.
	       (cl-assert (< i cslen))
		 
	       (process-char)
	       )

	     (end-processing ()
	       (= (1+ i) cslen))

	     (advance ()
	       (cl-incf i)
	       (setf c (aref c-string i)))

	     (moveto (pos)
	       (setq i pos
		     c (aref c-string i)))

	     (finish ()
	       (msg 1 "CSTRINGIZE: finish.")
	       (cl-return-from processing
		 (apply #'unibyte-string
			(reverse bytes))
		 ))
	 
	     (process-char ()
	       (msg 1 "CSTRINGIZE: processing %c" c)
	       (if (char-equal c ?\\)
		   (process-escape)
		 (process-normal-char)))

	     (process-escape ()
	       (when (end-processing)
		 ;; (write-char c)
		 (push c bytes)
		 (finish))

	       (advance)
	       (msg 1 "CSTRINGIZE: processing \\%c" c)
	       (pcase c
		 (?a ; (write-char ?\a)
		  (push ?\a bytes))
		 (?b ; (write-char ?\b)
		  (push ?\b bytes))
		 (?f ; (write-char ?\f)
		  (push ?\f bytes))
		 (?n ; (write-char ?\n)
		  (push ?\n bytes))
		 (?r ; (write-char ?\r)
		  (push ?\r bytes))
		 (?t ; (write-char ?\t)
		  (push ?\t bytes))
		 (?v ; (write-char ?\v)
		  (push ?\v bytes))
		 (?d ; (write-char ?\d)
		  (push ?\d bytes))
		 (?\\ ; (write-char ?\\)
		  (push ?\\ bytes))
		 (?x (process-hex-escape))
		 ((rx (in "0-7")) (maybe-process-oct-escape))
		 (_ ; (write-char ?\\)
		  (push ?\\ bytes)
		  ;; (write-char c)
		  (push c bytes)
		  )
		 )
	       (when (end-processing) (finish))
	       (advance)
	       (process-char)
	       )

	     (process-normal-char ()
	       (msg 1 "CSTRINGIZE: processing normal %c %d %s"
		    c
		    i
		    (substring c-string i)
		    )
	       ;; (write-char c)
	       (push c bytes)
	       (when (end-processing) (finish))
	       (advance)
	       (process-char)
	       )

	     (process-hex-escape ()
	       (msg 1 "CSTRINGIZE: processing hex %c %d %s"
		    c
		    i
		    (substring c-string i)
		    )
	       (let ((sm (string-match (rx (seq (in "xX")
						(group
						 (? xdigit)
						 (? xdigit))))
				       c-string
				       i))
		     )
		 (msg 1 "CSTRINGIZE: matching hex %s" sm)
		 (if (= i (or sm -1))
		     (let* ((hexs (match-string 1 c-string))
			    (hexn (if (string-empty-p hexs) "0" hexs))
			    (hexv (car (read-from-string (concat "#x" hexn))))
			    )
		       (msg 1 "CSTRINGIZE: writing hex %S %c"
			    hexs
			    hexv)
		       ;; (write-char hexv)
		       (push hexv bytes)
		       (moveto (+ i (length hexs)))
		       )
		   ;; Nope. If no match, just do nothing: cfr.,
		   ;; `read-from-string` behavior.
		   )
	       
		 (when (end-processing) (finish))
		 (advance)
		 (process-char))
	       )

	     (maybe-process-oct-escape ()
	       (msg 1 "CSTRINGIZE: processing oct %c %d %s"
		    c
		    i
		    (substring c-string i)
		    )
	       (let ((om (string-match (rx (seq (? (in "0-7"))
						(? (in "0-7"))))
				       c-string
				       (1+ i)))
		     )
		 (if (= (1+ i) (or om -1))
		     (let* ((octs (match-string 0 c-string))
			    (octn (if (string-empty-p octs)
				      (make-string 1 c nil)
				    (concat (list c) octs)))
			    (octv (car (read-from-string (concat "#o" octn))))
			    )
		       (msg 1 "CSTRINGIZE: writing oct %c" octv)
		       ;; (write-char octv)
		       (push octv bytes)
		       (moveto (+ 1 i (length octs))))
		   (progn
		     (msg 1 "CSTRINGIZE: writing single oct %c" c)
		     ;; (write-char c)
		     (push c bytes)
		     (moveto (1+ i))))
		 )
	       (when (end-processing) (finish))
	       (advance)
	       (process-char)
	       )
	     )				; cl-labels.
	  (start)
	  ))
      )))

;;; cstringzing.el ends here
