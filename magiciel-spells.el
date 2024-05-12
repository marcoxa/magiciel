;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; magiciel-spells.el


;;; Commentary:
;;
;; Get mime types using the "file" command on UN*X and querying the
;; registry on Windows.
;;
;; This file contains the package top level API.


;;; Code:

(require 'cl-lib)

;; (load "magiciel-constants.el")		; This is pleonastic.


(cl-defstruct (magiciel-file-info)
  (filename "" :type string)
  (extension nil)
  (content-type "application/octet-stream")
  (charset "")

  ;; More below?
  
  (content-tranfer-encoding nil)	; Unused.
  )



(defun magiciel--guess-with-file-cmd (filename)
  "Given a `filename' calls the \"file (1)\" UN*X command.

The function takes a `filename' (string) with extension (if any),
expands it with `expand-file-name' and calls \"file (1)\" by means of
`call-process-shell-command' with switches -I, -E, and -b.  If the
command is succesful then a string containing a MIME type and a
charset is returned, otherwise, a list (`status` `error string`) is
returned.

The function fails with a condition if called on a non UN*X system.
"

  (cl-assert (not (member system-type '(windows-nt ms-dos haiku))))
  
  (let ((file-cmd
	 (concat "file"
		 " -I -E -b "
		 (expand-file-name filename)))
	(cmd-status 0)
	)
    (with-temp-buffer
      (setf cmd-status
	    (call-process-shell-command file-cmd nil '(t nil)))
      (if (zerop cmd-status)
 	  (buffer-string)
	(list cmd-status (buffer-string))))
    ))


;; Extension to MIME type map.
;; A fall back mechanism based on file extensions.
;;
;; I worse comes to worse the result is "application/octect-stream".
;;
;; This could go in a separate file.

(defvar magiciel--*ext-mime-type-map*
  (make-hash-table :test #'equal))


(defun magiciel--add-ext-mime-type (ext mime-type)
  (setf (gethash ext magiciel--*ext-mime-type-map*) mime-type))


(defun magiciel--get-mime-type-by-ext (ext)
  (gethash ext magiciel--*ext-mime-type-map* "application/octet-stream"))


(cl-block magiciel--init-ext-mime-type-table ; Block name could be `nil'.

  ;; Why I started this stuff.
  (magiciel--add-ext-mime-type "xmi"  "application/x-xmit")

  ;; Lisp related.
  (magiciel--add-ext-mime-type "cl"   "text/x-common-lisp")
  (magiciel--add-ext-mime-type "lisp" "text/x-common-lisp")
  (magiciel--add-ext-mime-type "lsp"  "text/x-common-lisp")
  (magiciel--add-ext-mime-type "el"   "text/x-emacs-lisp")
  (magiciel--add-ext-mime-type "elc"  "application/x-emacs-lisp")
  (magiciel--add-ext-mime-type "fasl" "application/x-common-lisp")

  ;; Julia
  (magiciel--add-ext-mime-type "jl"   "text/x-julia")
  )


(cl-defun magiciel--guess-by-file-ext (filename &optional (ext nil elp))
  (let ((extension (if elp ext (file-name-extension filename))))
    ;; On Windows we can try the registry.

    (when (eq system-type 'windows-nt)
      ;; Go to the registry.
      (cl-return-from magiciel--guess-by-file-ext
	"application/octet-stream"	; FTTB
	))

    (magiciel--get-mime-type-by-ext extension)
    ))

	 
;; magiciel-file-info
	 
(cl-defun magiciel-file-info (filename)
  (let ((ext (file-name-extension filename)))
    (cl-flet
	((call-file-cmd (filename ext)
	   (let ((file-cmd-output
		  (magiciel--guess-with-file-cmd filename)))

	     (message "MAGICIEL: file cmd output %S." file-cmd-output)

	     (if (stringp file-cmd-output)
		 (let ((parts
			(string-split file-cmd-output ";" nil t)))
		   (make-magiciel-file-info
		    :filename filename
		    :extension ext
		    :content-type (cl-first parts)

		    ;; Should munge the charest too.
		    :charset (cl-second parts)
		    )
		   )
	       (progn
		 (message "MACICEL: call to \"file (1)\" failed.")
		 (message "MACICEL: %s" (cl-second file-cmd-output)))
	       )))
	 )

      ;; Special cases.

      (when (string-equal "xmi" ext)
	(cl-return-from magiciel-file-info
	  (make-magiciel-file-info
	   :filename filename
	   :extension ext
	   :charset "charset=ebcdic-int"
	   :content-type "application/x-xmit"
	   )))
      
		  
      ;; The cases below come from the doc string for `system-type'.
      (cl-case system-type
	(windows-nt
	 (message "MAGICIEL: getting file info via the Windows Registry.")
	 (message "MAGICIEL: not implemented yet; returning a kitchen sink.")
	 (make-magiciel-file-info :filename filename
				 :extension ext)
	 )

	((ms-dos haiku)
	 (message "MAGICIEL: cannot get file info in MS-DOS.")
	 (message "MAGICIEL: returning a kitchen sink.")
	 (make-magiciel-file-info :filename filename
				 :extension ext)
	 )

	(t
	 ;; Assumme a UN*X system with "file" available.
	 (message "MAGICIEL: assuming UN*X system; getting file info.")
	 (condition-case nil
	     (call-file-cmd filename ext))
		  
	 ))
      )))


(cl-defun magiciel-data-info (byte-vector &optional (up-to 1))
  
  (cl-assert (not (multibyte-string-p byte-vector)))
  (cl-assert (<= 1 up-to (length byte-vector)))

  ;; FTTB...
  
  "application/octet-stream"
  )


(cl-defgeneric magiciel-cast-spell (spell bytes))


(cl-defun magiciel--get-bytes (filename)
  "Return a byte vector representing FILENAME.

This is an internal function, factored out just for debugging."

  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename)
    (buffer-substring-no-properties (point-min) (point-max))
    )
  )

  
(cl-defun magiciel-divine (filename)
  "The main command of the Magiciel library.

The type of FILENAME will be divined by the call."
  
  (interactive "fFilename: ")
  
  (unless (file-exists-p filename)
    (error "MAGICIEL: file '%s' does not exists." filename))

  (when (file-directory-p filename)
    (error "MAGICIEL: name '%s' denotes a directory." filename))

  (unless (file-readable-p filename)
    (error "MAGICIEL: file '%s' cannot be read." filename))
  
  (let ((spells (magiciel--get-spells))
	(fs (with-temp-buffer
	      (set-buffer-multibyte nil)
	      (insert-file-contents-literally filename)
	      (buffer-substring-no-properties (point-min) (point-max))
	      )
	    )
	)
    (when (multibyte-string-p fs)
      (error "MAGICIEL: reading file '%s',\nbut results in a multibyte buffer."
	     filename))

    (let ((divinations ()))
      (cl-loop for spell in spells
	       for divination = (magiciel-cast-spell spell fs)
	       when (and divination
			 (not (string-empty-p divination)))

	       collect divination into guesses
	       end
	       finally (setq divinations guesses)
	       )

      (dolist (d divinations (cons filename divinations))
	(when (called-interactively-p 'any)
	  (message "MAGICIEL Divination:%s." d)))
      )))


(cl-defmethod magiciel-cast-spell ((spell function) bytes)
  (funcall spell bytes 0 'magiciel--%normal-context%))


(provide 'magiciel-spells)

;;; magiciel-spells.el ends here
