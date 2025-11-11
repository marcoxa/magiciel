;;; magiciel --- Guessing the content of a file in simple as possible way. -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; magiciel-spells.el
;;
;; See the file COPYING license and copyright information.


;;; Commentary:
;;
;; Get mime types using the "file" command on UN*X and querying the
;; registry on Windows.
;;
;; This file contains the package user level and interactive API.


;;; Code:

(require 'cl-lib)
(require 'magiciel-compile-magic)
(require 'magiciel-mime-db)

;; (load "magiciel-constants.el")		; This is pleonastic.


(cl-defstruct (magiciel-file-info)
  "The File Info Structure

This structure contains information about a file being checked.

Notes:

Not completely used."
  
  (filename "" :type string)
  (extension nil)
  (content-type "application/octet-stream")
  (charset "")

  ;; More below?
  
  (content-tranfer-encoding nil)	; Unused.
  )


(cl-defun magiciel-guess (filename &key divine)
  "Guess the \"mime type\" of FILENAME.

If DIVINE is non-nil and the guessing did not succeed, proceed with
full blown \\='divination\\=', calling `magiciel-divine'."
  (interactive
   (let ((f (read-file-name "Filename: " nil nil t))
	 (d (when current-prefix-arg
	      (y-or-n-p "Try harder by divination if unsuccesful?"))))
     (list f :divine d)))

  (let ((guess (magiciel--guess-by-file-ext filename)))
    (if (and (string= "application/octect-stream" guess) divine)
	(let ((divination (magiciel-divine filename '!:mime)))
	  (setq guess (cl-second divination)))
	  
      (when (called-interactively-p 'any)
	(magiciel--info "filename %S has mime type '%s'."
			(file-name-nondirectory filename)
			guess)))
    guess))


(defun magiciel--guess-by-file-cmd (filename)
  "Given a FILENAME, call the \"file (1)\" UN*X command.

The function takes a FILENAME (string) with extension (if any),
expands it with `expand-file-name' and calls \"file (1)\" by means of
`call-process-shell-command' with switches -I, -E, and -b.  If the
command is succesful then a string containing a MIME type and a
charset is returned, otherwise, a list (`status` `error string`) is
returned.

The function fails with a condition if called on a non UN*X system."

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


(cl-defun magiciel--guess-by-file-ext (filename &optional (ext nil elp))
  "Guess the MIME type of FILENAME by EXT (if supplied).

If no MIME type can be found, the function returs
\\='application/octet-stream\\='."
  
  (let* ((extension (if elp ext (file-name-extension filename)))
	 (mime-type (magiciel--ext-mime-type extension))
	 )

    (if (not (equal '(application/octect-stream) mime-type))
	(symbol-name (cl-first mime-type))
      
      ;; On Windows we can try the registry.

      (when (eq system-type 'windows-nt)
	;; Go to the registry.
	(magiciel--warning
	 (concat "checking extension %s using the"
		 "Windows registry is not (yet) implemented."))
	(cl-return-from magiciel--guess-by-file-ext
	  "application/octect-stream"	; FTTB
	  ))
      )))

	 
;; magiciel-file-info
	 
(cl-defun magiciel-file-info (filename)
  "Construct a `magiciel-file-info' instance, given FILENAME."
  
  (let ((ext (file-name-extension filename)))
    (cl-flet
	((call-file-cmd (filename ext)
	   (let ((file-cmd-output
		  (magiciel--guess-with-file-cmd filename)))

	     (magiciel--info "file cmd output %S." file-cmd-output)

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
		 (magiciel--warning "call to \"file (1)\" failed.")
		 (magiciel--warning "%s" (cl-second file-cmd-output)))
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
	 (magiciel-info "getting file info via the Windows Registry.")
	 (magiciel-info "not implemented yet; returning a kitchen sink.")
	 (make-magiciel-file-info :filename filename
				  :extension ext)
	 )

	((ms-dos haiku)
	 (magiciel-info "cannot get file info in MS-DOS.")
	 (magiciel-info "returning a kitchen sink.")
	 (make-magiciel-file-info :filename filename
				  :extension ext)
	 )

	(t
	 ;; Assumme a UN*X system with "file" available.
	 (magiciel-info "assuming UN*X system; getting file info.")
	 (condition-case nil
	     (call-file-cmd filename ext))
		  
	 ))
      )))


(cl-defun magiciel-data-info (byte-vector &optional (up-to 1024))
  "Guess the data information of BYTE-VECTOR, considering UP-TO bytes.

Notes:

This function is unnused and essentially a no-op, always returning
\"application/octet-stream\"."
  
  (cl-assert (not (multibyte-string-p byte-vector)))
  (cl-assert (<= 1 up-to (length byte-vector)))

  ;; FTTB...
  
  "application/octet-stream"
  )



(cl-deftype magiciel--spell-context ()
  '(member :normal !:normal !:ext !:mime))


(cl-defgeneric magiciel-cast-spell (spell bytes &optional context)
  "Invoke the type guessing machinery.

Arguments and Values:

SPELL : a designator for a guessing method, e.g., a symbol.
BYTES : what to inspect.
CONTEXT : a `magiciel--spell-context', i.e., what to look for (:normal
          for regular checks, !:ext for extension, !:mime for MIME
          type etc.")


(defun magiciel--get-bytes (filename)
  "Return a byte vector representing FILENAME.

This is an internal function, factored out just for debugging."

  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename)
    (buffer-substring-no-properties (point-min) (point-max))
    ))


;; magiciel-divine
;; The main `magiciel' interactive command.
  
(cl-defun magiciel-divine (filename
			   &optional
			   (context :normal)
			   (keep-going nil))
  "The main command of the Magiciel library.

The type of FILENAME will be divined by the call.

CONTEXT can be either `:normal' or `!:normal', in which case the
function will return a description of the file content (as per the
simple use of the UN*X `file' command; or CONTEXT can be `!:mime' in
which case it will return the MIME type of the file (or
\"application/octet-stream\" or \"data\" as last resort.  Finally,
CONTEXT can be `!:ext', in which case the function will return the
most likely extension for FILENAME, by querying the magic
spells (i.e., not by looking at the filename extension).

KEEP-GOING can be set to non NIL to produce all possible guesses.

If called interactively, the command will default to `:normal'
CONTEXT, unless invoked with a prefix (use the ?d choice for `:normal'
operation in that case)."
  
  ;; (interactive "fFilename: \nSContext: ")

  (interactive
   (let ((f (read-file-name "Filename: " nil nil t))
	 (c (if current-prefix-arg
		(cl-first
		 (read-multiple-choice
		  "What information? "
		  '((?d "description" "File content description.")
		    (?m "mime type" "File guessed MIME type.")
		    (?e "extension" "File usual extension."))))
	      ?d))
	 (k (when current-prefix-arg (y-or-n-p "Check all possibilities?")))
	 )
     
     (list f
	   (cl-ecase c
	     (?d :normal)
	     (?m '!:mime)
	     (?e '!:ext))
	   k)
     ))
	      

  ;; The next checks work only for non-interactive calls.

  (unless (called-interactively-p 'interactive)
    (unless (file-exists-p filename)
      (magiciel--error "File '%s' does not exists" filename))

    (when (file-directory-p filename)
      (magiciel--error "Name '%s' denotes a directory" filename))

    (unless (file-readable-p filename)
      (magiciel--error "File '%s' cannot be read" filename))

    (cl-check-type context magiciel--spell-context)
    )
  
  (let* ((fn (expand-file-name filename))
	 (fnnd (file-name-nondirectory fn))
	 (spells (magiciel--get-spells))
	 (fs (magiciel--get-bytes fn))
	 )
    (when (multibyte-string-p fs)
      (magiciel--error "Reading file '%s',\nbut results in a multibyte buffer"
		       fn))

    (magiciel--msg 1 "")
    (magiciel--msg 1 "Divination: trying %d different spells."
		   (length spells))
    
    (let ((divinations ()))
      (cl-loop for spell in spells
	       for divination = (magiciel-cast-spell spell fs context)
	       do (magiciel--msg 0 "found divination %S for spell %S in context %s."
				 divination spell context)
	       when (and divination
			 (not (string-empty-p divination)))

	         if keep-going
		   collect divination into guesses
		 else
		   return (setq divinations (list divination))
	         end
	       end
	       finally (setq divinations guesses)
	       )

      (if (zerop (length divinations))
	  (magiciel--msg 1 "Divination: collected 0 divinations.")
	(magiciel--msg 1 "Divination: collected %d divinations."
		       (length divinations)))

      (if divinations
	  (dolist (d divinations)
	    (when (called-interactively-p 'any)
	      (cond ((string-empty-p d)
		     (magiciel--warning "empty divination."))
		    ((or (eq context :normal)
			 (eq context '!:normal))
		     (magiciel--info "divination: %S is a%s."
				     fnnd d))
		    ((eq context '!:mime)
		     (magiciel--info "divination: %S mime type is %S."
				     fnnd d))
		    ((eq context '!:ext)
		     (magiciel--info "divination: %S usual extension is %S."
				     fnnd d))
		    )))
	(when (called-interactively-p 'any)
	  (magiciel--info
	   "divination: %S is undivinable, but it is %d bytes long."
	   fnnd
	   (length fs)))
	)

      (cons fn divinations)
      )))


(cl-defmethod magiciel-cast-spell ((spell function)
				   bytes
				   &optional
				   (context :normal))
  "Invoke function SPELL on BYTES with CONTEXT to guess the content type."
  
  (cl-check-type context magiciel--spell-context)
  (magiciel--msg 0 "casting function spell %s" spell)
  (funcall spell bytes 0 context))


(cl-defmethod magiciel-cast-spell ((spell symbol)
				   bytes
				   &optional
				   (context :normal))
  "Invoke function named SPELL on BYTES with CONTEXT to guess the content type."
  (cl-check-type context magiciel--spell-context)
  (magiciel--msg 0 "casting symbol spell %s" spell)
  (funcall spell bytes 0 context))


(provide 'magiciel-spells)

;;; magiciel-spells.el ends here
