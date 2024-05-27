;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; brew.el


;;; Commentary:
;;
;; Get mime types using the "file" command on UN*X and querying the
;; registry on Windows.
;;
;; This file contains the code to fill in the `apothecary' folder with
;; the code obtained by compiling the specs obtained from
;; `/usr/share/file/magic' (un a UN*X machine).
;;
;; The code can then be used to fill the `cauldron', thus making
;; `magiciel-divine' usable.


;;; Code:

(require 'cl-lib)
(require 'magiciel-cauldron)


(defvar magiciel--*magic-folder*
  (unless (eq system-type 'windows-nt)
    (expand-file-name "/usr/share/file/magic"))
  "The system folder where magic(5) files are kept.

This is `nil' on Windows systems.")


(defvar magiciel--*apothecary-folder*
  (expand-file-name "apothecary/")
  "The \"magiciel\" folder where compiled recipes are kept.")


(defvar magiciel--*magic-files-structs* ())


(cl-defun magiciel-load-magic-recipes (&optional
				       (step nil)
				       (magic-folder
					magiciel--*magic-folder*)
				       (apothecary-folder
					magiciel--*apothecary-folder*)
				       )

  "Create a list of `magiciel--magic' structures.

The structures are created by reading \"magic(5)\" files from the UN*X
MAGIC-FOLDER, which defaults to `magiciel--*magic-folder*', that is
\"/usr/share/file/magic/\".

If STEP is non-nil, a confirmation is asked to load each file.

The parameter apothecary-folder is a directory where the generated
code will be stored (unused for the time being)."

  (interactive
   (let ((mf (expand-file-name
	      (read-directory-name "Magic folder: "
				   "."
				   magiciel--*magic-folder*
				   t)))
	 (af (expand-file-name
	      (read-directory-name "Apothecary folder: "
				   "."
				   magiciel--*apothecary-folder*
				   t)))
	 (step (y-or-n-p "Confirm file by file?"))
	 )
     (list step mf af)))

  (unless (and magic-folder (file-directory-p magic-folder))
    (if (and (eq system-type 'windows-nt)
	     (null magic-folder))
	(magiciel--error
	 "there is no magic(5) folder on a a Windows system.")
      (magiciel--error
       "magic(5) %S either does not exists or is not a directory."
       magic-folder)))
  
  (unless (and apothecary-folder (file-directory-p apothecary-folder))
    (magiciel--warning
     "apothecary %S either does not exists or is not a directory."
     apothecary-folder)
    (when (yes-or-no-p
	   (format "MAGICIEL: create folder %S? "
		   apothecary-folder))
      (mkdir apothecary-folder t))
    )

  (cl-loop for recipe in (directory-files magic-folder)
	   for long-recipe = (expand-file-name recipe magic-folder)
	   do (magiciel--info "loading recipe %S" recipe)
	   do (magiciel--info "long recipe    %S\n" long-recipe)
	   unless (or (string-equal "." recipe)
		      (string-equal ".." recipe))
	     when (or (and step
			   (yes-or-no-p
			    (format "MAGICIEL: load the magic(5) file %S? "
				    recipe)))
		      (null step))
	       collect (magiciel--load-magic-file
			(expand-file-name recipe magic-folder))
	       into mfs
	   finally
	   (cl-return
	    (setq magiciel--*magic-files-structs* mfs))
	   )
  
  (if (called-interactively-p 'interactive)
      (magiciel--info "loaded %d magic files from %S."
		      (length magiciel--*magic-files-structs*)
		      magiciel--*magic-folder*)
    magiciel--*magic-files-structs*
    ))


(cl-defun magiciel--write-spell-file (mfs
				      mfname
				      magic-fun
				      &optional
				      (apothecary
				       magiciel--*apothecary-folder*))
  (let* ((spell-name (format "%s.el" (magiciel--magic-name mfs)))
	 (spell-filename (concat apothecary spell-name))
	 (spell-buffer (get-buffer-create spell-name))
	 (spell-code `(cl-defun ,mfname ,@(cl-rest magic-fun)))
	 )
    (unwind-protect
	(with-current-buffer spell-buffer

	  (insert ";;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-\n")
	  (insert
	   (format ";;; %s --- Guessing the content of a file in"
		   spell-name))
	  (insert " simple as possible way.\n")
	  (insert ";; See the `magiciel' COPYING file for")
	  (insert " licensing information.\n")
	  (insert "\n")
	  (insert ";; This is a generated file!  You have been warned!\n\n")
	  (insert (format ";;; %s\n\n" mfname))

	  ;; Nice, but awfully slow and ureliable.
	  ;; (let ((pp-max-width
	  ;; 	 ;; 80
	  ;; 	 nil))
	  ;;   (pp-emacs-lisp-code spell-code))

	  (print spell-code (current-buffer))

	  (insert
	   (format "\n\n;;; %s ends here.\n" spell-name))
	  (write-file spell-filename)
	  )
      (kill-buffer spell-buffer)
      )
    ))


(cl-defun magiciel--brew-potion (recipe
				 &optional
				 (apothecary
				  magiciel--*apothecary-folder*)
				 (magic-folder
				  magiciel--*magic-folder*)
				 )
  ;; `recipe' is a file name.
  
  (let ((magic-s (magiciel--load-magic-file
		  (expand-file-name recipe magic-folder)))
	)
    (cl-multiple-value-bind (frecs fgrps)
	(condition-case magic-compilation-error
	    (magiciel-compile-magic magic-s 2)
	  (error
	   (magiciel--warning "condition %S %S\n\twhile compiling %s; skipping..."
			      (car magic-compilation-error)
			      (cdr magic-compilation-error)
			      (magiciel--magic-name magic-s))
	   (cl-values nil nil))
	  )
      (ignore frecs)
      (when fgrps
	(cl-multiple-value-bind (mfname gfns gns magic-fun)
	    (magiciel--magic-emit magic-s fgrps)
	  (ignore gfns gns)
	  (magiciel--write-spell-file magic-s mfname magic-fun apothecary)
	  ))
      )))


(cl-defun magiciel--fill-apothecary (&optional
				     (step nil)
				     (apothecary
				      magiciel--*apothecary-folder*))
  (interactive
   (let ((ad (read-directory-name
	      "MAGICIEL apothecary folder: "
	      magiciel--*apothecary-folder*
	      magiciel--*apothecary-folder*
	      t
	      ))
	 (step (y-or-n-p "Step through loading and compiling?"))
	 )
     (list step ad)))

  (ignore step)
  
  (let ((mfile-structs (magiciel-load-magic-recipes)))

    (magiciel--info "magic recipes loaded.")
      
    (dolist (mfs mfile-structs t)
	
      (if (called-interactively-p 'interactive)
	  (magiciel--info "filling apothecary with %S."
			  (magiciel--magic-name mfs))
	(princ (format "MAGICIEL: filling apothecary with %S.\n"
		       (magiciel--magic-name mfs))
	       standard-output))
	
      (cl-multiple-value-bind (recs grps)
	  (condition-case magic-compilation-error
	      (magiciel-compile-magic mfs 2)
	    (error
	     (magiciel--warning "condition %S %S\n\twhile compiling %S; skipping..."
				(car magic-compilation-error)
				(cdr magic-compilation-error)
				(magiciel--magic-name mfs))
	     (cl-values nil nil))
	    )
	(ignore recs)
	(when grps
	  (cl-multiple-value-bind (mfname gfns gns magic-fun)
	      (magiciel--magic-emit mfs grps)
	    (ignore gfns gns)
	    (magiciel--write-spell-file mfs mfname magic-fun apothecary)
	    ))))			; dolist.
    ))


;;; Epilogue.

(provide 'magiciel-brew)

;;; brew.el ends here
