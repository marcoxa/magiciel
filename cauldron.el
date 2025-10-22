;;; magiciel --- Guessing the content of a file in simple as possible way. -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; cauldron.el
;;
;; See the file COPYING license and copyright information.

;;; Commentary:
;;
;; Get mime types using the "file" command on UN*X and querying the
;; registry on Windows.
;;
;; This file contains the package handling of the compiled functions
;; recognizing the file types, the mime types, the file extensions etc etc.


;;; Code:

(require 'cl-lib)

(cl-defstruct (magiciel--cauldron
	       (:constructor magiciel--make-cauldron)
	       )
  "The structure holding the functions usable to recognize file properties. "
  (fun-table (make-hash-table :test #'equal)
	     :type hash-table)
  )


(defvar magiciel--*cauldron*
  (magiciel--make-cauldron))


(cl-defun magiciel--get-spells (&optional (cauldron magiciel--*cauldron*))
  "Return a list of all the spells in CAULDRON.

CAULDRON is of type `magiciel--cauldron'."
  (cl-loop for spell being the hash-value
	   of (magiciel--cauldron-fun-table cauldron)
	   collect spell))


(cl-defun magiciel--spell-names (&optional (cauldron magiciel--*cauldron*))
  "Return a list of the known spell names in CAULDRON.

CAULDRON is of type `magiciel--cauldron'."
  (cl-loop for spell being the hash-key
	   of (magiciel--cauldron-fun-table cauldron)
	   collect spell))


(cl-defun magiciel--get-spell (spell-name
			       &optional
			       (cauldron magiciel--*cauldron*))
  "Get spell with name SPELL-NAME from CAULDRON.

CAULDRON is of type `magiciel--cauldron'."
  (gethash spell-name (magiciel--cauldron-fun-table cauldron)))	; Add a default maybe?


(cl-defun magiciel--add-spell (spell-name
			       spell-code
			       &optional
			       (cauldron magiciel--*cauldron*))
  "Add spell with code SPELL-CODE and name SPELL-NAME into CAULDRON.

CAULDRON is of type `magiciel--cauldron'."
  (setf (gethash spell-name (magiciel--cauldron-fun-table cauldron))
	spell-code))


(provide 'magiciel-cauldron)

;;; cauldron.el ends here
