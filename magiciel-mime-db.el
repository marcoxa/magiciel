;;; magiciel --- Guessing the content of a file in the guise of `file' `magic'. -*- lexical-binding: t -*-

;;; magiciel-mime-db.el
;;;
;;; Functions to manipulate mime data base.

;;; Commentary:
;;
;; Each record in the "mime data base" is the result of reading the
;; Javascript "mime-db" JSON file (which is incomplete).
;;
;; Each "record" in the resulting list is:
;;
;;    (<mime-type> <field>*)
;;
;; where
;;
;;    <mime-type> is a simbol <mime-category>/<mime-category-subtype>
;;
;; and each <field> is a cons
;;
;;    (<field-name> . <value>)
;;
;; The functions below act on this structure.


;;; Code:

(require 'cl-lib)

;; Setup
;; -----

(defvar magiciel-*mime-data-db-file*
  (expand-file-name "magiciel-mime-data.el" magiciel-path)
  "The default file containing the translation of Javascript \"mime-db\".

The translation is done via `json-read-file'.")


(defvar magiciel-*mime-data-db-records* ()
  "The currently loaded \\='mime-record\\='.")


;; MIME Records.

(cl-defun magiciel--make-mime-record (mime-type &rest fields)
  "Create a \\='mime-record\\=' with MIME-TYPE and FIELDS."
  (cons mime-type fields))


(cl-defun magiciel--mime-record-type (mr)
  "Return the \\='mime-type\\=' of the \\='mime-record\\=' MR."
  (cl-first mr))


(cl-defun magiciel--mime-record-fields (mr)
  "Return the \\='fields\\=' of the \\='mime-record\\=' MR."
  (cl-rest mr))


(cl-defun magiciel--mime-record-field (mr field)
  "Return the FIELD value of the \\='mime-record\\=' MR.

The values are actually two: the value of the FIELD and whether the
FIELD was actually found.  This is to deal with NIL-valued fields."
  (let* ((fv (assq field (magiciel--mime-record-fields mr)))
	 (fv-found-p (not (null fv)))
	 (fv-value (cl-rest fv))
	 (fv-value (if (eq fv-value :json-false) nil fv-value)))
    (cl-values fv-value fv-found-p)))


;; Functions.

(cl-defun magiciel-load-mime-db (&optional
				 (mime-type-file magiciel-*mime-data-db-file*))
  "Load the mime data base (a list) from MIME-TYPE-FILE.

The result is a list of \\='mime-record\\='s.  The internal state of
the \\='magiciel\\=' library is changed."
  (with-temp-buffer
    (insert-file-contents mime-type-file)
    (setf magiciel-*mime-data-db-records*
	  (read (current-buffer)))))


(cl-defun magiciel-mime-type-split (mime-type)
  "Split the MIME-TYPE in <category> and <subtype>.

The result is a list of one or two symbols."
  (mapcar #'intern (split-string (symbol-name mime-type) "[/]" t)))


(cl-defun magiciel-mime-type-category (mime-type)
  "Return the \"category\" of MIME-TYPE.

MIME-TYPE is a symbol of the form <category>/<subtype>."
  (cl-first (magiciel-mime-type-split mime-type)))


(cl-defun magiciel-mime-type-subtype (mime-type)
  "Return the \"subtype\" of MIME-TYPE.

MIME-TYPE is a symbol of the form <category>/<subtype>.  This function
may return NIL, indicating no <subtype>."
  (cl-second (magiciel-mime-type-split mime-type)))


;; Reverse index extension -> mime-type

(defvar magiciel--*extension-to-mime-type*
  (make-hash-table :test #'equal)
  "The map from <extensions> to <mime-type>.")


(defun magiciel--ext-mime-type (ext)
  "Return the <mime-type> associated to extension EXT."
  (gethash ext magiciel--*extension-to-mime-type*))



(cl-defun magiciel--set-ext-mime-type (ext mt)
  "Associate the <mime-type> MT to extension EXT."
  (cl-pushnew mt (gethash ext magiciel--*extension-to-mime-type*)))


(cl-defun magiciel--fill-extension-index
    (&optional (mtdb magiciel-*mime-data-db-records*))
  "Fill the internal map form extensions to mime-type.

The function uses a list of mime-type records MTDB."
  (cl-loop for r in mtdb
	   for mrt = (magiciel--mime-record-type r)
	   do (cl-multiple-value-bind (mexts extsp)
		  (magiciel--mime-record-field r 'extensions)
		(when extsp
		  (cl-loop for ext across mexts
			   do (magiciel--set-ext-mime-type ext mrt))))))


;; Epilogue


;; Local Variables:
;; mode: Emacs-lisp
;; End:

(provide 'magiciel-mime-db)

;;; magiciel-mime-db.el ends here.
