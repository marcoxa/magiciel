;;; magiciel --- Guessing the content of a file in simple as possible way. -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; bookshelf.el
;;
;; See the file COPYING license and copyright information.

;;; Commentary:
;;
;; This file contains the package handling of the magic files internal
;; representation.  That is, each 'magic(5)' file (eventually from,
;; e.g., '/usr/share/file/magic')  will be indexed in a 'bookshelf'.


;;; Code:

(require 'cl-lib)
(require 'magiciel-utilities) ; messaging functions.


(cl-defstruct (magiciel--bookshelf
	       (:constructor magiciel--make-bookshelf)
	       )
  "The structure holding the magic file representations."
  (scroll-index (make-hash-table :test #'equal)
		:type hash-table)
  )


(defvar magiciel--*bookshelf*
  (magiciel--make-bookshelf))


(cl-defun magiciel--get-scrolls (&optional (bookshelf magiciel--*bookshelf*))
  "Get the \\='scrolls\\=' from a BOOKSHELF."
  (cl-loop for scroll being the hash-value
	   of (magiciel--bookshelf-scroll-index bookshelf)
	   collect scroll))


(cl-defun magiciel--scroll-names (&optional (bookshelf magiciel--*bookshelf*))
  "Get the \\='scroll names\\=' from a BOOKSHELF."
  (cl-loop for scroll being the hash-key
	   of (magiciel--bookshelf-scroll-index bookshelf)
	   collect scroll))


(cl-defun magiciel--get-scroll (scroll-name
				&optional
				(bookshelf magiciel--*bookshelf*))
  "Get a scroll named SCROLL-NAME from BOOKSHELF."
  (gethash scroll-name
	   (magiciel--bookshelf-scroll-index bookshelf))) ; Add a default maybe?


(cl-defun magiciel--add-scroll (scroll-name
				scroll-rep
				&key
				(bookshelf magiciel--*bookshelf*)
				(if-exists :warn)
				(replace t))
  "Add a scroll SCROLL-REP named SCROLL-NAME to BOOKSHELF.

The variable IF-EXISTS can be \\=':supersede\\=', \\=':warn\\=' (the
default) or \\':error\\='.  If an entry named SCROLL-NAME already exists
in the BOOKSHELF, either it is substituted (\\=':supersede\\='), a
warning or an error is issued.  If an entry already exists, no error is
signalled and REPLACE is non-nil the new SCROLL-REP replaces the old
one."
  (let ((old-sr (gethash scroll-name
			 (magiciel--bookshelf-scroll-index bookshelf))))
    (cl-flet ((insert-scroll ()
		(setf (gethash scroll-name
			       (magiciel--bookshelf-scroll-index bookshelf))
		      scroll-rep)))
      (cond ((null old-sr)
	     (magiciel--msg 0 "adding scroll %S (content %S)."
			    scroll-name
			    (cl-type-of scroll-rep))
	     (insert-scroll))
	    (t
	     (cl-ecase if-exists
	       (:supersede
		(magiciel--warning "substituting scroll-name %S."
				   scroll-name)
		(insert-scroll))
	       (:warn
		(magiciel--warning "scroll-name %S already exists,"
				   scroll-name)
		(cond
		 (replace
		  (magiciel--warning "replacing it.")
		  (insert-scroll))
		 (t
		  (magiciel--warning "old one retained."))))
	       (:error
		(magiciel--warning "scroll-name %S already exists."
				   scroll-name))
	       ))))))


(cl-defun magiciel--clear-bookshelf (&optional
				     (bookshelf magiciel--*bookshelf*))
  "Clear the BOOKSHELF."
  (clrhash (magiciel--bookshelf-scroll-index bookshelf)))


(provide 'magiciel-bookshelf)

;;; bookshelf.el ends here
