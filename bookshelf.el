;;; magiciel --- Guessing the content of a file in simple as possible way. -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; bookshelf.el
;;
;; See the file COPYING license and copyright information.

;;; Commentary:
;;
;; Get mime types using the "file" command on UN*X and querying the
;; registry on Windows.
;;
;; This file contains the package handling of the magic files internal
;; representation.


;;; Code:

(require 'cl-lib)

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
  "Get the \\'=scrolls\\=' from a BOOKSHELF."
  (cl-loop for scroll being the hash-value
	   of (magiciel--bookshelf-scroll-index bookshelf)
	   collect scroll))


(cl-defun magiciel--scroll-names (&optional (bookshelf magiciel--*bookshelf*))
  "Get the \\'=scroll names\\=' from a BOOKSHELF."
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
				&optional
				(bookshelf magiciel--*bookshelf*))
  "Add a scroll SCROLL-REP named SCROLL-NAME to BOOKSHELF."
  (setf (gethash scroll-name
		 (magiciel--bookshelf-scroll-index bookshelf))
	scroll-rep))


(cl-defun magiciel--clear-bookshelf (&optional
				     (bookshelf magiciel--*bookshelf*))
  "Clear the BOOKSHELF."
  (clrhash (magiciel--bookshelf-scroll-index bookshelf)))


(provide 'magiciel-bookshelf)

;;; bookshelf.el ends here
