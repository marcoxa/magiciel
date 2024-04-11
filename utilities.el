;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; utilities.el


;;; Commentary:
;;
;; Utility functions for the `magiciel' library.


;;; Code:

;; Magiciel setup.

(require 'cl-lib)


;; Utilities.
;; ----------


;; Debugging.
;; ----------

(defvar magiciel--*debug-level* 0)

(defvar magiciel--*debug-low-level* 0)
(defvar magiciel--*debug-mid-level* 5)
(defvar magiciel--*debug-top-level* 9)


(cl-defun magiciel--msg (level fmt &rest fmt-args)
  (when (>= level magiciel--*debug-level*)
    (apply #'message fmt fmt-args)))


;; Yeah, yeah, yeah.  Just bear with me and enjoy the curries.

(defun curry1 (f arg)
  #'(lambda (x) (apply f arg (list x))))


(defun curry2 (f arg1 arg2)
  #'(lambda (x) (apply f arg1 arg2 (list x))))

;;; magiciel.el ends here
