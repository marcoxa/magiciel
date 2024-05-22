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


(defun magiciel--info (fmt &rest args)
  (apply #'message (concat "MAGICIEL: " fmt) args))


(defun magiciel--warning (fmt &rest args)
  (apply #'warn (concat "MAGICIEL: " fmt) args))


(defun magiciel--error (fmt &rest args)
  (apply #'error (concat "MAGICIEL: " fmt) args))


;; Debugging/Logging.
;; ------------------

(defvar magiciel--*debug-low-level* 0)
(defvar magiciel--*debug-mid-level* 5)
(defvar magiciel--*debug-top-level* 9)

(defvar magiciel--*debug-level* magiciel--*debug-low-level*)
(defvar magiciel--*current-debug-level* magiciel--*debug-level*)


(defun magiciel--set-debug-level (l)
  (setq magiciel--*current-debug-level*
	(magiciel--clamp-debug-level l)))


(cl-defun magiciel--msg (level fmt &rest fmt-args)
  (when (<= 0 level magiciel--*current-debug-level*)
    (apply #'magiciel--info fmt fmt-args)))


(cl-defun magiciel--warn (level fmt &rest fmt-args)
  (when (<= 0 level magiciel--*current-debug-level*)
    (apply #'magiciel--warning fmt fmt-args)))


(cl-defun magiciel--err (level fmt &rest fmt-args)
  (when (<= 0 level magiciel--*current-debug-level*)
    (apply #'magiciel--error fmt fmt-args)))


(cl-defmacro with-debug-level (l &body code)
  `(let ((magiciel--*current-debug-level* ,l))
     ,@code))


(defun magiciel--clamp-debug-level (n)
  (min (max 0 n) magiciel--*debug-top-level*))


;; Yeah, yeah, yeah.  Just bear with me and enjoy the curries.

(defun magiciel--curry1 (f arg)
  #'(lambda (x) (apply f arg (list x))))


(defun magiciel--curry2 (f arg1 arg2)
  #'(lambda (x) (apply f arg1 arg2 (list x))))


;; Epilogue.
;; ---------

(provide 'magiciel-utilities)

;;; utilities.el ends here
