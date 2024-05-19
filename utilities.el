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


(defun magiciel--warn (fmt &rest args)
  (apply #'warn (concat "MAGICIEL: " fmt) args))


(defun magiciel--error (fmt &rest args)
  (apply #'error (concat "MAGICIEL: " fmt) args))


;; Debugging.
;; ----------

(defvar magiciel--*debug-low-level* 0)
(defvar magiciel--*debug-mid-level* 5)
(defvar magiciel--*debug-top-level* 9)

(defvar magiciel--*debug-level* magiciel--*debug-low-level*)
(defvar magiciel--*current-debug-level* magiciel--*debug-level*)


(defun magiciel--set-debug-level (l)
  (setq magiciel--*debug-level* l))


(cl-defun magiciel--msg (fmt &rest fmt-args)
  (when (>= magiciel--*debug-level* magiciel--*current-debug-level*)
    (apply #'message fmt fmt-args)))


(cl-defmacro with-debug-level (l &body code)
  `(let ((magiciel--*current-debug-level* ,l))
     ,@code))


;; Yeah, yeah, yeah.  Just bear with me and enjoy the curries.

(defun curry1 (f arg)
  #'(lambda (x) (apply f arg (list x))))


(defun curry2 (f arg1 arg2)
  #'(lambda (x) (apply f arg1 arg2 (list x))))

;;; utilities.el ends here
