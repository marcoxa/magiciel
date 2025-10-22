;;; magiciel --- Guessing the content of a file in simple as possible way. -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; utilities.el
;;
;; See the file COPYING license and copyright information.

;;; Commentary:
;;
;; Utility functions for the `magiciel' library.


;;; Code:

;; Magiciel setup.

(require 'cl-lib)


;; Utilities.
;; ----------

(defun magiciel--info (fmt &rest args)
  "Apply `message' to FMT and ARGS.

The  FMT string is prepended with the \"MAGICIEL:\" string."
  (apply #'message (concat "MAGICIEL: " fmt) args))


(defun magiciel--warning (fmt &rest args)
  "Apply `warn' to FMT and ARGS.

The  FMT string is prepended with the \"MAGICIEL:\" string."
  (apply #'warn (concat "MAGICIEL: " fmt) args))


(defun magiciel--error (fmt &rest args)
  "Apply `error' to FMT and ARGS.

The  FMT string is prepended with the \"MAGICIEL:\" string."
  (apply #'error (concat "MAGICIEL: " fmt) args))


;; Debugging/Logging.
;; ------------------
;; Some simple facilities to selectively debug code.

(defvar magiciel--*debug-low-level* 0
  "The minimum debug level.")
(defvar magiciel--*debug-mid-level* 5
  "The middle debug level.")
(defvar magiciel--*debug-top-level* 9
  "The maximum debug level.")

(defvar magiciel--*debug-level* magiciel--*debug-low-level*)
(defvar magiciel--*current-debug-level* magiciel--*debug-level*
  "The current debug level.

Change with `magiciel--set-debug-level' or bay means of `with-debug-level'.

See Also:

`magiciel--set-debug-level' and `with-debug-level'.")


(defun magiciel--set-debug-level (l)
  "Set the current debug level to L.

See Also:

`magiciel--*current-debug-level*'"
  (setq magiciel--*current-debug-level*
	(magiciel--clamp-debug-level l)))


(cl-defun magiciel--msg (level fmt &rest fmt-args)
  "Conditionallly call `message' with FMT and FMT-ARGS.

The call happens only if LEVEL is comprised between 0 and
`magiciel--*current-debug-level*'.

See Also:

`magiciel--*current-debug-level*'."
  (when (<= 0 level magiciel--*current-debug-level*)
    (apply #'magiciel--info fmt fmt-args)))


(cl-defun magiciel--warn (level fmt &rest fmt-args)
  "Conditionallly call `magiciel--warning' with FMT and FMT-ARGS.

The call happens only if LEVEL is comprised between 0 and
`magiciel--*current-debug-level*'.

See Also:

`magiciel--*current-debug-level*', `magiciel-warining'."
  (when (<= 0 level magiciel--*current-debug-level*)
    (apply #'magiciel--warning fmt fmt-args)))


(cl-defun magiciel--err (level fmt &rest fmt-args)
    "Conditionallly call `magiciel--error' with FMT and FMT-ARGS.

The call happens only if LEVEL is comprised between 0 and
`magiciel--*current-debug-level*'.

See Also:

`magiciel--*current-debug-level*', `magiciel-error'."
  (when (<= 0 level magiciel--*current-debug-level*)
    (apply #'magiciel--error fmt fmt-args)))


(cl-defmacro with-debug-level (l &body code)
  "Locally set the current debug level to L while executing CODE."
  `(let ((magiciel--*current-debug-level* ,l))
     ,@code))


(defun magiciel--clamp-debug-level (n)
  "Ensure that the debug level N is neither too low or too high.

See Also:

`magiciel--*debug-top-level'."
  (min (max 0 n) magiciel--*debug-top-level*))


;; Saving code to files

(cl-defun magiciel--save-code (code filename &optional append)
  "Save CODE (a S-expr) to file named FILENAME.

Just append CODE to FILENAME if APPEND is not nil."
  (with-temp-buffer
    (pp-emacs-lisp-code code)
    (if append
	(append-to-file (point-min) (point-max) filename)
      (write-file filename))))


;; Yeah, yeah, yeah.  Just bear with me and enjoy the curries.

(defun magiciel--curry1 (f arg)
  "Curry function F fixing the first ARG."
  #'(lambda (x) (apply f arg (list x))))


(defun magiciel--curry2 (f arg1 arg2)
  "Curry function F fixing the first two arguments, ARG1 and ARG2."
  #'(lambda (x) (apply f arg1 arg2 (list x))))


;; Epilogue.
;; ---------

(provide 'magiciel-utilities)

;;; utilities.el ends here
