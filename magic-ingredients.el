;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; magic-ingredients.el


;;; Commentary:
;;
;; The offset and test concoctions require a "type" (ingredient)
;; sublanguage.  Of course, the Common Lisp type system provides a
;; good starting point.
;;
;; The types we define will be used just as "representations", to
;; denote specialized low level representations.
;;
;; Notes:
;;
;; Ok.  There is a tricky definition of what is a "byte" and what
;; "endianness" means w.r.t. bytes and sequences of bytes.


;;; Code:

(require 'cl-lib)
(require 'bindat)


;; magiciel-endianness

(cl-deftype magiciel-endiannes ()
  '(member :le :be :m :me))


;; magiciel magic(5) "type" specifiers.

(defconst magiciel--+numeric-type-specs+
  '(number				; "number" is not in the
					; specs, but it is useful to
					; have it here.
    byte
    short beshort leshort
    long  belong  lelong melong
    quad  bequad  lequad
    ))

(cl-deftype magiciel--numeric-type-spec ()
  `(member ,@magiciel--+numeric-type-specs+))


(defconst magiciel--+date-type-specs+
  '(date bedate ledate medate
	 ldate beldate leldate meldate
	 qdate beqdate leqdate
    	 qldate beqldate leqldate
	 ))

(cl-deftype magiciel--date-type-spec ()
  `(member ,@magiciel--+date-type-specs+))


(defconst magiciel--+string-type-specs+
  '(string
    pstring
    bestring16
    lestring16
    guid
    ))

(cl-deftype magiciel--string-type-spec ()
  `(member ,@magiciel--+string-type-specs+))


(defconst magiciel--+exec-type-specs+
  '(indirect
    name
    use
    regexp
    search
    default
    clear
    der
    offset
    )
  "These \"type\" specifications have an effect on tests' executions.")

(cl-deftype magiciel--exec-type-spec ()
  `(member ,@magiciel--+exec-type-specs+))


;; CL (numerical) type system basis.
;; ---------------------------------
;;
;; Note the "extra" bit/bytes types including endiannes.
;;
;; Notes:
;;
;; The following is not strictly necessary, plus, it uses non-kosher
;; things - a function, not a symbol - in the `satisfies' type
;; specifier.

(cl-deftype magiciel-signed-byte (&optional (bits 8))
  `(satisfies ,(curry1 'magiciel--sb-test bits)))


(defun magiciel--sb-test (bits x)
  (and (integerp x)
       (let ((bound (expt 2 (1- bits)))) ; Check the bounds.
	 (<= (- bound) x bound))))


(cl-deftype magiciel-signed-byte* (&optional (bits 8) (endian :le))
  `(satisfies ,(curry2 'magiciel--sbe-test bits endian)))


(defun magiciel--sbe-test (bits endian x)
  (ignore endian)			; FTTB.  TRT is to use `platel'.
  (magiciel--sb-test bits x))


(cl-deftype magiciel-unsigned-byte (&optional (bits 8))
  `(satisfies ,(curry1 'magiciel--ub-test bits)))


(defun magiciel--ub-test (bits x)
  (and (integerp x)
       (let ((bound (expt 2 bits)))	; Check the bounds.
	 (<= 0 x bound))))


(cl-deftype magiciel-signed-byte* (&optional (bits 8) (endian :le))
  `(satisfies ,(curry2 'magiciel--sbe-test bits endian)))


(defun magiciel--ube-test (bits endian x)
  (ignore endian)			; FTTB.  TRT is to use `platel'.
  (magiciel--ub-test bits x))


;; Other magic(5) "types" below: dates, strings, etc., etc.


;; Utilities.
;; ----------

;; Yeah, yeah, yeah.  Just bear with me and enjoy the curries.

(defun curry1 (f arg)
  #'(lambda (x) (apply f arg (list x))))


(defun curry2 (f arg1 arg2)
  #'(lambda (x) (apply f arg1 arg2 (list x))))


(provide 'magic-ingredients)

;;; magic-ingredients.el ends here
