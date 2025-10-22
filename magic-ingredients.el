;;; magiciel --- Guessing the content of a file in simple as possible way. -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; magic-ingredients.el
;;
;; See the file COPYING license and copyright information.


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
;; "endianness" means w.r.t.  bytes and sequences of bytes.


;;; Code:

(require 'cl-lib)
(require 'bindat)
(require 'bitpack)			; Getting ready for it.


;; magiciel-endianness

(cl-deftype magiciel-endiannes ()
  "Type of \\='endianness\\=' indicators."
  '(member :le :be :m :me :< :>))


;; magiciel magic(5) "type" specifiers.

(defconst magiciel--+integer-type-specs+
  '(number				; "number" is not in the
					; specs, but it is useful to
					; have it here.
    byte
    short beshort leshort
    long  belong  lelong melong
    quad  bequad  lequad
    )
  "List of known 'magic(5)' integer specifiers.")


(defconst magiciel--+real-type-specs+
  '(float
    befloat lefloat
    
    double
    bedouble ledouble
    )
  "List of known 'magic(5)' real specifiers." )


(cl-deftype magiciel--integer-type-spec ()
  "Type of integer type specifiers."
  `(member ,@magiciel--+integer-type-specs+))


(cl-deftype magiciel--real-type-spec ()
  "Type of real type specifiers."
  `(member ,@magiciel--+real-type-specs+))


(cl-deftype magiciel--numeric-type-spec ()
  "Type of numeric type specifiers."
  '(or magiciel--real-type-spec magiciel--integer-type-spec))


(defconst magiciel--+date-type-specs+
  '(date bedate ledate medate
	 ldate beldate leldate meldate
	 qdate beqdate leqdate
    	 qldate beqldate leqldate
	 qwdate beqwdate leqwdate
	 )
  "List of known 'magic(5)' \\=`date\\=' specifiers.")

(cl-deftype magiciel--date-type-spec ()
  "Type of date type specifiers."
  `(member ,@magiciel--+date-type-specs+))


(defconst magiciel--+string-type-specs+
  '(string
    pstring
    bestring16
    lestring16
    guid
    )
  "List of known 'magic(5)' \\=`string\\=' specifiers.")

(cl-deftype magiciel--string-type-spec ()
  "Type of date type specifiers."
  `(member ,@magiciel--+string-type-specs+))


(defconst magiciel--+exec-type-specs+
  '(indirect
    name
    use
    regex
    search
    default
    clear
    der
    offset
    )
  "List of known 'magic(5)' \\=`string\\=' specifiers.

These \"type\" specifications have an effect on tests' executions.")

(cl-deftype magiciel--exec-type-spec ()
  "Type of \\='exec\\=' type specifiers."
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
  "Type of \\='signed-byte\\=' numbers.

BITS is the length defaulting to 8."
  `(satisfies ,(curry1 'magiciel--sb-test bits)))


(defun magiciel--sb-test (bits x)
  "Test that X can fit in BITS."

  (and (integerp x)
       (let ((bound (expt 2 (1- bits)))) ; Check the bounds.
	 (<= (- bound) x bound))))


(cl-deftype magiciel-signed-byte* (&optional (bits 8) (endian :le))
  "Type of \\='signed-byte\\=' numbers with endianness.

BITS is the length defaulting to 8, and ENDIAN is a
`magiciel-endianness'."
  `(satisfies ,(curry2 'magiciel--sbe-test bits endian)))


(defun magiciel--sbe-test (bits endian x)
  "Test that X can fit in BITS, given ENDIAN.

Notes:

ENDIAN is actually ignored, and the current version of this test is
not that useful."
  (ignore endian)			; FTTB.  TRT is to use `platel'.
  (magiciel--sb-test bits x))


(cl-deftype magiciel-unsigned-byte (&optional (bits 8))
  "Type of \\='unsigned-byte\\=' numbers with endianness.

BITS is the length defaulting to 8, and ENDIAN is a
`magiciel-endianness'."
  `(satisfies ,(curry1 'magiciel--ub-test bits)))


(defun magiciel--ub-test (bits x)
  "Test that the (unsigned) X is between 0 and BITS."
  (and (integerp x)
       (let ((bound (expt 2 bits)))	; Check the bounds.
	 (<= 0 x bound))))


(cl-deftype magiciel-signed-byte* (&optional (bits 8) (endian :le))
  "Type of \\='unsigned-byte\\=' numbers with endianness.

BITS is the length defaulting to 8, and ENDIAN is a
`magiciel-endianness'."
  `(satisfies ,(curry2 'magiciel--sbe-test bits endian)))


(defun magiciel--ube-test (bits endian x)
  "Test that X is between 0 and BITS, given ENDIAN.

Notes:

ENDIAN is actually ignored, and the current version of this test is
not that useful."
  (ignore endian)			; FTTB.  TRT is to use `platel'.
  (magiciel--ub-test bits x))


;; Other magic(5) "types" below: dates, strings, etc., etc.


(provide 'magic-ingredients)

;;; magic-ingredients.el ends here
