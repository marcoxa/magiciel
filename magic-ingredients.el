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
;; (require 'bitpack)			; Getting ready for it.


;; magiciel-endianness

(cl-deftype magiciel-endiannes ()
  "Type of \\='endianness\\=' indicators."
  '(member :le :be :m :me :< :>))


;; magiciel magic(5) "type" specifiers.
;;
;; Notes:
;; 2025-10-26: the "real" type spec, should become "float".

(defconst magiciel--+integer-type-specs+
  '(number				; "number" is not in the
					; specs, but it is useful to
					; have it here.
    byte
    short beshort leshort
    long  belong  lelong melong
    quad  bequad  lequad
    )
  "List of known \"magic(5)\" integer specifiers.")


(defconst magiciel--+real-type-specs+
  '(float
    befloat lefloat
    
    double
    bedouble ledouble
    )
  "List of known \"magic(5)\" real specifiers." )


(cl-deftype magiciel--integer-type-spec ()
  "Type of integer type specifiers."
  `(member ,@magiciel--+integer-type-specs+))


(cl-deftype magiciel--real-type-spec ()
  "Type of real type specifiers."
  `(member ,@magiciel--+real-type-specs+))


(cl-deftype magiciel--numeric-type-spec ()
  "Type of numeric type specifiers."
  '(or magiciel--real-type-spec magiciel--integer-type-spec))


(defun magiciel--integer-type-spec-p (x)
  "Return non-nil if X is a `magiciel--integer-type-spec'."
  (and (symbolp x)
       (cl-typep x 'magiciel--integer-type-spec)))


(defun magiciel--real-type-spec-p (x)
  "Return non-nil if X is a `magiciel--real-type-spec'."
  (and (symbolp x)
       (cl-typep x 'magiciel--real-type-spec)))


(defun magiciel--numeric-type-spec-p (x)
  "Return non-nil if X is a `magiciel--numeric-type-spec'."
  (and (symbolp x)
       (cl-typep x 'magiciel--numeric-type-spec)))


(defconst magiciel--+date-type-specs+
  '(date bedate ledate medate
	 ldate beldate leldate meldate
	 qdate beqdate leqdate
    	 qldate beqldate leqldate
	 qwdate beqwdate leqwdate

	 ;; Added after looking at C code
	 msdosdate lemsdosdate bemsdosdate
	 msdostime lemsdostime bemsdostime
	 )
  "List of known \"magic(5)\" \\=`date\\=' specifiers.")

(cl-deftype magiciel--date-type-spec ()
  "Type of date type specifiers."
  `(member ,@magiciel--+date-type-specs+))

(defun magiciel--date-type-spec-p (x)
  "Return non-nil if X is a `magiciel--date-type-spec'."
  (and (symbolp x)
       (cl-typep x 'magiciel--date-type-spec)))
  

(defconst magiciel--+string-type-specs+
  '(string
    pstring
    bestring16
    lestring16
    guid
    )
  "List of known \"magic(5)\" \\=`string\\=' specifiers.")

(cl-deftype magiciel--string-type-spec ()
  "Type of date type specifiers."
  `(member ,@magiciel--+string-type-specs+))

(defun magiciel--string-type-spec-p (x)
  "Return non-nil if X is a `magiciel--string-type-spec'."
  (and (symbolp x)
       (cl-typep x 'magiciel--string-type-spec)))


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
  "List of known \"magic(5)\" \\=`string\\=' specifiers.

These \"type\" specifications have an effect on tests' executions.")

(cl-deftype magiciel--exec-type-spec ()
  "Type of \\='exec\\=' type specifiers."
  `(member ,@magiciel--+exec-type-specs+))

(defun magiciel--exec-type-spec-p (x)
  "Return non-nil if X is a `magiciel--exec-type-spec'."
  (and (symbolp x)
       (cl-typep x 'magiciel--exec-type-spec)))


(defun magiciel--type-spec-p (x)
  "Return non-nil if X is a \\='magiciel\\=' type spec."
  (or (magiciel--numeric-type-spec-p x)
      (magiciel--date-type-spec-p x)
      (magiciel--string-type-spec-p x)
      (magiciel--exec-type-spec-p x)))


(defun magiciel--type-spec-size (ts)
  "Return the size in bytes of type specification TS.

The result is the number of octets TS uses."
  (unless (magiciel--type-spec-p ts)
    (magiciel--error "%s is not a valid type specifier" ts))

  (cl-ecase ts
    (byte 1)
    ((short beshort leshort) 2)
    ((msdosdate bemsdosdate lemsdosdate msdostime bemsdostime lemsdostime) 2)
    ((long lelong belong melong) 4)
    ((date ledate bedate medate ldate leldate beldate meldate) 4)
    ((float lefloat befloat) 4)
    ((beid3 leid3) 4)
    ((quad lequad bequad
	   qdate leqdate beqdate
	   qldate leqldate beqldate
	   qwdate leqwdate beqwdate)
     8)
    ((double ledouble bedouble) 8)
    ((offset bevarint levarint) 8)
    (guid 16)
    ))


;; Some string and regexp utilities.
;; ---------------------------------

(defun magiciel--count-nonmagic (str)
  "Compute the real length of a \"magic(5)\" expression STR.

The purposes is to determine how \"strong\" a magic expression is
\(approximating how specific its matches are):
 - magic characters count 0 unless escaped.
 - [] expressions count 1
 - {} expressions count 0
 - regular characters or escaped magic characters count 1
 - 0 length expressions count as one

STR is a string.

Notes:

This function is patterned after the C code."

  (cl-loop with rv = 0
	   with sl = (length str)
	   for i from 0 below sl
	   do
	   (magiciel--msg 1 "\n>> checking %d %c (%d)" i (aref str i) rv)
	   (cl-case (aref str i)
	     (?\\
	      ;; (when (= (1+ i) sl) (cl-decf i))
	      (cl-incf rv)
	      (magiciel--msg 1 ">> %d %c (%d)\n" i (aref str i) rv))

	     ((?? ?* ?. ?+ ?^ ?$)	; Do nothing
	      )

	     (?\[
	      (let ((close-sq-pos (cl-position ?\] str :start i)))
		(magiciel--msg 1 ">> ] pos %s" close-sq-pos)
		(if close-sq-pos
		    (setf i (1- close-sq-pos))
		  (setf i (1- sl))))
	      (magiciel--msg 1 ">> %d %c (%d)" i (aref str i) rv))

	     (?\{
	      (let ((close-sb-pos (cl-position ?\} str :start i)))
		(magiciel--msg 1 ">> } pos %s" close-sb-pos)
		(if close-sb-pos
		    (setf i close-sb-pos)
		  (setf i (1- sl))))
	      (magiciel--msg 1 ">> %d %c (%d)" i (aref str i) rv))

	     (t
	      (magiciel--msg 1 ">> %d %c default (%d)" i (aref str i) rv)
	      (cl-incf rv)))

	   finally (cl-return (if (zerop rv) 1 rv))
	   ))


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
  `(satisfies ,(magiciel--curry1 'magiciel--sb-test bits)))


(defun magiciel--sb-test (bits x)
  "Test that X can fit in BITS."

  (and (integerp x)
       (let ((bound (expt 2 (1- bits)))) ; Check the bounds.
	 (<= (- bound) x bound))))


(cl-deftype magiciel-signed-byte* (&optional (bits 8) (endian :le))
  "Type of \\='signed-byte\\=' numbers with endianness.

BITS is the length defaulting to 8, and ENDIAN is a
`magiciel-endianness'."
  `(satisfies ,(magiciel--curry2 'magiciel--sbe-test bits endian)))


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
  `(satisfies ,(magiciel--curry1 'magiciel--ub-test bits)))


(defun magiciel--ub-test (bits x)
  "Test that the (unsigned) X is between 0 and BITS."
  (and (integerp x)
       (let ((bound (expt 2 bits)))	; Check the bounds.
	 (<= 0 x bound))))


(cl-deftype magiciel-signed-byte* (&optional (bits 8) (endian :le))
  "Type of \\='unsigned-byte\\=' numbers with endianness.

BITS is the length defaulting to 8, and ENDIAN is a
`magiciel-endianness'."
  `(satisfies ,(magiciel--curry2 'magiciel--sbe-test bits endian)))


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
