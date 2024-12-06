;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; magic-bits.el


;;; Commentary:
;;
;; Low level bits and byte handling.


;;; Code:

(require 'cl-lib)
(require 'bindat)
(require 'magic-ingredients)


;; Lifted and expanded from `xmit'.


;; Useful unpacking of binary bits.

;; Network order, big endian unsigned ints.

(defvar magiciel--*uint8-bindat*
  (bindat-type (int u8)))


(defvar magiciel--*uint16-bindat*
  (bindat-type (int uint 16)))


(defvar magiciel--*uint24-bindat*
  (bindat-type (int uint 24)))


(defvar magiciel--*uint32-bindat*
  (bindat-type (int uint 32)))


(defvar magiciel--*uint64-bindat*
  (bindat-type (int uint 64)))


;; Little endian unsigned ints.

(defvar magiciel--*uint8le-bindat*
  (bindat-type (int u8)))


(defvar magiciel--*uint16le-bindat*
  (bindat-type (int uint 16 t)))


(defvar magiciel--*uint24le-bindat*
  (bindat-type (int uint 24 t)))


(defvar magiciel--*uint32le-bindat*
  (bindat-type (int uint 32 t)))


(defvar magiciel--*uint64le-bindat*
  (bindat-type (int uint 64 t)))


;; Network order, big endian signed ints.

(defvar magiciel--*sint8-bindat*
  (bindat-type (int sint 8 nil)))


(defvar magiciel--*sint16-bindat*
  (bindat-type (int sint 16 nil)))


(defvar magiciel--*sint24-bindat*
  (bindat-type (int sint 24 nil)))


(defvar magiciel--*sint32-bindat*
  (bindat-type (int sint 32 nil)))


(defvar magiciel--*sint64-bindat*
  (bindat-type (int sint 64 nil)))


;; Little endian signed ints.

(defvar magiciel--*sint8le-bindat*
  (bindat-type (int sint 8 t)))


(defvar magiciel--*sint16le-bindat*
  (bindat-type (int sint 16 t)))


(defvar magiciel--*sint24le-bindat*
  (bindat-type (int sint 24 t)))


(defvar magiciel--*sint32le-bindat*
  (bindat-type (int sint 32 t)))


(defvar magiciel--*sint64le-bindat*
  (bindat-type (int sint 64 t)))


(cl-defun magiciel--get-uint (binary-data
			     loc
			     &optional
			     (n-bytes 1)
			     (endianness :le)
			     )
    "Read an unsigned integer from BINARY-DATA starting at position LOC.

An unsigned integer of length N-BYTES is read in, also according to
the ENDIANNESS parameter (either `:le' or `:be')."
  (let* ((unpacker
	  (cl-ecase endianness
	    ((:le :<) (cl-ecase n-bytes
			(1 magiciel--*uint8le-bindat*)
			(2 magiciel--*uint16le-bindat*)
			(3 magiciel--*uint24le-bindat*)
			(4 magiciel--*uint32le-bindat*)
			(8 magiciel--*uint64le-bindat*)
			))
	    ((:be :>) (cl-ecase n-bytes
			(1 magiciel--*uint8-bindat*)
			(2 magiciel--*uint16-bindat*)
			(3 magiciel--*uint24-bindat*)
			(4 magiciel--*uint32-bindat*)
			(8 magiciel--*uint64-bindat*)
			))
	    ))
	 (i (bindat-unpack unpacker binary-data loc))
	 )
    (if i
	(cdar i)
      (error "MAGICIEL: no unsigend int unpacked"))))


(cl-defun magiciel--get-sint (binary-data
			     loc
			     &optional
			     (n-bytes 1)
			     (endianness :le)
			     )
  "Read a signed integer from BINARY-DATA starting at position LOC.

A signed integer of length N-BYTES is read in, also according to the
ENDIANNESS parameter (either `:le' or `:be')."
  (let* ((unpacker
	  (cl-ecase endianness
	    ((:le :<) (cl-ecase n-bytes
			(1 magiciel--*sint8le-bindat*)
			(2 magiciel--*sint16le-bindat*)
			(3 magiciel--*sint24le-bindat*)
			(4 magiciel--*sint32le-bindat*)
			(8 magiciel--*sint64le-bindat*)
			))
	    ((:be :>) (cl-ecase n-bytes
			(1 magiciel--*sint8-bindat*)
			(2 magiciel--*sint16-bindat*)
			(3 magiciel--*sint24-bindat*)
			(4 magiciel--*sint32-bindat*)
			(8 magiciel--*sint64-bindat*)
			))
	    ))
	 (i (bindat-unpack unpacker binary-data loc))
	 )
    (if i
	(cdar i)
      (error "MAGICIEL: no int unpacked"))))


;; magiciel--*invert-endianness*

(defvar magiciel--*invert-endianness* nil
  "This is a flag to be used at run time.

If T the \"endiannes\" of accesses and tests is inverted.  That is, if
the endianness is `:le' and the value of
`magiciel--*invert-endianness*' is T, then accesses and tests are done
as if the endianness were big-endian (`:be').

This is necessary because of the quirks of the `use' type of
instructions in the \"magic(5)\" specification."
  )


;; magiciel--use-endianness

(cl-defun magiciel--use-endianness (&optional
				    (endianness
				     (magiciel--machine-endiannes)))
  "Return the ENDIANNESS to use in accesses.

It has an effect only within \"name\" instruction groups as a
consequence of their \"use\".  Cf., the \"magic(5)\" specification for
this.

Notes:

This function is needed to dynamically account for the \"use\"
specification."
  (defvar magiciel--*invert-endianness*)

  (let ((e
	 (cl-ecase endianness
	   ((:> :be) (if magiciel--*invert-endianness* :le :be))
	   ((:< :le) (if magiciel--*invert-endianness* :be :le))
	   ((:m :me) :me)
	   ))
	)
    (magiciel--msg 1 "using endianness %s (was %s)" e endianness)
    e))


;; magiciel--get-value-at*
;; Kitchen sink function.
;; Eventually low level access will be compiled.


(cl-defun magiciel--get-value-at* (bytes ; 1
				   pos	 ; 2
				   type-spec ; 3
				   unsigned  ; 4
				   endianness ; 5
				   mods	      ; 6
				   len	      ; 7
				   &optional
				   (off-op 0)
				   )

  "Dispatches on the proper type of value to read, according to TYPE-SPEC.

The function reads a value from BYTES (a vector or a buffer), starting
at position POS, depending on TYPE-SPEC (a number, a date or a
string).

The arguments UNSIGNED and ENDIANNESS control how the value is read
in.  For string and date values, LEN bytes are read in.

MODS, if not NIL, must be of the form (<op> <arg>), where
<op> and <arg> are strings."
  
  ;; The handling of MODS is done here even if a bit inefficient.
  ;; Only numeric types modifiers are considered.
  
  (cl-assert (not (multibyte-string-p bytes)))

  ;; Note that the coding-system is assumend to be ... "the usual one".
  (cl-etypecase type-spec
    (magiciel--numeric-type-spec
     (let ((mod-op  (cl-first mods))
	   (mod-arg (cl-second mods))
	   (v (if unsigned
		  (magiciel--get-uint bytes
				      pos
				      len
				      (magiciel--use-endianness endianness))
		(magiciel--get-sint bytes
				    pos
				    len
				    (magiciel--use-endianness endianness))
		))
	   )
       (if (and mod-op (string= "&" mod-op) mod-arg)
	   (logand (magiciel--convert-c-integer mod-arg) v)
	 v)
       ))
    
    (magiciel--date-type-spec
     (substring-no-properties bytes pos (+ pos len))
     )
    
    (magiciel--string-type-spec
     (substring-no-properties bytes pos (+ pos len))
     )
    ))


;;; Epilogue:

(provide 'magiciel-magic-bits)

;;; magic-bits.el ends here

