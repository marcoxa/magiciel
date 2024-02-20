;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; magic-bits.el


;;; Commentary:
;;
;; Low level bits and byte handling.


;;; Code:

(require 'cl-lib)
(require 'bindat)


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
  (let* ((unpacker
	  (cl-ecase endianness
	    (:le (cl-ecase n-bytes
		   (1 magiciel--*uint8le-bindat*)
		   (2 magiciel--*uint16le-bindat*)
		   (3 magiciel--*uint24le-bindat*)
		   (4 magiciel--*uint32le-bindat*)
		   (8 magiciel--*uint64le-bindat*)
		   ))
	    (:be (cl-ecase n-bytes
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
      (error "MAGICIEL: no int unpacked."))))


(cl-defun magiciel--get-sint (binary-data
			     loc
			     &optional
			     (n-bytes 1)
			     (endianness :le)
			     )
  (let* ((unpacker
	  (cl-ecase endianness
	    (:le (cl-ecase n-bytes
		   (1 magiciel--*sint8le-bindat*)
		   (2 magiciel--*sint16le-bindat*)
		   (3 magiciel--*sint24le-bindat*)
		   (4 magiciel--*sint32le-bindat*)
		   (8 magiciel--*sint64le-bindat*)
		   ))
	    (:be (cl-ecase n-bytes
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
      (error "MAGICIEL: no int unpacked."))))



(cl-defun magiciel--get-value-at* (bytes
				  pos
				  type-spec
				  type-sign
				  type-endian
				  type-mods
				  type-len)
  (cl-assert (not (multibyte-string-p bytes)))
  )
				  
				 


(provide 'magiciel-magic-bits)

;; magic-bits.el ends here

