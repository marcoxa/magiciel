;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; compile-magic.el


;;; Commentary:
;;
;; Compile magic records to guess a file type.
;;
;; Notes:
;;
;; 2024-02-12:
;; The interesting thing about the magic(5) file syntax is that the
;; kludge encrusting is a sign of... whatever.
;;
;; Basically you have the "type" field in the record, *and* you
;; have different, but essentially semantically the same, syntax
;; in indirect addresses to read a value in the byte stream.
;;
;; Plus the "magic(5)" man page is not up-to-date anywhere (Ubuntu
;; and MacOS).  E.g., there are a couple of obvious mistakes; cfr, the
;; use of `.' and `,' in indirect addresses, and, always in indirect
;; addresses the type specifier `l' which is not mentioned in the text
;; at all, especially in the summary table.
;;
;; Luckily there is a BNF spec in the main "file" repository.
;;
;; Oh, well!


;;; Code:

(require 'cl-lib)
(require 'magiciel-parse-magic)


(cl-defun magiciel-compile-magic (magic &optional (steps 1))
  (message "\nMAGICIEL: compiling magic...\n")
  (let ((code-recs
	 (cl-remove-if #'(lambda (r)
			   (or (magiciel--magic-record-is-comment r)
			       (magiciel--magic-record-is-empty r)))
		       (magiciel--magic-records magic))
	 )
	)
    ;; Assume records are in proper order.

    ;; Step 1: group records.
    ;; A group (a list) is a 0 offset level record, followed by
    ;; records at offset level 1 or more and by special ones.

    (let ((recs-groups ()))
      (cl-labels
	  ((start (recs)
	     (process-normal-rec recs ()))

	   (finish ()
	     recs-groups
	     )
	     
	   (process-normal-rec (recs current-group &aux (r (car recs)))
	     (message "MAGICIEL: process-normal-rec: %S." r)
	     (message "MAGICIEL: process-normal-rec: current group size %d.\n"
		      (length current-group))
	     (if (null recs)
		 ;; Done.
		 (finish)
	       (cond ((magiciel--magic-record-is-normal r)

		      (if (zerop
			   (magiciel--offset-level
			    (magiciel--magic-record-offset r)))
			  (progn
			    (when current-group
			      (message "MAGICIEL: current group size %d."
				       (length current-group))
			      (setf recs-groups
				    (append recs-groups
					    (list current-group))))
			    (process-normal-rec (cl-rest recs) (list r)))
			;; Continue with current group...
			(process-normal-rec (cl-rest recs)
					    (append current-group (list r)))))

		     ((magiciel--magic-record-is-special r)
		      (process-special-rec recs current-group))

		     (t
		      (error "MAGICIEL: unexpected record %S." r))
		     )
	       )
	     )				; process-normal-rec

	   (process-special-rec (recs current-group &aux (r (car recs)))
	     (if (null recs)
		 ;;
		 (finish)
	       (cond ((magiciel--magic-record-is-normal r)
		      (process-normal-rec recs current-group))

		     ((magiciel--magic-record-is-special r)
		      (process-special-rec (cl-rest recs)
					   (append current-group (list r))))

		     (t
		      (error "MAGICIEL: unexpected record %S." r))
		     )
	       )
	     )
	   )				; cl-labels


	(message "\nMAGICIEL: grouping records.")
	(start code-recs)
	(when (<= steps 1)
	  (cl-return-from magiciel-compile-magic recs-groups))

	;; recs-groups


	;; Step 2: compile each record in each group.
	;; First 0-level test in each group is then treated specially.
	
	(message "\nMAGICIEL: compiling record groups.")
	(cl-loop for g in recs-groups
		 collect (mapcar #'magiciel--compile-record  g)
		 ))
      )))


(cl-defgeneric magiciel--select-op (op type))


(cl-defmethod magiciel--select-op ((op symbol) (type symbol))
  ;; Kitchen sink (with a very large hole).
  (cl-assert (member type '(number string date)))

  op)


;; The next one is trickier as I am kludging this.

(cl-defmethod magiciel--select-op ((op (eql '!)) (type symbol))
  (ignore op)			; ELisp is a bit stupid here.

  (cond ((cl-typep type 'magiciel--numeric-type-spec)
	 '/=)

	;; Assume "dates" are numeric.
	
	((cl-typep type 'magiciel--date-type-spec)
	 '/=)
	
	((cl-typep type 'magiciel--string-type-spec)
	 '(lambda (s1 s2)
	    (not (string-equal s1 s2))))
	
	(t
	 (error "MAGICIEL: Cannot select a negation operator for type %s."
		type)))
  )


;; `number' is not a "class".
;; (cl-defmethod magiciel--select-op ((op (eql '!)) (type 'number))
;;   (ignore op type)
;;  '/=)


(cl-defmethod magiciel--select-op ((op (eql '=)) (type (eql 'string)))
  (ignore op type)			; ELisp is a bit stupid here.
  'string-equal)

(cl-defmethod magiciel--select-op ((op (eql '>)) (type (eql 'string)))
  (ignore op type)			; ELisp is a bit stupid here.
  'string-greaterp)

(cl-defmethod magiciel--select-op ((op (eql '<)) (type (eql 'string)))
  (ignore op type)			; ELisp is a bit stupid here.
  'string-lessp)

(cl-defmethod magiciel--select-op ((op (eql '!)) (type (eql 'string)))
  (ignore op type)			; ELisp is a bit stupid here.
  ;; This works as this results is put in a macro expansion.
  '(lambda (s1 s2) (not (string-equal s1 s2)))
  )


;; Date operators assume "dates" are numeric.

(cl-defmethod magiciel--select-op ((op (eql '=)) (type (eql 'date)))
  (ignore op type)			; ELisp is a bit stupid here.
  '=)

(cl-defmethod magiciel--select-op ((op (eql '>)) (type (eql 'date)))
  (ignore op type)			; ELisp is a bit stupid here.
  '>)


(cl-defmethod magiciel--select-op ((op (eql '<)) (type (eql 'date)))
  (ignore op type)			; ELisp is a bit stupid here.
  '<)

(cl-defmethod magiciel--select-op ((op (eql '!)) (type (eql 'date)))
  (ignore op type)			; ELisp is a bit stupid here.
  '/=)


;; magiciel--compile-record
;; Generate a lambda that does sets up all the necessary
;; addressing and reading.
;;
;; The generated lambda takes two arguments, a "sequence" of bytes
;; (i.e., a "unibyte" string in Emacs Lisp parlance) called `bytes'
;; and a position in said string, called `pos'.

(cl-defun magiciel--compile-record (r)
  (message "\nMAGICIEL: compiling record %S." r)

  (unless (magiciel--magic-record-p r)
    (cl-return-from magiciel--compile-record nil))
  
  (cl-assert (magiciel--magic-record-is-normal r))
  (let ((offset (magiciel--magic-record-offset r))
	(type   (magiciel--magic-record-type r))
	(test   (magiciel--magic-record-test r))
	(msg    (magiciel--magic-record-message r))
	)
    
    (cl-labels
	((compile-offset (offset)
	   (cl-etypecase offset
	     (magiciel--direct-offset
	      (if (magiciel--direct-offset-cont offset)
		  `(+ pos ,(magiciel--direct-offset-value offset))
		(magiciel--direct-offset-value offset))
	      )
	     (magiciel--indirect-offset
	      (if (magiciel--indirect-offset-cont offset)
		  `(+ pos ,(compile-indirect-offset offset))
		(compile-indirect-offset offset))
	      )
	     )
	   )

	 (compile-indirect-offset (ind-off)
	   ;; FTTB
	   (cl-destructuring-bind (ind-addr ind-subtype)
	       (compile-ind-offset-type ind-off)
	     (cl-destructuring-bind (ind-op ind-sign ind-num)
		 (compile-ind-offset-modifier ind-off)
	       (ignore ind-sign)
	       (let ((type-spec
		      (magiciel--offset-type-spec-spec ind-subtype))
		     (type-endian
		      (magiciel--offset-type-spec-endian ind-subtype))
		     (type-len
		      (magiciel--offset-type-spec-byte-len ind-subtype))
		     (type-sign
		      (magiciel--offset-type-spec-unsigned ind-subtype))
		     ;; (test-op  (magiciel--test-op test))
		     ;; (test-val (magiciel--test-value test))
		     )
		 `(,ind-op
		   ,(if (magiciel--indirect-offset-rel ind-off)
			`(magiciel--get-value-at* bytes
						  (+ pos ,ind-addr)
						  ',type-spec
						  ',type-sign
						  ',type-endian
						  ()
						  ',type-len
						  )
		      `(magiciel--get-value-at* bytes
						,ind-addr
						',type-spec
						',type-sign
						',type-endian
						()
						',type-len
						))
		   ,(if (listp ind-num)
			(if (magiciel--indirect-offset-rel ind-off)
			    `(magiciel--get-value-at
			      (+ pos
				 ,ind-addr
				 ,ind-num)
			      ',ind-subtype)
			  `(magiciel--get-value-at
			    (+ ,ind-addr ,ind-num)))
		      ind-num)))
	       )))

	 ;; (compile-ind-offset-type (ind-off)
	 ;;   ;; Cfr. the UN*X manual entry for "magic(5)".
	 ;;   (let ((ind-addr (magiciel--indirect-offset-addr ind-off))
	 ;; 	 (ind-type (magiciel--indirect-offset-type ind-off))
	 ;; 	 (sign 'unsigned)

	 ;; 	 ;; `subtype' defaults to `long' little endian. Unused
	 ;; 	 ;; for 'byte'.
	 ;; 	 (subtype '(long 8 :le))
	 ;; 	 )
	 ;;     (when (cl-plusp (length ind-type)) ; Should be 2.
	 ;;       (cond ((char-equal ?, (aref ind-type 0))
	 ;; 	      (setf sign 'signed))
	 ;; 	     ((char-equal ?. (aref ind-type 0)))
	 ;; 	     (t
	 ;; 	      (error "MAGICIEL: incorrect sign '%c' in %S."
	 ;; 		     (aref ind-type 0)
	 ;; 		     ind-type))
	 ;; 	     )
	 ;;       (setf subtype (magiciel--addr-select-subtype (aref ind-type 1)))
	 ;;       )
	 ;;     (list ind-addr (cons sign subtype))))

	 
	 (compile-ind-offset-type (ind-off)

	   ;; New verision.
	   
	   (let ((ind-addr (magiciel--indirect-offset-addr ind-off))
		 (ind-type (magiciel--indirect-offset-type ind-off))
		 )

	     ;; `ind-type' is a `magiciel--offset-type-spec'.

	     (list ind-addr ind-type)))


	 (compile-ind-offset-modifier (ind-off)
	   (let ((ind-op     (magiciel--indirect-offset-op   ind-off))
		 (ind-is-len (magiciel--indirect-offset-is-len ind-off))
		 (ind-sign   (magiciel--indirect-offset-sign ind-off))
		 (ind-num    (magiciel--indirect-offset-num  ind-off))
		 )

	     ;; Should move these in parsing.
	     
	     (setf ind-op (select-op ind-op)

		   ind-sign (pcase ind-sign
			      ((or "" "+") '+)
			      (_ '-))

		   ind-num (if ind-is-len
			       `(,ind-sign ,ind-num)
			     ind-num)
		   )

	     (list ind-op ind-sign ind-num)
	     ))

	 ;; (select-op (op)
	 ;;   ;; I could use `intern', but I need to get the ELisp
	 ;;   ;; operators.
	 ;;   (pcase op
	 ;;     ("-" '-)
	 ;;     ("+" '+)
	 ;;     ("*" '*)
	 ;;     ("/" '/)
	 ;;     ("%" 'mod)
	 ;;     ("-" '-)
	 ;;     ("&" 'logand)
	 ;;     ("|" 'logior)
	 ;;     ("^" 'logxor)		; Check the `logior'..
	 ;;     (_ '+)			; Really nothing, as number
	 ;; 				; will be "", hence 0.
	 ;;     )
	 ;;   )

	 (select-op (op)
	   ;; `op' should be a symbol.
	   (pcase op
	     ((or '- '+ '* '/) op)
	     ('% 'mod)
	     ('& 'logand)
	     ('| 'logior)
	     ('^ 'logxor)		; Check the `logior'..
	     (_ '+)			; Really nothing, as number
					; will be "", hence 0.
	     )
	   )


	 (select-test-op (op type-spec)
	   ;; Made it into a generic function.

	   (magiciel--select-op op type-spec)
	     
	   )
	 

	 (compile-test-value (test-value type-spec)
	   (if (magiciel--numeric-type-spec-p type-spec)
	       (car (read-from-string (magiciel--lispify-number-string test-value)))
	     test-value)
	   )

	 
	 (compile-test (off-op type test)
	   (let ((type-spec   (magiciel--record-type-spec-spec type))
		 (type-mods   (magiciel--record-type-spec-modifiers type))
		 (type-endian (magiciel--record-type-spec-endian type))
		 (type-len    (magiciel--record-type-spec-byte-len type))
		 (type-sign   (magiciel--record-type-spec-unsigned type))
		 (test-op  (magiciel--test-op test))
		 (test-val (magiciel--test-value test))
		 )

	     (cond ((magiciel--numeric-type-spec-p type-spec)
		    `(,(select-test-op test-op 'number)
		      (magiciel--get-value-at* bytes
					       pos
					       ',type-spec
					       ',type-sign
					       ',type-endian
					       ',type-mods
					       ',type-len
					       ,off-op)
		      ',(compile-test-value test-val type-spec)))

		   ((magiciel--date-type-spec-p type-spec)
		    `(,(select-test-op test-op 'date)
		      (magiciel--get-value-at* bytes
					       pos
					       ',type-spec
					       ',type-sign
					       ',type-endian
					       ',type-mods
					       ',type-len
					       ,off-op)
		      ',test-val))	; A string, but it must be double checked..

		   ((magiciel--string-type-spec-p type-spec)
		    `(,(select-test-op test-op 'string)
		      (magiciel--get-value-at* bytes
					       pos
					       ',type-spec
					       ',type-sign
					       ',type-endian
					       ',type-mods
					       ',type-len
					       ,off-op)
		      ',test-val))	; Already a string.
		   ))
	   )
	 )				; cl-labels

      (let* ((off-op  (compile-offset offset))
	     (test-op (compile-test off-op type test))
	     )
	`(lambda (bytes pos)
	   (when ,test-op ,(magiciel--message-msg-string msg)))
	)
      )))


;; The next function and its use cases will be moved in parsing.
;; Hence it will disapper soon.

(defun magiciel--addr-select-subtype (subtype-char)

  ;; The "subtype-char" comes from the indirect address specification.
  ;; Value is a list/tuple (<type> <size> <endiannes>).

  (cl-ecase subtype-char
    ((?b ?c ?B ?C) '(byte 1 :le))
    ((?e ?f ?g) '(double-float 8 :le))	; Let's be Common-istic.
    ((?E ?F ?G) '(double-float 8 :be))
    (?l '(long 8 :le))
    (?L '(long 8 :be))
    ((?h ?s) '(short 2 :le))
    ((?H ?S) '(short 2 :be))
    (?i '(id3 4 :le))
    (?I '(id3 4 :be))
    (?m '(middle 4 :me))
    (?q '(quad 8 :le))
    (?Q '(quad 8 :be))
    
    ;; Other missing according to latest manual.
    
    ))


(defun magiciel--select-type (type)

  ;; Interpret the "type-field".

  ;; Nothing FTTB, apart from a check.
  ;; Parsing already takes care of this; this function will disappear
  ;; soon.

  (cl-assert (magiciel--record-type-spec-p type))
  type)


(provide 'magiciel-compile-magic)

;;; compile-magic.el ends here
