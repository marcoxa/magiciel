;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; compile-magic.el


;;; Commentary:
;;
;; Compile magic records to guess a file type.
;;
;; The "compilation" is organized around four functions, each
;; compiling one of four "fields" of each record.
;; - `magiciel--compile-offset'
;; - `magiciel--compile-test'
;; - `magiciel--compile-test-value'
;; - `magiciel--compile-message'
;;
;; Of course, the last two are rather simple.
;; The results of these functions are combined by the function
;; `magiciel--compile-instr'.
;;
;; The "instructions" returned by the previous functions are combined
;; in "checks", which are the end, funcallable, result of the
;; "compilation".
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
;; Plus the "magic(5)" man page is not "up-to-date" anywhere (Ubuntu
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


(cl-defgeneric magiciel--compile-record (r)
  )

(cl-defgeneric magiciel--compile-offset (offset)
  )

(cl-defgeneric magiciel--compile-type (type)
  )

(cl-defgeneric magiciel--compile-test (test)
  )

(cl-defgeneric magiciel--compile-message (message)
  )

(cl-defgeneric magiciel--compile-instr (off-op type test result-msg)
  )

(cl-defgeneric magiciel--compile-access
    (type-spec unsigned endiennes mods len offset)
  )


;; magiciel--instruction
;;
;; The results of `magiciel--compile-record'.

(cl-defstruct (magiciel--instruction
	       (:constructor magiciel--make-instruction
			     (&key
			      offset
			      test
			      result
			      (level
			       (if (magiciel--offset-p offset)
				   ;; Paranoid.
				   (magiciel--offset-level offset)
				 0))
			      )))
  (level 0 :type integer :read-only t)
  offset
  test
  result
  )


(cl-defmethod cl-print-object ((mi magiciel--instruction) stream)
  (princ (format "#<MAGICIEL Instruction: (%d) %s %s %s>"
		 (magiciel--instruction-level mi)
		 (magiciel--instruction-offset mi)
		 (magiciel--instruction-test mi)
		 (magiciel--instruction-result mi))
	 stream))


;; magiciel-compile-magic

(cl-defun magiciel-compile-magic (magic &optional (steps 1))
  "Compiles a `magiciel-magic' structure."
  
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

    (let ((recs-groups ())
	  (comp-rec-groups ())
	  )
      (cl-labels
	  ((start (recs)
	     (process-magic-records recs ()))

	   (finish ()
	     (message "MAGICIEL: finishing.")
	     (setf recs-groups (nreverse recs-groups))
	     )

	   (process-magic-records (recs current-group  &aux (r (car recs)))
	     (message "MAGICIEL: process-magic-records: %d to go..."
		      (length recs))
	     
	     (cond ((null recs)
		    (push current-group recs-groups)
		    (finish))

		   ((magiciel--magic-record-is-normal r)
		    (process-normal-rec r (cl-rest recs) current-group))

		   ((magiciel--magic-record-is-special r)
		    (process-special-rec r (cl-rest recs) current-group))

		   (t 
		    (error "MAGICIEL: unexpected record %S." r))
		   )
	     )				; process-magic-records
	     
	   (process-normal-rec (r more-recs current-group)
	     (message "MAGICIEL: process-normal-rec: %S (len %d)."
		      r
		      (length current-group))

	     (cl-assert (magiciel--magic-record-is-normal r)) ; Paranoia.

	     (if (zerop (magiciel--offset-level
			 (magiciel--normal-record-offset r)))
		 (progn
		   (when current-group
		     (message "MAGICIEL: current group size %d."
			      (length current-group))
		     (push current-group recs-groups))
		   (process-magic-records more-recs (list r)))
	       ;; Continue with current group...
	       (process-magic-records more-recs
				      (append current-group (list r))))
	     )				; process-normal-rec

	   (process-special-rec (r more-recs current-group)
	     (message "MAGICIEL: process-normal-rec: %S (len %d)."
		      r
		      (length current-group))
	     (process-magic-records more-recs
				    (append current-group (list r)))
	     )				; process-special-rec
	   )				; cl-labels

	(message "\nMAGICIEL: grouping records...")
	(start code-recs)
	
	(when (<= steps 1)
	  (cl-return-from magiciel-compile-magic recs-groups))

	;; Step 2: compile each record in each group.
	;; First 0-level test in each group is then treated specially.
	
	(message "\nMAGICIEL: compiling record groups...")
	(cl-loop for g in recs-groups
		 collect (mapcar #'magiciel--compile-record  g)
		 into cgrps
		 finally
		 (setf comp-rec-groups cgrps)
		 (cl-return comp-rec-groups)
		 ))
      )))


;; magiciel--select-op

(cl-defgeneric magiciel--select-op (op type))


(cl-defmethod magiciel--select-op ((op symbol) (type symbol))
  ;; Kitchen sink (with a very large hole).

  (ignore type)
  
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
;; (cl-defmethod magiciel--select-op ((op (eql '!)) (type number))
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


;; magiciel--compile-exec
;; Generic function used to compile the special tests dubbed "execs".

;; (cl-defgeneric magiciel--compile-exec (spec test-val off-op
;; 					    &optional
;; 					    test-op
;; 					    msg)
;;   )


;; (cl-defmethod magiciel--compile-exec ((spec (eql 'regex))
;; 				      test-val
;; 				      off-op
;; 				      &optional
;; 				      test-op
;; 				      (msg ""))
;;   (ignore spec off-op test-op)
;;   `(when (string-match ',test-val bytes pos) ,msg))


;; (cl-defmethod magiciel--compile-exec ((spec (eql 'name))
;; 				      test-val
;; 				      off-op
;; 				      &optional
;; 				      test-op
;; 				      (msg ""))
;;   (ignore spec off-op test-op msg)
;;   `(name ,test-val))


;; (cl-defmethod magiciel--compile-exec ((spec (eql 'indirect))
;; 				      test-val
;; 				      off-op
;; 				      &optional
;; 				      test-op
;; 				      (msg ""))
;;   (ignore spec test-val off-op test-op)
;;   `(when (indirect ,off-op) ,msg))


;; (cl-defmethod magiciel--compile-exec ((spec (eql 'use))
;; 				      test-val
;; 				      off-op
;; 				      &optional
;; 				      test-op
;; 				      (msg ""))
;;   (ignore spec test-val off-op test-op)
;;   `(when (use ,test-val) ,msg))


;; (cl-defmethod magiciel--compile-exec ((spec (eql 'search))
;; 				      test-val
;; 				      off-op
;; 				      &optional
;; 				      test-op
;; 				      (msg ""))
;;   (ignore spec test-val off-op test-op)
;;   `(when (string-search ,test-val bytes ,off-op) ,msg))


;; (cl-defmethod magiciel--compile-exec ((spec (eql 'default))
;; 				      test-val
;; 				      off-op
;; 				      &optional
;; 				      test-op
;; 				      (msg ""))
;;   (ignore spec test-val off-op test-op)

;;   ;; Should check that `test-val' is \"x\".
;;   `(when t ,msg)
;;   )


;; (cl-defmethod magiciel--compile-exec ((spec (eql 'clear))
;; 				      test-val
;; 				      off-op
;; 				      &optional
;; 				      test-op
;; 				      (msg ""))
;;   (ignore spec test-val off-op test-op msg)
  
;;   ;; Should check that `test-val' is \"x\".
;;   '(clear)
;;   )


;; (cl-defmethod magiciel--compile-exec ((spec (eql 'offset))
;; 				      test-val
;; 				      off-op
;; 				      &optional
;; 				      test-op
;; 				      (msg ""))
;;   (ignore spec test-val off-op test-op msg)
  
;;   ;; Should check that `test-val' is \"x\".
;;   '(offset)
;;   )


;; magiciel-compile-instr
;; Methods.

(cl-defmethod magiciel--compile-instr ((off-op magiciel--offset)
				       (type magiciel--common-type-spec)
				       test
				       result-msg)
  ;; The calls to `magiciel--compile-test' and
  ;; `magiciel--compile-message' are really a no-ops, but are there
  ;; for symmetry.

  (if (magiciel--exec-type-spec-p type)
      ;; Records that have a "command" as type are treated specially.
      
      (magiciel--compile-instr off-op
			       (magiciel--common-type-spec-spec type)
			       (magiciel--compile-test test)
			       (magiciel--compile-message result-msg))
    
    (let* ((compiled-type (magiciel--compile-type type))
	   (compiled-test (magiciel--compile-test test)) 
	   (compiled-msg  (magiciel--compile-message result-msg)) ; A string.

	   ;; Unpack the type and the test.
	   (type-spec   (magiciel--record-type-spec-spec compiled-type))
	   (type-mods   (magiciel--record-type-spec-modifiers compiled-type))
	   (type-endian (magiciel--record-type-spec-endian compiled-type))
	   (type-len    (magiciel--record-type-spec-byte-len compiled-type))
	   (type-sign   (magiciel--record-type-spec-unsigned compiled-type))
	   
	   (test-op  (magiciel--test-op compiled-test))
	   (test-val (magiciel--test-value compiled-test))

	   (selected-test-op
	    (cond ((magiciel--numeric-type-spec-p type)
		   (magiciel--select-op test-op 'number))
		  
		  ((magiciel--date-type-spec-p type)
		   (magiciel--select-op test-op 'date))

		  ((magiciel--string-type-spec-p type)
		   (magiciel--select-op test-op 'string))
		  )
	    )
	   )

      (magiciel--make-instruction
       :offset off-op
       :test `(,selected-test-op
	       ,(magiciel--compile-access type-spec
					  type-sign
					  type-endian
					  type-mods
					  type-len
					  off-op
					  )
	       ,(magiciel--compile-test-value
		 test-val
		 type-spec))
       :result compiled-msg)
      ))
  )					; magiciel--compile-inst


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'regex))
				       test-val
				       result-msg
				       )
  (ignore type)
  
  ;; Should check that `test-val' is \"x\".
  
  (magiciel--make-instruction :offset offset
			      :test `(string-match ',test-val %bytes% %pos%)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'name))
				       test-val
				       result-msg
				       )
  (ignore type test-val)
  
  (magiciel--make-instruction :offset offset
			      :test '(name ,test-val)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'indirect))
				       test-val
				       result-msg
				       )
  (ignore type test-val)
  
  (magiciel--make-instruction :offset offset
			      :test '(indirect ,offset)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'use))
				       test-val
				       result-msg
				       )
  (ignore type test-val)
  
  (magiciel--make-instruction :offset offset
			      :test `(use ,test-val)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'search))
				       test-val
				       result-msg
				       )
  (ignore type test-val)
  
  (magiciel--make-instruction :offset offset
			      :test `(string-search ,test-val %bytes% ,offset)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'default))
				       test-val
				       result-msg
				       )
  (ignore type test-val)
  
  ;; Should check that `test-val' is \"x\".
  
  (magiciel--make-instruction :offset offset
			      :test `(default)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'clear))
				       test-val
				       result-msg
				       )
  (ignore type test-val)
  
  ;; Should check that `test-val' is \"x\".
  
  (magiciel--make-instruction :offset offset
			      :test `(clear)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'offset))
				       test-val
				       result-msg
				       )
  (ignore type test-val)
  
  ;; Should check that `test-val' is \"x\".
  
  (magiciel--make-instruction :offset offset
			      :test '(offset)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr
  :before ((offset magiciel--offset)
	   type
	   test
	   result-msg)

  (ignore type test result-msg)
  
  ;; Ensure that the offset has already been compiled.
  
  (unless (magiciel--offset-code offset)
    (setf (magiciel--offset-code offset)
	  (magiciel--compile-offset offset))))


;; magiciel--compile-offset
;; Methods.

(cl-defmethod magiciel--compile-offset ((offset magiciel--direct-offset))
  (if (magiciel--direct-offset-cont offset)
	 `(+ %pos% ,(magiciel--direct-offset-value offset))
    (magiciel--direct-offset-value offset)))


(cl-defmethod magiciel--compile-offset ((offset magiciel--indirect-offset))
  (if (magiciel--indirect-offset-cont offset)
	 `(+ %pos% ,(magiciel--compile-indirect-offset offset))
       (magiciel--compile-indirect-offset offset)))


;; magiciel--compile-indirect-offset

(cl-defun magiciel--compile-indirect-offset (ind-off)
  ;; FTTB
  (cl-labels
      ((compile-ind-offset-type (ind-off)

	 ;; New verision.
	   
	 (let ((ind-addr (magiciel--indirect-offset-addr ind-off))
	       (ind-type (magiciel--indirect-offset-type ind-off))
	       )

	   ;; `ind-type' is a `magiciel--offset-type-spec'.
	   
	   (list ind-addr ind-type)))

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
       )	    
    (cl-destructuring-bind (ind-addr ind-subtype)
	(compile-ind-offset-type ind-off)
      (cl-destructuring-bind (ind-op ind-sign ind-num)
	  (compile-ind-offset-modifier ind-off)
	(ignore ind-sign)
	(let ((type-spec   (magiciel--offset-type-spec-spec ind-subtype))
	      (type-endian (magiciel--offset-type-spec-endian ind-subtype))
	      (type-len    (magiciel--offset-type-spec-byte-len ind-subtype))
	      (type-sign   (magiciel--offset-type-spec-unsigned ind-subtype))
	      ;; (test-op  (magiciel--test-op test))
	      ;; (test-val (magiciel--test-value test))
	      )
	  `(,ind-op
	    ,(if (magiciel--indirect-offset-rel ind-off)
		 `(magiciel--get-value-at* %bytes%
					   (+ %pos% ,ind-addr)
					   ',type-spec
					   ',type-sign
					   ',type-endian
					   ()
					   ',type-len
					   )
	       `(magiciel--get-value-at* %bytes%
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
		       (+ %pos% ,ind-addr ,ind-num)
		       ',ind-subtype)
		   `(magiciel--get-value-at
		     (+ ,ind-addr ,ind-num)))
	       ind-num)))
	)))
  )



;; magiciel--compile-type
;; Methods

(cl-defmethod magiciel--compile-type ((ct magiciel--common-type-spec))
  ct)


;; magiciel--compile-test
;; Methods

(cl-defmethod magiciel--compile-test ((mt magiciel--test))
  mt)


;; magiciel--compile-test-value

(cl-defun magiciel--compile-test-value (test-value type-spec)
  (cond ((magiciel--numeric-type-spec-p type-spec)
	 ;; (car (read-from-string
	 ;;       (magiciel--lispify-number-string test-value)))
	 (magiciel--convert-c-number test-value) ; The result may be inconsistent.
	 )
	((magiciel--exec-type-spec-p type-spec) ; Symmetry.
	 test-value
	 )
	(t test-value))
  )


;; magiciel--compile-message
;; Methods.

(cl-defmethod magiciel--compile-message ((m string))
  m)


(cl-defmethod magiciel--compile-message ((m magiciel--message))
  (magiciel--message-msg-string m))


;; magiciel--compile-access

(cl-defmethod magiciel--compile-access ((type-spec symbol)
					unsigned
					endianness
					mods
					len
					offset)
  (cl-etypecase type-spec
    (magiciel--numeric-type-spec
     (magiciel--compile-num-access type-spec
				   unsigned
				   endianness
				   mods
				   len
				   offset))
    (magiciel--date-type-spec
     (magiciel--compile-date-access type-spec
				    unsigned
				    endianness
				    mods
				    len
				    offset))
    (magiciel--string-type-spec
     (magiciel--compile-string-access type-spec
				      unsigned
				      endianness
				      mods
				      len
				      offset))
    ))


;; (cl-defun magiciel--compile-num-access (type-spec unsigned endianness mods len)
;;   (cl-labels				; The labels 
;;       ((get-unpacker (unsigned endianness n-bytes)
;; 	 (if unsigned
;; 	     (get-u-unpacker endianness n-bytes)
;; 	   (get-s-unpacker endianness n-bytes))
;; 	 )

;;        (get-u-unpackage (endianness n-bytes)
;; 	 (cl-ecase endianness
;; 	   ((:le :<) (cl-ecase n-bytes
;; 		       (1 magiciel--*uint8le-bindat*)
;; 		       (2 magiciel--*uint16le-bindat*)
;; 		       (3 magiciel--*uint24le-bindat*)
;; 		       (4 magiciel--*uint32le-bindat*)
;; 		       (8 magiciel--*uint64le-bindat*)
;; 		       ))
;; 	   ((:be :>) (cl-ecase n-bytes
;; 		       (1 magiciel--*uint8-bindat*)
;; 		       (2 magiciel--*uint16-bindat*)
;; 		       (3 magiciel--*uint24-bindat*)
;; 		       (4 magiciel--*uint32-bindat*)
;; 		       (8 magiciel--*uint64-bindat*)
;; 		       ))
;; 	   ))

;;        (get-s-unpackage (endianness n-bytes)
;; 	 (cl-ecase endianness
;; 	   ((:le :<) (cl-ecase n-bytes
;; 		       (1 magiciel--*sint8le-bindat*)
;; 		       (2 magiciel--*sint16le-bindat*)
;; 		       (3 magiciel--*sint24le-bindat*)
;; 		       (4 magiciel--*sint32le-bindat*)
;; 		       (8 magiciel--*sint64le-bindat*)
;; 		       ))
;; 	   ((:be :>) (cl-ecase n-bytes
;; 		       (1 magiciel--*sint8-bindat*)
;; 		       (2 magiciel--*sint16-bindat*)
;; 		       (3 magiciel--*sint24-bindat*)
;; 		       (4 magiciel--*sint32-bindat*)
;; 		       (8 magiciel--*sint64-bindat*)
;; 		       ))
;; 	   ))
;;        )
    
;;   (cl-etypecase type-spec
;;     (magiciel--integer-type-spec
;;      `(magiciel--get-value-at* magigiel-%bytes%
;; 			       magiciel-%pos%
;; 			       ',type-spec
;; 			       ',type-sign
;; 			       ',type-endian
;; 			       ',type-mods
;; 			       ',type-len
;; 			       ,off-op)
;;      )
;;     (magiciel--real-type-spec
;;      (warn (concat
;; 	    "MAGICIEL: warning: "
;; 	    "no access compiler for REAL type specifications yet."))
;;      42.0)
;;      )
;;     ))


(cl-defun magiciel--compile-num-access (type-spec
					unsigned
					endianness
					mods
					len
					offset)

  (let ((off-op (if (magiciel--offset-p offset)
		    (magiciel--offset-code offset)
		  offset		; Assume it is ok...
		  ))
	)

    (cl-etypecase type-spec
      (magiciel--integer-type-spec
       `(magiciel--get-value-at* %bytes%
				 %pos%
				 ',type-spec
				 ',unsigned
				 ',endianness
				 ',mods
				 ',len
				 ,off-op)
       )
      (magiciel--real-type-spec
       (warn (concat
	      "MAGICIEL: warning: "
	      "no access compiler for REAL type specifications yet."))
       42.0)
      )
    ))


(cl-defun magiciel--compile-date-access (type-spec
					 unsigned
					 endianness
					 mods
					 len
					 offset)
  (let ((off-op (if (magiciel--offset-p offset)
		    (magiciel--offset-code offset)
		  offset		; Assume it is ok...
		  ))
	)
    `(magiciel--get-value-at* %bytes%
			      %pos%
			      ',type-spec
			      ',unsigned
			      ',endianness
			      ',mods
			      ',len
			      ,off-op)
    ))


(cl-defun magiciel--compile-string-access (type-spec
					   unsigned
					   endianness
					   mods
					   len
					   offset)
  (let ((off-op (if (magiciel--offset-p offset)
		    (magiciel--offset-code offset)
		  offset		; Assume it is ok...
		  ))
	)
    `(magiciel--get-value-at* %bytes%
			      %pos%
			      ',type-spec
			      ',unsigned
			      ',endianness
			      ',mods
			      ',len
			      ,off-op)
    ))
  

;; magiciel--compile-record
;; Generate a lambda that does sets up all the necessary
;; addressing and reading.
;;
;; The generated lambda takes two arguments, a "sequence" of bytes
;; (i.e., a "unibyte" string in Emacs Lisp parlance) called `%bytes%'
;; and a position in said string, called `%pos%'.
;;
;; Methods.

(cl-defmethod magiciel--compile-record ((r t))
  (message "MAGICIEL: not a magic record: %s." r)
  nil)


(cl-defmethod magiciel--compile-record ((r magiciel--magic-record))
  (message "MAGICIEL: not a normal magic record: %s." r)
  nil)

  
;; (cl-defmethod magiciel--compile-record-saved ((r magiciel--normal-record))
;;   (let ((offset (magiciel--normal-record-offset r))
;; 	(type   (magiciel--normal-record-type r))
;; 	(test   (magiciel--normal-record-test r))
;; 	(msg    (magiciel--normal-record-message r))
;; 	)
    
;;     (cl-labels
;; 	((compile-test (off-op type test)

;; 	   ;; The following bindings may be taken out of the two
;; 	   ;; functions `compile-test' and `compile-exec' (this one).

;; 	   (let ((type-spec   (magiciel--record-type-spec-spec type))
;; 		 (type-mods   (magiciel--record-type-spec-modifiers type))
;; 		 (type-endian (magiciel--record-type-spec-endian type))
;; 		 (type-len    (magiciel--record-type-spec-byte-len type))
;; 		 (type-sign   (magiciel--record-type-spec-unsigned type))
;; 		 (test-op  (magiciel--test-op test))
;; 		 (test-val (magiciel--test-value test))
;; 		 )

;; 	     (cond ((string-equal "x" test-val)
;; 		    ;; "x" matches anything; a wart in the spec.  What
;; 		    ;; about a string that should match exactly "x"?
;; 		    t
;; 		    )
		   
;; 		   ((magiciel--numeric-type-spec-p type-spec)
;; 		    `(,(magiciel--select-op test-op 'number)
;; 		      (magiciel--get-value-at* bytes
;; 					       pos
;; 					       ',type-spec
;; 					       ',type-sign
;; 					       ',type-endian
;; 					       ',type-mods
;; 					       ',type-len
;; 					       ,off-op)
;; 		      ',(magiciel--compile-test-value test-val type-spec)))

;; 		   ((magiciel--date-type-spec-p type-spec)
;; 		    `(,(magiciel--select-op test-op 'date)
;; 		      (magiciel--get-value-at* bytes
;; 					       pos
;; 					       ',type-spec
;; 					       ',type-sign
;; 					       ',type-endian
;; 					       ',type-mods
;; 					       ',type-len
;; 					       ,off-op)
;; 		      ',test-val)) ; A string, but it must be double checked..

;; 		   ((magiciel--string-type-spec-p type-spec)
;; 		    `(,(magiciel--select-op test-op 'string)
;; 		      (magiciel--get-value-at* bytes
;; 					       pos
;; 					       ',type-spec
;; 					       ',type-sign
;; 					       ',type-endian
;; 					       ',type-mods
;; 					       ',type-len
;; 					       ,off-op)
;; 		      ',test-val))	; Already a string.
		   
;; 		   ))
;; 	   )				; compile-test

;; 	 (compile-exec (off-op type test msg)
	   
;; 	   ;; The following bindings may be taken out of the two
;; 	   ;; functions `compile-test' and `compile-exec' (this one).
	   
;; 	   (let ((type-spec   (magiciel--record-type-spec-spec type))
;; 		 (type-mods   (magiciel--record-type-spec-modifiers type))
;; 		 (type-endian (magiciel--record-type-spec-endian type))
;; 		 (type-len    (magiciel--record-type-spec-byte-len type))
;; 		 (type-sign   (magiciel--record-type-spec-unsigned type))
;; 		 (test-op  (magiciel--test-op test))
;; 		 (test-val (magiciel--test-value test))
;; 		 )

;; 	     (ignore type-mods type-endian type-len type-sign)

;; 	     (magiciel--compile-exec type-spec test-val off-op test-op msg)
;; 	     )
;; 	   )				; compile-exec

;; 	 (compile-record-test (off-op type test result-msg)
;; 	   `(lambda (bytes pos)
;; 	      (when ,(compile-test off-op type test)
;; 		,result-msg))
;; 	   )				; compile-record-test
;; 	 )				; cl-labels

;;       ;; (let ((off-op (magiciel--compile-offset offset))
;;       ;;       (result-msg (magiciel--message-msg-string msg))
;;       ;;       )
;;       ;; 	(cond ((magiciel--exec-type-spec-p type)
;;       ;; 	       (compile-exec off-op type test result-msg))
;;       ;; 	      (t
;;       ;; 	       (compile-record-test off-op type test result-msg))
;;       ;; 	      )
;;       ;; 	)

;;       (magiciel--compile-instr offset
;; 			       type
;; 			       test
;; 			       (magiciel--message-msg-string msg))

;;       )
;;     )
;;   )


(cl-defmethod magiciel--compile-record ((r magiciel--normal-record))
  (magiciel--compile-instr (magiciel--normal-record-offset r)
			   (magiciel--normal-record-type r)
			   (magiciel--normal-record-test r)
			   (magiciel--normal-record-message r))
  )


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
