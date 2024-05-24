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
;; - `magiciel--compile-type'
;; - `magiciel--compile-test' (actually the "value" being tested)
;; - `magiciel--compile-message'
;;
;; Of course, the last two are rather simple.
;; The results of these functions are combined by the function
;; `magiciel--compile-instr', which also relies on the function
;; `magiciel--compile-test-value'.
;;
;; The "instructions" returned by the previous functions are combined
;; in "checks", which are the end, funcallable result of the
;; "compilation".  The main function that builds the checks is
;; `magiciel--emit'.
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


;; Compilation functions.

(cl-defgeneric magiciel--compile-record (r &key &allow-other-keys)
  )

(cl-defgeneric magiciel--compile-offset (offset)
  )

(cl-defgeneric magiciel--compile-type (type)
  )

(cl-defgeneric magiciel--compile-test (test)
  )

(cl-defgeneric magiciel--compile-message (message)
  )

(cl-defgeneric magiciel--compile-instr
    (off-op type test result-msg &key &allow-other-keys)
  )

(cl-defgeneric magiciel--compile-access
    (type-spec offset unsigned endiennes mods len)
  )


(cl-defgeneric magiciel--emit (kind
			       instruction
			       &key
			       m-name
			       &allow-other-keys
			       ;; f-name
			       )
  (:documentation
   "The main function used for code emission.

The function operats on an INSTRUCTION of a given KIND.  F-NAME,
if passed among the \\='other-keys\\=', is the name of the
top-level function INSTRUCTION will be part of; M-NAME can be the
name of a lower level code block."))


;; magiciel--instruction
;;
;; The results of `magiciel--compile-record'.
;;
;; The fields are the results of the compilation and are predisposed
;; to the actual checking operations.
;;
;; A checking operation "accesses" some "data"...
;; At a given "offset"...
;; Performing some "test"...
;; While using a "value"...
;; Returning a string (or a special record) using said "data".

(cl-defstruct (magiciel--instruction
	       (:constructor magiciel--make-instruction
			     (&key
			      (group 0)
			      (kind '=)
			      offset
			      access
			      test
			      (value t)
			      (result "")
			      (level
			       (if (magiciel--offset-p offset)
				   ;; Paranoid.
				   (magiciel--offset-level offset)
				 0))
			      (displacement 0)
			      )))
  "The `magiciel-instruction' structure."

  (group 0 :type integer :read-only t)
  (level 0 :type integer :read-only t)
  (kind '= :type symbol)
  (offset nil :type (or null integer magiciel--offset))
  access
  test
  (value t)
  (result "" :type string :read-only t)
  (displacement 0)	; How "long" is the value tested/found. It is
			; an expression: either a constant or some
			; measurement of the resulting value (cfr.,
			; the `regex' test).
  )


(cl-defmethod cl-print-object ((mi magiciel--instruction) stream)
  (princ (format "#<MAGICIEL Instruction: [GL %d/%d] %s %S %s %s %s>"
		 (magiciel--instruction-group mi)
		 (magiciel--instruction-level mi)
		 (magiciel--instruction-offset mi)
		 (magiciel--instruction-access mi)
		 (magiciel--instruction-test mi)
		 (magiciel--instruction-value mi)
		 (magiciel--instruction-result mi))
	 stream))


;; magiciel-compile-magic

(cl-defun magiciel-compile-magic (magic &optional (steps 1))
  "Compiles a `magiciel-magic' structure."
  
  (magiciel--msg 1 "compiling magic...\n")
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
	     (magiciel--msg 1 "finishing.")
	     (setf recs-groups (nreverse recs-groups))
	     )

	   (process-magic-records (recs current-group  &aux (r (car recs)))
	     (magiciel--msg 1 "process-magic-records: %d to go..."
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
	     (magiciel--msg 1 "process-normal-rec: %S (len %d)."
			    r (length current-group))

	     (cl-assert (magiciel--magic-record-is-normal r)) ; Paranoia.

	     (if (zerop (magiciel--offset-level
			 (magiciel--normal-record-offset r)))
		 (progn
		   (when current-group
		     (magiciel--msg 1 "current group size %d."
				    (length current-group))
		     (push current-group recs-groups))
		   (process-magic-records more-recs (list r)))
	       ;; Continue with current group...
	       (process-magic-records more-recs
				      (append current-group (list r))))
	     )				; process-normal-rec

	   (process-special-rec (r more-recs current-group)
	     (magiciel--msg 1 "process-normal-rec: %S (len %d)."
			    r (length current-group))
	     (process-magic-records more-recs
				    (append current-group (list r)))
	     )				; process-special-rec
	   )				; cl-labels

	(magiciel--msg 1 "grouping records...")
	(start code-recs)
	
	(when (<= steps 1)
	  (cl-return-from magiciel-compile-magic
	    (cl-values recs-groups comp-rec-groups)))

	;; Step 2: compile each record in each group.
	;; First 0-level test in each group is then treated specially.
	
	(magiciel--msg 1 "compiling record groups...")
	(cl-loop for g in recs-groups
		 for gn from 0
		 collect (mapcar #'(lambda (r)
				     (magiciel--compile-record r :group-id gn))
				 g)
		 into cgrps
		 finally
		 (setf comp-rec-groups cgrps)
		 )

	(when (<= steps 2)
	  (cl-return-from magiciel-compile-magic
	    (cl-values recs-groups comp-rec-groups)))
	)				; cl-labels
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


;; magiciel-compile-instr
;; Methods.

(cl-defun magiciel--pass-instr (offset msg)
  (magiciel--make-instruction :offset offset
			      :access (magiciel--offset-code offset)
			      :test 'and
			      :result msg))


(cl-defmethod magiciel--compile-instr ((off-op magiciel--offset)
				       (type magiciel--record-type-spec)
				       (test magiciel--test)
				       result-msg
				       &key
				       &allow-other-keys)
  (let ((type-mods (magiciel--record-type-spec-modifiers type))
	(test-mods (magiciel--test-modifiers test))
	)
    
    ;; The calls to `magiciel--compile-test' and
    ;; `magiciel--compile-message' are really a no-ops, but are there
    ;; for symmetry.

    (if (magiciel--exec-type-spec-p type)
	;; Records that have a "command" as type are treated specially.
      
	(magiciel--compile-instr off-op
				 (magiciel--common-type-spec-spec type)
				 (magiciel--compile-test test)
				 (magiciel--compile-message result-msg)
				 :type-mods type-mods
				 :test-mods test-mods
				 )
    
      (let* ((compiled-type (magiciel--compile-type type))
	     (compiled-test (magiciel--compile-test test)) 
	     (compiled-msg  (magiciel--compile-message result-msg)) ; A string.

	     ;; Unpack the type and the test.
	     (type-spec   (magiciel--record-type-spec-spec compiled-type))
	     ;; (type-mods (magiciel--record-type-spec-modifiers compiled-type))
	     ;; Already unpacked.
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
	     (actual-value (magiciel--compile-test-value test-val type-spec))
	     (actual-length		; Kludge FTTB.
	      (if (eql type-spec 'string)
		  (length actual-value)
		type-len)
	      )
	     )

	(magiciel--make-instruction
	 :offset off-op
	 :access (magiciel--compile-access type-spec
					   off-op
					   type-sign
					   type-endian
					   type-mods
					   actual-length
					   )
	 :test selected-test-op
	 :value actual-value
	 :result compiled-msg
	 :displacement type-len		; It should work (except for
					; the "wrong" specifications
					; in "parse-magic.el").
	 )
	))
    ))					; magiciel--compile-inst


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'regex))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       &allow-other-keys
				       )
  (ignore type-mods test-mods)

  (cl-flet ((compile-type-mods (mods
				&aux
				(flag-pos
				 (and mods
				      (string-match-p
				       "[cls]"
				       (magiciel--modifier-args mods))))
				)
	      ;; (message "compile-instr regex >>>>>> mods %S" mods)
	      (cond ((null mods) (cl-values nil nil))
		    (flag-pos
		     (cl-values (car (read-from-string mods 0 flag-pos))
				(substring mods flag-pos)))
		    (t (cl-values (string-to-number mods) nil))
		    ))

	    (fix-test-value (test-value-string)
	      ;; See the NOTES.md file for the reason we need this
	      ;; here. (And maybe elsewhere).

	      (let ((c0 (aref test-value-string 0)))
		(if (and (or (eql ?\\ c0) (eql ?= c0))
			 (eql (aref test-value-string 1) ?^)) ; Should
					; test for other operators as well?
		    ;; Since the magic test is `regex` I can do away
		    ;; with the `=` as well.
		    (substring test-value-string 1)
		  test-value-string)
		))
	    )				; cl-flet.
    (let ((off-op (magiciel--offset-code offset))
	  (test-val (fix-test-value (magiciel--test-value test)))
	  )
      (cl-multiple-value-bind (range flags)
	  (compile-type-mods (car type-mods))

	(ignore range)
	
	(when (cl-find ?l flags)
	  (magiciel--warn 1 "line flag in \"regex\" type not yet handled."))

	(let ((case-fold-search (cl-find ?c flags))
	      )
	  (magiciel--make-instruction
	   :kind type
	   :offset offset
	   :test `(let ((case-fold-search ',case-fold-search))
		    (string-match ',test-val %bytes% ,off-op))
	   :result result-msg
	   :displacement '(length (match-string 0))
	   ))
	))
    ))


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'name))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       &allow-other-keys
				       )
  (ignore type-mods test-mods)

  (let ((off-op (magiciel--offset-code offset))
	(test-val (magiciel--test-value test))
	)
    
    (magiciel--make-instruction :kind type
				:offset offset
				:test `(name ,test-val ,off-op)
				:result result-msg
				))
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'indirect))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       &allow-other-keys
				       )
  (ignore test type-mods test-mods)

  (let ((off-op (magiciel--offset-code offset)))
    (magiciel--make-instruction :kind type
				:offset offset
				:test `(indirect ,off-op)
				:result result-msg))
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'use))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       &allow-other-keys
				       )
  (ignore type type-mods test-mods)

  (let ((test-val (magiciel--test-value test)))
    (magiciel--make-instruction :kind type
				:offset offset
				:test `(use ,test-val)
				:result result-msg))
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'search))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       &allow-other-keys
				       )
  (ignore type type-mods test-mods)

  (let* ((off-op (magiciel--offset-code offset))
	 (test-val (magiciel--test-value test))
	 (range (and type-mods (string-to-number (car type-mods))))
	 (limit (if range
		    `((string-limit %bytes%
				    (+ ,off-op ,range)))
		  `(%bytes%)
		  ))
	 )
    (magiciel--make-instruction
     :kind type
     :offset offset
     :test `(string-search ,test-val ,@limit ,off-op)
     :result result-msg
     :displacement `(length ,test-val)
     ))
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'default))
				       test-val
				       result-msg
				       &key
				       type-mods
				       test-mods
				       &allow-other-keys
				       )
  (ignore type-mods test-mods)
  
  ;; Should check that `test-val' is \"x\".
  
  (magiciel--make-instruction :kind type
			      :offset offset
			      :test `(default)
			      :value (magiciel--test-value test-val)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'clear))
				       test-val
				       result-msg
				       &key
				       type-mods
				       test-mods
				       &allow-other-keys
				       )
  (ignore type-mods test-mods)
  
  ;; Should check that `test-val' is \"x\".
  
  (magiciel--make-instruction :kind type
			      :offset offset
			      :test `(clear)
			      :value (magiciel--test-value test-val)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'offset))
				       test-val
				       result-msg
				       &key
				       type-mods
				       test-mods
				       &allow-other-keys
				       )
  (ignore test-val type-mods test-mods)
  
  ;; Should check that `test-val' is \"x\".
  (let ((off-op (magiciel--offset-code offset)))
    (magiciel--make-instruction :kind type
				:offset offset
				:test `(offset ,off-op)
				:result result-msg))
  )


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type symbol)
				       test-val
				       result-msg
				       &key
				       &allow-other-keys)
  (magiciel--msg 1 "MCI: provisional compilation of; %s." type)
  (magiciel--msg 1 "MCI: offset %s."       offset)
  (magiciel--msg 1 "MCI: test-val %s."     test-val)
  (magiciel--msg 1 "MCI: result-msg %s.\n" result-msg)
  (magiciel--make-instruction :kind type
			      :offset offset
			      :test `(in-special-context ,type)
			      :result result-msg)
  )


(cl-defmethod magiciel--compile-instr
  :before ((offset magiciel--offset)
	   type
	   test
	   result-msg
	   &key
	   &allow-other-keys)

  (ignore type test result-msg)
  
  ;; Ensure that the offset has already been compiled.
  
  (unless (magiciel--offset-code offset)
    (setf (magiciel--offset-code offset)
	  (magiciel--compile-offset offset))))


;; magiciel--compile-offset
;; Methods.

(cl-defmethod magiciel--compile-offset ((offset magiciel--direct-offset))
  (if (magiciel--offset-cont offset)
      `(+ %end-pos% ,(magiciel--direct-offset-value offset))
    (magiciel--direct-offset-value offset)) ; Actually (+ %pos% ...)
  )


(cl-defmethod magiciel--compile-offset ((offset magiciel--indirect-offset))
  (if (magiciel--offset-cont offset)
      `(+ %end-pos% ,(magiciel--compile-indirect-offset offset))
    (magiciel--compile-indirect-offset offset)) ; Actually (+ %pos% ...)
  )


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
	   ('^ 'logxor)			; Check the `logior'..
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
       )	    			; cl-labels
    
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
		 `(magiciel--get-value-at* %bytes% ; 1
					   (+ %pos% ,ind-addr) ; 2
					   ',type-spec	       ; 3
					   ',type-sign	       ; 4
					   ',type-endian       ; 5
					   ()		       ; 6
					   ',type-len	       ; 7
					   )
	       `(magiciel--get-value-at* %bytes% ; 1
					 ,ind-addr ; 2
					 ',type-spec ; 3
					 ',type-sign ; 4
					 ',type-endian ; 5
					 ()	       ; 6
					 ',type-len    ; 7
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
					(offset magiciel--offset)
					unsigned
					endianness
					mods
					len
					)

  (let ((mod-op (and mods (magiciel--modifier-op (cl-first mods))))
	(mod-args (and mods (magiciel--modifier-args (cl-first mods))))
	)

    ;; (message ">>>> compiling access %S %S" mod-op mod-args)
    
    (cl-etypecase type-spec
      (magiciel--numeric-type-spec
       (magiciel--compile-num-access type-spec
				     unsigned
				     endianness
				     mod-op
				     mod-args
				     len
				     offset))
      (magiciel--date-type-spec
       (magiciel--compile-date-access type-spec
				      unsigned
				      endianness
				      mod-op
				      mod-args
				      len
				      offset))
      (magiciel--string-type-spec
       (magiciel--compile-string-access type-spec
					unsigned
					endianness
					mod-op
					mod-args
					len
					offset))
      )))


(cl-defun magiciel--compile-num-access (type-spec
					unsigned
					endianness
					mod-op
					mod-args
					len
					offset)
  "Compile a number-based access.

TYPE-SPEC  : a `magiciel--numeric-type-spec'
UNSIGNED   : a `boolean'
ENDIANNESS : a `magiciel-endiannes'
MODS       : a list of modifiers (cfr., the \"magic(5)\" specs).
LEN        : the length, in \"bytes\" to be read off the byte stream.
OFFSET     : a `magiciel--offset'."

  (let* ((off-op (magiciel--offset-code offset))
	 (off-cont (magiciel--offset-cont offset))
	 (pos-expr (if off-cont '%end-pos% '%pos%))
	 )

    (ignore off-cont pos-expr)
    
    (cl-etypecase type-spec
      (magiciel--integer-type-spec
       `(magiciel--get-value-at* %bytes% ; 1
				 ;; ,pos-expr	; 2
				 ,off-op
				 ',type-spec	; 3
				 ',unsigned	; 4
				 ',endianness	; 5
				 ',(list mod-op mod-args) ; 6; Unused FTTB.
				 ',len		; 7
				 ,off-op	; 8
				 )
       )
      (magiciel--real-type-spec
       (magiciel--warn 1
		       "no access compiler for REAL type specifications yet.")
       42.0)
      )
    ))


(cl-defun magiciel--compile-date-access (type-spec
					 unsigned
					 endianness
					 mod-op
					 mod-args
					 len
					 offset)
  (let ((off-op (if (magiciel--offset-p offset)
		    (magiciel--offset-code offset)
		  offset		; Assume it is ok...
		  ))
	)

    (ignore off-op)
    
    `(magiciel--get-value-at* %bytes%	; 1
			      ;; %pos%	; 2
			      ,off-op
			      ',type-spec ; 3
			      ',unsigned  ; 4
			      ',endianness ; 5
			      ',(list mod-op mod-args) ; 6; Unused FTTB.
			      ',len	   ; 7
			      ,off-op
			      )
    ))


(cl-defun magiciel--compile-string-access (type-spec
					   unsigned
					   endianness
					   mod-op
					   mod-args
					   len
					   offset)
  (let ((off-op (if (magiciel--offset-p offset)
		    (magiciel--offset-code offset)
		  offset		; Assume it is ok...
		  ))
	)

    (ignore off-op)
    
    `(magiciel--get-value-at* %bytes%	; 1
			      ;; %pos%	; 2
			      ,off-op
			      ',type-spec ; 3
			      ',unsigned  ; 4
			      ',endianness ; 5
			      ',(list mod-op mod-args) ; 6; Unused FTTB.
			      ',len	   ; 7 ; This will most likely be wrong.
			      ,off-op
			      )
    ))
  

;; magiciel--compile-record
;;
;; The way this should work would be to have the lambda generated
;; organized in continuation passing style, in order to take care of
;; the "test tree" nature of the code; alas it may add too much
;; complextiy and... unreadability.
;;
;; In any case the code evolved by having `magiciel--compile-record'
;; generate an "instruction" (which may be useless, but re-reprents
;; the record in a more convenient way, which is then "emitted"; the
;; "-emit-" functions are those that generate the code.
;;
;; Code emission must keep track of "positions" in order to make sure
;; that relative and indirect (possibly relative again) offsets are
;; treated correcly: this implies a stack.  While "one instruction =
;; one lambda" is conceptually simple and "easy", FTTB (2024-04-30) I
;; opt for a "macro" wrapping code that ensures that positions
;; bindings are undone once a piece of code is done; i.e., when it
;; must pop up the test tree.
;;
;; The code emitted will therefore be, after macroexpansion, something
;; like.
;;
;; (let* ((%pos% %end-pos%) ; `%end-pos%` may be the same as the "outer" `%pos%`
;;        (%end-pos% %pos%)
;;        )
;;   <code-emitted-here-using %pos%>
;;   (setf %end-pos% (computed-end-pos %pos% <code-info>))
;;
;;   <other-tests>
;;   )
;;
;; In the above <other-tests> are themselves wrapped in the `let*'
;; above whenever they code for sub-tests.
;;	
;;
;; NOTES:
;;
;; Old comments: pre 2024-04-30.
;;
;; Generate a lambda that does sets up all the necessary
;; addressing and reading.
;;
;; The generated lambda takes two arguments, a "sequence" of bytes
;; (i.e., a "unibyte" string in Emacs Lisp parlance) called `%bytes%'
;; and a position in said string, called `%pos%'.
;;
;; Methods.

(cl-defmethod magiciel--compile-record ((r t)
					&key &allow-other-keys)
  (error "MAGICIEL: not a magic record: %s." r)
  nil)


(cl-defmethod magiciel--compile-record ((r magiciel--magic-record)
					&key &allow-other-keys)
  (warn "MAGICIEL: not a usable magic record: %s." r)
  nil)


(cl-defmethod magiciel--compile-record ((r magiciel--normal-record)
					&key
					(group-id 0)
					&allow-other-keys
					)
  (magiciel--compile-instr (magiciel--normal-record-offset r)
			   (magiciel--normal-record-type r)
			   (magiciel--normal-record-test r)
			   (magiciel--normal-record-message r)
			   :group-id group-id
			   )
  )


(cl-defmethod magiciel--compile-record ((r magiciel--special-record)
					&key
					(group-id 0)
					&allow-other-keys
					)
  (magiciel--compile-instr (magiciel--make-direct-offset :level 1)
			   ;; Fake offset. Level 1 necessary.
			   (magiciel--special-record-type r)
			   (magiciel--make-test :value t)
			   (cl-first (magiciel--special-record-value r))
			   :group-id group-id
			   )
  )


;; magiciel--addr-select-subtype
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


;; Compiling tests and test groups into functions.

;; magiciel--compile-rec-group

(cl-defun magiciel--compile-rec-group (recs-group
				       &optional
				       (magic-name "magiciel--n-"))
  (let ((group-name (gensym magic-name)))
    (cons group-name recs-group))
  )


;; magiciel--compile-rec-groups

(cl-defun magiciel--compile-rec-groups (recs-groups
					&optional
					(magic-name "magiciel--n-"))
  (cl-loop for rg in recs-groups
	   collect (magiciel--compile-rec-group rg magic-name)))


;; Emit functions.
;; ---------------
;;
;; The semantics of magic(5) tests is very "assembly-like" because of
;; the "tree-like" arrangement of the tests.
;; I could set up a continuation style compilation scheme, but I
;; keep track of the "state", i.e., the level, of the checks.
;;
;; The "machine" uses the following variables/locations/registers.
;;
;; `--magiciel--value' : the object (bytes) read by the "access" code.
;; `--magiciel-msg'   : the "message" resulting as a description of
;;                       the file.
;; `--magiciel--test-result' : a boolean that contains the result of
;;                             the test.
;; `--magiciel--level-state' : the current test level; all tests at
;;                             the current level are executed in
;;                             sequence.
;;
;; Cfr., the following excertp from the magic(5) manual entry
;;
;; """
;; Some file formats contain additional information which is to be printed
;; along with the file type or need additional tests to determine the true
;; file type.  These additional tests are introduced by one or more >
;; characters preceding the offset.  The number of > on the line indicates
;; the level of the test; a line with no > at the beginning is considered to
;; be at level 0.  Tests are arranged in a tree-like hierarchy: if the test
;; on a line at level n succeeds, all following tests at level n+1 are
;; performed, and the messages printed if the tests succeed, until a line
;; with level n (or less) appears.  For more complex files, one can use
;; empty messages to get just the "if/then" effect, in the following way:
;;
;;       0      string   MZ
;;       >0x18  leshort  <0x40   MS-DOS executable
;;       >0x18  leshort  >0x3f   extended PC executable (e.g., MS Windows)
;; """

;; Support code.

;; magiciel--check
;; Sets up and track the %pos% and %end-pos% variables (FTTB).
;;
;; It should probably become a macrolet in the main generate function.

(cl-defmacro magiciel--check (tag access test &body continue-tests)
  "The macro ensures that `%end-pos%` is preserved after a TEST and subtests.

The variable ACCESS contains an expression that represents how
the code will get the value to be tested by TEST (if any).
CONTINUE-TESTS contains the code for the \"next\" tests (and
other code as well; usually the first expression of
CONTINUE-TESTS will be a setting of the variable `%end-pos% ( the
\"end\" of the value previously matched), followed by other
house-keeping code."

  (declare (indent 3))

  `(cl-block %%test-block%%		; Maybe this tag is not right.
     (let* ((%saved-end-pos% %end-pos%))
       (magiciel--msg 1 "running check; %s (%s)" ,tag %pos%)
       (magiciel--msg 1 "access: %s" ',access)
       (magiciel--msg 1 "test: %S (%d)." ',test %end-pos%)
       (unwind-protect
	   ,(if access
		`(let* ((--magiciel-value ,access)
			(--magiciel-test-result ,test)
			)
		   (magiciel--msg 1 "accessed value: %S.\n" --magiciel-value)

		   (magiciel--msg 1 (if --magiciel-test-result
					"check succeded."
				      "check failed."))
		   (when --magiciel-test-result
		     ,@continue-tests))
	      `(let ((--magiciel-test-result ,test))
		 (magiciel--msg 1 (if --magiciel-test-result
				      "check succeded."
				    "check failed."))
		 (when --magiciel-test-result
		   (magiciel--msg 1 "check succeded.")
		   ,@continue-tests)))
	 (setq %end-pos% %saved-end-pos%)))
     )
  )


;; magiciel--$acc-msg-disp

(cl-defmacro magiciel--$acc-msg-disp (s &optional (d 0))
  `(setf --magiciel-msg
         (concat --magiciel-msg ,s)
	 
	 --magiciel-disp
	 ,d))


;; magiciel--$store-access-result

(cl-defmacro magiciel--$store-access-result (&optional
					     (result "")
					     (start-pos '%pos%)
					     (displ 0)
					     (val '--magiciel-value)
					     )
  "Houskeeping macro used in generated code.

Accumulates the RESULT (must be a literal string) in the final message
returned by the compiled function.  It also records the `$end-pos$'
based on START-POS (defaulting to `%pos%' and the displacement DISPL.
VAL is the expression constructing the value accessed (defaulting to
`--magiciel-value')."
  (let* ((msg1 (let ((bpos (string-search "\\b" result)))
		 (cond ((and bpos (zerop bpos))
			(substring result 2))
		       ((string-empty-p result)
			"")
		       (t
			(concat " " result))
		       )))
	 
	 (msg  (if (cl-find ?% msg1)
		   `(format ,msg1 ,val)
		 msg1))
	 )
    `(setf --magiciel-msg
	   (concat --magiciel-msg ,msg)

	   %end-pos%
	   (+ ,start-pos ,displ)))
  )


(cl-defmacro magiciel--$noop (&rest args)
  `(progn
     (ignore ',args)
     t))


(cl-defmacro magiciel--$in-context-p (c)
  `(eq %context% ,c))


(cl-defmacro magiciel--$use (name-label)
  `(,name-label)
  )


;; magiciel-emit methods.

(cl-defmethod magiciel--emit ((kind (eql '==)) ; Never match!
			      (inst magiciel--instruction)
			      &key
			      (f-name (gensym "magiciel--%%fn-top-"))
			      (m-name (gensym "magiciel--i-n-"))
			      (displacement 0)
			      &allow-other-keys)
  "This method is the main \\='emit\\=' method for regular tests.

The KIND argument indicates all boolean tests.  The DISPLACEMENT
variable is an \\='expression\\=' that comes from the \"previous\"
test and that is used for \"continuation\" offsets."

  ;; Note:
  ;; Maybe change KIND specializer to `boolean'.

  (ignore kind f-name m-name displacement)
  
  (let ((level  (magiciel--instruction-level inst))
	(offset (magiciel--instruction-offset inst))
	(access (magiciel--instruction-access inst))
	(test   (magiciel--instruction-test inst))
	(value  (magiciel--instruction-value inst))
	(result (magiciel--instruction-result inst))
	(disp   (magiciel--instruction-displacement inst))
	)

    (ignore level disp offset)

    ;; Let's handle test value \\='x\\=' (cfr., "magic(5)" man page.)

    (if (or (eq value 'x)
	    (string-equal value "x")
	    (string-equal value "X"))
	`(let ((--magiciel-value ,access))
	   (when t (magiciel--$acc-msg-disp (format ,result --magiciel-value)))
	   ;; The "when" here is needed because I am an idiot (FTTB).
	   )
    
      `(let ((--magiciel-value ,access))
	 (when (,test --magiciel--value ,value)
           (magiciel--$acc-msg-disp (format ,result --magiciel-value))
           ))
      )
    ))


;; This is unused FTTB.  The \\='boolean is not used (yet), but this
;; is where I experiment.

(cl-defmethod magiciel--emit ((kind (eql '=))
			      ;; (kind (eql 'boolean))
			      (inst magiciel--instruction)
			      &key
			      (f-name (gensym "magiciel--%%fn-top-"))
			      (m-name (gensym "magiciel--i-n-"))
			      (displacement 0)
			      &allow-other-keys)
  "This method is the main \\='emit\\=' method for regular tests.

The KIND argument indicates all boolean tests.  The DISPLACEMENT
variable is an \\='expression\\=' that comes from the \"previous\"
test and that is used for \"continuation\" offsets."

  (ignore kind f-name m-name displacement)
  
  (let ((level  (magiciel--instruction-level inst))
	(offset (magiciel--instruction-offset inst))
	(access (magiciel--instruction-access inst))
	(test   (magiciel--instruction-test inst))
	(value  (magiciel--instruction-value inst))
	(result (magiciel--instruction-result inst))
	(disp   (magiciel--instruction-displacement inst))
	)

    (ignore level disp)
    
    (let ((pos (if (magiciel--offset-cont offset) '%end-pos% '%pos%))
	  (tag (intern (format "%s-%s" kind value)))
	  )

      (magiciel--msg 1 "%s" inst)
      
      (if (or (eql value 'x)
	      (and (stringp value)
		   (or (string-equal value "x")
		       (string-equal value "X"))))
	  `(magiciel--check ',tag
	       ,access
	       t
	     (magiciel--$store-access-result ,result ,pos ,disp))
	`(magiciel--check ',tag
	     ,access
	     (,test --magiciel-value ,value)
	   (magiciel--$store-access-result ,result ,pos ',disp))
	)
      )))


(cl-defmethod magiciel--emit ((kind (eql 'regex-old))
			      (inst magiciel--instruction)
			      &key
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)

  (ignore kind m-name)
  
  (let (
	;; (level  (magiciel--instruction-level inst))
	;; (access (magiciel--instruction-access inst))
	(test   (magiciel--instruction-test inst))
	;; (value  (magiciel--instruction-test inst))
	(result (magiciel--instruction-result inst))
	)
    `(when ,test
       (magiciel--$acc-msg-disp (format ,result (match-string 0)))
       ))
  )


(cl-defmethod magiciel--emit ((kind (eql 'regex))
			      (inst magiciel--instruction)
			      &key
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)

  (ignore kind m-name)
  
  (let* (	       ; (level  (magiciel--instruction-level inst))
	 ;; (access (magiciel--instruction-access inst))
	 (test   (magiciel--instruction-test inst))
	 (value  (magiciel--instruction-value inst))
	 (result (magiciel--instruction-result inst))
	 (tag (intern (format "regexp-%s" value)))
	 )
    `(magiciel--check ',tag nil ,test
       (magiciel--$store-access-result ,result
				       %pos%
				       (length (match-string 0 %bytes%))
				       (format ,result
					       (match-string 0 %bytes%)))))
  )


(cl-defmethod magiciel--emit ((kind (eql 'name))
			      (inst magiciel--instruction)
			      &key
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)

  (ignore kind m-name)
  
  (let ((level  (magiciel--instruction-level inst))
	;; (access (magiciel--instruction-access inst))
	(test   (magiciel--instruction-test inst))
	;; (value  (magiciel--instruction-test inst))
	(result (magiciel--instruction-result inst))
	)

    (ignore level)
    
    `(magiciel--$noop ',(cl-second test) ',(cl-third test) ',result)
    )
  )


(cl-defmethod magiciel--emit ((kind (eql 'indirect))
			      (inst magiciel--instruction)
			      &key
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)

  (ignore kind inst m-name)
  
  (let (
	;; (level  (magiciel--instruction-level inst))
	;; (access (magiciel--instruction-access inst))
	;; (test   (magiciel--instruction-test inst))
	;; (value  (magiciel--instruction-test inst))
	;; (result (magiciel--instruction-result inst))
	)
    `(progn
       (warn "MAGICIEL: 'indirect' not implemented; failing.")
       )
    )
  )


(cl-defmethod magiciel--emit ((kind (eql 'use))
			      (inst magiciel--instruction)
			      &key
			      (m-name (gensym "magiciel--i-n-"))
			      (env ())
			      &allow-other-keys)

  ;; For this instruction I need to refer to the proper "name", hence
  ;; I rely on the `env' variable.

  (ignore kind m-name)
  
  (let* (
	 ;; (level  (magiciel--instruction-level inst))
	 ;; (access (magiciel--instruction-access inst))
	 (offset (magiciel--instruction-offset inst))
	 (test   (magiciel--instruction-test inst))
	 ;; (value  (magiciel--instruction-value inst))
	 (result (magiciel--instruction-result inst))

	 (name-label (assoc (cl-second test) env #'string-equal))
	 )

    (unless name-label
      (warn "MAGICIEL: unknown label %s; test will fail."
	    (cl-second test)))

    (if name-label
	(let ((pos (if (magiciel--offset-cont offset) '%end-pos% '%pos%))
	      (label-name (cdr name-label))
	      (use-block-name (intern (concat (cl-second test) "-use-block")))
	      )
	  `(cl-block ,use-block-name
	     (magiciel--$use ,label-name)
	     ;; (magiciel--$acc-msg-disp ,result)
	     (magiciel--$store-access-result ,result ,pos)
	     ))
      `(progn nil))
    ))


(cl-defmethod magiciel--emit ((kind (eql 'search))
			      (inst magiciel--instruction)
			      &key
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)

  (ignore kind m-name)
  
  (let* (
	 ;; (level  (magiciel--instruction-level inst))
	 ;; (access (magiciel--instruction-access inst))
	 (offset (magiciel--instruction-offset inst))
	 (test   (magiciel--instruction-test inst))
	 (value  (magiciel--instruction-value inst))
	 (result (magiciel--instruction-result inst))
	 (disp   (magiciel--instruction-displacement inst))
	 (tag (intern (format "search-%s" value)))
	 (pos (if (magiciel--offset-cont offset) '%end-pos% '%pos%))
	 )
    `(magiciel--check ',tag nil ,test
       (magiciel--$store-access-result ,result ,pos ,disp)))
  )


(cl-defmethod magiciel--emit ((kind (eql 'default))
			      (inst magiciel--instruction)
			      &key
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)

  (ignore kind m-name)
  
  (let (
	;; (level  (magiciel--instruction-level inst))
	;; (access (magiciel--instruction-access inst))
	(offset (magiciel--instruction-offset inst))
	(test   (magiciel--instruction-test inst))
	;; (value  (magiciel--instruction-test inst))
	(result (magiciel--instruction-result inst))
	)
    (let ((pos (if (magiciel--offset-cont offset) '%end-pos% '%pos%))
	  (tag 'default)
	  )
      `(magiciel--check ',tag nil ,test
	 (magiciel--$store-access-result ,result ,pos)))
    )
  )


(cl-defmethod magiciel--emit ((kind (eql 'clear))
			      (inst magiciel--instruction)
			      &key
			      ;; (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)
  (let ((level  (magiciel--instruction-level inst))
	(offset (magiciel--instruction-offset inst))
	;; (access (magiciel--instruction-access inst))
	;; (test   (magiciel--instruction-test inst))
	(value  (magiciel--instruction-value inst))
	(result (magiciel--instruction-result inst))
	(disp   (magiciel--instruction-displacement inst))
	)
    
    (ignore level disp)
    
    (let ((pos (if (magiciel--offset-cont offset) '%end-pos% '%pos%))
	  (tag (intern (format "%s-%s" kind value)))
	  )

      (magiciel--msg 1 "%s" inst)
      
      (unless (or (eql value 'x)
		  (and (stringp value)
		       (or (string-equal value "x")
			   (string-equal value "X"))))
	(magiciel--warn "value for `clear' is not `x` (%s)."
			value))
      
      `(cl-block ',tag
	 (set-match-data ())		; Living dangerously!
	 (magiciel--$store-access-result ,result ,pos))
      )))


(cl-defmethod magiciel--emit ((kind (eql '!:apple))
			      (inst magiciel--instruction)
			      &key
			      (f-name (gensym "magiciel--%%fn-top-"))
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)
  (ignore m-name)

  
  (magiciel--msg 1 "EMIT: emit APPLE %s." kind)
  `(when (magiciel--$in-context-p ',kind)
     (warn "MAGICIEL: !:apple %s not properly handled yet."
	   ,(magiciel--instruction-result inst))
     ;; (cl-return-from %%test-block%% ,(magiciel--instruction-result inst))
     (cl-return-from ,f-name ,(magiciel--instruction-result inst))
     )
  )


(cl-defmethod magiciel--emit ((kind (eql '!:mime-old))
			      (inst magiciel--instruction)
			      &key
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)
  (ignore m-name)
  
  (magiciel--msg 1 "EMIT: emit MIME %s." kind)
  `(when (magiciel--$in-context-p ',kind)
     (cl-return-from %%test-block%% ,(magiciel--instruction-result inst)))
  )


(cl-defmethod magiciel--emit ((kind (eql '!:mime))
			      (inst magiciel--instruction)
			      &key
			      (f-name (gensym "magiciel--%%fn-top-"))
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)
  (ignore m-name)
  
  (magiciel--msg 1 "EMIT: emit MIME %s." kind)
  `(magiciel--check '!:mime-check nil (magiciel--$in-context-p ',kind)
     (setq --magiciel-msg ,(magiciel--instruction-result inst))
     ;; (cl-return-from %%test-block%% --magiciel-msg)
     (cl-return-from ,f-name --magiciel-msg)
     )
  )


(cl-defmethod magiciel--emit ((kind (eql '!:strength))
			      (inst magiciel--instruction)
			      &key
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)
  (ignore m-name)

  (magiciel--msg 1 "EMIT: emit STRENGTH %s." kind)
  `(when (magiciel--$in-context-p ',kind)
     (magiciel--$noop ,(magiciel--instruction-result inst)))
  )


(cl-defmethod magiciel--emit ((kind (eql '!:ext))
			      (inst magiciel--instruction)
			      &key
			      (f-name (gensym "magiciel--%%fn-top-"))
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)
  (ignore m-name)

  (magiciel--msg 1 "EMIT: emit EXT %s (%S)." kind inst)
  `(magiciel--check '!:ext-check nil (magiciel--$in-context-p ',kind)
     (setq --magiciel-msg ,(magiciel--instruction-result inst))
     ;; (cl-return-from %%test-block%% --magiciel-msg)
     (cl-return-from ,f-name --magiciel-msg)
     )
  )


(cl-defmethod magiciel--emit ((kind symbol)
			      (inst magiciel--instruction)
			      &key
			      (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)
  (ignore m-name)

  (magiciel--msg 1 "EMIT: unhandled kind %s." kind)
  `(; cl-return-from %%test-block%%
    ;; progn
    cl-block ,kind
     ,(magiciel--instruction-result inst))
  )


;; Instruction trees.
;; Another function to build up the instruction tree.

(cl-defstruct (magiciel--instr-node
	       (:type list)
	       (:constructor
		magiciel--make-instr-node (instr &optional sub-instr-nodes))
	       )
  (id (gensym "IN-"))
  instr
  (sub-instr-nodes ())
  )


;; magiciel--dump-instr-tree, magiciel--dump-instr-level-alist
;; Useful debugginf functions.

(cl-defun magiciel--dump-instr-tree (it &optional (indent 0))
  (unless (null it)
    (let ((i (magiciel--instr-node-instr it))
	  (id (magiciel--instr-node-id it))
	  (sts (magiciel--instr-node-sub-instr-nodes it))
	  )
      (magiciel--msg 1 "MDIT: %s%s %s %s"
		     (make-string indent ? )
		     (magiciel--instruction-level i)
		     id
		     (magiciel--instruction-kind i))
      (dolist (st sts)
	(magiciel--dump-instr-tree st (+ indent 4)))
      )))


(cl-defun magiciel--dump-instr-level-alist (instr-nodes-at-level)
  (magiciel--msg 1 "\nMILA:")
  (cl-loop for (il . instr-node) in instr-nodes-at-level
	   do (magiciel--msg 1 "MILA: %s => %s %s"
			     il
			     (magiciel--instr-node-id instr-node)
			     (magiciel--instruction-kind
			      (magiciel--instr-node-instr instr-node)))
	   finally (magiciel--msg 1 "MILA:\n")
	   ))


;; Construction instruction trees for groups.

(cl-defun magiciel--group-trees (instr-group)
  (magiciel--msg 1 "MGT:")
  (let ((instrs-nodes
	 (mapcar #'magiciel--make-instr-node instr-group))
	)
    (magiciel--group-instrs-trees (car instrs-nodes)
				  (cdr instrs-nodes)
				  ())
    ))


(cl-defun magiciel--group-instrs-trees (instr-node
					more-instrs
					instr-nodes-at-level
					)
  ;; A "group has one 0 level entry point; the first instruction.

  ;; `instr-node'  : first instruction node in (remaining) instruction
  ;;                 node list.
  ;; `more-instrs' : instructions after `instr'; can be an empty list.
  ;; `instr-nodes-at-level' : an a-list (organized in a stack-like
  ;;                          manner) of nodes at a given level, where
  ;;                          to add subtrees; keys are the "level" of
  ;;                          the node last seen at a given level.
  ;;
  ;; The function returns a subtree rooted at the first (0 level)
  ;; instruction. Each (sub) tree is simply a cons (root . children)

  (let* ((instr (magiciel--instr-node-instr instr-node))
	 (il (magiciel--instruction-level instr))
	 (parent-node (cdr (assoc (1- il) instr-nodes-at-level)))
	 )
    ;; `parent-node' is the last level (`level' - 1) node seen.

    
    (magiciel--msg 1 "\nMGIT: instr level %d, instr kind '%s'."
		   il
		   (magiciel--instruction-kind instr)
		   )
    
    (magiciel--msg 1 "MGIT: parent %s %s"
		   (magiciel--instr-node-id parent-node)
		   (magiciel--instr-node-instr parent-node)
		   )

    (magiciel--dump-instr-level-alist instr-nodes-at-level)
    
    (when (and parent-node (not (zerop il))) ; We are not at level 0.
      ;; Insert the node among the siblings of the parent.
      (setf (magiciel--instr-node-sub-instr-nodes parent-node)
	    (append (magiciel--instr-node-sub-instr-nodes parent-node)
		    (list instr-node)))
      )

    (when parent-node
      (magiciel--msg 1 "MGIT: parent subtrees %s"
		     (magiciel--instr-node-sub-instr-nodes parent-node))
      (magiciel--dump-instr-tree parent-node)
      )

    (cond ((and (null parent-node) (zerop il) (null more-instrs))
	   ;; This is a "root-only" group.
	   ;; Return it as is.
	   (magiciel--msg 1 "MGIT: just the root %s." instr-node)
	   instr-node
	   )

	  ;; Regular cases.
	  ;;
	  ;; No more siblings: return the "root".
	  ((null more-instrs)
	   (cdr (assoc 0 instr-nodes-at-level))
	   )

	  ;; Recur on subtrees.
	  (t
	   (magiciel--group-instrs-trees (car more-instrs)
     				    (cdr more-instrs)
     				    (cl-acons il instr-node
     					      instr-nodes-at-level)
     				    )
	   )
	  )
	
    ;; (if (null more-instrs)
    ;; 	(cdr (assoc 0 instr-nodes-at-level))
    ;;   (magiciel--group-instrs-trees (car more-instrs)
    ;; 				    (cdr more-instrs)
    ;; 				    (cl-acons il instr-node
    ;; 					      instr-nodes-at-level)
    ;; 				    )
    ;;   )
    ))



;; magiciel--group-func-code

(cl-defstruct (magiciel--group-func-code
	       (:constructor magiciel--make-group-func-code
			     (name body &rest properties))
	       )
  "Internal structure packing the code blocks corresponding to \\='groups\\='."

  (name (gensym "MGFC-") :type symbol)
  (body () :type list)
  (properties () :type list)
  )


;; Constructing the functions that check for a given file type, mime
;; type and extension.

(cl-defun magiciel--group-emit (magic
				instr-group
				group-number
				&optional
				(magic-env ())
				)
  "Builds the function body that runs the i-th check.

The function obtained correspond to the groups obtained from the
compilation of a magic(5) file.  The name of the function is
\\='=magiciel--<magic name>-<group-number>\\='=.

Arguments and Values:

`magic' : is a `magiciel--magic' struct.
`instr-group' : is a list of `magiciel--instruction'.
`group-number' : is the id of the instruction; must be unique.
`magic-env' : is a simple \\='enviroment\\=' (an a-list) containg
              bindings for names used in the group/block; mostly label
              names associated to \"name\" records, to be used by
              \"use\" records.

Notes:

Groups with \\='=name\\='= and \\='=use\\='= operation are treated
specially.
"
  (let* ((name (magiciel--magic-name magic))

	 (magic-fname (magiciel--magic-fname magic))
	 
	 (magic-group-fname
	  (intern (format "%s-%d" magic-fname group-number)))

	 (magic-group-block-name
	  (intern (format "%s-block-%d" name group-number)))
	 
	 (instrs-tree
	  (magiciel--group-trees instr-group))
	 
	 (exit-code
	  `(cl-return-from ,magic-fname
	      --magiciel-msg))
	 )

    (magiciel--msg 1 "\nMCM: instruction tree:")

    (magiciel--dump-instr-tree instrs-tree)


    ;; The code emitted for each "tree" will cause side effects and
    ;; will return a generalized boolean (or better, the returned
    ;; value will be considered as such).
    
    (cl-labels
	((emit-tree (instr-node)
	   (let* ((instr
		   (magiciel--instr-node-instr instr-node))
                 
		  (sub-tests
		   (magiciel--instr-node-sub-instr-nodes instr-node))

                  (instr-root-code
                   (magiciel--emit (magiciel--instruction-kind instr)
				   instr
				   :f-name magic-fname
				   :env magic-env)
		   )
                  )

             ;; Here comes the fun (FTTB).
             ;; If the `instr-code-root' is a `when' clause, then I
             ;; have to attach the `sub-test' inside it.  Same thing,
             ;; but more involved if I have a `let/let*'.  Otherwise, I
             ;; must wrap everything in a (possibly useless) `progn'.
             ;;
             ;; I know I should resort to more sophisticated compiler
             ;; techniques, but just bear with me.
	     ;;
	     ;; Finally, I have to bail out of the "top" function
             ;; ("group 0" wrapping) if the main test succeds.
	     ;;
	     ;; Yeah, I should have proper continuations here.

             (cl-case (cl-first instr-root-code)
               (when (append instr-root-code
                             (cl-loop for sub-node in sub-tests
			              collect (emit-tree sub-node))
			     '(t)
			     )
                 )
               ((let let*)
		(pcase instr-root-code
                  (`(let ,mv-bind ,when-form)
                   (cl-assert (eq 'when (cl-first when-form)))
                   ;; This should always be the case.
                   `(let ,mv-bind
                      ,(append when-form
                               (cl-loop for sub-node in sub-tests
			                collect (emit-tree sub-node))
			       '(t)
                               ))
                   ))
                 )
	       (progn
		 ;; I want to "flatten" the "progn's, if possible.
		 (append (cl-rest instr-root-code)
			 (cl-loop for sub-node in sub-tests
			          collect (emit-tree sub-node))
			 '(t)
			 )
		 )
	       (cl-block (append instr-root-code
				 (cl-loop for sub-node in sub-tests
					  collect (emit-tree sub-node))
				 '(t)
				 )
		 )
	       (magiciel--check
		(append instr-root-code
			(cl-loop for sub-node in sub-tests
				 collect (emit-tree sub-node))
			'(t)
			)
		   )
               (t
                `(progn ,instr-root-code
                        ,@(cl-loop for sub-node in sub-tests
			           collect (emit-tree sub-node))
			t
			)
                ))
             )
	   )

         (emit-group-tree (instr-node)
	   
           ;; The `instr-node' is at level 0.
	   ;; If we get a non-nil return code, we exit the top-level
	   ;; function.
	   
           `(cl-block ,magic-group-block-name
              (when ,(emit-tree instr-node)
		,exit-code)
	      ))
	 )
      
      (magiciel--make-group-func-code magic-group-fname
                                      (emit-group-tree instrs-tree))      
      )))


(cl-defun magiciel--group-is-named (ig)
  "Check that the (first) group of IG is a \\='name\\=' one."

  (let ((i1 (cl-first ig)))
    (eq 'name (magiciel--instruction-kind i1))))



(cl-defun magiciel--group-name (ig)
  "Get the \\='name\\=' of an instruction group IG."

  (let ((i1 (cl-first ig)))
    (when (eq 'name (magiciel--instruction-kind i1))
      (cl-second (magiciel--instruction-test i1)))))


;; magiciel--magic-emit

(cl-defun magiciel--magic-emit (magic instr-groups)
  "Emit the function that checks for a specific \"magic\" specification."

  (let* ((name (magiciel--magic-name magic))
	 
	 (magic-fname (magiciel--magic-fname magic))

	 (group-fns ())
	 (groups-named ())

	 (groups-names-env ())
	 )

    ;; First, collect "name" instructions for "use" ones.
    ;; (I should build up this environment while building the
    ;; instructions, but the double pass shouldn't matter so much.)
    
    (cl-loop for ig in instr-groups
	     for ign-name = (magiciel--group-name ig)
	     when ign-name
	       collect (cons ign-name
			     (intern (format "%s-label-%s" name ign-name)))
	       into igs-env
	     end
	     finally
	     (setf groups-names-env igs-env))

    ;; Some code duplication below (see `intern' -- I am to lazy to
    ;; refactor right now).
    
    (cl-loop for ig in instr-groups
	     for ign from 0
	     for ign-name = (magiciel--group-name ig)
	     if ign-name
	       collect (list
			(intern (format "%s-label-%s" name ign-name))
			(magiciel--group-emit magic ig ign groups-names-env))
	       into named-groups
	     else
	       collect (magiciel--group-emit magic ig ign groups-names-env)
	       into fns-groups
	     end
	     finally
	     (setf group-fns fns-groups
		   groups-named named-groups))
    

    (let ((magic-fun (cl-constantly "MAGICIEL: unknown format."))
          (magic-fun-body (cl-constantly nil))
          )
      ;; Here we build the function for the magic entry.

      (if groups-named
	  ;; Create `cl-labels' for each named group.
	  (setq magic-fun-body
		`(cl-block ,magic-fname
		   (cl-labels
		       ,(cl-loop for (named-name code) in groups-named
				 collect
				 `(,named-name (&rest $$$args-here%%%)
				    ,(magiciel--group-func-code-body code)))
		     ,@(mapcar #'magiciel--group-func-code-body group-fns)
		     )
                   ))
	
	;; Create a simplified function.
	(setq magic-fun-body
	      `(cl-block ,magic-fname
		 ,@(mapcar #'magiciel--group-func-code-body group-fns)
                 )
	      ))


      (setq magic-fun
            `(lambda (%bytes%	; The file/stream/buffer data.
		      %pos%	; Where we start scanning the %%bytes%%.
		      %context% ; `%context' can be ':normal' (or `!:normal'),
				; '!:mime', '!:ext', or '!:apple'.
				; '!:strength' unhandled.
		      )
               (let ((--magiciel-value 0) ; Actually `#\NULL' in Common Lisp.
                     (--magiciel-test-result nil)
                     (--magiciel-msg "")

		     ;; The displacement of the "previous"
		     ;; instruction; starts at 0.  It is used for
		     ;; "continuing" records.
		     
		     (--magiciel-disp 0)
		     (%end-pos% %pos%)
                     )

		 ;; The next one would be a `ignorable' declaration in CL.
		 (ignore --magiciel-value
			 --magiciel-test-result
			 --magiciel-disp)
                 ,magic-fun-body
		 --magiciel-msg)
               ))

      (setf (symbol-function magic-fname) magic-fun)

      (magiciel--add-spell name (byte-compile magic-fname))
      
      (cl-values magic-fname
		 group-fns
		 groups-named
		 magic-fun
		 )
      )))


;; Epilogue.

(provide 'magiciel-compile-magic)

;;; compile-magic.el ends here
