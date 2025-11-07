;;; magiciel --- Guessing the content of a file in simple as possible way. -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; compile-magic.el
;;
;; See the file COPYING license and copyright information.

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
;;
;; 2024-11-29:
;; Notes were moved in the NOTES.md file some time ago; please check
;; that file for updated comments about the `magiciel'  development.


;;; Code:

(require 'cl-lib)
(require 'pcre2el)
(require 'magiciel-parse-magic)


;; Compilation functions.

(cl-defgeneric magiciel--compile-record (r &key &allow-other-keys)
  (:documentation "Compile a magiciel record R.")
  )

(cl-defgeneric magiciel--compile-offset (offset)
  (:documentation "Compile a magiciel offset OFFSET.")
  )

(cl-defgeneric magiciel--compile-type (type)
  (:documentation "Compile a magiciel type TYPE.")
  )

(cl-defgeneric magiciel--compile-test (test)
  (:documentation "Compile a magiciel test TEST.")
  )

(cl-defgeneric magiciel--compile-message (message)
  (:documentation "Compile a magiciel message MESSAGE.")
  )

(cl-defgeneric magiciel--compile-instr
    (off-op type test result-msg &key &allow-other-keys)
  (:documentation "Compile a magiciel instruction.

OFF-OP is the offset specification, TEST, TYPE and RESULT-MESSAGE are
the parts of a \\='magic(5)\\=' specification line.")
  )

(cl-defgeneric magiciel--compile-access
    (type-spec offset unsigned endiennes mods len)
  (:documentation
   "Compile the expression that will access the value to be tested..

TYPE-SPEC speficies the kind of access to be prepared.  OFFSET specifies
wher to look for the value in the byte stream.  UNSIGNED tells the code
whether the access should be made as unsigned or not.  ENDIANNESS
specifies what kind of endianness to expect in the data stream.  MODS is
list of \\='modifiers\\='.  LEN tells the function how many bytes to
read.")
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
  "SPecialized `print-object` method for a `magiciel--instruction` MI on STREAM."
  (princ (format "#<MAGICIEL Instruction: [GL %d/%d] %s %S %s %s %s>"
		 (magiciel--instruction-group mi)
		 (magiciel--instruction-level mi)
		 (magiciel--instruction-offset mi)
		 (magiciel--instruction-access mi)
		 (magiciel--instruction-test mi)
		 (magiciel--instruction-value mi)
		 (magiciel--instruction-result mi))
	 stream))


;; Groups
;;
;; The following types are structures representing a group of records
;; or a group of instructions. Each "group" is essentially a "rule"
;; with tests and substests.

(cl-defstruct (magiciel--group
	       (:constructor magiciel--make-group))
  "A `magiciel--group' collects a number of items."
  (id -1 :type fixnum)
  (items () :type list)
  (strength 0 :type fixnum)
  )


(cl-defstruct (magiciel--records-group
	       (:include magiciel--group)
	       (:constructor magiciel--make-records-group)
	       (:conc-name magiciel--recs-grp-))
  "A collection of `magiciel--magic-record's.")


(cl-defstruct (magiciel--instructions-group
	       (:include magiciel--group)
	       (:constructor magiciel--make-instructions-group)
	       (:conc-name magiciel--inst-grp-))
  "A collection of `magiciel--instruction's.")


;; Compiling the Magic.
;; --------------------

;; The main function of the two steps code generation is
;; `magiciel-compile-magic' below.
;; Other ancillary functions are in this section.
;;
;; The 'file(1)' and 'magic(5)' specifications order the "rules"
;; according to "strength" (in a misterious way - meaning only by
;; looking at the C code to fathom what it is done).  Each "rule" is a
;; "group", that is a set of rules/records/instructions starting with
;; a 0-level one, plus "special" records.
;; The function `magiciel--order-groups-by-strength' operates on a
;; list of such groups (themselves lists of `magiciel--magic-record's)
;;
;; The function `magiciel--assess-group-strength' yields the
;; "strength" of a group, buy taking into consideration various
;; factors (see the 'file(5)' code) and the '!:strength' records.


;; macigiel--assess-group-strength
;;
;; This is one of the few functions built by looking at the 'file'
;; code at 'https://github.com/file/file/tree/master' repository (cf.,
;; the function `apprentice_magic_strength_1' in file
;; 'src/apprentice.c'.

(defun magiciel--assess-group-strength (group)
  "Assess the \"strength\" of a GROUP."

  (declare (ftype (function (magiciel--records-group) number)))

  (cl-flet
      ((assess-strength-r0 (r0)
	 (let* ((rec0-type (magiciel--normal-record-type r0))
		(rec0-type-test (magiciel--normal-record-test r0))
		(rec0-type-spec (magiciel--record-type-spec-spec
				 rec0-type))
		(rec0-type-test-ts (magiciel--test-test-string
				    rec0-type-test))
		(rec0-type-test-op (magiciel--test-op
				    rec0-type-test))
		(rec0-type-test-val (magiciel--test-value
				     rec0-type-test))
		(mult 10)	       ; Cf., the 'file' C source file
		(base (* 2 mult))
		(default 0)		; This is for 'default' rules.
		)
		
	   ;; Using `memq' everywhere just for symmetry.
	   ;; The code follows the C code, but with a few changes
	   ;; to shorten the code. The clause order is important.
	   ;; Some type specs may be missing.
		
	   (cond ((memq rec0-type-spec '(default))
		  (cl-return-from assess-strength-r0 default))
		 
		 ((or (magiciel--numeric-type-spec-p rec0-type-spec)
		      (magiciel--date-type-spec-p rec0-type-spec)
		      (memq rec0-type-spec '(guid)))
		  (cl-incf base (magiciel--type-spec-size
				 rec0-type-spec)))
		 
		 ((memq rec0-type-spec '(string pstring octal))
		  (cl-incf base
			   (* mult
			      (length rec0-type-test-val))))
		
		 ((memq rec0-type-spec '(bestring16 lestring16))
		  (cl-incf base
			   (* (/ mult 2)
			      (length rec0-type-test-val))))
		
		 ((memq rec0-type-spec '(search))
		  (unless (zerop (length rec0-type-test-val))
		    (cl-incf base
			     (* (length rec0-type-test-val)
				(max (/ mult
					(length rec0-type-test-val))
				     1)))))
		 
		 ((memq rec0-type-spec '(regex))
		  (let ((cnm (magiciel--count-nonmagic rec0-type-test-val)))
		    ;; Iffy.
		    (cl-incf base
			     (* cnm (max (/ mult cnm) 1)))))
		 
		 ((memq rec0-type-spec '(indirect name use clear))
					; Do nothing
		  )

		 ((memq rec0-type-spec '(der))
		  (cl-incf base mult))

		 ((not (magiciel--type-spec-p rec0-type-spec))
		  (magiciel--error "bad type spec %s" rec0-type-spec))
		 )

	   ;; Final computation on the test operator based on the actual
	   ;; test.

	   (cl-case rec0-type-test-op
	     (=
	      ;; Need to special case the 'x' test value.
	      (if (and (string= rec0-type-test-val "x")
		       (string= rec0-type-test-ts "x"))
		  (setf base 0)
		(cl-incf base mult)))
	     
	     (! (setf base 0))

	     ((< >)
	      (cl-decf base (* 2 mult)))

	     ((^ &)
	      (cl-decf base mult))
	     )

	   base
	   ))				; assess-strength-r0
       )
    (let* ((recs (magiciel--recs-grp-items group))
	   (rec0 (cl-first recs))	; This should always
					; be the 0-level record.
	   (strength-rec (cl-find-if
			  #'(lambda (r)
			      (and (magiciel--magic-record-is-special r)
				   (eq (magiciel--special-record-type r)
				       '!:strength)))
			  recs))
	   )
    
      (cl-assert (magiciel--normal-record-p rec0))
    
      (cl-multiple-value-bind (op factor)
	  (if strength-rec
	      (let ((srv (magiciel--special-record-value strength-rec)))
		(cl-values (intern (cl-first srv)) ; The operator.
			   (cl-first (read-from-string (cl-second srv)))))
	    (cl-values nil nil))

	(let ((val (assess-strength-r0 rec0)))
	  (cond ((and op factor (<= 0 factor 255))
		 (cl-case op
		   (+ (cl-incf val factor))
		   (- (cl-decf val factor))
		   (* (setf val (* val factor)))
		   (/ (setf val (/ val factor)))
		   )
 		 (if (<= val 0)
		     1
		   val))
		((null op)
		 (if (<= val 0)
		     1
		   val))
		(t
		 (magiciel--warning "!:strength record with op %s and factor %d"
				    op factor)
		 val
		 )))
	))))


(defun magiciel--assess-groups-strength (groups)
  "Compute the \"strength\" of each group in GROUPS."

  (dolist (g groups groups)
    (cl-check-type g magiciel--records-group)
    (setf (magiciel--group-strength g)
	  (magiciel--assess-group-strength g))))


;; magiciel--order-groups-by-strength

(defun magiciel--order-groups-by-strength (rec-groups)
  "Order REC-GROUPS according to their \\=':!strength\\=' specifications."

  (cl-assert (cl-every #'magiciel--records-group-p rec-groups))
  
  (magiciel--assess-groups-strength rec-groups)
	
  (sort rec-groups
	:key #'magiciel--group-strength
	:lessp #'<=
	:reverse t
	:in-place nil))


;; magiciel-compile-magic

(cl-defun magiciel-compile-magic (magic
				  &optional
				  (steps 1)
				  &aux
				  (mname
				   (magiciel--magic-name magic)))
  "Compile the MAGIC `magiciel-magic' structure.

STEPS for the time being controls where to stop in the compilation
process.

MNAME is a string containing the name of the \\='magic(5)\\=' structure
to be compiled."
  
  (magiciel--msg 1 "compiling magic %S...\n" mname)
  
  (let ((max-lisp-eval-depth 4800)
	
	;; This is necessary because of the sizes of certain groups
	;; and because of the code is not particularly smart with tail
	;; recursion.
	;;
	;; The number 4800 has been empirically determined.
	;; It works on a 2020 Intel Mac running Sonoma 14.5.
	;; YMMV, etc. etc.
	
	(code-recs
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
	  ((group-records-start (recs)
	     (process-magic-records recs ()))

	   (finish ()
	     (magiciel--msg 1 "finishing.")
	     (setf recs-groups (nreverse recs-groups))
	     )

	   (process-magic-records (recs current-group &aux (r (car recs)))
	     (magiciel--msg 1 "process-magic-records: %d to go..."
			    (length recs))
	     
	     (cond ((null recs)
		    (push (magiciel--make-records-group
			   :items current-group)
			  recs-groups)
		    (finish))

		   ((magiciel--magic-record-is-normal r)
		    (process-normal-rec r (cl-rest recs) current-group))

		   ((magiciel--magic-record-is-special r)
		    (process-special-rec r (cl-rest recs) current-group))

		   (t
		    (magiciel--error "Unexpected record %S" r))
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
		     ;; (push current-group recs-groups)
		     (push (magiciel--make-records-group
			    :items current-group)
			   recs-groups)
		     )
		   (process-magic-records more-recs (list r)))
	       ;; else...
	       ;; Continue with current group...
	       (process-magic-records more-recs
				      (append current-group (list r))))
	     )				; process-normal-rec

	   (process-special-rec (r more-recs current-group)
	     (magiciel--msg 1 "process-special-rec: %S (len %d)."
			    r (length current-group))
	     (process-magic-records more-recs
				    (append current-group (list r)))
	     )				; process-special-rec

	   (compile-group-records (recs group-n start-level)
	     
	     ;; This little local function does something crucial: it
	     ;; fixes the 'level' of the special records, by passing
	     ;; the correct 'level' to `magiciel--compile-record'.
	     ;;
	     ;; START-LEVEL should initially be 0, and, at each call
	     ;; of `magiciel--compile-record' is the level of the last
	     ;; instruction result from compiling the previous record.
	     
	     (cl-loop with l = start-level
		      for r in recs

		      if (magiciel--normal-record-p r)
		        do (setq l (magiciel--offset-level
				    (magiciel--normal-record-offset r)))
			and collect
			    (magiciel--compile-record r :group-id group-n)
		      else
		        collect
			(magiciel--compile-record r
						  :group-id group-n
						  :special-level (1+ l))
		      end
		      ))		; compile-group-records
	   )				; cl-labels

	(magiciel--msg 1 "grouping %s records..." mname)

	(when code-recs (group-records-start code-recs))
	
	(when (<= steps 1)
	  (cl-return-from magiciel-compile-magic
	    (cl-values recs-groups comp-rec-groups)))


	(cl-assert (cl-every #'magiciel--records-group-p recs-groups))
	
	;; Step 2: sorting records by "strength".

	(magiciel--msg 1 "sorting %d record groups by strength..."
		       (length recs-groups))

	(setf recs-groups
	      (magiciel--order-groups-by-strength recs-groups))


	(when (<= steps 2)
	  (cl-return-from magiciel-compile-magic
	    (cl-values recs-groups comp-rec-groups)))
	

	;; Step 3: compile each record in each group.
	;; First 0-level test in each group is then treated specially.
	
	(magiciel--msg 1 "compiling %d record groups..."
		       (length recs-groups))
	
	(cl-loop for g in recs-groups
		 for gn from 0
		 for gs = (magiciel--recs-grp-strength g)
		 for cgrp = (compile-group-records (magiciel--recs-grp-items g)
						   gn
						   0)
		 collect (magiciel--make-instructions-group
			  :id gn
			  :items cgrp
			  :strength gs)
		 into cgrps
		 finally
		 (setf comp-rec-groups cgrps)
		 )

	(when (<= steps 3)
	  (cl-return-from magiciel-compile-magic
	    (cl-values recs-groups comp-rec-groups)))
	)				; cl-labels
      )))


;; magiciel--select-op

(cl-defgeneric magiciel--select-op (op type)
  (:documentation "Select the most appropriate operator given OP and TYPE."))


(cl-defmethod magiciel--select-op ((op symbol) (type symbol))
  "Kitchen sink method for symbol OP and TYPE  (with a very large hole)."

  (ignore type)
  
  (cl-assert (member type '(number string date)))

  op)


;; The next one is trickier as I am kludging this.

(cl-defmethod magiciel--select-op ((op (eql '!)) (type symbol))
  "Select the proper operator given OP equal to `!' and TYPE being a type spec."
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
	 (magiciel--error "Cannot select a negation operator for type %s"
			  type)))
  )


;; `number' is not a "class".
;; (cl-defmethod magiciel--select-op ((op (eql '!)) (type number))
;;   (ignore op type)
;;  '/=)

(cl-defmethod magiciel--select-op ((op (eql '=)) (type (eql 'string)))
  "Select the proper operator given OP equal to `=' and TYPE being `string'."
  (ignore op type)			; ELisp is a bit stupid here.
  'string-equal)

(cl-defmethod magiciel--select-op ((op (eql '>)) (type (eql 'string)))
  "Select the proper operator given OP equal to `>' and TYPE being `string'."
  (ignore op type)			; ELisp is a bit stupid here.
  'string-greaterp)

(cl-defmethod magiciel--select-op ((op (eql '<)) (type (eql 'string)))
  "Select the proper operator given OP equal to `<' and TYPE being `string'."
  (ignore op type)			; ELisp is a bit stupid here.
  'string-lessp)

(cl-defmethod magiciel--select-op ((op (eql '!)) (type (eql 'string)))
  "Select the proper operator given OP equal to `!' and TYPE being `string'."
  (ignore op type)			; ELisp is a bit stupid here.
  ;; This works as this results is put in a macro expansion.
  '(lambda (s1 s2) (not (string-equal s1 s2)))
  )


;; Date operators assume "dates" are numeric.

(cl-defmethod magiciel--select-op ((op (eql '=)) (type (eql 'date)))
  "Select the proper operator given OP equal to `=' and TYPE being `date'."
  (ignore op type)			; ELisp is a bit stupid here.
  '=)

(cl-defmethod magiciel--select-op ((op (eql '>)) (type (eql 'date)))
  "Select the proper operator given OP equal to `>' and TYPE being `date'."
  (ignore op type)			; ELisp is a bit stupid here.
  '>)

(cl-defmethod magiciel--select-op ((op (eql '<)) (type (eql 'date)))
  "Select the proper operator given OP equal to `<' and TYPE being `date'."
  (ignore op type)			; ELisp is a bit stupid here.
  '<)

(cl-defmethod magiciel--select-op ((op (eql '!)) (type (eql 'date)))
  "Select the proper operator given OP equal to `!' and TYPE being `date'."
  (ignore op type)			; ELisp is a bit stupid here.
  '/=)


;; magiciel-compile-instr
;; Methods.

(cl-defun magiciel--pass-instr (offset msg)
  "Create a `magiciel--instruction' given OFFSET and MSG."
  ;; Unused.
  (magiciel--make-instruction :offset offset
			      :access (magiciel--offset-code offset)
			      :test 'and
			      :result msg))


(cl-defun magiciel--fix-test-value (test-value-string)
  "Fix TEST-VALUE-STRING to accommodate some of the \\='magic(5)\\=' quirks."
  ;; See the NOTES.md file for the reason we need this
  ;; here. (And maybe elsewhere).

  (let ((c0 (aref test-value-string 0)))
    (if (and (or (eql ?\\ c0) (eql ?= c0))
	     (eql (aref test-value-string 1) ?^)) ; Should test
					; for other operators as well?
	;; Since the magic test is `regex` I can do away
	;; with the `=` as well.
	(substring test-value-string 1)
      test-value-string)
    ))


(cl-defmethod magiciel--compile-instr ((off-op magiciel--offset)
				       (type magiciel--record-type-spec)
				       (test magiciel--test)
				       result-msg
				       &key
				       (group-id 0)
				       &allow-other-keys)
  "Compile and create a `magiciel--instruction'.

The compilation process dependds on record type TYPE, test
specification TEST, offset OFF-OP and message RESUTL-MGS."
  
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
				 :group-id group-id
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
	 :group group-id
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


(cl-defmethod magiciel--compile-instr
  :before ((off-op magiciel--offset)
	   (type magiciel--record-type-spec)
	   (test magiciel--test)
	   result-msg
	   &key
	   &allow-other-keys)
  "Compile instructon :before method.

OFF-OP is a `magiciel--offset', TYPE is a
`magiciel--record-type-spec', TEST is a `magiciel--test', and
RESULT-MSG is a string."
  (ignore result-msg)
  
  (let ((type-mods (magiciel--record-type-spec-modifiers type))
	(test-mods (magiciel--test-modifiers test))
	)
    (magiciel--msg 1 "compile instr\n\t%S\n\t%S\n\t%S\n\t%S\n\t%S"
		   off-op
		   type
		   type-mods
		   test
		   test-mods)
    ))


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'regex))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       (group-id 0)
				       &allow-other-keys
				       )
  "Compile a \\='regex\\=' instruction (the TYPE argument).

Arguments and Values:

OFFSET -- a `magiciel--offset'
TYPE   -- the \\='regexp\\=' symbol
TEST   -- a `magiciel--test'
RESULT-MSG -- a string
TYPE-MODS  -- a list (or NIL)
TEST-MODS  -- a list (or NIL; currently ignored)
GROUP-ID   -- an `integer' denoting the group of this instruction."
  (ignore test-mods)

  (cl-assert (or (null type-mods)
		 (magiciel--modifier-p type-mods)))

  (cl-flet
      ((compile-type-mods (mods)
	 (magiciel--msg 1 "compile-instr regex mods %S" mods)

	 (cl-assert (magiciel--modifier-p type-mods))

	 (let* ((mods-args (magiciel--modifier-args mods))
		(mod2-pos (cl-position ?/ mods-args))
		(mods1 (substring mods-args 0 mod2-pos))
		(mods2 (if mod2-pos
			   (substring mods-args mod2-pos)
			 ""
			 ))
   		(flag-pos1 (string-match-p "[cls]" mods1))
		(flag-pos2 (string-match-p "[cls]" mods2))
		)
	   (cond (flag-pos1
		  ;; "[cls]" in first part of modifiers
		  (cl-values mods1
			     (unless (string-empty-p mods2)
			       (car (read-from-string mods2)))))
		 (flag-pos2
		  (cl-values (car (read-from-string mods1))
			     mods2))
		  
		 (t (cl-values (string-to-number mods-args) nil))
		 ))
	 )

       (fix-test-value (test-value-string)
	 ;; See the NOTES.md file for the reason we need this
	 ;; here. (And maybe elsewhere).

	 (let ((c0 (aref test-value-string 0)))
	   (if (and (or (eql ?\\ c0) (eql ?= c0))
		    (eql (aref test-value-string 1) ?^)) ; Should test
					; for other operators as well?
	       ;; Since the magic test is `regex` I can do away
	       ;; with the `=` as well.
	       (substring test-value-string 1)
	     test-value-string)
	   ))
       )				; cl-flet.
    (let ((off-op (magiciel--offset-code offset))
	  (test-val
	   (rxt-pcre-to-elisp (fix-test-value (magiciel--test-value test))))
	  )
      (cl-multiple-value-bind (range flags)
	  (and type-mods (compile-type-mods type-mods))

	(ignore range)
	
	(when (cl-find ?l flags)
	  (magiciel--warn 1 "line flag in \"regex\" type not yet handled."))

	(let ((case-fold-search (cl-find ?c flags))
	      )
	  (magiciel--make-instruction
	   :group group-id
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
				       (group-id 0)
				       &allow-other-keys
				       )
  "Compile a \\='name\\=' instruction.

Arguments and Values:

OFFSET -- a `magiciel--offset'
TYPE   -- the \\='name\\=' symbol
TEST   -- a `magiciel--test'
RESULT-MSG -- a string
TYPE-MODS  -- a list (or NIL; currently ignored)
TEST-MODS  -- a list (or NIL; currently ignored)
GROUP-ID   -- an `integer' denoting the group of this instruction."
  
  (ignore type-mods test-mods)

  (let ((off-op (magiciel--offset-code offset))
	(test-val (magiciel--test-value test))
	)
    
    (magiciel--make-instruction :group group-id
				:kind type
				:offset offset
				:test `(name ,test-val ,off-op)
				:result result-msg
				)))


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'indirect))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       (group-id 0)
				       &allow-other-keys
				       )
  "Compile a \\='indirect\\=' instruction.

Arguments and Values:

OFFSET -- a `magiciel--offset'
TYPE   -- the \\='indirect\\=' symbol
TEST   -- a `magiciel--test' (currently ignored)
RESULT-MSG -- a string
TYPE-MODS  -- a list (or NIL; currently ignored)
TEST-MODS  -- a list (or NIL; currently ignored)
GROUP-ID   -- an `integer' denoting the group of this instruction."
    
  (ignore test type-mods test-mods)

  (let ((off-op (magiciel--offset-code offset)))
    (magiciel--make-instruction :group group-id
				:kind type
				:offset offset
				:test `(indirect ,off-op)
				:result result-msg)))


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'use))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       (group-id 0)
				       &allow-other-keys
				       )
  "Compile a \\='use\\=' instruction.

Arguments and Values:

OFFSET -- a `magiciel--offset'
TYPE   -- the \\='use\\=' symbol
TEST   -- a `magiciel--test'
RESULT-MSG -- a string
TYPE-MODS  -- a list (or NIL; currently ignored)
TEST-MODS  -- a list (or NIL; currently ignored)
GROUP-ID   -- an `integer' denoting the group of this instruction."
  
  (ignore type type-mods test-mods)

  (let* ((test-val (magiciel--fix-test-value (magiciel--test-value test)))
	 (switch-endian (= ?^ (aref test-val 0)))
	 )
			
    (magiciel--make-instruction :group group-id
				:kind type
				:offset offset
				:test `(use ,test-val ,switch-endian)
				:result result-msg)))


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'search))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       (group-id 0)
				       &allow-other-keys
				       )
  "Compile a \\='search\\=' instruction.

Arguments and Values:

OFFSET -- a `magiciel--offset'
TYPE   -- the \\='search\\=' symbol
TEST   -- a `magiciel--test'
RESULT-MSG -- a string
TYPE-MODS  -- a list (or NIL)
TEST-MODS  -- a list (or NIL; currently ignored)
GROUP-ID   -- an `integer' denoting the group of this instruction."
  
  (ignore test-mods)

  (let* ((off-op (magiciel--offset-code offset))
	 (test-val (magiciel--test-value test))
	 ;; (range (and type-mods (string-to-number (car type-mods))))
	 (range (and type-mods
		     (string-to-number
		      (magiciel--modifier-args type-mods))))
	 (limit (if range
		    `((string-limit %bytes%
				    (+ ,off-op ,range)))
		  `(%bytes%)
		  ))
	 )
    (magiciel--make-instruction
     :group group-id
     :kind type
     :offset offset
     :test `(string-search ,test-val ,@limit ,off-op)
     :result result-msg
     :displacement `(length ,test-val)
     )))


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'default))
				       test-val
				       result-msg
				       &key
				       type-mods
				       test-mods
				       (group-id 0)
				       &allow-other-keys
				       )
  "Compile a \\='default\\=' instruction.

Arguments and Values:

OFFSET -- a `magiciel--offset'
TYPE   -- the \\='search\\=' symbol
TEST-VAL   -- a `magiciel--test'
RESULT-MSG -- a string
TYPE-MODS  -- a list (or NIL; currently ignored)
TEST-MODS  -- a list (or NIL; currently ignored)
GROUP-ID   -- an `integer' denoting the group of this instruction."
  
  (ignore type-mods test-mods)
  
  ;; Should check that `test-val' is \"x\".
  
  (magiciel--make-instruction :group group-id
			      :kind type
			      :offset offset
			      ;; :test `(default)
			      :test t
			      :value (magiciel--test-value test-val)
			      :result result-msg))


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'clear))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       (group-id 0)
				       &allow-other-keys
				       )
  "Compile a \\='clear\\=' instruction.

Arguments and Values:

OFFSET -- a `magiciel--offset'
TYPE   -- the \\='clear\\=' symbol
TEST   -- a `magiciel--test'
RESULT-MSG -- a string
TYPE-MODS  -- a list (or NIL; currently ignored)
TEST-MODS  -- a list (or NIL; currently ignored)
GROUP-ID   -- an `integer' denoting the group of this instruction."
  
  (ignore type-mods test-mods)
  
  ;; Should check that `test-val' is \"x\".
  
  (magiciel--make-instruction :group group-id
			      :kind type
			      :offset offset
			      :test `(clear)
			      :value (magiciel--test-value test)
			      :result result-msg))


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type (eql 'offset))
				       test
				       result-msg
				       &key
				       type-mods
				       test-mods
				       (group-id 0)
				       &allow-other-keys
				       )
  "Compile a \\='offset\\=' instruction.

Arguments and Values:

OFFSET -- a `magiciel--offset'
TYPE   -- the \\='offset\\=' symbol
TEST   -- a `magiciel--test'
RESULT-MSG -- a string
TYPE-MODS  -- a list (or NIL; currently ignored)
TEST-MODS  -- a list (or NIL; currently ignored)
GROUP-ID   -- an `integer' denoting the group of this instruction."
  
  (ignore test type-mods test-mods)
  
  ;; Should check that `test-val' is \"x\".
  (let ((off-op (magiciel--offset-code offset)))
    (magiciel--make-instruction :group group-id
				:kind type
				:offset offset
				:test `(offset ,off-op)
				:result result-msg)))


(cl-defmethod magiciel--compile-instr ((offset magiciel--offset)
				       (type symbol)
				       test
				       result-msg
				       &key
				       (group-id 0)
				       &allow-other-keys)
  "Compile an instruction.

This is the kitchen sink method.

Arguments and Values:

OFFSET -- a `magiciel--offset'
TYPE   -- the \\='offset\\=' symbol
TEST   -- a `magiciel--test'
RESULT-MSG -- a string
GROUP-ID   -- an `integer' denoting the group of this instruction."

  (magiciel--msg 1 "MCI: provisional compilation of; %s." type)
  (magiciel--msg 1 "MCI: offset %s."       offset)
  (magiciel--msg 1 "MCI: level  %s."       (magiciel--offset-level offset))
  (magiciel--msg 1 "MCI: test-val %s."     test)
  (magiciel--msg 1 "MCI: result-msg %s.\n" result-msg)
  
  (magiciel--make-instruction :group group-id
			      :kind type
			      :offset offset
			      :test `(in-special-context ,type)
			      :result result-msg))


(cl-defmethod magiciel--compile-instr
  :before ((offset magiciel--offset)
	   type
	   test
	   result-msg
	   &key
	   &allow-other-keys)

  "Compile an instruction :before method.

Ensures that the OFFSET is itself compiled by calling
`magiciel--compile-offset'.

Arguments and Values:
OFFSET -- a `magiciel--offset'
TYPE   -- usually a  symbol
TEST   -- usually a `magiciel--test'
RESULT-MSG -- a string"

  (ignore type test result-msg)
  
  ;; Ensure that the offset has already been compiled.
  
  (unless (magiciel--offset-code offset)
    (setf (magiciel--offset-code offset)
	  (magiciel--compile-offset offset))))


;; magiciel--compile-offset
;; Methods.

(cl-defmethod magiciel--compile-offset ((offset
					 magiciel--direct-offset))
  "Compiles the code that will be used to compute the OFFSET."
  (if (magiciel--offset-cont offset)
      `(+ %end-pos% ,(magiciel--direct-offset-value offset))
    (magiciel--direct-offset-value offset)) ; Actually (+ %pos% ...)
  )


(cl-defmethod magiciel--compile-offset ((offset
					 magiciel--indirect-offset))
    "Compiles the code that will be used to compute the OFFSET."
  (if (magiciel--offset-cont offset)
      `(+ %end-pos% ,(magiciel--compile-indirect-offset offset))
    (magiciel--compile-indirect-offset offset)) ; Actually (+ %pos% ...)
  )


;; magiciel--compile-indirect-offset

(cl-defun magiciel--compile-indirect-offset (ind-off)
  "Compiles the code that will be used to compute an indirect offset IND-OFF."
  
  ;; FTTB
  (cl-labels
      ((compile-ind-offset-type (ind-off)

	 ;; New version.
	   
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
		 `(magiciel--get-value-at* %bytes%	       ; 1
					   (+ %pos% ,ind-addr) ; 2
					   ',type-spec	       ; 3
					   ',type-sign	       ; 4
					   ',type-endian       ; 5
					   ()		       ; 6
					   ',type-len	       ; 7
					   )
	       `(magiciel--get-value-at* %bytes%       ; 1
					 ,ind-addr     ; 2
					 ',type-spec   ; 3
					 ',type-sign   ; 4
					 ',type-endian ; 5
					 ()	       ; 6
					 ',type-len    ; 7
					 ))
	    ,(if (listp ind-num)
		 (if (magiciel--indirect-offset-rel ind-off)
		     `(magiciel--get-value-at* %bytes%
					       (+ %pos% ,ind-addr ,ind-num)
					       ',ind-subtype
					       ',(eq ind-sign '+)
					       ',type-endian
					       ()
					       ',type-len
					       )
		   `(magiciel--get-value-at* %bytes%
					     (+ ,ind-addr ,ind-num)
					     ',ind-subtype
					     ',(eq ind-sign '+)
					     ',type-endian
					     ()
					     ',type-len
					     ))
	       ind-num)))
	)))
  )


;; magiciel--compile-type
;; Methods

(cl-defmethod magiciel--compile-type ((ct magiciel--common-type-spec))
  "Compile a `magiciel--common-type-spec' CT."
  ct)


;; magiciel--compile-test
;; Methods

(cl-defmethod magiciel--compile-test ((mt magiciel--test))
  "Compile a `magiciel--test' MT."
  mt)


;; magiciel--compile-test-value

(cl-defun magiciel--compile-test-value (test-value type-spec)
  "Compile a TEST-VALUE, given a TYPE-SPEC.

Also, this is where the special TEST value \\='x\\=' is handled.:"
  
  (cond ((or (string-equal test-value "x")
	     (string-equal test-value "X")
	     ;; Yes I know about `string-equal-ignore-case'.
	     )
	 ;; This is to be treated specially.
	 ;; Hopefully the logic is ok.
	 test-value
	 )
	((magiciel--numeric-type-spec-p type-spec)
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
  "Compile a string message M.

This is just an identity operation."
  m)


(cl-defmethod magiciel--compile-message ((m magiciel--message))
  "Compile a `magiciel--message' message M."
  (magiciel--message-msg-string m))


;; magiciel--compile-access

(cl-defmethod magiciel--compile-access ((type-spec symbol)
					(offset magiciel--offset)
					unsigned
					endianness
					mods
					len
					)
  "Compile the code that will access a value in the bytes sequence.

Arguments and Values:

TYPE-SPEC  -- a symbol
OFFSET     -- a `magiciel-offset'
UNSIGNED   -- non NIL if access is unsigned
ENDIANNESS -- an indicator for the endianness (most of the times `:le' or `:be')
MODS       -- a list of modifiers (strings)
LEN        -- the length of the accessed value (in bytes)"

  (cl-assert (or (null mods) (magiciel--modifier-p mods)))
  
  (let ((mod-op (and mods (magiciel--modifier-op mods)))
	(mod-args (and mods (magiciel--modifier-args mods)))
	)

    (magiciel--msg 1 "compiling access %S %S" mod-op mod-args)
    
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

Arguments and Values:

TYPE-SPEC  : a `magiciel--numeric-type-spec'
UNSIGNED   : a `boolean'
ENDIANNESS : a `magiciel-endiannes'
MOD-OP     : an operator used to modify the access
MOD-ARGS   : a list of modifiers (cfr., the \"magic(5)\" specs).
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
				 ;; ,off-op
				 (+ %pos% ,off-op)
				 ',type-spec		  ; 3
				 ',unsigned		  ; 4
				 ',endianness		  ; 5
				 ',(list mod-op mod-args) ; 6; Unused FTTB.
				 ',len			  ; 7
				 ,off-op		  ; 8
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
  "Compile a date-based access.

Arguments and Values:

TYPE-SPEC  : a `magiciel--date-type-spec'
UNSIGNED   : a `boolean'
ENDIANNESS : a `magiciel-endiannes'
MOD-OP     : an operator used to modify the access
MOD-ARGS   : a list of modifiers (cfr., the \"magic(5)\" specs).
LEN        : the length, in \"bytes\" to be read off the byte stream.
OFFSET     : a `magiciel--offset'."
  
  (let ((off-op (if (magiciel--offset-p offset)
		    (magiciel--offset-code offset)
		  offset		; Assume it is ok...
		  ))
	)

    (ignore off-op)
    
    `(magiciel--get-value-at* %bytes%	; 1
			      ;; %pos%	; 2
			      ;; ,off-op
			      (+ %pos% ,off-op)
			      ',type-spec	       ; 3
			      ',unsigned	       ; 4
			      ',endianness	       ; 5
			      ',(list mod-op mod-args) ; 6; Unused FTTB.
			      ',len		       ; 7
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
  "Compile a string-based access.

Arguments and Values:

TYPE-SPEC  : a `magiciel--string-type-spec'
UNSIGNED   : a `boolean'
ENDIANNESS : a `magiciel-endiannes'
MOD-OP     : an operator used to modify the access
MOD-ARGS   : a list of modifiers (cfr., the \"magic(5)\" specs).
LEN        : the length, in \"bytes\" to be read off the byte stream.
OFFSET     : a `magiciel--offset'."
  
  (let ((off-op (if (magiciel--offset-p offset)
		    (magiciel--offset-code offset)
		  offset		; Assume it is ok...
		  ))
	)

    (ignore off-op)
    
    `(magiciel--get-value-at* %bytes%	; 1
			      ;; %pos%	; 2
			      ;; ,off-op
			      (+ %pos% ,off-op)
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
;; In any case, the code evolved by having `magiciel--compile-record'
;; generate an "instruction" (which may be useless, but re-represents
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
  "Compile a record.

Arguments and Values:

R  : anything"
  
  (magiciel--error "not a magic record: %s" r)
  nil)


(cl-defmethod magiciel--compile-record ((r magiciel--magic-record)
					&key &allow-other-keys)
  "Compile a record.

Arguments and Values:

R  : an unspecified `magiciel--magic-record'."
  
  (magiciel--warning "not a usable magic record: %s." r)
  nil)


(cl-defmethod magiciel--compile-record
  :before ((r magiciel--magic-record) &key &allow-other-keys)
  "Compile a record :before method.

Just used as a debugging tool.

Arguments and Values:

R  : a `magiciel--magic-record'"
  
  (magiciel--msg 1 "compiling record %S." r))

  
(cl-defmethod magiciel--compile-record
  :around ((r magiciel--magic-record) &key &allow-other-keys)
  "Compile a record :around method.

Used to catch errors while compiling.

Arguments and Values:

R  : a `magiciel--magic-record'"
  
  (condition-case cnm-error
      (cl-call-next-method)
    (error
     (magiciel--error "error %S\n\twhile compiling record\n\t%S"
		      (error-message-string cnm-error)
		      r))))


(cl-defmethod magiciel--compile-record ((r magiciel--normal-record)
					&key
					(group-id 0)
					&allow-other-keys
					)
  "Compile a normal record.

Arguments and Values:

R        : a `magiciel--normal-record'.
GROUP-ID : an integer identifying at which \\='level-0\\=' group R belongs."
    
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
					(special-level 1)
					&allow-other-keys
					)
  "Compile a special record (\\='!:mime\\=', \\='!strength\\=', etc).

Arguments and Values:

R        : a `magiciel--special-record'.
GROUP-ID : an integer identifying at which \\='level-0\\=' group R belongs.
SPECIAL-LEVEL : the level the special records must be tagged with."

  ;; The offset is fake. A default level 1 is necessary to properly
  ;; group the instruction.
  
  (magiciel--compile-instr (magiciel--make-direct-offset :level special-level)
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
  "Select the `bindat' specification based on SUBTYPE-CHAR.

SUBTYPE-CHAR comes from the indirect address specification.
Value is a list/tuple (<type> <size> <endiannes>)."

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
;; -----------------------------------------------

;; magiciel--compile-rec-group

(cl-defun magiciel--compile-rec-group (recs-group
				       &optional
				       (magic-name "magiciel--n-"))
  "Return a \\='group\\=' with a (gensymmed) name.

The \\'group\\' is simply a cons of a gensymmed MAGIC-NAME and the
records in RECS-GROUP."
  
  (let ((group-name (gensym magic-name)))
    (cons group-name recs-group)))


;; magiciel--compile-rec-groups

(cl-defun magiciel--compile-rec-groups (recs-groups
					&optional
					(magic-name "magiciel--n-"))
  "Return a list of the \\'groups\\=' in RECS-GROUPS.

The returned value is a list of conses produced by
`magiciel--compile-rec-group' called with MAGIC-NAME."
  
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
;; `--magiciel-msg'    : the "message" resulting as a description of
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

;; magiciel--check-state
;; A structure that contains the current "state" of the check.
;; This is passed around the various functions that implement the
;; scanning of a file.
;;
;; "New" state can be created as necessary.
;;
;; Notes:
;; 2024-11-11: unused, FTTB.

(cl-defstruct (magiciel--check-state
	       (:constructor magiciel--make-check-state))

  (value nil)				; The recognized value
					; (usually a string).
  (msg "" :type string)			; Accumulated "printout"
					; string.
  (start 0 :type integer)		; Starting position for check.
  (end   0 :type integer)		; End position of a check.
  (result nil :type boolean)		; A boolean containing the
					; result of the test.
  (level 0 :type integer)		; The "level" of the test.
  )


;; magiciel--$test-block
;; Useful for some of the special forms like `clear' and `default'.

(cl-defmacro magiciel--$test-block (tb-name
				    (&optional (test-result t) (msg ""))
				    &body forms)
  "Expanding into a `cl-block' named TB-NAME ending with FORMS.

Defaulting `--magiciel-test-result' and `--magiciel-msg' to
TEST-RESULT and MSG."
  
  (declare (indent 2))
  `(cl-block ,tb-name
     (setq --magiciel-test-result ,test-result
	   --magiciel-msg ,msg)
     ,@forms))


;; magiciel--$dump-state
;; Debugging macro.

(cl-defmacro magiciel--$dump-state (tag
				    &optional
				    (level magiciel--*debug-level*))
  "Debugging macro.

Prints out most of the parsing state tagged with TAG.  The printing
happens only at an appropriate debugging LEVEL (defualting to
`magiciel--**debug-level*')."
  
  `(progn
     (magiciel--msg ,level "tag         %S" ,tag)
     (magiciel--msg ,level "pos         %s" %pos%)
     (magiciel--msg ,level "end pos     %s" %end-pos%)
     (magiciel--msg ,level "context     %s" %context%)
     (magiciel--msg ,level "test-result %s" --magiciel-test-result)
     (magiciel--msg ,level "value       %S" --magiciel-value)
     (magiciel--msg ,level "disp        %S" --magiciel-disp)
     (magiciel--msg ,level "inv endian  %s" magiciel--*invert-endianness*)
     (magiciel--msg ,level "msg         %S\n" --magiciel-msg)
     ))


;; magiciel--$check
;; Sets up and track the %pos% and %end-pos% variables (FTTB).
;;
;; It should probably become a macrolet in the main generated function.
;;
;; Notes:
;; 2024-11-11: Fix doc string.

;; Old version

;; (cl-defmacro magiciel--$check (tag access test &body continue-tests)
;;   "The macro ensures that `%end-pos%` is preserved after a TEST and subtests.

;; The variable ACCESS contains an expression that represents how
;; the code will get the value to be tested by TEST (if any).
;; CONTINUE-TESTS contains the code for the \"next\" tests (and
;; other code as well; usually the first expression of
;; CONTINUE-TESTS will be a setting of the variable `%end-pos%` (the
;; \"end\" of the value previously matched), followed by other
;; house-keeping code."

;;   (declare (indent 3))

;;   `(cl-block %%test-block%%		; Maybe this tag is not right.
;;      (let* ((%saved-end-pos% %end-pos%))
;;        (magiciel--msg 1 "running check; %s (%s)" ,tag %pos%)
;;        (magiciel--msg 1 "access: %s" ',access)
;;        (magiciel--msg 1 "test: %S (%d)." ',test %end-pos%)
;;        (unwind-protect
;; 	   ,(if access
;; 		`(let* ((--magiciel-value ,access)
;; 			(--magiciel-test-result ,test)
;; 			)
;; 		   (magiciel--msg 1 "accessed value: %S.\n" --magiciel-value)

;; 		   (magiciel--msg 1 (if --magiciel-test-result
;; 					"check succeded."
;; 				      "check failed."))
;; 		   (when --magiciel-test-result
;; 		     ,@continue-tests))
;; 	      `(let ((--magiciel-test-result ,test))
;; 		 (magiciel--msg 1 (if --magiciel-test-result
;; 				      "check succeded."
;; 				    "check failed."))
;; 		 (when --magiciel-test-result
;; 		   (magiciel--msg 1 "check succeded.")
;; 		   ,@continue-tests)))
;; 	 (setq %end-pos% %saved-end-pos%)))
;;      )
;;   )

;; New Version

;; (cl-defmacro magiciel--$check (tag
;; 			       access
;; 			       test
;; 			       ;; test-false-throw-label
;; 			       test-level
;; 			       &body continue-tests)
;;   "The macro ensures that `%end-pos%` is preserved after a TEST and subtests.

;; The variable ACCESS contains an expression that represents how
;; the code will get the value to be tested by TEST (if any).
;; CONTINUE-TESTS contains the code for the \"next\" tests (and
;; other code as well; usually the first expression of
;; CONTINUE-TESTS will be a setting of the variable `%end-pos%` (the
;; \"end\" of the value previously matched), followed by other
;; house-keeping code."

;;   (declare (indent 4))

;;   `(let* ((%saved-end-pos% %end-pos%)
;; 	  ;; (--magiciel-test-result nil)
;; 	  )
;;      (magiciel--msg 1 "running check; %S (pos: %s)" ,tag %pos%)
;;      (magiciel--msg 1 "access: %S" ',access)
;;      (magiciel--msg 1 "test: %S (end pos: %d)." ',test %end-pos%)
;;      (unwind-protect
;; 	 ,(if access
	      
;; 	      `(let ((--magiciel-value ,access))
;; 		 (setq --magiciel-test-result ,test)
;; 		 (magiciel--msg 1 "accessed value: %S." --magiciel-value)
;; 		 (if (numberp --magiciel-value)
;; 		     (magiciel--msg 1 "accessed num value: %s (%x %o)."
;; 				    --magiciel-value
;; 				    --magiciel-value
;; 				    --magiciel-value
;; 				    )
;; 		   (magiciel--msg 1 "accessed value: %S." --magiciel-value)
;; 		   )

;; 		 (magiciel--msg 1 "--magiciel-msg = '%s'" --magiciel-msg)
;; 		 (magiciel--msg 1 (if --magiciel-test-result
;; 				      "check succeded."
;; 				    "check failed."))
;; 		 (magiciel--msg 1 "")
;; 		 (if --magiciel-test-result
;; 		     (progn ,@continue-tests)
;; 		   ;; (go ,test-false-throw-label)
;; 		   (when (zerop ,test-level)
;; 		     (magiciel--msg 1 "throwing to level 0 exit from '%s'." ,tag)
;; 		     (magiciel--msg 1 "")
;; 		     (throw '%%exit-level%%
;; 		    	    (cl-values
;; 		    	     --magiciel-msg
;; 			     --magiciel-disp
;; 			     --magiciel-test-result ; This should be `nil'.
;; 		    	     )))
;; 		   ))
	    
;; 	    `(progn
;; 	       (setq --magiciel-test-result ,test)
;; 	       (magiciel--msg 1 "--magiciel-msg = '%s'" --magiciel-msg)
;; 	       (magiciel--msg 1 (if --magiciel-test-result
;; 				    "check succeded."
;; 				  "check failed."))
;; 	       (magiciel--msg 1 "")
;; 	       (if --magiciel-test-result
;; 		   (progn ,@continue-tests)
;; 		 ;; (go ,test-false-throw-label)
;; 		 (when (zerop ,test-level)
;; 		   (magiciel--msg 1 "throwing to level 0 exit from '%s'." ',tag)
;; 		   (magiciel--msg 1 "")
;; 		   (throw '%%exit-level%%
;; 		  	  (cl-values
;; 		  	   --magiciel-msg
;; 		  	   --magiciel-disp
;; 		  	   --magiciel-test-result ; This will be `nil'.
;; 		  	   )))
;; 		 ))
;; 	    )
;;        (magiciel--msg 1 "test result %S." --magiciel-test-result)
;;        (unless --magiciel-test-result
;; 	 (setq %end-pos% %saved-end-pos%)))
;;      )
;;   )

;; New, new version, much simplified.
;; The "exit" at level zero is handled only at level zero.

(cl-defmacro magiciel--$check (tag
			       access
			       test
			       ;; test-false-throw-label
			       test-level
			       &body continue-tests)
  "The macro ensures that `%end-pos%' is preserved after a TEST and subtests.

The variable ACCESS contains an expression that represents how
the code will get the value to be tested by TEST (if any).
CONTINUE-TESTS contains the code for the \"next\" tests (and
other code as well; usually the first expression of
CONTINUE-TESTS will be a setting of the variable `%end-pos%' (the
\"end\" of the value previously matched), followed by other
house-keeping code.  TAG is used for debugging purposes."

  (declare (indent 4))
  (ignore test-level)

  `(let* ((%saved-end-pos% %end-pos%)  ; Need to save more state.
	  ;; (--magiciel-test-result nil)
	  )
     (magiciel--msg 1 "running check; %S (pos: %s)" ,tag %pos%)
     (magiciel--msg 1 "access: %S" ',access)
     (magiciel--msg 1 "test: %S (end pos: %d)." ',test %end-pos%)
     (unwind-protect
	 ;; I produce slightly different code depending on the
	 ;; `access' expression.
	 
	 ,(if access
	      
	      `(let ((--magiciel-value ,access))
		 (setq --magiciel-test-result ,test)
		 (magiciel--msg 1 "accessed value: %S." --magiciel-value)
		 (if (numberp --magiciel-value)
		     (magiciel--msg 1 "accessed num value: %s (%x %o)."
				    --magiciel-value
				    --magiciel-value
				    --magiciel-value
				    )
		   (magiciel--msg 1 "accessed value: %S." --magiciel-value)
		   )

		 (magiciel--msg 1 "--magiciel-msg = '%s'" --magiciel-msg)
		 (magiciel--msg 1 (if --magiciel-test-result
				      "check succeded."
				    "check failed."))
		 (magiciel--msg 1 "")
		 (when --magiciel-test-result
		     ;; `continue-tests' contains all the "next" code
		     ;; and the next tests at level N+1.  Cf., the
		     ;; overall tree code construction below.
		   ,@continue-tests
		   )
		 )
	    
	    `(progn
	       (setq --magiciel-test-result ,test)
	       (magiciel--msg 1 "--magiciel-msg = '%s'" --magiciel-msg)
	       (magiciel--msg 1 (if --magiciel-test-result
				    "check succeded."
				  "check failed."))
	       (magiciel--msg 1 "")
	       (when --magiciel-test-result
		 ;; See comment above for `continue-tests'.
		 ,@continue-tests
		 ))
	    )
       (magiciel--msg 1 "test result %S." --magiciel-test-result)
       (unless --magiciel-test-result
	 (setq %end-pos% %saved-end-pos%)))
     )
  )


;; magiciel--$acc-msg-disp

(cl-defmacro magiciel--$acc-msg-disp (s &optional (d 0))
  "Macro used to accumulate the message S into `--magiciel-msg'.

The macro also sets `--magiciel-disp' to D.

Notes:

Unused."
  
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
    `(progn
       (setf --magiciel-msg
	     (concat --magiciel-msg ,msg)

	     %end-pos%
	     (+ ,start-pos ,displ))
       (magiciel--msg 1 "stored --magiciel-test-result = %s" --magiciel-test-result)
       (magiciel--msg 1 "stored --magiciel-msg = '%s'" --magiciel-msg)
       )
    )
  )


(cl-defmacro magiciel--$noop (&rest args)
  "Macro used in generated code that results in a no-op, ignoring ARGS."
  
  `(progn
     (ignore ',args)
     t))


(cl-defmacro magiciel--$name (&rest args)
  "Macro used in generated code for \\='name\\=' clauses; ignoring ARGS."

  `(progn
     (ignore ',args)
     (setf --magiciel-test-result t))	; This setting is implied by the spec.
  )


(cl-defmacro magiciel--$in-context-p (c)
  "Macro used to test that the parsing context is C."
  
  `(eq %context% ,c))


(cl-defmacro magiciel--$level-0-exit (level test-result)
  "Macro used to jump out of LEVEL 0 when TEST-RESULT is false.

Notes:

Unused."
  
  `(when (and (zerop ,level) (null ,test-result))
     (magiciel--msg 1 "throwing to level 0 exit.")
     (magiciel--msg 1 "")
     (throw '%%exit-level%%
	    (cl-values
	     --magiciel-msg
	     --magiciel-disp
	     ,test-result		; This should be `nil'.
	     ))
     ))


;; Old version.

;; (cl-defmacro magiciel--$use (name-label &optional invert)
;;   `(,name-label %bytes% %pos% %end-pos% %context% ,invert)
;;   )

;; New version that allows for "external calls".  It assumes that the
;; "used" function (the `name-label') returns the "displacement" and
;; the "message" if produced.

(cl-defmacro magiciel--$use (name-label pos context &optional invert)
  "Macro used in generated code to handle \\='use\\=' clauses.

The function NAME-LABEL is called with `%bytes%', POS (evaluated),
`%end-pos%' and CONTEXT (evaluated)

`--maiciel-msg' and `%end-pos%' are updated as a result of the call to
NAME-LABEL.

Notes:

The INVERT argument is essentially ignored, as the endianness
\"inversion\" is dealt differently, with a special (dynamic)
variable."
  
  `(progn
     (magiciel--msg 1 "$use %S with pos %s, context %s, invert end %s."
		    ',name-label ,pos ,context ,invert)
     (cl-multiple-value-bind (%msg% %p% %ep%)
	 (,name-label %bytes%
		      ,pos
		      %end-pos%
		      ,context
		      ;; ,invert
		      )

       (ignore %p%)
       ;; I cannot use `magiciel--$store-access-result' because of the
       ;; macro-expansion time processing of the first argument.
       ;; Therefore I just deal with `%disp%' and `%message' directly.
       ;;
       ;; (magiciel--$store-access-result %message% '%pos% %disp%)
       (setf --magiciel-msg %msg%
	     %end-pos% %ep%)
       ))
  )


;; magiciel--emit methods.

;; (cl-defmethod magiciel--emit ((kind (eql '==)) ; Never match!
;; 			      (inst magiciel--instruction)
;; 			      &key
;; 			      (f-name (gensym "magiciel--%%fn-top-"))
;; 			      (m-name (gensym "magiciel--i-n-"))
;; 			      (displacement 0)
;; 			      &allow-other-keys)
;;   "This method is the main \\='emit\\=' method for regular tests.

;; The KIND argument indicates all boolean tests.  The DISPLACEMENT
;; variable is an \\='expression\\=' that comes from the \"previous\"
;; test and that is used for \"continuation\" offsets."

;;   ;; Note:
;;   ;; Maybe change KIND specializer to `boolean'.

;;   (ignore kind f-name m-name displacement)
  
;;   (let ((level  (magiciel--instruction-level inst))
;; 	(offset (magiciel--instruction-offset inst))
;; 	(access (magiciel--instruction-access inst))
;; 	(test   (magiciel--instruction-test inst))
;; 	(value  (magiciel--instruction-value inst))
;; 	(result (magiciel--instruction-result inst))
;; 	(disp   (magiciel--instruction-displacement inst))
;; 	)

;;     (ignore level disp offset)

;;     ;; Let's handle test value \\='x\\=' (cfr., "magic(5)" man page.)

;;     (if (or (eq value 'x)
;; 	    (string-equal value "x")
;; 	    (string-equal value "X"))
;; 	`(let ((--magiciel-value ,access))
;; 	   (when t (magiciel--$acc-msg-disp (format ,result --magiciel-value)))
;; 	   ;; The "when" here is needed because I am an idiot (FTTB).
;; 	   )
    
;;       `(let ((--magiciel-value ,access))
;; 	 (when (,test --magiciel--value ,value)
;;            (magiciel--$acc-msg-disp (format ,result --magiciel-value))
;;            ))
;;       )
;;     ))


;; This is unused FTTB.  The \\='boolean is not used (yet), but this
;; is where I experiment.




;; (cl-defmethod magiciel--emit ((kind (eql '=))
;; 			      ;; (kind (eql 'boolean))
;; 			      (inst magiciel--instruction)
;; 			      &key
;; 			      (f-name (gensym "magiciel--%%fn-top-"))
;; 			      (m-name (gensym "magiciel--i-n-"))
;; 			      (displacement 0)
;; 			      &allow-other-keys)
;;   "This method is the main \\='emit\\=' method for regular tests.

;; The KIND argument indicates all boolean tests.  The DISPLACEMENT
;; variable is an \\='expression\\=' that comes from the \"previous\"
;; test and that is used for \"continuation\" offsets."

;;   (ignore kind f-name m-name displacement)
  
;;   (let ((level  (magiciel--instruction-level inst))
;; 	(offset (magiciel--instruction-offset inst))
;; 	(access (magiciel--instruction-access inst))
;; 	(test   (magiciel--instruction-test inst))
;; 	(value  (magiciel--instruction-value inst))
;; 	(result (magiciel--instruction-result inst))
;; 	(disp   (magiciel--instruction-displacement inst))
;; 	)

;;     (ignore level disp)
    
;;     (let ((pos (if (magiciel--offset-cont offset) '%end-pos% '%pos%))
;; 	  (tag (intern (format "%s-%s" kind value)))
;; 	  )

;;       (magiciel--msg 1 "%s" inst)
      
;;       (if (or (eql value 'x)
;; 	      (and (stringp value)
;; 		   (or (string-equal value "x")
;; 		       (string-equal value "X"))))
;; 	  `(magiciel--$check ',tag
;; 	       ,access
;; 	       t
;; 	       %%exit-label%%
;; 	     (magiciel--$store-access-result ,result ,pos ,disp))
;; 	`(magiciel--$check ',tag
;; 	     ,access
;; 	     (,test --magiciel-value ,value)
;; 	     %%exit-label%%
;; 	   (magiciel--$store-access-result ,result ,pos ',disp))
;; 	)
;;       )))


;; New version

(cl-defmethod magiciel--emit ((kind (eql '=))
			      ;; (kind (eql 'boolean))
			      (inst magiciel--instruction)
			      &key
			      (f-name (gensym "magiciel--%%fn-top-"))
			      (m-name (gensym "magiciel--i-n-"))
			      (displacement 0)
			      &allow-other-keys)
  "Emit code for regular \\='magic(5)\\=' clauses.

This method is the main \\='emit\\=' method for normal tests.

The KIND argument indicates all boolean tests.  The DISPLACEMENT
variable is an \\='expression\\=' that comes from the \"previous\"
test and that is used for \"continuation\" offsets.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST.  F-NAME is the
name of the enclosing function and M-NAME are used as a debugging aid."

  (ignore kind f-name m-name displacement)
  
  (let ((level  (magiciel--instruction-level inst))
	(offset (magiciel--instruction-offset inst))
	(access (magiciel--instruction-access inst))
	(test   (magiciel--instruction-test inst))
	(value  (magiciel--instruction-value inst))
	(result (magiciel--instruction-result inst))
	(disp   (magiciel--instruction-displacement inst))
	)

    (ignore disp)

    (magiciel--msg 1 "%s" inst)
    
    (let ((pos (if (magiciel--offset-cont offset) '%end-pos% '%pos%))
	  ;; (tag (intern (format "%s-%s" kind value)))
	  (tag (intern (format "%s-%s-%s" kind test value)))
	  (test (if (or (eql value 'x)
			 (and (stringp value)
			      (or (string-equal value "x")
				  (string-equal value "X"))))
		    t
		  `(,test --magiciel-value ,value)
		  ))
	  )
      `(magiciel--$check ',tag
	   ,access
	   ,test
	   ',level
	 (magiciel--$store-access-result ,result ,pos ,disp))
      )))


(cl-defmethod magiciel--emit ((kind (eql 'regex))
			      (inst magiciel--instruction)
			      &key
			      &allow-other-keys)

  "Emit code for \\='magic(5)\\=' \\='regex\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST."

  (ignore kind)
  (let* ((level  (magiciel--instruction-level inst))
	 ;; (access (magiciel--instruction-access inst))
	 (test   (magiciel--instruction-test inst))
	 (value  (magiciel--instruction-value inst))
	 (result (magiciel--instruction-result inst))
	 (tag (intern (format "regexp-%s" value)))
	 )
    `(magiciel--$check ',tag
	 nil
	 ,test
	 ',level
       (magiciel--$store-access-result ,result
				       %pos%
				       (length (match-string 0 %bytes%))
				       (format ,result
					       (match-string 0 %bytes%)))))
  )


(cl-defmethod magiciel--emit ((kind (eql 'name))
			      (inst magiciel--instruction)
			      &key
			      &allow-other-keys)
  "Emit code for \\='magic(5)\\=' \\='name\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST."

  (ignore kind)

  (let ((level  (magiciel--instruction-level inst))
	;; (access (magiciel--instruction-access inst))
	(test   (magiciel--instruction-test inst))
	;; (value  (magiciel--instruction-test inst))
	(result (magiciel--instruction-result inst))
	)

    (ignore level)
    
    `(cl-block nil (magiciel--$name ',(cl-second test) ',(cl-third test) ',result))
    )
  )


(cl-defmethod magiciel--emit ((kind (eql 'indirect))
			      (inst magiciel--instruction)
			      &key
			      &allow-other-keys)

  "Emit code for \\='magic(5)\\=' \\='indirect\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST.

Notes:

This test is not currently handled."
    
  (ignore kind inst)
  
  `(magiciel--$test-block indirect (nil --magiciel-msg)
     (magiciel--warn 0 "'indirect' not implemented; failing.")
     nil
     ))


(cl-defmethod magiciel--emit ((kind (eql 'use))
			      (inst magiciel--instruction)
			      &key
			      (env ())
			      &allow-other-keys)

  "Emit code for \\='magic(5)\\=' \\='name\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST.

Notes:

For this instruction the code needs to refer to the proper \"name\",
hence the reliance on the ENV variable."

  (ignore kind)
  
  (let* (
	 ;; (level  (magiciel--instruction-level inst))
	 ;; (access (magiciel--instruction-access inst))
	 (offset (magiciel--instruction-offset inst))
	 (test   (magiciel--instruction-test inst))
	 ;; (value  (magiciel--instruction-value inst))
	 (result (magiciel--instruction-result inst))

	 ;; The next use fo the "fix" is just being paranoid.
	 (use-name-0 (magiciel--fix-test-value (cl-second test)))
	 
	 (invert-endianness (char-equal ?^ (aref use-name-0 0)))
	 (use-name (if invert-endianness
		       (substring use-name-0 1)
		     use-name-0))
	 (name-label (assoc use-name env #'string-equal))
	 )

    (unless name-label
      (magiciel--warning "unknown label %S; test will probably fail."
			 use-name))

    (if name-label
	(let ((pos
	       ;; (if (magiciel--offset-cont offset) '%end-pos% '%pos%)
	       (magiciel--compile-offset offset)
	       )
	      (label-name (cdr name-label))
	      (use-block-name (intern (concat (cl-second test) "-use-block")))
	      )
	  ;; `(cl-block ,use-block-name
	  ;;    (magiciel--$use ,label-name ,invert-endianness)
	  ;;    ;; (magiciel--$acc-msg-disp ,result)
	  ;;    (magiciel--$store-access-result (concat --magiciel-msg ,result)
	  ;; 				     ,pos
	  ;; 				     )
	  ;;    )
	  ;; `(magiciel--$test-block ,use-block-name ()
	  ;;    (magiciel--$use ,label-name ,invert-endianness)
	  ;;    ;; (magiciel--$acc-msg-disp ,result)
	  ;;    (magiciel--$store-access-result ,result ,pos)
	  ;;    )
	  (if invert-endianness
	      ;; Wrap the call to the $use instuction in a proper `let'.
	      `(cl-block ,use-block-name
		 (let ((magiciel--*invert-endianness* t))
		   (magiciel--$use ,label-name
				   (+ %pos% ,pos)
				   %context%
				   ;; ,(or invert-endianness magiciel--*invert-endianness*)
				   ))
		 (magiciel--$store-access-result ,result ,pos)
		 )
	    ;; ... else, do nothing.
	    `(cl-block ,use-block-name
	       (magiciel--$use ,label-name
			       (+ %pos% ,pos)
			       %context%
			       ;; ,(or invert-endianness magiciel--*invert-endianness*)
			       )
	       (magiciel--$store-access-result ,result ,pos)
	       )
	    ))
      `(progn nil))
    ))


(cl-defmethod magiciel--emit ((kind (eql 'search))
			      (inst magiciel--instruction)
			      &key
			      &allow-other-keys)
  "Emit code for \\='magic(5)\\=' \\='search\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST."

  (ignore kind)
  
  (let* ((level  (magiciel--instruction-level inst))
	 ;; (access (magiciel--instruction-access inst))
	 (offset (magiciel--instruction-offset inst))
	 (test   (magiciel--instruction-test inst))
	 (value  (magiciel--instruction-value inst))
	 (result (magiciel--instruction-result inst))
	 (disp   (magiciel--instruction-displacement inst))
	 (tag (intern (format "search-%s" value)))
	 (pos (if (magiciel--offset-cont offset) '%end-pos% '%pos%))
	 )
    `(magiciel--$check ',tag
	 nil
	 ,test
	 ',level
       (magiciel--$store-access-result ,result ,pos ,disp)))
  )


(cl-defmethod magiciel--emit ((kind (eql 'default))
			      (inst magiciel--instruction)
			      &key
			      &allow-other-keys)
    "Emit code for \\='magic(5)\\=' \\='default\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST."

  (ignore kind)
  
  (let ((level  (magiciel--instruction-level inst))
	;; (access (magiciel--instruction-access inst))
	(offset (magiciel--instruction-offset inst))
	(test   (magiciel--instruction-test inst))
	;; (value  (magiciel--instruction-test inst))
	(result (magiciel--instruction-result inst))
	)
    (let ((pos (if (magiciel--offset-cont offset) '%end-pos% '%pos%))
	  (tag 'default)
	  )
      `(magiciel--$check ',tag
	   nil
	   ,test
	   ',level
	 ;; %%exit-level%%
	 (magiciel--$store-access-result ,result ,pos)))
    )
  )


(cl-defmethod magiciel--emit ((kind (eql 'clear))
			      (inst magiciel--instruction)
			      &key
			      ;; (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)

  "Emit code for \\='magic(5)\\=' \\='clear\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST."
  
  (let (;; (level  (magiciel--instruction-level inst))
	(offset (magiciel--instruction-offset inst))
	;; (access (magiciel--instruction-access inst))
	;; (test   (magiciel--instruction-test inst))
	(value  (magiciel--instruction-value inst))
	(result (magiciel--instruction-result inst))
	;; (disp   (magiciel--instruction-displacement inst))
	)
        
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
      
      `(cl-block ,tag
	 (setq --magiciel-test-result t)
	 (magiciel--msg 1 "clear; '%s' '%s'" --magiciel-msg ',result)
	 (set-match-data ())		; Living dangerously!
	 (magiciel--$store-access-result ,result ,pos))
      )))


(cl-defmethod magiciel--emit ((kind (eql '!:apple))
			      (inst magiciel--instruction)
			      &key
			      (f-name (gensym "magiciel--%%fn-top-"))
			      ;; (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)
  "Emit code for \\='magic(5)\\=' \\='!:apple\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST.

F-NAME is the name of the enclosing function and `cl-block' to be
returned from."

  (magiciel--msg 1 "EMIT: emit APPLE %s." kind)
  `(cl-block ,kind
     (when (magiciel--$in-context-p ',kind)
       (warn "MAGICIEL: !:apple %s not properly handled yet."
	     ,(magiciel--instruction-result inst))
       ;; (cl-return-from %%test-block%% ,(magiciel--instruction-result inst))
       (cl-return-from ,f-name ,(magiciel--instruction-result inst))
       ))
  )


;; (cl-defmethod magiciel--emit ((kind (eql '!:mime))
;; 			      (inst magiciel--instruction)
;; 			      &key
;; 			      (f-name (gensym "magiciel--%%fn-top-"))
;; 			      (m-name (gensym "magiciel--i-n-"))
;; 			      &allow-other-keys)
;;   (ignore m-name)
  
;;   (magiciel--msg 1 "EMIT: emit MIME %s." kind)
;;   `(magiciel--$check '!:mime-check
;;        nil
;;        (magiciel--$in-context-p ',kind)
;;      %%exit-label%%
;;      (setq --magiciel-msg ,(magiciel--instruction-result inst))
;;      ;; (cl-return-from %%test-block%% --magiciel-msg)
;;      (cl-return-from ,f-name --magiciel-msg)
;;      )
;;   )

(cl-defmethod magiciel--emit ((kind (eql '!:mime))
			      (inst magiciel--instruction)
			      &key
			      (f-name (gensym "magiciel--%%fn-top-"))
			      ;; (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)
  "Emit code for \\='magic(5)\\=' \\='!:mime\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST.

F-NAME is the name of the enclosing function and `cl-block' to be
returned from."
   
  (magiciel--msg 1 "EMIT: emit MIME %s." kind)

  `(when (magiciel--$in-context-p ',kind)
     (setq --magiciel-msg ,(magiciel--instruction-result inst))
     (cl-return-from ,f-name --magiciel-msg))
  )


(cl-defmethod magiciel--emit ((kind (eql '!:strength))
			      (inst magiciel--instruction)
			      &key
			      &allow-other-keys)

  "Emit code for \\='magic(5)\\=' \\='!:strength\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST.

Notes:

Unimplemented."

  (magiciel--msg 1 "EMIT: emit STRENGTH %s." kind)
  `(when (magiciel--$in-context-p ',kind)
     (magiciel--$noop ,(magiciel--instruction-result inst)))
  )


(cl-defmethod magiciel--emit ((kind (eql '!:ext))
			      (inst magiciel--instruction)
			      &key
			      (f-name (gensym "magiciel--%%fn-top-"))
			      ;; (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)

  "Emit code for \\='magic(5)\\=' \\='!:ext\\=' KIND clauses.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST.

F-NAME is the name of the enclosing function and `cl-block' to be
returned from."

  (magiciel--msg 1 "EMIT: emit EXT %s (%S)." kind inst)
  `(when (magiciel--$in-context-p ',kind)
     (setq --magiciel-msg ,(magiciel--instruction-result inst))
     (cl-return-from ,f-name --magiciel-msg)
     )
  )


(cl-defmethod magiciel--emit ((kind symbol)
			      (inst magiciel--instruction)
			      &key
			      ;; (m-name (gensym "magiciel--i-n-"))
			      &allow-other-keys)
  "Emit code for clauses of unhandled symbol KIND.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST.

This method is a \"kitchen sink\" that will essentially just emit code
to return the result in INST.

Notes:

This method should never get called in a meaningful way; it is here
just to avoid blocking the compilation process."

  (magiciel--msg 1 "EMIT: unhandled kind %s." kind)
  `(; cl-return-from %%test-block%%
    ;; progn
    cl-block ,kind
     ,(magiciel--instruction-result inst))
  )


(cl-defmethod magiciel--emit :before ((kind symbol)
				      (inst magiciel--instruction)
				      &key
				      (m-name (gensym "magiciel--i-n-"))
				      &allow-other-keys)
  "Yields debugging message before emitting code.

All the necessary information to properly \\'emit\\' the code for the
test is packed into the `magiciel--instruction' INST.

M-name is symbol used for debugging purposes."

  (ignore inst)
  
  (magiciel--msg 1 "EMIT: emitting %s for %s." kind m-name)
  )


;; Instruction trees.
;; Another function to build up the instruction tree.

(cl-defstruct (magiciel--instr-node
	       (:type list)
	       (:constructor
		magiciel--make-instr-node (instr &optional sub-instr-nodes))
	       )
  (id (gensym "INSTRN-"))
  instr
  (sub-instr-nodes ())
  )


;; magiciel--dump-instr-tree, magiciel--dump-instr-level-alist
;; Useful debugging functions.

(cl-defun magiciel--dump-instr-tree (it &optional (indent 0))
  "Dumps a textual representation of an instruction tree IT.

Each leve is indented a multiple of INDENT."
  
  (unless (null it)
    (let ((i (magiciel--instr-node-instr it))
	  (id (magiciel--instr-node-id it))
	  (sts (magiciel--instr-node-sub-instr-nodes it))
	  )
      (magiciel--msg 1 "MDIT: %s%s %s %s '%s'"
		     (make-string indent ? )
		     (magiciel--instruction-level i)
		     id
		     (magiciel--instruction-kind i)
		     (magiciel--instruction-test i))
      (dolist (st sts)
	(magiciel--dump-instr-tree st (+ indent 4)))
      )))


(cl-defun magiciel--dump-instr-level-alist (instr-nodes-at-level)
  "Dumps an a-list representation of the instructions INSTR-NODES-AT-LEVEL."
  
  (magiciel--msg 1 "MILA:")
  (cl-loop for (il . instr-node) in instr-nodes-at-level
	   do (magiciel--msg 1 "MILA: %s => %s %s '%s'"
			     il
			     (magiciel--instr-node-id instr-node)
			     (magiciel--instruction-kind
			      (magiciel--instr-node-instr instr-node))
			     (magiciel--instruction-test
			      (magiciel--instr-node-instr instr-node))
			     )
	   finally (magiciel--msg 1 "MILA:\n")
	   ))


;; Construction instruction trees for groups.

(cl-defun magiciel--group-trees (instr-group)
  "Organize a group of instructions INSTR-GROUP into a tree."
  
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
  "Builds a tree of instructions mirroring the \\='magic(5)\\=' clauses.

A group has one 0 level entry point; the first instruction.
The function returns a subtree rooted at the first (0 level)
instruction.  Each (sub) tree is simply a cons (root . children)

Arguments and Values:

INSTR-NODE  : first instruction node in (remaining) instruction node list.
MORE-INSTRS : instructions after `instr'; can be an empty list.
INSTR-NODES-AT-LEVEL' : an a-list (organized in a stack-like manner)
                        of nodes at a given level, where to add
                        subtrees; keys are the \"level\" of the node
                        last seen at a given level.

Notes:

The function is not particularly efficient, but magic(5) files
are never too big."

  (let* ((instr (magiciel--instr-node-instr instr-node))
	 (il (magiciel--instruction-level instr))
	 (parent-node (cdr (assoc (1- il) instr-nodes-at-level)))
	 )
    ;; `parent-node' is the last level (`level' - 1) node seen.
    
    (magiciel--msg 1 "MGIT: instr level %d, instr kind '%s'."
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
  "Build the function or block body to run the i-th check.

The function obtained correspond to the groups obtained from the
compilation of a magic(5) file.  The name of the function is
\\='=magiciel--<magic name>-<group-number>\\='=.

Arguments and Values:

MAGIC : is a `magiciel--magic' struct.
;; INSTR-GROUP  : is a list of `magiciel--instruction'.
INSTR-GROUP  : is a `magiciel--instructions-group'.
GROUP-NUMBER : is the id of the instruction; must be unique.
MAGIC-ENV    : is a simple \\='enviroment\\=' (an a-list) containg
               bindings for names used in the group/block; mostly label
               names associated to \"name\" records, to be used by
               \"use\" records.

Notes:

Groups with \\='=name\\='= and \\='=use\\='= operation are treated
specially."
  
  (let* ((name (magiciel--magic-name magic))

	 (instrs (magiciel--group-items instr-group))

	 (ig-strength (magiciel--group-strength instr-group))

	 (magic-fname (magiciel--magic-fname magic))

	 (magic-group-fname
	  (intern (format "%s-%d" magic-fname group-number)))

	 (magic-group-block-name
	  (intern (format "%s-block-%d-%d" name group-number ig-strength)))

	 (magic-group-exit-label
	  (intern (format "%s-%d-exit-label" name group-number)))

	 (magic-group-continue-label
	  (intern (format "%s-%d-continue-label" name group-number)))

	 ;; (instrs-tree
	 ;;  (magiciel--group-trees instr-group))

	 ;; (instrs-tree-name
	 ;;  (magiciel--group-name instr-group))

	 (instrs-tree
	  (magiciel--group-trees instrs))

	 (instrs-tree-name
	  (magiciel--group-name instr-group))
	 
	 (exit-code
	  (if instrs-tree-name		; We have a "name" group.
	      `(cl-return-from ,instrs-tree-name
		 (cl-values
		  --magiciel-msg
		  %pos%
		  ;; --magiciel-disp	; Or maybe (- %end-pos% %pos%)
		  %end-pos%
		  ))
	    
	    `(cl-return-from ,magic-fname
	       (cl-values
		--magiciel-msg
		%pos%
		;; --magiciel-disp	; Or maybe (- %end-pos% %pos%)
		%end-pos%
		)))
	  )
	 )

    (ignore magic-group-exit-label
	    magic-group-continue-label
	    exit-code)

    (magiciel--msg 1 "MCM: instruction tree:")
    (magiciel--msg 1 "MCM: instruction tree is named (%s):" instrs-tree-name)

    (magiciel--dump-instr-tree instrs-tree)


    ;; The code emitted for each "tree" will cause side effects and
    ;; will return a generalized boolean (or better, the returned
    ;; value will be considered as such).
    
    (cl-labels
	((emit-tree (instr-node &optional (level 0))
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

		  (magic-subgroup-block-name
		   (intern (format "%s.%d" magic-group-block-name
				   (1+ level))))

		  ;; (sub-tests-code
		  ;;  (when sub-tests
		  ;;    ;; Double list is necessary as this will be used
		  ;;    ;; in a ',@' backquote.
		  ;;    `((cl-block ,magic-subgroup-block-name
		  ;; 	 ;; catch '%%exit-level%%
		  ;; 	 ,@(cl-loop for sub-node in sub-tests
		  ;; 		    append (emit-tree sub-node (1+ level))
		  ;; 		    )))
		  ;;    ))
		  (sub-tests-code
		   (when sub-tests
		     (cl-loop for sub-node in sub-tests
			      append (emit-tree sub-node (1+ level))
			      )))
		  )

	     (ignore magic-subgroup-block-name)

	     (cl-case (cl-first instr-root-code)

	       ((progn-no)    ; Just to emphasize that this is "code".
		;; I want to "flatten" the "progn's, if possible.
		(magiciel--msg 1 "MGE: progn: %s." (cl-first instr-root-code))
		(append instr-root-code
			;; (cl-rest instr-root-code)
		  	(cl-loop for sub-node in sub-tests
			         collect (emit-tree sub-node (1+ level)))
			)
		)
	       ((cl-block-no)
		(magiciel--msg 1 "MGE: cl-block: %s." (cl-first instr-root-code))
		(append instr-root-code
			(cl-loop for sub-node in sub-tests
				 collect (emit-tree sub-node (1+ level)))
			)
		)
	       (t
		(magiciel--msg 1 "MGE: t: %s." (cl-first instr-root-code))
		(if (zerop level)
		    (list
		     (append instr-root-code
			    `((if (not --magiciel-test-result)
				  ;; Don't do subtests if this is false.
				  (throw '%%exit-level%%
					 ;; The values of the `throw' may be
					 ;; wrong. Or useless.
					 (cl-values --magiciel-msg
						    %pos%
						    %end-pos%
						    ))
				(setq magiciel--level-0-result t))
			      ,@sub-tests-code)))
		  (list (append instr-root-code sub-tests-code))
		  ))
	       )			; case
             )
	   )				; emit-tree

	 (emit-group-tree (instr-node)
	   
	   ;; The `instr-node' is at level 0.
	   ;; If we get a non-nil return code, we exit the top-level
	   ;; function.
	   
	   `(let ((magiciel--level-0-result nil))
	      (cl-block ,magic-group-block-name
		(magiciel--msg 1 "entering test %s." ',magic-group-block-name)
	      
		;; (cl-tagbody
		;;  ,@(cl-subst magic-group-exit-label
		;; 		   '%%exit-label%%
		;; 		   (emit-tree instr-node))
		;;  ,magic-group-exit-label
		;;  ,exit-code
		;;  ,magic-group-continue-label
		;;  )

		(catch '%%exit-level%%
		  ,@(emit-tree instr-node 0))
		(when ;; --magiciel-test-result
		    magiciel--level-0-result
		  (cl-return-from
		      ,magic-fname
		    ;; ,magic-group-block-name
		    ;; (cl-values --magiciel-msg --magiciel-disp)
		    (cl-values --magiciel-msg %pos% %end-pos%)
		    ))
		)))			; emit-group-tree
	 )				; labels
      
      (magiciel--make-group-func-code magic-group-fname
                                      (emit-group-tree instrs-tree))
      )))


(cl-defun magiciel--group-is-named (ig)
  "Check that the (first) group of IG is a \\='name\\=' one."
  (cl-declare (type magiciel--group ig))
  
  (let ((i1 (cl-first (magiciel--group-items ig))))
    (eq 'name (magiciel--instruction-kind i1))))


(cl-defun magiciel--group-name (ig)
  "Get the \\='name\\=' of an instruction group IG."
  (cl-declare (type magiciel--group ig))

  (let ((i1 (cl-first (magiciel--group-items ig))))
    (when (eq 'name (magiciel--instruction-kind i1))
      (cl-second (magiciel--instruction-test i1)))))


;; magiciel--magic-emit
;;
;; Notes:
;;
;; 2024-11-05:
;; Changing the overall architecture using proper continuations.
;; The main function will be something like the following (just like
;; in the old version.

;; (defun magiciel--<magic-file-name>-fn (%bytes% %pos% %context%)
;;   ...
;;   )

;; Each test is a function of the form

;; (defun magiciel--<magic-file-name>-test%<d>
;;     (ckeck-state cont)
;;   ...)

;; where `cont' is the continuation, which takes a
;; `magiciel-check-state' (I hope this will work); that is, at a
;; minumum, a
;;
;; (lambda (check-state)
;;   (if (not (magiciel--check-state-result check-state))
;;       check-state
;;     (funcall cont
;; 	     (magiciel--<magic-file-name>-test%<d+1>
;; 	      %bytes%
;; 	      %context%
;; 	      (magiciel--check-state-start check-state)
;; 	      (magiciel--check-state-end check-state)
;; 	      (MUNGE (magiciel--check-state-msg
;; 		      check-state))
;; 	      (1+ (magiciel--check-state-level check-state))
;; 	      ))))
;;
;; 2024-11-06:
;; Deour.  Try using dynamic variables instead of re-doing the whole
;; thing.



;; magiciel--magic-emit
;; Refactored to expose the "name" entries and now to deal with
;; '!:strength' records.
;;
;; Several auxiliary functions are defined before
;; `magiciel--magic-emit'; they could go into `flet's, but it is
;; better to factor them out.

(cl-defun magiciel--cons-groups-env (name instr-groups)
  "Create names for groups in INSTR-GROUPS.

NAME is the name of the main entry.
Return an a-list of names and groups."
  (cl-loop for ig in instr-groups
	   for ign-name = (magiciel--group-name ig)
	   when ign-name
	     collect (cons ign-name
			   (intern (format "%s-name-%s"
					   name
					   ign-name)))))


(defun magiciel--emit-groups-code (magic instr-groups groups-names-env)
  "Collect the code for each group of instructions in INSTR-GROUPS.

Arguments and Values:

MAGIC : a `magiciel--magic' structure.
INSTR-GROUPS : a set (a list) of `magiciel--group's.
GROUPS-NAMES-ENV : an alist of \"name\"d code names."

  ;; First assess the "strength" of each instruction group in
  ;; INSTR-GROUPS.
  
  ;; (magiciel--assess-groups-strength instr-groups)
   
  (cl-loop with name = (magiciel--magic-name magic)
	   for ig in instr-groups
	   for ign from 0
	   for ign-name = (magiciel--group-name ig)
	   if ign-name
	     collect (list
		      (intern (format "%s-name-%s" name ign-name))
		      (magiciel--group-emit magic ig ign groups-names-env))
	     into named-groups
	   else
	     collect (magiciel--group-emit magic ig ign groups-names-env)
	     into fns-groups
	   end
	   finally (cl-return (cl-values fns-groups named-groups))))


(cl-defun magiciel--magic-emit (magic instr-groups)
  "Emit the functions that check for a specific \"magic(5)\" specification.

Arguments and Values:

MAGIC : a `magiciel--magic' structure.
INSTR-GROUPS : a set (a list) of `magiciel--group's."

  (magiciel--msg 1 "MME:")
  (magiciel--msg 1 "MME: emitting code for '%s' with %d groups."
		 (magiciel--magic-name magic)
		 (length instr-groups))
  (magiciel--msg 1 "MME:")

  (cl-check-type magic magiciel--magic)
  ;; (cl-assert (cl-every #'magiciel--records-group-p instr-groups))
  (cl-assert (cl-every #'magiciel--instructions-group-p instr-groups))
		 
  (let* ((max-lisp-eval-depth 4800)
	
	 ;; The extension to `max-lisp-eval-depth' is necessary
	 ;; because of the sizes of certain groups and because the
	 ;; code is not particularly smart with tail recursion.
	 ;;
	 ;; The number 4800 has been empirically determined.
	 ;; It works on a 2020 Intel Mac running Sonoma 14.5.
	 ;; YMMV, etc. etc.

	 (name (magiciel--magic-name magic))
	 (magic-fname (magiciel--magic-fname magic))

	 (groups-names-env (magiciel--cons-groups-env name instr-groups))
	 
	 (group-fns ())
	 (groups-named ())

	 )

    ;; First, collect "name" instructions for "use" ones.
    ;; (I should build up this environment while building the
    ;; instructions, but the double pass shouldn't matter so much.)
    
    ;; (cl-loop for ig in instr-groups
    ;; 	     for ign-name = (magiciel--group-name ig)
    ;; 	     when ign-name
    ;; 	       collect (cons ign-name
    ;; 			     (intern (format "%s-name-%s" name ign-name)))
    ;; 	       into igs-env
    ;; 	     end
    ;; 	     finally (setf groups-names-env igs-env))

    ;; Some code duplication below (see `intern' -- I am to lazy to
    ;; refactor right now).

    (cl-multiple-value-setq (group-fns groups-named)
      (magiciel--emit-groups-code magic instr-groups groups-names-env))
    
    ;; (cl-loop for ig in instr-groups
    ;; 	     for ign from 0
    ;; 	     for ign-name = (magiciel--group-name ig)
    ;; 	     if ign-name
    ;; 	       collect (list
    ;; 			(intern (format "%s-name-%s" name ign-name))
    ;; 			(magiciel--group-emit magic ig ign groups-names-env))
    ;; 	       into named-groups
    ;; 	     else
    ;; 	       collect (magiciel--group-emit magic ig ign groups-names-env)
    ;; 	       into fns-groups
    ;; 	     end
    ;; 	     finally (setf group-fns fns-groups
    ;; 			   groups-named named-groups))
    

    ;; Now we have all the pieces in place to build the code.
    
    (let ((magic-fun (cl-constantly "MAGICIEL: unknown format."))
          (magic-fun-body (cl-constantly nil))
	  (magic-external-name-funs ())
          )

      (magiciel--msg 0 "there are %d 'group-fns' and %d 'groups-named'."
		     (length group-fns)
		     (length groups-named))
      (magiciel--msg 0 "'magic-fun' is %s." magic-fun)
      
      ;; Here we build the function for the magic entry.

      (when group-fns
	;; If there are no groups we don't need to do anything.
	
	(when groups-named

	  ;; Create functions for each named group.
	  
	  (setq magic-external-name-funs
		(cl-loop
		 for (named-name code) in groups-named
		 for nf from 0
		 do (magiciel--msg 0 "collecting named fun %d.\n" nf)
		 collect
		 `(cl-defun ,named-name
		      ;; (&rest $$$args-here%%%)
		      (%bytes%		; The file/stream/buffer data.
		       %pos%  ; Where we start scanning the %%bytes%%.
		       %end-pos%      ; Where the previous test ended.
		       %context%      ; What we are looking for
		       &optional
		       (%invert-endianness% magiciel--*invert-endianness*)
		       ;; Do we invert endianness?
		       )
		    (ignore %bytes%
			    %pos%
			    %end-pos%
			    %context%
			    )

		    ;; Ensure that the following variables are *dynamic*
		    ;; (I should also change the naming convention to the
		    ;; usual earmuff one)

		    (defvar magiciel--*invert-endianness*)
		    (defvar --magiciel-value)
		    (defvar --magiciel-test-result)
		    (defvar --magiciel-msg)
		    (defvar --magiciel-disp)
		    (defvar %end-pos%)

		    (magiciel--$dump-state ',named-name)
		    
		    (cl-multiple-value-setq (--magiciel-msg
					     %pos% ; This may not work.
					     %end-pos%
					     )
		      ;; Probably pleonastic, but...
		      ;; (let ((magiciel--*invert-endianness*
		      ;; 	     %invert-endianness%))
		      ;; 	(defvar magiciel--*invert-endianness*)
		      ;; 	(magiciel--msg 1 "in %s invert arg %s; invert param %s."
		      ;; 		       ',named-name
		      ;; 		       %invert-endianness%
		      ;; 		       magiciel--*invert-endianness*)
		      ;; 	,(magiciel--group-func-code-body code))

		      (progn
			(magiciel--msg 1 "in %s invert arg %s; invert param %s."
				       ',named-name
				       %invert-endianness%
				       magiciel--*invert-endianness*)
			,(magiciel--group-func-code-body code))
		      )
		    (magiciel--$dump-state ',named-name)
		    (cl-values --magiciel-msg
			       %pos%	; This may not work.
			       %end-pos%
			       )
		    ))
		)
	  )
	
	;; Create the main function body...

	(magiciel--msg 0 "creating main function '%s' body.\n" magic-fname)
	
	(setq magic-fun-body
	      `(cl-block ,magic-fname
		 ,@(mapcar #'magiciel--group-func-code-body group-fns)
                 )
	      )

	(setq magic-fun
              `(lambda (%bytes%   ; The file/stream/buffer data.
			%pos%     ; Where we start scanning the %%bytes%%.
			%context% ; `%context' can be ':normal' (or `!:normal'),
                                  ; '!:mime', '!:ext', or '!:apple'.
				  ; '!:strength' unhandled.
			)
		 ;; Ensure that the following variables are *dynamic*
		 ;; (I should also change the naming convention to the
		 ;; usual earmuff one)

		 (defvar magiciel--*invert-endianness*)
		 (defvar --magiciel-value)
		 (defvar --magiciel-test-result)
		 (defvar --magiciel-msg)
		 (defvar --magiciel-disp)
		 (defvar %end-pos%)
		 
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

		   (magiciel--msg magiciel--*debug-mid-level*
				  "trying test '%s'..."
				  ',name
				  )

		   (magiciel--$dump-state ',magic-fname)
		   
                   ,magic-fun-body
		   ;; (cl-values --magiciel-msg %pos% %end-pos%)

		   (magiciel--$dump-state ',magic-fname)
		   --magiciel-msg
		   )
		 ))
	)				; when group-fns

      (magiciel--eval-compile-fns magic-external-name-funs)
      
      (setf (symbol-function magic-fname) magic-fun)

      (magiciel--msg 0 "function '%s' is now %s."
		     magic-fname
		     magic-fun)

      (byte-compile magic-fname)
      
      (magiciel--add-spell name magic-fname)
      
      (cl-values magic-fname
		 group-fns
		 groups-named
		 magic-fun
		 magic-external-name-funs
		 )
      )))


;; magiciel--eval-compile-fns
;; Evals and possibly compiles a list of `defun's.
;; Used internally to compile "use" functions.

(cl-defun magiciel--eval-compile-fns (funs
				      &optional
				      (do-byte-compile t))
  "Eval and (byte) compile FUNS online.

Byte compilation is done if DO-BYTE-COMPILE is true."
  
  (cl-loop for f in funs
	   for (nil name) = f

	   do (eval f t)
	   when do-byte-compile do (byte-compile name)))


;; Epilogue.

(provide 'magiciel-compile-magic)

;;; compile-magic.el ends here
