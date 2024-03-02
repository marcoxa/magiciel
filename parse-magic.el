;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; magiciel --- Guessing the content of a file in simple as possible way.

;;; parse-magic.el
;;
;; A nice rabbit hole I went in.  And rabbit holes are magic.

;;; Commentary:
;;
;; This is a parser for "magic(5)" entries.
;; Obvously the file format was ... accrued over the years by people
;; who liked the (bad) assembly feel of the format, cooked up in a
;; smelly cauldron, when computer file formats were few and far apart.
;;
;; Because I am an idiot, I want to drink the original recipe, study it
;; and turn it into nouvelle cuisine.
;;
;; This file contains code to "parse" the entries and turn them into
;; something... else.  Of course, the man page for "magic(5)" has a -
;; quote - grammar - unquote - which leaves much to the imagination. 
;;
;; Bear with me.

;;; Code:

(require 'cl-lib)
(require 'magic-ingredients)
;; (require 'platel)


;; magiciel--common-type-spec

(cl-defstruct (magiciel--common-type-spec
	       (:constructor
		magiciel--%make-common-type-spec (type-spec-string
						 spec
						 &optional
						 (byte-len 1)
						 (endian :le)
						 unsigned
						 )))
  "The representation of \"types\" in a \"magic(5)\" record."
  
  (type-spec-string "" :type string)
  (unsigned nil :type boolean)
  (spec nil :type symbol)
  (endian :le)				; One of :le, :be, :me or :m (go
					; figure).  :le should work
					; for most platforms; but do check.
  (byte-len 1 :type integer)		; Number of bytes.
  
  )


;; magiciel--record-type-spec

(cl-defstruct (magiciel--record-type-spec
	       (:include magiciel--common-type-spec)
	       (:constructor
		magiciel--%make-record-type-spec (type-spec-string
						 spec
						 &optional
						 modifiers
						 (byte-len 1)
						 (endian :le)
						 unsigned
						 )))
  "The representation of the type field in a \"magic(5)\" record."
  
  (modifiers ())
  )


(cl-defun magiciel--make-record-type-spec (type-spec-string
					   spec
					   &optional
					   modifiers
					   (byte-len 1)
					   (endian :le)
					   unsigned
					   )
  (magiciel--%make-record-type-spec type-spec-string
				    (intern spec)
				    modifiers
				    byte-len
				    endian
				    unsigned))


(cl-defmethod cl-print-object ((rts magiciel--record-type-spec) stream)
  (princ (format "#<MAGICIEL record type %s %S %S %S %S>"
		 (if (magiciel--record-type-spec-unsigned rts)
		     'unsigned
		   'signed)
		 (magiciel--record-type-spec-spec rts)
		 (magiciel--record-type-spec-modifiers rts)
		 (magiciel--record-type-spec-endian rts)
		 (magiciel--record-type-spec-byte-len rts)
		 )
	 stream))



(cl-defstruct (magiciel--offset-type-spec
	       (:include magiciel--common-type-spec)
	       (:constructor
		magiciel--%make-offset-type-spec (type-spec-string
						  spec
						  &optional
						  (byte-len 1)
						  (endian :le)
						  unsigned
						  )))
  "The representation of the type field in a \"magic(5)\" record."

  )


(cl-defun magiciel--make-offset-type-spec (type-spec-string
					   spec
					   &optional
					   (byte-len 1)
					   (endian :le)
					   unsigned
					   )
  (magiciel--%make-offset-type-spec type-spec-string
				    (cl-etypecase spec
				      (string (intern spec))
				      (symbol spec))
				    byte-len
				    endian
				    unsigned))


(cl-defmethod cl-print-object ((rts magiciel--offset-type-spec) stream)
  (princ (format "#<MAGICIEL offset type %s %S %S %S>"
		 (if (magiciel--offset-type-spec-unsigned rts)
		     'unsigned
		   'signed)
		 (magiciel--offset-type-spec-spec rts)
		 (magiciel--offset-type-spec-endian rts)
		 (magiciel--offset-type-spec-byte-len rts)
		 )
	 stream))


;; Type spec checkers.

(cl-defun magiciel--numeric-type-spec-p (spec)
  (when (magiciel--record-type-spec-p spec)
    (setf spec (magiciel--record-type-spec-spec spec)))

  (cl-typep spec 'magiciel--numeric-type-spec))


(cl-defun magiciel--date-type-spec-p (spec)
  (when (magiciel--record-type-spec-p spec)
    (setf spec (magiciel--record-type-spec-spec spec)))

  (cl-typep spec 'magiciel--date-type-spec))


(cl-defun magiciel--string-type-spec-p (spec)
  (when (magiciel--record-type-spec-p spec)
    (setf spec (magiciel--record-type-spec-spec spec)))

  (cl-typep spec 'magiciel--string-type-spec))


(cl-defun magiciel--exec-type-spec-p (spec)
  (when (magiciel--record-type-spec-p spec)
    (setf spec (magiciel--record-type-spec-spec spec)))

  (cl-typep spec 'magiciel--exec-type-spec))


;; Magiciel records.
;;
;; Notes:
;; I but the bullet and I subtype them.

(cl-defstruct (magiciel--magic-record (:constructor nil))
  "The representation of a line in a \"magic(5)\" UN*X file.

This is the top struct and it is \"abstract\"."

  (line "" :type string)
  )


(cl-defstruct (magiciel--empty-record
	       (:include magiciel--magic-record)
	       (:constructor magiciel--make-empty-record ()))
  "The representation of an empty line in a \"magic(5)\" UN*X file."
  )


(cl-defstruct (magiciel--normal-record
	       (:include magiciel--magic-record)
	       (:constructor magiciel--make-normal-record))
  "The representation of a normal record in a \"magic(5)\" UN*X file."
  offset
  type
  test
  (message "" :type string)
  )


(cl-defstruct (magiciel--special-record
	       (:include magiciel--magic-record)
	       (:constructor magiciel--make-special-record))
  "The representation of a special record in a \"magic(5)\" UN*X file.

Recognized special records are \"mime\" and \"ext\": other special
ones are read in but untreated."
  type
  value
  )


(cl-defstruct (magiciel--comment-record
	       (:include magiciel--magic-record)
	       (:constructor magiciel--make-comment-record (line)))
  "The representation of a comment record in a \"magic(5)\" UN*X file."
  )


;; Magiciel records `cl-print-object' methods.

(cl-defmethod cl-print-object ((ml magiciel--empty-record) stream)
  (ignore ml)
  (princ "#<MAGIC-RECORD empty>" stream))


(cl-defmethod cl-print-object ((ml magiciel--normal-record) stream)
  (let ((line (magiciel--magic-record-line ml)))
    (princ (format "#<MAGIC-RECORD Normal %d %S>"
		   (length line)
		   line)
	   stream)))


(cl-defmethod cl-print-object ((ml magiciel--special-record) stream)
  (let ((line (magiciel--magic-record-line ml)))
    (princ (format "#<MAGIC-RECORD Special %d %S>"
		   (length line)
		   line)
	   stream)))


(cl-defmethod cl-print-object ((ml magiciel--comment-record) stream)
  (let ((line (magiciel--magic-record-line ml)))
    (if (and line (not (string-equal line "")))
	(princ (format "#<MAGIC-RECORD Comment: %s>" line)
	       stream)
      (princ (format "#<MAGIC-RECORD Comment>") stream))))
    

;; Magiciel records predicates.

(defun magiciel--magic-record-is-comment (r)
  (magiciel--comment-record-p r))


(defun magiciel--magic-record-is-empty (r)
  (magiciel--empty-record-p r))


(defun magiciel--magic-record-is-special (r)
  (magiciel--special-record-p r))


(defun magiciel--magic-record-is-normal (r)
  (magiciel--normal-record-p r))


;; magiciel--magic

(cl-defstruct (magiciel--magic
	       (:constructor magiciel--make-magic))
  "The representation of a \"magic(5)\" UN*X file.

Notes:

The order of the non comment records (lines) is meaningful; do not
reorder the \"records\" field."

  (name nil :type (or null string))
  (file nil :type (or null string))
  (records nil :type list)
  )


(cl-defmethod cl-print-object ((mm magiciel--magic) stream)
  (princ (format "#<MAGICIEL-MAGIC spec file %S (%S) with %d records>"
		 (magiciel--magic-name mm)
		 (magiciel--magic-file mm)
		 (cl-count-if #'(lambda (r)
				  (or (magiciel--magic-record-is-normal r)
				      (magiciel--magic-record-is-special r)))
			      (magiciel--magic-records mm)))
	 stream))


;; Regexp used to extract the four parts of a magic record.
;; I need to `rx'ify it.

;; (defvar magiciel--*normal-record-regexp*
;;   (concat "^\\([-+*^<>&)(/.,[:alnum:]]+\\)"
;; 	  "[[:space:]]+\\([/[:alnum:]]+\\)"
;; 	  "[[:space:]]+\\([<>/[:graph:][:cntrl:]]+\\)"
;; 	  "\\(?:[[:space:]]+\\(.*\\)\\)?$")
;;   )

(defvar magiciel--*normal-record-regexp*
  (concat "^\\([-+*^<>&)(/.,[:alnum:]]+\\)"
	  "[[:space:]]+\\([/[:alnum:]]+\\)"
	  "[[:space:]]+\\([^[:space:]]+\\)"
	  "\\(?:[[:space:]]+\\(.*\\)\\)?$")
  )


(defvar magiciel--*normal-record-rx*
  (rx bol
      (group (+ (in "-+*/^<>&(),." alnum))) (+ (in blank))
      (group (+ (in "/" alnum))) (+ (in blank))
      ;; (group (+ (in graph cntrl)))
      (group (+ (not space)))
      (opt (+ (in space)) (group (* nonl)))
      eol))


(defun magiciel--parse-magic-record (line)
  (cl-flet
      ((parse-normal-record (rec)

	 (unless (string-match magiciel--*normal-record-regexp* rec)
	   (error (concat "MAGICIEL: could not parse record:\n"
			  "MAGICIEL: |%s|")
		  line))

	 ;; Get stuff out of match-data before it gets
	 ;; clobbered.
	 (let ((offset-m (match-string 1 rec))
	       (type-m   (match-string 2 rec))
	       (test-m   (match-string 3 rec))
	       (msg-m    (match-string 4 rec))
	       )
	   (magiciel--make-normal-record
	    :line    rec
	    :offset  (car (magiciel--parse-offset offset-m))
	    :type    (car (magiciel--parse-type-spec type-m))
	    :test    (car (magiciel--parse-test test-m))
	    :message (car (magiciel--parse-message (or msg-m "")))
	    )))

       (parse-special-record (rec)
	 (let ((srp (magiciel--parse-special-record rec)))
	   (magiciel--make-special-record 
	    :line  rec
	    :type  (cl-first srp)
	    :value (cl-rest srp)
	    )))

       (parse-comment-record (rec)
	 ;; Just for symmetry.
	 (magiciel--make-comment-record rec)
	 )
       )				; cl-flet
    (message "MAGICIEL: parsing line: %s." line)
    (if (string= "" line)
	(magiciel--make-empty-record)
      (cl-case (aref line 0)		; Check the first char.
	(?#				; Comment line
	 (parse-comment-record line))
	(?!				; Special line
	 (parse-special-record line))
	(t
	 (parse-normal-record line))
	))
    ))


;; magiciel--load-magic-file

(defun magiciel--load-magic-file (filename)
  (let ((fname (file-name-base filename)))
    (with-temp-buffer
      ;; I should ensure the buffer is unibyte.
      (cl-destructuring-bind (abs-filename nchars)
	  (insert-file-contents filename)
	(ignore nchars)
	(cl-loop while (not (eobp))
		 collect (magiciel--parse-magic-record
			  (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
		 into lines
		 do (forward-line 1)
		 finally (cl-return
			  (magiciel--make-magic
			   :name fname
			   :file abs-filename
			   :records lines))
		 )))
    ))

;; magiciel--load-magic-file-raw-bytes
;; Used mostly for debugging.

(defun magiciel--load-magic-file-raw-bytes (filename)
  ;; `filename' is a magic(5) file.

  (with-temp-buffer
    ;; I should ensure the buffer is unibyte.

    (insert-file-contents filename)
    (buffer-substring-no-properties (point-min) (point-max))))



;; Parsing the magic entries.

;; magiciel--lispify-number-string
;; This function essentially convers from C to Lisp syntax.

(cl-defun magiciel--lispify-number-string (ns &optional (start 0) end)
  "Assuming the string `ns' contains an integer, lispify it.

Lispification means turning \"OxFF\" into \"#xFF\", \"0o7\" into
\"#o7\" and \"0b01\" into \"#b01\"; no \"radix\" handling."
  
  (let ((xp (or (cl-position ?x ns :start start :end end)
		(cl-position ?X ns :start start :end end)))
	)
    ;; Hex
    (cond ((and xp (cl-plusp xp))
	   (setf (aref ns (1- xp)) ?#))
	  ((and xp (zerop xp))
	   (error "MAGICIEL: not a numberish string %S." ns))
	  (t
	   (let ((op (or (cl-position ?o ns :start start :end end)
			 (cl-position ?O ns :start start :end end)))
		 )
	     ;; Oct
	     (cond ((and op (cl-plusp op))
		    (setf (aref ns (1- op)) ?#))
		   ((and op (zerop op))
		    (error "MAGICIEL: not a numberish string %S." ns))
		   (t
		    (let ((bp (or (cl-position ?b ns :start start :end end)
				  (cl-position ?B ns :start start :end end)))
			  )
		      ;; Bin
		      (cond ((and bp (cl-plusp bp))
			     (setf (aref ns (1- bp)) ?#))
			    ((and bp (zerop bp))
			     (error "MAGICIEL: not a numberish string %S." ns))
			    (t
			     ;; (and (null xp) (null op) (null bp))
			     ns)	; Pointless return here.
			    )))
		   )))
	  )
    )
  ns)					; Real result.


;; magiciel--interpret-c-integer --

(defun magiciel--interpret-c-integer (i)
  "Takes a string `i' and reads an integer in C syntax from it.

The integer is supposed to be in position 0.

Cfr., the C standard(s) for integer syntax."
  
  ;; `i' is a string.
  ;; The numbers are in C syntax.
  (if (char-equal ?0 (aref i 0))	; Octal or Hex
      (if (and (> (length i) 1)
	       (let ((i1 (aref i 1)))
		 (or (char-equal i1 ?x)
		     (char-equal i1 ?X))))
	  ;; Hex
	  (car (read-from-string (concat "#x"
					 (substring i 2))))
	;; Octal (or 0; the "#o0" takes care of it.
	(car (read-from-string (concat "#o0"
				       (substring i 1))))
	)
    ;; Decimal
    (car (read-from-string i))
    ))


;; Magiciel offsets.




;; Number regexps.
;; Note that octal, decimal and hex numbers are in C format (no
;; suffixes).  The binary and radix form are an extra.

(defconst magiciel--*hex-regexp*
  (rx "0" (in "xX") (+ xdigit)))

;; (defconst magiciel--*oct-regexp*
;;  (rx "0" (in "oO") (? (in "-+")) (+ (in "0-7"))))

(defconst magiciel--*oct-regexp* ; Let's pray. Note that `0' is parsed
  (rx "0" (* (in "0-7"))))	 ; as octal.

(defconst magiciel--*bin-regexp*
  (rx "0" (in "bB") (+ (in "01"))))

(defconst magiciel--*dec-regexp*
  (rx (in "1-9") (* digit)))

(defconst magiciel--*radix-regexp*
  (rx "0"
      (group (regexp magiciel--*dec-regexp*))
      (in "Rr")
      (group (+ alnum))))


(defconst magiciel--*uint-regexp*
  (rx (or (regexp magiciel--*hex-regexp*)
	  (regexp magiciel--*oct-regexp*)
	  (regexp magiciel--*bin-regexp*)
	  (regexp magiciel--*dec-regexp*)
	  )))


;; Offset grammar, as inferred by the "magic(5)" man page.
;;
;;   offset = levels direct-offset
;;          | levels '&'? relative-offset
;;          | levels '&'? indirect-offset
;;
;;   levels = '>'*
;;
;;   direct-offset = number-offset
;;
;;   relative-offset = number-offset
;;
;;   number-offset = integer
;;
;;   indirect-offset = computed-offset
;;
;;   computed-offset = '(' direct-offset [sign type [expr]] ')'
;;                   | '(' relative-offset [sign type [expr]] ')'
;;
;;   sign = '.' | ','
;;
;;   type = '' | oneof(bBcCeEfFgGhHiIlmsSqQ)
;;
;;   expr = ''
;;        | op number
;;        | op '(' op number ')'
;;
;;   op = oneof(+-*/%&|^)
;;
;; What follows is a mix of recursive descent parsing and regexp kludging.
;; Doing it consistently one oway or the other may be better, but
;; bear with me.
;;
;; Old, first try, version:
;;
;; (defvar magiciel--*indirect-offset-regexp*
;;  "(\\(.+\\)\\(\\([.,][bBcCeEfFgGhHiIlmsSqQ]\\)*\\([-+]*.+\\)\\)?)")

(defconst magiciel--*indirect-offset-regexp*
  (rx "("
      (group (opt "&"))					 ; 1
      (group (regexp magiciel--*uint-regexp*))		 ; 2
      (? (group (in ".,") (in "bBcCeEfFgGhHiIlLmsSqQ"))) ; 3

      ;; Ok, the parentheses here may match wrongly, but the "grammar"
      ;; from "magic(5)" is rather, ahem, underspecified, and, in any
      ;; case I am parsing magic files that, hopefully, are correct. I
      ;; know I should go the full context-free expression parsing
      ;; here.
      ;;
      ;; The regexp "parses" the following.
      ;; ... and other crap as well.
      ;;
      ;;     (&0xe.l+(-4))
      ;;
      ;; Cfr., the "magic(5)" page.
      ;;
      ;; My "reverse angineering" is that some guy, at some point
      ;; said "well, we have been doing this with regexps, but it
      ;; will be nice to have a parenthesized thingie in here"; also,
      ;; "darn we did not think of this case before, let's hammer it
      ;; in"; you know what happenend next.  No, I *did not* look at the
      ;; "file" source code.  It's way above my pay grade.
      ;;
      ;; Luckily there is a BNF grammar in the main file(1)
      ;; repository; I am pretty much compliant with it.

      (? (group					   ; 4
	  (? (group (in "-+*/%&|^")))		   ; op   ; 5
	  (? (group "("))			   ; lpar ; 6
	  (? (group (in "-+")))			   ; sign ; 7
	  (group (regexp magiciel--*uint-regexp*)) ; offset expr ; 8
	  (? ")")				   ; rpar
	  ))
      ")")
  )


;; Offset types.

(cl-defstruct (magiciel--offset (:constructor nil))
  "The representation of a magic(5) \"offset\".

This is the \"abstract\" offset type."
  (string-rep "" :type string)
  (level 0 :type integer)
  cont
  )


(cl-defstruct (magiciel--direct-offset
	       (:include magiciel--offset)
	       (:constructor magiciel--make-direct-offset))
  "The representation of a \"direct offset\"."
  (value 0 :type integer)
  )


(cl-defstruct (magiciel--indirect-offset
	       (:include magiciel--offset)
	       (:constructor magiciel--make-indirect-offset)
	       )
  "The representation of an \"indirect offset\"."
  
  ;; The fields correspond to the regex groups in
  ;; `magiciel--*indirect-offset-regexp*'.
  
  rel					; 1
  addr					; 2
  type					; 3; this becomes a type.
  op					; 5; yes: 5!
  is-len				; 6
  sign					; 7
  num					; 8
  )


;; Offsets `cl-print-object' methods.

(cl-defmethod cl-print-object ((mo magiciel--offset) stream)
  (princ (format "#<MAGICIEL record offset %S>"
		 (magiciel--offset-string-rep mo))
	 stream))
      

;; magiciel--parse-offset

(cl-defun magiciel--parse-offset (record)
  
  (unless (or (stringp record)
	      (magiciel--magic-record-is-normal record))
    (cl-return-from magiciel--parse-offset nil))

  (let* ((offset-string
	  (copy-sequence       ; Make sure to have stuff to muck with.
	   (if (stringp record)	   ; Useful for testing and debugging.
	       record
	     (magiciel--normal-record-offset record))))
	 (olen (length offset-string))
	 (olen-1 (1- olen))
	 (index 0)
	 (level 0)
	 (matched-string "")
	 (cont nil)
	 (direct nil)
	 (relative nil)
	 (indirect nil)
	 )

    ;; Hand-crafted recursive descent parser!!!
    
    (cl-labels
	((start ()
	   (reset)			; Being paranoid.
	   (parse-offset)
	   )

	 (reset ()
	   (setf index 0))
		
	 (next ()
	   (cl-incf index))

	 (current ()
	   (if (<= 0 index olen-1)
	       (aref offset-string index)
	     ?\0
	     ))

	 (peek ()
	   (if (= index olen-1)
	       ?\0
	     (aref offset-string (1+ index))))

	 (previous ()
	   (cl-decf index))

	 (string-match-at (regx str &optional (idx 0))
	   ;; `string-match' searches the string; I want to be
	   ;; sure that the match is at `idx'.
	   (let ((match-at (string-match regx str idx)))
	     ;; idx can be past the length of the string.
	     (and match-at (= match-at idx))))

	 (parse-offset ()
	   (parse-levels)
	   (or (parse-direct-offset)
	       (and (parse-opt-cont)
		    (or (parse-relative-offset)
			(parse-indirect-offset)))
	       )
	   )

	 (parse-levels ()
	   (when (char-equal ?> (current))
	     (cl-incf level)
	     (next)
	     (parse-levels))
	   t)

	 (parse-opt-cont ()
	   (when (char-equal ?& (current))
	     (setq cont t)
	     (next))
	   t
	   )

	 (parse-direct-offset ()
	   (and (parse-number-offset)
		(setq direct t)))

	 (parse-number-offset ()
	   (when (string-match-at magiciel--*uint-regexp*
				  offset-string
				  index)
	     ;; I should check to have "consumed" the whole
	     ;; string.
	     (message "MAGICIEL: parse-number-offset: M1 %S."
		      (match-string 0 offset-string))
		    
	     (setf matched-string
		   (match-string 0 offset-string))
	     ))

	 (parse-relative-offset ()
	   (and (parse-number-offset)
		(setq relative t))
	   )

	 (parse-indirect-offset ()
	   (and (parse-computed-offset)
		(setq indirect t)))

	 (parse-computed-offset ()
	   (when (string-match-at magiciel--*indirect-offset-regexp*
				  offset-string
				  index)
	     (message "MAGICIEL: parse-compute-offset: M1 %S."
		      (match-string 0 offset-string))
	     ;; Munge match
	     (setf matched-string (match-string 0 offset-string))
	     ))

	 (parse-c-integer (i)
	   ;; `i' is a string.
	   ;; The numbers are in C syntax.
	   (if (char-equal ?0 (aref i 0)) ; Octal or Hex
	       (if (and (> (length i) 1)
			(let ((i1 (aref i 1)))
			  (or (char-equal i1 ?x)
			      (char-equal i1 ?X))))
		   ;; Hex
		   (car (read-from-string (concat "#x"
						  (substring i 2))))
		 ;; Octal (or 0; the "#o0" takes care of it.
		 (car (read-from-string (concat "#o0"
						(substring i 1))))
		 )
	     ;; Decimal
	     (car (read-from-string i))
	     ))
		
	 (finish ()
	   ;; Do stuff.
	   )


	 (select-addr-subtype (ind-type)
	   (cl-block select-addr-subtype-blk
	     ;; Necessary block. This seems a buglet in ELisp cl-lib.

	     (unless ind-type
	       (cl-return-from select-addr-subtype-blk
		 (magiciel--make-offset-type-spec "" 'long 4 :le t)))

	     ;; `ind-type' should be a string of length 2: first
	     ;; character either a \\='.\\=' or a \\=',\\='; second
	     ;; character a "type" one.

	     (cl-assert (= (length ind-type) 2))
	   
	     (let ((sign
		    (cond ((char-equal ?, (aref ind-type 0)) nil)
			  ((char-equal ?. (aref ind-type 0)) t)
			  (t
			   (error "MAGICIEL: incorrect sign '%c' in %S."
				  (aref ind-type 0)
				  ind-type))
			  ))
		   (subtype-char (aref ind-type 1))
		   )
	   
	       ;; The "subtype-char" comes from the indirect address
	       ;; specification.
	       ;; Value is a `magiciel--offset-type-spec'.

	       (cl-ecase subtype-char
		 ((?b ?c ?B ?C)
		  (magiciel--make-offset-type-spec ind-type 'byte))
		
		 ((?e ?f ?g)
		  ;; Let's be Common-istic.
		  (magiciel--make-offset-type-spec ind-type
						   'double-float
						   8
						   :le
						   sign))
		 ((?E ?F ?G)
		  (magiciel--make-offset-type-spec ind-type
						   'double-float
						   8
						   :be
						   sign))
		 (?l
		  (magiciel--make-offset-type-spec ind-type
						   'long
						   8
						   :le
						   sign))
		 (?L
		  (magiciel--make-offset-type-spec ind-type
						   'long
						   8
						   :be
						   sign))
		 ((?h ?s)
		  (magiciel--make-offset-type-spec ind-type
						   'short
						   2
						   :le
						   sign))
		 ((?H ?S)
		  (magiciel--make-offset-type-spec ind-type
						   'short
						   2
						   :be
						   sign))
		 (?i
		  (magiciel--make-offset-type-spec ind-type
						   'id3
						   4
						   :le
						   sign))
		 (?I
		  (magiciel--make-offset-type-spec ind-type
						   'id3
						   4
						   :be
						   sign))
		 (?m
		  (magiciel--make-offset-type-spec ind-type
						   'middle
						   4
						   :me
						   sign))
		 (?q
		  (magiciel--make-offset-type-spec ind-type
						   'quad
						   8
						   :le
						   sign))
		 (?Q
		  (magiciel--make-offset-type-spec ind-type
						   'quad
						   8
						   :be
						   sign))
    
		 ;; Other missing according to latest manual.
		 ))
	     ))
	 )
	 
      (start)
      (list ;; This cond should go in `finish' above
	    (cond (indirect
		   (let ((rel    (match-string 1 offset-string))
			 (addr   (match-string 2 offset-string))
			 (type   (match-string 3 offset-string))
			 (op     (match-string 5 offset-string)) ; yes: 5!
			 (is-len (match-string 6 offset-string)) ; lpar
			 (sign   (match-string 7 offset-string))
			 (num    (match-string 8 offset-string))
			 )
		     (magiciel--make-indirect-offset
		      :string-rep offset-string
		      :level  level
		      :cont   cont
		      :rel    (and rel (not (string= "" rel)))
		      :addr   (parse-c-integer addr)
		      :type   (select-addr-subtype type)
		      :sign   (if sign sign "")
		      :op     (if op op "")
		      :is-len (not (null is-len))
		      :num    (if num (parse-c-integer num) 0)))
		   )
		  
		  ((or direct relative)
		   (magiciel--make-direct-offset
		    :string-rep offset-string
		    :level level
		    :cont  cont
		    :value (parse-c-integer matched-string)
		    ))
		  
		  (t nil))
	    
	    offset-string index level matched-string
	    )
      )))


(cl-defun magiciel--parse-type-spec (record)
  (unless (or (stringp record)
	      (magiciel--magic-record-is-normal record))
    (cl-return-from magiciel--parse-offset nil))

  (let* ((type-spec-string
	  (copy-sequence       ; Make sure to have stuff to muck with.
	   (if (stringp record)	   ; Useful for testing and debugging.
	       record
	     (magiciel--magic-record-type record))))
	 (spec-list (split-string type-spec-string "/" t))
	 (spec (cl-first spec-list))
	 (unsigned
	  (when (and (not (string-equal "use" spec)) (char-equal ?u (aref spec 0)))
	    (setf spec (substring spec 1))
	    t))
	 (mods (cl-rest  spec-list))
	 (endian (magiciel--machine-endiannes)) ; CHECK THIS!
	 (spec-struct
	  (pcase spec
	    ;; See "magic(5)" man page for list.
	    ;; See also
	    ;; "https://www.commandlinux.com/man-page/man5/magic.5.html"
	    
	    ("byte"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      1
					      endian
					      unsigned
					      ))

	    ("id3"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      4
					      endian
					      unsigned
					      ))

	    ("short"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      2
					      endian
					      unsigned
					      ))

	    ("long"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      4
					      endian
					      unsigned
					      ))

	    ("quad"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      8
					      endian
					      unsigned
					      ))

	    ("float"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      4
					      endian
					      unsigned
					      ))

	    ("double"
	     (magiciel--make-record-type-spec type-spec-string
					      (intern spec)
					      mods
					      8
					      endian
					      unsigned
					      ))


	    ("string"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      0 ; Depends on the test value.
					      endian
					      unsigned
					      ))

	    ("pstring"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      0 ; Depends on the test value.
					      endian
					      unsigned
					      ))
	    ("date"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      4
					      endian
					      unsigned
					      ))

	    ("qdate"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      8
					      endian
					      unsigned
					      ))


	    ("ldate"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      4
					      endian
					      unsigned
					      ))

	    ("qldate"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      8
					      endian
					      unsigned
					      ))

	    ("qwdate"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      8
					      endian
					      unsigned
					      ))

	    ("beid3"
	     (magiciel--make-record-type-spec type-spec-string
					      "id3"
					      mods
					      4
					      :be
					      unsigned
					      ))

	    ("beshort"
	     (magiciel--make-record-type-spec type-spec-string
					      "short"
					      mods
					      2
					      :be
					      unsigned
					      ))

	    ("belong"
	     (magiciel--make-record-type-spec type-spec-string
					      "long"
					      mods
					      4
					      :be
					      unsigned
					      ))


	    ("bequad"
	     (magiciel--make-record-type-spec type-spec-string
					      "quad"
					      mods
					      8
					      :be
					      unsigned
					      ))

	    ("befloat"
	     (magiciel--make-record-type-spec type-spec-string
					      "float"
					      mods
					      4
					      :be
					      unsigned
					      ))

	    ("bedouble"
	     (magiciel--make-record-type-spec type-spec-string
					      "double"
					      mods
					      8
					      :be
					      unsigned
					      ))

	    ("bedate"
	     (magiciel--make-record-type-spec type-spec-string
					      "date"
					      mods
					      4
					      :be
					      unsigned
					      ))

	    ("beqdate"
	     (magiciel--make-record-type-spec type-spec-string
					      "qdate"
					      mods
					      8
					      :be
					      unsigned
					      ))


	    ("beldate"
	     (magiciel--make-record-type-spec type-spec-string
					      "ldate"
					      mods
					      4
					      :be
					      unsigned
					      ))

	    ("beqldate"
	     (magiciel--make-record-type-spec type-spec-string
					      "qldate"
					      mods
					      8
					      :be
					      unsigned
					      ))

	    ("beqwdate"
	     (magiciel--make-record-type-spec type-spec-string
					      "qwdate"
					      mods
					      8
					      :be
					      unsigned
					      ))

	    ("bestring16"
	     (magiciel--make-record-type-spec type-spec-string
					      "string16"
					      mods
					      0
					      :be
					      unsigned
					      ))

	    ("leid3"
	     (magiciel--make-record-type-spec type-spec-string
					      "id3"
					      mods
					      4
					      :le
					      unsigned
					      ))

	    ("leshort"
	     (magiciel--make-record-type-spec type-spec-string
					      "short"
					      mods
					      2
					      :le
					      unsigned
					      ))

	    ("lelong"
	     (magiciel--make-record-type-spec type-spec-string
					      "long"
					      mods
					      4
					      :le
					      unsigned
					      ))


	    ("lequad"
	     (magiciel--make-record-type-spec type-spec-string
					      "quad"
					      mods
					      8
					      :le
					      unsigned
					      ))

	    ("lefloat"
	     (magiciel--make-record-type-spec type-spec-string
					      "float"
					      mods
					      4
					      :le
					      unsigned
					      ))

	    ("ledouble"
	     (magiciel--make-record-type-spec type-spec-string
					      "double"
					      mods
					      8
					      :le
					      unsigned
					      ))

	    ("ledate"
	     (magiciel--make-record-type-spec type-spec-string
					      "date"
					      mods
					      4
					      :le
					      unsigned
					      ))

	    ("leqdate"
	     (magiciel--make-record-type-spec type-spec-string
					      "qdate"
					      mods
					      8
					      :le
					      unsigned
					      ))


	    ("leldate"
	     (magiciel--make-record-type-spec type-spec-string
					      "ldate"
					      mods
					      4
					      :le
					      unsigned
					      ))

	    ("leqldate"
	     (magiciel--make-record-type-spec type-spec-string
					      "qldate"
					      mods
					      8
					      :le
					      unsigned
					      ))

	    ("leqwdate"
	     (magiciel--make-record-type-spec type-spec-string
					      "qwdate"
					      mods
					      8
					      :le
					      unsigned
					      ))

	    ("lestring16"
	     (magiciel--make-record-type-spec type-spec-string
					      "string16"
					      mods
					      0
					      :le
					      unsigned
					      ))

	    ("melong"
	     (magiciel--make-record-type-spec type-spec-string
					      "long"
					      mods
					      4
					      :me
					      unsigned
					      ))

	    ("medate"
	     (magiciel--make-record-type-spec type-spec-string
					      "date"
					      mods
					      4
					      :me
					      unsigned
					      ))

	    ("meldate"
	     (magiciel--make-record-type-spec type-spec-string
					      "ldate"
					      mods
					      4
					      :me
					      unsigned
					      ))

	    ("indirect"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      ))

	    ("name"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      ))

	    ("use"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      ))

	    ("regex"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      ))

	    ("search"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      ))

	    ("default"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      ))

	    ("clear"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      ))

	    ("der"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      ))

	    ("guid"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      36
					      ))

	    ("offset"
	     (magiciel--make-record-type-spec type-spec-string
					      spec
					      mods
					      8
					      ))

	    (_
	     (error "MAGICIEL: unknown spec %s." spec))
		    
	    ))
	 )				; let* bindings.
    
    (list spec-struct type-spec-string spec spec-list)
    ))


;; Magiciel test values or operations.

(cl-defstruct (magiciel--test
	       (:constructor magiciel--make-test))
  (test-string "" :type string)
  (op nil :type symbol)
  (value nil)
  )


(cl-defmethod cl-print-object ((mt magiciel--test) stream)
  (princ (format "#<MAGICIEL Test: %S>"
		 (magiciel--test-test-string mt))
	 stream))


(cl-defun magiciel--parse-test (record)
  (unless (or (stringp record)
	      (magiciel--magic-record-is-normal record))
    (cl-return-from magiciel--parse-test nil))

  (cl-flet ((extract-op (test-string)
	      (cl-assert (cl-plusp (length test-string)))
	      (let ((possible-op (aref test-string 0)))
		(cl-case possible-op
		  ((?= ?< ?> ?& ?^ ?~ ?!) (intern (format "%c" possible-op)))
		  (t nil)
		  )
		))
	    )
    (let* ((test-spec-string
	    (copy-sequence     ; Make sure to have stuff to muck with.
	     (if (stringp record)  ; Useful for testing and debugging.
		 record
	       (magiciel--magic-record-test record))))
	   (op (extract-op test-spec-string))
	   )
      (list
       (magiciel--make-test :test-string test-spec-string
			    :op (if op op '=) ; = is the default.
			    :value (if op
				       (cl-subseq test-spec-string 1)
				     test-spec-string)))
      ))
  ) ; magiciel--parse-test


(cl-defstruct (magiciel--message
	       (:constructor magiciel--make-message))
  (msg-string "" :type string))


(cl-defmethod cl-print-object ((mm magiciel--message) stream)
  (princ (format "#<MAGICIEL Message: %s>"
		 (magiciel--message-msg-string mm))
	 stream))


(cl-defun magiciel--parse-message (record)
  (unless (or (stringp record)
	      (magiciel--magic-record-is-normal record))
    (cl-return-from magiciel--parse-message nil))

  (let* ((msg-spec-string
	  (copy-sequence     ; Make sure to have stuff to muck with.
	   (if (stringp record)  ; Useful for testing and debugging.
	       record
	     (magiciel--magic-record-message record))))
	 )

    ;; Ok, This function could be simpler.
    
    (list (magiciel--make-message :msg-string msg-spec-string))))
			     


(cl-defun magiciel--parse-special-record (record)
  (unless (or (stringp record)
	      (magiciel--magic-record-is-special record))
    (cl-return-from magiciel--parse-special-record nil))

  (let* ((line
	  (copy-sequence       ; Make sure to have stuff to muck with.
	   (if (stringp record)	   ; Useful for testing and debugging.
	       record
	     (magiciel--magic-record-line record))))
	 )
    (cl-flet ((string-prefix-p (s p)
		(let ((pos (string-search p s)))
		  (and pos
		       (zerop pos))))
	      )
      (if (or (string-prefix-p line "!:apple")
	      (string-prefix-p line "!:mime")
	      (string-prefix-p line "!:strength")
	      (string-prefix-p line "!:ext"))
	  (split-string line)
	(let ((sline (split-string line)))
	  (warn "MAGICIEL: unrecognized special record %S."
		(car sline))
	  sline))
      )))
  

;; Utilities

(cl-defun magiciel--machine-endiannes ()
  ;; This MUST be checked!
  :le)

  
(cl-defun magiciel--code-list (&optional (code "ff01fe100000"))
  (let ((bytes ()))
    (dotimes (i (/ (length code) 2))
      (let ((next-byte (substring code (* 2 i) (* 2 (1+ i)))))
	(push (cl-first (read-from-string (concat "#x" next-byte)))
              bytes)))
    (reverse bytes))
  )


(defun magiciel--parse-c-integer (i)
  "Parses the string `i` as an integer in C syntax.

No checks are made for correctness.  An integer is returned if all
goes well."
  
  (if (char-equal ?0 (aref i 0))	; Octal or Hex
      (if (and (> (length i) 1)
	       (let ((i1 (aref i 1)))
		 (or (char-equal i1 ?x)
		     (char-equal i1 ?X))))
	  ;; Hex
	  (car (read-from-string (concat "#x"
					 (substring i 2))))
	;; Octal (or 0; the "#o0" takes care of it.
	(car (read-from-string (concat "#o0"
				       (substring i 1))))
	)
    ;; Decimal
    (car (read-from-string i))
    ))


(provide 'magiciel-parse-magic)

;;; parse-magic.el end here.
