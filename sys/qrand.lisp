;;; -*- Package:SYSTEM-INTERNALS; Mode:LISP; Base:8 -*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;THIS FILE CONTAINS RANDOM FUNCTIONS WHICH MUST BE IN THE COLD LOAD
;;--NOTE--  This file cannot be read into the machine after it is running
;; because it defines INTERN as the bootstrap INTERN, which will cause lossage 
;; after the bootstrapping has been done and the package system installed.
;; This is a loss..  unfortunately, a conditional can not be used since this
;; file must be processible by the cold load generator.

;SPECIAL, UNSPECIAL, PUTPROP, AND REMPROP ARE HERE BECAUSE
;VARIOUS QFASL FILES IN THE COLD LOAD WILL CAUSE THESE TO
;BE CALLED AT INITIAL STARTUP.

(DECLARE (SPECIAL OBARRAY))  ;This is going away, but for now the old style
			     ;INTERN in here is still being used, so we need it declared.

(DEFUN SPECIAL (&REST &QUOTE L)
	(MAPC (FUNCTION (LAMBDA (X) (PUTPROP X T 'SPECIAL)))
	      L)
	T)

(DEFUN UNSPECIAL (&REST &QUOTE L)
       (MAPC (FUNCTION (LAMBDA (X) (REMPROP X 'SPECIAL)))
	     L)
       T)

;These are in microcode now
;This is like LIST except that "the last pair is dotted"
;(DEFUN LIST* (&REST ARGS)
;  (COND ((NULL ARGS)
;	 (CERROR T NIL ':WRONG-NUMBER-OF-ARGUMENTS "~S called with ~D arguments (too few)"
;		 'LIST* 0 NIL))
;	((NULL (CDR ARGS)) (CAR ARGS))
;	(T (LET ((LST (MAKE-LIST DEFAULT-CONS-AREA (LENGTH ARGS))))
;	     (DO ((ARGS ARGS (CDR ARGS))
;		  (CNS LST (CDR CNS)))
;		 ((NULL (CDDR CNS))
;		  (LET ((FOO (CDR CNS)))	;Final dotted pair
;		    (%P-STORE-CDR-CODE CNS CDR-NORMAL)
;		    (%P-STORE-CDR-CODE FOO CDR-ERROR))
;		  (RPLACA CNS (CAR ARGS))
;		  (RPLACD CNS (CADR ARGS))
;		  LST)
;	       (RPLACA CNS (CAR ARGS)))))))
;
;This is like LIST-IN-AREA except that "the last pair is dotted"
;(DEFUN LIST*-IN-AREA (AREA &REST ARGS)
;  (COND ((NULL ARGS)
;	 (CERROR T NIL ':WRONG-NUMBER-OF-ARGUMENTS "~S called with ~D arguments (too few)"
;		 'LIST*-IN-AREA 0 NIL))
;	((NULL (CDR ARGS)) (CAR ARGS))
;	(T (LET ((LST (MAKE-LIST AREA (LENGTH ARGS))))
;	     (DO ((ARGS ARGS (CDR ARGS))
;		  (CNS LST (CDR CNS)))
;		 ((NULL (CDDR CNS))
;		  (LET ((FOO (CDR CNS)))	;Final dotted pair
;		    (%P-STORE-CDR-CODE CNS CDR-NORMAL)
;		    (%P-STORE-CDR-CODE FOO CDR-ERROR))
;		  (RPLACA CNS (CAR ARGS))
;		  (RPLACD CNS (CADR ARGS))
;		  LST)
;	       (RPLACA CNS (CAR ARGS)))))))

;Note: this is done with an explicit SETQ so that it will happen
;inside the cold-load generator rather than as part of LISP-CRASH-LIST.
;It depends on the fact that the fasdumper can dump this SETQ form
;entirely in terms of fasl-ops rather than as a form to be evaluated.
(DEFVAR AREA-FOR-PROPERTY-LISTS)
(SETQ AREA-FOR-PROPERTY-LISTS PROPERTY-LIST-AREA)

(DEFUN PUTPROP (SYM PROP INDICATOR &AUX PLLOC)
  (SETQ PLLOC (COND ((SYMBOLP SYM)
		     (PROPERTY-CELL-LOCATION SYM))
		    ((OR (LISTP SYM) (LOCATIVEP SYM)) SYM)
		    (T (FERROR NIL "~S is not a symbol, a list, or a locative" SYM))))
  ;; If the indicator is already present, just change it to the new property value.
  ;; Otherwise cons a new one onto the front of the property list.
  (WITHOUT-INTERRUPTS
    (DO ((PL (CDR PLLOC) (CDDR PL)))
	((NULL PL)
	 (RPLACD PLLOC (LIST*-IN-AREA (IF (AND (SYMBOLP SYM)
					       (NOT (NULL (CDR (PACKAGE-CELL-LOCATION SYM)))))
					  AREA-FOR-PROPERTY-LISTS
					  DEFAULT-CONS-AREA)
				      INDICATOR PROP (CDR PLLOC))))
      (IF (EQ (CAR PL) INDICATOR)
	  (RETURN (SETF (CADR PL) PROP))))
    PROP))

(DEFUN DEFPROP (&QUOTE SYM PROP INDICATOR)
	(PUTPROP SYM PROP INDICATOR)
	SYM)

(DEFUN REMPROP (SYM INDICATOR)
  "Remove a property.  Returns NIL if not present, or a list whose CAR is the property."
  (LET ((PLLOC (COND ((SYMBOLP SYM) (PROPERTY-CELL-LOCATION SYM))
		     ((OR (LISTP SYM) (LOCATIVEP SYM)) SYM)
		     (T (FERROR NIL "~S is not a symbol or a list" SYM))))
	(INHIBIT-SCHEDULING-FLAG T)) ;atomic
    (DO ((PL (CDR PLLOC) (CDDR PL))
	 (PPL PLLOC (CDR PL)))
	((NULL PL) NIL)
      (COND ((EQ (CAR PL) INDICATOR)
	     (RPLACD PPL (CDDR PL))
	     (RETURN (CDR PL)))))))

(DEFUN LOCATIVEP (X)
    (= (%DATA-TYPE X) DTP-LOCATIVE))

(DEFUN LIST-SUM (X)
  (DO ((L X (CDR L))
       (ANS 0))
      ((NULL L) ANS)
    (SETQ ANS (+ ANS (ATOMEVAL (CAR L))))))

(DEFUN LIST-PRODUCT (X)
  (DO ((L X (CDR L))
       (ANS 1))
      ((NULL L) ANS)
    (SETQ ANS (* ANS (ATOMEVAL (CAR L))))))
       
(DEFUN ATOMEVAL (X)
	(COND ((NUMBERP X) X)
	      (T (SYMEVAL X))))

;--Q
(COMMENT ;The following are no longer used, they were for testing
(DEFUN PRINT-ALL-SYMBOLS NIL
  (PROG ((COUNT 0) (BUCKET 0) TAIL)
L	(COND ((NOT (< BUCKET SIZE-OF-OB-TBL)) (PRINT COUNT)
						(TERPRI)
						(RETURN T)))
	(SETQ TAIL (AR-1 OBARRAY BUCKET))
L1	(COND ((NULL TAIL) (TERPRI)
			   (SETQ BUCKET (1+ BUCKET))
			   (GO L)))
	(PRINT (CAR TAIL))
	(SETQ COUNT (1+ COUNT))
	(SETQ TAIL (CDR TAIL))
	(GO L1)))

(DEFUN CHECK-OBTBL NIL
  (PROG (BUCKET TEM TAIL HSH)
	(SETQ BUCKET 0)
L	(COND ((NOT (< BUCKET SIZE-OF-OB-TBL)) (RETURN T)))
	(SETQ TAIL (AR-1 OBARRAY BUCKET))
L1	(COND ((NULL TAIL) (SETQ BUCKET (1+ BUCKET))
			   (GO L)))
	(SETQ HSH (SXHASH (CAR TAIL)))
	(COND ((NOT (= BUCKET (\ HSH SIZE-OF-OB-TBL)))
		(PRIN1 (CAR TAIL))
		(PRINC " ")
		(PRIN1 HSH)
		(PRINC " ")
		(PRIN1 BUCKET)
		(TERPRI)))
	(SETQ TAIL (CDR TAIL))
	(GO L1)))
);end COMMENT

;This always returns a positive fixnum.  The order of COND clauses is carefully
;chosen to make the most common and important cases fast.  There is duplicated
;code in this function for the same reason.  Two things with the same printed
;representation will get the same sxhash, even in different system incarnations.
;Nothing is guaranteed about the relation between sxhash of a list and sxhash
;of subparts of that list; this is exploited to bum a little extra speed.
(DEFUN SXHASH (X)
  (COND ((SYMBOLP X) (SXHASH-STRING (GET-PNAME X)))
	((STRINGP X) (SXHASH-STRING X))
	((TYPEP X ':FIXNUM) (IF (MINUSP X) (LOGXOR X -37777777) X))	;-37777777 = 40000001
	((LISTP X)		;Rotate car by 11. and cdr by 7, but do it efficiently
	 (DO ((ROT 4) (HASH 0) Y)
	     ((ATOM X)
	      (OR (NULL X) (SETQ HASH (LOGXOR (ROT (SXHASH X) (- ROT 4)) HASH)))
	      (IF (MINUSP HASH) (LOGXOR HASH -37777777) HASH))	;-37777777 = 40000001
	   (SETQ Y (CAR X) X (CDR X))
	   (OR (< (SETQ ROT (+ ROT 7)) 24.) (SETQ ROT (- ROT 24.)))
	   (SETQ HASH (LOGXOR (ROT (COND ((SYMBOLP Y) (SXHASH-STRING (GET-PNAME Y)))
					 ((STRINGP Y) (SXHASH-STRING Y))
					 ((TYPEP Y ':FIXNUM) Y)
					 (T (SXHASH Y)))
				   ROT)
			      HASH))))
	((FIXP X) (LOGXOR (LDB 2701 X) (LDB 0027 X)))	;Bignum
	((TYPEP X ':SMALL-FLONUM)
	 (SETQ X (%POINTER X))
	 (IF (MINUSP X) (LOGXOR X -37777777) X))
	((FLOATP X) (LOGXOR (%P-LDB-OFFSET 0027 X 1)
			    (%P-LDB-OFFSET 2701 X 1)
			    (%P-LDB 0022 X)))
	(T 0)))					;0 for things that can't be read

;This ignores bit 5 of each character so that equal strings will have the same sxhash.
;Also ignores any high-order bits if the string is a 16-bit string.
(DEFUN SXHASH-STRING (STRING)
  (DO ((I 0 (1+ I))
       (N (ARRAY-ACTIVE-LENGTH STRING))
       (HASH 0))
      (( I N)
       (IF (MINUSP HASH)
	   (LOGXOR HASH -37777777)		;-37777777 = 40000001
	   HASH))
    (SETQ HASH (ROT (LOGXOR (LOGAND (AREF STRING I) 337) HASH) 7))))

(DEFUN GET-MACRO-ARG-DESC-POINTER (FEF-POINTER &AUX ORIGIN)
  (CHECK-ARG FEF-POINTER (= (%DATA-TYPE FEF-POINTER) DTP-FEF-POINTER) "a FEF pointer")
  (COND ((= 1 (%P-LDB-OFFSET %%FEFH-NO-ADL FEF-POINTER %FEFHI-IPC))
	 NIL)
	((= 0 (SETQ ORIGIN
		    (%P-LDB-OFFSET %%FEFHI-MS-ARG-DESC-ORG FEF-POINTER %FEFHI-MISC)))
	 NIL)
	(T (%MAKE-POINTER-OFFSET DTP-LIST FEF-POINTER ORIGIN))))

;;; This is the new version of MAKE-ARRAY.  It takes the old argument
;;; format as well as the new one, with heuristics to disambiguate whether
;;; a given call is old-style or new-style.
;;; Anyone caught using :OLD-LEADER-LENGTH-OR-LIST other than DEFSTRUCT
;;; will be summarily executed.
(DEFUN MAKE-ARRAY (DIMENSIONS &REST OPTIONS)
  (LET ((LENGTH-OF-OPTIONS (LENGTH OPTIONS))
	LEADER-LIST DISPLACED-TO DISPLACED-INDEX-OFFSET NAMED-STRUCTURE-SYMBOL
	(AREA NIL) (TYPE 'ART-Q)
	ARRAY N-DIMENSIONS INDEX-LENGTH LONG-ARRAY-P LEADER-QS DATA-LENGTH LEADER-LENGTH)
    ;; Figure out whether it is old-style.
    (COND ((AND ( LENGTH-OF-OPTIONS 2)
		(OR (NUMBERP (FIRST OPTIONS))
		    (MEMQ (FIRST OPTIONS) ARRAY-TYPES)))
	   ;; It is old-style.  The first arg is actually AREA.
	   (SETQ AREA DIMENSIONS)
	   (SETQ TYPE (FIRST OPTIONS))
	   (SETQ DIMENSIONS (SECOND OPTIONS))
	   (SETQ DISPLACED-TO (THIRD OPTIONS))
	   (SETQ LEADER-LIST (FOURTH OPTIONS))
	   (SETQ DISPLACED-INDEX-OFFSET (FIFTH OPTIONS))
	   (SETQ NAMED-STRUCTURE-SYMBOL (SIXTH OPTIONS))
	   (IF (NUMBERP LEADER-LIST)
	       (SETQ LEADER-LENGTH LEADER-LIST
		     LEADER-LIST NIL)
	       (SETQ LEADER-LIST (REVERSE LEADER-LIST))))
	  (T
	   ;; It is new-style.
	   (IF (NOT (EVENP LENGTH-OF-OPTIONS))
	       (FERROR NIL "Odd-length options list: ~S" OPTIONS))
	   (DO ((OPTIONS OPTIONS (CDDR OPTIONS)))
	       ((NULL OPTIONS))
	     (LET ((VALUE (SECOND OPTIONS)))
	       (SELECTQ (FIRST OPTIONS)
		 (:AREA (SETQ AREA VALUE))
		 (:TYPE (SETQ TYPE VALUE))
		 (:DISPLACED-TO (SETQ DISPLACED-TO VALUE))
		 (:DISPLACED-INDEX-OFFSET (SETQ DISPLACED-INDEX-OFFSET VALUE))
		 (:LEADER-LIST (SETQ LEADER-LIST VALUE))
		 (:LEADER-LENGTH (SETQ LEADER-LENGTH VALUE))
		 (:OLD-LEADER-LENGTH-OR-LIST (IF (NUMBERP VALUE)
						 (SETQ LEADER-LENGTH VALUE)
						 (SETQ LEADER-LIST (REVERSE VALUE))))
		 (:NAMED-STRUCTURE-SYMBOL (SETQ NAMED-STRUCTURE-SYMBOL VALUE))
		 (OTHERWISE
		  (FERROR NIL "~S is not a known MAKE-ARRAY keyword." (FIRST OPTIONS))))))))
    ;; Process the DIMENSIONS argument.
    (CHECK-ARG DIMENSIONS (OR (FIXP DIMENSIONS) (LISTP DIMENSIONS)) "a fixnum or a list")
    (COND ((FIXP DIMENSIONS)
	   (IF (MINUSP DIMENSIONS)
	       (FERROR NIL "The negative array length ~S is illegal." DIMENSIONS))
	   (SETQ N-DIMENSIONS 1
		 INDEX-LENGTH DIMENSIONS))
	  ((LISTP DIMENSIONS)
	   (DOLIST (DIM DIMENSIONS)
	     (IF (NOT (FIXP DIM))
		 (FERROR NIL "The dimension ~S is not a fixnum." DIM))
	     (IF (MINUSP DIM)
		 (FERROR NIL "The negative array dimension ~S is illegal." DIM)))
	   (SETQ N-DIMENSIONS (LENGTH DIMENSIONS))
	   (IF (> N-DIMENSIONS 7)
	       (FERROR NIL "Arrays may only have 7 dimensions, not ~S" N-DIMENSIONS))
	   (SETQ INDEX-LENGTH (LIST-PRODUCT DIMENSIONS))))
    ;; Process the DISPLACED-TO argument.
    (CHECK-ARG DISPLACED-TO
	       (OR (NULL DISPLACED-TO) (FIXP DISPLACED-TO) (ARRAYP DISPLACED-TO)
		   (LOCATIVEP DISPLACED-TO))
	       "NIL, a fixnum, a locative, or an array")
    ;; See whether this is a "short" or "long" format array.
    (IF (AND (> INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
	     (NOT DISPLACED-TO))
	(SETQ LONG-ARRAY-P T))
    (OR (= (%DATA-TYPE INDEX-LENGTH) DTP-FIX)
	(FERROR NIL "Attempt to make array too large; total length ~S" INDEX-LENGTH))
    ;; Process the LEADER and NAMED-STRUCTURE-SYMBOL arguments.
    (CHECK-ARG LEADER-LIST (OR (NULL LEADER-LIST) (LISTP LEADER-LIST))
	       "NIL, or a list")
    (AND (NULL LEADER-LENGTH) (NOT (NULL LEADER-LIST))
	 (SETQ LEADER-LENGTH (LENGTH LEADER-LIST)))
    (IF (AND LEADER-LENGTH (> (LENGTH LEADER-LIST) LEADER-LENGTH))
	(FERROR NIL "Length of leader initialization list is greater than leader length"))
    (COND (NAMED-STRUCTURE-SYMBOL
	   (COND (LEADER-LENGTH
		  (CHECK-ARG LEADER-LENGTH (> LEADER-LENGTH 1) "greater than 1"))
		 (T (OR (= N-DIMENSIONS 1)
			(FERROR NIL "A named-structure array may not be ~S-dimensional"
				    N-DIMENSIONS)))))
	  (LEADER-LENGTH
	   (CHECK-ARG LEADER-LENGTH (> LEADER-LENGTH 0) "greater than zero")))
    (SETQ LEADER-QS (IF LEADER-LENGTH
			(+ 2 LEADER-LENGTH)
			0))
    ;; Process the TYPE argument.
    (CHECK-ARG TYPE (OR (FIXP TYPE) (MEMQ TYPE ARRAY-TYPES)) "an array type")
    (SETQ TYPE (IF (FIXP TYPE)
		   (LDB %%ARRAY-TYPE-FIELD TYPE)
		   (FIND-POSITION-IN-LIST TYPE ARRAY-TYPES)))
    (LET ((ENTRIES-PER-Q (ARRAY-ELEMENTS-PER-Q TYPE)))
      ;; This is positive if there are 1 or more entries per Q.  It is
      ;; negative if there are more than one Qs per entry.
      (SETQ DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
			    (// (+ INDEX-LENGTH (1- ENTRIES-PER-Q))
				ENTRIES-PER-Q)
			    (* INDEX-LENGTH (MINUS ENTRIES-PER-Q)))))
    ;; Process the DISPLACED-INDEX-OFFSET argument.
    (CHECK-ARG DISPLACED-INDEX-OFFSET
	       (OR (NULL DISPLACED-INDEX-OFFSET)
		   (AND (= (%DATA-TYPE DISPLACED-INDEX-OFFSET) DTP-FIX)
			(NOT (MINUSP DISPLACED-INDEX-OFFSET))))
	       "NIL or a non-negative fixnum")
    (LET ((HEADER-WORD
	    ;; Put in array type and number of dims.
	    (%LOGDPB N-DIMENSIONS %%ARRAY-NUMBER-DIMENSIONS
		     (%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0))))
      ;; If there is a leader, set the flag.
      (IF LEADER-LENGTH
	  (SETQ HEADER-WORD (%LOGDPB 1 %%ARRAY-LEADER-BIT HEADER-WORD)))
      (SETQ HEADER-WORD
	    (COND (DISPLACED-TO
		   ;; Array is displaced; turn on the bit, and the array is 2 long
		   ;; plus one for the index-offset if any.
		   (+ (%LOGDPB 1 %%ARRAY-DISPLACED-BIT HEADER-WORD)
		      (IF DISPLACED-INDEX-OFFSET 3 2)))
		  (LONG-ARRAY-P
		   ;; It is local; if it is a long array, the length is not in the
		   ;; header at all; set the bit instead.
		   (%LOGDPB 1 %%ARRAY-LONG-LENGTH-FLAG HEADER-WORD))
		  (T
		   ;; It is a short array; the length is in the header.
		   (+ INDEX-LENGTH HEADER-WORD))))
      ;; Create the array.
      (SETQ ARRAY (%ALLOCATE-AND-INITIALIZE-ARRAY HEADER-WORD
						  INDEX-LENGTH
						  (OR LEADER-LENGTH 0)
						  AREA
						  (+ N-DIMENSIONS
						     LEADER-QS
						     (COND (DISPLACED-TO
							    (IF DISPLACED-INDEX-OFFSET 3 2))
							   (LONG-ARRAY-P
							    (1+ DATA-LENGTH))
							   (T DATA-LENGTH)))
						  )))
    (COND ((LISTP DIMENSIONS)
	   ;; It is a multi-dimensional array.  Fill in the "dope vector".
	   (DO ((I (IF LONG-ARRAY-P 2 1) (1+ I))
		(DIMLIST DIMENSIONS (CDR DIMLIST))
		(N N-DIMENSIONS (1- N)))
	       ((< N 2))
	     (%P-STORE-CONTENTS-OFFSET (CAR DIMLIST) ARRAY I))))
    (COND (DISPLACED-TO
	   ;; It is displaced.  Put information after the dope vector, and after
	   ;; the "long array" word if any.
	   (LET ((IDX (IF LONG-ARRAY-P (1+ N-DIMENSIONS) N-DIMENSIONS)))
	     (%P-STORE-CONTENTS-OFFSET DISPLACED-TO ARRAY IDX)
	     (%P-STORE-CONTENTS-OFFSET INDEX-LENGTH ARRAY (1+ IDX))
	     (COND (DISPLACED-INDEX-OFFSET
		    ;; Index offset feature is in use.
		    ;; Store the index offset in the next Q.
		    (%P-STORE-CONTENTS-OFFSET DISPLACED-INDEX-OFFSET ARRAY (+ IDX 2)))))))
    ;; The leader's initial values were specified.
    (DO ((I 0 (1+ I))
	 (LEADER-LIST LEADER-LIST (CDR LEADER-LIST)))
	((NULL LEADER-LIST))
      (STORE-ARRAY-LEADER (CAR LEADER-LIST) ARRAY I))
;;; Cretinism associated with make-array, in that the leader list can overlap
;;; with the name-structure slot, which is how fasd dumps the named-structure-symbol
;;; So we check for the symbol being t and not smash it in that case
    (COND (NAMED-STRUCTURE-SYMBOL
	   ;; It is a named structure.  Set the flag.
	   (%P-DPB-OFFSET 1 %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY 0)
	   (COND ((NULL LEADER-LENGTH)
		  ;; There is no leader; put it in element zero of the body.
		  (AS-1 NAMED-STRUCTURE-SYMBOL ARRAY 0))
		 (T
		  ;; There is a leader; use element one of the leader.
		  (IF (NEQ NAMED-STRUCTURE-SYMBOL T)
		      (STORE-ARRAY-LEADER NAMED-STRUCTURE-SYMBOL ARRAY 1))))))
    ;; If there is a fill pointer on an art-q-list array, then it should control
    ;; the length of the list as well.  See array-push and array-pop.
    (COND ((AND (= N-DIMENSIONS 1)
		(NOT (NULL LEADER-LIST))
		;; The cold load generator's frame builder isn't smart enough for a #, here.
		(= TYPE #|'#,|# (LDB %%ARRAY-TYPE-FIELD ART-Q-LIST)))
	   (LET ((FILL-POINTER (CAR LEADER-LIST)))
	     (COND ((AND (FIXP FILL-POINTER)
			 (> FILL-POINTER 0)
			 (< FILL-POINTER (ARRAY-LENGTH ARRAY)))
		    (%P-DPB CDR-NIL %%Q-CDR-CODE (AP-1 ARRAY (1- FILL-POINTER))))))))
    (VALUES ARRAY DATA-LENGTH)))

(DEFUN MAKE-LIST (LENGTH &REST OPTIONS)
  (LET ((LENGTH-OF-OPTIONS (LENGTH OPTIONS))
	(AREA NIL) (INITIAL-VALUE NIL))
    ;; Figure out whether it is old-style.
    (COND ((= LENGTH-OF-OPTIONS 1)
	   ;; It is old-style.
	   (SETQ AREA LENGTH
		 LENGTH (FIRST OPTIONS)))
	  (T
	   ;; It is new-style.
	   (IF (NOT (EVENP LENGTH-OF-OPTIONS))
	       (FERROR NIL "Odd-length options list: ~S" OPTIONS))
	   (DO ((OPTIONS OPTIONS (CDDR OPTIONS)))
	       ((NULL OPTIONS))
	     (LET ((VALUE (SECOND OPTIONS)))
	       (SELECTQ (FIRST OPTIONS)
		 (:AREA (SETQ AREA VALUE))
		 (:INITIAL-VALUE (SETQ INITIAL-VALUE VALUE))
		 (OTHERWISE
		  (FERROR NIL "~S is not a known keyword." (FIRST OPTIONS))))))))
    (%MAKE-LIST INITIAL-VALUE AREA LENGTH)))

;;; This is an internal function designed to be called by code generated
;;; be a compiler optimizer of simple calls to MAKE-ARRAY.
(DEFUN SIMPLE-MAKE-ARRAY (DIMENSIONS TYPE &OPTIONAL AREA LEADER-LENGTH
			  &AUX DATA-LENGTH LONG-ARRAY-P ARRAY)
  (COND ((LISTP DIMENSIONS)
	 (FUNCALL #'MAKE-ARRAY			;This had better not get optimized!
		  DIMENSIONS ':TYPE TYPE ':AREA AREA ':LEADER-LENGTH LEADER-LENGTH))
	(T
	 (CHECK-ARG DIMENSIONS FIXP "a fixnum or a list")
	 (IF (MINUSP DIMENSIONS)
	     (FERROR NIL "The negative array length ~S is illegal." DIMENSIONS))
	 (IF (> DIMENSIONS %ARRAY-MAX-SHORT-INDEX-LENGTH)
	     (SETQ LONG-ARRAY-P T))
	 (IF (NOT (NULL LEADER-LENGTH))
	     (CHECK-ARG LEADER-LENGTH (> LEADER-LENGTH 0) "greater than zero"))
	 (CHECK-ARG TYPE (OR (FIXP TYPE) (MEMQ TYPE ARRAY-TYPES)) "an array type")
	 (SETQ TYPE (IF (FIXP TYPE)
			(LDB %%ARRAY-TYPE-FIELD TYPE)
			(FIND-POSITION-IN-LIST TYPE ARRAY-TYPES)))
	 (LET ((ENTRIES-PER-Q (ARRAY-ELEMENTS-PER-Q TYPE)))
	   ;; This is positive if there are 1 or more entries per Q.  It is
	   ;; negative if there are more than one Qs per entry.
	   (SETQ DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
				 (// (+ DIMENSIONS (1- ENTRIES-PER-Q))
				     ENTRIES-PER-Q)
				 (* DIMENSIONS (MINUS ENTRIES-PER-Q)))))
	 (LET ((HEADER-WORD
		 ;; Put in array type and number of dims.
		 (%LOGDPB 1 %%ARRAY-NUMBER-DIMENSIONS
			  (%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0))))
	   ;; If there is a leader, set the flag.
	   (IF LEADER-LENGTH
	       (SETQ HEADER-WORD (%LOGDPB 1 %%ARRAY-LEADER-BIT HEADER-WORD)))
	   (SETQ HEADER-WORD
		 (COND (LONG-ARRAY-P
			;; It is local; if it is a long array, the length is not in the
			;; header at all; set the bit instead.
			(%LOGDPB 1 %%ARRAY-LONG-LENGTH-FLAG HEADER-WORD))
		       (T
			;; It is a short array; the length is in the header.
			(+ DIMENSIONS HEADER-WORD))))
	   ;; Create the array.
	   (SETQ ARRAY (%ALLOCATE-AND-INITIALIZE-ARRAY HEADER-WORD
						       DIMENSIONS
						       (OR LEADER-LENGTH 0)
						       AREA
						       (+ 1 ;header
							  (IF LEADER-LENGTH
							      (+ 2 LEADER-LENGTH)
							      0)
							  (COND (LONG-ARRAY-P
								 (1+ DATA-LENGTH))
								(T DATA-LENGTH)))
						       )))
	 (VALUES ARRAY DATA-LENGTH))))



(DEFUN MAKE-SYMBOL (PNAME &OPTIONAL PERMANENT-P)
  (CHECK-ARG PNAME STRINGP "a string")
  (AND PERMANENT-P (NOT (= (%AREA-NUMBER PNAME) P-N-STRING))
       (LET ((DEFAULT-CONS-AREA P-N-STRING))
	 (SETQ PNAME (STRING-APPEND PNAME))))
  (LET ((SYMB (%ALLOCATE-AND-INITIALIZE DTP-SYMBOL	;Type to return.
			     DTP-SYMBOL-HEADER	;Type of header.
			     PNAME		;Pointer field of header.
			     NIL		;Value for second word.
			     (AND PERMANENT-P NR-SYM)	;Area.
			     LENGTH-OF-ATOM-HEAD)))	;Length.
    (MAKUNBOUND SYMB)			;Was initialized to NIL
    (FMAKUNBOUND SYMB)
    SYMB))

;SYM MAY BE EITHER A FULL SYMBOL OR A STRING. IN EITHER CASE,
; A FULL SYMBOL IS RETURNED.
;RETURNS A SECOND VALUE WHICH IS
; T IF SYMBOL WAS ALREADY ON THE OBARRAY, NIL IF WE ADDED IT
(DEFUN INTERN-OLD (SYM &OPTIONAL (OBARRAY OBARRAY))
  (PROG (HSH BUCKET ISTRING BUCKETN (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
	(COND ((SYMBOLP SYM) (SETQ ISTRING (GET-PNAME SYM)))
	      ((STRINGP SYM) (SETQ ISTRING SYM))
	      (T (FERROR NIL "~S is not a symbol or string" SYM)))
	(SETQ HSH (SXHASH-STRING ISTRING))
	(SETQ BUCKET (AR-1 OBARRAY (SETQ BUCKETN (\ HSH SIZE-OF-OB-TBL))))
  L	(COND ((NULL BUCKET) (GO NEW))
	      ((STRING-EQUAL ISTRING (GET-PNAME (CAR BUCKET)))
	       (RETURN (CAR BUCKET) T)))
	(SETQ BUCKET (CDR BUCKET))
	(GO L)
  NEW	(COND ((STRINGP SYM)
	       (SETQ SYM (MAKE-SYMBOL ISTRING T))))
	(AS-1 (CONS-IN-AREA SYM (AR-1 OBARRAY BUCKETN) OBT-TAILS) OBARRAY BUCKETN) 
	(RETURN SYM NIL)
))

(DEFUN REMOB-OLD (SYM)
  (PROG (HSH BUCKET BUCKETN)
	(SETQ HSH (SXHASH-STRING (GET-PNAME SYM)))
	(SETQ BUCKET (AREF OBARRAY (SETQ BUCKETN (\ HSH SIZE-OF-OB-TBL))))
	(ASET (DELQ SYM BUCKET) OBARRAY BUCKETN)))

;Revised version which uses microcode assist as much as possible
(DEFUN STRING-EQUAL (STRING1 STRING2 &OPTIONAL (IDX1 0) (IDX2 0) LIM1 LIM2)
  (OR (STRINGP STRING1) (SETQ STRING1 (STRING STRING1)))
  (OR (STRINGP STRING2) (SETQ STRING2 (STRING STRING2)))
  (COND ((OR LIM1 LIM2) 
	 (OR LIM1 (SETQ LIM1 (ARRAY-ACTIVE-LENGTH STRING1)))
	 (OR LIM2 (SETQ LIM2 (ARRAY-ACTIVE-LENGTH STRING2)))
	 (AND (= (SETQ LIM1 (- LIM1 IDX1)) (- LIM2 IDX2))
	      (%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 LIM1)))
	(T (%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 NIL))))

;;; T means case matters in string comparisons, NIL means it is ignored.
;;; This is bound to T by certain routines, such as INTERN, but I do not
;;; recommend changing its global value to T rather than NIL; many system
;;; functions, or at least their user interfaces, assume that string
;;; comparison is case-insensitive.
(DEFVAR ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON)
;This is initialized by the microcode now
;(SETQ ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON NIL)	;Not in DEFVAR for cold-load.

;T if two characters are equal, ignoring font and alphabetic case
;This is in microcode now
;(DEFUN CHAR-EQUAL (CH1 CH2)
;  (OR (= (SETQ CH1 (LDB %%CH-CHAR CH1)) (SETQ CH2 (LDB %%CH-CHAR CH2)))
;      (AND (NOT ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON)
;	   (ZEROP (LOGAND 337 (LOGXOR CH1 CH2)))
;	   (OR (AND (>= CH1 101) (<= CH1 132))
;	       (AND (>= CH1 141) (<= CH1 172)))
;	   (OR (AND (>= CH2 101) (<= CH2 132))
;	       (AND (>= CH2 141) (<= CH2 172))))))

(DEFUN CHAR-LESSP (CH1 CH2)
       (SETQ CH1 (LDB %%CH-CHAR CH1))
       (SETQ CH2 (LDB %%CH-CHAR CH2))
       (COND ((NOT ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON)
	      (AND (>= CH1 141) (<= CH1 172) (SETQ CH1 (LOGXOR CH1 40)))
	      (AND (>= CH2 141) (<= CH2 172) (SETQ CH2 (LOGXOR CH2 40)))))
       (< CH1 CH2))

;; Runtime support for compiled functions that use &key.

;; Required keyword args are initialized to this value.
;; We compare the values against it to see which ones are missing.
(DEFCONST KEYWORD-GARBAGE (LIST NIL))

(DEFPROP STORE-KEYWORD-ARG-VALUES T :ERROR-REPORTER)

;Given ARGS, the list of key names and values;
;KEYKEYS, the list of keywords we understand, in their order;
;and FRAME-POINTER, a locative pointing at our caller's frame;
;decode the ARGS and stick the values into the right slots in the frame.
;The first NUMBER-OF-REQUIRED-KEYWORDS keywords are required.
;Report an error if any of them is missing.
;SPECVAR-LIST is a list of NIL for nonspecial keyword args
;and symbols for special ones.
;It runs in parallel with KEYKEYS.
(DEFUN STORE-KEYWORD-ARG-VALUES (FRAME-POINTER ARGS KEYKEYS ALLOW-OTHER-KEYS
					       &OPTIONAL NUMBER-OF-REQUIRED-KEYWORDS
					       SPECVAR-LIST)
  (LET ((FIRST-KEYWORD-ARG-INDEX
	  (%P-LDB-OFFSET %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN
			 FRAME-POINTER
			 %LP-ENTRY-STATE)))
    ;; First decode what was specified.
    (DO ((ARGS-LEFT ARGS (CDDR ARGS-LEFT)))
	((NULL ARGS-LEFT))
      (LET ((KEYWORD (CAR ARGS-LEFT)))
	(DO () (())
	  (LET ((INDEX (FIND-POSITION-IN-LIST KEYWORD KEYKEYS)))
	    (COND (INDEX
		   (LET ((SPECVAR (NTH INDEX SPECVAR-LIST)))
		     (IF SPECVAR
			 (SET SPECVAR (CADR ARGS-LEFT)))
		     (%P-STORE-CONTENTS-OFFSET (CADR ARGS-LEFT) FRAME-POINTER
					       (+ 1 INDEX FIRST-KEYWORD-ARG-INDEX)))
		   (RETURN))
		  ((NOT ALLOW-OTHER-KEYS)
		   (SETQ KEYWORD (CERROR T NIL ':UNDEFINED-ARG-KEYWORD
					 "Keyword arg keyword ~S unrecognized"
					 KEYWORD))
		   (OR KEYWORD (RETURN)))
		  (T (RETURN)))))))
    ;; Now complain about any required keywords that we didn't find.
    ;; Old compiled functions do not pass this argument;
    ;; refrain from checking here, since they will check for themselves.
    (AND NUMBER-OF-REQUIRED-KEYWORDS
	 (DOTIMES (INDEX NUMBER-OF-REQUIRED-KEYWORDS)
	   (AND (EQ (%P-CONTENTS-OFFSET FRAME-POINTER (+ 1 INDEX FIRST-KEYWORD-ARG-INDEX))
		    KEYWORD-GARBAGE)
		(LET ((NEWVAL (CERROR T NIL ':MISSING-KEYWORD-ARG
				      "The required keyword arg ~S is missing"
				      (NTH INDEX KEYKEYS)))
		      (SPECVAR (NTH INDEX SPECVAR-LIST)))
		  (%P-STORE-CONTENTS-OFFSET NEWVAL FRAME-POINTER
					    (+ 1 INDEX FIRST-KEYWORD-ARG-INDEX))
		  (IF SPECVAR (SET SPECVAR NEWVAL))))))))

;This is in microcode now
;(DEFUN NTH (N OBJECT)
;   (CHECK-ARG N (AND (FIXP N) (NOT (MINUSP N))) "a non-negative integer") 
;   (DO ((N N (1- N))
;	(OBJECT OBJECT (CDR OBJECT)))
;       ((ZEROP N) (CAR OBJECT))))


(DEFUN ARRAY-LEADER-LENGTH (ARRAY)
    (COND ((ARRAY-HAS-LEADER-P ARRAY)
	   (%P-CONTENTS-OFFSET ARRAY -1))))

;Return the number of Qs distance from the array's address
;to that of its first data element.
(DEFUN ARRAY-DATA-OFFSET (ARRAY)
  (+ (ARRAY-#-DIMS ARRAY)	;For the array header and all but last dimension.
     (IF (ARRAY-DISPLACED-P ARRAY)
	 (IF (ARRAY-INDEXED-P ARRAY) 3 2)
	 (IF (> (ARRAY-LENGTH ARRAY) %ARRAY-MAX-SHORT-INDEX-LENGTH)
	     1 0))))

(DEFUN ARRAY-DIMENSION-N (N ARRAY &AUX NDIMS INDEX-LENGTH LONG-ARRAY-P)
  (CHECK-ARG ARRAY ARRAYP "an array")
  (COND ((> N (SETQ NDIMS (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)))
	 NIL)
	(T
	 (SETQ LONG-ARRAY-P (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))
	 (COND ((<= N 0)
		(ARRAY-LEADER-LENGTH ARRAY))
	       ((< N NDIMS)
		(%P-LDB-OFFSET %%Q-POINTER ARRAY (+ N LONG-ARRAY-P)))
	       ((= N NDIMS)
		(SETQ INDEX-LENGTH
		      (COND ((NOT (ZEROP (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0)))
			     (%P-LDB-OFFSET %%Q-POINTER ARRAY (1+ NDIMS)))
			    ((= LONG-ARRAY-P 1) (%P-LDB-OFFSET %%Q-POINTER ARRAY 1))
			    (T (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0))))
		;; As far as I can tell, there's no way to determine the last dimension
		;; if the index-length is 0.  Might as well just give it as zero? -- DLA
		(OR (ZEROP INDEX-LENGTH)
		    (DO I 1 (1+ I) (>= I NDIMS)
			(SETQ INDEX-LENGTH
			      (// INDEX-LENGTH 
				  (%P-LDB-OFFSET %%Q-POINTER ARRAY (+ LONG-ARRAY-P I))))))
		INDEX-LENGTH)))))

;This is in microcode
;(DEFUN EQUAL (A B)
;  (PROG NIL 
;    L	(COND ((EQ A B) (RETURN T))
;	      ((NOT (= (%DATA-TYPE A) (%DATA-TYPE B))) (RETURN NIL))
;	      ((NUMBERP A) (RETURN (= A B)))
;	      ((ARRAYP A)
;	       (RETURN (AND (STRINGP A)
;			    (STRINGP B)
;			    (%STRING-EQUAL A 0 B 0 NIL))))
;	      ((ATOM A) (RETURN NIL))
;	      ((NOT (EQUAL (CAR A) (CAR B))) (RETURN NIL)))
;	(SETQ A (CDR A))
;	(SETQ B (CDR B))
;	(GO L)))

;This is here because the TV package calls it during initialization.
(DEFUN DELQ (ITEM LIST &OPTIONAL (/#TIMES -1))
  (PROG (LL PL)
A   (COND ((OR (= 0 /#TIMES) (ATOM LIST))
	   (RETURN LIST))
	  ((EQ ITEM (CAR LIST))
	   (SETQ LIST (CDR LIST))
	   (SETQ /#TIMES (1- /#TIMES))
	   (GO A)))
    (SETQ LL LIST)
B   (COND ((OR (= 0 /#TIMES) (ATOM LL))
	   (RETURN LIST))
	  ((EQ ITEM (CAR LL))
	   (RPLACD PL (CDR LL))
	   (SETQ /#TIMES (1- /#TIMES)))
	  ((SETQ PL LL)))
    (SETQ LL (CDR LL))
    (GO B)))

;The following are here because MAKE-SYMBOL uses them.  Maybe some of them
;should be in microcode?

(DEFUN MAKUNBOUND (X)
    (AND (MEMQ X '(T NIL))			;I guess it's worth checking
	 (FERROR NIL "Don't makunbound ~S please" X))
    ;; Value cell could be forwarded somewhere, e.g. into microcode memory
    (DO ((LOC (VALUE-CELL-LOCATION X) (%P-CONTENTS-AS-LOCATIVE LOC)))
	(( (%P-DATA-TYPE LOC) DTP-ONE-Q-FORWARD)
	 (%P-STORE-TAG-AND-POINTER LOC (+ DTP-NULL (LSH CDR-NEXT 6)) X)))
    X)

(DEFUN FMAKUNBOUND (X)
    (%P-STORE-TAG-AND-POINTER (FUNCTION-CELL-LOCATION X)
			      (+ DTP-NULL (LSH CDR-NEXT 6))
			      X)
    X)

(DEFUN SETPLIST (X L)
    (RPLACA (PROPERTY-CELL-LOCATION X)
	    L)
    L)

(DEFUN FSET (SYMBOL DEFINITION)
  (COND ((SYMBOLP SYMBOL)
	 (RPLACA (FUNCTION-CELL-LOCATION SYMBOL) DEFINITION)
	 DEFINITION)
	((FERROR NIL "~S is not a symbol" SYMBOL))))

(DEFUN NAMED-STRUCTURE-SYMBOL (NAMED-STRUCTURE)
  (LET ((SYM (IF (ARRAY-HAS-LEADER-P NAMED-STRUCTURE) (ARRAY-LEADER NAMED-STRUCTURE 1)
		 (AREF NAMED-STRUCTURE 0))))
    (COND ((SYMBOLP SYM) SYM)
	  (T (AND (CLOSUREP SYM)
		  (SETQ SYM (CAR (%MAKE-POINTER DTP-LIST SYM))))
	     (OR (SYMBOLP SYM)
		 (FERROR NIL "~S not a symbol in named-structure-symbol slot of ~S"
			     SYM NAMED-STRUCTURE))
	     SYM))))

(DEFUN NAMED-STRUCTURE-P (STRUCTURE)
   "If argument is a named-structure, return its name, otherwise NIL"
   (AND (ARRAYP STRUCTURE)
	(NOT (ZEROP (%P-LDB-OFFSET %%ARRAY-NAMED-STRUCTURE-FLAG STRUCTURE 0)))
	(NAMED-STRUCTURE-SYMBOL STRUCTURE)))

;This function exists mostly for easing the phaseover to the new OBJECT scheme
; (which flushes the SELF argument to the named-structure handler, and uses instead
; a free reference to the variable SELF).
(DEFUN NAMED-STRUCTURE-INVOKE (OPERATION STRUCTURE &REST ARGS)
  ;; This function used to take its first two arguments in the other order.
  ;; We are comitted to supporting the old argument order indefinitely.
  (IF (ARRAYP OPERATION)
      (PSETQ OPERATION STRUCTURE STRUCTURE OPERATION))
  (CHECK-ARG OPERATION SYMBOLP "a symbol")
  (CHECK-ARG STRUCTURE ARRAYP "an array")
  (LET* ((SELF STRUCTURE)
	 (C (IF (ARRAY-HAS-LEADER-P SELF)
		(ARRAY-LEADER SELF 1)
		(AREF SELF 0))))
    (IF (SYMBOLP C)
	(SETQ C (GET C 'NAMED-STRUCTURE-INVOKE)))
    (IF (EQ (TYPEP C) 'CLOSURE)			;If a closure, assume knows about SELF
	(LEXPR-FUNCALL C OPERATION ARGS)
	(LEXPR-FUNCALL C OPERATION SELF ARGS))))	;flush the SELF arg
						;when the phaseover is made (if ever).

;;; A number which increments approximately 60 times a second, and wraps
;;; around now and then (about once a day); it's only 23 bits long.
;;; 60-cycle clock not hooked up yet, simulate with microsecond clock.

(DECLARE (SPECIAL TIME-LAST-VALUE)) ;Remembers high-order bits and detects carries
(SETQ TIME-LAST-VALUE 0)            ;Maintenance of this depends on TIME being called
				    ; periodically by the scheduler

(DEFUN TIME ()
  (WITHOUT-INTERRUPTS
    (LET ((LOW (%UNIBUS-READ 764120))  ;Hardware synchronizes if you read this one first
	  (HIGH (%UNIBUS-READ 764122))
	  (SOFT (LDB 2205 TIME-LAST-VALUE)))
      (LET ((LOWTIME (DPB HIGH 0220 (LDB 1602 LOW))))  ;low 18 bits
	(SETQ TIME-LAST-VALUE
	      (DPB (IF (< LOWTIME (LDB 0022 TIME-LAST-VALUE))  ;If a carry into the high bits
		       (1+ SOFT)
		       SOFT)
		   2205 LOWTIME))))))

;;; These two functions deal with the wrap-around lossage
(DEFUN TIME-LESSP (TIME1 TIME2)
  (BIT-TEST 20000000 (%24-BIT-DIFFERENCE TIME1 TIME2)))

(DEFUN TIME-DIFFERENCE (TIME1 TIME2)
  (COND ((< TIME1 TIME2)
	 (+ (%24-BIT-DIFFERENCE TIME1 TIME2) 20000000 20000000))
	(T (%24-BIT-DIFFERENCE TIME1 TIME2))))

;This gets called during initialization to build the obarray.
(DEFUN BUILD-INITIAL-OBARRAY ()
  (SETQ OBARRAY (MAKE-ARRAY SIZE-OF-OB-TBL ':AREA CONTROL-TABLES ':TYPE ART-Q-LIST))
  (MAPATOMS-NR-SYM
    #'(LAMBDA (SYM &AUX OTHERSYM PKG OTHERPKG FLAG)
	(MULTIPLE-VALUE (OTHERSYM FLAG) (INTERN SYM))
	(COND (FLAG		;Conflict, try to pick the more useful symbol
	       (SETQ PKG (CAR (PACKAGE-CELL-LOCATION SYM))
		     OTHERPKG (CAR (PACKAGE-CELL-LOCATION OTHERSYM)))
	       (COND ((OR (AND (NULL PKG) (NOT (NULL OTHERPKG)))
			  (EQ PKG 'GLOBAL)
			  (AND (MEMQ PKG '(SYS SYSTEM)) (NEQ OTHERPKG 'GLOBAL)))
		      (REMOB-OLD OTHERSYM)
		      (INTERN SYM))))))))

;The elements of <ARGS> are arguments to be supplied to <FUNCTION>,
; except for the last one, which is a list whose elements are arguments
; to be supplied to <FUNCTION>.
(DEFUN LEXPR-FUNCALL (FUNCTION &REST ARGS)
    (CHECK-ARG ARGS (NOT (NULL ARGS)) "a list of at least one element, the /"rest/" arg")
    (%OPEN-CALL-BLOCK FUNCTION 0 4)  ;No ADI, D-RETURN
    (%ASSURE-PDL-ROOM (+ (1- (LENGTH ARGS)) (LENGTH (LAST ARGS))))
    (DO ((ARGL ARGS (CDR ARGL)))
	((NULL (CDR ARGL))
	 (DO ((RESTARGL (CAR ARGL) (CDR RESTARGL)))
	     ((NULL RESTARGL)
	      (%ACTIVATE-OPEN-CALL-BLOCK))
	   (%PUSH (CAR RESTARGL))))
      (%PUSH (CAR ARGL))))

;USED BY LDB SETF PROPERTY.  THIS IS PRETTY CROCKISH.
(DEFUN DPB-VIA-LOCATIVE (VAL PPSS LOCATIVE)
    (RPLACD LOCATIVE (DPB VAL PPSS (CDR LOCATIVE))))

;;; Set all free pointers of an area to 0, deleting its entire contents
(DEFUN RESET-TEMPORARY-AREA (AREA)
  (WITHOUT-INTERRUPTS  ;don't let the area's region list change
    (DO REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION) (MINUSP REGION)
      (GC-RESET-FREE-POINTER REGION 0))))

;This function is used to adjust the free pointer of a region up or down,
;for functions other than the normal microcoded CONS and UN-CONS.
;It must do the following:
; Store into REGION-FREE-POINTER
; If REGION-GC-POINTER >= the old free pointer, set it to the new free pointer
;   (This does not work for compact-consing list regions, but it won't be called for them)
;   (This is actually only necessary when decreasing the free pointer, but it
;    doesn't hurt to do it all the time.)
; Reset the scavenger if it is in that region.  Could check for an actual
;  conflict, but that would be more difficult and wouldn't buy a great deal.
; Adjust A-CONS-WORK-DONE
(DEFUN GC-RESET-FREE-POINTER (REGION NEWFP)
  (OR INHIBIT-SCHEDULING-FLAG
      (FERROR NIL "This function must be called with scheduling inhibited"))
  (LET ((OLDFP (REGION-FREE-POINTER REGION)))
    (STORE (REGION-FREE-POINTER REGION) NEWFP)
    (AND (>= (REGION-GC-POINTER REGION) OLDFP)
	 (STORE (REGION-GC-POINTER REGION) NEWFP))
    (%GC-SCAV-RESET REGION)
    (%GC-CONS-WORK (- NEWFP OLDFP))))

;;; Make an array large or smaller.  For multi-dimensional arrays,
;;; changes the last dimension (the one which varies slowest).
;;; If array displaced, adjust request refers to the displaced header, not
;;; pointed-to data.
;;; If the array is copied, the value returned is the new copy but the old
;;; copy is forwarded to point to the new.
;;;*** Fails to adjust the cdr-codes of ART-Q-LIST arrays
(DEFUN ADJUST-ARRAY-SIZE (ARRAY NEW-INDEX-LENGTH
			  &AUX REGION CURRENT-DATA-LENGTH ARRAY-TYPE-NUMBER 
			       NDIMS ENTRIES-PER-Q NEW-DATA-LENGTH NEW-ARRAY 
			       FREED-ARRAY-LOCN FREED-ARRAY-LENGTH
			       ARRAY-DATA-BASE LONG-ARRAY-BIT CURRENT-INDEX-LENGTH
			       ARRAY-DATA-BASE-RELATIVE-TO-REGION-ORIGIN)
  (CHECK-ARG ARRAY ARRAYP "an array")
  (WITHOUT-INTERRUPTS		;Disallow garbage collection (flipping), references
				; to the array, and allocation in the region.
    (SETQ ARRAY (FOLLOW-STRUCTURE-FORWARDING ARRAY))
    ;;By this point, ARRAY cannot be in oldspace
    (SETQ NDIMS (%P-LDB %%ARRAY-NUMBER-DIMENSIONS ARRAY)
	  LONG-ARRAY-BIT (%P-LDB %%ARRAY-LONG-LENGTH-FLAG ARRAY)
	  ARRAY-DATA-BASE (+ (%MAKE-POINTER DTP-FIX ARRAY) ;Safe since can't move now
			     LONG-ARRAY-BIT	;Careful, this can be a negative number!
			     NDIMS)
	  CURRENT-INDEX-LENGTH (IF (ZEROP LONG-ARRAY-BIT)
				   (%P-LDB %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY)
				   (%P-CONTENTS-OFFSET ARRAY 1))
	  REGION (%REGION-NUMBER ARRAY)
	  ARRAY-DATA-BASE-RELATIVE-TO-REGION-ORIGIN
	  	 (%24-BIT-DIFFERENCE ARRAY-DATA-BASE
				     (REGION-ORIGIN REGION))				     
	  ARRAY-TYPE-NUMBER (%P-LDB %%ARRAY-TYPE-FIELD ARRAY)
	  ENTRIES-PER-Q (AR-1 (FUNCTION ARRAY-ELEMENTS-PER-Q) ARRAY-TYPE-NUMBER)
	  NEW-DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
			      (// (+ NEW-INDEX-LENGTH (1- ENTRIES-PER-Q))
				  ENTRIES-PER-Q)
			      (* NEW-INDEX-LENGTH (MINUS ENTRIES-PER-Q)))
	  CURRENT-DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
				  (// (+ CURRENT-INDEX-LENGTH (1- ENTRIES-PER-Q))
				      ENTRIES-PER-Q)
				  (* CURRENT-INDEX-LENGTH (MINUS ENTRIES-PER-Q))))
    (COND ((NOT (ZEROP (%P-LDB %%ARRAY-DISPLACED-BIT ARRAY)))	;Displaced array
	   (SETQ CURRENT-INDEX-LENGTH (%P-CONTENTS-OFFSET ARRAY-DATA-BASE 1))
	   (COND ((> NEW-INDEX-LENGTH CURRENT-INDEX-LENGTH)
		  (FERROR NIL "Can't make displaced array ~S bigger" ARRAY)))
	   (%P-STORE-CONTENTS-OFFSET NEW-INDEX-LENGTH ARRAY-DATA-BASE 1)
	   ARRAY)
	  ((<= NEW-DATA-LENGTH CURRENT-DATA-LENGTH)	;No new storage required
	   (COND ((= NEW-DATA-LENGTH CURRENT-DATA-LENGTH))	;No storage change
		 ((= (+ ARRAY-DATA-BASE-RELATIVE-TO-REGION-ORIGIN
			CURRENT-DATA-LENGTH)	;Give back from end of region
		     (REGION-FREE-POINTER REGION))
		  (GC-RESET-FREE-POINTER REGION
					 (+ ARRAY-DATA-BASE-RELATIVE-TO-REGION-ORIGIN
					    NEW-DATA-LENGTH)))
		 (T	;Fill hole in region with an ART-32B array
		  (%GC-SCAV-RESET REGION) ;Make scavenger forget about this region
		  (SETQ FREED-ARRAY-LOCN
			(%24-BIT-PLUS ARRAY-DATA-BASE NEW-DATA-LENGTH))
		  (COND ((<= (SETQ FREED-ARRAY-LENGTH
				   (1- (- CURRENT-DATA-LENGTH NEW-DATA-LENGTH)))
			     %ARRAY-MAX-SHORT-INDEX-LENGTH)
			 (%P-STORE-TAG-AND-POINTER FREED-ARRAY-LOCN DTP-ARRAY-HEADER 
						   (+ ARRAY-DIM-MULT ART-32B
						      FREED-ARRAY-LENGTH)))
			(T (%P-STORE-TAG-AND-POINTER FREED-ARRAY-LOCN DTP-ARRAY-HEADER 
						     (+ ARRAY-DIM-MULT ART-32B
							ARRAY-LONG-LENGTH-FLAG))
			   (%P-STORE-CONTENTS-OFFSET (1- FREED-ARRAY-LENGTH)
						     FREED-ARRAY-LOCN
						     1)))))
	   (IF (ZEROP LONG-ARRAY-BIT)
	       (%P-DPB NEW-INDEX-LENGTH %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY)
	       (%P-STORE-CONTENTS-OFFSET NEW-INDEX-LENGTH ARRAY 1))
	   ARRAY)
	  ;; Need increased storage.  Either make fresh copy or extend existing copy.
	  ((OR (AND (ZEROP LONG-ARRAY-BIT)	     ;See if need to make fresh copy because
		    (> NEW-INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)) ;need format change
	       (< (+ ARRAY-DATA-BASE-RELATIVE-TO-REGION-ORIGIN
		     CURRENT-DATA-LENGTH) ;or not at end of region
		  (REGION-FREE-POINTER REGION))
	       (> (+ ARRAY-DATA-BASE-RELATIVE-TO-REGION-ORIGIN
		     NEW-DATA-LENGTH)	;or region isn't big enough
		  (REGION-LENGTH REGION)))
	   (SETQ NEW-ARRAY (MAKE-ARRAY (IF (= NDIMS 1) NEW-INDEX-LENGTH
					   (LET ((DIMS (ARRAY-DIMENSIONS ARRAY)))
					     (RPLACA (LAST DIMS) 1)
					     (RPLACA (LAST DIMS) (// NEW-INDEX-LENGTH
								     (LIST-PRODUCT DIMS)))
					     DIMS))
				       ':AREA (%AREA-NUMBER ARRAY)
				       ':TYPE (AR-1 (FUNCTION ARRAY-TYPES) ARRAY-TYPE-NUMBER)
				       ':LEADER-LENGTH (ARRAY-DIMENSION-N 0 ARRAY)))
	   (COPY-ARRAY-CONTENTS-AND-LEADER ARRAY NEW-ARRAY)
	   (%P-DPB (%P-LDB %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY)
		   %%ARRAY-NAMED-STRUCTURE-FLAG NEW-ARRAY)
	   (%P-DPB (%P-LDB %%ARRAY-FLAG-BIT ARRAY)
		   %%ARRAY-FLAG-BIT NEW-ARRAY)
	   (STRUCTURE-FORWARD ARRAY NEW-ARRAY)
	   NEW-ARRAY)
	  (T					;Array is at end of region, just make bigger
	   (GC-RESET-FREE-POINTER REGION (+ ARRAY-DATA-BASE-RELATIVE-TO-REGION-ORIGIN
					    NEW-DATA-LENGTH))
	   (IF (ZEROP LONG-ARRAY-BIT)
	       (%P-DPB NEW-INDEX-LENGTH %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY)
	       (%P-STORE-CONTENTS-OFFSET NEW-INDEX-LENGTH ARRAY 1))
	   ;; Fill with NIL or 0
	   (DO ((ADR (%24-BIT-PLUS ARRAY-DATA-BASE CURRENT-DATA-LENGTH)
		     (%24-BIT-PLUS ADR 1))
		(N (- NEW-DATA-LENGTH CURRENT-DATA-LENGTH) (1- N))
		(NUMERIC-P (ARRAY-BITS-PER-ELEMENT ARRAY-TYPE-NUMBER)))
	       ((ZEROP N))
	     (IF NUMERIC-P
		 (%P-STORE-TAG-AND-POINTER ADR 0 0)
		 (%P-STORE-TAG-AND-POINTER ADR (+ (LSH CDR-NIL 5) DTP-SYMBOL) NIL)))
	   ARRAY))))

;;; Dispose of an array, returning its storage to free if possible.
;;; If the array is displaced, the displaced-array header is disposed of,
;;; not the pointed-to data.
;;; You had better get rid of all pointers to this array before calling this,
;;; e.g. (RETURN-ARRAY (PROG1 FOO (SETQ FOO NIL)))
;;; Returns T if storage really reclaimed, NIL if not.
(DEFUN RETURN-ARRAY (ARRAY &AUX REGION ARRAY-ORIGIN ARRAY-SIZE)
  (CHECK-ARG ARRAY ARRAYP "an array")
  (WITHOUT-INTERRUPTS	;Turn off garbage collection, allocation in this region
    (SETQ ARRAY (FOLLOW-STRUCTURE-FORWARDING ARRAY))
    ;;By this point, ARRAY cannot be in oldspace
    (SETQ REGION (%REGION-NUMBER ARRAY)
	  ARRAY-ORIGIN (%POINTER (%FIND-STRUCTURE-LEADER ARRAY))
	  ARRAY-SIZE (%STRUCTURE-TOTAL-SIZE ARRAY))
    (COND ((= (%24-BIT-PLUS ARRAY-ORIGIN ARRAY-SIZE)
	      (%24-BIT-PLUS (REGION-ORIGIN REGION) (REGION-FREE-POINTER REGION)))
	   (GC-RESET-FREE-POINTER REGION (%24-BIT-DIFFERENCE ARRAY-ORIGIN
							     (REGION-ORIGIN REGION)))
	   T)
	  (T NIL))))

;;; Formerly used by the DEFVAR macro in LMMAC.  Keep this around because it
;;; is compiled into old QFASL files.
(DEFUN DEFVAR-1 (SYMBOL VALUE)
  (OR (BOUNDP SYMBOL) (SET SYMBOL VALUE)))

;;; Used by DEFVAR
(DEFUN SETQ-IF-UNBOUND (&QUOTE SYMBOL FORM)
  (OR (BOUNDP SYMBOL) (SET SYMBOL (EVAL FORM))))

;;; Function spec and source file name stuff
;;; (The rest of this is in QMISC)

;; A function-specifier is just a way of talking about a function
;; for purposes other than applying it.  It can be a symbol, in which case
;; the function cell of the symbol is used.  Or it can be a list of one of
;; these formats:
;; (:METHOD class-name operation) refers to the method in that class for
;;   that operation; this works for both Class methods and Flavor methods.
;;   In the case of Flavor methods, the specification may also be of the form
;;   (:METHOD flavor-name type operation).
;; (:INSTANCE-METHOD exp operation).  exp should evaluate to an entity.
;;   Reference is then to the operation directly on that instance.
;; (:HANDLER flavor operation) refers to the function that is called when
;;   an object of flavor FLAVOR is sent the message OPERATION.
;; (:WITHIN within-function renamed-function) refers to renamed-function,
;;   but only as called directly from within-function.
;;   Actually, renamed-function is replaced throughout within-function
;;   by an uninterned symbol whose definition is just renamed-function
;;   as soon as an attempt is made to do anything to a function spec
;;   of this form.  The function spec is from then on equivalent
;;   to that uninterned symbol.
;; (:PROPERTY symbol property) refers to (GET symbol property).
;; (:LOCATION locative-or-list-pointer) refers to the CDR of the pointer.
;;   This is for pointing at an arbitrary place
;;   which there is no special way to describe.
;; One place you can use a function specifier is in DEFUN.
;
; For Maclisp compatibility, a list whose car is not recognized is taken
; to be a list of a symbol and a property, by DEFUN and DEFMACRO.  They
; standardize this by putting :PROPERTY on the front.  These
; non-standard function specs are not accepted by the rest of the
; system.  This is done to avoid ambiguities and inconsistencies.

;The SYS:FUNCTION-SPEC-HANDLER property of a symbol, if present means that that
;symbol is legal as the car of a function spec.  The value of the property
;is a function whose arguments are the function in behalf
;of which to act (not a keyword symbol!) and the arguments to that
;function (the first of which is always the function spec).
;Functions are:
;	FDEFINE definition
;	FDEFINEDP
;	FDEFINITION
;	FDEFINITION-LOCATION
;	FUNDEFINE
;	FUNCTION-PARENT
;	COMPILER-FDEFINEDP -- returns T if will be fdefinedp at run time
;	GET indicator
;	PUTPROP value indicator

;;; NIL, T, or :JUST-WARN
(DEFVAR INHIBIT-FDEFINE-WARNINGS NIL)

;Predicate for use with CHECK-ARG
;Also returns the type keyword (T for a symbol)
(DEFUN VALIDATE-FUNCTION-SPEC (FUNCTION-SPEC &AUX HANDLER)
  (COND ((ATOM FUNCTION-SPEC)
	 (AND (NOT (NULL FUNCTION-SPEC)) (SYMBOLP FUNCTION-SPEC)))
	((AND (SYMBOLP (CAR FUNCTION-SPEC))
	      (SETQ HANDLER (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER))
	      (FUNCALL HANDLER 'VALIDATE-FUNCTION-SPEC FUNCTION-SPEC))
	 (CAR FUNCTION-SPEC))))

(DEFUN FDEFINE (FUNCTION-SPEC DEFINITION &OPTIONAL CAREFULLY-FLAG NO-QUERY-FLAG
					 &AUX TYPE INNER-SPEC)
"Alter the function definition of a function specifier.
CAREFULLY-FLAG means preserve any tracing or advice,
and save the old definition, when possible.
This function returns T if it does define the function, or NIL if it does not.
If FDEFINE-FILE-PATHNAME is non-NIL, then it is the file which this definition
was read from, and we make a note of that fact when possible."

  ;; First, validate the function spec and determine its type
  (CHECK-ARG FUNCTION-SPEC
	     (SETQ TYPE (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC))
	     "a symbol or a function-spec list")

  ;; Record the source file name, if desired, and check for redefinition errors
  (COND ((OR (EQ TYPE ':INTERNAL)
	     (RECORD-SOURCE-FILE-NAME FUNCTION-SPEC 'DEFUN
				      (OR NO-QUERY-FLAG (NOT CAREFULLY-FLAG)
					  (EQ INHIBIT-FDEFINE-WARNINGS T))))

	 ;; If there is a previous definition, save it (if desired).
	 ;; Also if it is encapsulated, set INNER-SPEC to the symbol
	 ;; which holds the real definition before encapsulation, and
	 ;; save that definition.
	 (COND ((AND CAREFULLY-FLAG (FDEFINEDP FUNCTION-SPEC))
		(SETQ INNER-SPEC (UNENCAPSULATE-FUNCTION-SPEC FUNCTION-SPEC))
		(AND (FDEFINEDP INNER-SPEC)
		     (LET ((INNER-DEF (FDEFINITION INNER-SPEC)))
		       (AND (LISTP INNER-DEF)
			    (NOT (AND (EQ (CAR INNER-DEF) 'MACRO)
				      (TYPEP (CDR INNER-DEF) ':COMPILED-FUNCTION)))
			    (FUNCTION-SPEC-PUTPROP FUNCTION-SPEC INNER-DEF
						   ':PREVIOUS-EXPR-DEFINITION))
		       (FUNCTION-SPEC-PUTPROP FUNCTION-SPEC INNER-DEF ':PREVIOUS-DEFINITION)))
		;; Carry over renamings from previous definition
		(AND (FBOUNDP 'RENAME-WITHIN-NEW-DEFINITION-MAYBE)
		     (SETQ DEFINITION (RENAME-WITHIN-NEW-DEFINITION-MAYBE FUNCTION-SPEC
									  DEFINITION))))
	       (T (SETQ INNER-SPEC FUNCTION-SPEC)))

	 ;; Now store the new definition in type-dependent fashion
	 (IF (SYMBOLP INNER-SPEC) (FSET INNER-SPEC DEFINITION)
	     (FUNCALL (GET TYPE 'FUNCTION-SPEC-HANDLER) 'FDEFINE INNER-SPEC DEFINITION))

	 ;; Return T since we did define the function
	 T)
	;; Return NIL since we decided not to define the function
	(T NIL)))

;; Is a function specifier defined?  A generalization of FBOUNDP.
(DEFUN FDEFINEDP (FUNCTION-SPEC &AUX TYPE)
  "Returns T if the function spec has a function definition."
  ;; First, validate the function spec and determine its type
  (CHECK-ARG FUNCTION-SPEC
	     (SETQ TYPE (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC))
	     "a symbol or a function-spec list")
  ;; Then perform type-dependent code
  (IF (SYMBOLP FUNCTION-SPEC) (FBOUNDP FUNCTION-SPEC)
      (NOT (NULL (FUNCALL (GET TYPE 'FUNCTION-SPEC-HANDLER) 'FDEFINEDP FUNCTION-SPEC)))))

;; Get the definition of a function specifier.  Generalized FSYMEVAL.
(DEFUN FDEFINITION (FUNCTION-SPEC &AUX TYPE)
  "Returns the function definition of a function spec"
  ;; First, validate the function spec and determine its type
  (CHECK-ARG FUNCTION-SPEC
	     (SETQ TYPE (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC))
	     "a symbol or a function-spec list")
  (IF (SYMBOLP FUNCTION-SPEC) (FSYMEVAL FUNCTION-SPEC)
      (FUNCALL (GET TYPE 'FUNCTION-SPEC-HANDLER) 'FDEFINITION FUNCTION-SPEC)))


;Default handler called by function-spec-handlers to do functions they don't
;handle specially.
(DEFUN FUNCTION-SPEC-DEFAULT-HANDLER (FUNCTION FUNCTION-SPEC ARG1 ARG2)
  (SELECTQ FUNCTION
    (FUNCTION-PARENT NIL)		;Default is no embedding in other definitions
    (COMPILER-FDEFINEDP NIL)		;Default is no remembering of compiled definitions
    (GET (IF FUNCTION-SPEC-HASH-TABLE
	     (FUNCTION-SPEC-GET-1 FUNCTION-SPEC ARG1)	;Default is to use plist hash table
	     (LOOP FOR (FS IND PROP) IN COLD-LOAD-FUNCTION-PROPERTY-LISTS
		   WHEN (AND (EQUAL FS FUNCTION-SPEC) (EQ IND ARG1))
		   RETURN PROP)))
    (PUTPROP (IF FUNCTION-SPEC-HASH-TABLE
		 (PUTHASH-EQUAL (LIST FUNCTION-SPEC ARG2) ARG1 FUNCTION-SPEC-HASH-TABLE)
		 (PUSH (LIST FUNCTION-SPEC ARG2 ARG1) COLD-LOAD-FUNCTION-PROPERTY-LISTS)))
    (OTHERWISE (FERROR NIL "~S is not implemented by the function spec ~S"
		           FUNCTION FUNCTION-SPEC))))

;; (:PROPERTY symbol property) refers to (GET symbol property).
;; This has to be defined with a separate DEFPROP for reasons which should be obvious.
(DEFPROP :PROPERTY PROPERTY-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN PROPERTY-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((SYMBOL (SECOND FUNCTION-SPEC))
	(INDICATOR (THIRD FUNCTION-SPEC)))
    (SELECTQ FUNCTION
      (VALIDATE-FUNCTION-SPEC (AND (= (LENGTH FUNCTION-SPEC) 3) (SYMBOLP SYMBOL)))
      (FDEFINE (PUTPROP SYMBOL ARG1 INDICATOR))
      ((FDEFINITION FDEFINEDP) (GET SYMBOL INDICATOR))
      (FDEFINITION-LOCATION (LOCF (GET SYMBOL INDICATOR)))	;Not perfect, but close
      (FUNDEFINE (REMPROP SYMBOL INDICATOR))
      (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))

;; (:INTERNAL parent-function index) refers to the index'th unnamed
;; broken-off lambda in the parent function.
;; parent-function is normally a function-spec, but it may also be a FEF.
;; Note that VALIDATE-FUNCTION-SPEC for :INTERNAL returns NIL if the
;; function-spec itself is malformed, however if the spec is well-formed
;; but the parent doesn't have internal functions, an error is signalled
;; giving a detailed explanation.
(DEFPROP :INTERNAL INTERNAL-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN INTERNAL-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((PARENT (SECOND FUNCTION-SPEC))
	(INDEX (THIRD FUNCTION-SPEC)) DIRECT-FEF)
    ;; Perform basic validation before accessing parent
    ;; This is a no-op except when the function is VALIDATE-FUNCTION-SPEC
    (AND (OR (SETQ DIRECT-FEF (= (%DATA-TYPE PARENT) DTP-FEF-POINTER))
	     (AND (VALIDATE-FUNCTION-SPEC PARENT) (FDEFINEDP PARENT)))
	 (FIXNUMP INDEX)
	 (NOT (MINUSP INDEX))
	 (LET ((FEF (IF DIRECT-FEF PARENT
			(FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC PARENT))))
	       TABLE OFFSET)
	   (AND (LISTP FEF) (EQ (CAR FEF) 'MACRO)
		(SETQ FEF (CDR FEF)))
	   (OR (= (%DATA-TYPE FEF) DTP-FEF-POINTER)
	       (FERROR NIL "The function spec ~S refers to ~S, which is not a FEF"
		           FUNCTION-SPEC FEF))
	   (OR (SETQ TABLE (CDR (ASSQ ':INTERNAL-FEF-OFFSETS (DEBUGGING-INFO FEF))))
	       (FERROR NIL
		       "The function spec ~S refers to ~S, which has no internal functions"
		       FUNCTION-SPEC FEF))
	   (OR (SETQ OFFSET (NTH INDEX TABLE))
	       (FERROR NIL "The function spec ~S is out of range" FUNCTION-SPEC))

	   ;; Function spec fully parsed, we can now earn our living
	   (SELECTQ FUNCTION
	     (VALIDATE-FUNCTION-SPEC (= (LENGTH FUNCTION-SPEC) 3))
	     (FDEFINE (%P-STORE-CONTENTS-OFFSET ARG1 FEF OFFSET))
	     (FDEFINITION (%P-CONTENTS-OFFSET FEF OFFSET))
	     (FDEFINEDP		;Random: look for what the compiler puts there initially
		(NOT (EQUAL (%P-CONTENTS-OFFSET FEF OFFSET) FUNCTION-SPEC)))
	     (FDEFINITION-LOCATION (%MAKE-POINTER-OFFSET DTP-LOCATIVE FEF OFFSET))
	     (FUNCTION-PARENT (VALUES PARENT 'DEFUN))
	     (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))))

;;; This is setup by QLD as soon as everything it will need is loaded in and pathnames work
;;; and so on.
(DEFVAR FUNCTION-SPEC-HASH-TABLE NIL)
;;; In the meantime, and from the cold load, this remembers non symbol source files,
;;; elements are (function-spec indicator value).
(DEFVAR COLD-LOAD-FUNCTION-PROPERTY-LISTS)

(DEFUN FUNCTION-SPEC-PUTPROP (FUNCTION-SPEC VALUE INDICATOR &AUX TYPE)
  (CHECK-ARG FUNCTION-SPEC
	     (SETQ TYPE (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC))
	     "a symbol or a function-spec list")
  (IF (SYMBOLP FUNCTION-SPEC)
      (PUTPROP FUNCTION-SPEC VALUE INDICATOR)
      (FUNCALL (GET TYPE 'FUNCTION-SPEC-HANDLER) 'PUTPROP FUNCTION-SPEC VALUE INDICATOR)))

(DEFUN FUNCTION-SPEC-GET (FUNCTION-SPEC INDICATOR &AUX TYPE)
  (CHECK-ARG FUNCTION-SPEC
	     (SETQ TYPE (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC))
	     "a symbol or a function-spec list")
  (IF (SYMBOLP FUNCTION-SPEC)
      (GET FUNCTION-SPEC INDICATOR)
      (FUNCALL (GET TYPE 'FUNCTION-SPEC-HANDLER) 'GET FUNCTION-SPEC INDICATOR)))

;This function exists only for the sake of an &rest argument to avoid consing
(DEFUN FUNCTION-SPEC-GET-1 (&REST KEY)
  (DECLARE (ARGLIST FUNCTION-SPEC INDICATOR))
  (GETHASH-EQUAL KEY FUNCTION-SPEC-HASH-TABLE))

(DEFPROP FUNCTION-SPEC-GET ((FUNCTION-SPEC-GET FUNCTION-SPEC INDICATOR) .
			    (FUNCTION-SPEC-PUTPROP FUNCTION-SPEC VAL INDICATOR))
	 SETF)

(SETQ FS:THIS-IS-A-PATCH-FILE NIL)	;For the cold load

;;; A :SOURCE-FILE-NAME property is a single pathname for DEFUN of a single file,
;;; or ((type . files) (type . files) ...).
;;; Value returned indicates whether to go ahead with the definition.
(DEFUN RECORD-SOURCE-FILE-NAME (FUNCTION-SPEC
				&OPTIONAL (TYPE 'DEFUN)
					  (NO-QUERY (EQ INHIBIT-FDEFINE-WARNINGS T)))
  (IF (NULL FDEFINE-FILE-PATHNAME)
      T
      (LET ((PATHNAME FDEFINE-FILE-PATHNAME)
	    (PROPERTY (FUNCTION-SPEC-GET FUNCTION-SPEC ':SOURCE-FILE-NAME)))
	(COND ((AND (NULL PROPERTY)		;Check most common case first
		    (EQ TYPE 'DEFUN))
	       (FUNCTION-SPEC-PUTPROP FUNCTION-SPEC PATHNAME ':SOURCE-FILE-NAME)
	       T)
	      ((IF (NLISTP PROPERTY)
		   (AND (EQ TYPE 'DEFUN) (EQ PATHNAME PROPERTY))
		   (EQ PATHNAME (CADR (ASSQ TYPE PROPERTY))))
	       T)				;This pathname already known
	      (T
	       (AND PROPERTY (NLISTP PROPERTY)
		    (SETQ PROPERTY `((DEFUN ,PROPERTY))))
	       (LET ((THIS-TYPE (ASSQ TYPE PROPERTY))
		     (OLD-FILE))
		 (COND ((COND ((NULL THIS-TYPE)
			       (SETQ THIS-TYPE `(,TYPE)
				     PROPERTY (NCONC PROPERTY
						     (NCONS THIS-TYPE)))
			       T)
			      (NO-QUERY T)
			      (FS:THIS-IS-A-PATCH-FILE T)
			      ((NOT (FBOUNDP 'FORMAT)) T)
			      ((NULL (SETQ OLD-FILE (LOOP FOR FILE IN (CDR THIS-TYPE)
							  UNLESS (FUNCALL FILE ':GET
									  ':PATCH-FILE)
							  RETURN FILE)))
			       T)
			      ((QUERY-ABOUT-REDEFINITION FUNCTION-SPEC PATHNAME TYPE
							 OLD-FILE)))
			(PUSH PATHNAME (CDR THIS-TYPE))
			(FUNCTION-SPEC-PUTPROP FUNCTION-SPEC PROPERTY ':SOURCE-FILE-NAME)
			T)
		       (T NIL))))))))

;;; Call on a function on every symbol in the world, regardless of obarrays
(DEFUN MAPATOMS-NR-SYM (FN)
  (FUNCALL FN NIL)
  (FUNCALL FN T)
  (DO REGION (AREA-REGION-LIST NR-SYM) (REGION-LIST-THREAD REGION) (MINUSP REGION)
    (DO ((SY (REGION-ORIGIN REGION) (%24-BIT-PLUS SY LENGTH-OF-ATOM-HEAD))
         (CT (// (REGION-FREE-POINTER REGION) LENGTH-OF-ATOM-HEAD) (1- CT))
         (SYM))
        ((ZEROP CT))
      (SETQ SYM (%MAKE-POINTER DTP-SYMBOL SY))
      (FUNCALL FN SYM))))

(DEFUN FOLLOW-STRUCTURE-FORWARDING (X)
  "Get the final structure this one may be forwarded to.
   Given a pointer to a structure, if it has been forwarded by STRUCTURE-FORWARD,
   ADJUST-ARRAY-SIZE, or the like, this will return the target structure, following
   any number of levels of forwarding."
  (WITHOUT-INTERRUPTS
    (COND ((= (%P-DATA-TYPE X) DTP-HEADER-FORWARD)
	   (FOLLOW-STRUCTURE-FORWARDING
	     (%MAKE-POINTER (%DATA-TYPE X) (%P-CONTENTS-AS-LOCATIVE X))))
	  ((= (%P-DATA-TYPE X) DTP-BODY-FORWARD)
	   (LET ((HDRP (%P-CONTENTS-AS-LOCATIVE X)))
	     (FOLLOW-STRUCTURE-FORWARDING
	       (%MAKE-POINTER-OFFSET (%DATA-TYPE X)
				     (%P-CONTENTS-AS-LOCATIVE HDRP)
				     (%POINTER-DIFFERENCE X HDRP)))))
	  (T X))))

;;; This is defined here since macros can be defined before the editor is loaded
;;; In fact even before DEFMACRO is loaded
(DEFVAR ZWEI:*INITIAL-LISP-INDENT-OFFSET-ALIST* NIL)

(DEFUN DEFMACRO-SET-INDENTATION-FOR-ZWEI (NAME NUMBER)
  (LET ((VARIABLE (IF (BOUNDP 'ZWEI:*LISP-INDENT-OFFSET-ALIST*)
		      'ZWEI:*LISP-INDENT-OFFSET-ALIST*
		      'ZWEI:*INITIAL-LISP-INDENT-OFFSET-ALIST*)))
    (LET ((X (ASSQ NAME (SYMEVAL VARIABLE))))
      (IF (NULL X)
	  (PUSH (LIST NAME NUMBER 1) (SYMEVAL VARIABLE))
	  (RPLACD X (LIST NUMBER 1))))))
