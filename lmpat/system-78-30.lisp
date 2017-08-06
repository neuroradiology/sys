;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.28
;;; Reason: Problems with ARGLIST
;;; Written 12/22/81 16:56:17 by DLA,
;;; while running on Lisp Machine Four from band 2
;;; with System 78.27, ZMail 38.4, microcode 841, 60Hz.



; From file QFCTNS > LISPM; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;; ARGLIST returns the list of argument names and the list of
;; returned value names of the definition of a function spec.
;; The first value is the arglist: a list of the names
;; of the arguments, together with lambda list keywords.
;; The second value is the list of returned value names.
;; This list is present only if the definition of the function
;; supplies one, and it is just a comment.  Those names play no
;; actual role in execution.

;; The argument REAL-FLAG is T to inhibit the use of any declared
;; (comment only) arglist information.  Only the actual arglist of the function
;; is returned.  Normally the arglist specified for human comsumption
;; with an arglist declaration overrides the real one.

;; REAL-FLAG also inhibits following encapsulations.
;; So you get the arglist of the encapsulation rather than the
;; original definition.

;; T should be used by anything that requires a "legitimate" arglist
;; that reliably corresponds to what the function does with its args.

;; We accept both functions and function specs.

(DEFUN ARGLIST (FUNCTION &OPTIONAL REAL-FLAG &AUX TYPE TEM DEBUG-INFO ARG-MAP LOCAL-MAP)
  (DECLARE (RETURN-LIST ARGLIST RETURN-LIST TYPE))
  (SETQ TYPE (%DATA-TYPE FUNCTION))
  (COND ((SYMBOLP FUNCTION)
	 (ARGLIST (FSYMEVAL FUNCTION) REAL-FLAG))
	((LISTP FUNCTION)
	 (COND ((EQ (CAR FUNCTION) 'LAMBDA)
		(LDIFF (CADR FUNCTION) (MEMQ '&AUX (CADR FUNCTION))))
	       ((EQ (CAR FUNCTION) 'SUBST)
		(VALUES (CADR FUNCTION) NIL 'SUBST))
	       ((OR (EQ (CAR FUNCTION) 'NAMED-SUBST)
		    (EQ (CAR FUNCTION) 'NAMED-LAMBDA))
		(SETQ DEBUG-INFO (FUNCTION-DEBUGGING-INFO FUNCTION))
		(COND ((AND (NOT REAL-FLAG)
			    (ASSQ 'ENCAPSULATED-DEFINITION DEBUG-INFO))
		       (ARGLIST (CADR (ASSQ 'ENCAPSULATED-DEFINITION DEBUG-INFO))))
		      (T
		       (VALUES
			 (OR (AND (NOT REAL-FLAG)
				  ;Take this check out someday after everything recompiled
				  (LET ((AL (CDR (ASSQ 'ARGLIST DEBUG-INFO))))
				    (IF (OR (ATOM AL) (ATOM (CAR AL))) AL	;New format
					(CAR AL))))		;Old format
			     (LDIFF (CADDR FUNCTION) (MEMQ '&AUX (CADDR FUNCTION))))
			 ;Take this check out someday after everything recompiled
			 (LET ((RL (CDR (ASSQ 'RETURN-LIST DEBUG-INFO))))
			   (IF (ATOM (CAR RL)) RL		;New format
			       (CAR RL)))			;Old format
			 (AND (EQ (CAR FUNCTION) 'NAMED-SUBST) 'SUBST)))))
	       ((MEMQ (CAR FUNCTION) '(CURRY-BEFORE CURRY-AFTER))
		'(&REST ARGLIST))
	       ((EQ (CAR FUNCTION) 'MACRO)
		;; Look for (LOCAL-DECLARE ((ARGLIST ...))) type arglist
		(VALUES (OR (AND (NOT REAL-FLAG)
				  ;Take this check out someday after everything recompiled
				  (LET ((AL (CDR (ASSQ 'ARGLIST (FUNCTION-DEBUGGING-INFO
								   (CDR FUNCTION))))))
				    (COND ((OR (ATOM AL) (ATOM (CAR AL))) AL)	;New format
					  (T (CAR AL)))))	;Old format
			    'MACRO)
			NIL
			'MACRO))
	       ((FDEFINEDP FUNCTION)
		(ARGLIST (FDEFINITION FUNCTION) REAL-FLAG))
	       (T (FERROR NIL "~S not a recognized function" FUNCTION))))
	((= TYPE DTP-STACK-GROUP) '(STACK-GROUP-ARG))
	((ARRAYP FUNCTION)
	 (DO ((I (%P-LDB %%ARRAY-NUMBER-DIMENSIONS FUNCTION) (1- I))
	      (L NIL))
	     ((<= I 0) L)
	   (SETQ L (CONS (FORMAT NIL "DIM-~D" I) L))))
	((OR (CLOSUREP FUNCTION) (ENTITYP FUNCTION))
	 (ARGLIST (CAR (%MAKE-POINTER DTP-LIST FUNCTION)) REAL-FLAG))
	((OR (= TYPE DTP-SELECT-METHOD) (= TYPE DTP-INSTANCE))
	 '(KEYWORD &REST SELECT-METHOD-ARGS-VARY))	;Can't tell arglist, shouldn't give error though
	((= TYPE DTP-FEF-POINTER )
	 (SETQ DEBUG-INFO (FUNCTION-DEBUGGING-INFO FUNCTION))
	 (SETQ ARG-MAP (CADR (ASSQ 'COMPILER:ARG-MAP DEBUG-INFO)))
	 (SETQ LOCAL-MAP (CADR (ASSQ 'COMPILER:LOCAL-MAP DEBUG-INFO)))
	 (VALUES
	   (COND ((AND (NOT REAL-FLAG)
		       ;Take this check out someday after everything recompiled
		       (LET ((AL (CDR (ASSQ 'ARGLIST DEBUG-INFO))))
			 (IF (OR (ATOM AL) (ATOM (CAR AL))) AL		;New format
			     (CAR AL)))))		;Old format
		 ((SETQ TEM (GET-MACRO-ARG-DESC-POINTER FUNCTION))
		  (DO ((ADL TEM (CDR ADL))
		       (ARGNUM 0 (1+ ARGNUM))
		       (ARGNAME)
		       (OPTIONALP NIL)
		       (QUOTEP NIL)
		       (DES-DT FEF-DT-DONTCARE)
		       (SPECIAL FEF-LOCAL)
		       (INIT)
		       (INITP T T)
		       (ADLWORD)
		       (ARGLIS NIL))
		      ((NULL ADL)
		       (NREVERSE ARGLIS))
		    (SETQ ADLWORD (CAR ADL))
		    (SELECT
		      (MASK-FIELD %%FEF-ARG-SYNTAX ADLWORD)
		      (FEF-ARG-REQ
		       (AND OPTIONALP
			    (FERROR NIL "Required args after optionals in ~S" FUNCTION)))
		      (FEF-ARG-OPT (OR OPTIONALP (SETQ ARGLIS (CONS '&OPTIONAL
								    ARGLIS)))
				   (SETQ OPTIONALP T))
		      (FEF-ARG-REST (SETQ ARGLIS (CONS '&REST ARGLIS)))
		      (OTHERWISE (RETURN (NREVERSE ARGLIS))))
		    (SELECT (MASK-FIELD %%FEF-QUOTE-STATUS ADLWORD)
		      (FEF-QT-QT (OR QUOTEP (SETQ ARGLIS (CONS '&QUOTE ARGLIS)))
				 (SETQ QUOTEP T))
		      (FEF-QT-EVAL (AND QUOTEP (SETQ ARGLIS (CONS '&EVAL
								  ARGLIS)))
				   (SETQ QUOTEP NIL)))
		    (SETQ TEM (LDB %%FEF-DES-DT ADLWORD))
		    (COND ((NEQ TEM DES-DT)
			   (SETQ DES-DT TEM)
			   (SETQ ARGLIS (CONS (NTH TEM '(&DT-DONTCARE &DT-NUMBER 
								      &DT-FIXNUM &DT-SYMBOL 
								      &ATOM &LIST &DT-FRAME))
					      ARGLIS))))
		    (SETQ TEM (LDB %%FEF-SPECIAL-BIT ADLWORD))	;handle remote some time?
		    (COND ((NEQ TEM SPECIAL)
			   (SETQ SPECIAL TEM)
			   (SETQ ARGLIS (CONS (NTH TEM '(&LOCAL &SPECIAL))
					      ARGLIS))))
		    (SETQ ARGNAME (COND ((= (LOGAND ADLWORD %FEF-NAME-PRESENT)
					    FEF-NM-YES)
					 (SETQ ADL (CDR ADL))
					 (CAR ADL))
					(T
					 (SETQ ARGNAME (COND (( (MASK-FIELD %%FEF-ARG-SYNTAX ADLWORD)
								 FEF-ARG-REST)
							      (NTH ARGNUM ARG-MAP))
							     (T (CAR LOCAL-MAP))))
					 (IF (SYMBOLP ARGNAME) ARGNAME (CAR ARGNAME)))))
		    (SELECT (MASK-FIELD %%FEF-INIT-OPTION ADLWORD)
		      (FEF-INI-NONE (SETQ INITP NIL))
		      (FEF-INI-NIL (SETQ INIT NIL))
		      (FEF-INI-PNTR (SETQ ADL (CDR ADL))
				    (SETQ INIT (LIST 'QUOTE (CAR ADL))))
		      (FEF-INI-C-PNTR
		       (SETQ ADL (CDR ADL))
		       (COND			;((= (%P-DATA-TYPE ADL) DTP-EXTERNAL-VALUE-CELL-POINTER)
						; (SETQ INIT			;THIS IS A BIT OF A KLUDGE
						;       (%FIND-STRUCTURE-HEADER (%P-CONTENTS-AS-LOCATIVE ADL))))
			 ((= (%DATA-TYPE (CAR ADL)) DTP-LOCATIVE)	;HOPE IT'S VALUE-CELL-LOCATION
			  (SETQ INIT (%FIND-STRUCTURE-HEADER (CAR ADL))))
			 ((SETQ INIT (CAAR ADL)))))
		      (FEF-INI-OPT-SA (SETQ ADL (CDR ADL))
				      (SETQ INIT '*HAIRY*))
		      (FEF-INI-COMP-C (SETQ INIT '*HAIRY*))
		      (FEF-INI-EFF-ADR (SETQ ADL (CDR ADL))
				       (SETQ INIT '*HAIRY*))
		      (FEF-INI-SELF (SETQ INIT ARGNAME)))
		    (SETQ ARGLIS (CONS (COND (INITP
					      (LIST ARGNAME INIT))
					     (T ARGNAME)) ARGLIS))))
		 (T ;No ADL.  Use the fast-arg-option to get the general pattern
						;and the argmap for the names.
		  (LET ((FAST-OPT (%ARGS-INFO FUNCTION))
			(RES NIL))
		    (LET ((MIN-ARGS (LDB %%ARG-DESC-MIN-ARGS FAST-OPT))
			  (MAX-ARGS (LDB %%ARG-DESC-MAX-ARGS FAST-OPT))
			  (EVALED-REST (LDB %%ARG-DESC-EVALED-REST FAST-OPT))
			  (QUOTED-REST (LDB %%ARG-DESC-QUOTED-REST FAST-OPT)))
		      (DOTIMES (I MIN-ARGS)
			(PUSH (CAAR ARG-MAP) RES)
			(SETQ ARG-MAP (CDR ARG-MAP)))
		      (OR (= MIN-ARGS MAX-ARGS)
			  (PUSH '&OPTIONAL RES))
		      (DOTIMES (I (- MAX-ARGS MIN-ARGS))
			(PUSH (CAAR ARG-MAP) RES)
			(SETQ ARG-MAP (CDR ARG-MAP)))
		      (OR (ZEROP QUOTED-REST)
			  (PUSH '&QUOTE RES))
		      (COND ((OR (NOT (ZEROP QUOTED-REST)) (NOT (ZEROP EVALED-REST)))
			     (PUSH '&REST RES)
			     (PUSH (CAAR LOCAL-MAP) RES)))
		      (NREVERSE RES)))))
	   ;Take this check out someday after everything recompiled
	   (LET ((RL (CDR (ASSQ 'RETURN-LIST DEBUG-INFO))))
	     (IF (ATOM (CAR RL)) RL		;New format
		 (CAR RL)))))			;Old format
	((= TYPE DTP-U-ENTRY)
	 (MICRO-CODE-ENTRY-ARGLIST-AREA (%POINTER FUNCTION)))
	(T (FERROR NIL "~S is not a function" FUNCTION))))

)
