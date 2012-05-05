;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-
;; These are the macros in the Lisp Machine system.
;; They used to be in LISPM;MACROS > but have been moved
;; for purposes of the cold load.

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

; Macros which do the equivalent of a displace MUST use DISPLACE
; to make sure that temporary area problems are worried about.

;The IF-IN-MACLISP/IF-IN-LISPM conditionals have to do with not breaking
;the Maclisp environment when compiling.  The optimizers in COMPAT take
;over these functions when compiling in Maclisp.

(DECLARE (SETQ INHIBIT-STYLE-WARNINGS-SWITCH T)
	 (SPECIAL COMPILING-FOR-LISPM))

;THESE ARE CONDITIONAL ON WHICH SYSTEM IS EXECUTING THEM.
(DEFMACRO IF-IN-MACLISP (&REST FORMS)
    (COND ((NOT (STATUS FEATURE LISPM))
	   `(PROGN 'COMPILE . ,FORMS))))

(DEFMACRO IF-IN-LISPM (&REST FORMS)
    (COND ((STATUS FEATURE LISPM)
	   `(PROGN 'COMPILE . ,FORMS))))

;THESE ARE CONDITIONAL ON WHICH SYSTEM RESULT IS INTENDED "FOR ".
; THIS IS THE SAME AS WHICH SYSTEM IS "IN" EXCEPT IN THE CASE
; COMPILING IN MACLISP FOR LISPM (IE QCMP, AFTER COMPILER ITSELF HAS
; BEEN LOADED).  THE COMPILING-FOR-LISPM SWITCH IS SET BY .LISP. (INIT)
; AFTER QCMP HAS BEEN LOADED.

(DEFMACRO IF-FOR-MACLISP (&REST FORMS)
    (COND ((AND (NOT (STATUS FEATURE LISPM))		;IN MACLISP
		(OR (NOT (BOUNDP 'COMPILING-FOR-LISPM))
		    (NULL COMPILING-FOR-LISPM)))
	   `(PROGN 'COMPILE . ,FORMS))))

(DEFMACRO IF-FOR-LISPM (&REST FORMS)
    (COND ((OR (STATUS FEATURE LISPM)
	       (AND (BOUNDP 'COMPILING-FOR-LISPM)
		    COMPILING-FOR-LISPM))
	   `(COMPILER-LET ((RUN-IN-MACLISP-SWITCH NIL))
			  (PROGN 'COMPILE . ,FORMS)))))

(DEFMACRO IF-FOR-MACLISP-ELSE-LISPM (MACLISP-FORM LISPM-FORM)
    (COND ((NOT (STATUS FEATURE LISPM))
	   (COND ((OR (NOT (BOUNDP 'COMPILING-FOR-LISPM))	;QCMP DEFINES THIS TO T
		      (NULL COMPILING-FOR-LISPM))
		  MACLISP-FORM)
		 (T `(COMPILER-LET ((RUN-IN-MACLISP-SWITCH NIL)) ,LISPM-FORM))))
    ;COMPLR DOESNT KNOW (OR CARE) ABOUT COMPILER-LET.
	  (T LISPM-FORM)))

(DEFMACRO SEND (OBJECT OPERATION &REST ARGUMENTS)
  `(FUNCALL ,OBJECT ,OPERATION . ,ARGUMENTS))

(DEFMACRO GETF (PLACE PROPERTY &OPTIONAL (DEFAULT NIL))
  `(OR (GET (LOCF ,PLACE) ,PROPERTY) ,DEFAULT))

;; Needed when conditionalizing something at top level with #Q or #M because
;; splicing readmacros flushed then.  #Q and #M now work at top level, so this
;; is for compatibility only.
(DEFMACRO NULL-MACRO (FORM) FORM)

;These must appear before anything in this file that uses LET in order to win
; at cold-load readin time.
#Q (PROGN 'COMPILE  ;Do not change this to IF-FOR-LISPM!!  that would lose because it
		    ; eventually expands into a LET.
;PUSH, POP, LET, LET* now exist in COMPLR and in ITS MacLisp.  -cwh

(DEFMACRO-DISPLACE PUSH (ITEM LIST)
   `(SETF ,LIST (CONS ,ITEM ,LIST)))

(DEFMACRO-DISPLACE POP (LIST &OPTIONAL DEST)
  `(PROG1 ,(COND ((NULL DEST)          
                  `(CAR ,LIST))
                 (T `(SETF ,DEST (CAR ,LIST))))
           (SETF ,LIST (CDR ,LIST))))
)

;;; (DEFSUBST FOO (X) (AR-1 X 5)) is like a similar DEFUN
;;; except that the definition of FOO will be substituted in at compile time
;;; and FOO's argument variables eliminated by substitution.
;;; It is your responsibility to make sure that FOO's args
;;; are evaluated exactly once, in the right ordr, in FOO's body,
;;; and that the symbols used for the args do not appear except
;;; to represent the args.
#Q
(DEFMACRO DEFSUBST (FUNCTION LAMBDA-LIST . BODY)
  (LET ((DEF1 `(NAMED-SUBST ,FUNCTION ,LAMBDA-LIST . ,BODY)))
    `(PROGN 'COMPILE
	    (EVAL-WHEN (COMPILE) (PUSH '(DEF ,FUNCTION . ,DEF1) LOCAL-DECLARATIONS))
	    (FSET-CAREFULLY ',FUNCTION ',DEF1))))

;Semi-adequate Maclisp version 
#M
(DEFMACRO DEFSUBST (FUNCTION LAMBDA-LIST . BODY)
  `(DEFUN ,FUNCTION MACRO (X)
     (SUBLIS (DO ((V ',LAMBDA-LIST (CDR V))
		  (X (CDR X) (CDR X))
		  (R NIL (CONS (CONS (CAR V) (CAR X)) R)))
		  ((NULL V) (NREVERSE R)))
	      '(PROGN . ,BODY))))

;This is for defstruct (or anything else which writes substs automatically
;as part of the expansion of some other form)
;PARENT is a list of the parent definition name and its definition type.
;Also accepted is a symbol, which is what it used to be (for old compiled defstructs).
;Note that this -does- -not- put the function on compiler:functions-defined, thus
;you get a warning if you fail to get open-coding due to a forward reference.
(DEFMACRO DEFSUBST-WITH-PARENT (FUNCTION PARENT LAMBDA-LIST . BODY)
  (OR (LISTP PARENT) (SETQ PARENT (LIST PARENT)))
  (LET ((DEF1 `(NAMED-SUBST (,FUNCTION (FUNCTION-PARENT . ,PARENT)) ,LAMBDA-LIST . ,BODY)))
    `(PROGN 'COMPILE
	    (EVAL-WHEN (COMPILE) (PUSH '(DEF ,FUNCTION . ,DEF1) LOCAL-DECLARATIONS))
	    (FSET-CAREFULLY ',FUNCTION ',DEF1))))

(DEFMACRO-DISPLACE @DEFINE (&REST IGNORE) NIL)

(DEFSUBST FIRST (LIST) (CAR LIST))

(DEFSUBST SECOND (LIST) (CADR LIST))

(DEFSUBST THIRD (LIST) (CADDR LIST))

(DEFSUBST FOURTH (LIST) (CADDDR LIST))

(DEFSUBST FIFTH (LIST) (CAR (CDDDDR LIST)))

(DEFSUBST SIXTH (LIST) (CADR (CDDDDR LIST)))

(DEFSUBST SEVENTH (LIST) (CADDR (CDDDDR LIST)))

(DEFSUBST REST1 (LIST) (CDR LIST))

(DEFSUBST REST2 (LIST) (CDDR LIST))

(DEFSUBST REST3 (LIST) (CDDDR LIST))

(DEFSUBST REST4 (LIST) (CDDDDR LIST))

(DEFSUBST NEQ (X Y) (NOT (EQ X Y)))

(DEFSUBST / (X Y) (NOT (= X Y)))

(DEFSUBST BIT-TEST (BITS WORD)
   (NOT (ZEROP (LOGAND BITS WORD))))

(DEFSUBST LDB-TEST (PPSS WORD)
   (NOT (ZEROP (LDB PPSS WORD))))

(IF-IN-LISPM
(DEFMACRO-DISPLACE CATCH (BODY TAG)
    `(*CATCH ',TAG ,BODY))
   )

(IF-IN-LISPM
(DEFMACRO-DISPLACE THROW (BODY TAG)
    `(*THROW ',TAG ,BODY))
   )

(IF-IN-LISPM
(DEFMACRO-DISPLACE ERRSET (BODY &OPTIONAL (PRINTFLAG T))
    `(LET ((EH:ERRSET-STATUS T)
	   (EH:ERRSET-PRINT-MSG ,PRINTFLAG))
	  (*CATCH 'EH:ERRSET-CATCH (LIST ,BODY))))
)
(IF-IN-LISPM 
(DEFMACRO-DISPLACE ERR (&OPTIONAL VALUE-FORM FLAG)
    (COND (FLAG (ERROR "ERR with two arguments is not implemented"))
	  ((NULL VALUE-FORM) '(ERROR ""))
	  (T `(COND (EH:ERRSET-STATUS (*THROW 'EH:ERRSET-CATCH ,VALUE-FORM))
		    (T (ERROR ""))))))
)

(DEFMACRO-DISPLACE CATCH-ERROR (BODY &OPTIONAL (PRINTFLAG T))
    `(LET ((EH:ERRSET-STATUS T)
	   (EH:ERRSET-PRINT-MSG ,PRINTFLAG))
       (*CATCH 'EH:ERRSET-CATCH ,BODY)))

(IF-IN-LISPM
(DEFMACRO-DISPLACE ARRAYCALL (IGNORE ARRAY &REST DIMS)
  `(FUNCALL ,ARRAY . ,DIMS))
)

(DEFMACRO-DISPLACE SELECTQ (TEST-OBJECT &BODY CLAUSES)
    (LET (TEST-EXP COND-EXP)
	 (SETQ TEST-EXP
	       (COND ((OR (ATOM TEST-OBJECT)
			  (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			       (ATOM (CADR TEST-OBJECT))))
		      TEST-OBJECT)
		     (T '*SELECTQ-ITEM*)))
	 (SETQ COND-EXP
	   (CONS 'COND
	    (MAPCAR (FUNCTION (LAMBDA (CLAUSE)
		       (COND ((OR (EQ (CAR CLAUSE) 'OTHERWISE)
				  (EQ (CAR CLAUSE) 'T)) ;Maclisp compatibility
			      (CONS T (CDR CLAUSE)))
			     ((ATOM (CAR CLAUSE))
			      `((EQ ,TEST-EXP ',(CAR CLAUSE)) . ,(CDR CLAUSE)))
			     (T
			      `((MEMQ ,TEST-EXP ',(CAR CLAUSE)) . ,(CDR CLAUSE))))))
		    CLAUSES)))
	 (COND ((EQ TEST-EXP TEST-OBJECT) COND-EXP)
	       (T
		`(LET ((*SELECTQ-ITEM* ,TEST-OBJECT))
		      ,COND-EXP)))))

;;;This should be sufficient for the moment:
(DEFMACRO-DISPLACE CASEQ (TEST-OBJECT &BODY CLAUSES)
  `(SELECTQ ,TEST-OBJECT ,@CLAUSES))

(DEFMACRO-DISPLACE SELECT (TEST-OBJECT &BODY CLAUSES)
    (LET (TEST-EXP COND-EXP)
	 (SETQ TEST-EXP
	       (COND ((OR (ATOM TEST-OBJECT)
			  (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			       (ATOM (CADR TEST-OBJECT))))
		      TEST-OBJECT)
		     (T '*SELECTQ-ITEM*)))
	 (SETQ COND-EXP
	   (CONS 'COND
	    (MAPCAR (FUNCTION (LAMBDA (CLAUSE)
		       (COND ((OR (EQ (CAR CLAUSE) 'OTHERWISE)
				  (EQ (CAR CLAUSE) 'T)) ;Maclisp compatibility
			      (CONS T (CDR CLAUSE)))
			     ((ATOM (CAR CLAUSE))
			      `((EQ ,TEST-EXP ,(CAR CLAUSE)) . ,(CDR CLAUSE)))
			     (T
			      `((OR . ,(MAPCAR (FUNCTION (LAMBDA (FORM)
					          `(EQ ,TEST-EXP ,FORM)))
					       (CAR CLAUSE)))
				. ,(CDR CLAUSE))))))
		    CLAUSES)))
	 (COND ((EQ TEST-EXP TEST-OBJECT) COND-EXP)
	       (T
		`(LET ((*SELECTQ-ITEM* ,TEST-OBJECT))
		      ,COND-EXP)))))

(DEFMACRO-DISPLACE SELECTOR (TEST-OBJECT TEST-FUNCTION &BODY CLAUSES)
    (LET (TEST-EXP COND-EXP)
	 (SETQ TEST-EXP
	       (COND ((OR (ATOM TEST-OBJECT)
			  (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			       (ATOM (CADR TEST-OBJECT))))
		      TEST-OBJECT)
		     (T '*SELECTQ-ITEM*)))
	 (SETQ COND-EXP
	   (CONS 'COND
	    (MAPCAR (FUNCTION (LAMBDA (CLAUSE)
		       (COND ((OR (EQ (CAR CLAUSE) 'OTHERWISE)
				  (EQ (CAR CLAUSE) 'T)) ;Maclisp compatibility
			      (CONS T (CDR CLAUSE)))
			     ((ATOM (CAR CLAUSE))
			      `((,TEST-FUNCTION ,TEST-EXP ,(CAR CLAUSE)) . ,(CDR CLAUSE)))
			     (T
			      `((OR . ,(MAPCAR (FUNCTION (LAMBDA (FORM)
					          `(,TEST-FUNCTION ,TEST-EXP ,FORM)))
					       (CAR CLAUSE)))
				. ,(CDR CLAUSE))))))
		    CLAUSES)))
	 (COND ((EQ TEST-EXP TEST-OBJECT) COND-EXP)
	       (T
		`(LET ((*SELECTQ-ITEM* ,TEST-OBJECT))
		      ,COND-EXP)))))

;EVENTUALLY THE MICRO COMPILER SHOULD BE AWARE OF THIS
(DEFMACRO-DISPLACE DISPATCH (PPSS WORD &BODY BODY)
  `(LET ((.DISPATCH-INTERNAL. (LDB ,PPSS ,WORD)))
     (COND ,@(MAPCAR #'(LAMBDA (CLAUSE)
			 `(,(COND ((EQ (CAR CLAUSE) 'OTHERWISE)
				   'T)
				  ((ATOM (CAR CLAUSE))
				   `(= .DISPATCH-INTERNAL. ,(CAR CLAUSE)))
				  (T
				   `(OR ,@(MAPCAR #'(LAMBDA (ITEM)
						      `(= .DISPATCH-INTERNAL. ,ITEM))
						  (CAR CLAUSE)))))
			   . ,(CDR CLAUSE)))
		     BODY))))

(DEFMACRO-DISPLACE EVERY (LIST PRED &OPTIONAL (STEP ''CDR))
   `(DO ((*L* ,LIST (FUNCALL ,STEP *L*)))
	((NULL *L*) T)
      (OR (FUNCALL ,PRED (CAR *L*)) (RETURN NIL))))

(DEFMACRO-DISPLACE SOME (LIST PRED &OPTIONAL (STEP ''CDR))
   `(DO ((*L* ,LIST (FUNCALL ,STEP *L*)))
	((NULL *L*) NIL)
      (AND (FUNCALL ,PRED (CAR *L*)) (RETURN *L*))))

;;;  LET-GLOBALLY IS SIMILAR TO LET, EXCEPT THAT THE BINDING APPLIES
;;;  TO THE WHOLE WORLD, NOT JUST THE CURRENTLY-EXECUTING STACK GROUP.
;;;  FOR THE MOMENT, ANYWAY, IT IS IMPLEMENTED USING UNWIND-PROTECT.
(DEFMACRO-DISPLACE LET-GLOBALLY (VARLIST &BODY BODY)
  (LET ((VARS (MAPCAR '(LAMBDA (V) (COND ((ATOM V) V) (T (CAR V)))) VARLIST))
	(VALS (MAPCAR '(LAMBDA (V) (COND ((ATOM V) NIL) (T (CADR V)))) VARLIST))
	(GENVARS (MAPCAR '(LAMBDA (IGNORE) (GENSYM)) VARLIST)))
     `(LET ,(MAPCAR 'LIST GENVARS VARS)
        (UNWIND-PROTECT (PROGN (SETQ . ,(MAPCAN 'LIST VARS VALS))
			       . ,BODY)
			(SETQ . ,(MAPCAN 'LIST VARS GENVARS))))))

;;; DEFUNP is like DEFUN but provides an implicit PROG.
;;; However, the value on falling off the end is the last thing in the body.

(DEFMACRO DEFUNP (FUNCTION ARGS &REST BODY)
  (LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)
	(LAST NIL)
	(DECLARES NIL))
    (SETQ BODY (APPEND BODY NIL))
    (SETQ LAST (LAST BODY))
    (DO () (())
      (IF (OR (NULL BODY) (EQ BODY LAST))
	  (RETURN))
      (LET ((FORM (CAR BODY)))
	(COND ((AND (NOT (ATOM FORM))
		    (EQ (CAR FORM) 'DECLARE))
	       (PUSH FORM DECLARES)
	       (POP BODY))
	      (T (RETURN)))))
    (COND ((OR (ATOM (CAR LAST)) (NOT (EQ 'RETURN (CAAR LAST))))
	   (RPLACA LAST (LIST 'RETURN (CAR LAST)))))
    `(DEFUN ,FUNCTION ,ARGS
       ,@(NREVERSE DECLARES)
       (PROG () . ,BODY))))

(DEFMACRO-DISPLACE CATCH-ALL BODY
   `(*CATCH NIL (PROGN . ,BODY)))

;;; (IF test then-action else-action)
(DEFMACRO-DISPLACE IF (TEST THEN &REST ELSES)
   (COND ((NULL TEST) (AND ELSES `(PROGN . ,ELSES)))	;macros can generate this case...
	 ((EQ TEST T) THEN)			;and this one (avoids compiler error msg)
	 (T `(COND (,TEST ,THEN) (T . ,(OR ELSES '(NIL)))))))

;;; (WHEN pred {form}*)
(DEFMACRO WHEN (PRED &BODY BODY)
  `(AND ,PRED (PROGN ,@BODY)))

;;; (UNLESS pred {form}*)
(DEFMACRO UNLESS (PRED &BODY BODY)
  `(IF ,PRED () ,@BODY))

;;; (CHECK-ARG STRING STRINGP "a string") signals an error if STRING is not a string.
;;; The error signals condition :WRONG-TYPE-ARGUMENT with arguments
;;; which are STRINGP (the predicate), the value of STRING (the losing value),
;;; the name of the argument (STRING), and the string "a string".
;;; If you try to proceed and do not supply a valid string to replace it,
;;; the error happens again.
;;; The second form may be the name of a predicate function, or it may be a full
;;; predicate form, as in:
;;; (CHECK-ARG A (AND (NUMBERP A) (< A 10.) (> A 0.)) "a number from one to ten" ONE-TO-TEN)
;;; ONE-TO-TEN is a symbol for the "type" which the argument failed to be.
;;; It is used instead of the second argument (the predicate) when signalling the error,
;;; since the second argument is not a suitable symbol.
;;; The value returned by CHECK-ARG is the argument's (original or respecified) value.
;;; In general, the condition :WRONG-TYPE-ARGUMENT is signalled with arguments
;;;    (1) A symbol for the desired type (NIL if not supplied)
;;;    (2) The bad value
;;;    (3) The name of the argument
;;;    (4) A string for the desired type.
(DEFMACRO-DISPLACE CHECK-ARG (ARG-NAME PREDICATE TYPE-STRING &OPTIONAL ERROR-TYPE-NAME)
    (AND (NULL ERROR-TYPE-NAME)
	 (SYMBOLP PREDICATE)
	 (SETQ ERROR-TYPE-NAME PREDICATE))
    `(DO () (,(COND ((SYMBOLP PREDICATE)
                     `(,PREDICATE ,ARG-NAME))
                    (T PREDICATE))
             ,ARG-NAME)
	 (SETQ ,ARG-NAME
	       (CERROR T NIL ':WRONG-TYPE-ARGUMENT
		       "The argument ~2G~A was ~1G~S, which is not ~3G~A"
		       ',ERROR-TYPE-NAME ,ARG-NAME ',ARG-NAME ',TYPE-STRING))))

;;; (CHECK-ARG-TYPE X FIXNUM) signals an error if (TYPEP X 'FIXNUM) is not true.
(DEFMACRO CHECK-ARG-TYPE (ARG-NAME TYPE &OPTIONAL TYPE-STRING)
  (IF (NULL TYPE-STRING)
      (SETQ TYPE-STRING
	    (COND ((AND (SYMBOLP TYPE)
			(GET TYPE 'SI:TYPE-NAME)))
		  (T
		   (STRING-APPEND "a " (STRING-DOWNCASE (FORMAT NIL "~A" TYPE)))))))
  `(DO () ((TYPEP ,ARG-NAME ',TYPE))
     (SETQ ,ARG-NAME
	   (CERROR T NIL ':WRONG-TYPE-ARGUMENT
		   "The argument ~2G~A was ~1G~S, which is not ~3G~A"
		   ',TYPE ,ARG-NAME ',ARG-NAME ',TYPE-STRING))))

;;; (KEYWORD-EXTRACT <keylist> KEY (FOO (UGH BLETCH) BAR) (FLAG FALG) <otherwise> ...)
;;; parses a TV-DEFINE-PC-PPR style list of alternating keywords and values, <keylist>.
;;; The symbol KEY is bound internally to remaineder of the keyword list.
;;; The keywords recognized are :FOO, :BAR and UGH;  whatever follows
;;; the keyword UGH is put in the variable BLETCH, whatever follows the
;;; keyword :FOO is put in the variable FOO, and similar for BAR.
;;; The flags are :FLAG and :FALG;  if :FLAG is seen, FLAG is set to T.
;;; <otherwise> is one or more SELECTQ clauses which can be used
;;; to recognize whatever else you like, in nonstandard format.
;;; To gobble the next thing from the <keylist>, say (CAR (SETQ KEY (CDR KEY))).
(DEFMACRO-DISPLACE KEYWORD-EXTRACT (KEYLIST KEYVAR KEYWORDS &OPTIONAL FLAGS &BODY OTHERWISE)
    `(DO ((,KEYVAR ,KEYLIST (CDR ,KEYVAR)))
	 ((NULL ,KEYVAR))
       (SELECTQ (CAR ,KEYVAR)
	   ,@(MAPCAR (FUNCTION (LAMBDA (KEYWORD)
				 (COND ((ATOM KEYWORD)
					`(,(INTERN (STRING KEYWORD) "USER")
					  (SETQ ,KEYWORD (CAR (SETQ ,KEYVAR (CDR ,KEYVAR))))))
				       (T `(,(CAR KEYWORD)
					    (SETQ ,(CADR KEYWORD)
						  (CAR (SETQ ,KEYVAR (CDR ,KEYVAR)))))))))
		     KEYWORDS)
	   ,@(MAPCAR (FUNCTION (LAMBDA (KEYWORD)
				 (COND ((ATOM KEYWORD)
					`(,(INTERN (STRING KEYWORD) "USER")
					  (SETQ ,KEYWORD T)))
				       (T `(,(CAR KEYWORD)
					    (SETQ ,(CADR KEYWORD) T))))))
		     FLAGS)
	   ,@OTHERWISE
	   (OTHERWISE
	    (FERROR NIL "~S is not a recognized keyword" (CAR ,KEYVAR))))))

;;; PSETQ looks like SETQ but does its work in parallel.
;;; Note that the return value of PSETQ is -not- guaranteed.
(DEFMACRO-DISPLACE PSETQ (&REST REST)
  ;; To improve the efficiency of do-stepping, by using the SETE-CDR, SETE-CDDR,
  ;; SETE-1+, and SETE-1- instructions, we try to do such operations with SETQ
  ;; rather than PSETQ.  To avoid having to do full code analysis, never rearrange
  ;; the order of any code when doing this, and only do it when there are no
  ;; variable name duplications.
  (LOOP FOR (VAL VAR) ON (REVERSE REST) BY 'CDDR
	WITH SETQS = NIL WITH PSETQS = NIL
	DO (IF (AND (NULL PSETQS)
		    (LISTP VAL)
		    (MEMQ (CAR VAL) '(1+ 1- CDR CDDR))
		    (EQ (CADR VAL) VAR)
		    (NOT (MEMQ VAR SETQS)))
	       (SETQ SETQS (CONS VAR (CONS VAL SETQS)))
	       (SETQ PSETQS (CONS VAR (CONS VAL PSETQS))))
	FINALLY
	  (SETQ PSETQS (PSETQ-PROG1IFY PSETQS))
	  (RETURN (COND ((NULL SETQS) PSETQS)
			((NULL PSETQS) (CONS 'SETQ SETQS))
			(T `(PROGN ,PSETQS (SETQ . ,SETQS)))))))

(DEFUN PSETQ-PROG1IFY (X)
  (COND ((NULL X) NIL)
	((NULL (CDDR X)) (CONS 'SETQ X))
	(T `(SETQ ,(CAR X) (PROG1 ,(CADR X) ,(PSETQ-PROG1IFY (CDDR X)))))))

(DEFPROP MULTIPLE-VALUE-CALL COMPILER:OBSOLETE COMPILER:STYLE-CHECKER)
;;; THIS FUNCTION IS OBSOLETE.
;;; For things which want to do a tail-recursive call, passing back multiple
;;; values.  This does not work in the interpreter.  This was a temporary measure
;;; and should go away when the calling protocol is changed to always pass back
;;; multiple values on "tail recursive" calls.
(DEFMACRO-DISPLACE MULTIPLE-VALUE-CALL ((FUNCTION . ARGS))
  `(PROGN (%OPEN-CALL-BLOCK (FUNCTION ,FUNCTION) 0 4) ;No ADI, destination-return
	  (%ASSURE-PDL-ROOM ,(LENGTH ARGS))
	  ,@(MAPCAR '(LAMBDA (A) `(%PUSH ,A)) ARGS)
	  (%ACTIVATE-OPEN-CALL-BLOCK)))

;;; (LOCAL-DECLARE ((SPECIAL FOO) (UNSPECIAL BAR)) code)
;;; declares FOO and BAR locally within <code>.
;;; LOCAL-DECLARE can also be used by macros to pass information down
;;; to other macros that expand inside the code they produce.
;;; The list of declarations (in this case, ((MUMBLE FOO BAR))) is appended
;;; onto the front of LOCAL-DECLARATIONS, which can be searched by
;;; macros expending inside of <code>.
(DEFMACRO-DISPLACE LOCAL-DECLARE (DECLARATIONS &BODY BODY)
    `(COMPILER-LET ((LOCAL-DECLARATIONS (APPEND ',DECLARATIONS LOCAL-DECLARATIONS)))
		   . ,BODY))

;;; INHIBIT-STYLE-WARNINGS inhibits compiler style checking of what is inside it.
;;; In the interpreter, it is a no-op.
(DEFMACRO-DISPLACE INHIBIT-STYLE-WARNINGS (BODY)
    BODY)

;;; (ERROR-RESTART .... (CERROR ...) ...) causes a request by the user
;;; or error handler to "restart" after the error to re-execute all the
;;; code inside the ERROR-RESTART.
(DEFMACRO-DISPLACE ERROR-RESTART (&REST BODY)
   `(PROG ()
	  LOOP
	  (*CATCH 'ERROR-RESTART (RETURN (PROGN . ,BODY)))
	  (GO LOOP)))

;;; (LET-CLOSED (variables as in LET) initializations ... (FUNCTION ..))
;;; binds the variables and executes the initialization,
;;; then returns the last thing in the body, closed over those variables.
(DEFMACRO-DISPLACE LET-CLOSED (VARS &BODY BODY)
    (LET ((VARNAMES (MAPCAR (FUNCTION (LAMBDA (V) (COND ((ATOM V) V) (T (CAR V))))) VARS)))
	 `(LOCAL-DECLARE ((SPECIAL . ,VARNAMES))
		 (LET ,VARS
		      (CLOSURE ',VARNAMES (PROGN . ,BODY))))))

;;; (DEF-OPEN-CODED FOO-COMPONENT (CURRY-AFTER AR-1 5))
;;; defines FOO-COMPONENT as an open-coded function with that definition.
(DEFMACRO DEF-OPEN-CODED (FUNCTION DEFINITION)
    `(PROGN 'COMPILE
	    (EVAL-WHEN (COMPILE)
		       (PUSH '(OPEN-CODE ,FUNCTION ,DEFINITION) LOCAL-DECLARATIONS))
	    (FSET-CAREFULLY ',FUNCTION ',DEFINITION)
	    (DEFPROP ,FUNCTION T OPEN-CODE)))

;;; Say that FUNCTION should be open-coded by the compiler as DEFINITION
;;; without changing FUNCTION's real definition.
;;; A call to this OPEN-CODE can be used as a local declaration, too.
;;; Giving NIL as the definition turns off open-coding.
(DEFMACRO OPEN-CODE (FUNCTION DEFINITION)
    `(PROGN 'COMPILE
	    (EVAL-WHEN (COMPILE)
		       (PUSH '(OPEN-CODE ,FUNCTION ,DEFINITION) LOCAL-DECLARATIONS))
	    (DEFPROP ,FUNCTION ,DEFINITION OPEN-CODE)))

;;; Make a variable special and, optionally, initialize it.
;;; This is recorded as a definition by TAGS and ZWEI.
(DEFMACRO DEFVAR (VARIABLE &OPTIONAL (INITIAL-VALUE NIL INITIALIZE-P)
			             (DOCUMENTATION "" DOCUMENTATION-P))
  `(PROGN 'COMPILE
	(SPECIAL ,VARIABLE)
	(RECORD-SOURCE-FILE-NAME ',VARIABLE 'DEFVAR)
	,(IF INITIALIZE-P
	     ;; Initialize in a way that works in the cold-load
	     ;; Don't evaluate INITIAL-VALUE unless used
	     `(,(IF FS:THIS-IS-A-PATCH-FILE 'SETQ 'SETQ-IF-UNBOUND)
	       ,VARIABLE ,INITIAL-VALUE))
	,(IF DOCUMENTATION-P
	     `(PUTPROP ',VARIABLE ',DOCUMENTATION ':VALUE-DOCUMENTATION))))

;;; Similar to DEFVAR, but if initialization given, always use it (not just if
;;; variable was previously unbound).
(DEFMACRO DEFCONST (VARIABLE &OPTIONAL (INITIAL-VALUE NIL INITIALIZE-P)
			               (DOCUMENTATION "" DOCUMENTATION-P))
  `(PROGN 'COMPILE
	(SPECIAL ,VARIABLE)
	(RECORD-SOURCE-FILE-NAME ',VARIABLE 'DEFVAR)
	,(AND INITIALIZE-P    
	      ;Initialize in a way that works in the cold-load
	      ;Don't evaluate INITIAL-VALUE unless used
	      `(SETQ ,VARIABLE ,INITIAL-VALUE))
	,(IF DOCUMENTATION-P
	     `(PUTPROP ',VARIABLE ',DOCUMENTATION ':VALUE-DOCUMENTATION))))

;;; Performs a sequence of operations while inhibiting scheduling
(DEFMACRO-DISPLACE WITHOUT-INTERRUPTS (&REST FORMS)
  `(LET ((INHIBIT-SCHEDULING-FLAG T))
     . ,FORMS))

;;; MAPC with a prog body instead of a function.  <form> evaluates to a list,
;;; and <body> is executed with <var> bound to successive elements of the list.
(DEFMACRO-DISPLACE DOLIST ((VAR FORM) &BODY BODY)
  `(DO ((.DOLIST-INTERNAL. ,FORM (CDR .DOLIST-INTERNAL.))
	(,VAR))
       ((NULL .DOLIST-INTERNAL.))
     (SETQ ,VAR (CAR .DOLIST-INTERNAL.))
     . ,BODY))

;;; Repeat a number of times.  <form> evaluates to the number of times,
;;; and <body> is executed with <var> bound to 0, 1, ...
;;; Don't generate dummy variable if <form> is an integer.  We could also do this
;;; if <form> were a symbol, but the symbol may get clobbered inside the body,
;;; so the behavior of the macro would change.
(DEFMACRO-DISPLACE DOTIMES ((VAR FORM) &BODY BODY)
  (COND ((FIXP FORM)
	 `(DO ((,VAR 0 (1+ ,VAR)))
	      (( ,VAR ,FORM))
	    . ,BODY))
	(T `(DO ((,VAR 0 (1+ ,VAR))
		 (.DOTIMES-INTERNAL. ,FORM))
		(( ,VAR .DOTIMES-INTERNAL.))
	      . ,BODY))))

(DEFMACRO DO-FOREVER (&BODY BODY)
  `(DO ()
       (())
     . ,BODY))

;;; Execute body with a stream open.  Abnormal exit aborts the file (if it's an output file).
(DEFMACRO-DISPLACE WITH-OPEN-STREAM ((STREAM CONSTRUCTION-FORM) &BODY BODY)
  `(LET ((,STREAM NIL)
	 (.FILE-ABORTED-FLAG. ':ABORT))
     (UNWIND-PROTECT
       (PROG2 (SETQ ,STREAM ,CONSTRUCTION-FORM)
	      (PROGN . ,BODY)
	      (SETQ .FILE-ABORTED-FLAG. NIL))
       (AND ,STREAM (NOT (STRINGP ,STREAM))
	    (FUNCALL ,STREAM ':CLOSE .FILE-ABORTED-FLAG.)))))

;;; Execute body with a file open.
(DEFMACRO-DISPLACE WITH-OPEN-FILE ((STREAM FILENAME . OPTIONS) &BODY BODY)
  `(WITH-OPEN-STREAM (,STREAM (OPEN ,FILENAME . ,OPTIONS))
     . ,BODY))

;;; Create code that is body, possibly with a lambda wrapped around it to make
;;; sure that the forms assigned to the listed variables only get evaluated once.
(DEFMACRO ONCE-ONLY (VARIABLE-LIST &BODY BODY)
  (DOLIST (VARIABLE VARIABLE-LIST)
    (IF (NOT (SYMBOLP VARIABLE))
	(FERROR NIL "~S is not a variable" VARIABLE)))
  (LET ((BIND-VARS (GENSYM))
	(BIND-VALS (GENSYM))
	(TEM (GENSYM)))
    `(LET ((,BIND-VARS NIL)
	   (,BIND-VALS NIL))
       (LET ((RESULT ((LAMBDA ,VARIABLE-LIST . ,BODY)
		      . ,(LOOP FOR VARIABLE IN VARIABLE-LIST
			       COLLECT `(IF (OR (ATOM ,VARIABLE)
						(EQ (CAR ,VARIABLE) 'QUOTE))
					    ,VARIABLE
					    (LET ((,TEM (GENSYM)))
					      (PUSH ,TEM ,BIND-VARS)
					      (PUSH ,VARIABLE ,BIND-VALS)
					      ,TEM))))))
	 (IF (NULL ,BIND-VARS)
	     RESULT
	     `((LAMBDA ,,BIND-VARS ,RESULT) . ,,BIND-VALS))))))

;Bind NAME-TO-BIND to a cleanup-list,
;and on exit do any cleanup-actions stored in the list.
;The body can pass NAME-TO-BIND to various allocation functions,
;which will attach cleanups to the car of the cleanup-list
;so that the objects they allocate will be returned.
;A cleanup is just a list of a function and (evaluated) args.
(DEFMACRO WITH-CLEANUP-LIST (NAME-TO-BIND &BODY BODY)
  `(LET ((,NAME-TO-BIND (LIST NIL)))
     (UNWIND-PROTECT (PROGN . ,BODY)
		     (MAPC 'FUNCALL (CAR ,NAME-TO-BIND)))))

;Move a specific cleanup action from one cleanup-list to another, atomically.
(DEFUN MOVE-CLEANUP (CLEANUP FROM-CLEANUP-LIST TO-CLEANUP-LIST)
  (WITHOUT-INTERRUPTS
    (SETF (CAR FROM-CLEANUP-LIST) (DELQ CLEANUP (CAR FROM-CLEANUP-LIST)))
    (PUSH CLEANUP (CAR TO-CLEANUP-LIST))))

;Replace one cleanup with another, atomically.
(DEFUN REPLACE-CLEANUP (OLD-CLEANUP NEW-CLEANUP CLEANUP-LIST)
  (WITHOUT-INTERRUPTS
    (SETF (CAR CLEANUP-LIST) (CONS NEW-CLEANUP (DELQ OLD-CLEANUP (CAR CLEANUP-LIST))))))
