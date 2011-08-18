;  -*-Package:COMPILER; Mode:LISP-*-
;This file contains pass 1 and the top level of the Lisp machine Lisp compiler

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;     "This is insane.  What we clearly want to do is not completely
;;      clear, and is rooted in NCOMPLR."   -- BSG/Dissociated Press.

(DECLARE (SPECIAL MC-HOLDPROG ULAP-DEBUG LAP-DEBUG QC-PREVIOUS-CONS-AREA
		  MS-HOLDPROG MSLAP-DEBUG FUNCTIONS-DEFINED
		  TRACE-TABLE QC-TF-PROCESSING-MODE QC-TF-OUTPUT-MODE))

(DECLARE (*EXPR MEMQL BUTLAST ADRREFP LDIFF GETARGDESC P2 P2SBIND
		OUTF BARF ULAP MICRO-COMPILE QLAPP))

;Initialize all global variables and compiler switches, and make sure
;that some built in variables are known to be special
;(logically, the cold load would contain SPECIAL properties for them,
;but this function is how they actually get put on).
(DEFUN QC-PROCESS-INITIALIZE NIL 
  (SETQ HOLDPROG T)
  (SETQ MC-HOLDPROG T)
  (SETQ ULAP-DEBUG NIL)
  (SETQ LAP-DEBUG NIL)
  (SETQ MS-HOLDPROG T)
  (SETQ MSLAP-DEBUG NIL)
  (SETQ FUNCTION-BEING-PROCESSED NIL)	;FOR ERROR PRINTOUTS.  AVOID ANY UNBOUND PROBLEMS
  (SETQ MACROLIST NIL)
  (SETQ OPEN-CODE-MAP-SWITCH T)
  (SETQ ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH NIL)
  (SETQ ALL-SPECIAL-SWITCH NIL)
  (SETQ OBSOLETE-FUNCTION-WARNING-SWITCH T)
  (SETQ RUN-IN-MACLISP-SWITCH NIL)
  (SETQ INHIBIT-STYLE-WARNINGS-SWITCH NIL)
  (OR (BOUNDP 'QCOMPILE-TEMPORARY-AREA)
      (SETQ QCOMPILE-TEMPORARY-AREA FASD-TEMPORARY-AREA))
  (OR (FBOUNDP 'QCMP-OUTPUT)
      (FSET 'QCMP-OUTPUT
	    (MAKE-ARRAY 2000 ':AREA WORKING-STORAGE-AREA
			':TYPE 'ART-Q-LIST
			':LEADER-LIST '(0))))
  (IF-FOR-LISPM
    (COND ((NULL (GET 'CDR-NIL 'SYSTEM-CONSTANT))
	   (MAPC (FUNCTION (LAMBDA (Y) 
			     (MAPC (FUNCTION (LAMBDA (X) 
					       (PUTPROP X T 'SYSTEM-CONSTANT)))
				   (SYMEVAL Y)) )) 
		 SYSTEM-CONSTANT-LISTS)
	   (MAPC (FUNCTION (LAMBDA (Y) 
			     (MAPC (FUNCTION (LAMBDA (X) 
					       (PUTPROP X T 'SPECIAL)))
				   (SYMEVAL Y)) )) 
		 SYSTEM-VARIABLE-LISTS))))
  )

;; Compile a function which already has an interpreted definition,
;; or define it to a newly supplied definition's compilation.
;; If the definition is one which is legal but cannot meaningfully
;; be compiled, we just leave it unchanged.
(IF-FOR-LISPM
(DEFUN COMPILE (NAME &OPTIONAL LAMBDA-EXP)
  (AND QC-FILE-IN-PROGRESS	;Check for condition likely to cause temporary area lossage
       (FORMAT ERROR-OUTPUT "~&COMPILE: Compiler recursively entered, you may lose.~%"))
  (COMPILER-WARNINGS-CONTEXT-BIND
    (PROG (TEM LAST-ERROR-FUNCTION COMPILING-WHOLE-FILE-P)
      (QC-PROCESS-INITIALIZE)
      (RESET-TEMPORARY-AREA FASD-TEMPORARY-AREA)
      (SETQ COMPILING-WHOLE-FILE-P (ZWEI:SETUP-COMPILER-WARNINGS))
      (COND (LAMBDA-EXP)
	    ((AND (FDEFINEDP NAME)
		  (LISTP (SETQ TEM (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC NAME)))))
	     (SETQ LAMBDA-EXP TEM))
	    ((AND (SYMBOLP NAME)
		  (SETQ TEM (GET NAME ':PREVIOUS-EXPR-DEFINITION)))
	     (SETQ LAMBDA-EXP TEM))
	    (T (FERROR NIL "Can't find LAMBDA expression for ~S" NAME)))
      (COMPILE-1 NAME LAMBDA-EXP)
      (RETURN NAME))))
)

(IF-FOR-LISPM		;Compile while already inside compiler environment
(DEFUN COMPILE-1 (NAME LAMBDA-EXP &OPTIONAL (PROCESSING-MODE 'MACRO-COMPILE))
  (COND ((NLISTP LAMBDA-EXP)
	 (FDEFINE NAME LAMBDA-EXP T))
	((OR (MEMQ (CAR LAMBDA-EXP) '(LAMBDA NAMED-LAMBDA))
	     (AND (EQ (CAR LAMBDA-EXP) 'MACRO)
		  (LISTP (CDR LAMBDA-EXP))
		  (MEMQ (CADR LAMBDA-EXP) '(LAMBDA NAMED-LAMBDA))))
	 (QC-TRANSLATE-FUNCTION NAME LAMBDA-EXP PROCESSING-MODE 'COMPILE-TO-CORE))
	(T (FDEFINE NAME LAMBDA-EXP T))))
 )

;; Restore the saved old interpreted definition of a function on which
;; COMPILE was used.

(IF-FOR-LISPM 
(DEFUN UNCOMPILE (FUNCTION-SPEC &AUX OLD)
  (COND ((AND (SETQ OLD (SI:FUNCTION-SPEC-GET FUNCTION-SPEC ':PREVIOUS-EXPR-DEFINITION))
	      (NOT (AND (FDEFINEDP FUNCTION-SPEC)
			(LISTP (FDEFINITION FUNCTION-SPEC))
			(NOT (AND (EQ (CAR (FDEFINITION FUNCTION-SPEC)) 'MACRO)
				  (TYPEP (CDR (FDEFINITION FUNCTION-SPEC))
					 ':COMPILED-FUNCTION))))))
	 (FDEFINE FUNCTION-SPEC OLD T T))
	(T "Not compiled")))
)

;; Compile one function.  All styles of the compiler come through here.
;; QC-TF-PROCESSING-MODE should be MACRO-COMPILE or MICRO-COMPILE.
;; QC-TF-OUTPUT-MODE is used by LAP to determine where to put the compiled code.
;; It is COMPILE-TO-CORE for making an actual FEF, QFASL, REL, or
;; QFASL-NO-FDEFINE to simply dump a FEF without trying to define a function
;; (poor modularity).
;; EXP is the lambda-expression.
;; In MACRO-COMPILE mode, the return value is the value of QLAPP for the first
;; function.
(DEFUN QC-TRANSLATE-FUNCTION (FUNCTION EXP QC-TF-PROCESSING-MODE QC-TF-OUTPUT-MODE)
  (LET ((ERROR-MESSAGE-HOOK
	  (LET-CLOSED ((FUNCTION-BEING-PROCESSED FUNCTION))
	    #'(LAMBDA () (AND FUNCTION-BEING-PROCESSED
			      (FORMAT T "Error occurred while compiling ~S"
				      FUNCTION-BEING-PROCESSED)))))
	(QC-FUNCTIONS-TO-BE-TRANSLATED (NCONS (LIST FUNCTION EXP LOCAL-DECLARATIONS)))
	VAL
	THIS-FUNCTION-BARF-SPECIAL-LIST
	VARIABLES-LISTS)
    (DO ((L QC-FUNCTIONS-TO-BE-TRANSLATED (CDR L))
	 (QC-PREVIOUS-CONS-AREA DEFAULT-CONS-AREA)
	 (DEFAULT-CONS-AREA QCOMPILE-TEMPORARY-AREA)
	 (FUNCTION-BEING-PROCESSED)
	 (COMPILER-LEXICAL-ENVIRONMENT)
	 (EXP)
	 (LOCAL-DECLARATIONS))
	((NULL L))
      (STORE-ARRAY-LEADER 0 (FUNCTION QCMP-OUTPUT) 0)
      (SETF `(,FUNCTION-BEING-PROCESSED ,EXP ,LOCAL-DECLARATIONS ,COMPILER-LEXICAL-ENVIRONMENT) (CAR L))
      (PUSH (QCOMPILE0 EXP FUNCTION-BEING-PROCESSED (EQ QC-TF-PROCESSING-MODE 'MICRO-COMPILE))
	    VARIABLES-LISTS)
      (SETQ QCMP-OUTPUT (G-L-P (FUNCTION QCMP-OUTPUT)))
      (AND PEEP-ENABLE (PEEP QCMP-OUTPUT))
      (LET ((DEFAULT-CONS-AREA QC-PREVIOUS-CONS-AREA))
	(COND ((NULL HOLDPROG))
	      ((EQ QC-TF-PROCESSING-MODE 'MACRO-COMPILE)
	       (SETQ EXP (QLAPP QCMP-OUTPUT QC-TF-OUTPUT-MODE))
	       (OR VAL (SETQ VAL EXP)))
	      ((EQ QC-TF-PROCESSING-MODE 'MICRO-COMPILE)
	       (MICRO-COMPILE QCMP-OUTPUT QC-TF-OUTPUT-MODE)))))
    (LET ((FUNCTION-BEING-PROCESSED FUNCTION))	;FUNCTION or its internals
      (DOLIST (VL VARIABLES-LISTS)
	(DOLIST (V VL)
	  (COND ((OR (STRING-EQUAL (VAR-NAME V) 'IGNORE)
		     (STRING-EQUAL (VAR-NAME V) 'IGNORED))
		 (OR (ZEROP (VAR-USE-COUNT V))
		     (BARF (VAR-NAME V)
			   "(the variable by that name) is bound but is not ignored"
			   'WARN)))
		((NOT (STRING-EQUAL (VAR-NAME V) 'OPERATION))
		 (AND (ZEROP (VAR-USE-COUNT V))
		      (EQ (VAR-TYPE V) 'FEF-LOCAL)
		      (BARF (VAR-NAME V)
			    "is bound but is never used" 'WARN)))))))
    VAL))

;Compile an internal lambda which must be passed as an argument
;into a separate function, which has its own name which is a list.
;That name is returned.

(DEFUN BREAKOFF (X &OPTIONAL LEXICAL &AUX FNAME)
  (SETQ FNAME (IF (EQ (CAR X) 'NAMED-LAMBDA)
		  (IF (ATOM (CADR X)) (CADR X) (CAADR X))
		  (PROG1 `(:INTERNAL ,FUNCTION-BEING-PROCESSED ,BREAKOFF-COUNT)
			 (SETQ BREAKOFF-COUNT (1+ BREAKOFF-COUNT)))))
  (SETQ QC-FUNCTIONS-TO-BE-TRANSLATED
	(NCONC QC-FUNCTIONS-TO-BE-TRANSLATED
	       (NCONS (LIST FNAME X LOCAL-DECLARATIONS 
			    (AND LEXICAL (CONS VARS COMPILER-LEXICAL-ENVIRONMENT))))))
  FNAME)

;; Given a function, break it off if appropriate
;; and return the result.
;; This will be (FUNCTION symbol) or (BREAKOFF-FUNCTION list).
;; If it doesn't need breaking off, return NIL.
(DEFUN MAYBE-BREAKOFF (FUNCTION &OPTIONAL LEXICAL)
    (COND ((ATOM FUNCTION) NIL)
	  ((MEMQ (CAR FUNCTION) '(LAMBDA NAMED-LAMBDA))
	   (LET ((TEM (BREAKOFF FUNCTION LEXICAL)))
	     (COND ((SYMBOLP TEM) (LIST 'FUNCTION TEM))
		   (T (LIST 'BREAKOFF-FUNCTION TEM)))))))

;; If FUNCTION needs to be broken off, make a lexical closure from it
;; and return the way to pass that along to pass 2.
;; Otherwise return nil.
(DEFUN MAYBE-BREAKOFF-LEXICAL-CLOSURE (FUNCTION)
  (LET ((TEM (MAYBE-BREAKOFF FUNCTION T)))
    (AND TEM `(LEXICAL-CLOSURE ,(MAKE-LEXICAL-CLOSURE-HOMES TEM)))))

;; Create the six local variables which will eventually be used as
;; the space to hold the data of a lexical closure for a specified function.
;; Returns a list (LEXICAL-CLOSURE var-home) containing the home
;; for the first of those six.  This list is the expression passed to pass 2.
;; FN-NAME should be (FUNCTION symbol) or (BREAKOFF-FUNCTION list).
(DEFUN MAKE-LEXICAL-CLOSURE-HOMES (FN-NAME)
  (LET* ((GEN (GENSYM))
	 (MYVARS
	   (LIST (VAR-MAKE-HOME GEN 'FEF-LOCAL 'FEF-ARG-AUX
				`(FEF-INI-PNTR ,FN-NAME) 'FEF-QT-EVAL ())
		 (VAR-MAKE-HOME GEN 'FEF-LOCAL 'FEF-ARG-AUX
				'(FEF-INI-PNTR (LOCATIVE-TO-S-V-CELL LEXICAL-ENVIRONMENT))
				'FEF-QT-EVAL ())
		 (VAR-MAKE-HOME GEN 'FEF-LOCAL 'FEF-ARG-AUX
				'(FEF-INI-NIL NIL) 'FEF-QT-EVAL ())
		 (VAR-MAKE-HOME GEN 'FEF-LOCAL 'FEF-ARG-AUX
				'(FEF-INI-NIL NIL) 'FEF-QT-EVAL ())
		 (VAR-MAKE-HOME GEN 'FEF-LOCAL 'FEF-ARG-AUX
				'(FEF-INI-NIL NIL) 'FEF-QT-EVAL ())
		 (VAR-MAKE-HOME GEN 'FEF-LOCAL 'FEF-ARG-AUX
				'(FEF-INI-C-PNTR (LOCATIVE-TO-S-V-CELL LEXICAL-ENVIRONMENT))
				'FEF-QT-EVAL ()))))
    (MAPC 'VAR-INCREMENT-USE-COUNT MYVARS)
    (SETQ ALLVARS (NRECONC MYVARS ALLVARS))
    (CAR MYVARS)))

;Return the home, and the index (for use in %LOCATE-IN-HIGHER-CONTEXT),
;of a variable from a higher context.
(DEFUN FIND-LEXICAL-VAR (VAR &AUX HOME LAP-ADDR)
  (DO ((I 0 (1+ I))
       (E COMPILER-LEXICAL-ENVIRONMENT (CDR E)))
      ((NULL E))
    (AND (SETQ HOME (ASSQ VAR (CAR E)))
	 (SETQ LAP-ADDR (VAR-LAP-ADDRESS HOME))
	 (SELECTQ (CAR LAP-ADDR)
	   (ARG (RETURN HOME (+ (CADR LAP-ADDR) (LSH I 12.))))
	   (LOCBLOCK (RETURN HOME (+ (CADR LAP-ADDR) (LSH I 12.) (LSH 1 23.))))))))

(DEFUN TRY-REF-LEXICAL-VAR (VAR)
  (MULTIPLE-VALUE-BIND (HOME IDX)
      (FIND-LEXICAL-VAR VAR)
    (COND (HOME (VAR-INCREMENT-USE-COUNT HOME)
		`(LEXICAL-REF ,IDX)))))

;QCOMPILE0 compiles one function, producing a list of lap code in QCMP-OUTPUT.
;The first argument is the lambda-expression which defines the function.
;  It must actually be a LAMBDA or NAMED-LAMBDA.  Other things are not allowed.
;The second argument is the name of the function.
;The third won't be useful till there's a microcompiler.

;We expect that DEFAULT-CONS-AREA has been bound to QCOMPILE-TEMPORARY-AREA.
;The compiler does ALL consing in that temporary area unless it specifies otherwise.

(DEFUN QCOMPILE0 (EXP NAME GENERATING-MICRO-COMPILER-INPUT-P)  
  (PROG (VARS EXP1 ARGN LVCNT
	 PDLLVL MAXPDLLVL CALL-BLOCK-PDL-LEVELS
	 ALLGOTAGS GOTAGS TLEVEL P1VALUE BINDP
	 DROPTHRU ALLVARS FREEVARS RETPROGDESC
	 PROGDESCS LL TAGOUT TLFUNINIT SPECIALFLAG MACROFLAG
	 LOCAL-MAP ARG-MAP DOCUMENTATION EXPR-DEBUG-INFO
	 FAST-ARGS-POSSIBLE BREAKOFF-COUNT
	 CLOBBER-NONSPECIAL-VARS-LISTS)
       (SETQ PDLLVL 0)		;RUNTINE LOCAL PDLLVL
       (SETQ DROPTHRU T)	;CAN DROP IN IF FALSE, FLUSH STUFF TILL TAG OR
       (SETQ MAXPDLLVL 0)	;DEEPEST LVL REACHED BY LOCAL PDL
       (SETQ TLEVEL T)
       (SETQ P1VALUE T)
       (SETQ FAST-ARGS-POSSIBLE T)
       (SETQ BREAKOFF-COUNT 0)
       ;; If compiling a macro, compile its expansion function
       ;; and direct lap to construct a macro later.
       (COND ((EQ (CAR EXP) 'MACRO)
	      (SETQ MACROFLAG T)
	      (SETQ EXP (CDR EXP))))
       (OR (EQ (CAR EXP) 'LAMBDA)
	   (EQ (CAR EXP) 'NAMED-LAMBDA)
	   (BARF EXP '|not a function| 'DATA))
       (BEGIN-PROCESSING-FUNCTION NAME)
       (SETQ EXP1 EXP)
       ;; If a NAMED-LAMBDA, discard the name and save debug-info in special place.
       (AND (EQ (CAR EXP1) 'NAMED-LAMBDA)
	    (SETQ EXPR-DEBUG-INFO
		  (AND (NOT (ATOM (CADR EXP1)))
		       (CDADR EXP1))
		  EXP1 (CDR EXP1)))
       (SETQ LL (CADR EXP1))	;lambda list.
       (SETQ EXP1 (CDDR EXP1))	;body
       ;; If there is a documentation string at the front
       ;; before the declaration, flush it.
       (AND (CDR EXP1)
	    (STRINGP (CAR EXP1))
	    (SETQ DOCUMENTATION (CAR EXP1) EXP1 (CDR EXP1)))
       ;; If the first thing in the body is (DECLARE (FOO X) (BAR Y)),
       ;; take the (FOO X) and (BAR Y) as local declarations for the whole function.
       (AND (NOT (ATOM (CAR EXP1)))
	    (EQ (CAAR EXP1) 'DECLARE)
	    (SETQ LOCAL-DECLARATIONS (APPEND (CDAR EXP1) LOCAL-DECLARATIONS)
		  EXP1 (CDR EXP1)))
       ;; Allow a documentation string after a declaration also.
       (AND (NULL DOCUMENTATION)
	    (CDR EXP1)
	    (STRINGP (CAR EXP1))
	    (SETQ DOCUMENTATION (CAR EXP1) EXP1 (CDR EXP1)))
       ;; Put arglist together with body again.
       (SETQ EXP1 `(LAMBDA ,LL . ,EXP1))
       ;; If there are keyword arguments, expand them.
       (AND (MEMQ '&KEY LL)
	    (SETQ EXP1 (EXPAND-KEYED-LAMBDA EXP1)))
       ;; Now turn any &AUX variables in the LAMBDA into a PROG in the body.
       (SETQ EXP1 (P1AUX EXP1))
       ;; Separate lambda list and body again.
       (SETQ LL (CADR EXP1) EXP1 (CDDR EXP1))
       ;; Now process the variables in the lambda list, after the local declarations.
       (SETQ LL (P1SBIND LL 'FEF-ARG-REQ NIL NIL))
       (COND ((NOT (NULL (CDR EXP1)))
	      (SETQ EXP1 (CONS 'PROGN EXP1)))
	     ((SETQ EXP1 (CAR EXP1))))
       (SETQ EXP1 (P1 EXP1))		;DO PASS 1 TO SINGLE-EXPRESSION BODY
       (ASSIGN-LAP-ADDRESSES)
       (OUTF (LIST 'MFEF NAME SPECIALFLAG (ELIMINATE-DUPLICATES-AND-REVERSE ALLVARS)
		   FREEVARS))
       (AND MACROFLAG (OUTF '(CONSTRUCT-MACRO)))
       (OUTF '(QTAG S-V-BASE))
       (OUTF '(S-V-BLOCK))
       (OUTF '(QTAG DESC-LIST-ORG))
       (OUTF '(A-D-L))
       (OUTF (LIST 'PARAM 'LLOCBLOCK LVCNT))
       (OUTF '(QTAG QUOTE-BASE))
       (OUTF '(ENDLIST))			;LAP WILL INSERT QUOTE VECTOR HERE
       ;; Set up the debug info from the local declarations and other things
       (LET ((DEBUG-INFO NIL) TEM)
	 (AND DOCUMENTATION (PUSH `(:DOCUMENTATION ,DOCUMENTATION) DEBUG-INFO))
	 (DOLIST (DCL *DEBUG-INFO-LOCAL-DECLARATION-TYPES*)
	   (IF (SETQ TEM (ASSQ DCL LOCAL-DECLARATIONS))
	       (PUSH TEM DEBUG-INFO)))
	 (DOLIST (DCL EXPR-DEBUG-INFO)
	   (OR (ASSQ (CAR DCL) DEBUG-INFO)
	       (PUSH DCL DEBUG-INFO)))
         (AND (PLUSP BREAKOFF-COUNT)
	      (LET ((INTERNAL-OFFSETS (MAKE-LIST BREAKOFF-COUNT)))
		(OUTF `(BREAKOFFS ,INTERNAL-OFFSETS))
		(PUSH `(:INTERNAL-FEF-OFFSETS . ,INTERNAL-OFFSETS) DEBUG-INFO)))
         ;; Include the local and arg maps if we have them.
         ;; They were built by ASSIGN-LAP-ADDRESSES.
         (AND LOCAL-MAP (PUSH `(LOCAL-MAP ,LOCAL-MAP) DEBUG-INFO))
         (AND ARG-MAP (PUSH `(ARG-MAP ,ARG-MAP) DEBUG-INFO))
	 (AND DEBUG-INFO
              (OUTF `(DEBUG-INFO . ,DEBUG-INFO))))
       (OUTF 'PROGSA)
       (P2SBIND LL VARS NIL)			;CAN COMPILE INITIALIZING CODE
       (P2 EXP1 'D-RETURN)			;DO PASS 2
       (OUTF (LIST 'PARAM 'MXPDL (1+ MAXPDLLVL)))
       (RETURN ALLVARS)))

;;; This should be called as each function is begun to be compiled
(DEFUN BEGIN-PROCESSING-FUNCTION (NAME)
  (OR COMPILING-WHOLE-FILE-P
      (ZWEI:COMPILER-WARNINGS-SETUP-FOR-PROCESSING-FUNCTION NAME))
  (SETQ FUNCTIONS-DEFINED
	(LET #Q ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)) #M NIL 
	  (CONS NAME FUNCTIONS-DEFINED))))

;There can be duplicates of local vars on allvars because of the variable overlaping hack.
;Dont disturb special vars.
(DEFUN ELIMINATE-DUPLICATES-AND-REVERSE (VAR-LIST)
  (PROG (ANS)
    L  (COND ((NULL VAR-LIST) (RETURN ANS))
	     ((NULL (DOLIST (V ANS)
		      (IF (AND (EQ (VAR-NAME V) (VAR-NAME (CAR VAR-LIST)))
			       (NOT (EQ (CAR (VAR-LAP-ADDRESS V)) 'SPECIAL))
			       (EQUAL (VAR-LAP-ADDRESS V) (VAR-LAP-ADDRESS (CAR VAR-LIST))))
			  (RETURN T))))		;this a local duplicate, flush
	      (SETQ ANS (CONS (CAR VAR-LIST) ANS))))
       (SETQ VAR-LIST (CDR VAR-LIST))
       (GO L)))

;Expand functions that want keyword arguments.
;Make them take &REST args instead, and give them code to look up the keywords.

(COMMENT  ;starting from this
(DEFUN FOO (X &REST Y &KEY MUMBLE &OPTIONAL (BLETCH T BLETCHP) &AUX BAZZZ)
   BODY)

;We create this:
;(We call with the rest arg starting with 'permutation-table <table>
; before the first keyword, if we want to memoize the keyword lookup.
; The permutation table gets filled with the index of each specified
; keyword in the list of allowed keywords, and then it is used to
; permute the args, rather than looking up the keywords again.
; The leader of the permutation table records the fef that the table
; was computed for.  If the function definition changes, the table
; is recomputed).
(DEFUN FOO (X &REST Y &AUX (MUMBLE KEYWORD-GARBAGE) (BLETCH T) BLETCHP)
  (SI:STORE-KEYWORD-ARG-VALUES (%STACK-FRAME-POINTER)
			       Y '(:MUMBLE :BLETCH)
			       NIL		;T if &ALLOW-OTHER-KEYS
			       2)		;1st 2 keywords required.
  (AND (EQ MUMBLE KEYWORD-GARBAGE) (FERROR ...))
  ((LAMBDA (&AUX BAZZZ)
     BODY)))

) ;end COMMENT

;Given a lambda which uses &KEY, return an equivalent one
;which does not use &KEY.  It takes a &REST arg instead
;(though if the original one had a rest arg, it uses that one).
;If there is no ARGLIST declaration for this function, we make one
;so that the user is still told that the function wants keyword args.
(DEFUN EXPAND-KEYED-LAMBDA (LAMBDA-EXP)
  (LET (LAMBDA-LIST BODY
	MAYBE-REST-ARG KEYCHECKS
	POSITIONAL-ARGS AUXVARS REST-ARG POSITIONAL-ARG-NAMES
 	KEYKEYS KEYNAMES KEYOPTFS KEYINITS KEYFLAGS ALLOW-OTHER-KEYS
	(NUMBER-OF-REQUIRED-KEYWORDS 0))
    (COND ((EQ (CAR LAMBDA-EXP) 'LAMBDA)
	   (SETQ LAMBDA-LIST (CADR LAMBDA-EXP) BODY (CDDR LAMBDA-EXP)))
	  (T
	   (SETQ LAMBDA-LIST (CADDR LAMBDA-EXP) BODY (CDDDR LAMBDA-EXP))))
    (MULTIPLE-VALUE (POSITIONAL-ARGS NIL AUXVARS
		     REST-ARG POSITIONAL-ARG-NAMES
		     KEYKEYS KEYNAMES KEYOPTFS KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
      (DECODE-KEYWORD-ARGLIST LAMBDA-LIST))
    ;; For each keyword arg, decide whether we need to nit it to KEYWORD-GARBAGE
    ;; and check explicitly whether that has been overridden.
    ;; If the arg is optional
    ;; and the initial value is a constant, we can really init it to that.
    ;; Otherwise, we change its KEYINITS element to
    ;; KEYWORD-GARBAGE and push a cleanup form on KEYCHECKS.
    (DO ((KIS KEYINITS (CDR KIS))
	 (KNS KEYNAMES (CDR KNS))
	 (KOS KEYOPTFS (CDR KOS))
	 (KKS KEYKEYS (CDR KKS))
	 (KFS KEYFLAGS (CDR KFS))
	 (MAP-BIT-NUMBER 0 (1+ MAP-BIT-NUMBER)))
	((NULL KNS))
      (OR (CAR KOS)
	  (INCF NUMBER-OF-REQUIRED-KEYWORDS))
      (LET ((KEYNAME (CAR KNS)) (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
	(IF (CAR KOS)
	    ;; Optional.
	    (OR (AND (NULL KEYFLAG)
		     (OR (CONSTANTP KEYINIT)
			 (QUOTEP KEYINIT)))
		(PROGN (RPLACA KIS 'SI:KEYWORD-GARBAGE)
		       (PUSH `(COND ((EQ ,KEYNAME SI:KEYWORD-GARBAGE)
				     (SETQ ,KEYNAME ,KEYINIT))
				    (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))))
			     KEYCHECKS)))
	    ;; Required
	    (RPLACA KIS 'SI:KEYWORD-GARBAGE))))
    (SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
    (SETQ KEYCHECKS (NREVERSE KEYCHECKS))
    
    ;; Add some declarations to inform user of the fact that this compiled
    ;; function still logically wants keyword args.
    ;; The arglist info can be overridden by the user's explicit declaration.
    (COND ((ASSQ 'ARGLIST LOCAL-DECLARATIONS))
	  (T
	   (PUSH `(ARGLIST . ,(LDIFF LAMBDA-LIST AUXVARS)) LOCAL-DECLARATIONS)))

    ;; If the user didn't ask for a rest arg, make one for the
    ;; outer function anyway.
    (OR REST-ARG (SETQ REST-ARG (GENSYM)
		       MAYBE-REST-ARG (LIST '&REST REST-ARG)))

    ;; Put our list of variable names onto CLOBBER-NONSPECIAL-VARS-LISTS
    ;; so that ASSIGN-LAP-ADDRESSES will clobber out the variables
    ;; which are not special with NIL.
    (PUSH KEYNAMES CLOBBER-NONSPECIAL-VARS-LISTS)
    `(LAMBDA (,@POSITIONAL-ARGS ,@MAYBE-REST-ARG
	      &AUX ,@(MAPCAR '(LAMBDA (V INIT) `(,V ,INIT)) KEYNAMES KEYINITS)
	      ,@KEYFLAGS)
;       (COND ((EQ (CAR ,REST-ARG) 'PERMUTATION-TABLE)
;	      (OR (%PERMUTE-ARGS)
;		  (PROGN (RECOMPUTE-KEYWORD-PERMUTATION-TABLE
;			   (CDR ,REST-ARG)
;			   (%P-CONTENTS-OFFSET (%STACK-FRAME-POINTER) %LP-FEF)
;			   ',KEYKEYS)
;			 (%PERMUTE-ARGS)))
;	      ;; If the function really wants the rest arg,
;	      ;; flush the permutation table and its keyword.
;	      ,(AND (NOT MAYBE-REST-ARG) `(SETQ ,REST-ARG (CDDR ,REST-ARG))))
;	     (T
	      (SI:STORE-KEYWORD-ARG-VALUES (%STACK-FRAME-POINTER)
					,REST-ARG ',KEYKEYS
					,ALLOW-OTHER-KEYS
					,NUMBER-OF-REQUIRED-KEYWORDS
					',KEYNAMES)
;	      ))
       ,@KEYCHECKS
       ((LAMBDA ,AUXVARS . ,BODY)))))

(comment   ;This optimization isn't in use yet, and may never be
  	   ;if microcoding STORE-KEYWORD-ARG-VALUES is winning enough.

;Given a permutation table for keyword args whose contents are garbage,
;and the actual arglist with keywords,
;compute the contents of the permutation table
;based on calling the fef NEW-FEF.
(DEFUN RECOMPUTE-KEYWORD-PERMUTATION-TABLE (TABLE-AND-ARGS NEW-FEF KEYWORDS)
  (LET ((TABLE (CAR TABLE-AND-ARGS)))
    (DO ((I 0 (1+ I))
	 (ARGS1 (CDR TABLE-AND-ARGS) (CDDR ARGS1)))
	((NULL ARGS1)
	 (SETF (ARRAY-LEADER TABLE 0) NEW-FEF))
      (LET ((KEYWORD (CAR ARGS1)))
	(DO (INDEX) (())
	  (SETQ INDEX (FIND-POSITION-IN-LIST KEYWORD KEYWORDS))
	  (AND INDEX (RETURN (SETF (AREF TABLE I) INDEX)))
	  (SETQ KEYWORD (CERROR T NIL ':UNDEFINED-ARG-KEYWORD
				"Keyword arg keyword ~S unrecognized"
				KEYWORD)))))))

;Given a form that is a call to a function which takes keyword args,
;stick in a permutation table, if the keyword names are constant.
;The question of how calls to such functions are detected is still open.
(DEFUN OPTIMIZE-KEYWORD-CALL (FORM)
  (LET ((ARGS-INFO (ARGS-INFO (CAR FORM))))
    (LET ((KEYARGS (CDR (NTHCDR (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO) FORM))))
      (COND ((DO ((TAIL KEYARGS (CDDR TAIL)))
		 ((NULL TAIL) T)
	       (OR (QUOTEP (CAR TAIL)) (RETURN NIL)))
	     ;; Here if every keyword name is quoted.
	     `(,@(LDIFF FORM KEYARGS)
	       'PERMUTATION-TABLE
	       ',(MAKE-ARRAY (// 2 (LENGTH KEYARGS))
			     ':LEADER-LENGTH 1 ':TYPE ART-8B)
	       . ,KEYARGS))
	    (T FORM)))))

;Temporary definition for what ought to be defined in the microcode.
;Keyed functions' expansions call this, but until calls are open-coded
;the arguments will never be such as to make the call actually happen.
;The open-coding won't be installed until the ucode function works.
;Meanwhile this prevents warning messages when keyed functions are compiled.
(DEFUN %PERMUTE-ARGS () (FERROR NIL "%PERMUTE-ARGS called"))

);end COMMENT

;Pass 1.
;We expand all macros and perform source-optimizations
;according to the OPTIMIZERS properties.  Internal lambdas turn into progs.
;Free variables are made special and put on FREEVARS.
;PROGs are converted into an internal form which contains pointers
;to the VARS and GOTAGS lists of bound variables and prog tags.
;All self-evaluating constants (including T and NIL) are replaced by
;quote of themselves.
;P1VALUE is NIL when compiling a form whose value is to be discarded.
;Some macros and optimizers look at it.

(DEFUN P1V (FORM)
    (LET ((P1VALUE T))
       (P1 FORM)))

(DEFUN P1 (FORM)
  (PROG (TM)
	(SETQ FORM (OPTIMIZE FORM T))
	(COND
	  ((CONSTANTP FORM) (RETURN (LIST 'QUOTE FORM)))
	  ((ATOM FORM)
	   (RETURN (COND ((SETQ TM (ASSQ FORM VARS))
			  (VAR-INCREMENT-USE-COUNT TM)
			  (VAR-LAP-ADDRESS TM))
			 ((TRY-REF-LEXICAL-VAR FORM))
			 (T (MAKESPECIAL FORM) FORM))))
	  ((EQ (CAR FORM) 'QUOTE) (RETURN  FORM))
	  ;; Certain constructs must be checked for here
	  ;; so we can call P1 recursively without setting TLEVEL to NIL.
	  ((NOT (ATOM (CAR FORM)))
	   (SELECTQ (CAAR FORM)
	     ((LAMBDA NAMED-LAMBDA)
	      (RETURN (P1LAMBDA (CAR FORM) (CDR FORM))))
	     (OTHERWISE
	      ;; Old Maclisp evaluated functions.
	      (BARF FORM '|function is a form to be evaluated; use FUNCALL| 'WARN)
	      (RETURN (P1 `(FUNCALL . ,FORM))))))
	  ((NOT (SYMBOLP (CAR FORM)))
	   (BARF (CAR FORM) '|function is not a symbol or list| 'WARN)
	   (RETURN (P1 (CONS 'PROGN (CDR FORM)))))
	  ((MEMQ (CAR FORM) '(PROG PROG* PPROG SPROG)) (RETURN (P1PROG FORM)))
	  ((MEMQ (CAR FORM) '(RETURN RETURN-FROM-T))
	   (AND (CDDR FORM) (SETQ TLEVEL NIL))
	   (RETURN (P1EVARGS FORM)))
	  ((EQ (CAR FORM) '%POP)		;P2 specially checks for this
	   (RETURN FORM)))
	(SETQ TLEVEL NIL)
	;; Check for functions with special P1 handlers.
	(COND ((SETQ TM (GET (CAR FORM) 'P1))
	       (RETURN (FUNCALL TM FORM))))
	(COND ((AND ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH
		    (ASSQ (CAR FORM) VARS)
		    (NULL (FUNCTION-P (CAR FORM))))
	       (BARF FORM
		     '|variable in function position; use FUNCALL|
		     'WARN)
	       (RETURN (P1 (CONS 'FUNCALL FORM)))))
	(RETURN (P1ARGC FORM (GETARGDESC (CAR FORM))))))

(DEFUN FUNCTION-P (X)
  (COND ((SYMBOLP X)
	 (OR (FBOUNDP X) (GETL X '(*EXPR ARGDESC))))
	((FDEFINEDP X))
	(T (FUNCALL (GET (CAR X) 'FUNCTION-SPEC-HANDLER) 'SI:COMPILER-FDEFINEDP X))))

(DEFUN MSPL2 (X)
  (COND ((LET ((BARF-SPECIAL-LIST THIS-FUNCTION-BARF-SPECIAL-LIST))
	   (NOT (SPECIALP X)))
	 ;; Here unless this variable was either 1) declared special, or
	 ;; 2) already used free in this function.
	 (BARF X '|declared special| 'WARN)
	 (LET #Q ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)) #M NIL 
	      (OR (MEMQ X BARF-SPECIAL-LIST)
		  (PUSH X BARF-SPECIAL-LIST))
	      (PUSH X THIS-FUNCTION-BARF-SPECIAL-LIST))
	 (COND ((ASSQ X ALLVARS)
		(BARF X '|previously assumed local; you will lose| 'WARN))))))

(DEFUN MAKESPECIAL (X)
  (MSPL2 X)
  (OR (MEMQ X FREEVARS)
      (PUSH X FREEVARS))
  T)

;Given a form, apply optimizations and expand macros until no more is possible
;(at the top level).  Also apply style-checkers to the supplied input
;but not to generated output.  This function is also in charge of checking for
;too few or too many arguments so that this happens before optimizers are applied.
(DEFUN OPTIMIZE (FORM CHECK-STYLE)
  (DO ((TM) (FN)) ((ATOM FORM))	;Do until no more expansions possible
    (SETQ FN (CAR FORM))
    ;; Check for too few or too many arguments
    (CHECK-NUMBER-OF-ARGS FORM)
    ;; Do style checking
    (AND CHECK-STYLE (NULL INHIBIT-STYLE-WARNINGS-SWITCH)
	 (COND ((ATOM FN)
		(AND (SYMBOLP FN)
		     (SETQ TM (GET FN 'STYLE-CHECKER))
		     (FUNCALL TM FORM)))
	       ((NOT RUN-IN-MACLISP-SWITCH))
	       ((OR (EQ (CAR FN) 'LAMBDA) (EQ (CAR FN) 'NAMED-LAMBDA))
		(LAMBDA-STYLE FN))
	       ((MEMQ (CAR FN) '(CURRY-BEFORE CURRY-AFTER))
		(BARF (CAR FN) '|does not work in Maclisp| 'WARN))))
    ;; Apply optimizations
    (COND ((NOT (SYMBOLP FN)) (RETURN))
	  ((DOLIST (OPT (GET FN 'OPTIMIZERS))
	     (OR (EQ FORM (SETQ FORM (FUNCALL OPT FORM)))
		 (RETURN T))))	;Optimizer changed something, don't do macros this pass
	  ;; Expand if macro
	  ((EQ FORM (SETQ FORM (MACROEXPAND-1 FORM T)))
	   (RETURN)))				;Stop looping, no expansions apply
    ;; Only do style checking the first time around
    (SETQ CHECK-STYLE NIL))
  ;; Result is FORM
  FORM)

;Given a non-atomic form issue any warnings required because of wrong number of arguments.
;This function has some of the same knowledge as GETARGDESC but doesn't call
;it because GETARGDESC has to do a lot more.
;This function should never get an error and never warn about
;anything that gets warned about elsewhere.
(IF-FOR-LISPM	;to inhibit the Maclisp style warnings
(DEFUN CHECK-NUMBER-OF-ARGS (FORM)
  (PROG ((FN (CAR FORM)) NARGS (MIN NIL) (MAX 0) (ARGS-INFO NIL) TEM)
   TOP
    (COND ((AND (LISTP FN) (FUNCTIONP FN T))
	   (SETQ TEM (ARGLIST FN T))
	   (OR (LISTP TEM) (RETURN))
	   (DOLIST (X TEM)
	     (COND ((EQ X '&OPTIONAL) (SETQ MIN MAX))
		   ((OR (EQ X '&REST) (EQ X '&KEY))
		    (SETQ MIN MAX MAX 37777777) (RETURN))
		   ((EQ X '&AUX) (RETURN))
		   ((MEMQ X LAMBDA-LIST-KEYWORDS))
		   (T (SETQ MAX (1+ MAX))))))
	  ((NOT (SYMBOLP FN)) (RETURN NIL))	;Unknown type, don't check
	  ((SETQ TEM (GET FN 'ARGDESC))
	   (DOLIST (X TEM)
	     (COND ((MEMQ 'FEF-ARG-REQ (CADR X))
		    (SETQ MAX (+ MAX (CAR X))))
		   ((MEMQ 'FEF-ARG-OPT (CADR X))
		    (OR MIN (SETQ MIN MAX))
		    (SETQ MAX (+ MAX (CAR X))))
		   ((MEMQ 'FEF-ARG-REST (CADR X))
		    (OR MIN (SETQ MIN MAX))
		    (SETQ MAX 37777777)))))
	  ((SETQ TEM (GET FN 'QINTCMP))
	   (SETQ MAX TEM))
	  ((SETQ TEM (GET FN 'Q-ARGS-PROP))
	   (SETQ ARGS-INFO TEM))
	  ((FBOUNDP FN)
	   (SETQ TEM (SI:UNENCAPSULATE-FUNCTION-SPEC FN))
	   (COND ((NOT (EQ TEM FN))
		  (SETQ FN TEM)
		  (GO TOP)))
	   (SETQ TEM (FSYMEVAL FN))
	   (COND ((OR (SYMBOLP TEM) (LISTP TEM))
		  (SETQ FN TEM)
		  (GO TOP))
		 (T (SETQ ARGS-INFO (%ARGS-INFO TEM)))))
	  (T (RETURN NIL)))			;No information available
    (AND ARGS-INFO
	 (SETQ MIN (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
	       MAX (IF (BIT-TEST (LOGIOR %ARG-DESC-QUOTED-REST %ARG-DESC-EVALED-REST)
				 ARGS-INFO)
		       37777777
		       (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO))))
    (SETQ NARGS (LENGTH (CDR FORM)))		;Now that we know it's not a macro
    (COND ((< NARGS (OR MIN MAX))
	   (BARF FORM '|Too few arguments| 'WARN))
	  ((> NARGS MAX)
	   (BARF FORM '|Too many arguments| 'WARN)))))
);if-for-lispm

;Pass 1 processing for a call to an ordinary function (ordinary, at least, for pass 1).
;FORM is the call to the function, and DESC is the GETARGDESC of the function.
;Processing consists of P1'ing all evaluated arguments, but not the quoted ones.
;DESC is used to determine which is which.
;In addition, &FUNCTIONAL arguments are broken off and separately compiled.
;We process the args by copying the arglist, and rplaca'ing each arg by P1 of itself if needed.
(DEFUN P1ARGC (FORM DESC)
       (PROG (COUNT TOKEN-LIST ARGS-LEFT DESCS-LEFT ARG-P1-RESULTS FCTN TM P1VALUE)
	     (SETQ P1VALUE T)
	     (SETQ DESCS-LEFT DESC)
	     (SETQ FCTN (CAR FORM))
	     (COND ((AND DESCS-LEFT
			 (MEMQ 'FEF-ARG-REST (SETQ TM (CADAR DESCS-LEFT)))
			 (MEMQ 'FEF-QT-QT TM))
		    (RETURN FORM))) 		;JUST FOR "EFFICIENCY"
	     (SETQ ARG-P1-RESULTS (SETQ ARGS-LEFT (APPEND (CDR FORM) NIL)))
             (SETQ COUNT 0)
	L3
	     ;; If all arguments processed, return.
	     (COND ((NULL ARGS-LEFT)
		    (RETURN (CONS FCTN ARG-P1-RESULTS))))

	     ;; Figure out what descriptor to use for the next argument.
	     ;; TOKEN-LIST is the actual descriptor, and COUNT
	     ;; is the number of argumens left for it to apply to.
	     (COND ((ZEROP COUNT)
		    (COND ((NULL DESCS-LEFT)
			   ;; Out of descriptors => treat excess args as evalled.
			   (SETQ DESCS-LEFT '((1005 (FEF-ARG-OPT FEF-QT-EVAL))))))
		    (SETQ COUNT (CAAR DESCS-LEFT))
		    (SETQ TOKEN-LIST (CADAR DESCS-LEFT))
		    (SETQ DESCS-LEFT (CDR DESCS-LEFT))
		    (COND ((MEMQ 'FEF-ARG-REST TOKEN-LIST)
			   (SETQ COUNT 1005)))))

	     ;; Process the next argument according to its descriptor.
	     (COND ((MEMQ 'FEF-QT-QT TOKEN-LIST))
		   ((OR (MEMQ 'FEF-QT-EVAL TOKEN-LIST)
			(MEMQ 'FEF-QT-DONTCARE TOKEN-LIST))
		    (RPLACA ARGS-LEFT
			  (COND ((AND (MEMQ 'FEF-FUNCTIONAL-ARG TOKEN-LIST)
				      (NOT (ATOM (SETQ TM (OPTIMIZE (CAR ARGS-LEFT) T))))
				      (EQ (CAR TM) 'QUOTE))	 ;LOOK FOR '(LAMBDA...)
				 (P1FUNCTION TM))
				(T (P1 (CAR ARGS-LEFT))))))
		   (T (BARF TOKEN-LIST 'BAD-EVAL-CODE 'BARF)))
	     (SETQ ARGS-LEFT (CDR ARGS-LEFT))
	     (SETQ COUNT (1- COUNT))
	     (GO L3)))

;Return T if OBJECT is self-evaluating.
(DEFUN CONSTANTP (OBJECT)
    (OR (NUMBERP OBJECT)
	#Q (STRINGP OBJECT)
	(NULL OBJECT)
	(EQ OBJECT T)))

;Return T if OBJECT is something quoted.
(DEFUN QUOTEP (OBJECT)
    (AND (NOT (ATOM OBJECT))
         (EQ (CAR OBJECT) 'QUOTE)))

;;When a var is handled by P1BINDVAR which is an optional arg with a specified-flag,
;;we push the flag name onto SPECIFIED-FLAGS so that a home will be made for the flag.
(DEFVAR SPECIFIED-FLAGS)

;Process a Lambda-list (X), making the variables by default of kind KIND
;(FEF-ARG-REQ for the top-level lambda,
; FEF-ARG-AUX or FEF-ARG-INTERNAL-AUX for progs).
;Return a prog variable list for the same variables with their initializations if any,
;with P1 done on each initialization.
;This function gobbles down the variables and processes keywords.
;Each variable, with its appropeiate keyword info, is passed to P1LMB.
;We can do either sequential or parallel binding.
;Processing of variables is done in two steps:
;First, create the homes
;Second, if these are not FEF-ARG-INTERNAL-AUX vars,
; put the homes on VARS and ALLVARS.
;Third, process all the variables' initializations.
;Finally, put the homes on VARS and ALLVARS if not already there.

;For variables whose scope is the whole function (not FEF-ARG-INTERNAL-AUX),
;the order is designed so that variables bound inside their initializations
;all come after all the variables of the original (higher) level.
;This is needed to make sure that (DEFUN FOO (&OPTIONAL (A (LET ((C ...)) ...)) B) ...)
;does not put C into VARS before B.

;For FEF-ARG-INTERNAL-AUX variables, we want the variables bound
;inside the initializations to come first, since they are used first.
;That way, our own variables overlap with them rather than vice versa.
;As a result, the variable with the original home is always the first one used.
;This is important for deciding which variables need explicit initialization.

;The IGNORE-NIL-P argument is used by MULTIPLE-VALUE-BIND to say
; that if NIL appears as a variable, its initial value should be evaluated
; and discarded.
(DEFUN P1SBIND (X KIND PARALLEL IGNORE-NIL-P)
  (LET (TM EVALCODE VARN MYVARS MISC-TYPES
	SPECIFIED-FLAGS (SPECIALNESS NIL))
    ;; First look at the var specs and make homes, pushing them on MYVARS (reversed).
    (PROG ()
	  (SETQ EVALCODE 'FEF-QT-DONTCARE)
       A  (COND ((NULL X) (RETURN))
		((SETQ TM (ASSQ (CAR X)
				'((&OPTIONAL . FEF-ARG-OPT)
				  (&REST . FEF-ARG-REST) (&AUX . FEF-ARG-AUX))))
		 (COND ((OR (EQ KIND 'FEF-ARG-AUX)
			    (EQ KIND 'FEF-ARG-INTERNAL-AUX))
			(BARF (CAR X) '|argument keywords in PROG variable list| 'WARN))
		       (T (SETQ KIND (CDR TM))))
		 (GO B))
		((SETQ TM (ASSQ (CAR X)
				'((&EVAL . FEF-QT-EVAL)
				  (&QUOTE . FEF-QT-QT)
				  (&QUOTE-DONTCARE . FEF-QT-DONTCARE))))
		 (SETQ EVALCODE (CDR TM))
		 (GO B))
		((SETQ TM (ASSQ (CAR X)
				'((&FUNCTIONAL . FEF-FUNCTIONAL-ARG))))
		 (PUSH (CDR TM) MISC-TYPES)
		 (GO B))
		((EQ (CAR X) '&SPECIAL)
		 (SETQ SPECIALNESS T)
		 (GO B))
		((EQ (CAR X) '&LOCAL)
		 (SETQ SPECIALNESS NIL)
		 (GO B))
		((MEMQ (CAR X) LAMBDA-LIST-KEYWORDS)
		 (GO B)))
	  ;; LAMBDA-list keywords have jumped to B.
	  ;; Get here when (CAR X) is a variable or (var init).
	  (SETQ VARN (COND ((ATOM (CAR X)) (CAR X)) (T (CAAR X))))
	  (AND PARALLEL	       
	       (NOT (OR (STRING-EQUAL VARN '|IGNORE|)
			(STRING-EQUAL VARN '|IGNORED|)
			(NULL VARN)))
	       (DOLIST (X1 (CDR X))
		 (COND ((OR (EQ X1 VARN)
			    (AND (NOT (ATOM X1)) (EQ (CAR X1) VARN)))
			(RETURN T))))
	       (BARF VARN '|duplicated in lambda-list| 'WARN))
	  (AND (= (AREF (GET-PNAME VARN) 0) #/&)
	       (BARF VARN '|probably mispelled keyword| 'WARN))
	  (COND ((AND IGNORE-NIL-P (NULL VARN))
		 (P1 (CADAR X)))		;Out of order, but works in these simple cases
		((OR (NULL VARN) (EQ VARN T))
		 (BARF VARN '|bound| 'WARN))
		(T
		 ;; Make the variable's home.
		 (IF SPECIALNESS
		     (PUSH `(SPECIAL ,(CAR X)) LOCAL-DECLARATIONS))
		 (PUSH (P1BINDVAR (CAR X) KIND EVALCODE MISC-TYPES)
		       MYVARS)))
	  (SETQ MISC-TYPES NIL)
       B
	  (SETQ X (CDR X))
	  (GO A))

    ;; Arguments should go on ALLVARS now, so all args precede all boundvars.
    (OR (EQ KIND 'FEF-ARG-INTERNAL-AUX)
	(EQ KIND 'FEF-ARG-AUX)
	(SETQ ALLVARS (APPEND SPECIFIED-FLAGS MYVARS ALLVARS)))
    (MAPC 'VAR-COMPUTE-INIT SPECIFIED-FLAGS (CIRCULAR-LIST NIL))

    ;; Now do pass 1 on the initializations for the variables.
    (DO ((ACCUM)
	 (VS (REVERSE MYVARS) (CDR VS)))
	((NULL VS)
	 ;; If parallel binding, put all var homes on VARS
	 ;; after all the inits are thru.
	 (COND (PARALLEL
		(SETQ VARS (APPEND MYVARS VARS))
		(COND ((OR (EQ KIND 'FEF-ARG-INTERNAL-AUX)
			   (EQ KIND 'FEF-ARG-AUX))
		       (MAPC 'VAR-CONSIDER-OVERLAP MYVARS)
		       (SETQ ALLVARS (APPEND MYVARS ALLVARS))))))
	 (NREVERSE ACCUM))
      (PUSH (VAR-COMPUTE-INIT (CAR VS) PARALLEL) ACCUM)
      ;; For sequential binding, put each var on VARS
      ;; after its own init.
      (OR PARALLEL
	  (PROGN (COND ((OR (EQ KIND 'FEF-ARG-INTERNAL-AUX)
			    (EQ KIND 'FEF-ARG-AUX))
			(VAR-CONSIDER-OVERLAP (CAR VS))
			(PUSH (CAR VS) ALLVARS)))
		 (PUSH (CAR VS) VARS)
		 (LET ((TEM (CDDR (VAR-INIT (CAR VS)))))
		   (AND TEM (PUSH TEM VARS))))))))

;Create a home for a variable.
;We fill the variable's INIT slot with a list whose car is the init form
;and whose cadr may be the supplied-flag-name, or with nil if there is no init at all,
;rather than what is ultimately to go there (which gets there in VAR-COMPUTE-INIT).
(DEFUN P1BINDVAR (VARSPEC KIND EVAL-TYPE MISC-TYPES)
  (LET (TYPE INIT-SPECS)
    (COND ((NOT (ATOM VARSPEC))
	   (SETQ INIT-SPECS (CDR VARSPEC))
	   (SETQ VARSPEC (CAR VARSPEC))))
    (IF (OR (EQ VARSPEC NIL) (EQ VARSPEC T))
	(BARF VARSPEC '|was used as a bound variable| 'WARN)
	;; If this variable is an optional arg with a specified-flag,
	;; remember to make a home for the flag as well.
	(AND (CADR INIT-SPECS)
	     (COND ((NEQ KIND 'FEF-ARG-OPT)
		    (BARF VARSPEC '|has a specified-flag but isn't an optional arg|
			  'WARN))
		   ((NOT (EQ (TYPEP (CADR INIT-SPECS)) 'SYMBOL))
		    (BARF VARSPEC '|has a specified-flag name which isn't a symbol|
			  'WARN))
		   (T
		    (PUSH (CREATE-SPECIFIED-FLAG-HOME (CADR INIT-SPECS)) SPECIFIED-FLAGS))))
	(COND ((NOT (EQ (TYPEP VARSPEC) 'SYMBOL))
	       (BARF VARSPEC '|non-atomic variable name| 'DATA)))
	(SETQ TYPE (FIND-TYPE VARSPEC))
	(COND ((MEMQ TYPE '(FEF-SPECIAL FEF-REMOTE)) (SETQ SPECIALFLAG T)))
	(VAR-MAKE-HOME VARSPEC TYPE KIND INIT-SPECS
		       EVAL-TYPE MISC-TYPES))))

;Make a home for the "specified-flag" of an optional variable
;(such as, FOOP in &OPTIONAL (FOO 69 FOOP)).
;It is marked with FEF-ARG-SPECIFIED-FLAG in the misc flags.
;This home is pushed on VARS right after the last argument, before
;the first actual aux variable, and also before any locals bound
;in initializations of optionals, and its scope is the entire function.
;It is of kind "aux" and initialized to the constant T
;regardless of the fact that TLFUNINIT is already set and so
;(usually) only FEF-INI-COMP-C is allowed at this point.
(DEFUN CREATE-SPECIFIED-FLAG-HOME (NAME)
  (VAR-MAKE-HOME NAME (FIND-TYPE NAME)
		 'FEF-ARG-AUX '('T)
		 'FEF-QT-DONTCARE '(FEF-ARG-SPECIFIED-FLAG)))

(DEFUN SPECIALP (SYMBOL)
  (OR (MEMQ SYMBOL BARF-SPECIAL-LIST)
      (DO ((LDS LOCAL-DECLARATIONS (CDR LDS))
	   (TEM)
	   (FIRST-P T))
	  (NIL)
	(AND (NULL LDS)
	     (IF FIRST-P (SETQ FIRST-P NIL LDS FILE-LOCAL-DECLARATIONS)
		(RETURN (OR ALL-SPECIAL-SWITCH
			    (GET SYMBOL 'SPECIAL)
			    (IF-FOR-LISPM (GET SYMBOL 'SYSTEM-CONSTANT))
			    (IF-FOR-LISPM (AND (SETQ TEM (CDR (PACKAGE-CELL-LOCATION SYMBOL)))
					       (MEMQ TEM SPECIAL-PKG-LIST)))))))
	(AND (MEMQ (CAAR LDS) '(SPECIAL UNSPECIAL))
	     (MEMQ SYMBOL (CDAR LDS))
	     (RETURN (EQ (CAAR LDS) 'SPECIAL))))))

(DEFUN FIND-TYPE (X) (COND ((SPECIALP X) 'FEF-SPECIAL) ;REMOTE???
			   (T 'FEF-LOCAL)))
                                               
;Construct and return a variable home to go on VARS and ALLVARS.
;This home has, in the VAR-INIT slot, not what is supposed to be there
;but the actual initialization-form for the variable.
;Later, VAR-COMPUTE-INIT is called to fix that up.
(DEFUN VAR-MAKE-HOME (NAME TYPE KIND INIT-SPECS EVAL-TYPE MISC-TYPES &AUX HOME)
    (COND ((NULL (MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST
                                          FEF-ARG-AUX FEF-ARG-INTERNAL-AUX)))
           (BARF KIND 'BAD-KIND 'BARF)))
    ;; Rest args interfere with fast arg option except when there are no specials.
    ;; We need to look at this to
    ;;  decide how to process all the AUX variables and can't tell when processing
    ;;  the first one whether the next will be special.
    ;;  In any case, being wrong about this should not be able to produce
    ;;  incorrect code.
    (COND ((EQ KIND 'FEF-ARG-REST)
           (SETQ FAST-ARGS-POSSIBLE NIL)))
    (COND ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
           (AND INIT-SPECS (SETQ FAST-ARGS-POSSIBLE NIL))))
    ;; Detect vars bound to themselves which fail to be special.
    (COND ((AND (EQ NAME (CAR INIT-SPECS))
                (NOT (ASSQ NAME VARS)))
           (MSPL2 NAME)
           (SETQ TYPE 'FEF-SPECIAL)))
    ;; Cons up the variable descriptor.
    (SETQ HOME (LIST NAME KIND TYPE 0
		     NIL			;Lap address will be assigned later.
		     INIT-SPECS			;Init info not determined yet; store init form.
		     EVAL-TYPE MISC-TYPES
		     NIL NIL))
    (SETF (VAR-LAP-ADDRESS HOME)
	  ;; Not the real lap address,
	  ;; but something for P1 to use for the value of the variable
	  (IF (EQ TYPE 'FEF-SPECIAL) NAME `(LOCAL-REF ,HOME)))
    HOME)

;; For a variable whose scope is ready to begin (it's about to be put on VARS),
;; look for another variable whose scope already ended, to share a slot with.
;; If we find a suitable one, just clobber it in.
(DEFUN VAR-CONSIDER-OVERLAP (VAR)
  (AND (EQ (VAR-KIND VAR) 'FEF-ARG-INTERNAL-AUX)
       (DO ((VS ALLVARS (CDR VS)))
	   ((NULL VS))
	 ;; Look for other vars with the same name;
	 ;; for a gensym, look for another gensym.
	 (AND (OR (EQ (VAR-NAME VAR) (CAAR VS))
		  (AND (NULL (SYMBOL-PACKAGE (CAAR VS)))
		       (NULL (SYMBOL-PACKAGE (VAR-NAME VAR)))))
	      ;; But don't try to overlap a local with a special that happens to have the same
	      ;; name.
	      (NEQ (VAR-TYPE (CAR VS)) 'FEF-SPECIAL)
	      ;; And don't overlap with arguments
	      ;; (in (LAMBDA (&OPTIONAL (A (LET (B)...)) B) ...) we might otherwise try to do it)
	      (EQ (VAR-KIND (CAR VS)) 'FEF-ARG-INTERNAL-AUX)
	      ;; Insist on a slot that represents a canonical home (does not
	      ;; map to another slot), and that is not currently in use
	      (NOT (OR (VAR-OVERLAP-VAR (CAR VS))
		       (DOLIST (V VARS)
			 (AND (OR (EQ V (CAR VS))
				  (EQ (VAR-OVERLAP-VAR V) (CAR VS)))
			      (RETURN T)))))
	      (RETURN (SETF (VAR-OVERLAP-VAR VAR) (CAR VS)))))))


;; After the end of pass 1, assign lap addresses to the variables.
(DEFUN ASSIGN-LAP-ADDRESSES ()
    (SETQ ARGN 0)	;Next argument number.
    (SETQ LVCNT 0)	;Next slot in local block.  Count rest arg, auxes,
                        ;and internal-auxes if they are not special.
    (SETQ ARG-MAP NIL)  ;We also build the arg map and local map,
    (SETQ LOCAL-MAP NIL) ;pushing things on in reverse order.
    (DOLIST (V (REVERSE ALLVARS))
      ;; Cons up the expression for Lap to use to refer to this variable.
      (LET ((TYPE (VAR-TYPE V))
            (KIND (VAR-KIND V))
	    (NAME (VAR-NAME V))
	    PERMANENT-NAME)
        (SETF-VAR-LAP-ADDRESS V
              (COND ((EQ TYPE 'FEF-SPECIAL)
                     `(SPECIAL ,NAME))
                    ((EQ TYPE 'FEF-REMOTE)
                     `(REMOTE ,NAME))
                    ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
                     `(ARG ,ARGN))
		    ((VAR-OVERLAP-VAR V)
		     (VAR-LAP-ADDRESS (VAR-OVERLAP-VAR V)))
                    (T `(LOCBLOCK ,LVCNT))))
	;; If the name is a gensym that is in the temporary area, don't put it in the
	;; arg/local map.  This is partly to avoid putting all these stupid gensyms
	;; into the qfasl file, but the real reason is to avoid the dreaded scourge
	;; of temporary area lossage in the error handler.
	(SETQ PERMANENT-NAME (IF (= (%AREA-NUMBER NAME) FASD-TEMPORARY-AREA)
				 NIL
				 NAME))
        ;; Now increment one or more of the counters of variables
        ;; and maybe make an entry on LOCAL-MAP or ARG-MAP
        (COND ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
               (PUSH (AND PERMANENT-NAME (LIST PERMANENT-NAME)) ARG-MAP)
               (AND (= (SETQ ARGN (1+ ARGN)) 101)
                    (BARF NAME '|More than 100 arguments accepted by one function| 'DATA)))
              ((OR (EQ TYPE 'FEF-LOCAL)
		   (NOT (MEMQ KIND '(FEF-ARG-INTERNAL FEF-ARG-INTERNAL-AUX))))
	       (COND ((NOT (VAR-OVERLAP-VAR V))
		      (PUSH (AND PERMANENT-NAME (LIST PERMANENT-NAME)) LOCAL-MAP)
		      (AND (= (SETQ LVCNT (1+ LVCNT)) 101)
			   (BARF NAME '|More than 100 local variable slots in one function|
				 'DATA)))
		     (T (LET ((L1 (NTHCDR (- (LENGTH LOCAL-MAP)
					     (CADR (VAR-LAP-ADDRESS V))
					     1)
					  LOCAL-MAP)))
			  (OR (NULL PERMANENT-NAME)
			      (NULL (SYMBOL-PACKAGE NAME))
			      (MEMQ NAME (CAR L1))
			      (PUSH NAME (CAR L1))))))))))
    (DOLIST (V ALLVARS)		;Fix FIXE's put in by VAR-COMPUTE-INIT
      (AND (EQ (CAR (VAR-INIT V)) 'FEF-INI-EFF-ADR)
	   (EQ (CAADR (VAR-INIT V)) 'FIXE)
	   (SETF (CADADR (VAR-INIT V)) (VAR-LAP-ADDRESS (CADR (CADADR (VAR-INIT V)))))))
    (SETQ LOCAL-MAP (NREVERSE LOCAL-MAP)
          ARG-MAP (NREVERSE ARG-MAP))
    ;; Clobber all nonspecial varnames in elements of
    ;; CLOBBER-NONSPECIAL-VARS-LISTS with NIL.
    ;; Clobber away all-NIL tails of those lists with NIL.
    (DOLIST (L CLOBBER-NONSPECIAL-VARS-LISTS)
      (LET ((LAST-NON-NIL-PTR L))
	(DO ((L1 L (CDR L1)))
	    ((NULL L1))
	  (LET ((HOME (ASSQ (CAR L1) ALLVARS)))
	    (IF (AND HOME (EQ (VAR-TYPE HOME) 'FEF-LOCAL))
		(RPLACA L1 NIL)
		(SETQ LAST-NON-NIL-PTR L1))))
	(RPLACD LAST-NON-NIL-PTR NIL))))

;Given a variable home, compute its VAR-INIT and install it.
;When we are called, the VAR-INIT contains the data for us to work on
;which looks like (init-form arg-supplied-flag-name).
;Note that for a FEF-ARG-INTERNAL-AUX variable, the init-type will
;always be FEF-INI-COMP-C.
;At time of call, VARS should be bound to the environment for
;execution of the init form for this variable.
(DEFUN VAR-COMPUTE-INIT (HOME PARALLEL)
  (PROG (INIT-SPECS INIT-FORM INIT-TYPE INIT-DATA SPECIFIED-FLAG-NAME NAME KIND TYPE)
	(SETQ NAME (VAR-NAME HOME)
	      KIND (VAR-KIND HOME)
	      TYPE (VAR-TYPE HOME)
	      INIT-SPECS (VAR-INIT HOME)
	      INIT-FORM (CAR INIT-SPECS)
	      SPECIFIED-FLAG-NAME (CADR INIT-SPECS))
    (COND ((NULL INIT-FORM))
	  ((CONSTANTP INIT-FORM)
	   (SETQ INIT-FORM `',INIT-FORM))
	  ((AND (NOT (ATOM INIT-FORM))
		(EQ (CAR INIT-FORM) 'QUOTE)))
	  (T
	   ;; Init is not NIL, constant or self => must P1 it, and maybe set TLFUNINIT.
	   (LET ((TLEVEL NIL))
	     (SETQ INIT-FORM (P1 INIT-FORM)))
	   (COND ((NOT (ADRREFP INIT-FORM))
		  (SETQ TLFUNINIT T)))))
    ;; Now that we have processed the init form, determine the ADL initialization field.
    ;; First, must we, or would we rather, use code to initialize the variable?
    ;; Note: specified-flags MUST be initted at entry time regardless of anything else.
    (COND ((AND (NOT (MEMQ 'FEF-ARG-SPECIFIED-FLAG (VAR-MISC HOME)))
		(OR (EQ KIND 'FEF-ARG-INTERNAL-AUX) TLFUNINIT
		    ;; Don't spoil the fast arg option with nontrivial inits for aux's. 
		    (AND (EQ KIND 'FEF-ARG-AUX)
			 FAST-ARGS-POSSIBLE
			 (NOT (MEMBER INIT-FORM '(NIL 'NIL))))
		    (COND (PARALLEL (NEQ TYPE 'FEF-LOCAL)))))
	   (SETQ INIT-TYPE 'FEF-INI-COMP-C)
	   ;; Note: if we are initting by code, there is no advantage
	   ;; in binding at function entry, and doing so would
	   ;; make lap stupidly turn off the fast arg option!
	   (AND (EQ KIND 'FEF-ARG-AUX)
		(SETF-VAR-KIND HOME (SETQ KIND 'FEF-ARG-INTERNAL-AUX)))
	   (SETQ TLFUNINIT T)))
    ;; If we aren't forced already not to use an init, figure out
    ;; what type of init to use if there's no init-form: either "none" or "nil".
    (OR INIT-TYPE
	(SETQ INIT-TYPE
	      (COND ((OR (EQ KIND 'FEF-ARG-OPT)
			 (AND (EQ KIND 'FEF-ARG-AUX)
			      (MEMQ TYPE '(FEF-SPECIAL FEF-REMOTE))))
		     'FEF-INI-NIL)
		    (T 'FEF-INI-NONE))))
    ;; Then, if there is an init form, gobble it.
    (COND ((AND INIT-FORM (NEQ INIT-TYPE 'FEF-INI-COMP-C))
	   (COND ((NOT (MEMQ KIND
			     '(FEF-ARG-OPT FEF-ARG-AUX FEF-ARG-INTERNAL-AUX)))
		  (BARF NAME
			'|initialization specified for mandatory argument|
			'WARN))
		 ;; There's a hack for binding a special var to itself.
		 ((AND (EQ NAME INIT-FORM)
		       (NEQ TYPE 'FEF-LOCAL))
		  (SETQ INIT-TYPE 'FEF-INI-SELF))
		 ((ATOM INIT-FORM)
		  (SETQ INIT-TYPE 'FEF-INI-C-PNTR)
		  (SETQ INIT-DATA (LIST 'LOCATIVE-TO-S-V-CELL INIT-FORM)))
		 ((EQ (CAR INIT-FORM) 'LOCAL-REF)
		  (SETQ INIT-TYPE 'FEF-INI-EFF-ADR) ;Initted to value of local var
		  (SETQ INIT-DATA (LIST 'FIXE INIT-FORM)))
		 ((MEMQ (CAR INIT-FORM) '(QUOTE FUNCTION BREAKOFF-FUNCTION))
		  (SETQ INIT-TYPE 'FEF-INI-PNTR)
		  (SETQ INIT-DATA INIT-FORM))
		 (T (BARF INIT-FORM '|init-form calculation confused| 'BARF)))))
    (COND ((AND (EQ KIND 'FEF-ARG-OPT)
		(OR TLFUNINIT SPECIFIED-FLAG-NAME))
	   ;; Once an opt arg gets an alternate starting address,
	   ;; all following args must be similar or else FEF-INI-COMP-C.
	   (SETQ TLFUNINIT T)
	   (SETQ INIT-TYPE 'FEF-INI-OPT-SA)
	   (SETQ INIT-DATA (GENSYM)))
	  ;; If something not an optional arg was given a specified-flag,
	  ;; discard that flag now.  There has already been an error message.
	  (T (SETQ SPECIFIED-FLAG-NAME NIL)))
    (SETF-VAR-INIT HOME (LIST* INIT-TYPE INIT-DATA
			       (AND SPECIFIED-FLAG-NAME
				    (DOLIST (V ALLVARS)
				      (AND (EQ (VAR-NAME V) SPECIFIED-FLAG-NAME)
					   (MEMQ 'FEF-ARG-SPECIFIED-FLAG (VAR-MISC V))
					   (RETURN V))))))
    (RETURN (COND ((NULL INIT-FORM) NAME) (T (LIST NAME INIT-FORM))))))

;; (MULTIPLE-VALUE-BIND variable-list m-v-returning-form . body)
;; turns into (MULTIPLE-VALUE-BIND variable-list vars-segment m-v-returning-form . body)
;; where vars-segment is a sublist of VARS that should be pushed onto VARS
;; while this form is being processed on pass 2.

(DEFUN (MULTIPLE-VALUE-BIND P1) (FORM)
  (LET ((VARIABLES (CADR FORM))
	(VARS VARS) BODY
	(M-V-FORM (CADDR FORM)))
    (SETQ TLEVEL NIL)
    ;; P1 the m-v-returning-form outside the bindings we make.
    (SETQ M-V-FORM (P1 M-V-FORM))
    ;; The code should initialize each variable by popping off the stack.
    ;; The values will be in forward order so we must pop in reverse order.
    (SETQ VARIABLES (MAPCAR #'(LAMBDA (V) `(,V (%POP))) VARIABLES))
    (P1SBIND VARIABLES 'FEF-ARG-INTERNAL-AUX T T)
    (SETQ BODY (P1PROGN-1 (CDDDR FORM)))
    `(,(CAR FORM) ,VARIABLES ,VARS ,M-V-FORM . ,BODY)))

;;; If this wasn't here the compiler would be confused into not compiling the arguments
(DEFUN (PROGN P1) (FORM)
  (CONS 'PROGN (MAPCAR 'P1 (CDR FORM))))

(DEFUN (UNWIND-PROTECT P1) (FORM)
  (CONS 'UNWIND-PROTECT (MAPCAR 'P1 (CDR FORM))))

;Analyze a prog's variable bindings and tags,
;and convert it to an internal form which looks like
;(SPROG <variable list, with keywords processed and removed>
;      <value of VARS for body of this prog>
;      <segment of GOTAGS for tags in body of this prog (not incl. outer progs)>
;      <name of this prog, or NIL>
;      <T if BIND used within this prog>
;      . <body, P1'ified>)
;Note that ALLGOTAGS contains a list of all tags seen in this function so far.
;We use it to determine whether we should
;rename a tag because it is shadowing an outer one.

;Since there is a confusion about whether PROG should compute all
;its initializations and then bind all the variables ("parallel binding"),
;or process one variable completely at a time ("sequential binding"),
;the compiler understands two forms of PROG:  SPROG and PPROG.
;SPROG does sequential binding, and PPROG does parallel binding.
;P1LAMBDA, P1AUX and DOEXPANDER all generate PPROG or SPROG as appropriate.
;PROG and PROG* are converted to PPROG and SPROG so that the decision
;of which of them is which is localized entirely within this function.

(DEFUN P1PROG (FORM)
    (LET ((PROGNAME) (VARS VARS) (GOTAGS)
	  (FN (CAR FORM)) (P1VALUE NIL) (BINDP) (BODY) (VLIST))
	 (SETQ FORM (CDR FORM))
	 ;; Extract the prog name if there is one.
	 (COND ((AND (CAR FORM)
                     (EQ (TYPEP (CAR FORM)) 'SYMBOL))
                (SETQ PROGNAME (CAR FORM))
                (SETQ FORM (CDR FORM))))
	 (COND ((EQ FN 'PROG)
		(SETQ FN 'PPROG))
	       ((EQ FN 'PROG*)
		(SETQ FN 'SPROG)))
	 (SETQ VLIST (CAR FORM))
         ;; Treat parallel binding as serial if it doesn't matter.
         (OR (CDR VLIST) (SETQ FN 'SPROG))
         (AND (EQ FN 'PPROG)
              (DO ((XX VLIST (CDR XX)))
                  ((NULL XX) (SETQ FN 'SPROG))
		;; Namely, if binding each symbol to NIL, a constant, or itself.
		(OR (ATOM (CAR XX))
		    (CONSTANTP (CADAR XX))
		    (QUOTEP (CADAR XX))
		    (EQ (CAAR XX) (CADAR XX))
		    (RETURN NIL))))
	 ;; Flush rebinding a var to itself if it isn't special
	 ;; and range of rebinding is rest of function.
	 (AND TLEVEL
	      (SETQ VLIST
		    (SUBSET-NOT #'(LAMBDA (VAR)
				    (AND (NOT (ATOM VAR))
					 (EQ (CAR VAR) (CADR VAR))
					 (EQ (VAR-TYPE (ASSQ (CAR VAR) VARS)) 'FEF-LOCAL)))
				VLIST)))
	 (SETQ VLIST (P1SBIND VLIST
			      (COND (TLEVEL 'FEF-ARG-AUX)
				    (T 'FEF-ARG-INTERNAL-AUX))
			      (EQ FN 'PPROG)
			      NIL))
         (SETQ BODY (CDR FORM))
	 ;; Now convert initial SETQs to variable initializations.
	 ;; We win only for SETQs of variables bound but with no initialization spec'd,
	 ;; which set them to constant values, and only if later vars' inits didn't use them.
	 ;; When we come to anything other than a SETQ we can win for, we stop.
	 ;; For SPROG, we can't win for a special variable if anyone has called a function
	 ;; to do initting, since that function might have referred to the special.
	 ;; Even if we don't use tha ADL to init them,
	 ;; we avoid redundant settings to NIL.
	 (DO ((TEM) (HOME)) (NIL)
	    (COND ((EQUAL (CAR BODY) '(SETQ))
		   (SETQ BODY (CDR BODY)))
		  ((OR (ATOM (CAR BODY))
		       (ATOM (SETQ TEM (OPTIMIZE (CAR BODY) NIL)))
		       (NEQ (CAR TEM) 'SETQ)
		       (NOT (P1PROG-VAR-FIND (CADR TEM) VLIST))
		       (NOT (OR (CONSTANTP (CADDR TEM))
				(AND (NOT (ATOM (CADDR TEM)))
				     (EQ (CAADDR TEM) 'QUOTE))))
		       (AND (SPECIALP (CADR TEM))
			    (OR TLFUNINIT (NOT TLEVEL))
			    (EQ FN 'SPROG))
		       (NOT (ZEROP (VAR-USE-COUNT (SETQ HOME (ASSQ (CADR TEM) VARS))))))
		   (RETURN NIL))
		  (T (SETQ BODY (CONS (CONS 'SETQ (CDDDR TEM)) (CDR BODY)))
		     (RPLACA (MEMQ (CADR TEM) VLIST)
			     `(,(CADR TEM) ,(P1 (CADDR TEM))))
		     ;; For a variable bound at function entry, really set up its init.
		     ;; Other vars (FEF-ARG-INTERNAL-AUX) will be initted by code,
		     ;; despite our optimization, but it will be better code.
		     (AND TLEVEL (EQ (VAR-KIND HOME) 'FEF-ARG-AUX)
			  (SETF-VAR-INIT HOME `(FEF-INI-PNTR ,(P1 (CADDR TEM))))))))

	 ;; Now P1 process what is left of the body.
	 (AND (CDR BODY) (SETQ TLEVEL NIL))
	 (SETQ BODY (MAPCAR (FUNCTION (LAMBDA (STMT)
			         (COND ((EQ (TYPEP STMT) 'SYMBOL) (P1TAGAD STMT))
				       (T (P1 STMT)))))
			    BODY))
	 (LET ((RETTAG (GENSYM)))
	      ;; Push on GOTAGS a description of this prog's "return tag",
	      ;; a tag we generate and stick at the end of the prog.
	      (PUSH (MAKE-GOTAG RETTAG RETTAG) GOTAGS)
	      `(,FN ,VLIST ,VARS ,GOTAGS ,PROGNAME ,BINDP . ,BODY))))

;MEMQ and ASSQ together.  Find the tail of VLIST
;whose CAR or CAAR is VARNAME.
(DEFUN P1PROG-VAR-FIND (VARNAME VLIST)
    (DO ((VL VLIST (CDR VL))) ((NULL VL) NIL)
       (AND (OR (EQ VARNAME (CAR VL))
		(AND (NOT (ATOM (CAR VL)))
		     (EQ VARNAME (CAAR VL))))
	    (RETURN VL))))

(DEFUN P1TAGAD (X)
    (COND ((ASSQ X GOTAGS)
	   (AND X (BARF X '|duplicated PROG tag| 'WARN))
	   ;; Replace duplicate progtags with something that
	   ;; will be ignored by pass 2, to avoid making LAP get unhappy.
	   '(QUOTE NIL))
	  (T (PUSH X ALLGOTAGS)
	     (PUSH (MAKE-GOTAG X (COND ((MEMQ X ALLGOTAGS) (GENSYM)) (T X)))
		   GOTAGS)
	     X)))

(DEFPROP GO P1GO P1)
(DEFUN P1GO (FORM) FORM)

(DEFPROP RETURN-FROM P1RETURN-FROM P1)
(DEFUN P1RETURN-FROM (FORM)
    (CONS 'RETURN-FROM (CONS (CADR FORM) (MAPCAR 'P1V (CDDR FORM)))))

(DEFUN (LET P1) (FORM)
  (P1 `(PROG T ,(CADR FORM) (RETURN-FROM-T (PROGN . ,(CDDR FORM))))))

(DEFUN (LET* P1) (FORM)
  (P1 `(PROG* T ,(CADR FORM) (RETURN-FROM-T (PROGN . ,(CDDR FORM))))))

;Turn an internal lambda containing &AUX variables
;into one containing an SPROG and having no &AUX variables.
(DEFUN P1AUX (LAMBDA)
    (PROG (STANDARDIZED AUXVARS)
	  (SETQ STANDARDIZED
		(COND ((EQ (CAR LAMBDA) 'NAMED-LAMBDA) (CDR LAMBDA))
		      (T LAMBDA)))
	  (OR (SETQ AUXVARS (MEMQ '&AUX (CADR STANDARDIZED)))
	      (RETURN LAMBDA))
	  (RETURN `(LAMBDA ,(LDIFF (CADR STANDARDIZED) AUXVARS)
			   (SPROG T ,(CDR AUXVARS)
				  (INHIBIT-STYLE-WARNINGS
                                     (RETURN-FROM-T (PROGN . ,(CDDR STANDARDIZED)))))))))

;Turn a call to an internal lambda into a prog, and return P1 of that prog.
;All &AUX variables in the lambda list are extracted by P1AUX.
;We generate a PPROG, since the lambda variables should all be computed and then bound.
;This means that &OPTIONALs don't work quite right;
;but they never used to work at all in internal lambdas anyway.
;The PPROG is named T so that RETURNs to the user's PROGs aren't screwed up.
;No checking of number of args here, because it is done elsewhere.
;We just eval and ignore extra args and take missing ones to be NIL.
(DEFUN P1LAMBDA (LAMBDA ARGS)
    (PROG (ARGLIST BODY ARGS1 OPTIONAL PROGVARS VAR)
	  (SETQ LAMBDA (P1AUX LAMBDA))
	  (AND (EQ (CAR LAMBDA) 'NAMED-LAMBDA)
	       (SETQ LAMBDA (CDR LAMBDA)))
	  (SETQ ARGLIST (CADR LAMBDA) BODY (CDDR LAMBDA))
	  (SETQ ARGS1 ARGS)
          (DO ((ARGLIST1 ARGLIST (CDR ARGLIST1)))
              (NIL)
              (SETQ VAR (CAR ARGLIST1))
              (COND ((NULL ARGLIST1)
                     (RETURN T))
		    ((EQ VAR '&KEY)
		     (BARF ARGLIST "&KEY in internal lambda" 'WARN))
                    ((EQ VAR '&REST)
                     (POP ARGLIST1)
                     (PUSH (LIST (CAR ARGLIST1) `(LIST . ,ARGS1)) PROGVARS)
                     (RETURN (SETQ ARGS1 NIL)))
                    ((EQ VAR '&OPTIONAL)
                     (SETQ OPTIONAL T))
                    ((MEMQ VAR LAMBDA-LIST-KEYWORDS))
                    (T (COND ((SYMBOLP VAR)
			      (PUSH (LIST VAR (CAR ARGS1)) PROGVARS))
			     (T
			      (COND ((NOT OPTIONAL)
				     (BARF ARGLIST
					   "Initialization given for non-optional argument in internal lambda"
					   'WARN)))
			      (PUSH (LIST (CAR VAR)
					  (COND (ARGS1 (CAR ARGS1))
						(T (CADR VAR)))) PROGVARS)))
                       (POP ARGS1))))
	  (RETURN (P1 `(PPROG T ,(NREVERSE PROGVARS)
			      ,@ARGS1    ;Eval any extra unnecessary args!
			      (INHIBIT-STYLE-WARNINGS (RETURN-FROM-T (PROGN . ,BODY))))))))

(DEFPROP COND P1COND P1)
(DEFUN P1COND (X)
       (CONS
	'COND
	(COND
	 ((ATOM (CDR X))
	  (BARF X '|atomic COND body| 'DATA))
	 (T (MAPCAR (FUNCTION P1COND-1) (CDR X))))))

(DEFUN P1COND-1 (FORMS)
  (IF (ATOM FORMS)
      (BARF FORMS "Atomic cond pair" 'DATA)
      (P1PROGN-1 FORMS)))

(ADD-OPTIMIZER PROGN 1-ARG-NO-OP)
(DEFUN P1PROGN (X)
    (SETQ TLEVEL NIL)
    (CONS (CAR X) (P1PROGN-1 (CDR X))))

(DEFUN P1PROGN-1 (FORMS)
    (DO ((FORMS-LEFT (SETQ FORMS (APPEND FORMS NIL)) (CDR FORMS-LEFT)))
	((NULL FORMS-LEFT) FORMS)
       (LET ((P1VALUE P1VALUE))
	  (AND (CDR FORMS-LEFT) (SETQ P1VALUE NIL))
	  (RPLACA FORMS-LEFT (P1 (CAR FORMS-LEFT))))))

(DEFPROP MULTIPLE-VALUE P1-MULTIPLE-VALUE P1)
(DEFUN P1-MULTIPLE-VALUE (FORM)
    (AND (CDDDR FORM)
	 (BARF FORM '|too many arguments| 'WARN))
    (LIST 'MULTIPLE-VALUE
	  (MAPCAR 'P1SETVAR (CADR FORM))
	  (P1V (CADDR FORM))))

(DEFPROP MULTIPLE-VALUE-LIST ((1 (FEF-ARG-REQ FEF-QT-EVAL))) ARGDESC)
		;In pass 1, pretend this isn't a special form

(DEFPROP MULTIPLE-VALUE-RETURN ((1 (FEF-ARG-REQ FEF-QT-EVAL))) ARGDESC)

(DEFPROP SETQ P1SETQ P1)
(DEFUN P1SETQ (FORM)
    (CONS 'SETQ (P1SETQ-1 (CDR FORM))))

(DEFUN P1SETQ-1 (PAIRS)
    (COND ((NULL PAIRS) NIL)
	  ((NULL (CDR PAIRS))
	   (BARF PAIRS '|odd number of args to SETQ| 'WARN))
	  ((OR (NULL (CAR PAIRS)) (EQ (CAR PAIRS) T))
	   (BARF (CAR PAIRS) '|being SETQ'd| 'WARN))
	  (T
	   (CONS (P1SETVAR (CAR PAIRS))
		 (CONS (P1V (CADR PAIRS)) (P1SETQ-1 (CDDR PAIRS)))))))

(DEFUN P1SETVAR (VAR)
  (COND ((NULL VAR) NIL)	;FOR MULTIPLE-VALUE
	((NOT (SYMBOLP VAR))
	 (BARF VAR '|attempt to SETQ something not a symbol| 'WARN)
	 NIL)
	(T (P1 VAR))))

;Given an entry on VARS, increment the usage count.
(DEFUN VAR-INCREMENT-USE-COUNT (VAR)
    (RPLACA (CDDDR VAR) (1+ (CADDDR VAR))))

;COMPILER-LET must be renamed to COMPILER-LET-INTERNAL
;by an "optimizer" so that its normal definition as a macro is bypassed.
(ADD-OPTIMIZER COMPILER-LET COMPILER-LET-INTERNALIZE)
(DEFUN COMPILER-LET-INTERNALIZE (FORM)
    `(COMPILER-LET-INTERNAL . ,(CDR FORM)))

;(compiler-let ((var form) (var form) ...) body...)
(DEFPROP COMPILER-LET-INTERNAL P1COMPILER-LET-INTERNAL P1)
(DEFUN P1COMPILER-LET-INTERNAL (FORM)
  (PROGV (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X))) (CADR FORM))
	 (MAPCAR #'(LAMBDA (X) (IF (ATOM X) NIL (EVAL (CADR X)))) (CADR FORM))
    (P1 (IF (CDDDR FORM) (CONS 'PROGN (CDDR FORM)) (CADDR FORM)))))

(DEFPROP FUNCTION P1FUNCTION P1)
(DEFUN P1FUNCTION (FORM)
    (COND ((SYMBOLP (CADR FORM)) FORM)		;Functional variable
	  ((FUNCTIONP (CADR FORM))		;Functional constant
	   (OR (MAYBE-BREAKOFF (CADR FORM)) (LIST 'QUOTE (CADR FORM))))
	  ((VALIDATE-FUNCTION-SPEC (CADR FORM))	;Function spec
	   FORM)
	  (T (BARF FORM "Contains neither a function nor the name of a function" 'DATA))))

(DEFUN (LEXICAL-CLOSURE P1) (FORM)
  (COND ((ATOM (CADR FORM)) `(FUNCTION ,(CADR FORM)))
	((MAYBE-BREAKOFF-LEXICAL-CLOSURE (CADR FORM)))
	(T (P1FUNCTION FORM))))

;FUNCTIONAL-ALIST IS LIKE QUOTE, BUT BREAKS OFF ANY FUNCTIONS IT FINDS
; IN THE CDR OR CADR POSITION OF AN ALIST ELEMENT.
;;;??? This does not work any more, and is hard to make work
;;;??? now that broken off functions don't have symbols for names.
;;;??? People expect their definitions to be found in slots in the
;;;??? main function's fef; but the functions in this list are not
;;;??? pointed to from anywhere in the main function's fef.
(DEFPROP FUNCTIONAL-ALIST P1FUNCTIONAL-ALIST P1)
(DEFUN P1FUNCTIONAL-ALIST (FORM)
   `(QUOTE ,(MAPCAR (FUNCTION (LAMBDA (X)
			(COND ((ATOM X) X)
			      (T (CONS (CAR X)
				       (COND ((ATOM (CDR X)) (CDR X))
					     ((ATOM (CADR X))
					      (OR (MAYBE-BREAKOFF (CDR X)) (CDR X)))
					     (T (CONS (OR (MAYBE-BREAKOFF (CADR X)) (CADR X))
						      (CDDR X)))))))))
		    (CADR FORM))))

(DEFPROP VALUE-CELL-LOCATION P1VALUE-CELL-LOCATION P1)

;Make sure that (VALUE-CELL-LOCATION 'FOO) marks FOO as "used".
(DEFUN P1VALUE-CELL-LOCATION (FORM &AUX TEM)
    (COND ((AND (NOT (ATOM (CADR FORM)))
		(EQ (CAR (CADR FORM)) 'QUOTE))
	   (COND ((NOT (SYMBOLP (CADADR FORM)))
		  (BARF FORM "Value cell location of non-symbol" 'WARN)
		  ''NIL)
		 ((SETQ TEM (ASSQ (CADADR FORM) VARS))
		  (VAR-INCREMENT-USE-COUNT TEM)
		  `(VALUE-CELL-LOCATION (QUOTE (LOCAL-REF ,TEM))))
		 ((SETQ TEM (TRY-REF-LEXICAL-VAR FORM))
		  `(VALUE-CELL-LOCATION (QUOTE ,TEM)))
		 (T FORM)))
	  (T (P1EVARGS FORM))))

(DEFPROP OR P1EVARGS P1)
(DEFPROP AND P1EVARGS P1)
(DEFUN P1EVARGS (FORM)
    (LET ((P1VALUE T))
	 (CONS (CAR FORM) (MAPCAR 'P1 (CDR FORM)))))

;Any use of BIND must set SPECIALFLAG.
(DEFPROP BIND P1BIND P1)
(DEFUN P1BIND (FORM)
    (SETQ SPECIALFLAG T)
    (SETQ BINDP T)
    (P1EVARGS FORM))

;For (CLOSURE '(X Y Z) ...), make sure that X, Y, Z are special.
(DEFPROP CLOSURE P1CLOSURE P1)
(DEFUN P1CLOSURE (FORM)
    (AND (NOT (ATOM (CADR FORM)))
	 (EQ (CAADR FORM) 'QUOTE)
	 (MAPC 'MSPL2 (CADADR FORM)))
    (P1EVARGS FORM))


;ARGDESC properties for functions with hairy eval/quote argument patterns
(DEFPROP ARRAY ((2 (FEF-ARG-REQ FEF-QT-QT)) (20 (FEF-ARG-OPT FEF-QT-EVAL))) ARGDESC)
   