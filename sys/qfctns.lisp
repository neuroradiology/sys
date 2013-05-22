;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(SPECIAL DEFAULT-CONS-AREA %NUMBER-OF-AREAS)

;;; Function and macro defining.

;This is used in this file so must be at begining of file to win at cold-load time.
(DEFUN DEF (&QUOTE FUNCTION &REST DEFINING-FORMS)
    (MAPC #'EVAL DEFINING-FORMS)
    FUNCTION)

(DEFUN DEFF (&QUOTE FUNCTION &EVAL DEFINITION)
    (FSET-CAREFULLY FUNCTION DEFINITION)
    FUNCTION)

(SPECIAL UNDO-DECLARATIONS-FLAG LOCAL-DECLARATIONS FILE-LOCAL-DECLARATIONS)

(DEFCONST *DEBUG-INFO-LOCAL-DECLARATION-TYPES*
	  '(ARGLIST RETURN-LIST FUNCTION-PARENT)
  "Local declaration types which are incorporated into the function debugging info by DEFUN")

(DEFUN DEFUN (&QUOTE &REST ARG)
  (DECLARE (ARGLIST &QUOTE SYMBOL LAMBDA-LIST &REST BODY))  ;What the user gets from ARGLIST.
  (PROG (SYM TEM (LOCAL-DECLARATIONS LOCAL-DECLARATIONS) DEBUG-INFO VARS+BODY)
	;; Extract any DECLARE from the front of the body and put it into
	;; the local declarations that are in effect.  Remove the DECLARE from the body.
	(LET ((BODY1 (CDDR ARG)))
	  (AND (STRINGP (CAR BODY1)) (POP BODY1))
	  (AND (LISTP (CAR BODY1))
	       (EQ (CAAR BODY1) 'DECLARE)
	       (SETQ LOCAL-DECLARATIONS (APPEND (CDAR BODY1) LOCAL-DECLARATIONS)
		     ARG (REMOVE (CAR BODY1) ARG 1))))
	;; Turn old Maclisp DEFUNs into standard ones, provided function to do it is loaded.
	(COND ((FBOUNDP 'DEFUN-COMPATIBILITY)
	       (SETQ ARG (DEFUN-COMPATIBILITY ARG))
	       (OR (EQ (CAR ARG) 'DEFUN)
		   (RETURN (EVAL ARG)))
	       (SETQ ARG (CDR ARG))))
	(SETQ SYM (CAR ARG) VARS+BODY (CDR ARG))
	;; Use some local declarations for making debug-info, if they are present.
	(LOOP FOR DCL IN LOCAL-DECLARATIONS
	      WHEN (MEMQ (CAR DCL) *DEBUG-INFO-LOCAL-DECLARATION-TYPES*)
	        DO (PUSH DCL DEBUG-INFO))
	;; Put whatever other local declarations there are
	;; into a DECLARE at the front of the transformed body.
	(AND LOCAL-DECLARATIONS
	     (SETQ TEM (SUBSET-NOT #'(LAMBDA (X)
				       (MEMQ (CAR X) *DEBUG-INFO-LOCAL-DECLARATION-TYPES*))
				   LOCAL-DECLARATIONS))
	     (SETQ VARS+BODY
		   `(,(CAR VARS+BODY)
		     (DECLARE . ,TEM)
		     . ,(CDR VARS+BODY))))
	;; Cons up and store a NAMED-LAMBDA.
	(FSET-CAREFULLY SYM
			`(NAMED-LAMBDA ,(IF (OR DEBUG-INFO (NOT (SYMBOLP SYM)))
					    (CONS SYM DEBUG-INFO)
					    SYM)
				       . ,VARS+BODY))
	(RETURN SYM)))

(DEFUN MACRO (&QUOTE SYMBOL &REST DEF &AUX DEF1 ARGL)
  (OR (SYMBOLP SYMBOL) (SETQ SYMBOL (STANDARDIZE-FUNCTION-SPEC SYMBOL)))
  (SETQ DEF1 `(MACRO NAMED-LAMBDA
		     ,(COND ((SETQ ARGL (ASSQ 'ARGLIST LOCAL-DECLARATIONS))
			     (LIST SYMBOL (LIST 'ARGLIST (CDR ARGL))))
			    ((SYMBOLP SYMBOL) SYMBOL)
			    (T (LIST SYMBOL)))
		     . ,DEF))
  (COND ((AND (BOUNDP 'UNDO-DECLARATIONS-FLAG) UNDO-DECLARATIONS-FLAG)
	 (PUSH `(DEF ,SYMBOL . ,DEF1) FILE-LOCAL-DECLARATIONS))
	(T
	 (FDEFINE SYMBOL DEF1 T)))
  SYMBOL)

; non-NIL to prevent macros from displacing.
(defvar inhibit-displacing-flag nil)

;; Make the original form now look like
;; (displaced (original-car . original-cdr) expanded-form)
;; avoiding timing errors in case two people displace the same form the same way
;; at the same time.
;; Note that if the original form is not in working-storage-area, don't try
;; to displace it.  It might be in the compiler temporary area, in which case
;; there wouldn't be much point to displacing.  It can also be in INIT-LIST-AREA,
;; in which case attempting to displace would crash the machine.
(defun displace (original-form expanded-form &aux area tem)
  (without-interrupts
      (cond (inhibit-displacing-flag)
	    ((eq (car original-form) 'displaced)
	     (setf (caddr original-form) expanded-form))
	    ((and (= (setq area (%area-number original-form)) working-storage-area)
		  (= (%area-number expanded-form) area))
	     ;; Above area tests are intended to avoid problems with the compiler
	     ;; temporary area, by not displacing anything with something that was
	     ;; consed in a temporary area.  Note that not only lists are in the
	     ;; temporary area, so are gensyms, strings, and flonums.
	     (let ((default-cons-area area))
	       (setq tem `((,(car original-form) . ,(cdr original-form)) ,expanded-form)))
	     (rplaca original-form 'displaced)
	     (rplacd original-form tem)))
      expanded-form))

(macro displaced (form)
    (caddr form))

;Functions for compatibility with MACLISP LEXPRs.  DEFUN-COMPATIBILITY
;is used to convert MACLISP LEXPR and FEXPR DEFUNs to Lisp machine form.

(SPECIAL *LEXPR-ARGLIST*)

(DEFUN ARG (N)
  (COND ((NULL N) (LENGTH *LEXPR-ARGLIST*))
	(T (LET ((ARGPTR (NTHCDR (1- N) *LEXPR-ARGLIST*)))
	     (COND ((OR ( N 0) (NULL ARGPTR))
		    (FERROR NIL "~D is not between 1 and the number of args" N)))
	     (CAR ARGPTR)))))

(DEFUN SETARG (N X)
    (LET ((ARGPTR (NTHCDR (1- N) *LEXPR-ARGLIST*)))
      (COND ((OR ( N 0) (NULL ARGPTR))
	     (FERROR NIL "~D is not between 1 and the number of args" N)))
      (RPLACA ARGPTR X)
      X))

(DEFUN LISTIFY (N)
  (COND ((MINUSP N) (APPEND (NLEFT (- N) *LEXPR-ARGLIST*) NIL))
	((ZEROP N) NIL)
	(T (FIRSTN N *LEXPR-ARGLIST*))))

;;; Control primitives

(DEFUN SETQ (&QUOTE &REST SYMBOLS-AND-VALUES)
  (PROG (VAL)
   L	(COND ((NULL SYMBOLS-AND-VALUES) (RETURN VAL)))
	(SET (CAR SYMBOLS-AND-VALUES) (SETQ VAL (EVAL (CADR SYMBOLS-AND-VALUES))))
	(SETQ SYMBOLS-AND-VALUES (CDDR SYMBOLS-AND-VALUES))
	(GO L)))

;Note: this function is a little slower than necessary, in order to work
;even if (CDR NIL) = NIL mode is shut off.
(DEFUN MULTIPLE-VALUE (&QUOTE VAR-LIST EXP)
  (PROG (VAL-LIST)
	(SETQ VAL-LIST (MULTIPLE-VALUE-LIST (EVAL EXP)))
	(SETQ EXP (CAR VAL-LIST))	;WHAT TO RETURN
   L	(COND ((NULL VAR-LIST) (RETURN EXP))
	      ((CAR VAR-LIST) (SET (CAR VAR-LIST) (AND VAL-LIST (CAR VAL-LIST)))))
	(SETQ VAL-LIST (AND VAL-LIST (CDR VAL-LIST))
	      VAR-LIST (CDR VAR-LIST))
	(GO L)))

(DEFUN MULTIPLE-VALUE-LIST (&QUOTE EXP)
       (MULTIPLE-VALUE-LIST (EVAL EXP)))

;Note: this function is a little slower than necessary, in order to work
;even if (CDR NIL) = NIL mode is shut off.
(DEFUN MULTIPLE-VALUE-BIND (&QUOTE VAR-LIST MV-RETURNING-FORM &REST BODY)
  (LET ((VAL-LIST (MULTIPLE-VALUE-LIST (EVAL MV-RETURNING-FORM))))
    (PROG ()
       LOOP
	  (OR VAR-LIST (GO DO-BODY))
	  (AND (CAR VAR-LIST)
	       (BIND (LOCF (SYMEVAL (CAR VAR-LIST))) (AND VAL-LIST (CAR VAL-LIST))))
	  (SETQ VAL-LIST (AND VAL-LIST (CDR VAL-LIST))
		VAR-LIST (CDR VAR-LIST))
	  (GO LOOP)

       DO-BODY
	  (OR BODY (RETURN NIL))
	  (OR (CDR BODY)
	      (RETURN (EVAL (CAR BODY))))
	  (EVAL (CAR BODY))
	  (POP BODY)
	  (GO DO-BODY))))

(DEFUN VALUES (&REST VALUES)
  (VALUES-LIST VALUES))

(DEFUN VALUES-LIST (VALUES)
  (VALUES-LIST VALUES))	;Relies on open-compilation of VALUES-LIST.

;;; These functions have hair to implement the correct rules for multiple values

(DEFUN PROGN (&QUOTE &REST EXPRESSIONS)
  (IF (NULL EXPRESSIONS) NIL
      (DO ((L EXPRESSIONS (CDR L)))
	  ((NULL (CDR L))
	   (EVAL (CAR L)))   ;Note: this works for multiple values now!
	(EVAL (CAR L)))))

(DEFUN PROG2 (IGNORED VALUE &REST IGNORED) VALUE)

(DEFUN PROG1 (VALUE &REST IGNORED) VALUE)

(DEFUN LET (&QUOTE VARLIST &REST EXPRESSIONS)
  (PROG (VARS-LEFT)
	;; Now bind all the prog-variables.
	;; DO cannot be used, since the scope of the BINDs would be wrong.
	(SETQ VARS-LEFT VARLIST)
   BINDLOOP
   	(COND (VARS-LEFT
	       ;; For each symbol, push 2 words on stack:
	       ;; value cell location and new value.
	       (COND ((ATOM (CAR VARS-LEFT))
		      (OR (CAR VARS-LEFT) (FERROR NIL "Attempt to bind NIL"))
		      (%PUSH (VALUE-CELL-LOCATION (CAR VARS-LEFT)))
		      (%PUSH NIL))
		     (T (OR (CAAR VARS-LEFT) (FERROR NIL "Attempt to bind NIL"))
			(%PUSH (VALUE-CELL-LOCATION (CAAR VARS-LEFT)))
			(%PUSH (EVAL (CADAR VARS-LEFT)))))
	       (POP VARS-LEFT)
	       (GO BINDLOOP)))

	(SETQ VARS-LEFT VARLIST)
   BINDLOOP1
	(COND (VARS-LEFT
	       ;; Pop off next symbol and value, and bind them.
	       (BIND (%POP) (%POP))
	       ;; Step down VARS-LEFT just so we pop as many pairs as we pushed.
	       (POP VARS-LEFT)
	       (GO BINDLOOP1)))

	(RETURN
	  (IF (NULL EXPRESSIONS) NIL
	      (DO ((L EXPRESSIONS (CDR L)))
		  ((NULL (CDR L))
		   (EVAL (CAR L)))		;Note: this works for multiple values now!
		(EVAL (CAR L)))))))

(DEFUN LET* (&QUOTE VARLIST &REST EXPRESSIONS)
  (PROG (VARS-LEFT)
	(SETQ VARS-LEFT VARLIST)
     BINDLOOP
   	(COND (VARS-LEFT
	       (COND ((ATOM (CAR VARS-LEFT))
		      (AND (NULL (CAR VARS-LEFT)) (FERROR NIL "Attempt to bind NIL"))
		      (BIND (VALUE-CELL-LOCATION (CAR VARS-LEFT))
			    NIL))
		     (T (AND (NULL (CAAR VARS-LEFT)) (FERROR NIL "Attempt to bind NIL"))
			(BIND (VALUE-CELL-LOCATION (CAAR VARS-LEFT))
			      (EVAL (CADAR VARS-LEFT)))))
	       (SETQ VARS-LEFT (CDR VARS-LEFT))
	       (GO BINDLOOP)))

	(RETURN
	  (IF (NULL EXPRESSIONS) NIL
	      (DO ((L EXPRESSIONS (CDR L)))
		  ((NULL (CDR L))
		   (EVAL (CAR L)))		;Note: this works for multiple values now!
		(EVAL (CAR L)))))))

(DEFUN COMMENT (&QUOTE &REST IGNORE)
      'COMMENT)

(DEFUN AND (&QUOTE &REST EXPRESSIONS)
  (IF (NULL EXPRESSIONS) T
      (DO ((L EXPRESSIONS (CDR L)))
	  ((NULL (CDR L))
	   (EVAL (CAR L)))   ;Note: this works for multiple values now!
	(OR (EVAL (CAR L))
	    (RETURN NIL)))))

(DEFUN OR (&QUOTE &REST EXPRESSIONS)
  (IF (NULL EXPRESSIONS) NIL
      (DO ((L EXPRESSIONS (CDR L))
	   (VAL))
	  ((NULL (CDR L))
	   (EVAL (CAR L)))   ;Note: this works for multiple values now!
	(AND (SETQ VAL (EVAL (CAR L)))
	     (RETURN VAL)))))

(DEFUN COND (&QUOTE &REST CLAUSES)
  (DO ((CLAUSES CLAUSES (CDR CLAUSES))
       (PREDVAL)(EXPRESSIONS))
      ((NULL CLAUSES) NIL)
    (COND ((ATOM (CAR CLAUSES))
	   (FERROR NIL "The atom ~S is not a valid COND clause." (CAR CLAUSES)))
	  ((AND (NULL (CDR CLAUSES)) (NULL (CDAR CLAUSES)))
	   ;; If this is the last clause, then treat its predicate as part of
	   ;; the body instead of as the predicate, so that multiple values
	   ;; get propagated properly.
	   (SETQ EXPRESSIONS (CAR CLAUSES)))
	  ((SETQ PREDVAL (EVAL (CAAR CLAUSES)))
	   (OR (SETQ EXPRESSIONS (CDAR CLAUSES))
	       (RETURN PREDVAL)))
	  (T (GO NEXTLOOP)))
    ;; Predicate true
   PREDTRUE
    (COND ((NULL (CDR EXPRESSIONS))
	   (RETURN (EVAL (CAR EXPRESSIONS))))
	  (T (EVAL (CAR EXPRESSIONS))
	     (SETQ EXPRESSIONS (CDR EXPRESSIONS))
	     (GO PREDTRUE)))
   NEXTLOOP
    ))

;;; PROG, GO, RETURN, RETURN-LIST, RETURN-FROM

(DEFVAR PROGDESCLIST NIL)
;; PROGDESCLIST is a stack of descriptors for all PROGs which are in progress
;; (in this stack-group).  Each descriptor has is a list of five elements:
;;   NAME - the name of the PROG, or NIL,
;;   BODY - the entire body of the PROG,
;;   OP   - the operation to be performed,
;;   VALUE- the operand.
;;   TAG  - the catch tag for returning from this PROG.
;;          The catch tag is a pointer to this very cell!
;; OP and VALUE are used by RETURN and GO to tell the PROG what to do.
;; RETURN and GO store into those slots and then throw to the tag.
;; The PROG then does the dirty work.

;; RETURN-LIST works by setting OP to 'RETURN-LIST and VALUE to the list of values.
;; RETURN turns itself into a RETURN-LIST.
;; GO works by setting OP to 'GO and VALUE to the body tail at the tag.

;; RETPROGDESC is the descriptor for the PROG which a simple RETURN should return from.
(DEFVAR RETPROGDESC NIL)

;; We used to use the descriptor itself as the catch tag,
;; but this loses when the stack is extended and forwarded.
;; For some reason, we used to try throwing using the new
;; location of the descriptor, while the stored catch tag was
;; still pointing to the old locations (which now contain forwarding pointers).
;; The new data structure ensures that everyone who tries to access
;; the catch tag of a descriptor always gets the same address
;; as was used in the catch.

;; This version binds arguments sequentially (PROG*).
(DEFUN PROG* (&QUOTE &REST PROG-ARGUMENTS)
 ;; Make sure that these variables aren't special, or pdl-list will be screwed up.
 (LOCAL-DECLARE ((UNSPECIAL DESCPTR CHAIN NAME BODY OP VALUE))
  (PROG* (DESCPTR (CHAIN PROGDESCLIST) NAME BODY OP VALUE CATCH-TAG
	  VARLIST EXP PROG-PC PROGDESCLIST (RETPROGDESC RETPROGDESC))
	(AND (ATOM (CAR PROG-ARGUMENTS)) (CAR PROG-ARGUMENTS)
	     (SETQ NAME (CAR PROG-ARGUMENTS) PROG-ARGUMENTS (CDR PROG-ARGUMENTS)))
	(SETQ VARLIST (CAR PROG-ARGUMENTS))
	(SETQ BODY (CDR PROG-ARGUMENTS))
	(SETQ CATCH-TAG (VALUE-CELL-LOCATION 'CATCH-TAG))
	(SETQ DESCPTR (%MAKE-POINTER DTP-LIST (VALUE-CELL-LOCATION 'NAME)))
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'DESCPTR) CDR-NORMAL)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'CHAIN) CDR-ERROR)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'NAME) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'BODY) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'OP) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'VALUE) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'CATCH-TAG) CDR-NIL)
	(SETQ PROGDESCLIST (%MAKE-POINTER DTP-LIST (VALUE-CELL-LOCATION 'DESCPTR)))
	(OR (EQ NAME T) (SETQ RETPROGDESC DESCPTR))
	;; We have now done, effectively, (PUSH `(,NAME ,BODY ,OP ,VALUE) PROGDESCLIST)
	;; but without consing.
	;; Now bind all the prog-variables.
	;; DO cannot be used, since the scope of the BINDs would be wrong.
   BINDLOOP
   	(COND (VARLIST
	       (COND ((ATOM (CAR VARLIST))
		      (AND (NULL (CAR VARLIST)) (FERROR NIL "Attempt to bind NIL"))
		      (BIND (VALUE-CELL-LOCATION (CAR VARLIST))
			    NIL))
		     (T (AND (NULL (CAAR VARLIST)) (FERROR NIL "Attempt to bind NIL"))
			(BIND (VALUE-CELL-LOCATION (CAAR VARLIST))
			      (EVAL (CADAR VARLIST)))))
	       (SETQ VARLIST (CDR VARLIST))
	       (GO BINDLOOP)))

   	(SETQ PROG-PC BODY)				;EXECUTE THE BODY
   L2	(COND ((NULL PROG-PC) (RETURN NIL))
	      ((NLISTP PROG-PC)
	       (FERROR NIL "~S is not a valid PROG form" BODY)
	       (RETURN NIL)))
	(SETQ EXP (CAR PROG-PC))
	(SETQ PROG-PC (CDR PROG-PC))
	(AND (ATOM EXP) (GO L2))			;SKIP TAGS
	(*CATCH CATCH-TAG (EVAL EXP))
	(COND ((NULL OP) (GO L2))
	      ((EQ OP 'GO)
	       (SETQ PROG-PC VALUE))
	      ((EQ OP 'RETURN-LIST)
	       (RETURN-LIST VALUE))
	      (T (FERROR NIL "Illegal PROG throw operation ~S, ~S" OP VALUE)))
	;; The following must NOT use (SETQ OP NIL), becauses that does
	;; not preserve the CDR codes withing the PDL buffer.
	(RPLACA (VALUE-CELL-LOCATION 'OP) NIL)
	(GO L2))))

;; This version binds arguments in parallel (PROG).
(DEFUN PROG (&QUOTE &REST PROG-ARGUMENTS)
 ;; Make sure that these variables aren't special, or pdl-list will be screwed up.
 (LOCAL-DECLARE ((UNSPECIAL DESCPTR CHAIN NAME BODY OP VALUE))
  (PROG* (DESCPTR (CHAIN PROGDESCLIST) NAME BODY OP VALUE CATCH-TAG
	  VARLIST VARS-LEFT
	  EXP PROG-PC PROGDESCLIST (RETPROGDESC RETPROGDESC))
	(AND (ATOM (CAR PROG-ARGUMENTS)) (CAR PROG-ARGUMENTS)
	     (SETQ NAME (CAR PROG-ARGUMENTS) PROG-ARGUMENTS (CDR PROG-ARGUMENTS)))
	(SETQ VARLIST (CAR PROG-ARGUMENTS))
	(SETQ BODY (CDR PROG-ARGUMENTS))

	(SETQ CATCH-TAG (VALUE-CELL-LOCATION 'CATCH-TAG))
	(SETQ DESCPTR (%MAKE-POINTER DTP-LIST (VALUE-CELL-LOCATION 'NAME)))
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'DESCPTR) CDR-NORMAL)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'CHAIN) CDR-ERROR)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'NAME) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'BODY) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'OP) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'VALUE) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'CATCH-TAG) CDR-NIL)
	(SETQ PROGDESCLIST (%MAKE-POINTER DTP-LIST (VALUE-CELL-LOCATION 'DESCPTR)))
	(OR (EQ NAME T) (SETQ RETPROGDESC DESCPTR))
	;; We have now done, effectively, (PUSH `(,NAME ,BODY ,OP ,VALUE) PROGDESCLIST)
	;; but without consing.

	;; Now bind all the prog-variables.
	;; DO cannot be used, since the scope of the BINDs would be wrong.
	(SETQ VARS-LEFT VARLIST)
   BINDLOOP
   	(COND (VARS-LEFT
	       ;; For each symbol, push 2 words on stack:
	       ;; value cell location and new value.
	       (COND ((ATOM (CAR VARS-LEFT))
		      (OR (CAR VARS-LEFT) (FERROR NIL "Attempt to bind NIL"))
		      (%PUSH (VALUE-CELL-LOCATION (CAR VARS-LEFT)))
		      (%PUSH NIL))
		     (T (OR (CAAR VARS-LEFT) (FERROR NIL "Attempt to bind NIL"))
			(%PUSH (VALUE-CELL-LOCATION (CAAR VARS-LEFT)))
			(%PUSH (EVAL (CADAR VARS-LEFT)))))
	       (POP VARS-LEFT)
	       (GO BINDLOOP)))

	(SETQ VARS-LEFT VARLIST)
   BINDLOOP1
	(COND (VARS-LEFT
	       ;; Pop off next symbol and value, and bind them.
	       (BIND (%POP) (%POP))
	       ;; Step down VARS-LEFT just so we pop as many pairs as we pushed.
	       (POP VARS-LEFT)
	       (GO BINDLOOP1)))

   	(SETQ PROG-PC BODY)				;EXECUTE THE BODY
   L2	(COND ((NULL PROG-PC) (RETURN NIL))
	      ((NLISTP PROG-PC)
	       (FERROR NIL "~S is not a valid PROG form" BODY)
	       (RETURN NIL)))
	(SETQ EXP (CAR PROG-PC))
	(SETQ PROG-PC (CDR PROG-PC))
	(AND (ATOM EXP) (GO L2))			;SKIP TAGS
	(*CATCH CATCH-TAG (EVAL EXP))
	(COND ((NULL OP) (GO L2))
	      ((EQ OP 'GO)
	       (SETQ PROG-PC VALUE))
	      ((EQ OP 'RETURN-LIST)
	       (RETURN-LIST VALUE))
	      (T (FERROR NIL "Illegal PROG throw operation ~S, ~S" OP VALUE)))
	;; The following must NOT use (SETQ OP NIL), becauses that does
	;; not preserve the CDR codes withing the PDL buffer.
	(RPLACA (VALUE-CELL-LOCATION 'OP) NIL)
	(GO L2))))

(DEFUN RETURN-FROM (&QUOTE PROG-NAME &REST VALS)
  (CHECK-ARG PROG-NAME SYMBOLP "a symbol")
  (LET ((VALUES (COND ((OR (NULL VALS) (CDR VALS))
		       (MAPCAR 'EVAL VALS))
		      (T (MULTIPLE-VALUE-LIST (EVAL (CAR VALS)))))))
    (LET ((RETPROGDESC (OR (ASSQ PROG-NAME PROGDESCLIST)
			   (FERROR NIL "No PROG is named ~S" PROG-NAME))))
      (FUNCALL (FSYMEVAL 'RETURN-LIST) VALUES))))

(DEFPROP MULTIPLE-VALUE-RETURN COMPILER:OBSOLETE COMPILER:STYLE-CHECKER)
(DEFPROP MULTIPLE-VALUE-RETURN "is an obsolete function; use RETURN" COMPILER:OBSOLETE)
(DEFF MULTIPLE-VALUE-RETURN 'RETURN)
(DEFUN RETURN (&QUOTE &REST VALS)
  (FUNCALL (FSYMEVAL 'RETURN-LIST)		;This kludge is so RETURN-LIST
	   (COND ((OR (NULL VALS) (CDR VALS))	;won't compile open.
		  (MAPCAR 'EVAL VALS))
		 (T (MULTIPLE-VALUE-LIST (EVAL (CAR VALS)))))))

(DEFUN RETURN-LIST (VALS)
  (OR RETPROGDESC
      (FERROR NIL "RETURN or RETURN-LIST not within a DO or PROG"))
  (RPLACA (CDDR RETPROGDESC) 'RETURN-LIST)
  (RPLACA (CDDDR RETPROGDESC) VALS)
  (*THROW (CAR (CDDDDR RETPROGDESC)) NIL))

(DEFUN GO (&QUOTE TAG &AUX TEM)
    (CHECK-ARG TAG SYMBOLP "a symbol")
    (DO ((DESCS PROGDESCLIST (CDR DESCS)))
	((NULL DESCS)
	 (FERROR NIL "Unseen GO-tag ~S" TAG))
      (SETQ TEM (MEMQ TAG (CADAR DESCS)))
      (AND TEM (PROGN
		(RPLACA (CDDAR DESCS) 'GO)
		(RPLACA (CDDDAR DESCS) TEM)
		(*THROW (CAR (CDDDDR (CAR DESCS))) NIL)))))

(DEFUN DO (&QUOTE &REST X)
    (DO-INTERNAL NIL X))

(DEFUN DO-NAMED (&QUOTE NAME &REST X)
    (DO-INTERNAL NAME X))

(DEFUN DO-INTERNAL (DO-NAME X)
 ;; Make sure that these variables aren't special, or pdl-list will be screwed up.
 (LOCAL-DECLARE ((UNSPECIAL DESCPTR CHAIN NAME BODY OP VALUE))
  (PROG* DO-INTERNAL
	 (DESCPTR (CHAIN PROGDESCLIST) NAME BODY OP VALUE CATCH-TAG
	  PROGDESCLIST (RETPROGDESC RETPROGDESC) PROG-PC
	  VARLIST VARS-LEFT ENDTEST RETVALS OLDP ONCEP TEM EXP)
	(SETQ NAME DO-NAME)
	(SETQ DESCPTR (%MAKE-POINTER DTP-LIST (VALUE-CELL-LOCATION 'NAME)))
	(SETQ CATCH-TAG (%MAKE-POINTER DTP-LIST (VALUE-CELL-LOCATION 'CATCH-TAG)))
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'DESCPTR) CDR-NORMAL)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'CHAIN) CDR-ERROR)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'NAME) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'BODY) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'OP) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'VALUE) CDR-NEXT)
	(%P-STORE-CDR-CODE (VALUE-CELL-LOCATION 'CATCH-TAG) CDR-NIL)
	(SETQ PROGDESCLIST (%MAKE-POINTER DTP-LIST (VALUE-CELL-LOCATION 'DESCPTR)))
	(OR (EQ NAME T) (SETQ RETPROGDESC DESCPTR))
	;; We have now done, effectively, (PUSH `(,NAME ,BODY ,OP ,VALUE) PROGDESCLIST)
	;; but without consing, except that BODY isn't set yet.
	;; Now bind all the prog-variables, and set BODY.
	;; DO cannot be used, since the scope of the BINDs would be wrong.
	(COND ((AND (NOT (NULL (CAR X))) (ATOM (CAR X)))	;OLD STYLE
	       (SETQ OLDP T)
	       (AND (NULL (CAR X)) (FERROR NIL "Attempt to bind NIL"))
	       (BIND (VALUE-CELL-LOCATION (CAR X)) (EVAL (CADR X)))
	       (SETQ ENDTEST (CADDDR X))
	       (SETQ RETVALS NIL)
	       ;; Don't ever SETQ the variable BODY - it would clobber the CDR code!
	       (RPLACA (VALUE-CELL-LOCATION 'BODY) (CDDDDR X))
	       (GO ITR)))
	(SETQ VARLIST (CAR X))
	;; Don't ever SETQ the variable BODY - it would clobber the CDR code!
	(RPLACA (VALUE-CELL-LOCATION 'BODY) (CDDR X))
	(SETQ ONCEP (NULL (CADR X)))
	(OR ONCEP (SETQ ENDTEST (CAADR X) RETVALS (CDADR X)))

	;; Now bind all the DO-variables.
	;; DO cannot be used, since the scope of the BINDs would be wrong.
	(SETQ VARS-LEFT VARLIST)
   BINDLOOP
   	(COND (VARS-LEFT
	       ;; For each symbol, push 2 words on stack:
	       ;; value cell location and new value.
	       (COND ((ATOM (CAR VARS-LEFT))
		      (OR (CAR VARS-LEFT) (FERROR NIL "Attempt to bind NIL"))
		      (%PUSH (VALUE-CELL-LOCATION (CAR VARS-LEFT)))
		      (%PUSH NIL))
		     (T (OR (CAAR VARS-LEFT) (FERROR NIL "Attempt to bind NIL"))
			(%PUSH (VALUE-CELL-LOCATION (CAAR VARS-LEFT)))
			(%PUSH (EVAL (CADAR VARS-LEFT)))))
	       (POP VARS-LEFT)
	       (GO BINDLOOP)))

	(SETQ VARS-LEFT VARLIST)
   BINDLOOP1
	(COND (VARS-LEFT
	       ;; Pop off next symbol and value, and bind them.
	       (BIND (%POP) (%POP))
	       ;; Step down VARS-LEFT just so we pop as many pairs as we pushed.
	       (POP VARS-LEFT)
	       (GO BINDLOOP1)))

	;; Now actually iterate the body.
   ITR
	(COND ((AND (NOT ONCEP) (*CATCH CATCH-TAG (EVAL ENDTEST)))
	       (SETQ TEM NIL)			       ;IF NO RETVALS, RETURN NIL, NOT ENDTEST
	       ;; Now evaluate the exit actions.
	       ;; The last one should return its values out of the DO.
	       (DO VL RETVALS (CDR VL) (NULL VL)
		   (SETQ TEM (*CATCH CATCH-TAG
				     (COND ((NULL (CDR VL))
					    (RETURN-FROM DO-INTERNAL
							 (EVAL (CAR VL))))
					   (T (EVAL (CAR VL))))))
		   ;; Make a RETURN or RETURN-LIST inside one of the exit forms work.
		   (COND (OP (COND ((EQ OP 'RETURN-LIST)
				    (GO RETURN-LIST))
				   ((EQ OP 'GO)
				    (FERROR NIL
  "GO to tag ~S inside exit forms of a DO" VALUE))
				   (T (FERROR NIL 
  "Illegal PROG throw operation ~S, ~S" OP VALUE))))))
	       (RETURN (AND (NOT OLDP) TEM))))

	(COND (OP (COND ((EQ OP 'RETURN-LIST)
			 (GO RETURN-LIST))
			(T (FERROR NIL 
  "Illegal PROG throw operation ~S, ~S" OP VALUE)))))
   	(SETQ PROG-PC BODY)				;EXECUTE THE BODY
   L2	(COND ((NULL PROG-PC) (GO STEP))		;At end of body, step the vars.
	      ((NLISTP PROG-PC)
	       (FERROR NIL "~S is not a valid PROG form" BODY)
	       (RETURN NIL)))
	(SETQ EXP (CAR PROG-PC))
	(SETQ PROG-PC (CDR PROG-PC))
	(AND (ATOM EXP) (GO L2))			;SKIP TAGS
	(*CATCH CATCH-TAG (EVAL EXP))
	(COND ((NULL OP) (GO L2))
	      ((EQ OP 'GO)
	       (SETQ PROG-PC VALUE))
	      ((EQ OP 'RETURN-LIST)
	       (GO RETURN-LIST))
	      (T (FERROR NIL "Illegal PROG throw operation ~S, ~S" OP VALUE)))
	;; The following must NOT use (SETQ OP NIL), becauses that does
	;; not preserve the CDR codes within the PDL buffer.
	(RPLACA (VALUE-CELL-LOCATION 'OP) NIL)
	(GO L2)

	;; Here to do (RETURN-LIST VALUE).  RETURN also handled through this path.
   RETURN-LIST
	(RETURN-LIST VALUE)

	;; Here after finishin the body to step the DO-variables.
   STEP
	(AND ONCEP (RETURN NIL))
	(COND (OLDP (SET (CAR X) (EVAL (CADDR X))))
	      (T (DO ((VL VARLIST (CDR VL))
		      (VALS (DO ((VL VARLIST (CDR VL))
				 (VALS NIL (CONS (AND (LISTP (CAR VL)) (CDAR VL) (CDDAR VL)
						      (EVAL (CADDAR VL)))
						 VALS)))	;******* CONS *******
				((NULL VL) (NREVERSE VALS)))
			    (CDR VALS)))
		     ((NULL VL))
		   (COND ((AND (LISTP (CAR VL)) (CDAR VL) (CDDAR VL))
			  (SET (CAAR VL) (CAR VALS)))))))
	(GO ITR))))

;;; List manipulation functions.

;This is in microcode now
;(DEFUN NTHCDR (N LIST)
;    (CHECK-ARG N (AND (FIXP N) (NOT (MINUSP N))) "a non-negative integer") 
;    (DO ((I 0 (1+ I))
;	 (LIST LIST (CDR LIST)))
;	((>= I N) LIST)))

(DEFUN REVERSE (LIST)
  (PROG (V)
  L	(COND ((ATOM LIST) (RETURN V)))
	(SETQ V (CONS (CAR LIST) V))
	(SETQ LIST (CDR LIST))
	(GO L)))

(DEFVAR QREVERSE-DUMMY-ARRAY-HEADER
	(MAKE-ARRAY 100
		    ':TYPE ':ART-Q-LIST
		    ':AREA PERMANENT-STORAGE-AREA
		    ':DISPLACED-TO (%MAKE-POINTER DTP-LOCATIVE NIL)))

(DEFUN QREVERSE (LIST &AUX LENGTH RETURN-LIST (INHIBIT-SCHEDULING-FLAG T))
  (SETQ RETURN-LIST (MAKE-LIST (SETQ LENGTH (LENGTH LIST))))
  (%P-STORE-CONTENTS-OFFSET RETURN-LIST QREVERSE-DUMMY-ARRAY-HEADER 1)
  (%P-STORE-CONTENTS-OFFSET LENGTH QREVERSE-DUMMY-ARRAY-HEADER 2)
  (DO ((I (1- LENGTH) (1- I))
       (L LIST (CDR L)))
      ((NULL L))
    (ASET (CAR L) QREVERSE-DUMMY-ARRAY-HEADER I))
  RETURN-LIST)

(DEFUN COPYLIST (LIST &OPTIONAL AREA FORCE-DOTTED)
  "Copy top level of list structure.  Dotted pair termination of list will be copied"
  (COND ((ATOM LIST) LIST)	;Might be NIL
	(T (LET ((DOTTED (OR FORCE-DOTTED (CDR (LAST LIST)))))
	     (LET ((NEWLIST (MAKE-LIST AREA (IF DOTTED (1+ (LENGTH LIST)) (LENGTH LIST)))))
	       (DO ((L1 LIST (CDR L1))
		    (L2 NEWLIST (CDR L2)))
		   ((ATOM L1)
		    (COND (DOTTED
			   (RPLACA L2 L1)
			   (WITHOUT-INTERRUPTS
			     (%P-DPB-OFFSET CDR-ERROR %%Q-CDR-CODE L2 0)
			     (%P-DPB-OFFSET CDR-NORMAL %%Q-CDR-CODE L2 -1)))))
		 (RPLACA L2 (CAR L1)))
	       NEWLIST)))))

(DEFUN COPYLIST* (LIST &OPTIONAL AREA)
  "Like COPYLIST but never cdr-codes the last pair of the list."
  (COPYLIST LIST AREA T))

(DEFUN COPYALIST (AL &OPTIONAL (DEFAULT-CONS-AREA DEFAULT-CONS-AREA))
  "Copies top two levels of list structure.  Dotted pair termination of list will be copied"
  (COND ((NLISTP AL) AL)
	(T (SETQ AL (APPEND AL (CDR (LAST AL))))   ;RECOPY THE TOP LEVEL.
	   (DO ((P AL (CDR P)))
	       ((NLISTP P) AL)
	     (COND ((LISTP (CAR P))		  ;THEN RECOPY THE ASSOC CELLS.
		    (RPLACA P (CONS (CAAR P) (CDAR P)))))))))

;;; (SUBST NIL NIL ...) is such an ugly language idiom...
(DEFUN COPYTREE (TREE &OPTIONAL (DEFAULT-CONS-AREA DEFAULT-CONS-AREA))
  "Copies a binary tree of CONSES."
  (IF (ATOM TREE)
      TREE
      (CONS (COPYTREE (CAR TREE))
	    (COPYTREE (CDR TREE)))))

(DEFUN APPEND (&REST LISTS)
  (PROG (TOTAL-LENGTH ARGP VAL VALP)
	(COND ((ATOM LISTS) (RETURN NIL))
	      ((ATOM (CDR LISTS)) 
		(RETURN (CAR LISTS))))
	(SETQ TOTAL-LENGTH 0)
	;; Accumulate length of args we must copy
	(DO ((ARGP LISTS (CDR ARGP)))
	    ((ATOM (CDR ARGP))
	     ;; Plus one more if the last arg is not NIL.
	     ;; But if all are NIL so far, leave it 0 as signal to COND that follows.
	     (AND (CAR ARGP) (NOT (ZEROP TOTAL-LENGTH))
		  (SETQ TOTAL-LENGTH (1+ TOTAL-LENGTH))))
	  (SETQ TOTAL-LENGTH (+ TOTAL-LENGTH (LENGTH (CAR ARGP)))))
  	(COND ((ZEROP TOTAL-LENGTH) (RETURN (CAR (LAST LISTS)))))
	(SETQ VALP (SETQ VAL (MAKE-LIST DEFAULT-CONS-AREA TOTAL-LENGTH)))
	(SETQ ARGP LISTS)
  L2	(COND ((NULL (CDR ARGP))
	       ;; When we reach the last arg, if it's NIL, we are done.
	       (OR (CAR ARGP) (RETURN VAL))
	       ;; Otherwise, stick in a pointer to the last arg,
	       ;; and then change it from an element to a cdr.
	       (RPLACA VALP (CAR ARGP))
	       (LET ((INHIBIT-SCHEDULING-FLAG T))
		 (%P-DPB-OFFSET CDR-ERROR %%Q-CDR-CODE VALP 0)
		 (%P-DPB-OFFSET CDR-NORMAL %%Q-CDR-CODE VALP -1))
	       (RETURN VAL)))
	(DO ((ARGLP (CAR ARGP) (CDR ARGLP)))
	    ((ATOM ARGLP) 
	     (SETQ ARGP (CDR ARGP))
	     (GO L2))
	  (RPLACA VALP (CAR ARGLP))
	  (SETQ VALP (CDR VALP))) ))

(DEFUN UNION (&REST LISTS &AUX ACCUM)
    (SETQ ACCUM (CAR LISTS))
    (LET ((TAIL (OR (LAST ACCUM) (VALUE-CELL-LOCATION 'ACCUM))))
	(DO LS (CDR LISTS) (CDR LS) (NULL LS)
	    (DO L (CAR LS) (CDR L) (NULL L)
		(OR (MEMQ (CAR L) ACCUM)
		    (RPLACD TAIL (SETQ TAIL (NCONS (CAR L))))))))
    ACCUM)

(DEFUN INTERSECTION (&REST LISTS)
  (DO ((LIST (CAR LISTS) (CDR LIST))
       (REST (CDR LISTS))
       (RESULT)
       (OLD))
      ((NULL LIST) RESULT)
    (COND ((DO ((X (CAR LIST))
		(REST REST (CDR REST)))
	       ((NULL REST) T)
	     (OR (MEMQ X (CAR REST))
		 (RETURN NIL)))
	   (OR RESULT (SETQ RESULT LIST))
	   (SETQ OLD LIST))
	  (OLD
	   (RPLACD OLD (CDR LIST))))))

(DEFUN NCONC (&REST ARG)
	(COND ((NULL ARG) NIL)
	      ((NULL (CDR ARG)) (CAR ARG))
	      (T (*NCONC1 ARG))))

(DEFUN *NCONC1 (ARG)
	(COND ((NULL (CDDR ARG))
		(*NCONC (CAR ARG) (CADR ARG))) 
	      (T (*NCONC (CAR ARG) (*NCONC1 (CDR ARG))))))

(DEFUN *NCONC (A B)
	(COND ((ATOM A) B)
	      (T (RPLACD (LAST A) B)
		 A)))

(DEFUN NBUTLAST (LIST)
  (PROG (TEM)
	(COND ((OR (NULL LIST)
		   (NULL (CDR LIST)))
	       (RETURN NIL)))
	(SETQ TEM LIST)
    L	(COND ((NULL (CDDR TEM))
	       (RPLACD TEM NIL)
	       (RETURN LIST)))
    	(SETQ TEM (CDR TEM))
	(GO L)))

(DEFUN SASSOC (ITEM IN-LIST ELSE)
	(OR (ASSOC ITEM IN-LIST)
	    (APPLY ELSE NIL)))

(DEFUN SASSQ (ITEM IN-LIST ELSE)
	(OR (ASSQ ITEM IN-LIST)
	    (APPLY ELSE NIL)))

(DEFUN ASSOC (ITEM IN-LIST)
  (PROG NIL 
   L	(COND ((NULL IN-LIST) (RETURN NIL))
	      ((EQUAL ITEM (CAAR IN-LIST)) (RETURN (CAR IN-LIST))))
	(SETQ IN-LIST (CDR IN-LIST))
	(GO L)))

(DEFUN ASS (PRED ITEM LIST)
    (DO L LIST (CDR L) (NULL L)
      (AND (FUNCALL PRED ITEM (CAAR L))
           (RETURN (CAR L)))))

(DEFUN RASSOC (ITEM IN-LIST) 
  (DO L IN-LIST (CDR L) (NULL L) 
    (AND (EQUAL ITEM (CDAR L)) 
	 (RETURN (CAR L)))))

(DEFUN RASSQ (ITEM IN-LIST) 
  (DO L IN-LIST (CDR L) (NULL L) 
    (AND (EQ ITEM (CDAR L)) 
	 (RETURN (CAR L)))))

(DEFUN RASS (PRED ITEM IN-LIST) 
  (DO L IN-LIST (CDR L) (NULL L) 
    (AND (FUNCALL PRED ITEM (CDAR L))
	 (RETURN (CAR L)))))

;Like ASSQ, but doesnt complain if list element is not a cons.  Just ignores those.
(DEFUN ASSQ-CAREFUL (KEY LST)
  (PROG NIL 
    L	(COND ((ATOM LST) (RETURN NIL))
              ((ATOM (CAR LST)))
              ((EQ KEY (CAAR LST))
               (RETURN (CAR LST))))
    	(SETQ LST (CDR LST))
        (GO L)))

(DEFUN ASSOC-CAREFUL (KEY LST)
  (PROG NIL 
    L	(COND ((ATOM LST) (RETURN NIL))
              ((ATOM (CAR LST)))
              ((EQUAL KEY (CAAR LST))
               (RETURN (CAR LST))))
    	(SETQ LST (CDR LST))
        (GO L)))

(DEFUN MEMBER (ITEM IN-LIST)
  (PROG NIL 
    L	(COND ((NULL IN-LIST) (RETURN NIL))
	      ((EQUAL ITEM (CAR IN-LIST)) (RETURN IN-LIST)))
	(SETQ IN-LIST (CDR IN-LIST))
	(GO L)))

(DEFUN MEM (PRED ITEM LIST)
    (DO L LIST (CDR L) (NULL L)
      (AND (FUNCALL PRED ITEM (CAR L))
           (RETURN L))))

;(MEMASS PRED ITEM LIST) = (MEM PRED (ASS PRED ITEM LIST) LIST) but twice as fast.
(DEFUN MEMASS (PRED ITEM LIST)
    (DO L LIST (CDR L) (NULL L)
      (AND (FUNCALL PRED ITEM (CAAR L))
           (RETURN L))))

(DEFUN MEMASSQ (ITEM LIST)
    (DO L LIST (CDR L) (NULL L)
      (AND (EQ ITEM (CAAR L)) (RETURN L))))

(DEFUN CIRCULAR-LIST (&REST ARGS &AUX TEM)
    (SETQ TEM (APPEND ARGS 'CIRCULAR-LIST))
    (RPLACD (LAST TEM) TEM)
    TEM)

;These are in microcode now. 
;(DEFUN LIST-IN-AREA (AREA &REST ELEMENTS)
;  (PROG (VAL TEM)
;	(OR ELEMENTS (RETURN NIL))
;	(OR (NUMBERP AREA) (SETQ AREA (SYMEVAL AREA)))
;	(SETQ TEM (SETQ VAL (MAKE-LIST AREA (LENGTH ELEMENTS))))
;  L	(RPLACA TEM (CAR ELEMENTS))
;	(COND ((NULL (SETQ TEM (CDR TEM))) (RETURN VAL)))
;	(SETQ ELEMENTS (CDR ELEMENTS))
;	(GO L)))
;	
;(DEFUN LIST (&REST ELEMENTS)
;  (PROG (VAL TEM)
;	(COND ((ATOM ELEMENTS) (RETURN NIL)))
;	(SETQ TEM (SETQ VAL (MAKE-LIST DEFAULT-CONS-AREA (LENGTH ELEMENTS))))
;  L	(RPLACA TEM (CAR ELEMENTS))
;	(COND ((NULL (SETQ TEM (CDR TEM))) (RETURN VAL)))
;	(SETQ ELEMENTS (CDR ELEMENTS))
;	(GO L)))

(DEFUN TAILP (TAIL LIST)
    (DO LIST LIST (CDR LIST) (NULL LIST)
      (AND (EQ TAIL LIST)
	   (RETURN T))))

;MEM and ASS are special cases of this, which is to TAILP as MEM is to MEMQ.
(DEFUN PRED-TAILP (PRED TAIL LIST)
    (DO LIST LIST (CDR LIST) (NULL LIST)
      (AND (FUNCALL PRED TAIL LIST)
	   (RETURN T))))

(DEFUN FIND-POSITION-IN-LIST-EQUAL (ITEM IN-LIST)
    (DO ((L IN-LIST (CDR L))
	 (C 0 (1+ C)))
	((NULL L))
      (AND (EQUAL ITEM (CAR L)) (RETURN C))))

(DEFUN NLEFT (N L &OPTIONAL TAIL)
  (DO ((L1 L (CDR L1))
       (L2 (NTHCDR N L) (CDR L2)))
      ((EQ L2 TAIL) L1)
    (AND (NULL L2) (RETURN NIL))))

;Returns a list of all of the elements of LIST, up to and not including
;the first one for which (FUNCALL PRED X <tail of list>) is true.
;If PRED is EQ, this performs LDIFF.
(DEFUN PRED-BUTLAST (PRED TAIL LIST)
   (COND ((FUNCALL PRED TAIL LIST) NIL)
	 (T (CONS (CAR LIST) (PRED-BUTLAST PRED TAIL (CDR LIST))))))

(DEFUN NREVERSE (L)
  (NRECONC L NIL))

(DEFUN NRECONC (L TAIL)	;WILL LOSE PRETTY POORLY FOR CDR-NEXT
  (PROG NIL 
A   (AND (ATOM L) (RETURN TAIL))
    (SETQ L (PROG2 NIL (CDR L) (RPLACD L TAIL) (SETQ TAIL L)))
    (GO A)))

(DEFUN DELETE (ITEM LIST &OPTIONAL (/#TIMES -1))
  (PROG (LL PL)
A   (COND ((OR (= 0 /#TIMES) (ATOM LIST))
	   (RETURN LIST))
	  ((EQUAL ITEM (CAR LIST))
	   (SETQ LIST (CDR LIST))
	   (SETQ /#TIMES (1- /#TIMES))
	   (GO A)))
    (SETQ LL LIST)
B   (COND ((OR (= 0 /#TIMES) (ATOM LL))
	   (RETURN LIST))
	  ((EQUAL ITEM (CAR LL))
	   (RPLACD PL (CDR LL))
	   (SETQ /#TIMES (1- /#TIMES)))
	  ((SETQ PL LL)))
    (SETQ LL (CDR LL))
    (GO B)))

(DEFUN DEL (PRED ITEM LIST &OPTIONAL (/#TIMES -1))
  (PROG (LL PL)
A   (COND ((OR (= 0 /#TIMES) (ATOM LIST))
	   (RETURN LIST))
	  ((FUNCALL PRED ITEM (CAR LIST))
	   (SETQ LIST (CDR LIST))
	   (SETQ /#TIMES (1- /#TIMES))
	   (GO A)))
    (SETQ LL LIST)
B   (COND ((OR (= 0 /#TIMES) (ATOM LL))
	   (RETURN LIST))
	  ((FUNCALL PRED ITEM (CAR LL))
	   (RPLACD PL (CDR LL))
	   (SETQ /#TIMES (1- /#TIMES)))
	  ((SETQ PL LL)))
    (SETQ LL (CDR LL))
    (GO B)))

(DEFUN REMOVE (ITEM LIST &OPTIONAL TIMES)
  (PROG (CHANGEDP NEWTAIL)
    (COND ((NULL LIST) (RETURN NIL NIL))
	  ((EQUAL ITEM (CAR LIST))
	   (RETURN (COND ((AND (NUMBERP TIMES) (<= TIMES 1)) (CDR LIST))
			 (T (REMOVE ITEM (CDR LIST) (AND TIMES (1- TIMES)))))
		   T))
	  ((CDR LIST)
	   (MULTIPLE-VALUE (NEWTAIL CHANGEDP) (REMOVE ITEM (CDR LIST) TIMES))
	   (RETURN (COND (CHANGEDP (CONS (CAR LIST) NEWTAIL))
			 (T LIST))
		   CHANGEDP))
	  (T (RETURN LIST NIL)))))

(DEFUN REMQ (ITEM LIST &OPTIONAL TIMES)
  (PROG (CHANGEDP NEWTAIL)
    (COND ((NULL LIST) (RETURN NIL NIL))
	  ((EQ ITEM (CAR LIST))
	   (RETURN (COND ((AND (NUMBERP TIMES) (<= TIMES 1)) (CDR LIST))
			 (T (REMQ ITEM (CDR LIST) (AND TIMES (1- TIMES)))))
		   T))
	  ((CDR LIST)
	   (MULTIPLE-VALUE (NEWTAIL CHANGEDP) (REMQ ITEM (CDR LIST) TIMES))
	   (RETURN (COND (CHANGEDP (CONS (CAR LIST) NEWTAIL))
			 (T LIST))
		   CHANGEDP))
	  (T (RETURN LIST NIL)))))

(DEFUN REM (PRED ITEM LIST &OPTIONAL TIMES)
  (PROG (CHANGEDP NEWTAIL)
    (COND ((NULL LIST) (RETURN NIL NIL))
	  ((FUNCALL PRED ITEM (CAR LIST))
	   (RETURN (COND ((AND (NUMBERP TIMES) (<= TIMES 1)) (CDR LIST))
			 (T (REM PRED ITEM (CDR LIST) (AND TIMES (1- TIMES)))))
		   T))
	  ((CDR LIST)
	   (MULTIPLE-VALUE (NEWTAIL CHANGEDP) (REM PRED ITEM (CDR LIST) TIMES))
	   (RETURN (COND (CHANGEDP (CONS (CAR LIST) NEWTAIL))
			 (T LIST))
		   CHANGEDP))
	  (T (RETURN LIST NIL)))))

(DEFF REM-IF-NOT (FUNCTION SUBSET))
(DEFF REM-IF (FUNCTION SUBSET-NOT))

(DEFUN ELIMINATE-DUPLICATES (L &OPTIONAL (PREDICATE #'EQ) &AUX (MARKER '(DUPLICATE)))
  "Destructively takes out any duplicate elements in the list.
   Leaves the first instance where it is and removes following instances."
  (DO ((L1 L (CDR L1)))
      ((NULL L1)
       (DELQ MARKER L))
    (OR (EQ (CAR L1) MARKER)
	(DO ((TEM (CDR L1) (CDR TEM)))
	    ((NULL (SETQ TEM (MEM PREDICATE (CAR L1) TEM))))
	  (RPLACA TEM MARKER)))))

(DEFUN NSUBST (NEW OLD S-EXP &AUX TEM)
    (COND ((EQ OLD S-EXP) NEW)
	  ((ATOM S-EXP) S-EXP)
	  (T (DO ((S S-EXP (CDR S))
		  (PREV NIL S))
		 ((ATOM S)
		  (SETQ TEM (NSUBST NEW OLD S))
		  (OR (EQ TEM S) (RPLACD PREV TEM)))
	       (RPLACA S (NSUBST NEW OLD (CAR S))))
	     S-EXP)))

(DEFUN SUBST (NEW OLD S-EXP &AUX TEM)
    (COND ((EQUAL OLD S-EXP) NEW)
	  ((ATOM S-EXP) S-EXP)
	  (T (SETQ S-EXP (COPYLIST S-EXP))
	     (DO ((S S-EXP (CDR S))
		  (PREV NIL S))
		 ((ATOM S)
		  (SETQ TEM (SUBST NEW OLD S))
		  (OR (EQ TEM S) (RPLACD PREV TEM)))
	       (RPLACA S (SUBST NEW OLD (CAR S))))
	     S-EXP)))

(DEFUN SUBLIS (ALIST FORM &AUX CAR CDR)
    (COND ((SYMBOLP FORM)
	   (COND ((SETQ CAR (ASSQ FORM ALIST))
		  (CDR CAR))
		 (T FORM)))
	  ((LISTP FORM)
	   (SETQ CAR (SUBLIS ALIST (CAR FORM))
		 CDR (SUBLIS ALIST (CDR FORM)))
	   (COND ((AND (EQ (CAR FORM) CAR)
		       (EQ (CDR FORM) CDR))
		  FORM)
		 (T (CONS CAR CDR))))
	  (T FORM)))

(DEFUN NSUBLIS (ALIST FORM &AUX TEM)
    (COND ((SYMBOLP FORM)
	   (COND ((SETQ TEM (ASSQ FORM ALIST))
		  (CDR TEM))
		 (T FORM)))
	  ((LISTP FORM)
	   (RPLACA FORM (NSUBLIS ALIST (CAR FORM)))
	   (SETQ TEM (NSUBLIS ALIST (CDR FORM)))
	   (OR (EQ (CDR FORM) TEM)
	       (RPLACD FORM TEM))
	   FORM)
	  (T FORM)))

(DEFVAR TYPEP-ALIST `((,DTP-SYMBOL . :SYMBOL)
		      (,DTP-LIST . :LIST)
		      (,DTP-FIX . :FIXNUM)
		      (,DTP-SMALL-FLONUM . :SMALL-FLONUM)
		      (,DTP-LOCATIVE . :LOCATIVE)
		      (,DTP-FEF-POINTER . :COMPILED-FUNCTION)
		      (,DTP-CLOSURE . :CLOSURE)
		      (,DTP-ENTITY . :ENTITY)
		      (,DTP-INSTANCE . :INSTANCE)
		      (,DTP-U-ENTRY . :MICROCODE-FUNCTION)
		      (,DTP-SELECT-METHOD . :SELECT-METHOD)
		      (,DTP-STACK-GROUP . :STACK-GROUP)))

(DEFPROP :STRING STRINGP TYPEP)
(DEFPROP :ARRAY ARRAYP TYPEP)
(DEFPROP :ATOM ATOM TYPEP)
(DEFPROP :BIGNUM BIGP TYPEP)
(DEFPROP :FIX FIXP TYPEP)
(DEFPROP :FLOAT FLOATP TYPEP)
(DEFPROP :NUMBER NUMBERP TYPEP)
(DEFPROP :FLONUM FLONUMP TYPEP)   ;true if full precision flonum
(DEFPROP :RATIONAL RATIONALP TYPEP)

;;; Pretty names for types.  This is used by the CHECK-ARG-TYPE macro.
(DEFPROP :SYMBOL "a symbol" TYPE-NAME)
(DEFPROP :LIST "a list" TYPE-NAME)
(DEFPROP :FIXNUM "a fixnum" TYPE-NAME)
(DEFPROP :SMALL-FLONUM "a small flonum" TYPE-NAME)
(DEFPROP :LOCATIVE "a locative" TYPE-NAME)
(DEFPROP :COMPILED-FUNCTION "a compiled function" TYPE-NAME)
(DEFPROP :CLOSURE "a closure" TYPE-NAME)
(DEFPROP :ENTITY "an entity" TYPE-NAME)
(DEFPROP :INSTANCE "an instance" TYPE-NAME)
(DEFPROP :MICROCODE-FUNCTION "a microcode function" TYPE-NAME)
(DEFPROP :SELECT-METHOD "a select-method" TYPE-NAME)
(DEFPROP :STACK-GROUP "a stack group" TYPE-NAME)
(DEFPROP :STRING "a string" TYPE-NAME)
(DEFPROP :ARRAY "an array" TYPE-NAME)
(DEFPROP :ATOM "an atom" TYPE-NAME)
(DEFPROP :BIGNUM "a bignum" TYPE-NAME)
(DEFPROP :FIX "a fixed-point number" TYPE-NAME)
(DEFPROP :FLOAT "a floating-point number" TYPE-NAME)
(DEFPROP :NUMBER "a number" TYPE-NAME)
(DEFPROP :FLONUM "a flonum" TYPE-NAME)
(DEFPROP :RATIONAL "a rational" TYPE-NAME)

(DEFSUBST FIXNUMP (X) "Unlike FIXP, true only for fixnums, not bignums"
  (= (%DATA-TYPE X) DTP-FIX))

(DEFUN FLONUMP (X)  "true if full precision flonum"
  (AND (= (%DATA-TYPE X) DTP-EXTENDED-NUMBER)
       (= (%P-LDB-OFFSET %%HEADER-TYPE-FIELD X 0) %HEADER-TYPE-FLONUM)))
;; These are redundant, but exist because LISTP is faster than = DTP-LIST
;; in compiled code.  The compiler uses TYPEP props before TYPEP-ALIST.
(DEFPROP :LIST LISTP TYPEP)
(DEFPROP :SYMBOL SYMBOLP TYPEP)

(DEFUN TYPEP (X &OPTIONAL TYPE &AUX PRED FL D (DTP (%DATA-TYPE X)))
  "(TYPEP x) => its type.  (TYPEP x y) => T if x is of type y"
  (COND (TYPE
	 (SETQ PRED (OR (CAR (RASSQ TYPE TYPEP-ALIST)) (GET TYPE 'TYPEP)))
	 (COND ((NUMBERP PRED) (= DTP PRED))
	       (PRED (FUNCALL PRED X))
	       ((GET TYPE 'FLAVOR)
		(AND (= DTP DTP-INSTANCE)
		     (= (%P-DATA-TYPE (SETQ FL (%P-CONTENTS-AS-LOCATIVE-OFFSET X 0)))
			DTP-ARRAY-HEADER)
		     (EQ (AREF (SETQ FL (%MAKE-POINTER DTP-ARRAY-POINTER FL)) 0) 'FLAVOR)
		     (NOT (NULL (MEMQ TYPE (FLAVOR-DEPENDS-ON-ALL FL))))))
	       ((OR (AND (SETQ D (GET TYPE 'DEFSTRUCT-DESCRIPTION))
			 (DEFSTRUCT-DESCRIPTION-NAMED-P D))
		    (GET TYPE 'DEFSTRUCT-NAMED-P))
		(DO ((SYMBOL (NAMED-STRUCTURE-P X)
			     (AND (SETQ D (GET SYMBOL 'DEFSTRUCT-DESCRIPTION))
				  (CAR (DEFSTRUCT-DESCRIPTION-INCLUDE D)))))
		    ((NULL SYMBOL) NIL)
		  (AND (EQ TYPE SYMBOL) (RETURN T))))
	       ((CLASS-SYMBOLP TYPE)
		(AND (ENTITYP X)
		     (SUBCLASS-OF-CLASS-SYMBOL-P (CLASS X) TYPE)))
	       (T (TYPEP X (CERROR T NIL ':WRONG-TYPE-ARG
				   "~1G~S is not a type known to TYPEP" 'TYPEP TYPE)))))
	 
	(T (COND ((= DTP DTP-INSTANCE)
		  (%P-CONTENTS-OFFSET
		    (%P-CONTENTS-AS-LOCATIVE-OFFSET X 0)   ;Follow forwarding pointers.
		    %INSTANCE-DESCRIPTOR-TYPENAME))
		 ((= DTP DTP-ARRAY-POINTER)
		  (COND ((NAMED-STRUCTURE-P X))
			((STRINGP X) ':STRING)
			(T ':ARRAY)))
		 ((= DTP DTP-ENTITY)
		  (CLASS-SYMBOL X))
		 ((= DTP DTP-EXTENDED-NUMBER) 
		  (SELECT (%P-LDB-OFFSET %%HEADER-TYPE-FIELD X 0)
		    (%HEADER-TYPE-FLONUM ':FLONUM)
		    (%HEADER-TYPE-BIGNUM ':BIGNUM)
		    (%HEADER-TYPE-RATIONAL-BIGNUM ':RATIONAL)
		    (OTHERWISE ':RANDOM)))
		 ((CDR (ASSQ DTP TYPEP-ALIST)))
		 (T ':RANDOM)))))

;;; These next two functions get called by compiler optimization of calls to TYPEP
(DEFUN TYPEP-STRUCTURE (X TYPE &AUX XNAME D)
  (COND ((SETQ XNAME (NAMED-STRUCTURE-P X))
	 (DO () ((EQ XNAME TYPE) T)
	   (OR (AND (SETQ D (GET XNAME 'DEFSTRUCT-DESCRIPTION))
		    (DEFSTRUCT-DESCRIPTION-NAMED-P D)
		    (SETQ XNAME (CAR (DEFSTRUCT-DESCRIPTION-INCLUDE D))))
	       (RETURN NIL))))
	((AND (SETQ D (GET TYPE 'DEFSTRUCT-DESCRIPTION))
	      (DEFSTRUCT-DESCRIPTION-NAMED-P D))
	 NIL)
	(T (TYPEP X TYPE))))			;Optimization turned out to be wrong

(DEFUN TYPEP-FLAVOR (X TYPE &AUX FL)
  (COND ((AND (= (%DATA-TYPE X) DTP-INSTANCE)
	      (= (%P-DATA-TYPE (SETQ FL (%P-CONTENTS-AS-LOCATIVE-OFFSET X 0)))
		 DTP-ARRAY-HEADER)
	      (EQ (AREF (SETQ FL (%MAKE-POINTER DTP-ARRAY-POINTER FL)) 0) 'FLAVOR))
	 (NOT (NULL (MEMQ TYPE (FLAVOR-DEPENDS-ON-ALL FL)))))
	((GET TYPE 'FLAVOR) NIL)
	(T (TYPEP X TYPE))))			;Optimization turned out to be wrong

(DEFUN SUBRP (X &AUX TEM)
    (SETQ TEM (%DATA-TYPE X))
    (OR (= TEM DTP-U-ENTRY)
	(= TEM DTP-FEF-POINTER)))

;T if X is something that can be applied to arguments,
;but abstract objects and stack groups are excluded
;because they are not normally thought of as functions
;even though they interface like functions.
;If ALLOW-SPECIAL-FORMS is NIL, this is only true of real functions.
;Otherwise it is also true of macros and special-form functions.
(DEFUN FUNCTIONP (X &OPTIONAL ALLOW-SPECIAL-FORMS)
  (PROG ()
   LOOP
    (RETURN (SELECT (%DATA-TYPE X)
	      ((DTP-FEF-POINTER DTP-U-ENTRY)
	       (OR ALLOW-SPECIAL-FORMS
		   (NOT (BIT-TEST (LOGIOR %ARG-DESC-QUOTED-REST %ARG-DESC-FEF-QUOTE-HAIR)
				  (%ARGS-INFO X)))))
	      ((DTP-LIST)
	       (COND ((EQ (CAR X) 'LAMBDA)
		      (OR ALLOW-SPECIAL-FORMS (NOT (MEMQ '&QUOTE (CADR X)))))
		     ((EQ (CAR X) 'NAMED-LAMBDA)
		      (OR ALLOW-SPECIAL-FORMS (NOT (MEMQ '&QUOTE (CADDR X)))))
		     ((MEMQ (CAR X) '(SUBST NAMED-SUBST CURRY-BEFORE CURRY-AFTER)) T)))
	      ((DTP-SELECT-METHOD DTP-CLOSURE) T)
	      ((DTP-SYMBOL)
	       (COND ((NOT (FBOUNDP X)) NIL)
		     ((ARRAYP (SETQ X (FSYMEVAL X))))
		     (T (GO LOOP))))))))

(DEFUN PLIST (SYMBOL)
  (IF (SYMBOLP SYMBOL)
      (CAR (PROPERTY-CELL-LOCATION SYMBOL))
      (CDR SYMBOL)))

;;; Number functions.

(DEFSUBST SMALL-FLOATP (X)
  (= (%DATA-TYPE X) #.DTP-SMALL-FLONUM))	;#, would lose with cold-load generator

(DEFUN BIGP (X)
  (LET ((DTP (%DATA-TYPE X)))
    (AND (= DTP DTP-EXTENDED-NUMBER)
	 (= (%P-LDB-OFFSET %%HEADER-TYPE-FIELD X 0) %HEADER-TYPE-BIGNUM))))

(DEFUN GREATERP (NUMBER &REST NUMBERS)
  (DO ((A NUMBER C)
       (B NUMBERS (CDR B))
       (C))
      ((NULL B) T)
    (SETQ C (CAR B))
    (OR (> A C) (RETURN NIL))))

(DEFUN LESSP (NUMBER &REST NUMBERS)
  (DO ((A NUMBER C)
       (B NUMBERS (CDR B))
       (C))
      ((NULL B) T)
    (SETQ C (CAR B))
    (OR (< A C) (RETURN NIL))))

(DEFF > #'GREATERP)

(DEFF < #'LESSP)

(DEFUN <= (NUMBER &REST NUMBERS)
  (DO ((A NUMBER C)
       (B NUMBERS (CDR B))
       (C))
      ((NULL B) T)
    (SETQ C (CAR B))
    (IF (> A C) (RETURN NIL))))

(DEFUN >= (NUMBER &REST NUMBERS)
  (DO ((A NUMBER C)
       (B NUMBERS (CDR B))
       (C))
      ((NULL B) T)
    (SETQ C (CAR B))
    (IF (< A C) (RETURN NIL))))

(DEFF  #'>=)

(DEFF  #'<=)

(DEFUN PLUS (&REST NUMBERS)
    (DO ((NUMBERS NUMBERS (CDR NUMBERS))
	 (ANS 0))
	((NULL NUMBERS) ANS)
      (SETQ ANS (+ ANS (CAR NUMBERS)))))

(DEFF + #'PLUS)
(DEFF +$ #'PLUS)

(DEFUN DIFFERENCE (NUMBER &REST NUMBERS)
    (DO ((NUMBERS NUMBERS (CDR NUMBERS))
	 (ANS NUMBER))
	((NULL NUMBERS) ANS)
      (SETQ ANS (- ANS (CAR NUMBERS)))))

(DEFUN - (NUMBER &REST NUMBERS)
  (COND ((NULL NUMBERS) (MINUS NUMBER))
	((DO ((NUMBERS NUMBERS (CDR NUMBERS))
	      (ANS NUMBER))
	     ((NULL NUMBERS) ANS)
	   (SETQ ANS (- ANS (CAR NUMBERS)))))))

(DEFF -$ #'-)

(DEFUN TIMES (&REST NUMBERS)
    (DO ((NUMBERS NUMBERS (CDR NUMBERS))
	 (ANS 1))
	((NULL NUMBERS) ANS)
      (SETQ ANS (* ANS (CAR NUMBERS)))))

(DEFF * #'TIMES)
(DEFF *$ #'TIMES)

(DEFUN QUOTIENT (NUMBER &REST NUMBERS)
  (DO ((NUMBERS NUMBERS (CDR NUMBERS))
       (ANS NUMBER))
      ((NULL NUMBERS) ANS)
    (SETQ ANS (// ANS (CAR NUMBERS)))))

(DEFUN // (NUMBER &REST NUMBERS)
  (COND ((NULL NUMBERS) (// 1 NUMBER))
	((DO ((NUMBERS NUMBERS (CDR NUMBERS))
	      (ANS NUMBER))
	     ((NULL NUMBERS) ANS)
	   (SETQ ANS (// ANS (CAR NUMBERS)))))))

(DEFF //$ #'//)

(DEFUN \\ (X Y &REST ARGS)
  (LOOP WITH ANSWER = (SYS:INTERNAL-\\ X Y)
	FOR Z IN ARGS
	DO (SETQ ANSWER (SYS:INTERNAL-\\ ANSWER Z))
	FINALLY (RETURN ANSWER)))

(DEFF GCD #'\\)

;MAX and MIN must be written with a single REST arg, otherwise,
; the hack of (APPLY #'MAX xx) can lose because it will try to
; copy the arglist to the stack.
(DEFUN MAX (&REST NUMBERS)
  (LET ((ARG0 (CAR NUMBERS)))
    (CHECK-ARG ARG0 (NUMBERP ARG0) "a number")
    (DO ((REST NUMBERS (CDR REST)))
	((NULL REST) ARG0)
      (SETQ ARG0 (MAX ARG0 (CAR REST))))))

(DEFUN MIN (&REST NUMBERS)
  (LET ((ARG0 (CAR NUMBERS)))
    (CHECK-ARG ARG0 (NUMBERP ARG0) "a number")
    (DO ((REST (CDR NUMBERS) (CDR REST)))
	((NULL REST) ARG0)
      (SETQ ARG0 (MIN ARG0 (CAR REST))))))

(DEFUN LOGAND (ARG1 &REST ARGS)
    (DO ((ANS ARG1 (LOGAND ANS (CAR L)))
	 (L ARGS (CDR L)))
	((NULL L) ANS)))

(DEFUN LOGIOR (ARG1 &REST ARGS)
    (DO ((ANS ARG1 (LOGIOR ANS (CAR L)))
	 (L ARGS (CDR L)))
	((NULL L) ANS)))

(DEFUN LOGXOR (ARG1 &REST ARGS)
    (DO ((ANS ARG1 (LOGXOR ANS (CAR L)))
	 (L ARGS (CDR L)))
	((NULL L) ANS)))

(DEFSUBST LOGNOT (NUMBER) (LOGXOR NUMBER -1))

(DEFUN LOAD-BYTE (FROM-VALUE POSITION WIDTH)
  (LDB (+ (LSH POSITION 6) WIDTH) FROM-VALUE))

(DEFUN DEPOSIT-BYTE (INTO-VALUE POSITION WIDTH BYTE-VALUE)
  (DPB BYTE-VALUE (+ (LSH POSITION 6) WIDTH) INTO-VALUE))

(DEFF ADD1 #'1+)
(DEFF SUB1 #'1-)
(DEFF 1+$ #'1+)
(DEFF 1-$ #'1-)
(DEFF REMAINDER #'\)
(DEFF ^$ #'^)
(DEFF EXPT #'^)

(DEFUN FIXR (FLONUM)
  (FIX (+ FLONUM 0.5s0)))

(DEFUN BOOLE (OP ARG1 &REST ARGS)
  (DO ((ANS ARG1 (*BOOLE OP ANS (CAR L)))
       (L ARGS (CDR L)))
      ((NULL L) ANS)))

(DEFUN SIGNP (&QUOTE TEST &EVAL NUM)
  (COND ((NOT (NUMBERP NUM)) NIL)
	((STRING-EQUAL TEST "L") (< NUM 0))
	((STRING-EQUAL TEST "LE") (<= NUM 0))
	((STRING-EQUAL TEST "E") (= NUM 0))
	((STRING-EQUAL TEST "N") (NEQ NUM 0))
	((STRING-EQUAL TEST "GE") (>= NUM 0))
	((STRING-EQUAL TEST "G") (> NUM 0))
	((FERROR NIL "~S is not a test name for SIGNP" TEST))))

;;; String, pname and character functions.

(DEFF SAMEPNAMEP 'STRING-EQUAL)

;; Compare how X and Y print.  Does X print as something "less" than what Y prints as?
;; Exception: numbers occurring anywhere in X and Y are compared numerically.
(DEFUN ALPHALESSP (X Y)
    (COND ((NUMBERP X) (OR (NOT (NUMBERP Y)) (< X Y)))
	  ((NUMBERP Y) NIL)
	  ((OR (SYMBOLP X) (STRINGP X))
	   (OR (NOT (OR (SYMBOLP Y) (STRINGP Y)))
	       (STRING-LESSP X Y)))
	  ((OR (SYMBOLP Y) (STRINGP Y)) NIL)
	  ((NLISTP X) (OR (LISTP Y)
			  (STRING-LESSP (FORMAT NIL "~S" X) (FORMAT NIL "~S" Y))))
	  ((NLISTP Y) NIL)
	  (T (DO ((X1 X (CDR X1)) (Y1 Y (CDR Y1)))
		 ((NULL Y1))
	       (OR X1 (RETURN T))
	       (AND (ALPHALESSP (CAR X1) (CAR Y1)) (RETURN T))
	       (AND (ALPHALESSP (CAR Y1) (CAR X1)) (RETURN NIL))))))

;; Compare how X and Y print.  Do they print the same way?
;; Exception: numbers occurring anywhere in X and Y are compared numerically.
(DEFUN ALPHAEQUAL (X Y)
    (COND ((NUMBERP X) (AND (NUMBERP Y) (= X Y)))
	  ((OR (SYMBOLP X) (STRINGP X))
	   (AND (OR (SYMBOLP Y) (STRINGP Y))
		(STRING-EQUAL X Y)))
	  ((NLISTP X) (AND (NLISTP Y)
			   (STRING-EQUAL (FORMAT NIL "~S" X) (FORMAT NIL "~S" Y))))
	  (T (DO ((X1 X (CDR X1)) (Y1 Y (CDR Y1)))
		 ((NULL X1) (NULL Y1))
	       (OR Y1 (RETURN NIL))
	       (OR (ALPHAEQUAL (CAR X1) (CAR Y1)) (RETURN NIL))))))

(DEFUN MAKNAM (CHARL)
  (MAKE-SYMBOL (MACLISP-MAKE-STRING CHARL)))

(DEFUN COPYSYMBOL (SYMBOL COPYPROPS &AUX NEWSYM)
  (SETQ NEWSYM (MAKE-SYMBOL (GET-PNAME SYMBOL)))
  (COND (COPYPROPS
	 (AND (BOUNDP SYMBOL)
	      (RPLACA (VALUE-CELL-LOCATION NEWSYM) (CAR (VALUE-CELL-LOCATION SYMBOL))))
	 (AND (FBOUNDP SYMBOL)
	      (RPLACA (FUNCTION-CELL-LOCATION NEWSYM) (CAR (FUNCTION-CELL-LOCATION SYMBOL))))
	 (RPLACA (PROPERTY-CELL-LOCATION NEWSYM)
		 (APPEND (CAR (PROPERTY-CELL-LOCATION SYMBOL)) NIL))))
  NEWSYM)

(SPECIAL *GENSYM-COUNTER *GENSYM-PREFIX) 

(SETQ *GENSYM-PREFIX #/G *GENSYM-COUNTER 0)

(DEFUN GENSYM (&OPTIONAL ARG &AUX PNAME)
  (COND ((NULL ARG))
	((NUMBERP ARG)
	 (SETQ *GENSYM-COUNTER ARG))
	((SYMBOLP ARG)
	 (SETQ *GENSYM-PREFIX (AR-1 (GET-PNAME ARG) 0)))
	((STRINGP ARG)
	 (SETQ *GENSYM-PREFIX (AR-1 ARG 0))))
  (AND (> (SETQ *GENSYM-COUNTER (1+ *GENSYM-COUNTER)) 9999.)
       (SETQ *GENSYM-COUNTER 0))
  (SETQ PNAME (MAKE-ARRAY 5 ':TYPE 'ART-STRING))
  (AS-1 *GENSYM-PREFIX PNAME 0)
  (AS-1 (+ 60 (// *GENSYM-COUNTER 1000.)) PNAME 1)
  (AS-1 (+ 60 (\ (// *GENSYM-COUNTER 100.) 10.)) PNAME 2)
  (AS-1 (+ 60 (\ (// *GENSYM-COUNTER 10.) 10.)) PNAME 3)
  (AS-1 (+ 60 (\ *GENSYM-COUNTER 10.)) PNAME 4)
  (MAKE-SYMBOL PNAME))

(DEFUN MACLISP-MAKE-STRING (CHARL &OPTIONAL AREA &AUX PNAME)
  (SETQ PNAME (MAKE-ARRAY (LENGTH CHARL) ':AREA AREA ':TYPE 'ART-STRING))
  (DO ((I 0 (1+ I))
       (L CHARL (CDR L)))
      ((NULL L))
    (AS-1 (CHARACTER (CAR L)) PNAME I))
   PNAME)

(DEFUN GETCHARN (S I)
  (SETQ S (STRING S))
  (COND ((AND (> I 0) (<= I (ARRAY-ACTIVE-LENGTH S)))
	 (AR-1 S (1- I)))
	(T 0)))

(DEFUN GETCHAR (S I)
  (SETQ S (STRING S))
  (COND ((AND (> I 0) (<= I (ARRAY-ACTIVE-LENGTH S)))
	 (ASCII (AR-1 S (1- I))))
	(T NIL)))

(DEFUN ASCII (N)
  (LET ((STR (MAKE-ARRAY 1 ':AREA P-N-STRING ':TYPE 'ART-STRING)))
    (ASET N STR 0)
    (MULTIPLE-VALUE-BIND (SYM FLAG) (INTERN STR)
      (AND FLAG (RETURN-ARRAY STR))
      SYM)))

(DEFUN STRING (X)
  (COND ((STRINGP X) X)
        ((SYMBOLP X)
	 (GET-PNAME X))
	((AND (FIXP X)
	      ( X 0)
	      (< X 400))
	 (ASET X (SETQ X (MAKE-ARRAY 1 ':TYPE 'ART-STRING)) 0)
	 X)
	((AND (TYPEP X ':INSTANCE)
	      (FUNCALL X ':SEND-IF-HANDLES ':STRING-FOR-PRINTING)))
	(T 
	 (FERROR NIL "Cannot coerce ~S into a string" X))))

(DEFUN CHARACTER (X)
   (COND ((NUMBERP X)
	  X)
	 ((STRINGP X)
	  (AREF X 0))
	 ((SYMBOLP X)
	  (AREF (GET-PNAME X) 0))
	 (T (FERROR NIL "Cannot coerce ~S into a character" X))))

(DEFUN IMPLODE (X)
  (PROG (VAL TEM TOK)
	(MULTIPLE-VALUE (VAL TEM)
	  (INTERN (SETQ TOK (MACLISP-MAKE-STRING X P-N-STRING))))
	(AND TEM (RETURN-ARRAY TOK))
	(RETURN VAL)))

(DEFUN READLIST (*IOLST &AUX (*IOCH NIL))
  (READ 'READLIST-STREAM))

(DEFPROP READLIST-STREAM T IO-STREAM-P)

(DEFUN READLIST-STREAM (OPERATION &OPTIONAL ARG1 &REST REST)
  (COND ((EQ OPERATION ':TYI)
	 (COND ((EQ *IOCH T)
		(FERROR NIL "EOF in middle of READLIST"))
	       ((NOT (NULL *IOCH))
		(PROG2 NIL *IOCH (SETQ *IOCH NIL)))
	       ((NULL *IOLST)
		(SETQ *IOCH T)
		40)
	       (T (PROG1 (CHARACTER (CAR *IOLST))
			 (SETQ *IOLST (CDR *IOLST))))))
	((EQ OPERATION ':UNTYI)
	 (SETQ *IOCH ARG1))
	((EQ OPERATION ':WHICH-OPERATIONS)
	 '(:TYI :UNTYI))
	(T (STREAM-DEFAULT-HANDLER 'READLIST-STREAM OPERATION ARG1 REST))))

(DEFUN READ-FROM-STRING (STRING &OPTIONAL (EOF-OPTION 'NO-EOF-OPTION)
			 (START-CHAR-POSITION 0)
			 &AUX (*IOLST STRING) (*IOCH START-CHAR-POSITION))
  (DECLARE (RETURN-LIST CONTENTS END-CHAR-POSITION))
  (VALUES (READ 'READ-FROM-STRING-STREAM EOF-OPTION) *IOCH))

(DEFPROP READ-FROM-STRING-STREAM T IO-STREAM-P)

(DEFUN READ-FROM-STRING-STREAM (OPERATION &OPTIONAL ARG1 &REST REST)
  (COND ((EQ OPERATION ':TYI)
	 (COND ((> *IOCH (ARRAY-ACTIVE-LENGTH *IOLST))
		(AND ARG1 (ERROR ARG1)))
	       ((= *IOCH (ARRAY-ACTIVE-LENGTH *IOLST))
		(SETQ *IOCH (1+ *IOCH))
		40)
	       (T (PROG2 NIL (AR-1 *IOLST *IOCH)
			     (SETQ *IOCH (1+ *IOCH))))))
	((EQ OPERATION ':UNTYI)
	 (SETQ *IOCH (1- *IOCH)))
	((EQ OPERATION ':WHICH-OPERATIONS)
	 '(:TYI :UNTYI))
	(T (STREAM-DEFAULT-HANDLER 'READ-FROM-STRING-STREAM OPERATION ARG1 REST))))

(SPECIAL *IOLST *IOCH)	;USED BY READLIST, EXPLODE, ETC.

(DEFUN EXPLODE (X &AUX (*IOLST NIL) (*IOCH T))
  (PRIN1 X (FUNCTION EXPLODE-STREAM))
  (NREVERSE *IOLST))

(DEFUN EXPLODEC (X &AUX (*IOLST NIL) (*IOCH T))
  (PRINC X (FUNCTION EXPLODE-STREAM))
  (NREVERSE *IOLST))

(DEFUN EXPLODEN (X &AUX (*IOLST NIL) (*IOCH NIL))
  (PRINC X (FUNCTION EXPLODE-STREAM))
  (NREVERSE *IOLST))

(DEFPROP EXPLODE-STREAM T IO-STREAM-P)

(DEFUN EXPLODE-STREAM (OPERATION &OPTIONAL ARG1 &REST REST &AUX STR OLDP)
  (COND ((EQ OPERATION ':TYO)
	 (COND (*IOCH
		(MULTIPLE-VALUE (ARG1 OLDP) (INTERN (SETQ STR (STRING ARG1))))
		(AND OLDP (RETURN-ARRAY STR))))
	 (SETQ *IOLST (CONS ARG1 *IOLST)))
	((EQ OPERATION ':WHICH-OPERATIONS)
	 '(:TYO))
	(T (STREAM-DEFAULT-HANDLER 'EXPLODE-STREAM OPERATION ARG1 REST))))

(DEFUN FLATSIZE (X &AUX (*IOCH 0))
  (PRIN1 X (FUNCTION FLATSIZE-STREAM))
  *IOCH)

(DEFUN FLATC (X &AUX (*IOCH 0))
  (PRINC X (FUNCTION FLATSIZE-STREAM))
  *IOCH)

(DEFPROP FLATSIZE-STREAM T IO-STREAM-P)

(DEFUN FLATSIZE-STREAM (OPERATION &OPTIONAL ARG1 &REST REST)
  (COND ((EQ OPERATION ':TYO)
	 (SETQ *IOCH (1+ *IOCH)))
	((EQ OPERATION ':WHICH-OPERATIONS)
	 '(:TYO))
	(T (STREAM-DEFAULT-HANDLER 'FLATSIZE-STREAM OPERATION ARG1 REST))))

(DEFUN READLINE (&REST READ-ARGS &AUX CH EOF STRING IDX LEN)
  (DECLARE (ARGLIST &OPTIONAL STREAM EOF-OPTION OPTIONS))
  (LET ((OPTIONS NIL))
    ;; This kludge is to let us take a third, optional argument.
    (COND ((> (LENGTH READ-ARGS) 2)
	   (SETQ OPTIONS (THIRD READ-ARGS))
	   (SETQ READ-ARGS (LIST (FIRST READ-ARGS) (SECOND READ-ARGS)))))
    (MULTIPLE-VALUE-BIND (STREAM EOF-OPTION)
	(DECODE-READ-ARGS READ-ARGS)
      ;; If stream does rubout handling, get inside the rubout handler
      (COND ((AND (NOT RUBOUT-HANDLER)
		  (MEMQ ':RUBOUT-HANDLER (FUNCALL STREAM ':WHICH-OPERATIONS)))
	     ;;Stream with rubouts assumed not to have EOFs
	     (FUNCALL STREAM ':RUBOUT-HANDLER OPTIONS #'READLINE STREAM EOF-OPTION))
	    ;; If stream does line-reading, let it do it
	    ((MEMQ ':LINE-IN (FUNCALL STREAM ':WHICH-OPERATIONS))
	     (MULTIPLE-VALUE (STRING EOF) (FUNCALL STREAM ':LINE-IN T))
	     (IF EOF
		 (IF (EQ EOF-OPTION 'NO-EOF-OPTION)
		     (FERROR NIL "End of file encountered on stream ~S" STREAM)
		     EOF-OPTION)
		 STRING))
	    ;; Accumulate a string until CR, ignoring control characters
	    ;; If immediate EOF return EOF to caller, otherwise take chars up to EOF as a line
	    ((NULL (SETQ CH (FUNCALL STREAM ':TYI)))
	     EOF-OPTION)
	    (T
	     (UNWIND-PROTECT
	       (PROGN (SETQ STRING (MAKE-ARRAY (SETQ LEN 200.) ':TYPE 'ART-STRING)
			    IDX 0)
		      (DO () (NIL)
			(COND ((OR (NULL CH) (= CH #\CR))
			       (ADJUST-ARRAY-SIZE STRING IDX)
			       (RETURN (PROG1 STRING (SETQ STRING NIL))))
			      ((LDB-TEST %%KBD-CONTROL-META CH) )	;Ignore controls
			      (T (AND (= IDX LEN)
				      (ADJUST-ARRAY-SIZE STRING (SETQ LEN (+ LEN 100))))
				 (ASET CH STRING IDX)
				 (SETQ IDX (1+ IDX))))
			(SETQ CH (FUNCALL STREAM ':TYI))))
	       (AND STRING (RETURN-ARRAY STRING))))))))

;;; Array functions.

(DEFUN AREF (ARRAY &REST SUBSCRIPTS)
   (CHECK-ARG ARRAY ARRAYP "an array")
   (APPLY ARRAY SUBSCRIPTS))

(DEFUN ASET (ELEMENT ARRAY &REST SUBSCRIPTS)
   (CHECK-ARG ARRAY ARRAYP "an array")
   (STORE (APPLY ARRAY SUBSCRIPTS) ELEMENT))

(DEFUN ALOC (ARRAY &REST SUBSCRIPTS)
   (CHECK-ARG ARRAY ARRAYP "an array")
   (GET-LOCATIVE-POINTER-INTO-ARRAY (APPLY ARRAY SUBSCRIPTS)))

(DEFUN STORE (&QUOTE ARRAY-REFERENCE VALUE-EXPRESSION)
  (XSTORE (EVAL VALUE-EXPRESSION) (EVAL ARRAY-REFERENCE)))

(DEFUN ARRAYDIMS (ARRAY &AUX TYPE)
  (AND (SYMBOLP ARRAY) (SETQ ARRAY (FSYMEVAL ARRAY)))
  (CHECK-ARG ARRAY ARRAYP "an array")
	;SHOULD CHECK FOR INVZ
  (SETQ TYPE (NTH (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0) ARRAY-TYPES))
  (CONS TYPE (ARRAY-DIMENSIONS ARRAY)))

(DEFUN ARRAY-DIMENSIONS (ARRAY &AUX INDEX-LENGTH NDIMS LONG-ARRAY-P DIMS PRODUCT)
  (AND (SYMBOLP ARRAY) (SETQ ARRAY (FSYMEVAL ARRAY)))
  (CHECK-ARG ARRAY ARRAYP "an array")
	;SHOULD CHECK FOR INVZ
  (SETQ NDIMS (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)
	LONG-ARRAY-P (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))
  (SETQ INDEX-LENGTH (COND ((= 0 (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0))
			    (COND ((= 1 LONG-ARRAY-P) (%P-LDB-OFFSET %%Q-POINTER ARRAY 1))
				  (T (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0))))
			   ((%P-LDB-OFFSET %%Q-POINTER ARRAY (1+ (+ NDIMS LONG-ARRAY-P))))))
  (DO ((N NDIMS (1- N))
       (I (1+ LONG-ARRAY-P) (1+ I)))
      ((<= N 1))
      (SETQ DIMS (CONS (%P-LDB-OFFSET %%Q-POINTER ARRAY I) DIMS)))
  (SETQ PRODUCT (LIST-PRODUCT DIMS))
  (NRECONC DIMS (NCONS (COND ((ZEROP PRODUCT) 0)
			     (T (// INDEX-LENGTH PRODUCT))))))

;Returns the number of bits that fit in an element of an array.
(DEFUN ARRAY-ELEMENT-SIZE (ARRAY)
  (OR (CDR (ASSQ (AREF #'ARRAY-TYPES (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0))
		 ARRAY-BITS-PER-ELEMENT))
      24.))	;Q-type, assume going to use unsigned fixnums.

(DEFUN ARRAY-PUSH-EXTEND (ARRAY DATA &OPTIONAL EXTENSION
			  &AUX (INHIBIT-SCHEDULING-FLAG T))
    (COND ((ARRAY-PUSH ARRAY DATA))
	  (T (ADJUST-ARRAY-SIZE ARRAY (+ (ARRAY-LENGTH ARRAY)
					 ;; If amount to extend by not specified,
					 ;; try to guess a reasonable amount
					 (COND (EXTENSION)
					       ((< (%STRUCTURE-TOTAL-SIZE ARRAY) PAGE-SIZE)
						(MAX (ARRAY-LENGTH ARRAY) 100))
					       (T (// (ARRAY-LENGTH ARRAY) 4)))))
	     (ARRAY-PUSH ARRAY DATA))))

(DEFUN ARRAY-IN-BOUNDS-P (ARRAY &REST POINT)
  (OR (= (LENGTH POINT) (ARRAY-#-DIMS ARRAY))
      (FERROR NIL "~S~:S wrong number of array subscripts" ARRAY POINT))
  (LOOP FOR SUBSCRIPT IN POINT AS I UPFROM 1
	DO (IF (OR (MINUSP SUBSCRIPT) ( SUBSCRIPT (ARRAY-DIMENSION-N I ARRAY)))
	       (RETURN NIL))
	FINALLY (RETURN T)))

(DEFUN ARRAY-DISPLACED-P (ARRAY)
    (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1))

(DEFUN ARRAY-INDIRECT-P (ARRAY &AUX OFFSET)
    (SETQ OFFSET (+ (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0)
		    (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)))
    (AND (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1)
	 (= (%P-LDB-OFFSET %%Q-DATA-TYPE ARRAY OFFSET) DTP-ARRAY-POINTER)))

;This is random, maybe it should be flushed.
(DEFUN ARRAY-INDEXED-P (ARRAY &AUX OFFSET)
    (SETQ OFFSET (+ (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0)
		    (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)))
    (AND (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1)
	 (= (%P-LDB-OFFSET %%Q-DATA-TYPE ARRAY OFFSET) DTP-ARRAY-POINTER)
	 (= (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0) 3)))

(DEFUN ARRAY (&QUOTE X TYPE &EVAL &REST DIMLIST)
    (APPLY (FUNCTION *ARRAY) (CONS X (CONS TYPE DIMLIST))))

(DEFUN *ARRAY (X TYPE &REST DIMLIST &AUX ARRAY)
    (AND (MEMQ TYPE '(READTABLE OBARRAY))
	 (FERROR NIL "The array type ~S is not yet in" TYPE))
    (SETQ ARRAY
	  (MAKE-ARRAY DIMLIST ':TYPE (IF (EQ TYPE 'FLONUM) 'ART-FLOAT 'ART-Q)))
    (IF (EQ TYPE 'FIXNUM)
	(FILL-ARRAY ARRAY (APPLY #'* DIMLIST) 0))
    (COND ((NULL X)
	   ARRAY)
	  ((SYMBOLP X)
	   (RPLACA (FUNCTION-CELL-LOCATION X) ARRAY)
	   X)
	  (T (FERROR NIL "~S is not a legal first arg for *ARRAY" X))))

(DEFUN FILL-ARRAY (ARRAY SIZE VAL)
  (DO ((ARY (MAKE-ARRAY SIZE ':DISPLACED-TO ARRAY))
       (I 0 (1+ I)))
      ((= I SIZE) (RETURN-ARRAY ARY))
    (ASET VAL ARY I)))

(DEFUN MAKE-ARRAY-INTO-NAMED-STRUCTURE (ARRAY)
    (COND ((ARRAYP ARRAY)
	   (%P-DPB-OFFSET 1 %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY 0)
	   ARRAY)
	  (T (FERROR NIL "~S is not an array" ARRAY))))

;; Alter the dimensions of an array, using an invisible pointer,
;; preserving all elements whose indices continue to be within range.
;; The leader is also preserved.  Like ADJUST-ARRAY-SIZE, we return the
;; new array, in case you want to make things point directly at it to save indirct cycles.
(DEFUN ARRAY-GROW (ARRAY &REST DIMENSIONS
			 &AUX (OLD-DIMS (ARRAY-DIMENSIONS ARRAY))
			 INDEX NEW-ARRAY)
    (PROG ()
        (CHECK-ARG ARRAY ARRAYP "an array")
	;; Extend or truncate the supplied list of dimensions.
	;; Omitted dimensions are left unchanged.
	(AND (< (LENGTH DIMENSIONS) (LENGTH OLD-DIMS))
	     (SETQ DIMENSIONS (APPEND DIMENSIONS (NTHCDR (LENGTH DIMENSIONS) OLD-DIMS))))
	(AND (> (LENGTH DIMENSIONS) (LENGTH OLD-DIMS))
	     (SETQ DIMENSIONS (FIRSTN (LENGTH OLD-DIMS) DIMENSIONS)))
	;; If it's 1-dimensional, might as well try to grow it in place.
	(AND (NULL (CDR DIMENSIONS))
	     (RETURN (ADJUST-ARRAY-SIZE ARRAY (CAR DIMENSIONS))))
	;; Make the new array.
	(SETQ NEW-ARRAY (MAKE-ARRAY DIMENSIONS
				    ':AREA (%AREA-NUMBER ARRAY)
				    ':TYPE (ARRAY-TYPE ARRAY)
				    ':LEADER-LENGTH (ARRAY-LEADER-LENGTH ARRAY)))
	;; Copy the array leader.
        (DO ((I 0 (1+ I))
             (N (OR (ARRAY-LEADER-LENGTH ARRAY) 0) (1- N)))
            ((ZEROP N))
	  (SETF (ARRAY-LEADER NEW-ARRAY I) (ARRAY-LEADER ARRAY I)))

	;; Check for zero-size array, which the code below doesn't handle correctly
	(AND (DO ((L DIMENSIONS (CDR L)) (L1 OLD-DIMS (CDR L1))) ((NULL L) NIL)
	       (AND (OR (ZEROP (CAR L)) (ZEROP (CAR L1)))
		    (RETURN T)))
	     (GO DONE))

	;; Create a vector of fixnums to use as subscripts to step thru the arrays.
	(SETQ INDEX NIL)
	(DO ((L DIMENSIONS (CDR L))) ((NULL L))
	   (SETQ INDEX (CONS 0 INDEX)))

        ;; Make the first increment of INDEX bring us to element 0 0 0 0..
        (RPLACA INDEX -1)

	LOOP
	
	;; Increment the vector of subscripts INDEX.
        ;; Go to DONE if we have exhausted all elements that need copying.
	(DO ((I INDEX (CDR I))
	     (O OLD-DIMS (CDR O))
	     (N DIMENSIONS (CDR N)))
	    ((NULL I) (GO DONE))
	   ;; Increment one index
	   (RPLACA I (1+ (CAR I)))
	   ;; and decide whether to "carry" to the next one.
	   (COND ((OR (>= (CAR I) (CAR O)) (>= (CAR I) (CAR N)))
		  (RPLACA I 0))
		 (T (RETURN NIL))))

	(STORE (APPLY NEW-ARRAY INDEX) (APPLY ARRAY INDEX))
	(GO LOOP)

	DONE

	;; The contents have been copied.  Copy a few random things.
	(%P-DPB (%P-LDB %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY)
		%%ARRAY-NAMED-STRUCTURE-FLAG NEW-ARRAY)
	(%P-DPB (%P-LDB %%ARRAY-FLAG-BIT ARRAY)
		%%ARRAY-FLAG-BIT NEW-ARRAY)
	(STRUCTURE-FORWARD ARRAY NEW-ARRAY)
	(RETURN NEW-ARRAY)))

;The argument must really be a structure, not a locative into the middle
;of something, and must not be in list space.  We store DTP-HEADER-FORWARD
;and DTP-BODY-FORWARDs from the old instance to the new instance, and return the old.
(DEFUN STRUCTURE-FORWARD (OLD NEW &AUX SIZE BASE)
  (OR (= (%DATA-TYPE OLD) (%DATA-TYPE NEW))
      (FERROR NIL "~S and ~S seem incompatible" OLD NEW))
  (OR (= (LDB %%REGION-REPRESENTATION-TYPE (REGION-BITS (%REGION-NUMBER OLD)))
	 %REGION-REPRESENTATION-TYPE-STRUCTURE)
      (FERROR NIL "~S is not in a structure region" OLD))
  (WITHOUT-INTERRUPTS	;Don't let anything move while in inconsistent state
    (SETQ BASE (%FIND-STRUCTURE-LEADER OLD))
    (SETQ SIZE (%STRUCTURE-TOTAL-SIZE BASE))
    (DO ((P (%POINTER BASE) (1+ P))
	 (Z (+ (%POINTER BASE) SIZE)))
	((= P Z))
      (%P-STORE-TAG-AND-POINTER P DTP-BODY-FORWARD OLD))
    (%P-STORE-TAG-AND-POINTER OLD DTP-HEADER-FORWARD NEW)
    OLD))

;Make two symbols share the same value, in spite of binding
;Don't do this while FROM-SYMBOL is bound, or it will get undone
;when it is unbound, since the microcode does not bother to take
;the overhead to check for that case.
(DEFUN FORWARD-VALUE-CELL (FROM-SYMBOL TO-SYMBOL)
  (CHECK-ARG FROM-SYMBOL SYMBOLP "a symbol")
  (CHECK-ARG TO-SYMBOL SYMBOLP "a symbol")
  (AND (EQ FROM-SYMBOL TO-SYMBOL)
       (FERROR NIL "Forwarding symbol's value to itself"))
  (%P-STORE-TAG-AND-POINTER (VALUE-CELL-LOCATION FROM-SYMBOL)
			    DTP-ONE-Q-FORWARD
			    (VALUE-CELL-LOCATION TO-SYMBOL)))

;Like FOLLOW-STRUCTURE-FORWARDING
(DEFUN FOLLOW-CELL-FORWARDING (LOC EVCP-P)
  (DO () (NIL)
    (SELECT (%P-DATA-TYPE LOC)
      ((DTP-HEADER-FORWARD DTP-BODY-FORWARD)
       (SETQ LOC (FOLLOW-STRUCTURE-FORWARDING LOC)))
      (DTP-ONE-Q-FORWARD
       (SETQ LOC (%MAKE-POINTER (%DATA-TYPE LOC) (%P-CONTENTS-AS-LOCATIVE LOC))))
      (DTP-EXTERNAL-VALUE-CELL-POINTER
       (OR EVCP-P (RETURN LOC))
       (SETQ LOC (%MAKE-POINTER (%DATA-TYPE LOC) (%P-CONTENTS-AS-LOCATIVE LOC))))
      (OTHERWISE (RETURN LOC)))))

(DEFUN CAR-LOCATION (CONS)
    (CHECK-ARG CONS (= (%DATA-TYPE CONS) DTP-LIST) "a CONS")
    (%MAKE-POINTER DTP-LOCATIVE CONS))

(DEFUN GET-LOCATION (SYMBOL PROPERTY)
    (DO ((L (PLIST SYMBOL) (CDDR L)))
        ((NULL L)
         (PUTPROP SYMBOL NIL PROPERTY)
         (GET-LOCATION SYMBOL PROPERTY))
       (AND (EQ (CAR L) PROPERTY)
            (RETURN (CAR-LOCATION (CDR L))))))

(DEFUN DECLARE (&QUOTE &REST IGNORE) 'DECLARE)

;This definition assumes we are evalling.
;COMPILE-DRIVER takes care of compiling and loading.
(DEFUN EVAL-WHEN (&QUOTE TIMES &REST FORMS &AUX VAL)
    (COND ((MEMQ 'EVAL TIMES)
	   (DOLIST (FORM FORMS) (SETQ VAL (EVAL FORM)))
	   VAL)))

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
				    (IF (ATOM (CAR AL)) AL	;New format
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
				    (IF (ATOM (CAR AL)) AL	;New format
					(CAR AL))))		;Old format
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
			 (IF (ATOM (CAR AL)) AL		;New format
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

;Like %ARGS-INFO but also works for interpreted functions
(DEFUN ARGS-INFO (FCN)
  ;; First, convert FCN from a function-spec to a function
  (LOOP WHILE (OR (SYMBOLP FCN)
		  (AND (LISTP FCN) (NOT (MEMQ (CAR FCN) '(LAMBDA NAMED-LAMBDA SUBST NAMED-SUBST
							  CURRY-BEFORE CURRY-AFTER MACRO)))))
	DO (SETQ FCN (FDEFINITION FCN)))
  (COND ((NLISTP FCN)
	 (%ARGS-INFO FCN))
	((MEMQ (CAR FCN) '(LAMBDA NAMED-LAMBDA SUBST NAMED-SUBST))
	 (AND (OR (EQ (CAR FCN) 'NAMED-LAMBDA) (EQ (CAR FCN) 'NAMED-SUBST))
	      (POP FCN))
	 (ARGS-INFO-FROM-LAMBDA-LIST (CADR FCN)))
	(T %%ARG-DESC-EVALED-REST)))		;Most unspecific value

(DEFUN ARGS-INFO-FROM-LAMBDA-LIST (LL &AUX (FLAGS 0) QUOTE MIN (N 0))
  (DO L LL (CDR L) (NULL L)
    (SELECTQ (CAR L)
	     (&QUOTE (SETQ QUOTE T))
	     ((&EVAL &QUOTE-DONTCARE) (SETQ QUOTE NIL))
	     (&OPTIONAL (SETQ MIN N))
	     (&AUX (RETURN NIL))
	     (&REST (RETURN (SETQ FLAGS (LOGIOR FLAGS
						(COND (QUOTE %ARG-DESC-QUOTED-REST)
						      (T %ARG-DESC-EVALED-REST))))))
	     (OTHERWISE					;A VARIABLE
	      (COND ((NOT (MEMQ (CAR L) LAMBDA-LIST-KEYWORDS))
		     (IF QUOTE				;QUOTED REGULAR ARGS PRESENT
			 (SETQ FLAGS (LOGIOR FLAGS (LOGIOR %ARG-DESC-INTERPRETED
							   %ARG-DESC-FEF-QUOTE-HAIR))))
		     (SETQ N (1+ N)))))))
  (OR MIN (SETQ MIN N)) ;NO OPTIONALS
  (DPB N %%ARG-DESC-MAX-ARGS
       (DPB MIN %%ARG-DESC-MIN-ARGS
	    FLAGS)))

;; Return the debugging info alist of a function.  NIL if there is none or unreasonable arg.
;; Elements of the alist look like one of these things:
;; (SI:ENCAPSULATED-DEFINITION <internal symbol> <type of encapsulation>)
;;    This means that this definition was made to encapsulate an inner definition.
;;    See the file LISPM2;ENCAPS > for more info on these.
;; (SI:RENAMINGS <alist of renamings>)
;;    This sort of item is used together with (ENCAPSULATED-DEFINITION ... SI:RENAME-WITHIN).
;;    It specifies what renamings are to be done to the original definition.
;;    It is an alist of entries of the form (<symbol to rename>  <new name>).
;;    See LISPM2;ENCAPS > for more information.
;; (ARGLIST <arglist>)
;;    The CADR of this element is an arglist to give the user when he asks.
;;    It is set up by having (LOCAL-DECLARE ((ARGLIST . <arglist>)) ...)
;;    around the function definition when it is compiled.
;;    This is for when the function's actual arglist is not informative enough.
;; (RETURN-LIST <return-list>)
;;    The CADR of this is the list of names of returned values,
;;    to return as the second value if the user calls ARGLIST.
;;    It is set up only by a local declare like the one which specifies
;;    an arglist (above).
;; (COMPILER:LOCAL-MAP <local map>)
;;    The CADR of this element is a local map which indicates how local variables
;;    are assigned to slots in the function's local block.
;;    The n'th element of the map is a list of the locals that live there.
;;    Actually, as of now, only one local can live in each slot, so the elements
;;    of the map are at most of length one.  The format was chosen to allow expansion.
;; (COMPILER:ARG-MAP <arg map>)
;;    This is just like a local map except that it describes slots in the argument block
;;    rather than slots in the local block.  It replaces keeping names in the ADL.
;; (SYS:FUNCTION-PARENT <name>)
;;    Gives the name of a definition whose source code includes this function.  This
;;    is for functions automatically generated by defstruct, defflavor, etc.
;; (:INTERNAL-FEF-OFFSETS <o1> <o2> ...)
;;    Offsets within the fef of function cells for the :INTERNAL functions
;;
;; The debugging info in a fef is made by the (DEBUG-INFO ...) lap instruction.
;; A NAMED-LAMBDA can also have debugging info.  If its CADR is not a symbol,
;; then it should be a list whose car is the function name and whose cdr is
;; the debugging info alist.

;; Our arg can be a function or a function spec.
(DEFUN DEBUGGING-INFO (FUNCTION)
    (DO () ((NOT (SYMBOLP FUNCTION)))
      (SETQ FUNCTION (FSYMEVAL FUNCTION)))
    (COND ((LISTP FUNCTION)
           (COND ((AND (OR (EQ (CAR FUNCTION) 'NAMED-LAMBDA)
			   (EQ (CAR FUNCTION) 'NAMED-SUBST))
		       (LISTP (CADR FUNCTION)))
		  (CDADR FUNCTION))
		 ((MEMQ (CAR FUNCTION) '(LAMBDA NAMED-LAMBDA SUBST NAMED-SUBST
						CURRY-BEFORE CURRY-AFTER MACRO))
		  NIL)
		 (T (DEBUGGING-INFO (FDEFINITION FUNCTION)))))
          ((EQ (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
           (AND (LDB-TEST %%FEFHI-MS-DEBUG-INFO-PRESENT
			  (%P-CONTENTS-OFFSET FUNCTION %FEFHI-MISC))
                (%P-CONTENTS-OFFSET FUNCTION (1- (%P-LDB %%FEFH-PC-IN-WORDS FUNCTION)))))))

;; Old name which should be flushed eventually.
(DEFF FUNCTION-DEBUGGING-INFO 'DEBUGGING-INFO)

(DEFF LEXICAL-CLOSURE #'QUOTE)

;;; Macro expansion.

;Expand any macros in top level of a form.
;MACROEXPAND-1 MACRO-CALL iteratively until it can't expand any more.
;MACROEXPAND, MACROEXPAND-1 and OPEN-CODE-P have alternate Maclisp-only versions in QCP1.
(DEFUN MACROEXPAND (MACRO-CALL &OPTIONAL IGNORE)
    (DO ((TM MACRO-CALL (MACROEXPAND-1 TM))
	 (OTM NIL TM))
	((OR (EQ TM OTM) (ATOM TM)) TM)))

;Macroexpand MACRO-CALL once, if possible.
;If there is nothing to expand, return it unchanged.
;Macros, open-coded functions and CURRY-BEFORE and CURRY-AFTER are expanded.
(DEFUN MACROEXPAND-1 (MACRO-CALL &OPTIONAL IGNORE)
    (PROG (TM)
	  (AND (ATOM MACRO-CALL) (RETURN MACRO-CALL))
	  (COND ((NOT (ATOM (CAR MACRO-CALL)))
		 (COND ((EQ (CAAR MACRO-CALL) 'CURRY-AFTER)
			(RETURN `(,(CADAR MACRO-CALL) ,@(CDR MACRO-CALL) . ,(CDDAR MACRO-CALL))))
		       ((EQ (CAAR MACRO-CALL) 'CURRY-BEFORE)
			(RETURN `(,(CADAR MACRO-CALL) ,@(CDDAR MACRO-CALL) . ,(CDR MACRO-CALL))))
                       ((OR (EQ (CAAR MACRO-CALL) 'SUBST) (EQ (CAAR MACRO-CALL) 'NAMED-SUBST))
                        (RETURN (SUBST-EXPAND (CAR MACRO-CALL) MACRO-CALL))))
		 (RETURN MACRO-CALL))
		((NOT (SYMBOLP (CAR MACRO-CALL))))
		((SETQ TM (DECLARED-DEFINITION (CAR MACRO-CALL)))
		 (RETURN (COND ((ATOM TM) MACRO-CALL)
			       ((EQ (CAR TM) 'MACRO) (FUNCALL (CDR TM) MACRO-CALL))
			       ((OR (EQ (CAR TM) 'SUBST)
				    (EQ (CAR TM) 'NAMED-SUBST))
				(SUBST-EXPAND TM MACRO-CALL))
			       (T MACRO-CALL))))
		((SETQ TM (OPEN-CODE-P (CAR MACRO-CALL)))
		 (RETURN (CONS TM (CDR MACRO-CALL)))))
	  (RETURN MACRO-CALL)))

;; Given a function-spec, find any definition it has been declared to have
;; for compilation purposes, and return it.
;; Unless the definition is a macro, unencapsulate it.
(DEFUN DECLARED-DEFINITION (FUNCTION &AUX DEF)
  (SETQ DEF (OR (DOLIST (L LOCAL-DECLARATIONS)
		  (AND (EQ (CAR L) 'DEF)
		       (EQUAL (CADR L) FUNCTION)	;Not EQ, might be a list
		       (RETURN (CDDR L))))
		(DOLIST (L FILE-LOCAL-DECLARATIONS)
		  (AND (EQ (CAR L) 'DEF)
		       (EQUAL (CADR L) FUNCTION)	;Not EQ, might be a list
		       (RETURN (CDDR L))))
		(AND (FDEFINEDP FUNCTION)
		     (SETQ DEF (FDEFINITION FUNCTION))
		     (COND ((ATOM DEF) DEF)
			   ((EQ (CAR DEF) 'MACRO) DEF)
			   (T (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC FUNCTION)))))))
  (COND ((AND DEF (SYMBOLP DEF)) (DECLARED-DEFINITION DEF))
	(T DEF)))

;; Expand a call to a SUBST function.  SUBST is the function definition to use.
;; FORM is the whole form.
;; Match the SUBST args with the expressions in the form
;; and then substitute the expressions for the args in the body of the function with SUBLIS.

(DEFUN SUBST-EXPAND (SUBST FORM)
  (LET (ALIST OPTIONAL-FLAG LAMBDA-LIST BODY FN-NAME)
    (COND ((EQ (CAR SUBST) 'NAMED-SUBST)
	   (SETQ LAMBDA-LIST (CADDR SUBST) BODY (CDDDR SUBST))
	   (SETQ FN-NAME (COND ((SYMBOLP (CADR SUBST)) (CADR SUBST))
			       (T (CAADR SUBST)))))
	  (T (SETQ LAMBDA-LIST (CADR SUBST) BODY (CDDR SUBST)
		   FN-NAME (CAR FORM))))
    ;; Provide an implicit PROGN for the body.
    (COND ((CDR BODY) (SETQ BODY `(PROGN . ,BODY)))
	  (T (SETQ BODY (CAR BODY))))
    ;; Process the lambda list and args to make the alist.
    (DO ((VALS (CDR FORM) (CDR VALS)))
	(NIL)
      ;; We allow only &OPTIONAL.
      (DO () ((NEQ (CAR LAMBDA-LIST) '&OPTIONAL))
	(SETQ OPTIONAL-FLAG T)
	(POP LAMBDA-LIST))
      ;; Detect runout of lambda list or of args.
      (COND ((NULL VALS)
	     (COND ((NULL LAMBDA-LIST)
		    (RETURN (SUBLIS ALIST BODY)))
		   ((NOT OPTIONAL-FLAG)
		    (RETURN (CERROR T NIL ':INVALID-MACRO-CALL
				    "Too few arguments for ~S"
				    FN-NAME FORM)))))
	    ((NULL LAMBDA-LIST)
	     (RETURN (CERROR T NIL ':INVALID-MACRO-CALL
			     "Too many arguments for ~S"
			     FN-NAME FORM))))
      ;; All lambda-list keywords aside from &OPTIONAL are erroneous.
      (AND (MEMQ (CAR LAMBDA-LIST) LAMBDA-LIST-KEYWORDS)
	   (RETURN
	     (CONS (CERROR T NIL ':INVALID-MACRO-CALL
			   "Subst-function ~S contains inappropriate keyword ~A"
			   FN-NAME (CAR LAMBDA-LIST))
		   (CDR FORM))))
      ;; Here we have one more arg.  Add it to the alist.
      (PUSH (CONS (COND ((ATOM (CAR LAMBDA-LIST)) (CAR LAMBDA-LIST))
			(T (CAAR LAMBDA-LIST)))
		  (COND (VALS (CAR VALS))
			((ATOM (CAR LAMBDA-LIST)) NIL)
			(T (CADAR LAMBDA-LIST))))
	    ALIST)
      (POP LAMBDA-LIST))))

(DEFPROP :INVALID-MACRO-CALL INVALID-MACRO-CALL-EH-PROCEED EH:PROCEED)
(DEFUN INVALID-MACRO-CALL-EH-PROCEED (IGNORE IGNORE)
  (EH:READ-OBJECT "Form to evaluate and use as replacement macro expansion"))

;; If symbol as a function should be open-coded
;; then return the definition to substitute in.
;; Otherwise, return NIL.
;; A local declaration (OPEN-CODE symbol definition) takes priority.
;; Next comes the value of an OPEN-CODE property.
;; If that is T, the actual function definition is used.
(DEFUN OPEN-CODE-P (SYMBOL)
    (OR (DO ((LDECLS LOCAL-DECLARATIONS (CDR LDECLS)))
	    ((NULL LDECLS) NIL)
	   (AND (EQ (CAAR LDECLS) 'OPEN-CODE)
		(EQ (CADAR LDECLS) SYMBOL)
		(RETURN (CADDAR LDECLS))))
	(LET ((TM (GET SYMBOL 'OPEN-CODE)))
	    (COND ((EQ TM T)
		   (FSYMEVAL SYMBOL))
		  (T TM)))))

;;; Create an area.  Takes keyword argument pairs as follows:
;;;  :NAME - name of area, this is required
;;;  :SIZE - maximum size (default=infinite)
;;;  :REGION-SIZE - size for regions, defaults to :SIZE if specified else medium-size.
;;;  :REPRESENTATION (:LIST, :STRUCTURE, number) - just for the initial region, default=struc
;;;  :GC (:STATIC, :DYNAMIC) - default = dynamic
;;;  :READ-ONLY, :PDL - attributes
;;;  :ROOM - if specified, push this area onto ROOM, so that (ROOM) will list it.
;;;   (more attributes to be added, especially for additional region-space-types)
;;;  :SWAP-RECOMMENDATIONS - swapin quantum size for area minus 1.
;;;  SYS:%%REGION-MAP-BITS - over-ride on map bits
;;;  SYS:%%REGION-SPACE-TYPE - over-ride on space type 
;;;  SYS:%%REGION-SCAVENGE-ENABLE - default 1, 0 disables scavenger
(DEFUN MAKE-AREA (&REST KEYWORDS
		  &AUX (NAME NIL) (SIZE 37777777) (THE-REGION-SIZE NIL)
		       (REPRESENTATION %REGION-REPRESENTATION-TYPE-STRUCTURE)
		       (GC ':DYNAMIC) (READ-ONLY NIL) (PDL NIL)
		       (MAP-BITS NIL) (SPACE-TYPE NIL) (SCAV-ENB NIL) (RM NIL)
		       (SWAP-RECOMMENDATIONS 0)
		       AREA-NUMBER REGION-NUMBER ARG THE-REGION-BITS)
  ;; Process keyword arguments
  (DO L KEYWORDS (CDDR L) (NULL L)
    (SETQ ARG (CADR L))
    (SELECTQ (CAR L)
      (:NAME
        (CHECK-ARG ARG SYMBOLP "a symbol")
	(SETQ NAME ARG))
      (:SIZE (CHECK-ARG ARG (AND (= (%DATA-TYPE ARG) DTP-FIX) (PLUSP ARG))
			    "a positive fixnum")
	     (SETQ SIZE ARG))
      (:REGION-SIZE
        (CHECK-ARG ARG (AND (= (%DATA-TYPE ARG) DTP-FIX) (PLUSP ARG))
		       "a positive fixnum")
	(SETQ THE-REGION-SIZE ARG))
      (:REPRESENTATION
	(CHECK-ARG ARG (OR (NUMBERP ARG) (MEMQ ARG '(:LIST :STRUCTURE)))
		   "a valid representation-type (:LIST, :STRUCTURE, or a number)")
	(SETQ REPRESENTATION
	      (COND ((EQ ARG ':LIST) %REGION-REPRESENTATION-TYPE-LIST)
		    ((EQ ARG ':STRUCTURE) %REGION-REPRESENTATION-TYPE-STRUCTURE)
		    (T ARG))))
      (:GC
	(CHECK-ARG ARG (MEMQ ARG '(:STATIC :DYNAMIC)) "a GC mode (:STATIC or :DYNAMIC)")
	(SETQ GC ARG))
      (:READ-ONLY (SETQ READ-ONLY ARG))
      (:PDL (SETQ PDL ARG))
      (:ROOM (SETQ RM ARG))
      (:SWAP-RECOMMENDATIONS (SETQ SWAP-RECOMMENDATIONS ARG))
      (%%REGION-MAP-BITS (SETQ MAP-BITS ARG))
      (%%REGION-SPACE-TYPE (SETQ SPACE-TYPE ARG))
      (%%REGION-SCAVENGE-ENABLE (SETQ SCAV-ENB ARG))
      (OTHERWISE (FERROR NIL "~S is not a valid keyword" (CAR L)))))
  ;; Perform defaulting and concordance
  (CHECK-ARG NAME (NOT (NULL NAME)) "specified explicitly")
  (AND (NULL THE-REGION-SIZE)
       (SETQ THE-REGION-SIZE (COND ((= SIZE 37777777) 40000)  ;Size unspecified
				   (T SIZE)))) ;Size specified, assume user wants single region
  (AND (NULL SPACE-TYPE)
       (SETQ SPACE-TYPE (IF (EQ GC ':STATIC) %REGION-SPACE-STATIC %REGION-SPACE-NEW)))
  (AND (NULL SCAV-ENB)
       (SETQ SCAV-ENB (SELECT SPACE-TYPE
			;; What happens if the user specifies a random space type?
			((%REGION-SPACE-STATIC %REGION-SPACE-FIXED %REGION-SPACE-COPY) 1)
			(OTHERWISE 0))))
  (AND (NULL MAP-BITS)
       (SETQ MAP-BITS
	     (LDB %%REGION-MAP-BITS
		  ;;Meta bits
		  (%LOGDPB 1 %%REGION-OLDSPACE-META-BIT
		    (%LOGDPB 1 %%REGION-EXTRA-PDL-META-BIT
		      (%LOGDPB REPRESENTATION %%REGION-REPRESENTATION-TYPE
		  ;;Map status code
			(%LOGDPB (COND (PDL %PHT-MAP-STATUS-PDL-BUFFER)
				       (READ-ONLY %PHT-MAP-STATUS-READ-ONLY)
				       (T %PHT-MAP-STATUS-READ-WRITE-FIRST))
			     %%PHT2-MAP-STATUS-CODE
		  ;;Hardware access code
			  (%LOGDPB (COND (PDL 0)
					 (READ-ONLY 2)
					 (T 3))
				   %%PHT2-MAP-ACCESS-CODE 0))))))))
  (SETQ THE-REGION-BITS
	(%LOGDPB MAP-BITS %%REGION-MAP-BITS
		 (%LOGDPB SPACE-TYPE %%REGION-SPACE-TYPE
			  (%LOGDPB SCAV-ENB %%REGION-SCAVENGE-ENABLE
				   (%LOGDPB SWAP-RECOMMENDATIONS
					    %%REGION-SWAPIN-QUANTUM
					    0)))))
  (LET ((INHIBIT-SCHEDULING-FLAG T)  ;Lock the area data-structure
	(INHIBIT-SCAVENGING-FLAG T))
    (AND (MEMQ NAME AREA-LIST)
	 (FERROR NIL "The area ~S already exists" NAME))
    (SETQ AREA-NUMBER (SYSTEM-COMMUNICATION-AREA %SYS-COM-FREE-AREA/#-LIST))
    (AND (ZEROP AREA-NUMBER)
	 (FERROR NIL "Out of area numbers, cannot create ~S" NAME))
    (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-FREE-AREA/#-LIST)
	   (AREA-REGION-LIST AREA-NUMBER))
    ;; Next two lines set up cdr codes correctly and concs onto end of area-list
    ;; Array leader should be right, but store anyway just in case
    (STORE-ARRAY-LEADER AREA-NUMBER #'AREA-NAME 0)
    (ARRAY-PUSH #'AREA-NAME NAME)
    (STORE (AREA-REGION-SIZE AREA-NUMBER) THE-REGION-SIZE)
    (STORE (AREA-MAXIMUM-SIZE AREA-NUMBER) SIZE)
    (STORE (AREA-SWAP-RECOMMENDATIONS AREA-NUMBER) SWAP-RECOMMENDATIONS)
    (SETQ REGION-NUMBER (%MAKE-REGION THE-REGION-BITS THE-REGION-SIZE))
    (STORE (AREA-REGION-LIST AREA-NUMBER) REGION-NUMBER)
    (STORE (REGION-LIST-THREAD REGION-NUMBER) (+ (LSH 1 23.) AREA-NUMBER))
    (AND RM (PUSH NAME ROOM))
    (SET NAME AREA-NUMBER)))		;Assign area as requested, return number

;;; New version of resource package, subsumes system-window facility
;;; Note that WITH-RESOURCE is obsolete because it takes its "arguments"
;;; in the wrong order.  It has been replaced by USING-RESOURCE.

;;; Old form of DEFRESOURCE:
;;;	(DEFRESOURCE [name | (name dont-make-initial-copy)] . creator-body)
;;; New form of DEFRESOURCE:
;;;	(DEFRESOURCE name parameters keyword value keyword value ...)
;;;  Keywords are:
;;;	:CONSTRUCTOR form   (this is required)
;;;		Sees parameters as arguments.
;;;	:FINDER form
;;;		Sees parameters as arguments.
;;;	:MATCHER form
;;;		Sees OBJECT (in current package) and parameters as arguments.
;;;	:CHECKER form
;;;		Sees OBJECT and IN-USE-P (in current package) and parameters as arguments.
;;;	  In the above four options, form may also be a symbol which is a function to call.
;;;	  It gets the resource data structure as its first argument then the specified args.
;;;	:INITIAL-COPIES number  (default 0)
;;;		If this is specified, all parameters must be &optional and
;;;		have suitable defaults.  This is generally a good idea anyway.
;;;		Specifying NIL here is the same as zero.
;;;  If :FINDER is specified, we keep no list of free objects and use :FINDER
;;;  to find a free one by looking through the general environment.
;;;  Otherwise we keep an a-list of objects and whether they are free.
;;;  If :MATCHER is specified, we use it to check them against the parameters.
;;;  Otherwise the a-list also includes the parameter values, which are checked
;;;  with EQUAL (not EQ).
;;;  If :CHECKER is specified, then it gets to pass on each object to decide whether
;;;  or not to reuse it, whether or not it is already marked as in-use.
;;;
;;;  The finder, matcher, and checker are called without-interrupts.
;;;
;;;  Possible features that might be added: ability to keep a free list threaded
;;;  through the objects.  Code to be run when something is deallocated, e.g.
;;;  to deactivate a window.
;;;
;;;  Note: for windows, you typically want to use DEFWINDOW-RESOURCE,
;;;  which supplies the right options to DEFRESOURCE.
;;;
;;; DEFRESOURCE no longer uses the value and function cells of the resource's name.
;;; It puts on a DEFRESOURCE property of the following defstruct.  Note: only the
;;; functions right here are "allowed" to know what is in this structure.

(DEFSTRUCT (RESOURCE :NAMED :CONC-NAME)
  NAME				;Symbol which names it
  PARAMETIZER			;Function which defaults the parameters and returns list
  CONSTRUCTOR			;Constructor function
  FINDER			;Optional finder function
  MATCHER			;Optional matcher function
  CHECKER			;Optional checker function
  (OBJECT-LIST NIL))		;List of elements: (object in-use-p parameters)

(DEFMACRO DEFRESOURCE (NAME PARAMETERS &REST OPTIONS)
  ;; Old format?
  (IF (OR (LISTP NAME) (NULL OPTIONS) (LISTP (CAR OPTIONS)))
      (PROGN (FORMAT ERROR-OUTPUT "~&WARNING: Obsolete form of DEFRESOURCE ~S.~%" NAME)
	     `(DEFRESOURCE ,(IF (LISTP NAME) (CAR NAME) NAME)
			   ()
			   :INITIAL-COPIES ,(IF (AND (LISTP NAME) (CADR NAME)) 0 1)
			   :CONSTRUCTOR (PROGN ,PARAMETERS ,@(COPYLIST OPTIONS))))
      (LET ((CONSTRUCTOR-FORM NIL) (FINDER-FORM NIL) (MATCHER-FORM NIL) (CHECKER-FORM NIL)
	    (CONSTRUCTOR-FUNCTION NIL) (FINDER-FUNCTION NIL) (MATCHER-FUNCTION NIL)
	    (PARAMETIZER-FUNCTION NIL) (CHECKER-FUNCTION NIL) (INITIAL-COPIES 0))
	(OR (LISTP PARAMETERS) (NULL PARAMETERS)
	    (FERROR NIL "~S invalid parameter list" PARAMETERS))
	(LOOP FOR (KEYWORD VALUE) ON OPTIONS BY 'CDDR
	      DO (SELECTQ KEYWORD
		   (:CONSTRUCTOR (SETQ CONSTRUCTOR-FORM VALUE))
		   (:FINDER (SETQ FINDER-FORM VALUE))
		   (:MATCHER (SETQ MATCHER-FORM VALUE))
		   (:CHECKER (SETQ CHECKER-FORM VALUE))
		   (:INITIAL-COPIES
		     (SETQ INITIAL-COPIES
			   (COND ((NULL VALUE) 0)
				 ((NUMBERP VALUE) VALUE)
				 (T (FERROR NIL ":INITIAL-COPIES ~S - number required"
						VALUE)))))
		   (OTHERWISE (FERROR NIL "~S illegal option in DEFRESOURCE" KEYWORD))))
	(OR CONSTRUCTOR-FORM (FERROR NIL "DEFRESOURCE requires the :CONSTRUCTOR option"))
	;;Pick function names.  Note that NIL is SYMBOLP.
	(SETQ CONSTRUCTOR-FUNCTION (IF (SYMBOLP CONSTRUCTOR-FORM) CONSTRUCTOR-FORM
				       `(:PROPERTY ,NAME RESOURCE-CONSTRUCTOR)))
	(SETQ FINDER-FUNCTION (IF (SYMBOLP FINDER-FORM) FINDER-FORM
				  `(:PROPERTY ,NAME RESOURCE-FINDER)))
	(SETQ MATCHER-FUNCTION (IF (SYMBOLP MATCHER-FORM) MATCHER-FORM
				   `(:PROPERTY ,NAME RESOURCE-MATCHER)))
	(SETQ CHECKER-FUNCTION (IF (SYMBOLP CHECKER-FORM) CHECKER-FORM
				   `(:PROPERTY ,NAME RESOURCE-CHECKER)))
	(SETQ PARAMETIZER-FUNCTION (IF (AND PARAMETERS (NOT MATCHER-FORM) (NOT FINDER-FORM))
				       `(:PROPERTY ,NAME RESOURCE-PARAMETIZER)))
	`(PROGN 'COMPILE
	   ,(IF (NOT (SYMBOLP CONSTRUCTOR-FORM))
		`(DEFUN ,CONSTRUCTOR-FUNCTION (IGNORE ,@PARAMETERS)
		   ,CONSTRUCTOR-FORM))
	   ,(IF (NOT (SYMBOLP FINDER-FORM))
		`(DEFUN ,FINDER-FUNCTION (IGNORE ,@PARAMETERS)
		   ,FINDER-FORM))
	   ,(IF (NOT (SYMBOLP MATCHER-FORM))
		`(DEFUN ,MATCHER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
		   ,MATCHER-FORM))
	   ,(IF (NOT (SYMBOLP CHECKER-FORM))
		`(DEFUN ,CHECKER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
					   ,@PARAMETERS)
		   ,CHECKER-FORM))
	   ,(IF PARAMETIZER-FUNCTION
		`(DEFUN ,PARAMETIZER-FUNCTION ,PARAMETERS
		   (LIST ,@(LOOP FOR P IN PARAMETERS
				 UNLESS (MEMQ P LAMBDA-LIST-KEYWORDS)
				  COLLECT (IF (SYMBOLP P) P (CAR P))))))
	   (INITIALIZE-RESOURCE ',NAME ',CONSTRUCTOR-FUNCTION ',FINDER-FUNCTION
				',MATCHER-FUNCTION ',CHECKER-FUNCTION
				',PARAMETIZER-FUNCTION ',INITIAL-COPIES)))))

(DEFPROP DEFRESOURCE "Resource" DEFINITION-TYPE-NAME)

(DEFUN INITIALIZE-RESOURCE (NAME CONSTRUCTOR-FUNCTION FINDER-FUNCTION MATCHER-FUNCTION
			    CHECKER-FUNCTION PARAMETIZER-FUNCTION INITIAL-COPIES)
  (OR (SYMBOLP CONSTRUCTOR-FUNCTION)
      (SETQ CONSTRUCTOR-FUNCTION (GET (SECOND CONSTRUCTOR-FUNCTION)
				      (THIRD CONSTRUCTOR-FUNCTION))))
  (OR (SYMBOLP FINDER-FUNCTION)
      (SETQ FINDER-FUNCTION (GET (SECOND FINDER-FUNCTION) (THIRD FINDER-FUNCTION))))
  (OR (SYMBOLP MATCHER-FUNCTION)
      (SETQ MATCHER-FUNCTION (GET (SECOND MATCHER-FUNCTION) (THIRD MATCHER-FUNCTION))))
  (OR (SYMBOLP CHECKER-FUNCTION)
      (SETQ CHECKER-FUNCTION (GET (SECOND CHECKER-FUNCTION) (THIRD CHECKER-FUNCTION))))
  (OR (SYMBOLP PARAMETIZER-FUNCTION)
      (SETQ PARAMETIZER-FUNCTION (GET (SECOND PARAMETIZER-FUNCTION)
				      (THIRD PARAMETIZER-FUNCTION))))
  (AND (RECORD-SOURCE-FILE-NAME NAME 'DEFRESOURCE)
       (LET ((RESOURCE (MAKE-RESOURCE NAME NAME
				      PARAMETIZER PARAMETIZER-FUNCTION
				      CONSTRUCTOR CONSTRUCTOR-FUNCTION
				      FINDER FINDER-FUNCTION
				      MATCHER MATCHER-FUNCTION
				      CHECKER CHECKER-FUNCTION))
	     (OLD-RESOURCE (GET NAME 'DEFRESOURCE)))
	 (AND OLD-RESOURCE		;Save any old objects when reloading a DEFRESOURCE
	      (SETF (RESOURCE-OBJECT-LIST RESOURCE) (RESOURCE-OBJECT-LIST OLD-RESOURCE)))
	 (PUTPROP NAME RESOURCE 'DEFRESOURCE)
	 (LOOP FOR OBJECT IN (LOOP REPEAT INITIAL-COPIES COLLECT (ALLOCATE-RESOURCE NAME))
	       DO (DEALLOCATE-RESOURCE NAME OBJECT))))
  NAME)

(DEFUN CLEAR-RESOURCE (RESOURCE-NAME &AUX RESOURCE)
  (CHECK-ARG RESOURCE-NAME (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))
	     "the name of a resource")
  (SETF (RESOURCE-OBJECT-LIST RESOURCE) NIL))

(DEFUN ALLOCATE-RESOURCE (RESOURCE-NAME &REST PARAMETERS &AUX RESOURCE (PARAMS NIL) TEM)
  (CHECK-ARG RESOURCE-NAME (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))
	     "the name of a resource")
  (AND (SETQ TEM (RESOURCE-PARAMETIZER RESOURCE))
       (SETQ PARAMS (APPLY TEM PARAMETERS)))
  (WITHOUT-INTERRUPTS
    (COND ((SETQ TEM (RESOURCE-FINDER RESOURCE))
	   (LEXPR-FUNCALL TEM RESOURCE PARAMETERS))
	  ((LOOP WITH CHECKER = (RESOURCE-CHECKER RESOURCE)
		 WITH MATCHER = (RESOURCE-MATCHER RESOURCE)
		 FOR ITEM IN (RESOURCE-OBJECT-LIST RESOURCE)
		 AS OBJ = (CAR ITEM) AND IN-USE-P = (CADR ITEM)
		 WHEN (AND (IF CHECKER
			       (LEXPR-FUNCALL CHECKER RESOURCE OBJ IN-USE-P PARAMETERS)
			       (NOT IN-USE-P))
			   (OR (NULL PARAMS) (EQUAL (CADDR ITEM) PARAMS))
			   (OR (NOT MATCHER) (LEXPR-FUNCALL MATCHER RESOURCE OBJ PARAMETERS)))
		   DO (SETF (CADR ITEM) T)
		      (RETURN OBJ)))
	  (T (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	     (SETQ TEM (LEXPR-FUNCALL (RESOURCE-CONSTRUCTOR RESOURCE) RESOURCE PARAMETERS))
	     (SETQ TEM (IF (NULL PARAMS) (LIST TEM T) (LIST TEM T PARAMS)))
	     (SETQ INHIBIT-SCHEDULING-FLAG T)
	     (PUSH TEM (RESOURCE-OBJECT-LIST RESOURCE))
	     (CAR TEM)))))

(DEFUN DEALLOCATE-RESOURCE (RESOURCE-NAME OBJECT &AUX RESOURCE TEM)
  (CHECK-ARG RESOURCE-NAME (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))
	     "the name of a resource")
  (COND ((NOT (RESOURCE-FINDER RESOURCE))
	 (OR (SETQ TEM (ASSQ OBJECT (RESOURCE-OBJECT-LIST RESOURCE)))
	     (FERROR NIL "~S is not an object from the ~S resource" OBJECT RESOURCE-NAME))
	 (SETF (CADR TEM) NIL))))	;Note, this doesn't need any locking

(DEFMACRO USING-RESOURCE ((VAR NAME . PARAMETERS) &BODY BODY)
  `(LET ((,VAR NIL))
     (UNWIND-PROTECT
       (PROGN
	 (SETQ ,VAR (ALLOCATE-RESOURCE ',NAME . ,PARAMETERS))
	   . ,BODY)
       (AND ,VAR (DEALLOCATE-RESOURCE ',NAME ,VAR)))))

;For compatibility with old programs
(DEFPROP WITH-RESOURCE COMPILER:OBSOLETE COMPILER:STYLE-CHECKER)
(DEFMACRO WITH-RESOURCE ((NAME VAR) &BODY BODY)
  `(USING-RESOURCE (,VAR ,NAME) . ,BODY))
