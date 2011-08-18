;  -*-Package:COMPILER; Mode:LISP-*-
;This file contains the source-level optimizers of the Lisp machine compiler.

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;     "This is insane.  What we clearly want to do is not completely
;;      clear, and is rooted in NCOMPLR."   -- BSG/Dissociated Press.

;;; Convert DOs into PROGs.

(ADD-OPTIMIZER DO DOEXPANDER)
(ADD-OPTIMIZER DO-NAMED DOEXPANDER)

(DEFUN DOEXPANDER (X)
  (LET ((PROGNAME) (PROGREST))
    (SETQ PROGREST
      (PROG (DOSPECS ENDTEST ENDVALS TAG1 TAG3 PVARS STEPDVARS ONCE)
            (COND ((EQ (CAR X) 'DO-NAMED)
                   (SETQ PROGNAME (CADR X))
                   (SETQ X (CDDR X)))
                  (T (SETQ X (CDR X))))			;Get rid of "DO".
            (COND ((AND (CAR X) (ATOM (CAR X)))
                   (SETQ  DOSPECS `((,(CAR X) ,(CADR X) ,(CADDR X)))
                          ENDTEST (CAR (SETQ X (CDDDR X)))
                          ENDVALS NIL))
                  (T (SETQ DOSPECS (CAR X))
                     (SETQ X (CDR X))
                     (COND ((CAR X)
                            (SETQ ENDTEST (CAAR X)
                                  ENDVALS (AND (OR (CDDAR X)
                                                   (CADAR X))
                                               (CDAR X))))
                           (T (SETQ ONCE T)))))
            (SETQ X (CDR X))
            (SETQ DOSPECS (REVERSE DOSPECS)); Do NOT use NREVERSE, or you will destroy
					    ; every macro definition in sight!! -DLW
            ;; DOVARS has new-style list of DO variable specs,
            ;; ENDTEST has the end test form,
            ;; ENDVALS has the list of forms to be evaluated when the end test succeeds,
            ;; ONCE is T if this is a DO-once as in (DO ((VAR)) () ...),
            ;; X has the body.
            ;; Now process the variable specs.
            (DO X DOSPECS (CDR X) (NULL X)
                (COND ((ATOM (CAR X))
		       (PUSH (CAR X) PVARS))
                      ((OR (> (LENGTH (CAR X)) 3) (NOT (ATOM (CAAR X))))
		       (BARF (CAR X) '|malformatted DO-variable specification|
			     'DATA))
		      (T (PUSH `(,(CAAR X) ,(CADAR X)) PVARS)
                         (AND (CDDAR X)
                              (PUSH `(,(CAAR X) ,(CADDAR X)) STEPDVARS)))))
            (COND (ONCE
                   (AND STEPDVARS (BARF STEPDVARS '|Stepped variables in once-through DO| 'WARN))
                   (RETURN `(,PVARS . ,X))))
	    ;; Turn STEPDVARS into a PSETQ form to step the vars,
	    ;; or into NIL if there are no vars to be stepped.
            (SETQ STEPDVARS (APPLY 'NCONC STEPDVARS))
	    (AND STEPDVARS (SETQ STEPDVARS (CONS 'PSETQ STEPDVARS)))
            (SETQ TAG3 (GENSYM))
            (SETQ TAG1 (GENSYM))
            (SETQ ENDTEST (OPTIMIZE ENDTEST NIL))
            (COND ((NULL ENDTEST)
		   (OPTIMIZE ENDTEST T)	;Get any style warnings we were supposed to get,
					;since ENDTEST won't actually be compiled.
                   (AND ENDVALS
                        (BARF ENDVALS
                              '|DO end-test is NIL, but forms are to be evalled on exit|
                              'WARN))
                   (RETURN `(,PVARS ,TAG1
                             ,@X
			     ,STEPDVARS
                             (GO ,TAG1)))))
            (COND ((EQ PROGNAME T)
                   (SETQ ENDVALS `(INHIBIT-STYLE-WARNINGS
                                     (RETURN-FROM-T (PROGN NIL . ,ENDVALS)))))
                  (T (SETQ ENDVALS `(RETURN (PROGN NIL . ,ENDVALS)))))
	    (RETURN `(,PVARS
		      (GO ,TAG3)
		      ,TAG1
		      ,@X	;body
		      ,STEPDVARS
		      ,TAG3
		      (OR ,ENDTEST (GO ,TAG1))
		      ,ENDVALS))))
    (AND PROGNAME (SETQ PROGREST (CONS PROGNAME PROGREST)))
    (CONS 'PPROG PROGREST)))


(ADD-OPTIMIZER MAP MAPEXPAND)
(ADD-OPTIMIZER MAPC MAPEXPAND)
(ADD-OPTIMIZER MAPCAR MAPEXPAND)
(ADD-OPTIMIZER MAPLIST MAPEXPAND)
(ADD-OPTIMIZER MAPCAN MAPEXPAND)
(ADD-OPTIMIZER MAPCON MAPEXPAND)

(DEFUN MAPEXPAND (FORM)
  (COND ((NULL (CDDR FORM)) FORM)  ;Don't bomb out if no args for the function to map.
	(T
    (LET ((FN (OPTIMIZE (CADR FORM) NIL))
	  CALL-FN
	  (TAKE-CARS (MEMQ (CAR FORM) '(MAPC MAPCAR MAPCAN)))
	  TEM)
         (COND ((NOT OPEN-CODE-MAP-SWITCH) FORM)
               ;; Expand maps only if specified function is a quoted LAMBDA or a SUBST,
	       ;; or some arg is a call to CIRCULAR-LIST and we are mapping on cars.
	       ((NOT (OR (AND (NOT (ATOM FN))
			      (MEMQ (CAR FN) '(QUOTE FUNCTION))
			      (NOT (ATOM (CADR FN))))
			 (AND (NOT (ATOM FN))
			      (EQ (CAR FN) 'FUNCTION)
			      (NOT (ATOM (SETQ TEM (DECLARED-DEFINITION (CADR FN)))))
			      (MEMQ (CAR TEM) '(SUBST NAMED-SUBST MACRO)))
			 (AND TAKE-CARS
			      (SOME (CDDR FORM)
				    (FUNCTION (LAMBDA (X)
						(AND (NOT (ATOM X))
						     (NULL (CDDR X))
						     (EQ (CAR X) 'CIRCULAR-LIST))))))))
		FORM)
	       (T (COND ((AND (NOT (ATOM FN)) (MEMQ (CAR FN) '(QUOTE FUNCTION)))
			 (SETQ CALL-FN (LIST (CADR FN))))
			(T (SETQ CALL-FN (LIST 'FUNCALL FN))))
		  ;; VARNMS gets a list of gensymmed variables to use to hold
		  ;; the tails of the lists we are mapping down.
		  (LET ((VARNMS)(DOCLAUSES) (ENDTEST) (CARS-OR-TAILS) (TEM))
		       ;; DOCLAUSES looks like ((G0001 expression (CDR G0001)) ...)
		       ;;  repeated for each variable.
		       ;; ENDTEST is (OR (NULL G0001) (NULL G0002) ...)
		       ;; CARS-OR-TAILS is what to pass to the specified function:
		       ;;  either (G0001 G0002 ...) or ((CAR G0001) (CAR G0002) ...)
		       (SETQ VARNMS (DO ((L (CDDR FORM) (CDR L)) (OUTPUT) )
					((NULL L) OUTPUT)
				      (PUSH (GENSYM) OUTPUT)))
		       (SETQ DOCLAUSES
			     (MAPCAR '(LAMBDA (V L)
				       (COND ((AND TAKE-CARS (NOT (ATOM L))
						   (EQ (CAR L) 'CIRCULAR-LIST)
						   (NULL (CDDR L)))
					      `(,V ,(CADR L)))
					     (T `(,V ,L (CDR ,V)))))
				     VARNMS (CDDR FORM)))
		       (SETQ ENDTEST
			     (CONS 'OR (MAPCAN '(LAMBDA (VL)
						 (AND (CDDR VL) `((NULL ,(CAR VL)))))
					       DOCLAUSES)))
		       (SETQ CARS-OR-TAILS
			     (COND (TAKE-CARS
				    (MAPCAR '(LAMBDA (DC)
					      (COND ((CDDR DC) `(CAR ,(CAR DC)))
						    (T (CAR DC))))
					    DOCLAUSES))
				   (T VARNMS)))
		       (COND ((MEMQ (CAR FORM) '(MAP MAPC))	;NO RESULT
			      (SETQ TEM `(INHIBIT-STYLE-WARNINGS
                                            (DO-NAMED T ,DOCLAUSES
						   (,ENDTEST)
						   (,@CALL-FN . ,CARS-OR-TAILS))))
			      ;; Special hack for MAP or MAPC for value:
			      ;; Bind an extra local to 1st list and return that.
			      (COND (P1VALUE
                                     `(LET ((MAP-RESULT ,(PROG1 (CADAR DOCLAUSES)
								(RPLACA (CDAR DOCLAUSES)
									'MAP-RESULT))))
                                         ,TEM
                                         MAP-RESULT))
				    (T TEM)))
			     ((MEMQ (CAR FORM) '(MAPCAR MAPLIST))	;CONS UP RESULT
			      `(LET ((MAP-RESULT))
                                  (INHIBIT-STYLE-WARNINGS
				    (DO-NAMED T ((MAP-TEMP (INHIBIT-STYLE-WARNINGS
                                                            (VALUE-CELL-LOCATION 'MAP-RESULT)))
                                                 . ,DOCLAUSES)
					      (,ENDTEST)
					   (RPLACD MAP-TEMP
						   (SETQ MAP-TEMP
							 (NCONS (,@CALL-FN . ,CARS-OR-TAILS))))))
				    MAP-RESULT))
			     (T
			      ;; MAPCAN and MAPCON:  NCONC the result.
			      `(INHIBIT-STYLE-WARNINGS
                                 (DO-NAMED T (,@DOCLAUSES (MAP-TEM) (MAP-RESULT))
				    (,ENDTEST MAP-RESULT)
				    (SETQ MAP-TEM (NCONC MAP-TEM (,@CALL-FN . ,CARS-OR-TAILS)))
				    (OR MAP-RESULT (SETQ MAP-RESULT MAP-TEM))
				    (SETQ MAP-TEM (LAST MAP-TEM))))))
			    )))))))


(ADD-OPTIMIZER SUBSET SUBSET-EXPAND)
(ADD-OPTIMIZER SUBSET-NOT SUBSET-EXPAND)

(DEFUN SUBSET-EXPAND (FORM)
  (LET ((FN (OPTIMIZE (CADR FORM) NIL))
	PREDARGS DOCLAUSES TEM)
    (COND ((NOT OPEN-CODE-MAP-SWITCH) FORM)
	  ;; Expand only if specified function is a quoted LAMBDA or a SUBST,
	  ((NOT (OR (AND (NOT (ATOM FN))
			 (MEMQ (CAR FN) '(QUOTE FUNCTION))
			 (NOT (ATOM (CADR FN))))
		    (AND (NOT (ATOM FN))
			 (EQ (CAR FN) 'FUNCTION)
			 (NOT (ATOM (SETQ TEM (DECLARED-DEFINITION (CADR FN)))))
			 (MEMQ (CAR TEM) '(SUBST NAMED-SUBST MACRO)))))
	   FORM)
	  (T (SETQ FN (CADR FN)) ;Strip off the QUOTE or FUNCTION.
	     ;; Generate N local variable names.
	     (DO ((L (CDDR FORM) (CDR L)) (I 0 (1+ I)))
		 ((NULL L))
	       (LET ((V (INTERN (FORMAT NIL "MAP-LOCAL-~D" I))))
		 (PUSH `(,V ,(CAR L) (CDR ,V)) DOCLAUSES)
		 (PUSH `(CAR ,V) PREDARGS)))	       
	     (SETQ DOCLAUSES (NREVERSE DOCLAUSES)
		   PREDARGS (NREVERSE PREDARGS))
	     `(LET (MAP-RESULT)
		(INHIBIT-STYLE-WARNINGS
		  (DO-NAMED T
		     ((MAP-TEMP (INHIBIT-STYLE-WARNINGS (VALUE-CELL-LOCATION 'MAP-RESULT)))
		      . ,DOCLAUSES)
		     ((NULL ,(CAAR DOCLAUSES)))	;Stop when first local variable runs out
		    (,(COND ((EQ (CAR FORM) 'SUBSET) 'AND) (T 'OR))
		      (,FN . ,PREDARGS)
		      (RPLACD MAP-TEMP (SETQ MAP-TEMP (NCONS ,(CAR PREDARGS)))))))
		MAP-RESULT)))))

;Express multi-argument arithmetic functions in terms of two-argument versions.
(MAPC (FUNCTION (LAMBDA (FN) (PUTPROP FN '(ARITHEXP) 'OPTIMIZERS)))
      '(+ * - // LOGAND LOGIOR LOGXOR MIN MAX PLUS TIMES DIFFERENCE QUOTIENT
        +$ *$ -$ //$))


(DEFPROP + *PLUS TWO-ARGUMENT-FUNCTION)
(DEFPROP +$ *PLUS TWO-ARGUMENT-FUNCTION)
(DEFPROP * *TIMES TWO-ARGUMENT-FUNCTION)
(DEFPROP *$ *TIMES TWO-ARGUMENT-FUNCTION)
(DEFPROP - *DIF TWO-ARGUMENT-FUNCTION)
(DEFPROP -$ *DIF TWO-ARGUMENT-FUNCTION)
(DEFPROP // *QUO TWO-ARGUMENT-FUNCTION)
(DEFPROP //$ *QUO TWO-ARGUMENT-FUNCTION)
(DEFPROP LOGIOR *LOGIOR TWO-ARGUMENT-FUNCTION)
(DEFPROP LOGAND *LOGAND TWO-ARGUMENT-FUNCTION)
(DEFPROP LOGXOR *LOGXOR TWO-ARGUMENT-FUNCTION)
(DEFPROP MIN *MIN TWO-ARGUMENT-FUNCTION)
(DEFPROP MAX *MAX TWO-ARGUMENT-FUNCTION)
(DEFPROP PLUS *PLUS TWO-ARGUMENT-FUNCTION)
(DEFPROP TIMES *TIMES TWO-ARGUMENT-FUNCTION)
(DEFPROP DIFFERENCE *DIF TWO-ARGUMENT-FUNCTION)
(DEFPROP QUOTIENT *QUO TWO-ARGUMENT-FUNCTION)

(DEFUN ARITHEXP (X)
       (PROG (L OP)
	     (SETQ L (LENGTH (CDR X)))
	     (COND ((NULL (SETQ OP (GET (CAR X) 'TWO-ARGUMENT-FUNCTION)))
		    (BARF X 'BAD-OP-ARITHEXP 'BARF))
		   ((= 0 L)
		    (OR (SETQ L (ASSQ OP '((*PLUS . 0) (*DIF . 0) (*TIMES . 1) (*QUO . 1))))
			(BARF X '|Illegal with no arguments| 'WARN))
		    (RETURN (CDR L)))
		   ((= L 1)
		    (RETURN (COND ((MEMQ (CAR X) '(- -$))
				   (LIST 'MINUS (CADR X)))
                                  ((MEMQ (CAR X) '(// //$))
				   (LIST '*QUO '1 (CADR X)))
				  (T (CADR X)))))
		   ((= L 2) (RETURN (CONS OP (CDR X))))
		   (T (RETURN (CONS OP
				    (CONS (CONS (CAR X)
						(BUTLAST (CDR X)))
					  (LAST X))))))))

(ADD-OPTIMIZER BOOLE BOOLE-EXPAND)
(DEFUN BOOLE-EXPAND (X)
  (PROG (L OP INST)
	(SETQ L (LENGTH (CDR X)))
	(SETQ OP (CADR X))
	(COND ((= L 2) (RETURN (CADDR X)))
	      ((AND (NUMBERP OP)
		    (SETQ INST (ASSQ OP '((1 . LOGAND)
				 (6 . LOGXOR) (7 . LOGIOR)))))
		(RETURN (CONS (CDR INST) (CDDR X))))
	      ((= L 3) (RETURN (CONS '*BOOLE (CDR X))))
	      (T (RETURN (LIST '*BOOLE (CADR X)
			       (CONS 'BOOLE (BUTLAST (CDR X)))
			       (CAR (LAST X))))))))


(ADD-OPTIMIZER > >-OPTIMIZER)
(DEFUN >-OPTIMIZER (FORM)
  (CONS 'GREATERP (CDR FORM)))

(ADD-OPTIMIZER < <-OPTIMIZER)
(DEFUN <-OPTIMIZER (FORM)
  (CONS 'LESSP (CDR FORM)))


(ADD-OPTIMIZER GREATERP GREATERP-OPTIMIZER)
(DEFUN GREATERP-OPTIMIZER (FORM)
  (LET* ((ARGS (CDR FORM))
	 (N-ARGS (LENGTH ARGS)))
    (COND ((< N-ARGS 2)
	   (BARF FORM '|Too few arguments| 'WARN)
	   ''T)
	  ((= N-ARGS 2)
	   (IF (MEMBER (SECOND ARGS) '(0 '0))
	       `(PLUSP ,(FIRST ARGS))
	       `(INTERNAL-> . ,ARGS)))
	  ((EVERY (CDR ARGS) #'TRIVIAL-FORM-P)
	   (CONS 'AND (LOOP FOR ARG IN (CDR ARGS)
			    AND FOR LAST-ARG FIRST (CAR ARGS) THEN ARG
			    COLLECT (LIST 'INTERNAL-> LAST-ARG ARG))))
	  (T FORM))))

(ADD-OPTIMIZER LESSP LESSP-OPTIMIZER)
(DEFUN LESSP-OPTIMIZER (FORM)
  (LET* ((ARGS (CDR FORM))
	 (N-ARGS (LENGTH ARGS)))
    (COND ((< N-ARGS 2)
	   (BARF FORM '|Too few arguments| 'WARN)
	   ''T)
	  ((= N-ARGS 2)
	   (IF (MEMBER (SECOND ARGS) '(0 '0))
	       `(MINUSP ,(FIRST ARGS))
	       `(INTERNAL-< . ,ARGS)))
	  ((EVERY (CDR ARGS) #'TRIVIAL-FORM-P)
	   (CONS 'AND (LOOP FOR ARG IN (CDR ARGS)
			    AND FOR LAST-ARG FIRST (CAR ARGS) THEN ARG
			    COLLECT (LIST 'INTERNAL-< LAST-ARG ARG))))
	  (T FORM))))


(ADD-OPTIMIZER >= >=-OPTIMIZER)
(ADD-OPTIMIZER  >=-OPTIMIZER)
(DEFUN >=-OPTIMIZER (FORM)
  (LET* ((ARGS (CDR FORM))
	 (N-ARGS (LENGTH ARGS)))
    (COND ((< N-ARGS 2)
	   (BARF FORM '|Too few arguments| 'WARN)
	   ''T)
	  ((= N-ARGS 2)
	   (IF (MEMBER (SECOND ARGS) '(0 '0))
	       `(NOT (MINUSP ,(FIRST ARGS)))
	       `(NOT (INTERNAL-< . ,ARGS))))
	  ((EVERY (CDR ARGS) #'TRIVIAL-FORM-P)
	   (CONS 'AND (LOOP FOR ARG IN (CDR ARGS)
			    AND FOR LAST-ARG FIRST (CAR ARGS) THEN ARG
			    COLLECT `(NOT (INTERNAL-< ,LAST-ARG ,ARG)))))
	  (T FORM))))

(ADD-OPTIMIZER <= <=-OPTIMIZER)
(ADD-OPTIMIZER  <=-OPTIMIZER)
(DEFUN <=-OPTIMIZER (FORM)
  (LET* ((ARGS (CDR FORM))
	 (N-ARGS (LENGTH ARGS)))
    (COND ((< N-ARGS 2)
	   (BARF FORM '|Too few arguments| 'WARN)
	   ''T)
	  ((= N-ARGS 2)
	   (IF (MEMBER (SECOND ARGS) '(0 '0))
	       `(NOT (PLUSP ,(FIRST ARGS)))
	       `(NOT (INTERNAL-> . ,ARGS))))
	  ((EVERY (CDR ARGS) #'TRIVIAL-FORM-P)
	   (CONS 'AND (LOOP FOR ARG IN (CDR ARGS)
			    AND FOR LAST-ARG FIRST (CAR ARGS) THEN ARG
			    COLLECT `(NOT (INTERNAL-> ,LAST-ARG ,ARG)))))
	  (T FORM))))

(DEFUN TRIVIAL-FORM-P (X)
  (OR (NUMBERP X)
      (SYMBOLP X)
      (AND (NOT (ATOM X))
	   (EQ (CAR X) 'QUOTE))))

(ADD-OPTIMIZER ADD1 ADD1-FIX)
(ADD-OPTIMIZER SUB1 ADD1-FIX)
(ADD-OPTIMIZER 1+$ ADD1-FIX)
(ADD-OPTIMIZER 1-$ ADD1-FIX)
(DEFUN ADD1-FIX (FORM)
  (CONS (CDR (ASSQ (CAR FORM) '((ADD1 . 1+)  (1+$ . 1+) (SUB1 . 1-) (1-$ . 1-))))
	(CDR FORM)))

(ADD-OPTIMIZER \\ CONVERT-\\)

(DEFUN CONVERT-\\ (FORM)
  (LOOP FOR ARG-FORM IN (CDDDR FORM)
	WITH ANSWER = `(SYS:INTERNAL-\\ ,(SECOND FORM) ,(THIRD FORM))
	DO (SETQ ANSWER `(SYS:INTERNAL-\\ ,ANSWER ,ARG-FORM))
	FINALLY (RETURN ANSWER)))

(ADD-OPTIMIZER GCD GCD-OPTIMIZER \\)

(DEFUN GCD-OPTIMIZER (FORM)
  (CONS '\\ (CDR FORM)))

(ADD-OPTIMIZER NULL NULL-NOT)
(DEFUN NULL-NOT (FORM)
    `(NOT . ,(CDR FORM)))

;Optimize (FUNCALL (FUNCTION (LAMBDA ...)) ...) into ((LAMBDA ...) ...).
;Do not optimize (FUNCALL (FUNCTION FOO) ...) because in that case
;it would not improve anything and would lose if FOO quotes some args.
(ADD-OPTIMIZER FUNCALL FUNCALL-FUNCTION)
(DEFUN FUNCALL-FUNCTION (FORM)
    (LET ((FNFORM (OPTIMIZE (CADR FORM) NIL)))
	 (COND ((AND (NOT (ATOM FNFORM))
		     (MEMQ (CAR FNFORM) '(FUNCTION QUOTE))
		     (NOT (ATOM (CADR FNFORM)))
		     (FUNCTIONP (CADR FNFORM)))
		(CONS (CADR FNFORM) (CDDR FORM)))
	       (T FORM))))

(ADD-OPTIMIZER LIST LIST-NO-ARGS)
(DEFUN LIST-NO-ARGS (FORM)
  (COND ((EQUAL FORM '(LIST))
	 'NIL)
	(T FORM)))

(ADD-OPTIMIZER APPLY APPLY-TO-LIST)
(DEFUN APPLY-TO-LIST (FORM)
    (LET ((ARGFORM (OPTIMIZE (CADDR FORM) NIL)))
	 (COND ((ATOM ARGFORM)
		FORM)
	       ((EQ (CAR ARGFORM) 'LIST)
		`(FUNCALL ,(CADR FORM) . ,(CDR ARGFORM)))
	       ((EQ (CAR ARGFORM) 'LIST*)
		`(LEXPR-FUNCALL ,(CADR FORM) . ,(CDR ARGFORM)))
	       ((EQ (CAR ARGFORM) 'QUOTE)
		`(FUNCALL ,(CADR FORM)
			  . ,(MAPCAR (FUNCTION (LAMBDA (X) (LIST 'QUOTE X))) (CADR ARGFORM))))
	       (T FORM))))

(ADD-OPTIMIZER AND AND-OR-NO-OP)
(ADD-OPTIMIZER OR AND-OR-NO-OP)
(DEFUN AND-OR-NO-OP (FORM)
    (COND ((NULL (CDR FORM))
	   (COND ((EQ (CAR FORM) 'AND)
		  ''T)
		 (T ''NIL)))
	  ((NULL (CDDR FORM))
	   (CADR FORM))
	  (T FORM)))

(ADD-OPTIMIZER PROGN 1-ARG-NO-OP)
(DEFUN 1-ARG-NO-OP (FORM)
       (COND ((CDDR FORM) FORM)
             (T (CADR FORM))))

(ADD-OPTIMIZER PROG2 PROG2-NO-OP)
(DEFUN PROG2-NO-OP (FORM)
    (COND ((OR (CADR FORM) (CDDDR FORM)) FORM)
          (T (CADDR FORM))))

;Here are a bunch of random compile-time expansions for built-in functions.

(ADD-OPTIMIZER STORE STORE-CONVERT)
(DEFUN STORE-CONVERT (X)
  (COND ((AND (NOT (ATOM (CADR X)))	;(STORE (ARRAYCALL ...) ...)
	      (EQ (CAADR X) 'ARRAYCALL))
	 `(ASET ,(CADDR X) ,(CADDR (CADR X)) . ,(CDDDR (CADR X))))
	(T `(XSTORE ,(CADDR X) ,(CADR X)))))

;Turn EQUAL into EQ when that is safe.
;EQUAL can never be turned into = alone because = signals an error if either
;arg is not a number, whereas EQUAL does not.  However, (EQUAL <fixnum> xxx)
;can be turned into EQ since EQ "works" for fixnums.
;Also EQUALwith one of the arguments a number turns into
;(AND (NUMBERP <form>) (= <number> <form>))
(ADD-OPTIMIZER EQUAL EQUAL-EQ-=)
(DEFUN EQUAL-EQ-= (FORM)
  (COND ((OR (POINTER-IDENTITY-P (CADR FORM))
	     (POINTER-IDENTITY-P (CADDR FORM)))
	 (CONS 'EQ (CDR FORM)))
	((AND (NUMBERP (CADR FORM)) (ATOM (CADDR FORM)))
	 (EQUAL-= (CADR FORM) (CADDR FORM)))
	((AND (NUMBERP (CADDR FORM)) (ATOM (CADR FORM)))
	 (EQUAL-= (CADDR FORM) (CADR FORM)))
	(T FORM)))

(DEFUN EQUAL-= (NUMBER ATOM)
       `(AND (NUMBERP ,ATOM) (= ,NUMBER ,ATOM)))

(IF-FOR-LISPM
(DEFUN POINTER-IDENTITY-P (QUAN)
   (OR (EQ (TYPEP QUAN) 'FIXNUM)
       (AND (NOT (ATOM QUAN))
	    (EQ (CAR QUAN) 'QUOTE)
	    (MEMQ (TYPEP (CADR QUAN))
		  '(FIXNUM SYMBOL))))) )

(IF-FOR-MACLISP 	;ON MACLISP, MUST VERIFY THAT IT WILL REALLY BE A FIXNUM
			; ON REAL MACHINE, IE FITS IN 24 BITS.
(DEFUN POINTER-IDENTITY-P (QUAN)
   (OR (AND (EQ (TYPEP QUAN) 'FIXNUM)
	    (< (ABS QUAN) 37777777))
       (AND (NOT (ATOM QUAN))
	    (EQ (CAR QUAN) 'QUOTE)
	    (OR (EQ (TYPEP (CADR QUAN)) 'SYMBOL)
		(AND (EQ (TYPEP (CADR QUAN)) 'FIXNUM)
		     (< (ABS QUAN) 37777777)))))) )

;Turn (EQ FOO NIL) into (NOT FOO).
(DEFPROP EQ (EQ-NIL EQ-TYPEP) OPTIMIZERS)
(DEFUN EQ-NIL (FORM)
    (COND ((NULL (CDDR FORM)) FORM)  ;0 or 1 arg => let it get the error.
	  ((MEMBER (CADR FORM) '(NIL 'NIL))
           `(NOT ,(CADDR FORM)))
          ((MEMBER (CADDR FORM) '(NIL 'NIL))
           `(NOT ,(CADR FORM)))
          (T FORM)))

;Optimize (EQ (TYPEP ...) 'SYMBOL), etc.
(DEFUN EQ-TYPEP (FORM)
  (PROG NIL     
      (AND (NOT (ATOM (CADR FORM)))
	   (NOT (ATOM (CADDR FORM)))
	   (COND ((AND (MEMQ (CAADR FORM) '(TYPEP DATA-TYPE))
		       (NULL (CDDADR FORM))  ;Check that TYPEP has only one arg!
		       (EQ (CAADDR FORM) 'QUOTE))
		  (RETURN (EQ-TYPEP-1 (CADADR FORM) (CADR (CADDR FORM)) FORM)))
		 ((AND (EQ (CAADR FORM) 'QUOTE)
		       (MEMQ (CAADDR FORM) '(TYPEP DATA-TYPE))
		       (NULL (CDDR (CADDR FORM))))
		  (RETURN (EQ-TYPEP-1 (CADR (CADDR FORM)) (CADADR FORM) FORM)))))
      (RETURN FORM)))

(DEFUN EQ-TYPEP-1 (FORM TYPE TOPFORM)
    (PROG (PRED)
	  (SETQ PRED (OR (AND (MEMQ TYPE '(STRING :SYMBOL LIST))
			      (GET TYPE 'TYPEP))
			 (CAR (RASSOC TYPE TYPEP-ALIST))))
	  (COND ((NULL PRED) (RETURN TOPFORM))
		((NUMBERP PRED) (RETURN `(= (%DATA-TYPE ,FORM) ,PRED)))
		((SYMBOLP PRED) (RETURN `(,PRED ,FORM)))
		(T (RETURN TOPFORM)))))

(ADD-OPTIMIZER TYPEP TYPEP-TWO-ARGS SI:TYPEP-FLAVOR SI:TYPEP-STRUCTURE
				    SUBINSTANCE-OF-CLASS-SYMBOL-P)
(DEFUN TYPEP-TWO-ARGS (FORM)
  (COND ((AND (CADDR FORM)
	      (QUOTEP (CADDR FORM)))
	 (LET ((TYPE (CADR (CADDR FORM))) PRED)
	   (COND ((SYMBOLP TYPE)
		  (SETQ PRED (OR (GET TYPE 'TYPEP) (CAR (RASSQ TYPE TYPEP-ALIST))))
		  (COND ((NUMBERP PRED)
			 `(= (%DATA-TYPE ,(CADR FORM)) ,PRED))
			(PRED `(,PRED ,(CADR FORM)))
			((GET TYPE 'SI:FLAVOR)
			 `(SI:TYPEP-FLAVOR . ,(CDR FORM)))
			((GET TYPE 'SI:DEFSTRUCT-DESCRIPTION)
			 `(SI:TYPEP-STRUCTURE . ,(CDR FORM)))
			((CLASS-SYMBOLP TYPE)
			 `(SUBINSTANCE-OF-CLASS-SYMBOL-P ,(CADR FORM) ',TYPE))
			(T FORM)))
		 (T FORM))))
	(T FORM)))


(ADD-OPTIMIZER SMALL-FLOATP OPEN-CODE-SMALL-FLOATP)
(DEFUN OPEN-CODE-SMALL-FLOATP (FORM)
  `(= (%DATA-TYPE ,(SECOND FORM)) ,DTP-SMALL-FLONUM))

;;; modify signp to be (AND (NUMBERP <form>) (<op> <form>)) if form is an atom
;;; and therefore can't have side effects
(ADD-OPTIMIZER SIGNP SIGNP-EXPAND)
(DEFUN SIGNP-EXPAND (X)
  (LET ((OP (CADR X))
	(OPND (CADDR X)))
     (COND ((ATOM OPND)(SIGNP-OPTIMIZE OP OPND))		;IF ATOM, OPTIMIZE IT
	   (T X))))

(DEFUN SIGNP-OPTIMIZE (OPERATION OPERAND)
  (PROG (NEW-FORM NOTP)
    (SETQ NEW-FORM
	  (LIST (COND ((STRING-EQUAL OPERATION 'E) 'ZEROP)
		      ((STRING-EQUAL OPERATION 'N) (SETQ NOTP T) 'ZEROP)
		      ((STRING-EQUAL OPERATION 'L) 'MINUSP)
		      ((STRING-EQUAL OPERATION 'GE) (SETQ NOTP T) 'MINUSP)
		      ((STRING-EQUAL OPERATION 'G) 'PLUSP)
		      ((STRING-EQUAL OPERATION 'LE) (SETQ NOTP T) 'PLUSP)
		      ((BARF OPERATION '|illegal SIGNP condition| 'WARN)
		       'PROGN))
		OPERAND))
    (AND NOTP (SETQ NEW-FORM (LIST 'NOT NEW-FORM)))
   (RETURN `(AND (NUMBERP ,OPERAND) ,NEW-FORM))))

(ADD-OPTIMIZER MAKE-ARRAY TRY-TO-USE-SIMPLE-MAKE-ARRAY SI:SIMPLE-MAKE-ARRAY)
(DEFUN TRY-TO-USE-SIMPLE-MAKE-ARRAY (FORM)
  (PROG TOP ((LEN (LENGTH FORM))
	     (DIMENSIONS-FORM NIL)
	     (AREA-FORM NIL)
	     (TYPE-FORM ''ART-Q)
	     (LEADER-LENGTH-FORM NIL))
    (IF (OR (< LEN 2)
	    (NOT (EVENP LEN)))
	(RETURN FORM))
    (SETQ DIMENSIONS-FORM (SECOND FORM))
    (LOOP FOR (KEYWORD-FORM ARGUMENT-FORM) ON (REST2 FORM) BY #'CDDR
	  DO (SELECTOR KEYWORD-FORM EQUAL
	       (('':AREA)
		(SETQ AREA-FORM ARGUMENT-FORM))
	       (('':TYPE)
		(SETQ TYPE-FORM ARGUMENT-FORM))
	       (('':LEADER-LENGTH)
		(SETQ LEADER-LENGTH-FORM ARGUMENT-FORM))
	       (OTHERWISE
		 (RETURN-FROM TOP FORM))))
    (IF (OR (NOT (TRIVIAL-FORM-P AREA-FORM))
	    (NOT (TRIVIAL-FORM-P TYPE-FORM))
	    (NOT (TRIVIAL-FORM-P LEADER-LENGTH-FORM)))
	(RETURN FORM))
    (RETURN
      (IF (NULL LEADER-LENGTH-FORM)
	  (IF (NULL AREA-FORM)
	      (LIST 'SI:SIMPLE-MAKE-ARRAY DIMENSIONS-FORM TYPE-FORM)
	      (LIST 'SI:SIMPLE-MAKE-ARRAY DIMENSIONS-FORM TYPE-FORM AREA-FORM))
	  (LIST 'SI:SIMPLE-MAKE-ARRAY DIMENSIONS-FORM TYPE-FORM AREA-FORM
		LEADER-LENGTH-FORM)))))

(ADD-OPTIMIZER AREF AREF-EXPANDER)
(DEFUN AREF-EXPANDER (FORM)
    (SELECTQ (LENGTH FORM)
       (3 (CONS 'AR-1 (CDR FORM)))
       (4 (CONS 'AR-2 (CDR FORM)))
       (5 (CONS 'AR-3 (CDR FORM)))
       (OTHERWISE (AND (< (LENGTH FORM) 3) (BARF FORM "Not enough args - AREF" 'WARN))
		  (CONS 'FUNCALL (CDR FORM)))))

(ADD-OPTIMIZER ASET ASET-EXPANDER)
(DEFUN ASET-EXPANDER (FORM)
    (SELECTQ (LENGTH FORM)
        (4 (CONS 'AS-1 (CDR FORM)))
        (5 (CONS 'AS-2 (CDR FORM)))
        (6 (CONS 'AS-3 (CDR FORM)))
	(OTHERWISE (AND (< (LENGTH FORM) 4) (BARF FORM "Not enough args - ASET" 'WARN))
		   (LIST 'STORE
                         (CONS 'FUNCALL (CDDR FORM))
                         (CADR FORM)))))

(ADD-OPTIMIZER ALOC ALOC-EXPANDER)
(DEFUN ALOC-EXPANDER (FORM)
    (SELECTQ (LENGTH FORM)
       (3 (CONS 'AP-1 (CDR FORM)))
       (4 (CONS 'AP-2 (CDR FORM)))
       (5 (CONS 'AP-3 (CDR FORM)))
       (OTHERWISE  (AND (< (LENGTH FORM) 3) (BARF FORM "Not enough args - ALOC" 'WARN))
		  `(GET-LOCATIVE-POINTER-INTO-ARRAY (FUNCALL . ,(CDR FORM))))))

;;; Find simple calls to MAKE-LIST and convert them into calls to the
;;; microcoded %MAKE-LIST.  NOTE THAT THIS CHANGES ORDER OF EVALUATION!
(ADD-OPTIMIZER MAKE-LIST MAKE-LIST-%MAKE-LIST %MAKE-LIST)
(DEFUN MAKE-LIST-%MAKE-LIST (FORM)
  (*CATCH 'GIVE-UP
    (LET ((LENGTH-OF-FORM (LENGTH FORM)))
      (COND ((= LENGTH-OF-FORM 3)
	     ;; It is old-style.
	     `(%MAKE-LIST 'NIL ,(SECOND FORM) ,(THIRD FORM)))
	    (T
	     ;; It is new-style.
	     (IF (NOT (EVENP LENGTH-OF-FORM))
		 (*THROW 'GIVE-UP FORM))
	     (LET ((AREA-FORM 'NIL) (INITIAL-VALUE-FORM 'NIL))
	       (DO ((OPTIONS (CDDR FORM) (CDDR OPTIONS)))
		   ((NULL OPTIONS))
		 (LET ((KEYWORD-FORM (FIRST OPTIONS))
		       (VALUE-FORM (SECOND OPTIONS)))
		   ;; If the keyword form isn't a quoted symbol, punt.
		   (IF (OR (NLISTP KEYWORD-FORM)
			   ( (LENGTH KEYWORD-FORM) 2)
			   (NEQ (FIRST KEYWORD-FORM) 'QUOTE)
			   (NOT (SYMBOLP (SECOND KEYWORD-FORM))))
		       (*THROW 'GIVE-UP FORM))
		   (SELECTQ (SECOND KEYWORD-FORM)
		     (:AREA (SETQ AREA-FORM VALUE-FORM))
		     (:INITIAL-VALUE (SETQ INITIAL-VALUE-FORM VALUE-FORM))
		     (OTHERWISE (*THROW 'GIVE-UP FORM)))))
	       `(%MAKE-LIST ,INITIAL-VALUE-FORM ,AREA-FORM ,(SECOND FORM))))))))

(ADD-OPTIMIZER STATUS SI:STATUS-OPTIMIZER)

;Next two are here mainly to avoid getting an error message from GETARGDESC about random FSUBR.
(ADD-OPTIMIZER COMMENT COMMENT-EXPAND)
(ADD-OPTIMIZER DECLARE COMMENT-EXPAND)
(DEFUN COMMENT-EXPAND (IGNORE) ''COMMENT)

(ADD-OPTIMIZER DEFPROP DEFPROP-EXPAND)
(DEFUN DEFPROP-EXPAND (X)
       `(PUTPROP ',(CADR X) ',(CADDR X) ',(CADDDR X)))

(ADD-OPTIMIZER GET-FROM-ALTERNATING-LIST GFAL-GET)
(DEFUN GFAL-GET (X)
  (COND ((OR (SYMBOLP (CADR X))
	     (AND (LISTP (CADR X))
		  (DO ((FORM1 (CADR X))) (NIL)
		    (AND (OR (GET (CAR FORM1) 'LOCF)
			     (GET (CAR FORM1) 'SI:SETF-EXPANDER))
			 (RETURN T))
		    (AND (EQ FORM1 (SETQ FORM1 (MACROEXPAND-1 FORM1))) (RETURN NIL)))))
	 `(GET (LOCF ,(CADR X)) . ,(CDDR X)))
	(T X)))

; Convert catches and throws
(ADD-OPTIMIZER CATCH CATCH-*CATCH)
(DEFUN CATCH-*CATCH (X)
       `(*CATCH ',(CADDR X) ,(CADR X)))

(ADD-OPTIMIZER THROW THROW-*THROW)
(DEFUN THROW-*THROW (X)
       `(*THROW ',(CADDR X) ,(CADR X)))

;;; Make *CATCH compile arguments other than the first and the last for
;;; effect rather than for value.
(ADD-OPTIMIZER *CATCH *CATCH-PROGNIFY)
(DEFUN *CATCH-PROGNIFY (FORM)
  (IF (CDDDR FORM) `(*CATCH ,(CADR FORM) (PROGN . ,(CDDR FORM)))
      FORM))

;Make PROGV work compiled.
;The resulting code will only work in CAR/CDR of NIL = NIL mode.
(ADD-OPTIMIZER PROGV PROGV-EXPAND)
(DEFUN PROGV-EXPAND (FORM)
   (LET ((VARNAMES (CADR FORM)) (VALS (CADDR FORM)) (BODY (CDDDR FORM)))
	`(PROG ((PROGV-VARS ,VARNAMES) (PROGV-VALS ,VALS))
	  LOOP (COND (PROGV-VARS
		      (INHIBIT-STYLE-WARNINGS
		         (BIND (INHIBIT-STYLE-WARNINGS (VALUE-CELL-LOCATION (CAR PROGV-VARS)))
			       (CAR PROGV-VALS)))
		      (SETQ PROGV-VARS (CDR PROGV-VARS))
		      (SETQ PROGV-VALS (CDR PROGV-VALS))
		      (GO LOOP)))
	       (RETURN (PROGN . ,BODY)))))

;Turn PROG1 into PROG2 since that is open-coded.
;Also turn (PROG1 FOO NIL) into FOO since PBIND generates that and it makes better code
(ADD-OPTIMIZER PROG1 PROG1-PROG2)
(DEFUN PROG1-PROG2 (FORM)
  (IF (EQUAL (CDDR FORM) '(NIL))
      (CADR FORM)
      `(PROG2 NIL . ,(CDR FORM))))

;;; Make PROGW work compiled, unfortunately, still calls EVAL in compiled code
(ADD-OPTIMIZER SI:PROGW PROGW-EXPAND)
(DEFUN PROGW-EXPAND (FORM)
  (DESTRUCTURING-BIND (IGNORE VARS-AND-VALS &BODY BODY) FORM
    `(PROG ((PROGW-VARS-AND-VALS ,VARS-AND-VALS))
	LOOP
	   (COND (PROGW-VARS-AND-VALS
		  (BIND (VALUE-CELL-LOCATION (CAAR PROGW-VARS-AND-VALS))
			(EVAL (CADAR PROGW-VARS-AND-VALS)))
		  (SETQ PROGW-VARS-AND-VALS (CDR PROGW-VARS-AND-VALS))
		  (GO LOOP)))
	   (RETURN (PROGN . ,BODY)))))

(ADD-OPTIMIZER LET-IF LET-IF-EXPAND)
(DEFUN LET-IF-EXPAND (FORM)
  (DESTRUCTURING-BIND (IGNORE COND VARS-AND-VALS &BODY BODY) FORM
     `(LET ()
	(COND (,COND ,(PBIND VARS-AND-VALS)))
	. ,BODY)))

(DEFUN PBIND (VARS-AND-VALS)
  (COND (VARS-AND-VALS
	 `(BIND (VALUE-CELL-LOCATION ',(CAAR VARS-AND-VALS))
		(PROG1 ,(CADAR VARS-AND-VALS)
		       ,(PBIND (CDR VARS-AND-VALS)))))))

(DEFPROP CONS (CONS-NCONS CONS-LIST) OPTIMIZERS)

;Turn (CONS foo NIL) into (NCONS foo), saving one instruction.
(DEFUN CONS-NCONS (FORM)
    (COND ((MEMBER (CADDR FORM) '(NIL 'NIL))
	   `(NCONS ,(CADR FORM)))
	  (T FORM)))

;Turn (CONS X (CONS Y NIL)) into (LIST X Y).  Doesn't change (CONS X NIL), though.
;Perhaps we want a hairier criterion, for the sake of
;those times when you create a list you are going to RPLACA
;and don't want LIST to be used.
(DEFUN CONS-LIST (FORM)
    (COND ((ATOM (CADDR FORM)) FORM)
	  ((EQ (CAADDR FORM) 'CONS)
	   (LET ((TEM (CONS-LIST (CADDR FORM))))
		(COND ((EQ (CAR TEM) 'LIST)
		       `(LIST ,(CADR FORM) . ,(CDR TEM)))
		      ((MEMBER (CADDR TEM) '(NIL 'NIL))
		       `(LIST ,(CADR FORM) ,(CADR TEM)))
		      (T FORM))))
	  (T FORM)))

;The following are here to make list-type structures work more efficiently.
;It's easier to put the optimization in the compiler than in DEFSTRUCT.

(ADD-OPTIMIZER NTH NTH-OPTIMIZE)
(ADD-OPTIMIZER NTHCDR NTHCDR-OPTIMIZE)

(DEFUN NTH-OPTIMIZE (X)
  (LET ((TEM (ASSOC (CADR X) '((0 . CAR) (1 . CADR) (2 . CADDR) (3 . CADDDR)))))
       (COND (TEM `(,(CDR TEM) ,(CADDR X)))
	     (T X))))

(DEFUN NTHCDR-OPTIMIZE (X)
  (LET ((TEM (ASSOC (CADR X) '((1 . CDR) (2 . CDDR) (3 . CDDDR) (4 . CDDDDR)))))
    (COND ((EQUAL (CADR X) 0) (CADDR X))
	  (TEM `(,(CDR TEM) ,(CADDR X)))
	  (T X))))

(DEFUN *LEXPR FEXPR (L)
  (DOLIST (X L)
    (AND (BOUNDP 'FUNCTIONS-DEFINED)
	 (SETQ FUNCTIONS-DEFINED
	       (LET #Q ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)) #M NIL 
		    (CONS X FUNCTIONS-DEFINED))))
    (PUTPROP X '((1005 (FEF-ARG-OPT FEF-QT-EVAL)))
	     'ARGDESC)))

(DEFUN *EXPR FEXPR (L)
  (DOLIST (X L)
    (AND (BOUNDP 'FUNCTIONS-DEFINED)
	 (SETQ FUNCTIONS-DEFINED
	       (LET #Q ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)) #M NIL 
		    (CONS X FUNCTIONS-DEFINED))))
    (PUTPROP X '((1005 (FEF-ARG-OPT FEF-QT-EVAL)))
	     'ARGDESC)))

(DEFUN *FEXPR FEXPR (L)
  (DOLIST (X L)
    (AND (BOUNDP 'FUNCTIONS-DEFINED)
	 (SETQ FUNCTIONS-DEFINED
	       (LET #Q ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)) #M NIL 
		    (CONS X FUNCTIONS-DEFINED))))
    (PUTPROP X '((1005 (FEF-ARG-OPT FEF-QT-QT)))
	     'ARGDESC)))

;Style checkers are, unlike optimizers or macro definitions,
;run only on user-supplied input, not the results of expansions.
;Also, they are not expected to return any values.
;They do not alter the input, merely print warnings if there
;is anything ugly in it.

;Style checkers are used to implement RUN-IN-MACLISP-SWITCH
;and OBSOLETE-FUNCTION-WARNING-SWITCH.  They can also warn
;about anything else that is ugly or frowned upon, though legal.

(DEFUN OBSOLETE (FORM)
    (AND OBSOLETE-FUNCTION-WARNING-SWITCH
	 (NOT RUN-IN-MACLISP-SWITCH)
	 (BARF (CAR FORM)
	       (OR (GET (CAR FORM) 'OBSOLETE)
		   '|obsolete function used|)
	       'WARN)))

(DEFUN MAKE-OBSOLETE (&QUOTE FUNCTION REASON)
  (PUTPROP FUNCTION 'OBSOLETE 'STYLE-CHECKER)
  (PUTPROP FUNCTION (STRING-APPEND "is an obsolete function; " REASON) 'OBSOLETE))

(MAKE-OBSOLETE GETCHAR "use strings")
(MAKE-OBSOLETE GETCHARN "use strings")
(MAKE-OBSOLETE IMPLODE "use strings")
(MAKE-OBSOLETE MAKNAM "use strings")
(MAKE-OBSOLETE EXPLODE "use strings")
(MAKE-OBSOLETE EXPLODEC "use strings")
(MAKE-OBSOLETE EXPLODEN "use strings")
(MAKE-OBSOLETE SAMEPNAMEP "use strings")
;This can't go in PROCES because it gets loaded before this file
(MAKE-OBSOLETE PROCESS-CREATE "it has been renamed to MAKE-PROCESS")

;; I guess these have to be on SYSTEM for this to work.
(DEFPROP MAKNUM UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP MUNKAM UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP *REARRAY UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP *FUNCTION UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP SUBRCALL UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP LSUBRCALL UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP PNGET UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP PNPUT UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP FSC UNIMPLEMENTED STYLE-CHECKER)

(DEFUN UNIMPLEMENTED (FORM)
    (BARF (CAR FORM) '|Maclisp function, not implemented in Lisp machine| 'WARN))

(COMMENT
;;; These are commented out because the style checker doesn't really manage
;;; to operate only on the user's typed-in code, and
;;; calls to these with one or zero args are generated by optimizers and macros, etc.
  (DEFPROP OR NEED-TWO-ARGS STYLE-CHECKER)
  (DEFPROP AND NEED-TWO-ARGS STYLE-CHECKER)
  (DEFPROP PROGN NEED-TWO-ARGS STYLE-CHECKER))

(DEFPROP PROG1 NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP PROG2 NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP + NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP * NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP PLUS NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP TIMES NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP QUOTIENT NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP DIFFERENCE NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP NCONC NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP APPEND NEED-TWO-ARGS STYLE-CHECKER)
(DEFUN NEED-TWO-ARGS (FORM)
    (COND ((NULL (CDDR FORM))
	   (BARF FORM '|Less than two arguments| 'WARN))))

(DEFPROP SETQ NEED-AN-ARG STYLE-CHECKER)
(DEFPROP PSETQ NEED-AN-ARG STYLE-CHECKER)
;Note:  one arg will cause an error, so who needs a warning too?

(DEFPROP COND NEED-AN-ARG STYLE-CHECKER)
(DEFPROP - NEED-AN-ARG STYLE-CHECKER)
(DEFPROP // NEED-AN-ARG STYLE-CHECKER)
(DEFUN NEED-AN-ARG (FORM)
    (OR (CDR FORM)
	(BARF (CAR FORM) '|with no arguments| 'WARN)))

;;; Style-checkers for things that don't work in Maclisp.

;These symbols don't exist in Maclisp, though they could, but they are likely losers.
(DEFPROP LISTP NOT-MACLISP STYLE-CHECKER)
(DEFPROP NLISTP NOT-MACLISP STYLE-CHECKER)
(DEFPROP NSYMBOLP NOT-MACLISP STYLE-CHECKER)

;These functions can't be added to Maclisp by a user.
(DEFPROP INTERN-LOCAL NOT-MACLISP STYLE-CHECKER)
(DEFPROP INTERN-SOFT NOT-MACLISP STYLE-CHECKER)
(DEFPROP INTERN-LOCAL-SOFT NOT-MACLISP STYLE-CHECKER)
(DEFPROP MAKE-ARRAY NOT-MACLISP STYLE-CHECKER)
(DEFPROP G-L-P NOT-MACLISP STYLE-CHECKER)
(DEFPROP ARRAY-LEADER NOT-MACLISP STYLE-CHECKER)
(DEFPROP STORE-ARRAY-LEADER NOT-MACLISP STYLE-CHECKER)
(DEFPROP MULTIPLE-VALUE NOT-MACLISP STYLE-CHECKER)
(DEFPROP MULTIPLE-VALUE-LIST NOT-MACLISP STYLE-CHECKER)
(DEFPROP MULTIPLE-VALUE-RETURN NOT-MACLISP STYLE-CHECKER)
(DEFPROP DO-NAMED NOT-MACLISP STYLE-CHECKER)
(DEFPROP RETURN-FROM NOT-MACLISP STYLE-CHECKER)
(DEFPROP RETURN-LIST NOT-MACLISP STYLE-CHECKER)
(DEFPROP BIND NOT-MACLISP STYLE-CHECKER)
(DEFPROP COMPILER-LET NOT-MACLISP STYLE-CHECKER)
(DEFPROP LOCAL-DECLARE NOT-MACLISP STYLE-CHECKER)
(DEFPROP CONS-IN-AREA NOT-MACLISP STYLE-CHECKER)
(DEFPROP LIST-IN-AREA NOT-MACLISP STYLE-CHECKER)
(DEFPROP NCONS-IN-AREA NOT-MACLISP STYLE-CHECKER)
(DEFPROP VALUE-CELL-LOCATION NOT-MACLISP STYLE-CHECKER)
(DEFPROP CAR-LOCATION NOT-MACLISP STYLE-CHECKER)
(DEFPROP PROPERTY-CELL-LOCATION NOT-MACLISP STYLE-CHECKER)
(DEFPROP FUNCTION-CELL-LOCATION NOT-MACLISP STYLE-CHECKER)
(DEFPROP FSET NOT-MACLISP STYLE-CHECKER)
(DEFPROP FBOUNDP NOT-MACLISP STYLE-CHECKER)
(DEFPROP FSYMEVAL NOT-MACLISP STYLE-CHECKER)
(DEFPROP CLOSURE NOT-MACLISP STYLE-CHECKER)

(DEFUN NOT-MACLISP (FORM)
    (AND RUN-IN-MACLISP-SWITCH
	 (BARF (CAR FORM) '|does not exist in Maclisp| 'WARN)))

;Return with more than one argument won't work in Maclisp.
(DEFPROP RETURN RETURN-STYLE STYLE-CHECKER)
(DEFUN RETURN-STYLE (FORM)
    (AND RUN-IN-MACLISP-SWITCH
	 (CDDR FORM)
	 (BARF FORM '|returning multiple values doesn't work in Maclisp| 'WARN)))

;Named PROGs don't work in Maclisp.  PROG variables can't be initialized.
;Also, lots of tags and things like a GO to a RETURN are ugly.
(DEFPROP PROG PROG-STYLE STYLE-CHECKER)
(DEFUN PROG-STYLE (FORM)
    (PROG (PROGNAME)
	  (AND (ATOM (CADR FORM))
	       (CADR FORM)
	       (PROGN (SETQ PROGNAME (CADR FORM))
		      (SETQ FORM (CDR FORM))))
	  (COND (RUN-IN-MACLISP-SWITCH
		 (AND PROGNAME
                      (BARF PROGNAME '|PROG names won't work in Maclisp| 'WARN))
		 (OR (EVERY (CADR FORM) (FUNCTION ATOM))
		     (BARF (CADR FORM) '|PROG variables can't be initialized in Maclisp|
			   'WARN))))))

;; Check a LAMBDA for things that aren't allowed in Maclisp.
;; Called only if RUN-IN-MACLISP-SWITCH is set.
(DEFUN LAMBDA-STYLE (LAMBDA-EXP)
    (COND ((EQ (CAR LAMBDA-EXP) 'NAMED-LAMBDA)
	   (BARF 'NAMED-LAMBDA '|does not work in Maclisp| 'WARN)
	   (POP LAMBDA-EXP)))
    (DO ((VARLIST (CADR LAMBDA-EXP) (CDR VARLIST)) (KWDBARF)) ((NULL VARLIST))
       (COND ((ATOM (CAR VARLIST))
	      (AND (NOT KWDBARF)
		   (MEMQ (CAR VARLIST) LAMBDA-LIST-KEYWORDS)
		   (SETQ KWDBARF T)
		   (BARF (CAR VARLIST)
			 '|LAMBDA-list keywords aren't allowed in Maclisp| 'WARN)))
	     (T (BARF (CAR VARLIST)
		      '|LAMBDA variables can't have initializations in Maclisp|
		      'WARN)))))

;;; These functions are defined in ENCAPS, but loaded here
(compiler:add-optimizer encapsulation-let encapsulation-let-fix)
(compiler:add-optimizer encapsulation-list* encapsulation-list*-fix)
   