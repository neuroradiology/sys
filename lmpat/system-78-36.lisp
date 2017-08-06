;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.36
;;; Reason: Repair ONCE-ONLY from changes in 78.11
;;; Written 12/25/81 14:05:49 by BEE,
;;; while running on Beagle from band 1
;;; with System 78.35, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.21, Canon 9.6, microcode 841.



; From file LMMAC.LISP >LISPM2 POINTER:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

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
	     `((LAMBDA ,(NREVERSE ,BIND-VARS) ,RESULT) . ,(NREVERSE ,BIND-VALS)))))))

)
