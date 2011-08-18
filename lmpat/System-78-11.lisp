;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.11
;;; Reason: Fix ONCE-ONLY to not reverse the order of evaluation.
;;; Written 12/12/81 11:35:04 by dlw,
;;; while running on Lisp Machine Eighteen from band 3
;;; with System 78.10, ZMail 38.1, Local-File 30.3, microcode 836.



; From file LMMAC > LISPM2; AI:
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
	     `((LAMBDA ,,(NREVERSE BIND-VARS) ,RESULT) . ,,(NREVERSE BIND-VALS)))))))

)
