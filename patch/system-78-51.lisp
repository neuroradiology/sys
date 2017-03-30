;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.51
;;; Reason: Make ASSIGN-ALTERNATE only read symbols into current package.
;;; Written 6/08/10 16:36:29 by RJS,
;;; while running on Unknown from band 1
;;; with System 78.50, ZMail 38.5, Tape 6.5, LMFS 21.34, Symbolics 8.13, microcode 841.



; From file qmisc.lisp >sys UNKNOWN:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;These are for reading in QCOM, and the like
(DEFUN ASSIGN-ALTERNATE (X)
   (PROG NIL 
    L	(COND ((NULL X)(RETURN NIL)))
	(SET (INTERN-LOCAL (GET-PNAME (CAR X)) PACKAGE) (CADR X))
	(SETQ X (CDDR X))
	(GO L)))

(GLOBALIZE 'ASSIGN-ALTERNATE)

)
