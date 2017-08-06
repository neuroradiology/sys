; -*-Package:SYSTEM-INTERNALS; Mode:LISP-*-
; LISP Machine Package for Logging In and Out.		DLW 11/13/77 
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;; List of forms to be evaluated on logout
;; to undo the things done at login.
;; The various LOGIN-MUMBLE functions push undo forms on this list.
(DEFVAR LOGOUT-LIST NIL)

;History so we can tell who has had their little paws into a saved band.
;List of elements (user host cadr time)
(DEFVAR LOGIN-HISTORY NIL)

(DEFUN LOGIN (USER-NAME &OPTIONAL (HOST ASSOCIATED-MACHINE) (LOAD-INIT-FILE-P T))
  ;; Do this so LOGIN init list has the correct enviroment.
  (DECLARE (SPECIAL USER-ID HOST LOAD-INIT-FILE-P))
  (LET ((WIN-P NIL))
    (UNWIND-PROTECT
      (PROGN
	(LOGOUT)
	(AND (EQ HOST T)			;For compatibility
	     (SETQ HOST ASSOCIATED-MACHINE LOAD-INIT-FILE-P NIL))
	(SETQ USER-ID (STRING-TRIM '(#\SP) (STRING USER-NAME)))
	(SETQ HOST (FS:GET-PATHNAME-HOST HOST))
	(SETQ FS:USER-LOGIN-MACHINE HOST)
	(INITIALIZATIONS 'LOGIN-INITIALIZATION-LIST)
	(RESET-INITIALIZATIONS 'LOGOUT-INITIALIZATION-LIST)
	(PUSH (LIST USER-ID HOST
		    (AND (BOUNDP 'LOCAL-PRETTY-HOST-NAME) LOCAL-PRETTY-HOST-NAME)
		    (AND (FBOUNDP 'TIME:PRINT-CURRENT-TIME) (TIME:PRINT-CURRENT-TIME NIL)))
	      LOGIN-HISTORY)
	;; This is an attempt to prevent losers from losing and writing on the LISPM directory
	(FS:SET-DEFAULT-PATHNAME (FS:USER-HOMEDIR) FS:*DEFAULT-PATHNAME-DEFAULTS*)
	(FS:SET-DEFAULT-PATHNAME (FS:USER-HOMEDIR) FS:LOAD-PATHNAME-DEFAULTS)
	(AND LOAD-INIT-FILE-P
	     (LOAD (FS:INIT-FILE-PATHNAME "LISPM" HOST T) "USER" T T))
	(SETQ WIN-P T))
      (IF (NOT WIN-P)
	  ;; If user aborts during login, particularly if he types Abort when
	  ;; being asked for his password, log him out so he can try again.
	  (LOGOUT))))
  T)

(DEFUN LOGOUT ()
  (MAPC 'EVAL LOGOUT-LIST)
  (INITIALIZATIONS 'LOGOUT-INITIALIZATION-LIST)
  (RESET-INITIALIZATIONS 'LOGIN-INITIALIZATION-LIST) 
  ;; Do this last so that the initializations won't ask you to login.
  (SETQ USER-ID ""
	FS:USER-HOMEDIRS NIL
	FS:USER-PERSONAL-NAME ""
	FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST ""
	FS:USER-GROUP-AFFILIATION #/-
	FS:USER-LOGIN-MACHINE ASSOCIATED-MACHINE)
  (SETQ LOGOUT-LIST NIL)
  T)

(DEFUN LOGIN-EVAL (X)	;Value returned by such a form is how to undo it
  (PUSH X LOGOUT-LIST))

(DEFUN LOGIN-SETQ (&QUOTE &REST L)  ;Undoing SETQ
  (DO L L (CDDR L) (NULL L)
      (COND ((BOUNDP (CAR L))
	     (PUSH `(SETQ ,(CAR L) ',(SYMEVAL (CAR L))) LOGOUT-LIST))
	    (T (PUSH `(MAKUNBOUND ',(CAR L)) LOGOUT-LIST)))
      (SET (CAR L) (EVAL (CADR L)))))

;Undoable FDEFINE.
;It would be nice if there were FUNDEFINE.
(DEFUN LOGIN-FDEFINE (FUNCTION-NAME DEFINITION)  ;Undoing FDEFINE
  (AND (FDEFINEDP FUNCTION-NAME)
       (PUSH `(FDEFINE ',FUNCTION-NAME ',(FDEFINITION FUNCTION-NAME)) LOGOUT-LIST))
  (FDEFINE FUNCTION-NAME DEFINITION))
