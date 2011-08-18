;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.38
;;; Reason: Minor system menu fix, add more info to PEEK process mode
;;; Written 12/25/81 17:08:03 by HIC,
;;; while running on Basset from band 2
;;; with System 78.36, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.21, Canon 9.6, microcode 841.



; From file SYSMEN.LISP >LMWIN POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

;Programs outside of the basic system which want to appear in the system menu
;call this function, specifying string, form to evaluate, mouse documentation
;string, and optionally the name of another program they should be listed after.
;The default (AFTER=NIL) is to add new programs at the bottom.
;If AFTER=T the new program is added at the top.
;The SELECT-OR-CREATE-WINDOW-OF-FLAVOR function is often useful.
(DEFUN ADD-TO-SYSTEM-MENU-PROGRAMS-COLUMN (NAME FORM DOCUMENTATION &OPTIONAL AFTER)
  (LET ((ITEM `(,NAME :EVAL ,FORM :DOCUMENTATION ,DOCUMENTATION))
	(OLD))
  (IF (SETQ OLD (ASSOC NAME *SYSTEM-MENU-PROGRAMS-COLUMN*))
      (SETF (CDR OLD) (CDR ITEM))
    (SETQ *SYSTEM-MENU-PROGRAMS-COLUMN*
	  (IF (EQ AFTER T) (CONS ITEM *SYSTEM-MENU-PROGRAMS-COLUMN*)
	    (LOOP WITH AFTER = (OR (ASSOC AFTER *SYSTEM-MENU-PROGRAMS-COLUMN*)
				   (CAR (LAST *SYSTEM-MENU-PROGRAMS-COLUMN*)))
		  FOR X IN *SYSTEM-MENU-PROGRAMS-COLUMN*
		  COLLECT X
		  WHEN (EQ X AFTER)
		  COLLECT ITEM))))))

)

; From file PEEK.LISP >LMWIN POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFUN PEEK-PROCESSES (IGNORE)
  "Shows state of all active processes."
  (LIST ()
	(SCROLL-PARSE-ITEM (FORMAT NIL "~30A~25A~10A~9A~14A"
				   "Process Name" "State" "Priority"
				   "Quantum" "Quantum Left"))
	(SCROLL-PARSE-ITEM "")
	(SCROLL-MAINTAIN-LIST #'(LAMBDA () ALL-PROCESSES)
			      #'(LAMBDA (PROCESS)
				  (SCROLL-PARSE-ITEM
				    `(:MOUSE-ITEM
				       (NIL :EVAL (PEEK-PROCESS-MENU ',PROCESS 'ITEM 0)
					    :DOCUMENTATION
					    "Menu of useful things to do to this process.")
				       :STRING ,(PROCESS-NAME PROCESS) 30.)
				    `(:FUNCTION ,#'PEEK-WHOSTATE ,(NCONS PROCESS) 25.)
				    `(:FUNCTION ,PROCESS (:PRIORITY) 10. ("~D."))
				    `(:FUNCTION ,PROCESS (:QUANTUM) 9. ("~D."))
				    `(:FUNCTION ,PROCESS (:QUANTUM-REMAINING) 14. ("~D."))))
			      NIL
			      #'(LAMBDA (STATE)
				  (PROG ()
				    (RETURN (CAR STATE) (CDR STATE) (NULL (CDR STATE))))))
	(SCROLL-PARSE-ITEM "")
	(SCROLL-PARSE-ITEM "Clock Function List")
	(SCROLL-MAINTAIN-LIST #'(LAMBDA () SI:CLOCK-FUNCTION-LIST)
			      #'(LAMBDA (FUNC)
				  (SCROLL-PARSE-ITEM `(:STRING ,(GET-PNAME FUNC)))))))

)
