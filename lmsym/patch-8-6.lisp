;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.6
;;; Reason: Peek process/FS features (in MIT source).
;;; Written 12/21/81 09:12:48 by BSG,
;;; while running on Retriever from band 5
;;; with System 78.23, ZMail 38.4, Symbolics 8.5, Tape 6.2, LMFS 21.15, Canon 9.1, microcode 841.



; From file PEEK.LISP DSK:<LMWIN> SCRC:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFUN PEEK-FILE-SYSTEM-STREAM-MENU (STREAM)
  (LEXPR-FUNCALL #'PROCESS-RUN-FUNCTION "Peek File System Menu"
		 SELF ':FUNCALL-INSIDE-YOURSELF #'PEEK-FILE-SYSTEM-MENU-INTERNAL
		 (LIST STREAM)))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-PEEK)
(DEFUN PEEK-FILE-SYSTEM-MENU-INTERNAL (STREAM)
  (LET ((TERMINAL-IO TYPEOUT-WINDOW))
    (MENU-CHOOSE `(("Close" :EVAL (FUNCALL ',STREAM ':CLOSE)
		    :DOCUMENTATION "Close selected file (normally).")
		   ("Abort" :EVAL (FUNCALL ',STREAM ':CLOSE ':ABORT)
		    :DOCUMENTATION "Close selected file (aborts writing).")
		   ("Delete" :EVAL (FUNCALL ',STREAM ':DELETE)
		    :DOCUMENTATION "Delete selected file, but don't close it.")
		   ("Describe" :EVAL (DESCRIBE ',STREAM)
		    :DOCUMENTATION "Describe the file's stream.")
		   ("Inspect" :EVAL (INSPECT ',STREAM)
		    :DOCUMENTATION "Inspect the file's stream.")
		   )))))

(DEFUN PEEK-PROCESS-MENU (&REST ARGS)
  (LEXPR-FUNCALL #'PROCESS-RUN-FUNCTION "Peek Process Menu"
		 SELF ':FUNCALL-INSIDE-YOURSELF #'PEEK-PROCESS-MENU-INTERNAL ARGS))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-PEEK)
(DEFUN PEEK-PROCESS-MENU-INTERNAL (PROCESS &REST IGNORE &AUX CHOICE)
  "Menu for interesting operations on processes in a peek display"
  (LET ((TERMINAL-IO TYPEOUT-WINDOW))
    (SETQ CHOICE (MENU-CHOOSE
		   '(("Arrest" :VALUE PROCESS-ARREST
		      :DOCUMENTATION "Arrest the selected process.  Undone by Un-Arrest.")
		     ("Un-Arrest" :VALUE PROCESS-UN-ARREST
		      :DOCUMENTATION "Un-Arrest the selected process.  Complement of Arrest.")
		     ("Flush" :VALUE PROCESS-FLUSH
		      :DOCUMENTATION
		      "Unwind the selected process' stack and make it unrunnable.  Ask for confirmation.")
		     ("Reset" :VALUE PROCESS-RESET
		      :DOCUMENTATION "Reset the selected process.  Ask for confirmation.")
		     ("Kill" :VALUE PROCESS-KILL
		      :DOCUMENTATION
		      "Kill the selected process.  Ask for confirmation.")
		     ("EH" :VALUE PROCESS-EH
		      :DOCUMENTATION
		      "Call the error handler to examine the selected process."))))
    (SELECTQ CHOICE
      (PROCESS-ARREST (FUNCALL PROCESS ':ARREST-REASON))
      (PROCESS-UN-ARREST (FUNCALL PROCESS ':REVOKE-ARREST-REASON))
      (PROCESS-FLUSH (IF (MOUSE-Y-OR-N-P (FORMAT NIL "Flush ~A" PROCESS))
			 (FUNCALL PROCESS ':FLUSH)))
      (PROCESS-RESET (IF (MOUSE-Y-OR-N-P (FORMAT NIL "Reset ~A" PROCESS))
			 (FUNCALL PROCESS ':RESET)))
      (PROCESS-KILL (IF (MOUSE-Y-OR-N-P (FORMAT NIL "Kill ~A" PROCESS))
			(FUNCALL PROCESS ':KILL)))
      (PROCESS-EH (FUNCALL-SELF ':FORCE-KBD-INPUT `(EH ,PROCESS)))
      (NIL)
      (OTHERWISE (BEEP))))))
)

