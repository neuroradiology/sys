;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.12
;;; Reason: Improve format of some error handler messages.
;;; Written 12/12/81 12:09:16 by dlw,
;;; while running on Lisp Machine Eighteen from band 3
;;; with System 78.10, ZMail 38.1, Local-File 30.3, microcode 836.



; From file EHC > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFUN COMMAND-LOOP (ERROR-SG ETE &AUX FUNCTION SEXP 
				       (EVALHOOK NIL)
				       (WINDOW-ERROR-HANDLER NIL)
				       IO-BUFFER)
  (COND ((MEMQ ':IO-BUFFER (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
	 (SETQ IO-BUFFER (FUNCALL STANDARD-INPUT ':IO-BUFFER))
	 (BIND (LOCF (TV:IO-BUFFER-OUTPUT-FUNCTION IO-BUFFER)) 'IO-BUFFER-OUTPUT-FUNCTION)
	 (BIND (LOCF (TV:IO-BUFFER-INPUT-FUNCTION IO-BUFFER)) NIL)))
  (DO ((NUMERIC-ARG NIL NIL)
       (-)
       (+ (SYMEVAL-IN-STACK-GROUP '- ERROR-SG))
       (* (SYMEVAL-IN-STACK-GROUP '* ERROR-SG)))
      (())
    (INHERITING-VARIABLES-FROM (ERROR-SG)  ;Do this every time around the loop in case of setq
      (*CATCH 'SYS:COMMAND-LEVEL
	(*CATCH 'QUIT
	  (FORMAT T "~&")
	  (DO () (NIL)			;This loop processes numeric args
	    ;; Read the next command or sexp, with combined rubout processing.
	    (MULTIPLE-VALUE (FUNCTION SEXP)
	      (COMMAND-LOOP-READ))
	    ;; If it's a character, execute the definition or complain.
	    (COND ((NUMBERP FUNCTION)
		   (SETQ NUMERIC-ARG
			 (IF (NULL NUMERIC-ARG) FUNCTION (+ FUNCTION (* 10. NUMERIC-ARG))))
		   (TYO #\SPACE))
		  (FUNCTION
		   (PRINC " ")	;Print a space after the echo in case it prints something
		   (RETURN (IF (NOT NUMERIC-ARG)
			       (FUNCALL FUNCTION ERROR-SG ETE)
			       (FUNCALL FUNCTION ERROR-SG ETE NUMERIC-ARG))))
		  ;; If there was no command, there was a sexp, so eval it.
		  (T
		   (LET ((// (SG-EVAL ERROR-SG (SETQ - SEXP) T)))
		     (SETQ + -)
		     (COND ((NEQ // ERROR-FLAG)
			    (SETQ * (CAR //))
			    (DOLIST (VALUE //)
			      (TERPRI)
			      (FUNCALL (OR PRIN1 #'PRIN1) VALUE)))))
		   (RETURN)))
	    ))))))

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFUN (ARGTYP HELP-MESSAGE) (IGNORE ETE)
  (AND (FIFTH ETE)
       (FORMAT T "C-C asks for a replacement argument and continues.")))

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP FIXNUM-OVERFLOW "C-C asks for a fixnum to use as the result." HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP FLOATING-EXPONENT-UNDERFLOW "C-C proceeds using 0.0 as the result." HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP FLOATING-EXPONENT-OVERFLOW "C-C asks for a flonum to use as the result." HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFUN (SUBSCRIPT-OOB HELP-MESSAGE) (IGNORE ETE)
  (IF (FOURTH ETE)
      (FORMAT T "C-C asks for a replacement subscript and proceeds.")))

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP ARRAY-HAS-NO-LEADER "C-C asks for a value to use as the result and proceeds."
	 HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP FILL-POINTER-NOT-FIXNUM "C-C asks for a value to use as the result and proceeds."
	 HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP PDL-OVERFLOW "C-C grows the pdl and proceeds." HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP MAR-BREAK "C-C proceeds." HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFUN (TRANS-TRAP HELP-MESSAGE) (SG IGNORE)
  (FORMAT T "C-C continues, using a specified value instead of the undefined ~A.~@
	    M-C defines the ~:*~A to a specified value, then continues."
	  (MULTIPLE-VALUE-BIND (SYMBOL CELL-TYPE) (DECODE-NULL-POINTER (SG-SAVED-VMA SG))
	    (OR (AND (SYMBOLP SYMBOL)
		     (SELECTQ CELL-TYPE
		       (VALUE "variable")
		       (FUNCTION "function")))
		"memory cell"))))

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFUN (FUNCTION-ENTRY HELP-MESSAGE) (SG IGNORE)
  (FORMAT T "C-C offers to try again")
  (SELECTQ (FUNCTION-ENTRY-ERROR SG)
    (< (FORMAT T ", asking you for additional arguments."))
    (> (FORMAT T ", dropping the extra arguments."))))

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP :BREAK "C-C proceeds." HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP CALL-TRAP "C-C proceeds." HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP EXIT-TRAP "C-C proceeds." HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP THROW-EXIT-TRAP "C-C proceeds." HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP :WRONG-TYPE-ARGUMENT "C-C asks for a replacement argument and proceeds." HELP-MESSAGE)

)

; From file EHR > LMWIN; AI:
#8R EH:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "EH")))

(DEFPROP TURD-ALERT "C-C proceeds, perhaps writing garbage on the screen." HELP-MESSAGE)

)
