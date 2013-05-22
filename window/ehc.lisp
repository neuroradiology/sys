;;;The error handler commands -*- Mode:LISP; Package:EH -*-

;; Commands in the dispatch table are given the SG and the ETE,
;; and a third arg which is the numeric argument may or may not be passed.

;; Any command which wants to return out of the error handler should
;; do a throw to FINISHED after restarting the erring stack group.

(DEFVAR WINDOW-ERROR-HANDLER NIL)			;Flag when inside window error handler

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
			 (IF (NULL NUMERIC-ARG) FUNCTION (+ FUNCTION (* 10. NUMERIC-ARG)))))
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

(DEFVAR READING-COMMAND NIL)	;This is bound to T while reading a command char, for the
				;io-buffer function.

;; Read from STANDARD-INPUT either a control-character (or ? or Help)
;; or a s-expression.  Return CHAR or return NIL and the s-expression.
(DEFUN COMMAND-LOOP-READ ()
  (PROG (CHAR SEXP FLAG FUNCTION)
    RETRY
     ;; Read a character.
     (LET ((READING-COMMAND T))
       (SETQ CHAR (FUNCALL STANDARD-INPUT ':TYI)))
     ;; Now, if the char is special, echo and return it.
     (COND ((OR (LDB-TEST %%KBD-CONTROL-META CHAR)
		(COMMAND-LOOKUP CHAR))
	    (COND ((SETQ FUNCTION (COMMAND-LOOKUP CHAR))
		   (AND (EQ FUNCTION 'COM-NUMBER)
			(SETQ FUNCTION (- (LDB %%CH-CHAR CHAR) #/0)))
		   (FORMAT T "~C" CHAR)
		   (RETURN FUNCTION))))
	   ((= CHAR #\RUBOUT) (GO RETRY)))	;Ignore rubouts
     ;; Otherwise, unread it and read an s-exp instead.
     (FUNCALL STANDARD-INPUT ':UNTYI CHAR)
     (COND ((AND (NOT RUBOUT-HANDLER)
		 (MEMQ ':RUBOUT-HANDLER (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS)))
	    (MULTIPLE-VALUE (SEXP FLAG)
	      (FUNCALL STANDARD-INPUT ':RUBOUT-HANDLER '((:FULL-RUBOUT :FULL-RUBOUT))
		       #'SI:READ-FOR-TOP-LEVEL))
	    (AND (EQ FLAG ':FULL-RUBOUT) (GO RETRY))
	    (RETURN NIL SEXP))
	   ;; If stream has no rubout handler, degrade gracefully.
	   ((RETURN NIL (SI:READ-FOR-TOP-LEVEL))))))

(DEFUN IO-BUFFER-OUTPUT-FUNCTION (IGNORE CHAR)
  (COND ((NOT (NUMBERP CHAR)) CHAR)		;Blips shouldn't get here, but don't die
	((AND READING-COMMAND (COMMAND-LOOKUP CHAR)) CHAR)	;Don't intercept commands
	((MEMQ CHAR TV:KBD-INTERCEPTED-CHARACTERS)	;Standard character processing
	 (TV:KBD-INTERCEPT-CHARACTER CHAR)
	 (FORMAT T "~&Back to error handler.~%")
	 (VALUES CHAR T))
	((EQ CHAR #/G)				;Compatibility with ancient history
	 (TV:KBD-INTERCEPT-CHARACTER #\ABORT))
	(T CHAR)))

(DEFUN COMMAND-LOOKUP (CHAR)
  (AREF COMMAND-DISPATCH-TABLE (LDB %%KBD-CONTROL-META CHAR)
			       (LDB %%KBD-CHAR (CHAR-UPCASE CHAR))))

;; Utility function used by the top level, and various commands.

;; Print out the error message of the error.
(DEFUN PRINT-ERROR-MESSAGE (SG ETE &OPTIONAL BRIEF-FLAG
				   &AUX (PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
					(PRINLENGTH ERROR-MESSAGE-PRINLENGTH)
					INFORM)
  (FORMAT T (IF (EQ BRIEF-FLAG 'RETURN)		;Flag beginning of error message 
		"~&Back to " "~&>>"))
  (PRINT-DEBUGGING-ERROR-MESSAGE SG ETE BRIEF-FLAG) ;Give type of error & microcode info
  (COND ((SETQ INFORM (GET (CAR ETE) 'INFORM))	;Give actual text of error message
	 (FUNCALL INFORM SG ETE))
	((EQ (CAR ETE) ':BREAK))
	(T
	 (FORMAT T "There is no error message provided for this error!~%")))
  (COND ((NOT BRIEF-FLAG)			;If not suppressed give backtrace
	 (FORMAT T "~&While in the function ")
	 (SHORT-BACKTRACE SG NIL ERROR-MESSAGE-BACKTRACE-LENGTH
			  (IF (NEQ (FIRST ETE) 'FERROR)
			      0
			      (DO ((FRAME ORIGINAL-FRAME (SG-PREVIOUS-ACTIVE SG FRAME))
				   (RP (SG-REGULAR-PDL SG))
				   (I 0 (1+ I)))
				  ((NULL FRAME) 0)
				(OR (GET (FUNCTION-NAME (RP-FUNCTION-WORD RP FRAME))
					 ':ERROR-REPORTER)
				    (RETURN I)))))
	 (TERPRI)
	 (AND					;Check for user message hook
	   (SETQ INFORM (SYMEVAL-IN-STACK-GROUP 'ERROR-MESSAGE-HOOK SG))
	   (FUNCALL INFORM))))	;Call user function to explain
  (FORMAT T "~&"))		;Compensate if it forgot to CRLF

;;; This function just prints the introduction telling you the type of error,
;;; and where it was in the microcode in the case of a microcode error
(DEFUN PRINT-DEBUGGING-ERROR-MESSAGE (SG ETE &OPTIONAL BRIEF &AUX TEM FLAG)
  (COND ((EQ (CAR ETE) 'BREAK)
	 (PRINC "BREAK"))
	((EQ (CAR ETE) 'FERROR)
	 (PRINC "ERROR: "))
	(BRIEF
	 (PRINC "TRAP: "))
	(T
	 (FORMAT T ">>TRAP ~A ~A"
		 (SG-TRAP-MICRO-PC SG) ETE)
	 (COND ((LDB-TEST %%LP-EXS-MICRO-STACK-SAVED
		      (RP-EXIT-WORD (SG-REGULAR-PDL SG) (SG-AP SG)))
		;; Process the micro-stack of the active frame, if any
		(DO I (SG-SPECIAL-PDL-POINTER SG) (1- I) NIL
		    (LET ((PC (AREF (SG-SPECIAL-PDL SG) I)))
		      (SETQ TEM (ASSQ (1- (%POINTER PC)) CALLS-SUB-LIST))
		      (COND (TEM
			     (OR FLAG (FORMAT T " ->"))
			     (SETQ FLAG T)
			     (FORMAT T "  ~A " (CDR TEM)))))
		    (OR (ZEROP (%P-FLAG-BIT (ALOC (SG-SPECIAL-PDL SG) I)))
			(RETURN NIL)))))
	 (TERPRI))))

(DEFUN CONSTANT-FORM-P (X)
  (COND ((SYMBOLP X)
	 (OR (EQ X 'T) (NULL X)))
	((LISTP X) (EQ (CAR X) 'QUOTE))
	(T T)))

;; This is the function used by proceed routines to ask for
;; a form to read and eval.  When proceeding is done under control
;; of a condition handler, this function arranges to get the
;; object supplied by the handler instead of asking.
(DEFUN READ-OBJECT (PROMPT &OPTIONAL (FRESH-LINE T) &AUX FORM OBJECT)
  (COND ((EQ CONDITION-PROCEED-FLAG T)
	 (SETQ CONDITION-PROCEED-FLAG 'GOBBLED)
	 CONDITION-PROCEED-VALUE)
	((EQ CONDITION-PROCEED-FLAG 'GOBBLED)
	 (FERROR NIL "READ-OBJECT called twice by proceed routine for ~S"
		 (CAR (SG-TRAP-TAG ERROR-SG))))
	(WINDOW-ERROR-HANDLER
	 (WINDOW-READ-OBJECT PROMPT))
	(T
	 (DO ()
	     ((PROGN
		(PRINC PROMPT)
		(AND FRESH-LINE (TERPRI))
		(SETQ FORM (SI:READ-FOR-TOP-LEVEL))
		(SETQ OBJECT (CAR (SG-EVAL ERROR-SG FORM)))
		(TERPRI)
		(OR (CONSTANT-FORM-P FORM)
		    (LET ((PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
			  (PRINLENGTH ERROR-MESSAGE-PRINLENGTH))
		      (FQUERY '(:LIST-CHOICES NIL) "The object is ~S, ok? " OBJECT))))
	      OBJECT)))))

;;;When about to exit the error handler, maybe get rid of the window
(DEFUN LEAVING-ERROR-HANDLER ()
  (AND WINDOW-ERROR-HANDLER
       (TV:DELAYING-SCREEN-MANAGEMENT
	 (IF (EQ WINDOW-ERROR-HANDLER T)
	     (FUNCALL ERROR-HANDLER-WINDOW ':DESELECT T)
	     (FUNCALL WINDOW-ERROR-HANDLER ':SELECT))
	 ;;If this doesn't leave the window still on the screen, it is useless, so bury it.
	 (OR (TV:SHEET-EXPOSED-P ERROR-HANDLER-WINDOW)
	     (FUNCALL ERROR-HANDLER-WINDOW ':BURY)))))

;;;Continue the stack group, returning VAL if specified
(DEFUN PROCEED-SG (SG &OPTIONAL VAL)
  (LEAVING-ERROR-HANDLER)
  (WITHOUT-INTERRUPTS
    (FREE-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP)
    (STACK-GROUP-RESUME SG VAL)))

;; Backtrace commands.

;; Short backtraces contain only function names.  Full ones contain arg names and values.
;; Both versions take arguments the same way.  The first is the SG, and the
;; second is ignored so that the functions may be used as commands.
;; They will print no more than N frames if N is present.
;; If SKIP is nonzero, the first that many frames are skipped before the
;; N frames are printed.  Used to put them in curly brackets, but that was
;; fairly useless.

;; This prints out like the Maclisp BAKTRACE, and does not TERPRI at beginning
;; nor end.

(DEFUN COM-SHORT-BACKTRACE (SG IGNORE &OPTIONAL (N 777777))
  (SHORT-BACKTRACE SG NIL N))

(DEFUN SHORT-BACKTRACE (SG IGNORE &OPTIONAL (N 777777) (SKIP 0) UNINTERESTING-FLAG)
  (PRINT-BACKTRACE SG N SKIP UNINTERESTING-FLAG
		   #'(LAMBDA (SG FRAME COUNT)
		       (OR (ZEROP COUNT) (PRINC "  "))
		       (PRIN1 (FUNCTION-NAME (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) FRAME))))))

(DEFUN FULL-BACKTRACE (SG IGNORE &OPTIONAL (N 777777) (SKIP 0) UNINTERESTING-FLAG)
  (PRINT-BACKTRACE SG N SKIP UNINTERESTING-FLAG
		   #'(LAMBDA (SG FRAME IGNORE)
		       (PRINT-FUNCTION-AND-ARGS SG FRAME))))

(DEFUN PRINT-BACKTRACE (SG N SKIP UNINTERESTING-FLAG
			FRAME-PRINTER-FUNCTION)
  (DO ((FRAME INNERMOST-VISIBLE-FRAME
	      (FUNCALL (IF UNINTERESTING-FLAG #'SG-PREVIOUS-ACTIVE
			   #'SG-PREVIOUS-INTERESTING-ACTIVE)
		       SG FRAME))
       (I (- SKIP) (1+ I)))
      ((OR (>= I N) (NULL FRAME)) NIL)
    (OR (MINUSP I) (FUNCALL FRAME-PRINTER-FUNCTION SG FRAME I))))

(DEFUN FULL-BACKTRACE-UNINTERESTING (SG IGNORE &OPTIONAL (N 777777) (SKIP 0))
  (FULL-BACKTRACE SG NIL N SKIP T))

(DEFUN PRINT-FUNCTION-AND-ARGS (SG FRAME
				&AUX FUNCTION (RP (SG-REGULAR-PDL SG))
				(PRINLEVEL FUNCTION-PRINLEVEL)
				(PRINLENGTH FUNCTION-PRINLENGTH))
  (SETQ FUNCTION (FUNCTION-NAME (RP-FUNCTION-WORD RP FRAME)))
  (CATCH-ERROR (FORMAT T "~%~S:" FUNCTION) NIL)
  (AND (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
       (FORMAT T " (P.C. = ~O)"		;Note that this displays the return-pc,
	       (RP-EXIT-PC RP FRAME)))	; which is one greater than the D-LAST.  
  (TERPRI)
  (PRINT-FRAME-ARGS SG FRAME 3))

;; Returns T if it displayed the rest arg, NIL otherwise.  See SHOW-ALL-MACRO.
(DEFUN PRINT-FRAME-ARGS (SG FRAME INDENT
			 &AUX (PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
			      (PRINLENGTH ERROR-MESSAGE-PRINLENGTH)
			      FUNCTION NARGS-SUPPLIED NARGS-TO-PRINT
			      (RP (SG-REGULAR-PDL SG))
			      NARGS-EXPECTED NARGS-REQUIRED ACTIVE-FLAG
			      LEXPR-CALL REST-ARG-P REST-ARG-VALUE)
  (SETQ FUNCTION (RP-FUNCTION-WORD RP FRAME)
	NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP FRAME))
  (SETQ ACTIVE-FLAG (SG-FRAME-ACTIVE-P SG FRAME))
  (COND (ACTIVE-FLAG
	 (COND ((LEGITIMATE-FUNCTION-P FUNCTION)
		(SETQ NARGS-REQUIRED
		      (LDB %%ARG-DESC-MIN-ARGS (ARGS-INFO FUNCTION)))
		(SETQ NARGS-EXPECTED
		      (LDB %%ARG-DESC-MAX-ARGS (ARGS-INFO FUNCTION)))))
	 (MULTIPLE-VALUE (REST-ARG-VALUE REST-ARG-P LEXPR-CALL)
	   (SG-REST-ARG-VALUE SG FRAME))
	 (SETQ NARGS-TO-PRINT (SG-NUMBER-OF-SPREAD-ARGS SG FRAME)))
	(T
	 (PRINC "  Frame still accumulating args;
  possible args or stack temps follow:
")
	 (LET ((TEM (SG-NEXT-OPEN SG FRAME)))
	   (SETQ NARGS-TO-PRINT (- (IF TEM (- TEM 4)
				       (SG-REGULAR-PDL-POINTER SG))
				   FRAME)
		 NARGS-SUPPLIED NARGS-TO-PRINT))))
  ;; Print the individual args.
  (DOTIMES (I NARGS-TO-PRINT)
    (AND (= I NARGS-SUPPLIED)
	 (IF (AND NARGS-REQUIRED (< I NARGS-REQUIRED)) (FORMAT T "   --Missing args:--~%")
	     (FORMAT T "   --Defaulted args:--~%"))) ;These "args" weren't supplied
    (AND NARGS-REQUIRED (< NARGS-SUPPLIED NARGS-REQUIRED) (= I NARGS-REQUIRED)
	 (FORMAT T "   --Optional args:--~%"))	;End of truly missing args
    (AND NARGS-EXPECTED (= I NARGS-EXPECTED)    ;Called with too many args
	 (FORMAT T "   --Extraneous args:--~%"))
    (FORMAT T "~VTArg ~D" INDENT I)
    (AND ACTIVE-FLAG (DISPLAY-ARG-NAME " (~A)" FUNCTION I))
    ;; Print the arg value unless the arg is missing (val is garbage).
    (COND ((NOT (AND NARGS-REQUIRED
		     (> NARGS-REQUIRED NARGS-SUPPLIED)
		     ( I NARGS-SUPPLIED)))
	   (PRINC ": ")
	   (CATCH-ERROR (PRIN1 (SG-FRAME-ARG-VALUE SG FRAME I))) NIL))
    (TERPRI))
  ;; Print the rest arg if any.
  (COND (REST-ARG-P
	 (FORMAT T "~VTRest arg" INDENT)
	 (DISPLAY-LOCAL-NAME " (~A)" FUNCTION 0)
	 (PRINC ": "))
	(LEXPR-CALL
	 (FORMAT T "~VTExtraneous Rest Arg: " INDENT)))
  (COND ((OR REST-ARG-P LEXPR-CALL)
	 (CATCH-ERROR (PRIN1 REST-ARG-VALUE) NIL)
	 (TERPRI)))
  REST-ARG-P)

(DEFUN DISPLAY-LOCAL-NAME (FORMAT-STRING FUNCTION LOCALNO &AUX NAME)
  (SETQ NAME (LOCAL-NAME FUNCTION LOCALNO))
  (AND NAME (FORMAT T FORMAT-STRING NAME)))

(DEFUN DISPLAY-ARG-NAME (FORMAT-STRING FUNCTION ARGNO &AUX NAME)
  (SETQ NAME (ARG-NAME FUNCTION ARGNO))
  (AND NAME (FORMAT T FORMAT-STRING NAME)))

;; Basic commands for moving between stack frames.
;; UP means closer to the top of the stack, DOWN means the base of the stack.

;; Control-P, <^>
(DEFUN COM-UP-STACK (SG IGNORE &OPTIONAL COUNT SHOW-ALL-FLAG REVERSE-FLAG UNINTERESTING-FLAG
			       &AUX FRAME COUNT1)
  (SETQ COUNT1 (OR COUNT 1))
  (AND REVERSE-FLAG (SETQ COUNT1 (- COUNT1)))
  (SETQ FRAME (FUNCALL (IF UNINTERESTING-FLAG #'SG-NEXT-NTH-OPEN
			   #'SG-NEXT-NTH-INTERESTING-ACTIVE)
		       SG CURRENT-FRAME COUNT1))
  (COND ((= FRAME CURRENT-FRAME)
	 (FORMAT T (COND (REVERSE-FLAG
			  "You are already at the bottom of the stack.~%")
			 (T "You are already at the top of the stack.~%"))))
	(T (SETQ CURRENT-FRAME FRAME)
	   (COND ((NOT SHOW-ALL-FLAG) (SHOW-FUNCTION-AND-ARGS SG))
		 (T (SHOW-ALL SG)))))
  NIL)

;; Control-N, <line>
(DEFUN COM-DOWN-STACK (SG ETE &OPTIONAL COUNT)
  (COM-UP-STACK SG ETE COUNT NIL T))

;; Meta-P.
(DEFUN COM-UP-STACK-ALL (SG ETE &OPTIONAL COUNT)
  (COM-UP-STACK SG ETE COUNT T))

;; Meta-N.
(DEFUN COM-DOWN-STACK-ALL (SG ETE &OPTIONAL COUNT)
  (COM-UP-STACK SG ETE COUNT T T))

;; Meta->.
(DEFUN COM-TOP-STACK (SG &REST IGNORE)
  (SETQ CURRENT-FRAME (SG-OUT-TO-INTERESTING-ACTIVE SG INNERMOST-VISIBLE-FRAME))
  (SHOW-FUNCTION-AND-ARGS SG)
  NIL)

;; Meta-<.
(DEFUN COM-BOTTOM-STACK (SG &REST IGNORE)
  (SETQ CURRENT-FRAME
	(DO ((FRAME INNERMOST-VISIBLE-FRAME (SG-PREVIOUS-ACTIVE SG FRAME))
	     (PREV-FRAME NIL FRAME))
	    ((NULL FRAME) PREV-FRAME)))
  (SHOW-FUNCTION-AND-ARGS SG)
  NIL)

;; Control-Meta-P.
(DEFUN COM-UP-STACK-UNINTERESTING (SG ETE &OPTIONAL COUNT)
  (COM-UP-STACK SG ETE COUNT NIL NIL T))

;; Control-Meta-N.
(DEFUN COM-DOWN-STACK-UNINTERESTING (SG ETE &OPTIONAL COUNT)
  (COM-UP-STACK SG ETE COUNT NIL T T))

;; Control-Meta-U.
(DEFUN COM-UP-TO-INTERESTING (SG IGNORE &OPTIONAL IGNORE)
  (SETQ CURRENT-FRAME (SG-OUT-TO-INTERESTING-ACTIVE SG CURRENT-FRAME))
  (SHOW-FUNCTION-AND-ARGS SG)
  NIL)

;; Control-L, form.
(DEFUN COM-CLEAR-AND-SHOW (SG ETE &REST IGNORE)
  (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
  (PRINT-ERROR-MESSAGE SG ETE)
  (SHOW-FUNCTION-AND-ARGS SG)
  NIL)

;; Meta-L.
(DEFUN COM-CLEAR-AND-SHOW-ALL (SG &REST IGNORE)
  (SHOW-ALL SG)
  NIL)

;; Control-S.
(DEFUN COM-SEARCH (SG IGNORE &OPTIONAL IGNORE FLAG &AUX KEY FRAME)
  (FORMAT T "~%String to search for (end with RETURN):~%")
  (SETQ KEY (READLINE))
  (SETQ FRAME 
	(DO ((FRAME INNERMOST-VISIBLE-FRAME (SG-PREVIOUS-ACTIVE SG FRAME))
	     (RP (SG-REGULAR-PDL SG))
	     (NAME)
	     )
	    ((NULL FRAME) NIL)
	  (SETQ NAME (FUNCTION-NAME (RP-FUNCTION-WORD RP FRAME)))
	  (SETQ NAME
		(COND ((STRINGP NAME) NAME)
		      ((SYMBOLP NAME) (STRING NAME))
		      (T (FORMAT NIL "~S" NAME))))
	  (AND (STRING-SEARCH KEY NAME)
	       (RETURN FRAME))))
  (COND ((NULL FRAME)
	 (FORMAT T "Search failed.~%"))
	(T
	 (SETQ CURRENT-FRAME FRAME)
	 (COND ((NOT FLAG) (SHOW-FUNCTION-AND-ARGS SG))
	       (T (SHOW-ALL SG))))))

(DEFUN COM-SEARCH-AND-SHOW-ALL (SG ETE &OPTIONAL (COUNT 1))
  (COM-SEARCH SG ETE COUNT T))

;; The guts of the commands on the previous page.

;; This is how the error message is printed when the error handler starts up.
(DEFUN SHOW (SG ETE &REST IGNORE)
  (TERPRI)
  (PRINT-ERROR-MESSAGE SG ETE)
  (SHOW-FUNCTION-AND-ARGS SG)
  NIL)

;; SHOW-FUNCTION-AND-ARGS is regular printing tty stuff.
;; SHOW-ALL clears the screen and then fills it up.
(DEFUN SHOW-FUNCTION-AND-ARGS (SG)
  (PRINT-FUNCTION-AND-ARGS SG CURRENT-FRAME))

(DEFUN SHOW-ALL (SG &AUX RP FUNCTION)
  (SETQ RP (SG-REGULAR-PDL SG)
	FUNCTION (RP-FUNCTION-WORD RP CURRENT-FRAME))
  (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
  ;; Print the header, including the function name
  (FORMAT T "Frame address ~O" CURRENT-FRAME)
  (IF (NOT (ZEROP (RP-ADI-PRESENT RP CURRENT-FRAME)))
      (SHOW-ADI RP (- CURRENT-FRAME 4)))
  (TERPRI)
  (SELECT (%DATA-TYPE FUNCTION)
    (DTP-FEF-POINTER (SHOW-ALL-MACRO SG RP FUNCTION))
    (OTHERWISE (SHOW-FUNCTION-AND-ARGS SG))))
	  
(DEFUN SHOW-ALL-MACRO (SG RP FUNCTION
		       &AUX N-LOCALS PC-NOW NAME REST-ARG-PRINTED NLINES WHERE LIM-PC)
  (SETQ N-LOCALS (FEF-NUMBER-OF-LOCALS FUNCTION)
	NAME (FEF-NAME FUNCTION)
	PC-NOW (RP-EXIT-PC RP CURRENT-FRAME)
	LIM-PC (COMPILER:DISASSEMBLE-LIM-PC FUNCTION))
  (FORMAT T "~%~s~2%" NAME)
  ;; Print the arguments, including the rest-arg which is the first local
  (SETQ REST-ARG-PRINTED (PRINT-FRAME-ARGS SG CURRENT-FRAME 0))
  (COND ((SG-FRAME-ACTIVE-P SG CURRENT-FRAME)
	 ;; Print the rest of the locals -- if the frame is active.
	 (DOTIMES (I N-LOCALS)
	   (COND ((NOT (AND REST-ARG-PRINTED (ZEROP I)))	;Don't show rest arg twice
		  (FORMAT T "Local ~D" I)
		  (DISPLAY-LOCAL-NAME " (~A)" FUNCTION I)
		  (LET ((PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
			(PRINLENGTH ERROR-MESSAGE-PRINLENGTH))
		    (FORMAT T ": ~S~%" (SG-FRAME-LOCAL-VALUE SG CURRENT-FRAME I))))))
	 (FORMAT T "~%Disassembled code:")
	 ;; Figure out how many instructions will fit in the stream we are using.
	 (SETQ NLINES
	       (MAX DISASSEMBLE-INSTRUCTION-COUNT	;don't show absurdly few
		    (COND ((MEMQ ':SIZE-IN-CHARACTERS (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))
			   (MULTIPLE-VALUE (NIL NLINES)
			     (FUNCALL STANDARD-OUTPUT ':SIZE-IN-CHARACTERS))
			   (MULTIPLE-VALUE (NIL WHERE)
			     (FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS ':CHARACTER))
			   (- NLINES WHERE 2))	;Leave 1 line for prompt, 1 for extra terpri
			  (T 0))))		;Don't know size of window, use default count
	 (DO ((I 0 (1+ I))
	      (PC (MAX (FEF-INITIAL-PC FUNCTION) (- PC-NOW (// NLINES 2)))
		  (+ PC (COMPILER:DISASSEMBLE-INSTRUCTION-LENGTH FUNCTION PC))))
	     ((OR ( I NLINES) ( PC LIM-PC))
	      (COND ((= PC PC-NOW)		;If arrow should point after all code,
		     (TERPRI) (PRINC "=> "))))
	   (TERPRI)
	   (PRINC (IF (= PC PC-NOW) "=> " "   "))
	   (COMPILER:DISASSEMBLE-INSTRUCTION FUNCTION PC))
	 ;; This kludge is to prevent the prompt from triggering a **MORE** when it comes out
	 ;; on the bottom line of the window
	 (IF (MEMQ ':NOTICE (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))
	     (FUNCALL STANDARD-OUTPUT ':NOTICE ':INPUT-WAIT)))))

(DEFUN SHOW-ADI (RP IDX)
  (FORMAT T "~2%Additional information supplied with call:")
  (DO ((TYPE (LDB SI:%%ADI-TYPE (AR-1 RP IDX)))
       (MORE-P (%P-LDB %%ADI-PREVIOUS-ADI-FLAG (AP-1 RP (1- IDX)))))
      (())
    (SELECT TYPE
      (SI:ADI-RETURN-INFO
       (FORMAT T "~%M.V. return info: storing-option ~S values expecting ~S"
	       (NTH (LDB SI:%%ADI-RET-STORING-OPTION (AR-1 RP IDX)) SI:ADI-STORING-OPTIONS)
	       (LDB SI:%%ADI-RET-NUM-VALS-EXPECTING (AR-1 RP IDX))))
      (SI:ADI-BIND-STACK-LEVEL
       (FORMAT T "~%Binding stack level: ~S" (LDB 0024 (AR-1 RP IDX))))
      (SI:ADI-RESTART-PC
       (FORMAT T "~%Restart PC on *THROW: ~S" (LDB 0024 (AR-1 RP IDX))))
      (OTHERWISE
       (FORMAT T "~%~S" (NTH TYPE SI:ADI-KINDS))))
    (IF (ZEROP MORE-P) (RETURN) (SETQ IDX (- IDX 2)))))

;Commands for looking at special bindings.

;Control-Meta-S: Print names and values of specials bound by this frame.
(DEFUN COM-PRINT-FRAME-BINDINGS (SG ETE &AUX START END)
  ETE
  ;; Find range of special pdl for this frame together with
  ;; all uninteresting frames called by it.
  (DO ((NEXT-INT (SG-NEXT-INTERESTING-ACTIVE SG CURRENT-FRAME))
       (NEXT CURRENT-FRAME (SG-NEXT-ACTIVE SG NEXT)))
      ((EQUAL NEXT NEXT-INT))
    (MULTIPLE-VALUE-BIND (MAYBE-START MAYBE-END)
	(SG-FRAME-SPECIAL-PDL-RANGE SG NEXT)
      (OR START (SETQ START MAYBE-START))
      (AND MAYBE-END (SETQ END (1+ MAYBE-END)))))
  (COND (START
	 (FORMAT T "~&Names and values of specials bound in this frame:~%")
	 (DO ((I START (+ I 2))
	      (SP (SG-SPECIAL-PDL SG)))
	     (( I END))
	   (TERPRI)
	   (PRIN1 (SYMBOL-FROM-VALUE-CELL-LOCATION (AREF SP (1+ I))))	;Name
	   (PRINC ": ")
	   (MULTIPLE-VALUE-BIND (VAL ERROR)				;Value
	       (CATCH-ERROR (PRIN1 (AREF SP I)) NIL)
		     (IF ERROR (PRINC "unbound") VAL))))
	(T (FORMAT T "~&No specials bound in this frame"))))

;Meta-S: Print the value as seen in the current frame (at the point at which
;it called out or erred) of a specified variable.
(DEFUN COM-PRINT-VARIABLE-FRAME-VALUE (SG ETE)
  ETE
  (TERPRI)
  (PRINC "Value in this frame of special variable ")
  (LET ((VAR (SI:READ-FOR-TOP-LEVEL)))
    (MULTIPLE-VALUE-BIND (VALUE BOUNDFLAG)
	(SYMEVAL-IN-STACK-GROUP VAR SG CURRENT-FRAME)
      (TERPRI)
      (IF BOUNDFLAG (PRIN1 (SETQ * VALUE)) (PRINC (SETQ * "Unbound"))))))

;; Other informational commands.

;; Control-A.
(DEFUN COM-ARGLIST (SG &REST IGNORE)
  (LET ((FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) CURRENT-FRAME)))
    (FORMAT T "~&Argument list for ~S is ~A.~%"
	    (FUNCTION-NAME FUNCTION)
	    (ARGLIST FUNCTION)))
  NIL)

;; Control-Meta-A
(DEFUN COM-GET-ARG (SG IGNORE &OPTIONAL (ARG 0))
  (MULTIPLE-VALUE (* +)
    (SG-FRAME-ARG-VALUE SG CURRENT-FRAME ARG))
  (FORMAT T "~&~S" *))

;; Control-Meta-L
(DEFUN COM-GET-LOCAL (SG IGNORE &OPTIONAL (ARG 0))
  (MULTIPLE-VALUE (* +)
    (SG-FRAME-LOCAL-VALUE SG CURRENT-FRAME ARG))
  (FORMAT T "~&~S" *))

;; c-m-F
(DEFUN COM-GET-FUNCTION (SG IGNORE &OPTIONAL IGNORE)
  (SETQ * (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) CURRENT-FRAME))
  (FORMAT T "~&~S" *))

;; Control-E
(DEFUN COM-EDIT-FRAME-FUNCTION (SG &REST IGNORE)
  (LET ((RP (SG-REGULAR-PDL SG)))
    (ED (FUNCTION-NAME (RP-FUNCTION-WORD RP CURRENT-FRAME)))))

;; ?, <help>
(DEFUN COM-HELP (SG ETE &REST IGNORE &AUX TEM)
  (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
  (FUNCALL STANDARD-OUTPUT ':STRING-OUT
"You are in the error handler.  If you type in a Lisp form, it will be
evaluated, and the result printed.  Type <Help> to print this text.
Type <Abort> or C-Z to get back to top level, or the previous error handler.
While in the error handler, C-G quits back to the error handler top level.
Other commands are...

Selecting stack frames:
C-N or <Line> goes down a frame, C-P or <Return> goes up.
M-N and M-P are similar but show args, locals and compiled code.
C-M-N and C-M-P are similar to C-N and C-P, but they show
   all the internal EVALs, PROGs, CONDs, etc. of interpreted code,
   and function calls whose args are still being computed.
M-< and M-> go to the top and bottom of the stack, respectively.
C-S followed by a substring of a function name searches down the stack
   for a frame calling a function whose name contains that substring.

Information display:
C-L or <Clear-Screen> clears screen and retypes info,
M-L clears screen and types args, locals and compiled code.
C-B gives a backtrace of function names.
M-B gives a backtrace of function names and argument names and values.
C-M-B is line M-B but shows EVALs, PROGs, CONDs, etc.
C-M-A prints an argument to the current function, and sets * to be that
   argument to let you do more complicated things with it.
   + is set to a locative to that argument, should you want to modify it.
   To specify which argument, type the argument number with Control
   or Meta held down before the C-M-A.
C-M-L is like C-M-A but works on the function's locals rather than the args.
C-M-F does likewise for the function itself.
C-A prints the arglist of the function in the current frame.
M-S prints the value in this frame of a special variable you specify.
C-M-S lists all special variable bindings in this frame.

Transfer to other systems:
C-E calls the editor to edit the current function.
C-M-W switches to the window-based error handler.

Stepping commands:
C-X toggles the trap-on-exit flag for the current frame.
M-X sets the trap-on-exit flag for the current frame and all outer frames.
C-M-X clears this flag for the current frame and all outer frames.
Trap on exit also occurs if the frame is thrown through.
C-D proceeds like C-C, but first sets the trap-on-next-function-call flag.
M-D toggles the trap-on-next-function-call flag.
Functions which get a trap on entry are automatically flagged for
trap on exit as well.  You can un-flag them with C-X.

Commands to continue execution:
")
  (COND ((SETQ TEM (OR (AND (EQ (FIRST ETE) 'FERROR) (GET (FOURTH ETE) 'HELP-MESSAGE))
		       (GET (FIRST ETE) 'HELP-MESSAGE)))
	 (IF (STRINGP TEM)
	     (FUNCALL STANDARD-OUTPUT ':STRING-OUT TEM)
	     (FUNCALL TEM SG ETE))
	 (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)))
  (FUNCALL STANDARD-OUTPUT ':STRING-OUT
"C-Z aborts to previous error handler or to top level.
C-R returns a value from the current frame.
C-M-R offers to reinvoke the current frame with the same arguments
   originally supplied (as best as they can be determined).
C-T throws to a specific tag.
C-M-C continues from an ERROR-RESTART special form."
  ))

;; Commands for resuming execution.

;; Control-Z.
(DEFUN COM-TOP-LEVEL-THROW (SG ETE &OPTIONAL (COUNT 1))
  ETE COUNT
  (LEAVING-ERROR-HANDLER)
  (COND ((NULL ERROR-HANDLER-RUNNING)
	 ;; Explicitly invoked error handler => return from it.
	 (*THROW 'EXIT NIL))
	((EQ SG SI:SCHEDULER-STACK-GROUP)
	 (FORMAT T "~&Restarting the scheduler.")
	 (COND (REAL-CURRENT-PROCESS
		(SI:PROCESS-BLAST REAL-CURRENT-PROCESS)
		(FORMAT T "~%Blasting ~S so this won't happen again" REAL-CURRENT-PROCESS)))
	 (STACK-GROUP-PRESET SG #'SI:PROCESS-SCHEDULER)
	 (SG-RUN-GOODBYE SG))
	(T
	 (COND ((AND (NEQ SG (PROCESS-INITIAL-STACK-GROUP CURRENT-PROCESS))
		     (NOT (MEMQ SG (FUNCALL CURRENT-PROCESS ':COROUTINE-STACK-GROUPS)))
		     (NOT (SYMEVAL-IN-STACK-GROUP 'ERROR-HANDLER-RUNNING SG)))
		;; Running in a random stack group, get rid of it then throw in the
		;; initial stack group, since SG probably doesn't have a catch for cmd-level.
		(UNWIND-SG SG %CURRENT-STACK-GROUP NIL NIL)
		(SETQ SG (PROCESS-INITIAL-STACK-GROUP CURRENT-PROCESS))))
	 ;; Prevent any trap-on-exits from this throw.
	 (DO ((FRAME INNERMOST-VISIBLE-FRAME (SG-PREVIOUS-ACTIVE SG FRAME)))
	     ((NULL FRAME))
	   (SETF (RP-TRAP-ON-EXIT (SG-REGULAR-PDL SG) FRAME) 0))
	 (SG-THROW SG 'SYS:COMMAND-LEVEL NIL)))
  NIL)

;; Control-T.
(DEFUN COM-THROW (SG &REST IGNORE TAG VAL)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL)))
  (FORMAT T "Throw a value to a tag.~%")
  (SETQ TAG (READ-OBJECT "What is the tag?")
	VAL (READ-OBJECT "What value do you wish to throw?"))
  (LEAVING-ERROR-HANDLER)
  (SETF (RP-TRAP-ON-EXIT (SG-REGULAR-PDL SG) INNERMOST-VISIBLE-FRAME) 0)
  (SG-THROW SG TAG VAL)
  NIL)

;; Control-R.
(DEFUN COM-RETURN-A-VALUE (SG &REST IGNORE &AUX VALUE)
  (COND ((NULL ERROR-HANDLER-RUNNING)
	 (FORMAT T "You can only examine this stack group, not modify it."))
	((NOT (SG-FRAME-ACTIVE-P SG CURRENT-FRAME))
	 (FORMAT T "This frame has not yet been activated; you cannot return from it."))
	((NULL (SG-PREVIOUS-ACTIVE SG CURRENT-FRAME))
	 (FORMAT T "This is the bottom frame; you cannot return from it."))
	(T (FORMAT T "Return a value from the function ~S.~%"
		   (FUNCTION-NAME (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) CURRENT-FRAME)))
	   (SETQ VALUE (READ-OBJECT "Form to evaluate and return:"))
	   (LEAVING-ERROR-HANDLER)
	   (SETF (RP-TRAP-ON-EXIT (SG-REGULAR-PDL SG) INNERMOST-VISIBLE-FRAME) 0)
	   (SG-UNWIND-TO-FRAME SG CURRENT-FRAME T VALUE)))
  NIL)

;; Meta-R.
(DEFUN COM-RETURN-MANY-VALUES (&REST IGNORE)
);WRITE THIS

;; Control-Meta-R
(DEFUN COM-RETURN-REINVOCATION (SG &REST IGNORE
				&AUX FORM (PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
					  (PRINLENGTH ERROR-MESSAGE-PRINLENGTH))
  (COND ((NULL ERROR-HANDLER-RUNNING)
	 (FORMAT T "You can only examine this stack group, not modify it."))
	((NOT (SG-FRAME-ACTIVE-P SG CURRENT-FRAME))
	 (FORMAT T "This frame's args are still being computed;
their values are not known, to re-evaluate with."))
	((FQUERY '(:LIST-CHOICES NIL :FRESH-LINE NIL)
		 " Re-evaluating ~S, OK? "
		 (SETQ FORM (GET-FRAME-FUNCTION-AND-ARGS SG CURRENT-FRAME)))
	 (SETF (RP-TRAP-ON-EXIT (SG-REGULAR-PDL SG) INNERMOST-VISIBLE-FRAME) 0)
	 (SG-UNWIND-TO-FRAME-AND-REINVOKE SG CURRENT-FRAME FORM)
	 (LEAVING-ERROR-HANDLER)
	 (WITHOUT-INTERRUPTS
	   (AND ERROR-HANDLER-RUNNING
		(FREE-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP))
	   (STACK-GROUP-RESUME SG NIL)))))

;; Control-C.
(DEFUN COM-PROCEED (SG ETE &REST IGNORE &AUX TEM OBJ)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL)))
  (COND ((AND (EQ (CAR ETE) 'FERROR) (CADR ETE))
	 (SETQ OBJ (COND ((SETQ TEM (GET (FOURTH ETE) 'PROCEED))	;Condition's property
			  (FUNCALL TEM SG ETE))
			 (T
			  (AND (EQ (CADR ETE) T)
			       (READ-OBJECT
				 "Form to evaluate and return from FERROR//CERROR:")))))
	 ;; Now restart the stack group, causing CERROR to return OBJ
	 (PROCEED-SG SG OBJ))
	(T
	 (LET ((FUNCTION (GET (FIRST ETE) 'PROCEED)))
	   (COND ((NULL FUNCTION)
		  (FORMAT T "There is no way to proceed from this error.~%"))
		 (T ;; Call the specific proceed routine.  It should call RESTART
		  ;; to mung the micro-stack appropriately.
		  (FUNCALL FUNCTION SG ETE)
		  ;; Now restart the stack group, as that routine left it.
		  (SETF (SG-CURRENT-STATE SG) SG-STATE-RESUMABLE)
		  (PROCEED-SG SG))))))
  NIL)

(DEFPROP FERROR "C-C returns a specified value from FERROR or CERROR" HELP-MESSAGE)

;; Meta-C.
(DEFUN COM-BASH-AND-PROCEED (SG ETE &REST IGNORE)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL)))
  (LET ((FUNCTION (GET (FIRST ETE) 'BASH-AND-PROCEED)))
    (COND ((NULL FUNCTION)
	   (FORMAT T "There is no way to bash-and-proceed from this error.~%"))
	  (T ;; Call the specific proceed routine.  It should call RESTART
	   ;; to mung the micro-stack appropriately.
	   (FUNCALL FUNCTION SG ETE)
	   ;; Now restart the stack group, as that routine left it.
	   (SETF (SG-CURRENT-STATE SG) SG-STATE-RESUMABLE)
	   (PROCEED-SG SG))))
  NIL)

;; Comtrol-meta-C.
(DEFUN COM-ERROR-RESTART (SG &REST IGNORE)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL)))
  (COND ((OR CONDITION-PROCEED-FLAG
	     (Y-OR-N-P "Are you SURE you want to restart? "))
	 (SETF (RP-TRAP-ON-EXIT (SG-REGULAR-PDL SG) INNERMOST-VISIBLE-FRAME) 0)
	 (SG-THROW SG 'ERROR-RESTART NIL)))
  (FORMAT T "Flushed.~%"))

;Stepping commands.

;C-X: Control the trap-on-exit bits of frames.
(DEFUN COM-TOGGLE-FRAME-TRAP-ON-EXIT (SG IGNORE &OPTIONAL IGNORE)
  (LET ((RP (SG-REGULAR-PDL SG)))
    (SETF (RP-TRAP-ON-EXIT RP CURRENT-FRAME)
	  (LOGXOR 1 (RP-TRAP-ON-EXIT RP CURRENT-FRAME)))
    (TERPRI)
    (PRINC (IF (ZEROP (RP-TRAP-ON-EXIT RP CURRENT-FRAME))
	       "Do not break"
	       "Break"))
    (PRINC " on exit from this frame.")))

;Meta-X
(DEFUN COM-SET-ALL-FRAMES-TRAP-ON-EXIT (SG IGNORE &OPTIONAL IGNORE)
  (DO ((FRAME CURRENT-FRAME (SG-PREVIOUS-ACTIVE SG FRAME)))
      ((NULL FRAME))
    (SETF (RP-TRAP-ON-EXIT (SG-REGULAR-PDL SG) FRAME) 1))
  (FORMAT T "~%Break on exit from this frame and all outer active frames."))

;Control-Meta-X
(DEFUN COM-CLEAR-ALL-FRAMES-TRAP-ON-EXIT (SG IGNORE &OPTIONAL IGNORE)
  (DO ((FRAME CURRENT-FRAME (SG-PREVIOUS-OPEN SG FRAME)))
      ((NULL FRAME))
    (SETF (RP-TRAP-ON-EXIT (SG-REGULAR-PDL SG) FRAME) 0))
  (FORMAT T "~%Do not break on exit from this frame and all outer frames."))

;Control-D: Proceed, and trap next function call.
(DEFUN COM-PROCEED-TRAP-ON-CALL (SG ETE &OPTIONAL IGNORE)
  (SETF (SG-FLAGS-TRAP-ON-CALL SG) 1)
  (FORMAT T "Trap on next function call. ")
  (COM-PROCEED SG ETE))

;Meta-D: Toggle whether to trap on next function call.
(DEFUN COM-TOGGLE-TRAP-ON-CALL (SG IGNORE &OPTIONAL IGNORE)
  (SETF (SG-FLAGS-TRAP-ON-CALL SG)
	(LOGXOR 1 (SG-FLAGS-TRAP-ON-CALL SG)))
  (TERPRI)
  (PRINC (IF (ZEROP (SG-FLAGS-TRAP-ON-CALL SG))
	     "Do not break"
	     "Break"))
  (PRINC " on next function call."))
	   
;List of functions which have breakons set.
(DEFVAR BREAKON-FUNCTIONS NIL)

(DEFUN BREAKON (FUNCTION &OPTIONAL (CONDITION T))
  (BREAKON-INIT FUNCTION)
  (SETQ CONDITION (SI:RENAME-WITHIN-NEW-DEFINITION-MAYBE FUNCTION CONDITION))
  (LET* ((DEF (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC FUNCTION 'BREAKON)))
	 (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
	 (DEFN-DATA (CADDR (CADDDR DEF)))	;Find our PROGN, etc.
	 (SLOT-LOC (CADR DEFN-DATA)))	;Within that, find ptr to list of conditions.
    (OR (MEMBER CONDITION (CDR SLOT-LOC)) (PUSH CONDITION (CDR SLOT-LOC))))
  FUNCTION)

(DEFUN UNBREAKON (&OPTIONAL FUNCTION (CONDITION T))
  (LET* ((SPEC1 (AND FUNCTION (SI:UNENCAPSULATE-FUNCTION-SPEC FUNCTION 'BREAKON))))
    (COND ((NULL FUNCTION)
	   (MAPC 'UNBREAKON BREAKON-FUNCTIONS))
	  ((EQ CONDITION T)
	   (FDEFINE SPEC1 (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC SPEC1 '(BREAKON))))
	   (SETQ BREAKON-FUNCTIONS (DELETE FUNCTION BREAKON-FUNCTIONS)))
	  ((NEQ SPEC1 (SI:UNENCAPSULATE-FUNCTION-SPEC SPEC1 '(BREAKON)))
	   (LET* ((DEF (FDEFINITION SPEC1))
		  (DEFN-DATA (CADDR (CADDDR DEF)))	;Find our PROGN, etc.
		  (SLOT-LOC (CADR DEFN-DATA)))	;Within that, find ptr to list of conditions.
	     (SETF (CDR SLOT-LOC)
		   (DELETE CONDITION (CDR SLOT-LOC)))
	     (COND ((NULL (CDR SLOT-LOC))
		    (FDEFINE SPEC1
			     (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC SPEC1 '(BREAKON))))
		    (SETQ BREAKON-FUNCTIONS (DELETE FUNCTION BREAKON-FUNCTIONS)))))))))

;; Make a specifed function into an broken-on function
;; (with no conditions yet) if it isn't one already.
(DEFUN BREAKON-INIT (FUNCTION)
  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
	(SPEC1 (SI:UNENCAPSULATE-FUNCTION-SPEC FUNCTION 'BREAKON)))
    (COND ((EQ SPEC1 (SI:UNENCAPSULATE-FUNCTION-SPEC SPEC1 '(BREAKON)))
	   (SI:ENCAPSULATE SPEC1 FUNCTION 'BREAKON
			   ;; Must cons the (OR) afresh -- it gets RPLAC'd.
			   `(BREAKON-THIS-TIME ,(LIST 'OR)
					       ,SI:ENCAPSULATED-FUNCTION
					       ARGLIST))
	   (PUSH FUNCTION BREAKON-FUNCTIONS)))))

(DEFUN BREAKON-THIS-TIME (BREAK-CONDITION FUNCTION ARGS)
  (AND BREAK-CONDITION (SETF (LDB %%M-FLAGS-TRAP-ON-CALL %MODE-FLAGS) 1))
  ;; The next call ought to be the function the user is trying to call.
  ;; That will be so only if this function is compiled.
  (LEXPR-FUNCALL FUNCTION ARGS))

;; This is the dispatch table of error handler commands, a 4 by 240 array.
(DEFVAR COMMAND-DISPATCH-TABLE)
;; This is a list, setq'd in this file, from which COMMAND-DISPATCH-TABLE is initialized.
(DEFVAR COMMAND-DISPATCH-LIST)
;; This file will set COMMAND-DISPATCH-LIST make sure COMMAND-DISPATCH-TABLE is recomputed
;; from it by ASSURE-DISPATCH-SET-UP.
(MAKUNBOUND 'COMMAND-DISPATCH-TABLE)

(DEFUN ASSURE-DISPATCH-SET-UP ()
  (COND ((NOT (BOUNDP 'COMMAND-DISPATCH-TABLE))
	 (SETQ COMMAND-DISPATCH-TABLE (MAKE-ARRAY NIL 'ART-Q '(4 240)))
	 (DOLIST (X COMMAND-DISPATCH-LIST)
	   (LET ((CHAR (CAR X))
		 (COM (CADR X))
		 (REPEAT (CADDR X)))
	     (LET ((I (LDB %%KBD-CONTROL-META CHAR)) (J (LDB %%KBD-CHAR CHAR)))
	       (DOTIMES (N (OR REPEAT 1))
		 (ASET COM COMMAND-DISPATCH-TABLE I J)
		 (SETQ J (1+ J)))))))))

;; The initial dispatch table.
(SETQ COMMAND-DISPATCH-LIST '(
       (#/? COM-HELP)
       (#\HELP COM-HELP)
       (#\LINE COM-DOWN-STACK)
       (#\FORM COM-CLEAR-AND-SHOW)
       (#\RETURN COM-UP-STACK)
       (#\RESUME COM-PROCEED)
       (#\ABORT COM-TOP-LEVEL-THROW)

       (#/0 COM-NUMBER 10.)		;control-digits
       (#/A COM-ARGLIST)
       (#/B COM-SHORT-BACKTRACE)
       (#/C COM-PROCEED)
       (#/D COM-PROCEED-TRAP-ON-CALL)
       (#/E COM-EDIT-FRAME-FUNCTION)
       (#/L COM-CLEAR-AND-SHOW)
       (#/N COM-DOWN-STACK)
       (#/P COM-UP-STACK)
       (#/R COM-RETURN-A-VALUE)
       (#/S COM-SEARCH)
       (#/T COM-THROW)
       (#/X COM-TOGGLE-FRAME-TRAP-ON-EXIT)
       (#/Z COM-TOP-LEVEL-THROW)

       (#/0 COM-NUMBER 10.)		;meta-digits
       (#/< COM-TOP-STACK)
       (#/> COM-BOTTOM-STACK)
       (#/B FULL-BACKTRACE)
       (#/C COM-BASH-AND-PROCEED)
       (#/D COM-TOGGLE-TRAP-ON-CALL)
       (#/L COM-CLEAR-AND-SHOW-ALL)
       (#/N COM-DOWN-STACK-ALL)
       (#/P COM-UP-STACK-ALL)
       (#/R COM-RETURN-MANY-VALUES)
       (#/S COM-PRINT-VARIABLE-FRAME-VALUE)
       (#/X COM-SET-ALL-FRAMES-TRAP-ON-EXIT)

       (#/0 COM-NUMBER 10.)		;control-meta-digits
       (#/A COM-GET-ARG)
       (#/B FULL-BACKTRACE-UNINTERESTING)
       (#/C COM-ERROR-RESTART)
       (#/F COM-GET-FUNCTION)
       (#/L COM-GET-LOCAL)
       (#/N COM-DOWN-STACK-UNINTERESTING)
       (#/P COM-UP-STACK-UNINTERESTING)
       (#/R COM-RETURN-REINVOCATION)
       (#/S COM-PRINT-FRAME-BINDINGS)
       (#/U COM-UP-TO-INTERESTING)
       (#/W COM-WINDOW-ERROR-HANDLER)
       (#/X COM-CLEAR-ALL-FRAMES-TRAP-ON-EXIT)

       ))
