;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.16
;;; Reason: Finish 78.13
;;; Written 12/15/81 14:35:06 by MMcM,
;;; while running on Lisp Machine Five from band 5
;;; with System 78.5, ZMail 38.0, microcode 836.



; From file ZMACS > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFUN MAKE-FILE-BUFFER-STREAM (PATHNAME &OPTIONAL (CONCATENATE-P T)
					 &AUX BUFFER ISTREAM)
  (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME *PATHNAME-DEFAULTS*))
  (SETQ BUFFER (FIND-BUFFER-NAMED PATHNAME T))
  (IF (BUFFER-FILE-ID BUFFER)
      (OR CONCATENATE-P (DELETE-INTERVAL BUFFER))
      (LET ((*INTERVAL* NIL))
	(SET-BUFFER-FILE-ID BUFFER T))
      (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)
      (LET ((GENERIC-PATHNAME (FUNCALL PATHNAME ':GENERIC-PATHNAME)))
	(SETF (BUFFER-GENERIC-PATHNAME BUFFER) GENERIC-PATHNAME)
	(INITIALIZE-GENERIC-PATHNAME GENERIC-PATHNAME)))
  (SETQ ISTREAM (INTERVAL-STREAM BUFFER))
  (FUNCALL ISTREAM ':SET-BP (IF (EQ CONCATENATE-P ':POINT) (BUFFER-SAVED-POINT BUFFER)
				(INTERVAL-LAST-BP BUFFER)))
  ISTREAM)

)

; From file ZMACS > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFCOM COM-WRITE-FILE "Write out the buffer to the specified file." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Write File:" (PATHNAME-DEFAULTS)
					   NIL NIL ':WRITE)))
    (SET-BUFFER-PATHNAME PATHNAME)
    (SET-BUFFER-FILE-ID *INTERVAL* NIL)
    (WRITE-FILE-INTERNAL PATHNAME))
  (MAYBE-DISPLAY-DIRECTORY ':WRITE)
  DIS-NONE)

)

; From file ZFIX
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))


(DOLIST (E *EDITORS-WHOSE-MODES-TO-RESET*)
  (LET* ((MLW (SYMEVAL-IN-INSTANCE E '*MODE-LINE-WINDOW*))
	 (MBW (FUNCALL MLW ':SEARCH-MINI-BUFFER-WINDOW)))
    (TV:BLINKER-SET-VISIBILITY (WINDOW-POINT-BLINKER MBW) NIL)
    (SETF (WINDOW-POINT-BLINKER MBW) (TV:MAKE-BLINKER (WINDOW-SHEET MBW)))))

)

; From file QFILE > LMIO; AI:
#8R FILE-SYSTEM:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))

;;; Check that connection hasn't gone away, making a new one if necessary
(DEFMETHOD (HOST-UNIT :VALIDATE-CONTROL-CONNECTION) (&OPTIONAL NO-ERROR-P)
  (LOCK-HOST-UNIT (SELF)
    (COND ((AND CONTROL-CONNECTION
		(EQ (CHAOS:STATE CONTROL-CONNECTION) 'CHAOS:OPEN-STATE)
		(LOOP FOR DATA-CONN IN DATA-CONNECTIONS
		      ALWAYS (EQ (CHAOS:STATE (DATA-CONNECTION DATA-CONN))
				 'CHAOS:OPEN-STATE)))
	   T)
	  (T
	   (FUNCALL-SELF ':RESET T)	;Arg of T means don't unlock lock
	   (DO (CONN) (NIL)
	     (SETQ CONN (CHAOS:CONNECT HOST *FILE-CONTACT-NAME* *FILE-CONTROL-WINDOW-SIZE*))
	     (COND ((NOT (STRINGP CONN))
		    (SETF (CHAOS:INTERRUPT-FUNCTION CONN) (LET-CLOSED ((HOST-UNIT SELF))
							    'HOST-CHAOS-INTERRUPT-FUNCTION))
		    (SETQ CONTROL-CONNECTION CONN)
		    (FUNCALL HOST ':LOGIN-UNIT SELF T)
		    (RETURN T))
		   (NO-ERROR-P
		    (RETURN NIL)))
	     (CERROR T NIL ':FILE-ERROR "Cannot connect to ~A: ~A" HOST CONN))))))

)

; From file SCREEN > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

;;; Update the mode line if necessary, FORCE says really do it
;;; MODE-LINE-LIST is a list of things to be displayed, whose elements can be:
;;;  a constant string
;;;  a symbol, which is evaluated to either a string or NIL, and printed in the former case
;;;  a list, the CAR of which should be an atom, which is evaluated and the rest of the
;;;    list handled as strings or symbols as above if it is non-NIL (up to any :ELSE), or
;;;    if NIL, anything after a :ELSE in the list.
;;;  eg ("FOOMACS" "(" MODE-NAME ")" (BUFFER-NAMED-P BUFFER-NAME :ELSE "(Null buffer)")
;;;      (FILE-NAME-P FILE-NAME))
;;;  a list starting with the symbol :RIGHT-FLUSH is special:
;;;    the cadr of the list is a string to be displayed flush against the right margin.
;;; As a special hack, if MODE-LINE-LIST is NIL, then the mode line is not changed,
;;;  this is appropriate for things that want to typeout on the prompt-line and then
;;;  invoke the mini-buffer.
;;; PREVIOUS-MODE-LINE is a list of strings that make up the line, since nothing we do
;;;  generates new guys for this, EQness is used to determine if the mode-line has changed
(DEFMETHOD (MODE-LINE-WINDOW-MIXIN :REDISPLAY) (MODE-LINE-LIST &OPTIONAL FORCE)
  (AND FORCE					;If we are going to type things out
       MODE-LINE-LIST				;unless suppressed
       (SETQ PREVIOUS-MODE-LINE NIL))
  (DO ((MODES MODE-LINE-LIST)
       (PREV PREVIOUS-MODE-LINE)
       (L)
       (THING))
      (NIL)
      (COND (L					;Still more to go on a list
	     (POP L THING)
	     (AND (EQ THING ':ELSE)
		  (SETQ L NIL THING NIL)))
	    ((NULL MODES)			;All done with MODE-LINE-LIST
	     (AND PREV (NOT FORCE) (FUNCALL-SELF ':REDISPLAY MODE-LINE-LIST T))
	     (RETURN NIL))
	    (T					;Get next object from MODE-LINE-LIST
	     (POP MODES THING)
	     (COND ((SYMBOLP THING)
		    (SETQ THING (SYMEVAL THING))
		    (AND (LISTP THING)		;If value is a list, dont check CAR
			 (SETQ L THING THING NIL)))
		   ((AND (LISTP THING)		;It's a list,
			 (NEQ (CAR THING) ':RIGHT-FLUSH))
		    (SETQ L THING)
		    (POP L THING)
		    (COND ((NULL (SYMEVAL THING))
			   (DO ()		;Failing conditional, look for :ELSE
			       ((NULL L))
			     (POP L THING)
			     (AND (EQ THING ':ELSE)
				  (RETURN NIL)))))
		    (SETQ THING NIL)))))	;And get stuff next pass
      (AND (SYMBOLP THING) (SETQ THING (SYMEVAL THING)))
      (COND ((NULL THING))
	    ;;THING is now the next string to be put into the mode line
	    (FORCE				;Put it in if consing new one
	     (PUSH THING PREVIOUS-MODE-LINE))
	    ((AND PREV (EQ THING (POP PREV))))	;Still matching?
	    (T					;Different thing,
	     (FUNCALL-SELF ':REDISPLAY MODE-LINE-LIST T)	;do it right this time!
	     (RETURN NIL))))
  (COND (FORCE
	 (SETQ PREVIOUS-MODE-LINE (NREVERSE PREVIOUS-MODE-LINE))
	 (COND (TV:EXPOSED-P
		(TV:SHEET-HOME SELF)
		(TV:SHEET-CLEAR-EOL SELF)
		(*CATCH 'MODE-LINE-OVERFLOW
		  (DOLIST (STR PREVIOUS-MODE-LINE)
		    (AND (STRINGP STR) (FUNCALL-SELF ':STRING-OUT STR))))
		(DOLIST (ELT PREVIOUS-MODE-LINE)
		  (AND (LISTP ELT)
		       (LET* ((STR (CADR ELT))
			      (LEN (TV:SHEET-STRING-LENGTH SELF STR)))
			 (TV:SHEET-SET-CURSORPOS SELF
						 (- (TV:SHEET-INSIDE-WIDTH SELF) LEN)
						 0)
			 (TV:SHEET-CLEAR-EOL SELF)
			 (*CATCH 'MODE-LINE-OVERFLOW
			   (FUNCALL-SELF ':STRING-OUT STR))
			 (RETURN)))))))))

)

; From file SCREEN > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :AFTER :REDISPLAY) (IGNORE &OPTIONAL FORCE)
  (AND FORCE (NOT TV:EXPOSED-P)
       (LET ((LEN (LOOP FOR STR IN PREVIOUS-MODE-LINE
			WHEN (LISTP STR) DO (SETQ STR (SECOND STR))
			SUM (TV:SHEET-STRING-LENGTH SELF STR))))
	 (AND (> LEN (TV:SHEET-INSIDE-WIDTH))
	      (FUNCALL-SELF ':SET-SIZE
			    (MIN (+ TV:LEFT-MARGIN-SIZE LEN TV:RIGHT-MARGIN-SIZE)
				 (TV:SHEET-INSIDE-WIDTH TV:SUPERIOR))
			    TV:HEIGHT))
	 (AND (> (+ TV:X-OFFSET TV:WIDTH) (TV:SHEET-INSIDE-RIGHT TV:SUPERIOR))
	      (FUNCALL-SELF ':SET-POSITION (- (TV:SHEET-INSIDE-RIGHT TV:SUPERIOR) TV:WIDTH)
			    TV:Y-OFFSET)))))

)
