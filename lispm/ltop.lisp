;;; -*-Mode:LISP; Package:SYSTEM-INTERNALS-*-
;;; Initialization & top-level READ-EVAL-PRINT loop

(DECLARE (SPECIAL ERROR-STACK-GROUP %ERROR-HANDLER-STACK-GROUP
		  G P TRACE-LEVEL SYN-TERMINAL-IO + - * // ++ +++ ** ***
		  RUBOUT-HANDLER ORIGINAL-LISP-CRASH-LIST
		  BUILD-INITIAL-OBARRAY-FLAG COLD-INITIALIZATION-LIST WARM-INITIALIZATION-LIST
		  ONCE-ONLY-INITIALIZATION-LIST SYSTEM-INITIALIZATION-LIST
		  SITE-INITIALIZATION-LIST LOGIN-INITIALIZATION-LIST LOGOUT-INITIALIZATION-LIST
		  LISP-TOP-LEVEL-INSIDE-EVAL TV:INITIAL-LISP-LISTENER
		  TV:SYNC-RAM-CONTENTS))

(DEFVAR INITIAL-READTABLE)	;The original one with no changes in it
(DEFVAR STANDARD-READTABLE)	;The one we reset to when booted, a copy of initial-readtable

;Come here when machine starts.  Provides a base frame.
(DEFUN LISP-TOP-LEVEL ()
  (LISP-REINITIALIZE NIL)			;(Re)Initialize critical variables and things
  (LOOP DOING
    (LISP-TOP-LEVEL1 (OR TV:INITIAL-LISP-LISTENER TERMINAL-IO))
    ;;LISP-TOP-LEVEL1 supposedly never returns, but loop anyway in case
    ;;someone forces it to return with the error-handler.
    ))

;Called when the main process is reset.
(DEFUN LISP-TOP-LEVEL2 ()
  (LISP-TOP-LEVEL1 (OR TV:INITIAL-LISP-LISTENER TERMINAL-IO)))

;Function to reset various things, do initialization that's inconvenient in cold load, etc.
(DEFUN LISP-REINITIALIZE (&OPTIONAL (CALLED-BY-USER T))
  (SETQ INHIBIT-SCHEDULING-FLAG T)		;In case called by the user
  (SETQ ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON NIL)
  ;; Provide ucode with space to keep EVCPs stuck into a-memory locations
  ;; by closure-binding the variables that forward there.
  (OR (BOUNDP 'AMEM-EVCP-VECTOR)
      (SETQ AMEM-EVCP-VECTOR
	    (MAKE-ARRAY (+ (LENGTH A-MEMORY-LOCATION-NAMES) 40 20)  ;20 in case ucode grows.
			':AREA PERMANENT-STORAGE-AREA)))
  (COND ((NOT CALLED-BY-USER)
	 (AND (FBOUNDP 'COMPILER:MA-RESET) ;Unload microcompiled defs, because they are gone!
	      (COMPILER:MA-RESET))	 ; Hopefully manage to do this before any gets called.
	 ;; Set up the TV sync program as soon as possible; until it is set up
	 ;; read references to the TV buffer can get NXM errors which cause a
	 ;; main-memory parity error halt.  Who-line updating can do this.
	 (IF (BOUNDP 'TV:DEFAULT-SCREEN)
	     (COND ((BOUNDP 'TV:SYNC-RAM-CONTENTS)
  ;if TV:SET-TV-SPEED has been done in this image, use the results from that.
		    (SETUP-CPT TV:SYNC-RAM-CONTENTS NIL T)
		    TV:(SETQ %DISK-RUN-LIGHT
			     (+ (- (* MAIN-SCREEN-HEIGHT
				      (SHEET-LOCATIONS-PER-LINE MAIN-SCREEN)) 15)
				(LSH 77 18.)))
		    TV:(SETQ WHO-LINE-RUN-LIGHT-LOC (+ 2 (LOGAND %DISK-RUN-LIGHT 777777))))
		   (T (SETUP-CPT))))))
  (OR (FBOUNDP 'INTERN) (FSET 'INTERN #'INTERN-OLD))
  ;; Do something at least if errors occur during loading
  (OR (FBOUNDP 'FERROR) (FSET 'FERROR #'FERROR-COLD-LOAD))
  (OR (FBOUNDP 'CERROR) (FSET 'CERROR #'CERROR-COLD-LOAD))
  (OR (FBOUNDP 'UNENCAPSULATE-FUNCTION-SPEC)
      (FSET 'UNENCAPSULATE-FUNCTION-SPEC #'(LAMBDA (X) X)))
  (OR (FBOUNDP 'FS:MAKE-PATHNAME-INTERNAL) (FSET 'FS:MAKE-PATHNAME-INTERNAL #'LIST))
  (OR (FBOUNDP 'FS:MAKE-FASLOAD-PATHNAME) (FSET 'FS:MAKE-FASLOAD-PATHNAME #'LIST))
  ;; Allow compilations before ZWEI loaded
  (OR (FBOUNDP 'ZWEI:SETUP-COMPILER-WARNINGS) (FSET 'ZWEI:SETUP-COMPILER-WARNINGS
						    #'(LAMBDA (&OPTIONAL IGNORE) T)))
  ;; Allow streams to work before WHOLIN loaded
  (OR (BOUNDP 'TV:WHO-LINE-FILE-STATE-SHEET)
      (SETQ TV:WHO-LINE-FILE-STATE-SHEET #'(LAMBDA (&REST IGNORE) NIL)))
  (SETQ DEFAULT-CONS-AREA WORKING-STORAGE-AREA)	;Reset default areas.
  (SETQ READ-AREA NIL)
  (NUMBER-GC-ON)	;This seems to work now, make it the default
  (IF (FBOUNDP 'SET-SWAP-RECOMMENDATIONS-OF-AREA)  ;make sure info propagates from area
      (DOLIST (AREA AREA-LIST)			  ;  to regions thereof.
	(LET ((AREA-NUMBER (SYMEVAL AREA)))
	  (SET-SWAP-RECOMMENDATIONS-OF-AREA
	    AREA-NUMBER
	    (AREA-SWAP-RECOMMENDATIONS AREA-NUMBER)))))
  (IF (FBOUNDP 'SET-SCAVENGER-WS)
      (SET-SCAVENGER-WS (IF (BOUNDP 'GC-SCAVENGER-WS-SIZE) GC-SCAVENGER-WS-SIZE 400)))
  (SETQ EH:CONDITION-HANDLERS NIL)
  (COND ((NOT (BOUNDP 'BUILD-INITIAL-OBARRAY-FLAG))
	 (BUILD-INITIAL-OBARRAY)
	 (SETQ BUILD-INITIAL-OBARRAY-FLAG T)))

  (COND ((NOT (BOUNDP 'CURRENT-PROCESS))	;Very first time around
	 (SETQ SCHEDULER-EXISTS NIL
	       CURRENT-PROCESS NIL
	       TV:WHO-LINE-PROCESS NIL
	       TV:LAST-WHO-LINE-PROCESS NIL)
	 (OR (FBOUNDP 'TV:WHO-LINE-RUN-STATE-UPDATE)
	     (FSET 'TV:WHO-LINE-RUN-STATE-UPDATE #'(LAMBDA (&REST IGNORE) NIL)))
	 (SETQ TV:KBD-LAST-ACTIVITY-TIME (TIME))	; C-M-C-M-Return should count.
	 (KBD-INITIALIZE)))
  (INITIALIZE-WIRED-KBD-BUFFER)

  ;Get the right readtable.
  (OR (BOUNDP 'INITIAL-READTABLE)
      (SETQ INITIAL-READTABLE (COPY-READTABLE READTABLE)
	    STANDARD-READTABLE READTABLE))
  (SETQ READTABLE STANDARD-READTABLE)

  ;; Initialize the rubout handler.
  (SETQ	RUBOUT-HANDLER NIL TV:RUBOUT-HANDLER-INSIDE NIL)	;We're not in it now

  ;; Initialize the error handler.
  (OR (BOUNDP 'ERROR-STACK-GROUP)
      (SETQ ERROR-STACK-GROUP (MAKE-STACK-GROUP 'ERROR-STACK-GROUP ':SAFE 0)))
  (SETQ %ERROR-HANDLER-STACK-GROUP ERROR-STACK-GROUP)
  (STACK-GROUP-PRESET ERROR-STACK-GROUP 'LISP-ERROR-HANDLER)	;May not be defined yet 
  (SETF (SG-FOOTHOLD-DATA %INITIAL-STACK-GROUP) NIL)	;EH depends on this
  (AND (FBOUNDP 'LISP-ERROR-HANDLER)
       (FUNCALL ERROR-STACK-GROUP '(INITIALIZE)))
  (COND ((AND (BOUNDP '%INITIALLY-DISABLE-TRAPPING)
	      (NULL %INITIALLY-DISABLE-TRAPPING)
	      (FBOUNDP 'LISP-ERROR-HANDLER)
	      (FBOUNDP 'ENABLE-TRAPPING))
	 (ENABLE-TRAPPING)))
  (SETQ EH:ERRSET-STATUS NIL)			;Turn off possible spurious errset

  ;And all kinds of randomness...

  (SETQ TRACE-LEVEL 0)
  (SETQ INSIDE-TRACE NIL)
  (SETQ G '?? P '??)
  (SETQ + NIL * NIL - NIL ;In case of error during first read/eval/print cycle
	// NIL ++ NIL +++ NIL ;or if their values were unprintable or obscene
	** NIL *** NIL)  ;and to get global values in case of break in a non-lisp-listener
  (SETQ LISP-TOP-LEVEL-INSIDE-EVAL NIL)
  (OR (BOUNDP 'PRIN1) (SETQ PRIN1 NIL))
  (SETQ EVALHOOK NIL)
  (FSET' EVAL #'*EVAL)
  (SETQ IBASE 8 BASE 8 *NOPOINT NIL)
  (SETQ XR-CORRESPONDENCE-FLAG NIL		;Prevent the reader from doing random things
	XR-CORRESPONDENCE NIL)
  (SETQ *RSET T)				;In case any MACLISP programs look at it
  (SETQ PROGDESCLIST NIL RETPROGDESC NIL)
  (SETQ SYS:UNDO-DECLARATIONS-FLAG NIL)		;Don't get screwed by MACRO!
  (SETQ FDEFINE-FILE-PATHNAME NIL)
  (SETQ LOCAL-DECLARATIONS NIL FILE-LOCAL-DECLARATIONS NIL)
  (SETQ SI:PRINT-READABLY NIL)
  (SETQ COMPILER:QC-FILE-IN-PROGRESS NIL COMPILER:QC-FILE-READ-IN-PROGRESS NIL)
  (AND (FBOUNDP 'PKG-FIND-PACKAGE)		;If package system is present
       (SETQ PACKAGE (PKG-FIND-PACKAGE "USER")))

  ;; The first time, this does top-level SETQ's from the cold-load files
  (OR (BOUNDP 'ORIGINAL-LISP-CRASH-LIST)	;Save it for possible later inspection
      (SETQ ORIGINAL-LISP-CRASH-LIST LISP-CRASH-LIST))
  (MAPC (FUNCTION EVAL) LISP-CRASH-LIST)
  (SETQ LISP-CRASH-LIST NIL)

  ;Reattach IO streams.  Note that TERMINAL-IO will be fixed later to go to a window.
  (OR (BOUNDP 'SYN-TERMINAL-IO) (SETQ SYN-TERMINAL-IO (MAKE-SYN-STREAM 'TERMINAL-IO)))
  (COND ((NOT CALLED-BY-USER)
	 (SETQ TERMINAL-IO     COLD-LOAD-STREAM
	       STANDARD-OUTPUT SYN-TERMINAL-IO
	       STANDARD-INPUT  SYN-TERMINAL-IO
	       QUERY-IO        SYN-TERMINAL-IO
	       TRACE-OUTPUT    SYN-TERMINAL-IO
	       ERROR-OUTPUT    SYN-TERMINAL-IO)
	 (FUNCALL TERMINAL-IO ':HOME-CURSOR)))

  (SETQ TV:MOUSE-WINDOW NIL)	;This gets looked at before the mouse process is turned on
  (KBD-CONVERT-NEW 1_15.)	;Reset state of shift keys

  (IF (FBOUNDP 'CADR:CLEAR-UNIBUS-MAP)  ;clear valid bits on unibus map.  Prevents randomness
      (CADR:CLEAR-UNIBUS-MAP))		; and necessary if sharing Unibus with PDP11.
					; Do this before SYSTEM-INITIALIZATION-LIST to
					; avoid screwwing ETHERNET code.
  ;; These are initializations that have to be done before other initializations
  (INITIALIZATIONS 'SYSTEM-INITIALIZATION-LIST T)
  ;; At this point if the window system is loaded, it is all ready to go
  ;; and the initial Lisp listener has been exposed and selected.  So do
  ;; any future typeout on it.  But if any typeout happened on the cold-load
  ;; stream, leave it there (clobbering the Lisp listener's bits).  This does not
  ;; normally happen, but just in case we do the set-cursorpos below so that
  ;; if anything strange gets typed out it won't get erased.  Note that normally
  ;; we do not do any typeout nor erasing on the cold-load-stream, to avoid bashing
  ;; the bits of whatever window was exposed before a warm boot.
  (COND (CALLED-BY-USER)
	((FBOUNDP 'TV:WINDOW-INITIALIZE)
	 (MULTIPLE-VALUE-BIND (X Y) (FUNCALL TERMINAL-IO ':READ-CURSORPOS)
	   (FUNCALL TV:INITIAL-LISP-LISTENER ':SET-CURSORPOS X Y))
	 (SETQ TERMINAL-IO TV:INITIAL-LISP-LISTENER)
	 (FUNCALL TERMINAL-IO ':FRESH-LINE))
	(T (SETQ TV:INITIAL-LISP-LISTENER NIL)	;Not created yet
	   (FUNCALL TERMINAL-IO ':CLEAR-EOL)))

  (AND CURRENT-PROCESS
       (FUNCALL CURRENT-PROCESS ':RUN-REASON 'LISP-INITIALIZE))

  (INITIALIZATIONS 'COLD-INITIALIZATION-LIST)

  (INITIALIZATIONS 'WARM-INITIALIZATION-LIST T)

  (IF (FBOUNDP 'PRINT-HERALD)
      (PRINT-HERALD)
      (FUNCALL STANDARD-OUTPUT ':CLEAR-EOL)
      (PRINC "Lisp Machine cold load environment, beware!"))

  ;; This process no longer needs to be able to run except for the usual reasons.
  ;; The delayed-restart processes may now be allowed to run
  (COND (CURRENT-PROCESS
	 (FUNCALL CURRENT-PROCESS ':REVOKE-RUN-REASON 'LISP-INITIALIZE)
	 (LOOP FOR (P . RR) IN DELAYED-RESTART-PROCESSES
	       DO (WITHOUT-INTERRUPTS
		    (SETF (PROCESS-RUN-REASONS P) RR)
		    (PROCESS-CONSIDER-RUNNABILITY P)))
	 (SETQ DELAYED-RESTART-PROCESSES NIL)
	 (IF WARM-BOOTED-PROCESS
	     (FORMAT T "Warm boot while running ~S.~%Its variable bindings remain in effect; ~
		        its unwind-protects have been lost.~%"
		     (PROG1 WARM-BOOTED-PROCESS (SETQ WARM-BOOTED-PROCESS NIL))))))

  ;; The global value of TERMINAL-IO is a stream which goes to an auto-exposing
  ;; window.  Some processes, such as Lisp listeners, rebind it to something else.
  ;; CALLED-BY-USER is T if called from inside one of those.
  (COND ((AND (NOT CALLED-BY-USER)
	      (FBOUNDP TV:DEFAULT-BACKGROUND-STREAM))
	 (SETQ TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM))))

;This is a temporary function, which turns on the "extra-pdl" feature
(DEFUN NUMBER-GC-ON (&OPTIONAL (ON-P T))
  (SETQ NUMBER-CONS-AREA
        (COND (ON-P EXTRA-PDL-AREA)
              (T WORKING-STORAGE-AREA))))

; The real top level.  Note that the stream to use is passed as an argument and
; bound to the special variable TERMINAL-IO.
(DEFUN LISP-TOP-LEVEL1 (TERMINAL-IO)
  (DO ((*) (+) (-)	;Bind these so that they are per-stack-group
       (//) (++) (+++) (**) (***)
       (THROW-FLAG))	;Gets non-NIL if throw to COMMAND-LEVEL (e.g. quitting from an error)
      (NIL)		;Do forever
    (MULTIPLE-VALUE (NIL THROW-FLAG)
      (*CATCH 'SYS:COMMAND-LEVEL
	      (PROGN (TERPRI)
		     (SETQ - (READ-FOR-TOP-LEVEL))
		     (LET ((LISP-TOP-LEVEL-INSIDE-EVAL T))
		       (SETQ // (MULTIPLE-VALUE-LIST (EVAL -))))
		     (SETQ *** **	;Save first value, propagate old saved values
			   ** *
			   * (CAR //))
		     (DOLIST (VALUE //)
		       (TERPRI)
		       (FUNCALL (OR PRIN1 #'PRIN1) VALUE)))))
    (AND THROW-FLAG (PRINT '*))	;Signal return to top level
    (SETQ +++ ++ ++ + + -)))			;Save last three input forms

;This variable is a list of variables and forms producing the values
;to bind them to.  They get bound by a BREAK.  It is not unreasonable
;for users to push onto this list.  The bindings happen sequentially.
(DEFVAR *BREAK-BINDINGS*
	'((RUBOUT-HANDLER NIL)			;Start new level of rubout catch
	  (READ-PRESERVE-DELIMITERS NIL)	;For normal Lisp syntax
	  (DEFAULT-CONS-AREA WORKING-STORAGE-AREA)	;as opposed to compiler temp area
	  ;Next line commented out since it causes more trouble in than out
	  ;(IBASE 8) (BASE 8)
	  (OLD-STANDARD-INPUT STANDARD-INPUT)	;So user can find old stream.  BREAK, too!
	  (OLD-QUERY-IO QUERY-IO)		;..
	  (STANDARD-INPUT 'SI:TERMINAL-IO-SYN-STREAM)	;Rebind streams to terminal
	  (STANDARD-OUTPUT 'SI:TERMINAL-IO-SYN-STREAM)
	  (QUERY-IO 'SI:TERMINAL-IO-SYN-STREAM)
	  (EH:ERRSET-STATUS NIL)		;"Condition Wall" for errsets
	  (EH:CONDITION-HANDLERS NIL)		; and for conditions
	  ;Changed 3/3/80 by Moon not to bind *, +, and -.
	  ))

(DEFVAR OLD-STANDARD-INPUT)
(DEFVAR OLD-QUERY-IO)

;Note that BREAK binds RUBOUT-HANDLER to NIL so that a new level of catch
;will be established.  Before returning it restores the old rubout handler's buffer.
(DEFUN BREAK (&OPTIONAL &QUOTE TAG &EVAL (CONDITIONAL T) &AUX TEM SAVED-BUFFER)
  (IF CONDITIONAL
      (PROGW *BREAK-BINDINGS*
	;; Deal with keyboard multiplexing in a way similar to the error-handler.
	;; If we break in the scheduler, set CURRENT-PROCESS to NIL.
	;; If this is not the scheduler process, make sure it has a run reason
	;; in case we broke in the middle of code manipulating process data.
	;; If INHIBIT-SCHEDULING-FLAG is set, turn it off and print a warning.
	(COND ((EQ %CURRENT-STACK-GROUP SCHEDULER-STACK-GROUP)
	       (SETQ CURRENT-PROCESS NIL)))
	(AND (NOT (NULL CURRENT-PROCESS))
	     (NULL (FUNCALL CURRENT-PROCESS ':RUN-REASONS))
	     (FUNCALL CURRENT-PROCESS ':RUN-REASON 'BREAK))
	(COND (INHIBIT-SCHEDULING-FLAG
	       (FORMAT T "~%---> Turning off INHIBIT-SCHEDULING-FLAG, you may lose. <---~%")
	       (SETQ INHIBIT-SCHEDULING-FLAG NIL)))
	(AND (MEMQ ':SAVE-RUBOUT-HANDLER-BUFFER (FUNCALL OLD-STANDARD-INPUT
							 ':WHICH-OPERATIONS))
	     (SETQ SAVED-BUFFER (FUNCALL OLD-STANDARD-INPUT ':SAVE-RUBOUT-HANDLER-BUFFER)))
	(FORMAT T "~&;Breakpoint ~A; ~:@C to continue, ~:@C to quit.~%" TAG #\RESUME #\ABORT)
	(LET ((VALUE
		(DO ()
		    (NIL)		;Do forever (until explicit return)
		  (TERPRI)
		 LOOK-FOR-SPECIAL-KEYS
		  (SETQ TEM (FUNCALL STANDARD-INPUT ':TYI))
		  ;; Intercept characters even if otherwise disabled in program
		  ;; broken out of.  Also treat c-Z like ABORT for convenience
		  ;; and for compatibility with the error handler.
		  (AND (= TEM #/Z) (SETQ TEM #\ABORT))
		  (COND ((AND (BOUNDP 'TV:KBD-STANDARD-INTERCEPTED-CHARACTERS)
			      (MEMQ TEM TV:KBD-STANDARD-INTERCEPTED-CHARACTERS))
			 (TV:KBD-INTERCEPT-CHARACTER TEM))
			((= TEM #\RESUME)
			 (FUNCALL STANDARD-OUTPUT ':STRING-OUT "[Resume]
")
			 (RETURN NIL))
			(T (FUNCALL STANDARD-INPUT ':UNTYI TEM)))
		  (MULTIPLE-VALUE (NIL TEM)
		    (*CATCH 'SYS:COMMAND-LEVEL
		      (MULTIPLE-VALUE (- TEM)
			(FUNCALL STANDARD-INPUT ':RUBOUT-HANDLER '((:FULL-RUBOUT
								     :FULL-RUBOUT))
				 #'READ-FOR-TOP-LEVEL))
		      (COND ((EQ TEM ':FULL-RUBOUT)
			     (GO LOOK-FOR-SPECIAL-KEYS))
			    ((EQ - 'P)		;Altmode-P proceeds from BREAK
			     (RETURN NIL))
			    ((AND (LISTP -) (EQ (CAR -) 'RETURN))
			     (RETURN (EVAL (CADR -)))))	;(RETURN form) proceeds
		      (SETQ // (MULTIPLE-VALUE-LIST (EVAL -)))
		      (SETQ *** **
			    ** *
			    * (CAR //))		;Save first value
		      (DOLIST (VALUE //)
			(TERPRI)
			(FUNCALL (OR PRIN1 #'PRIN1) VALUE))
		      (SETQ +++ ++ ++ + + -)))
		  (AND TEM (FORMAT T "~&;Back to Breakpoint ~A; ~:@C to continue, ~
				      ~:@C to quit.~%" TAG #\RESUME #\ABORT)))))
	  ;; Before returning, restore and redisplay rubout handler's buffer so user
	  ;; gets what he sees, if we broke out of reading through the rubout handler.
	  ;; If we weren't inside there, the rubout handler buffer is now empty because
	  ;; we read from it, so leave it alone.  (Used to :CLEAR-INPUT).
	  (COND (SAVED-BUFFER
		 (FUNCALL OLD-STANDARD-INPUT ':RESTORE-RUBOUT-HANDLER-BUFFER SAVED-BUFFER)))
	  VALUE))))

;;; Initialization stuff

;; Some code relies on INIT-NAME being the CAR of the init entry.  **DO NOT CHANGE THIS**
(DEFSTRUCT (INIT-LIST-ENTRY :LIST
			    (:CONSTRUCTOR MAKE-INIT-LIST-ENTRY (NAME FORM FLAG SOURCE-FILE))
			    (:CONC-NAME "INIT-")
			    (:ALTERANT NIL))
  NAME
  FORM
  FLAG						;Non-NIL means init has been run.
  SOURCE-FILE)

(DEFMACRO INIT-LIST-CHECK (NAME)
  `(PROGN (OR (BOUNDP ,NAME)
	      (SET ,NAME NIL))
	  (OR (GET ,NAME 'INITIALIZATION-LIST)
	      (PUTPROP ,NAME T 'INITIALIZATION-LIST))))

;;; Run the inits in the specified list.
;;; If init has been run before it will only be run again if the second arg is non-NIL.
;;; The third arg is the flag to be RLACA'd into the flag slot.  If it is NIL it will
;;;  look as if the inits have never been run.  This may be useful for some applications.
(DEFUN INITIALIZATIONS (LIST-NAME &OPTIONAL (REDO-FLAG NIL) (FLAG T))
  (INIT-LIST-CHECK LIST-NAME)
  (DO ((INIT (SYMEVAL LIST-NAME) (CDR INIT)))
      ((NULL INIT))
      (COND ((OR (NULL (INIT-FLAG (CAR INIT))) REDO-FLAG)
             (EVAL (INIT-FORM (CAR INIT)))
             (SETF (INIT-FLAG (CAR INIT)) FLAG)))))

;;; Adds a new init to the list.
;;; Keywords are:
;;; NOW		Run the init now
;;; FIRST	Run the init now if this is the first entry for the specified name
;;; NORMAL	Do the "normal" thing (init when initializations normally run)
;;; REDO	Do nothing now, but set up things so init gets redone
;;; COLD	Use the cold boot list
;;; WARM	Use the warm boot list
;;; ONCE	Use the once-only list
;;; SYSTEM	Use the system list
;;; BEFORE-COLD	The list that gets done before disk-save'ing out
;;; LOGIN       Use the login list
;;; LOGOUT      Use the logout list
;;; SITE	Use the site list (also run once)
;;; If neither WARM nor COLD are specified, warm is assumed.  If a fourth argument
;;; is given, then it is the list to use.  WARM and COLD will override the fourth argument.
(DEFUN ADD-INITIALIZATION (NAME FORM &OPTIONAL KEYWORDS (LIST-NAME 'WARM-INITIALIZATION-LIST)
                                     &AUX (WHEN NIL) INIT)
  (DO ((L KEYWORDS (CDR L))
       (V))
      ((NULL L))
      (SETQ V (GET-PNAME (CAR L)))
      (COND ((STRING-EQUAL "NOW" V) (SETQ WHEN 'NOW))
            ((STRING-EQUAL "FIRST" V) (SETQ WHEN 'FIRST))
            ((STRING-EQUAL "NORMAL" V) (SETQ WHEN NIL))
            ((STRING-EQUAL "REDO" V) (SETQ WHEN 'REDO))
            ((STRING-EQUAL "WARM" V) (SETQ LIST-NAME 'WARM-INITIALIZATION-LIST))
            ((STRING-EQUAL "COLD" V) (SETQ LIST-NAME 'COLD-INITIALIZATION-LIST))
            ((STRING-EQUAL "BEFORE-COLD" V) (SETQ LIST-NAME 'BEFORE-COLD-INITIALIZATION-LIST))
	    ((STRING-EQUAL "SYSTEM" V)
             (SETQ LIST-NAME 'SYSTEM-INITIALIZATION-LIST)
             (SETQ WHEN 'FIRST))
            ((STRING-EQUAL "ONCE" V)
             (SETQ LIST-NAME 'ONCE-ONLY-INITIALIZATION-LIST)
             (SETQ WHEN 'FIRST))
	    ((STRING-EQUAL "LOGIN" V)
	     (SETQ LIST-NAME 'LOGIN-INITIALIZATION-LIST))
	    ((STRING-EQUAL "LOGOUT" V)
	     (SETQ LIST-NAME 'LOGOUT-INITIALIZATION-LIST))
            ((STRING-EQUAL "SITE" V)
	     (SETQ LIST-NAME 'SITE-INITIALIZATION-LIST
		   WHEN 'NOW))
	    (T (FERROR NIL "Illegal keyword ~S" (CAR L)))))
  (INIT-LIST-CHECK LIST-NAME)
  (SETQ INIT
        (DO ((L (SYMEVAL LIST-NAME) (CDR L)))
            ((NULL L)
             (COND ((NULL (SYMEVAL LIST-NAME))
                    (CAR (SET LIST-NAME (NCONS (MAKE-INIT-LIST-ENTRY
						 NAME FORM NIL SI:FDEFINE-FILE-PATHNAME)))))
                   (T (CADR (RPLACD (LAST (SYMEVAL LIST-NAME))
                                    (NCONS (MAKE-INIT-LIST-ENTRY
					     NAME FORM NIL SI:FDEFINE-FILE-PATHNAME)))))))
            (COND ((STRING-EQUAL (INIT-NAME (CAR L)) NAME)
                   (SETF (INIT-FORM (CAR L)) FORM)
                   (RETURN (CAR L))))))
  (COND ((EQ WHEN 'REDO) (SETF (INIT-FLAG INIT) NIL))
        ((OR (EQ WHEN 'NOW)
             (AND (EQ WHEN 'FIRST) (NULL (INIT-FLAG INIT))))
         (EVAL (INIT-FORM INIT))
         (SETF (INIT-FLAG INIT) T))))

;;; Deletes an init from the list.
;;; Keywords are:
;;; COLD	Use the cold boot list
;;; WARM	Use the warm boot list
;;; ONCE	Use the once-only list
;;; SYSTEM	Use the system list
;;; BEFORE-COLD	The list that gets done before disk-save'ing out
;;; LOGIN       Use the login list
;;; LOGOUT      Use the LOGOUT list
;;; SITE	Use the site list
;;; If neither WARM nor COLD are specified, warm is assumed.  If a third argument
;;; is given, then it is the list to use.  WARM and COLD will override the third argument.
(DEFUN DELETE-INITIALIZATION (NAME &OPTIONAL KEYWORDS (LIST-NAME 'WARM-INITIALIZATION-LIST))
  (DO ((L KEYWORDS (CDR L))
       (V))
      ((NULL L))
      (SETQ V (GET-PNAME (CAR L)))
      (COND ((STRING-EQUAL "WARM" V) (SETQ LIST-NAME 'WARM-INITIALIZATION-LIST))
            ((STRING-EQUAL "COLD" V) (SETQ LIST-NAME 'COLD-INITIALIZATION-LIST))
            ((STRING-EQUAL "BEFORE-COLD" V) (SETQ LIST-NAME 'BEFORE-COLD-INITIALIZATION-LIST))
            ((STRING-EQUAL "ONCE" V) (SETQ LIST-NAME 'ONCE-ONLY-INITIALIZATION-LIST))
            ((STRING-EQUAL "SYSTEM" V) (SETQ LIST-NAME 'SYSTEM-INITIALIZATION-LIST))
	    ((STRING-EQUAL "LOGIN" V) (SETQ LIST-NAME 'LOGIN-INITIALIZATION-LIST))
	    ((STRING-EQUAL "LOGOUT" V) (SETQ LIST-NAME 'LOGOUT-INITIALIZATION-LIST))
	    ((STRING-EQUAL "SITE" V) (SETQ LIST-NAME 'SITE-INITIALIZATION-LIST))
            (T (FERROR NIL "Illegal keyword ~S" (CAR L)))))
  (INIT-LIST-CHECK LIST-NAME)
  (DO ((L (SYMEVAL LIST-NAME) (CDR L))
       (FLAG NIL))
      ((NULL L) FLAG)
      (COND ((STRING-EQUAL (INIT-NAME (CAR L)) NAME)
             (SET LIST-NAME (DELQ (CAR L) (SYMEVAL LIST-NAME)))
	     (SETQ FLAG T)))))

(DEFUN RESET-INITIALIZATIONS (LIST-NAME)
  (INIT-LIST-CHECK LIST-NAME)
  (DO ((L (SYMEVAL LIST-NAME) (CDR L)))
      ((NULL L))
      (SETF (INIT-FLAG (CAR L)) NIL)))

;;; This is an old name for FDEFINE which everyone uses.
(DEFUN FSET-CAREFULLY (FUNCTION-SPEC DEFINITION &OPTIONAL NO-QUERY-FLAG)
  (FDEFINE FUNCTION-SPEC DEFINITION T NO-QUERY-FLAG))

;Simple version of FERROR to be used in the cold load environment.
(DEFUN FERROR-COLD-LOAD (&REST ARGS)
  (PRINT ARGS)
  (BREAK FERROR))

;Simple version of CERROR to be used in the cold load environment.
(DEFUN CERROR-COLD-LOAD (&REST ARGS)
  (PRINT ARGS)
  (BREAK CERROR))

;;; Stuff which has to go somewhere, to be around in the cold-load,
;;; and doesn't have any logical place where it belongs (this used to
;;; be in LMIO;KBD)

(DEFVAR USER-ID "")	;Not logged in


;; This is here rather than with the scheduler because it has to be
;; in the cold-load.  It checks for the non-existence of a scheduler
;; and does it itself in that case.

;; Takes a predicate and arguments to it.  The process becomes blocked
;; until the application of the predicate to those arguments returns T.
;; Note that the function is run in the SCHEDULER stack group, not the
;; process's stack group!  This means that bindings in effect at the
;; time PROCESS-WAIT is called will not be in effect; don't refer to
;; variables "freely" if you are binding them.
;;    Kludge:  if the scheduler seems broken, or we ARE the scheduler
;; (i.e. a clock function tries to block), then loop-wait (no blinkers...)

;; In case of a process-level interrupt while waiting, this function can get
;; restarted from its beginning.  Therefore, it must not modify its arguments,
;; and the way it does its WITHOUT-INTERRUPTS must not be changed.
;; See (:METHOD SI:PROCESS :INTERRUPT)
(DEFUN PROCESS-WAIT (WHOSTATE FUNCTION &REST ARGUMENTS)
  (COND ((APPLY FUNCTION ARGUMENTS)	;Test condition before doing slow stack-group switch
	 NIL)				;Hmm, no need to wait after all
	((OR (NOT SCHEDULER-EXISTS)
	     (EQ SCHEDULER-STACK-GROUP %CURRENT-STACK-GROUP)
	     (NULL CURRENT-PROCESS)
	     (LET ((STATE (SG-CURRENT-STATE SCHEDULER-STACK-GROUP)))
	       (NOT (OR (= STATE SG-STATE-AWAITING-INITIAL-CALL)
			(= STATE SG-STATE-AWAITING-CALL)
			(= STATE SG-STATE-AWAITING-RETURN)))))
	 (DO () (NIL)
	   (AND (APPLY FUNCTION ARGUMENTS)
		(RETURN NIL))))
	(T
	 (WITHOUT-INTERRUPTS	;A sequence break would reset my state to "running"
	   (SETF (PROCESS-WHOSTATE CURRENT-PROCESS) WHOSTATE)
	   (TV:WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS)
	   (SET-PROCESS-WAIT CURRENT-PROCESS FUNCTION ARGUMENTS)
	   ;; DON'T change this FUNCALL to a STACK-GROUP-RESUME!  The scheduler
	   ;; needs to know what the process's current stack group is.
	   (FUNCALL SCHEDULER-STACK-GROUP))
	 (TV:WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS))))

;;; System initialization
(DECLARE (SPECIAL INNER-SYSTEM-FILE-ALIST))
(DECLARE (SPECIAL QLD-GROUND-DONE QLD-MINI-DONE))

;;; Procedure for booting up a world load:
;;; 1. Use MINI to load PACK4.  Create packages.
;;; 2. Use MINI to load kernel system, viz. FORMAT, flavors, processes, error handler,
;;;    chaos, QFILE.
;;; 3. Do like a cold boot.  This turns on the real file system.
;;; 4. Load MAKSYS and SYSDCL to build the initial systems.
;;; 5. Use MAKE-SYSTEM to load the rest of the top level system.
(DEFUN QLD (&OPTIONAL (FROM-THE-GROUND-UP-P NIL)
		      (LOAD-KEYWORDS '(:NOCONFIRM :SILENT :NO-RELOAD-SYSTEM-DECLARATION)))
  (SETQ AREA-FOR-PROPERTY-LISTS WORKING-STORAGE-AREA)	;Because will be recopied at end
  (OR (BOUNDP 'QLD-GROUND-DONE)
      (SETQ QLD-GROUND-DONE NIL))
  (OR (BOUNDP 'QLD-MINI-DONE)
      (SETQ QLD-MINI-DONE NIL))
  (COND ((OR FROM-THE-GROUND-UP-P		;If we are starting from a cold-load
	     (NOT QLD-GROUND-DONE))
	 (TERPRI)
	 (PRINC "Loading and installing PACK4")
	 (MINI-LOAD-FILE-ALIST LOAD-PACKAGES-FILE-ALIST-1)
	 (PKG-INSTALL)
	 (MINI-LOAD-FILE-ALIST LOAD-PACKAGES-FILE-ALIST-2)
	 (SETQ QLD-GROUND-DONE T)))
  (TERPRI)
  (COND ((NULL QLD-MINI-DONE)
	 (PRINC "Loading inner system")
	 (MINI-LOAD-FILE-ALIST INNER-SYSTEM-FILE-ALIST)
	 ;; Even though PATHNM is now loaded, it doesn't work yet.  So must disable
	 ;; FS:MAKE-FASLOAD-PATHNAME until it does.
	 (LET ()
	      (BIND (FUNCTION-CELL-LOCATION 'FS:MAKE-FASLOAD-PATHNAME) #'LIST)
	   (SET-SITE SITE-NAME)			;Declare site setup by the cold-load generator
	   ;; Now load the network specific code.
	   (AND (GET-SITE-OPTION ':CHAOS)
		(MINI-LOAD-FILE-ALIST CHAOS-FILE-ALIST))
	   (AND (GET-SITE-OPTION ':ETHER)
		(MINI-LOAD-FILE-ALIST ETHER-FILE-ALIST))
	   ;; All the local file computers should now be defined, so we can setup SYS:
	   ;; for use by MAKSYS.
	   (ADD-INITIALIZATION "DEFINE-SYS-LOGICAL-DEVICE" '(FS:DEFINE-SYS-LOGICAL-DEVICE)
			       '(SITE))
	   ;; Now fix up the site initialization list.  The host table loading initialization
	   ;; is the first one, from QMISC.  Next comes the host reseting one from HOST.  Then
	   ;; the SYS: initialization from PATHNM.  When loading the system now, the host
	   ;; table is loaded via MINI with explicit host pathnames, and so must come before
	   ;; SYS:.  When changing the site, the host table is loaded from SYS:, so that must
	   ;; be setup before.  Therefore, move the host table initialization (the CAR) to
	   ;; just after the SYS: one.
	   (LET ((ELEM (ASSOC "DEFINE-SYS-LOGICAL-DEVICE" SITE-INITIALIZATION-LIST)))
	     (PUSH (POP SITE-INITIALIZATION-LIST) (CDR (MEMQ ELEM SITE-INITIALIZATION-LIST))))
	   )
	 (LISP-REINITIALIZE)			;Turn on network, load error table, etc.
	 (LOGIN "LISPM" T)			;So that we can do file I/O
	 (FS:CANONICALIZE-COLD-LOAD-PATHNAMES)	;Update properties for real pathnames
	 (DOLIST (F SYSTEM-SYSTEM-FILE-ALIST)
	   (LOAD (CAR F) (CADR F)))
	 (SETQ QLD-MINI-DONE T))
	(T (LOGIN "LISPM" T)))			;So that we can do file I/O
  (PRINC "Loading rest of world")
  ;; Make sure symbols that MAKSYS binds are set correctly
  (DOLIST (X '((COMPILER:COMPILER-WARNINGS-CONTEXT . NIL)
	       (COMPILER:COMPILER-WARNINGS-BUFFER . "Compiler Warnings")
	       (TV:MORE-PROCESSING-GLOBAL-ENABLE . T)))
    (OR (BOUNDP (CAR X))
	(SET (CAR X) (CDR X))))
  (LET-IF (NOT (FBOUNDP 'COMPILER:ENTER-COMPILER-WARNINGS-CONTEXT))
       ;;Set up a fake compiler context
       ((COMPILER:COMPILER-WARNINGS-CONTEXT T)
	(COMPILER:COMPILER-WARNINGS-INTERVAL-STREAM NIL)
	(COMPILER:FUNCTIONS-REFERENCED NIL)
	(COMPILER:FUNCTIONS-DEFINED NIL)
	(COMPILER:BARF-SPECIAL-LIST NIL))
    (LEXPR-FUNCALL #'MAKE-SYSTEM 'SYSTEM LOAD-KEYWORDS))
  (ASSURE-CC-SYMBOLS-LOADED)
  ;; Compactify property lists in the hopes of speeding up compilation
  (SETQ AREA-FOR-PROPERTY-LISTS PROPERTY-LIST-AREA)
  (MAPATOMS-ALL #'(LAMBDA (X) (SETPLIST X (COPYLIST (PLIST X) PROPERTY-LIST-AREA))))
  (FORMAT T "~%Size of new world is ~D blocks~%" (FIND-MAX-ADDR))
  (PRINT-DISK-LABEL)
  (FORMAT T "~%OK, now do a DISK-SAVE~%"))

;;; This is used for things like host tables which can get loaded again when the world
;;; is already built.
(DEFUN MAYBE-MINI-LOAD-FILE-ALIST (ALIST)
  (IF (NOT QLD-MINI-DONE)
      (MINI-LOAD-FILE-ALIST ALIST)
      (DOLIST (F ALIST)
	(LOAD (CAR F) (CADR F)))))

(DEFCONST SYSTEM-SYSTEM-FILE-ALIST
	  '(("SYS: SYS2; MAKSYS QFASL >" "SI")
	    ("SYS: SYS2; PATCH QFASL >" "SI")
	    ("SYS: SYS; SYSDCL QFASL >" "SI")))
