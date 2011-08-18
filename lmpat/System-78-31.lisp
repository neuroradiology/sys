;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.31
;;; Reason: Record time of cold booting in si:time-booted.
;;; Written 12/24/81 09:56:16 by dlw,
;;; while running on Spaniel from band 2
;;; with System 78.30, ZMail 38.5, Symbolics 8.7, Tape 6.4, LMFS 21.18, Canon 9.5, microcode 841.



; From file LTOP.LISP >LISPM POINTER:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

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
	 (COND ((NULL WARM-BOOTED-PROCESS)
		;; Remember when we were cold booted, just in case anyone is interested.
		(SETQ TIME-BOOTED (ERRSET (TIME:GET-UNIVERSAL-TIME))))
	       (T
		(FORMAT T "Warm boot while running ~S.~@
                           Its variable bindings remain in effect; ~
		                its unwind-protects have been lost.~%"
			(PROG1 WARM-BOOTED-PROCESS (SETQ WARM-BOOTED-PROCESS NIL)))))))

  ;; The global value of TERMINAL-IO is a stream which goes to an auto-exposing
  ;; window.  Some processes, such as Lisp listeners, rebind it to something else.
  ;; CALLED-BY-USER is T if called from inside one of those.
  (COND ((AND (NOT CALLED-BY-USER)
	      (FBOUNDP TV:DEFAULT-BACKGROUND-STREAM))
	 (SETQ TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM))))

)
