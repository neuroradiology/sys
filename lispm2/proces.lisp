;;; -*- Mode: LISP; Package: SYSTEM-INTERNALS; Base: 8 -*-

;;; Process system and scheduler

; A process is an instance which embodies one or several stack groups as well as
; appropriate variables to determine the stack-group's status and runnability.
; See PRODEF

;;; ACTIVE-PROCESSES	An alist of all processes that are runnable.
;;;			A process is runnable if it has at least one run
;;;			reason, and no arrest reasons.  This list is maintained
;;;			because it is considered too expensive to have the
;;;			scheduler inspect each process' run and arrest reasons.
;;; Each element on ACTIVE-PROCESSES looks like:
;;;	(process wait-function wait-arglist priority <slots for wait args>)
;;;	wait-arglist is usually a tail of this list
;;; CURRENT-PROCESS	The process that is currently running.  NIL inside the
;;;			scheduler.

(DEFVAR ACTIVE-PROCESSES-ELEMENT-SIZE 9)
(DEFVAR ACTIVE-PROCESSES-PREFIX-SIZE 4) ;Process, wait-function, wait-arglist, priority

(DEFUN MAKE-ACTIVE-PROCESSES (LEN &AUX AP)
  (WITHOUT-INTERRUPTS
    ;; Make sure that list gets allocated contiguously
    (SETQ AP (MAKE-LIST PERMANENT-STORAGE-AREA LEN))
    (DO ((L AP (CDR L)))
	((NULL L) AP)
      (RPLACA L (MAKE-LIST PERMANENT-STORAGE-AREA ACTIVE-PROCESSES-ELEMENT-SIZE)))))

(DEFVAR ACTIVE-PROCESSES (MAKE-ACTIVE-PROCESSES PROCESS-ACTIVE-LENGTH))

;Make an entry for this process in ACTIVE-PROCESSES, with its current wait condition,
;when it first becomes runnable.  Try not to cons.
(DEFUN PROCESS-ACTIVE-ENTRY (PROC &AUX AENTRY)
  (WITHOUT-INTERRUPTS
    (PROCESS-ALL-PROCESSES PROC T)
    (OR (SETQ AENTRY (ASSQ PROC ACTIVE-PROCESSES))
	(SETQ AENTRY (ASSQ NIL ACTIVE-PROCESSES))
	(RPLACD (LAST ACTIVE-PROCESSES)
		(NCONS (SETQ AENTRY (MAKE-LIST PERMANENT-STORAGE-AREA
					       ACTIVE-PROCESSES-ELEMENT-SIZE)))))
    (SETF (FIRST AENTRY) PROC)
    (SETF (FOURTH AENTRY) (PROCESS-PRIORITY PROC))
    (PROCESS-ORDER-ACTIVE-PROCESSES)
    (SET-PROCESS-WAIT PROC (PROCESS-WAIT-FUNCTION PROC) (PROCESS-WAIT-ARGUMENT-LIST PROC))))

(DEFUN PROCESS-ALL-PROCESSES (PROC ADD-P)
  ;; Must be called with interrupts inhibited
  (IF ADD-P
      (OR (MEMQ PROC ALL-PROCESSES) (PUSH PROC ALL-PROCESSES))
      (SETQ ALL-PROCESSES (DELQ PROC ALL-PROCESSES))))

;Set up a process's wait condition, in both places. 
(DEFUN SET-PROCESS-WAIT (PROC FUN ARGS &AUX IDX APE)
  (WITHOUT-INTERRUPTS
    (SETF (PROCESS-WAIT-FUNCTION PROC) FUN)
    (SETF (PROCESS-WAIT-ARGUMENT-LIST PROC) ARGS)
    (COND ((NULL (SETQ APE (ASSQ PROC ACTIVE-PROCESSES))))
	  (T
	    (SETF (SECOND APE) FUN)
	    (COND (( (SETQ IDX (- ACTIVE-PROCESSES-ELEMENT-SIZE (LENGTH ARGS)))
		      ACTIVE-PROCESSES-PREFIX-SIZE)
		   (LET ((L (NTHCDR IDX APE)))
		     (SETF (THIRD APE) L)
		     (DO ((L L (CDR L))
			  (ARGS ARGS (CDR ARGS)))
			 ((NULL ARGS))
		       (RPLACA L (CAR ARGS)))))
		  (T (SETF (THIRD APE) ARGS)))))))

(DEFUN MAKE-PROCESS (NAME &REST INIT-ARGS)
  (OR (CAR INIT-ARGS) (SETQ INIT-ARGS (CDR INIT-ARGS)))	;For backward compatibility
  (SETQ INIT-ARGS (LIST* ':NAME NAME INIT-ARGS))
  (INSTANTIATE-FLAVOR (OR (GET (LOCF INIT-ARGS) ':FLAVOR)
			  (AND (GET (LOCF INIT-ARGS) ':SIMPLE-P) 'SIMPLE-PROCESS)
			  'PROCESS)
		      (LOCF INIT-ARGS)
		      T))

(DEFF PROCESS-CREATE 'MAKE-PROCESS)

(DEFMETHOD (PROCESS :INIT) (INIT-PLIST)
  (OR (BOUNDP 'STACK-GROUP)
      (SETQ STACK-GROUP (LEXPR-FUNCALL #'MAKE-STACK-GROUP NAME
  				       ':ALLOW-UNKNOWN-KEYWORDS T ':SAFE 0
				       (CAR INIT-PLIST))))
  (SETQ INITIAL-STACK-GROUP STACK-GROUP))

(DEFMETHOD (SIMPLE-PROCESS :INIT) (IGNORE)
  (SETQ INITIAL-FORM NIL
	STACK-GROUP NIL
	INITIAL-STACK-GROUP NIL))

(DEFMETHOD (PROCESS :AFTER :INIT) (IGNORE)
  (WITHOUT-INTERRUPTS
    (PROCESS-ALL-PROCESSES SELF T)))

(DEFMETHOD (PROCESS :PRINT-SELF) (STREAM &REST IGNORE)
  (SI:PRINTING-RANDOM-OBJECT (SELF STREAM)
    (PRINC (TYPEP SELF) STREAM)
    (FUNCALL STREAM ':TYO #\SP)
    (PRINC NAME STREAM)))

(DEFUN PROCESS-PRESET (PROCESS FUNCTION &REST ARGS)
  (LEXPR-FUNCALL PROCESS ':PRESET FUNCTION ARGS))

(DEFMETHOD (SIMPLE-PROCESS :PRESET) (FUNCTION &REST ARGS)
  (OR (NULL ARGS) (FERROR NIL "Simple processes have no args to their functions"))
  (SETQ INITIAL-FORM (NCONS FUNCTION))
  (FUNCALL-SELF ':RESET))

(DEFMETHOD (PROCESS :PRESET) (FUNCTION &REST ARGS)
  (SETQ INITIAL-FORM (CONS FUNCTION (COPYLIST ARGS)))
  (FUNCALL-SELF ':RESET))

;This is the real initial function of all processes' initial stack groups.
;Its purpose is to make sure that the error handler C-Z command works.
;It also prevents anything bad from happening if the specified top-level returns
;and arranges for typing out to do "background" stuff.
(DEFUN PROCESS-TOP-LEVEL (&OPTIONAL IGNORE)
  (LET ((TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM))
    (UNWIND-PROTECT
      (DO () (())
	(*CATCH 'SYS:COMMAND-LEVEL
	  (PROGN (APPLY (CAR (PROCESS-INITIAL-FORM CURRENT-PROCESS))
			(CDR (PROCESS-INITIAL-FORM CURRENT-PROCESS)))
		 (PROCESS-FLUSH-BACKGROUND-STREAM)
		 (PROCESS-WAIT-FOREVER))))
      (PROCESS-FLUSH-BACKGROUND-STREAM))))

(DEFUN PROCESS-KILL-TOP-LEVEL (&OPTIONAL ARG)
  "Get here after unwinding the stack due to a kill type :RESET.  Makes the 
process unrunnable, and removes it from the ALL-PROCESSES list.  The process may be
enabled later.  If so, it will do the right thing by calling PROCESS-TOP-LEVEL."
  (WITHOUT-INTERRUPTS
    (PROCESS-DISABLE CURRENT-PROCESS)
    (PROCESS-ALL-PROCESSES CURRENT-PROCESS NIL)
    ;; This will never return unless the process is reenabled
    (PROCESS-ALLOW-SCHEDULE))
  ;; In case we are enabled again, act like we were just reset
  (PROCESS-TOP-LEVEL ARG))


(DEFUN PROCESS-FLUSH-BACKGROUND-STREAM ()
  (COND ((AND (NEQ TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM)
	      (TYPEP TERMINAL-IO 'TV:SHEET))
	 (AND (GET-HANDLER-FOR TERMINAL-IO ':WAIT-UNTIL-SEEN)
	      (FUNCALL TERMINAL-IO ':WAIT-UNTIL-SEEN))
	 (FUNCALL TERMINAL-IO ':DEACTIVATE)
	 (DEALLOCATE-RESOURCE 'TV:BACKGROUND-LISP-INTERACTORS TERMINAL-IO)
	 (SETQ TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM))))

(DEFUN PROCESS-RESET (PROCESS) (FUNCALL PROCESS ':RESET))

(DEFMETHOD (PROCESS :RESET) (&OPTIONAL UNWIND-OPTION KILL &AUX RESTART-FUN)
  "UNWIND-OPTION: T, never unwind; :UNLESS-CURRENT or NIL, unwinds the stack unless
the stack group is either in the current process or is the current stack group;
:ALWAYS, always unwinds the stack.  KILL is T to kill the process after optionally
unwinding it."
  (WITHOUT-INTERRUPTS
    (SETQ RESTART-FUN (COND (KILL #'PROCESS-KILL-TOP-LEVEL)
			    ((EQ STACK-GROUP INITIAL-STACK-GROUP) #'PROCESS-TOP-LEVEL)
			    (T #'(LAMBDA (&REST IGNORE)	;Unwind and switch SG's
				   (EH:UNWIND-SG (PROCESS-INITIAL-STACK-GROUP CURRENT-PROCESS)
						 #'PROCESS-TOP-LEVEL NIL NIL)))))
    ;; Wake up
    (SETQ WHOSTATE "RUN")
    (SET-PROCESS-WAIT SELF #'TRUE NIL)
    (COND ((EQ SELF CURRENT-PROCESS)
	   (IF (EQ UNWIND-OPTION ':ALWAYS)
	       (*UNWIND-STACK T NIL NIL RESTART-FUN)
	       (COND (KILL
		      (PROCESS-DISABLE CURRENT-PROCESS)
		      (PROCESS-ALL-PROCESSES CURRENT-PROCESS NIL)))))
	  (T
	   ;; Note -- the following code is not logically necessary.  However,
	   ;; it is here to make the cold-load come up when EH:UNWIND-SG
	   ;; is not loaded yet.  We avoid unwinding the stack-group if it
	   ;; has just been created.
	   (LET ((ST (SG-CURRENT-STATE STACK-GROUP)))
	     (AND (OR (= ST SG-STATE-AWAITING-INITIAL-CALL) (= ST 0))
		  (SETQ UNWIND-OPTION T)))
	   ;; Cause the process, when next scheduled, to unwind itself and
	   ;; call its initial function in the right stack group.
	   (COND ((EQ %CURRENT-STACK-GROUP STACK-GROUP)
		  ;; Not current process, but our stack group is the one running.
		  ;; Respect NOUNWIND
		  (IF (EQ UNWIND-OPTION ':ALWAYS)
		      (*UNWIND-STACK T NIL NIL RESTART-FUN)
		      (COND (KILL
			     (PROCESS-DISABLE CURRENT-PROCESS)
			     (PROCESS-ALL-PROCESSES CURRENT-PROCESS NIL)))))
		 ((NEQ UNWIND-OPTION 'T)
		  (LET ((EH:ALLOW-PDL-GROW-MESSAGE NIL))
		    (EH:UNWIND-SG STACK-GROUP RESTART-FUN NIL T)))
		 (T
		  (STACK-GROUP-PRESET STACK-GROUP RESTART-FUN)))))))

(DEFMETHOD (SIMPLE-PROCESS :RESET) (&OPTIONAL UNWIND-OPTION KILL)
  UNWIND-OPTION	;ignored -- there is no stack group
  (WITHOUT-INTERRUPTS
    (SETQ STACK-GROUP (CAR INITIAL-FORM))	;Reset to initial function
    (SETQ WHOSTATE "RUN")			;and un-block
    (SET-PROCESS-WAIT SELF #'TRUE NIL)
    (COND (KILL
	   (PROCESS-DISABLE SELF)		;Killing: remove from scheduler lists
	   (PROCESS-ALL-PROCESSES SELF NIL)))))


; Process Interrupt Mechanism

(DEFMETHOD (SIMPLE-PROCESS :INTERRUPT) (FUNCTION &REST ARGS)
  FUNCTION ARGS ;ignored
  (FERROR NIL "Cannot interrupt a simple process"))

(DEFMETHOD (PROCESS :INTERRUPT) (FUNCTION &REST ARGS)
  (IF (EQ SELF CURRENT-PROCESS)
	(PROGN (LEXPR-FUNCALL FUNCTION ARGS) T) ;Note destination must be D-IGNORE
      (DO (STATE) (NIL)			;Loop until in interruptible state
	(WITHOUT-INTERRUPTS
	  (SETQ STATE (SG-CURRENT-STATE STACK-GROUP))
	  (COND ((= STATE SG-STATE-AWAITING-RETURN)	;Called scheduler
		 (LET ((RP (SG-REGULAR-PDL STACK-GROUP))
		       (PP (SG-REGULAR-PDL-POINTER STACK-GROUP))
		       (SP (SG-SPECIAL-PDL STACK-GROUP))
		       (SPP (SG-SPECIAL-PDL-POINTER STACK-GROUP))
		       (AP (SG-AP STACK-GROUP)))
		   (OR (EQ (AREF RP AP) SCHEDULER-STACK-GROUP)
		       (FERROR NIL "Call to ~S where scheduler stack group expected"
				   (AREF RP AP)))
		   ;; Remove frame of call to scheduler.  PP := M-AP minus 4
		   (SETF (SG-PDL-PHASE STACK-GROUP) (LOGAND (- (SG-PDL-PHASE STACK-GROUP)
							       (- PP (SETQ PP (- AP 4))))
							    1777))
		   (SETF (SG-REGULAR-PDL-POINTER STACK-GROUP) PP)
		   (SETF (SG-IPMARK STACK-GROUP) (EH:SG-PREVIOUS-OPEN STACK-GROUP AP))
		   (SETF (SG-AP STACK-GROUP) (SETQ AP (EH:SG-PREVIOUS-ACTIVE STACK-GROUP AP)))
		   (SETF (SG-FLAGS-QBBFL STACK-GROUP) ; Must correspond to current frame
			 (RP-BINDING-BLOCK-PUSHED RP AP))
		   (SETQ STATE SG-STATE-RESUMABLE)
		   (SET-PROCESS-WAIT SELF #'TRUE NIL)	;Allow to wake up
		   ;; If this function is PROCESS-WAIT, restart it at its start PC
		   ;; so that when returned to, it will test the wait condition again.
		   ;; Its stack level is sort of random, but that shouldn't hurt anything.
		   ;; Also it has a binding of INHIBIT-SCHEDULING-FLAG which needs attention
		   (COND ((EQ (AREF RP AP) #'PROCESS-WAIT)
			  (SETF (RP-EXIT-PC RP AP) (FEF-INITIAL-PC #'PROCESS-WAIT))
			  (OR (EQ (AREF SP SPP)
				  (%P-CONTENTS-AS-LOCATIVE
				    ;(VALUE-CELL-LOCATION 'INHIBIT-SCHEDULING-FLAG)
				    (%MAKE-POINTER-OFFSET DTP-LOCATIVE	;above doesn't work
				      'INHIBIT-SCHEDULING-FLAG 1)
				    ))
			      (FERROR NIL "Where's my binding of INHIBIT-SCHEDULING-FLAG ?"))
			  (%P-STORE-CONTENTS		;Leave bound to NIL, not T
			    (ALOC SP (1- SPP))		;Without clobbering the flag bit
			    NIL))))))
	  (COND ((= STATE SG-STATE-RESUMABLE)		;Safe state to interrupt
		 (EH:SG-MAYBE-GROW-PDLS STACK-GROUP NIL 200 100) ;Make space with no typeout
		 (EH:SG-SAVE-STATE STACK-GROUP T)	;Save M-T, microcode state
		 (EH:SG-OPEN-CALL-BLOCK STACK-GROUP 0 FUNCTION)
		 ;(SETF (SG-FLAGS-QBBFL STACK-GROUP) 0)	;SG-ENTER-CALL won't do it
							;but SG-SAVE-STATE does it
		 (DOLIST (ARG ARGS)
		   (EH:SG-REGPDL-PUSH ARG STACK-GROUP))
		 (%P-STORE-CDR-CODE (ALOC (SG-REGULAR-PDL STACK-GROUP)	;Terminate arg list
					  (SG-REGULAR-PDL-POINTER STACK-GROUP))
				    CDR-NIL)
		 (SETF (SG-CURRENT-STATE STACK-GROUP) SG-STATE-INVOKE-CALL-ON-RETURN)
		 (RETURN))))		;Interrupt will go off when process next scheduled
	(PROCESS-WAIT "Interruptible" #'(LAMBDA (P S)
					  ( (SG-CURRENT-STATE (PROCESS-STACK-GROUP P)) S))
				      SELF STATE))))

(DEFMETHOD (PROCESS :FLUSH) ()
  "Put a process into 'flushed' state.  The process will remain flushed until it
is reset."
  (COND ((EQ SELF CURRENT-PROCESS))
	(T
	 (SETQ WHOSTATE "Flushed")
	 (SET-PROCESS-WAIT SELF #'FALSE NIL))))

(DEFUN PROCESS-BLAST (&OPTIONAL (PROC CURRENT-PROCESS))
  "Blasting a process resets its wait function and argument list.  It is useful
when one of these generates an error."
  (SET-PROCESS-WAIT PROC #'FALSE NIL))

(DEFUN PROCESS-DISABLE (PROCESS)
  (WITHOUT-INTERRUPTS
    (SETF (PROCESS-RUN-REASONS PROCESS) NIL)
    (SETF (PROCESS-ARREST-REASONS PROCESS) NIL)
    (PROCESS-CONSIDER-RUNNABILITY PROCESS)))

(DEFUN PROCESS-ENABLE (PROCESS)
  (WITHOUT-INTERRUPTS
    (SETF (PROCESS-RUN-REASONS PROCESS) NIL)
    (SETF (PROCESS-ARREST-REASONS PROCESS) NIL)
    (FUNCALL PROCESS ':RUN-REASON ':ENABLE)))

(DEFUN PROCESS-RESET-AND-ENABLE (PROCESS)
  (WITHOUT-INTERRUPTS
   (FUNCALL PROCESS ':RESET)
   (PROCESS-ENABLE PROCESS)))

(DEFMETHOD (PROCESS :ACTIVE-P) ()
  (ASSQ SELF ACTIVE-PROCESSES))

(DEFMETHOD (PROCESS :RUNNABLE-P) ()
  (ASSQ SELF ACTIVE-PROCESSES))

(DEFUN PROCESS-CONSIDER-RUNNABILITY (&OPTIONAL (PROCESS SELF))
  (WITHOUT-INTERRUPTS
   (COND ((OR (PROCESS-ARREST-REASONS PROCESS) (NULL (PROCESS-RUN-REASONS PROCESS)))
	  ;; Process is arrested, better not be active
	  (LET ((APE (ASSQ PROCESS ACTIVE-PROCESSES)))
	    (AND APE (RPLACA APE NIL))
	    (PROCESS-ORDER-ACTIVE-PROCESSES)
	    (TV:WHO-LINE-RUN-STATE-UPDATE)))
	 ((ASSQ PROCESS ACTIVE-PROCESSES))
	 (T
	  (PROCESS-ACTIVE-ENTRY PROCESS)
	  ;; If process's stack group is in a bad state,
	  ;; make it wait instead of actually running (unless it's current!).
	  ;; ACTIVE is a bad state for a process which isn't running!
	  (AND (NOT (PROCESS-SIMPLE-P PROCESS))
	       (LET ((STATE (SG-CURRENT-STATE (PROCESS-STACK-GROUP PROCESS))))
		    (OR (= STATE SG-STATE-ERROR)
			(= STATE SG-STATE-ACTIVE)
			(= STATE SG-STATE-EXHAUSTED)))
	       CURRENT-PROCESS			 ;Prevents lossage in PROCESS-INITIALIZE
	       (FUNCALL PROCESS ':FLUSH))
	  (TV:WHO-LINE-RUN-STATE-UPDATE)))))

(DEFMETHOD (PROCESS :RUN-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (COND ((NOT (MEMQ REASON RUN-REASONS))
	   (PUSH REASON RUN-REASONS)
	   (PROCESS-CONSIDER-RUNNABILITY)))))

(DEFMETHOD (PROCESS :REVOKE-RUN-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (SETQ RUN-REASONS (DELQ REASON RUN-REASONS))
    (PROCESS-CONSIDER-RUNNABILITY)))

(DEFMETHOD (PROCESS :ARREST-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (COND ((NOT (MEMQ REASON ARREST-REASONS))
	   (PUSH REASON ARREST-REASONS)
	   (PROCESS-CONSIDER-RUNNABILITY)))))

(DEFMETHOD (PROCESS :REVOKE-ARREST-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (SETQ ARREST-REASONS (DELQ REASON ARREST-REASONS))
    (PROCESS-CONSIDER-RUNNABILITY)))

(DEFMETHOD (PROCESS :KILL) ()
  (FUNCALL-SELF ':RESET ':ALWAYS T))

;;; Priority and quantum stuff
(DEFMETHOD (PROCESS :SET-QUANTUM) (NEW-QUANTUM)
  (CHECK-ARG NEW-QUANTUM NUMBERP "a number")
  (SETQ QUANTUM NEW-QUANTUM))

(DEFMETHOD (PROCESS :SET-PRIORITY) (NEW-PRIORITY)
  (CHECK-ARG NEW-PRIORITY NUMBERP "a number")
  (WITHOUT-INTERRUPTS
    (SETQ PRIORITY NEW-PRIORITY)
    (AND (ASSQ SELF ACTIVE-PROCESSES)
	 (PROCESS-ACTIVE-ENTRY SELF))))

(DEFUN PROCESS-ORDER-ACTIVE-PROCESSES ()
  "Imposes an ordering on active processes for the priority mechanism.  Order is
from highest to lowest priority.  Priorities are simply compared numerically.  This
function MUST be called with interrupts inhibited."
  (AND (FBOUNDP 'SORT-SHORT-LIST) ;Cold-load!
       (SETQ ACTIVE-PROCESSES (SORT-SHORT-LIST ACTIVE-PROCESSES
					       #'(LAMBDA (P1 P2)
						   (COND ((NULL (FIRST P1)) (NULL (FIRST P2)))
							 ((NULL (FIRST P2)) T)
							 (T (> (FOURTH P1)
							       (FOURTH P2)))))))))

;;; This is for the error handler
(DEFMETHOD (PROCESS :COROUTINE-STACK-GROUPS) () NIL)

(DEFMETHOD (COROUTINING-PROCESS :ADD-COROUTINE-STACK-GROUP) (STACK-GROUP)
  (OR (MEMQ STACK-GROUP COROUTINE-STACK-GROUPS)
      (PUSH STACK-GROUP COROUTINE-STACK-GROUPS)))

;;; Miscellaneous process synchronization functions

(DEFUN PROCESS-ALLOW-SCHEDULE ()
  (SETF (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS) -1)
  (FUNCALL SCHEDULER-STACK-GROUP)
  (TV:WHO-LINE-RUN-STATE-UPDATE))

;; Takes one argument, a number of 60ths of a second for which to sleep.
(DEFUN PROCESS-SLEEP (INTERVAL &OPTIONAL (WHOSTATE "Sleep"))
  (PROCESS-WAIT WHOSTATE #'(LAMBDA (START-TIME INTERVAL)
			     ( (TIME-DIFFERENCE (TIME) START-TIME)
			        INTERVAL))
		(TIME) INTERVAL))

;;; Returns T if condition is true, NIL if you time out.
(DEFUN PROCESS-WAIT-WITH-TIMEOUT (WHOSTATE TIME FUNCTION &REST ARGS)
  (PROCESS-WAIT WHOSTATE #'(LAMBDA (START-TIME TIME FUNCTION ARGS)
			     (OR (APPLY FUNCTION ARGS)
				 ( (TIME-DIFFERENCE (TIME) START-TIME) TIME)))
		(TIME) TIME FUNCTION ARGS)
  (NOT (NULL (APPLY FUNCTION ARGS))))

(DEFUN PROCESS-WAIT-FOREVER ()
    (PROCESS-WAIT "Wait forever" #'FALSE))

;; A lock may be any cell.  When a lock is in the unlocked state, the cell
;; contains NIL; otherwise the cell contains the process which locked the lock.
;; A lock is referred to by a locative pointer to the cell.

;; Lock the given lock, blocking until it is sucessfully locked.
(DEFUN PROCESS-LOCK (LOCATIVE-POINTER &OPTIONAL LOCK-VALUE (WHOSTATE "Lock"))
  (OR LOCK-VALUE (SETQ LOCK-VALUE CURRENT-PROCESS))
  (DO ((LOCKER (CAR LOCATIVE-POINTER) (CAR LOCATIVE-POINTER)))
      ((%STORE-CONDITIONAL LOCATIVE-POINTER NIL LOCK-VALUE))
    (AND (EQ LOCKER LOCK-VALUE)
	 (FERROR NIL "Lock ~S already locked by this process" LOCATIVE-POINTER))
    (PROCESS-WAIT WHOSTATE
		  #'(LAMBDA (BAD-CONTENTS POINTER)
		      (NEQ (CAR POINTER) BAD-CONTENTS))
		  LOCKER
		  LOCATIVE-POINTER)))

;; Unlock the given lock.  The unlocker must be the same as the locker.
(DEFUN PROCESS-UNLOCK (LOCATIVE-POINTER &OPTIONAL (LOCK-VALUE CURRENT-PROCESS))
  (OR (%STORE-CONDITIONAL LOCATIVE-POINTER LOCK-VALUE NIL)
      (FERROR NIL "Attempt to unlock ~S, which you don't have locked" LOCATIVE-POINTER)))

;;; The scheduler

;;; The processes on ACTIVE-PROCESSES are sorted according to priority.
;;; A process is runnable if its flush instruction returns non-NIL.

;;; This function runs in the scheduler stack group.  Its job is to decide which
;;; process is to be run next.  It does this in the following way:

;;; If the current process is runnable, it has not exceeded its quantum, and
;;; no higher priority task is runnable, then it is run.  If not, the queue
;;; is searched from left to right for the highest
;;; priority process that is runnable and has not been run in a while.  This
;;; process is then run for its quantum.

;;; The scheduler also knows about a clock queue.  Every time the clock ticks,
;;; the queue is inspected for entries which should be run.  If any are found,
;;; they are run and the entry is deactivated.

(DEFUN PROCESS-SCHEDULER (&AUX (INHIBIT-SCHEDULING-FLAG T));No seq breaks in the scheduler
  (DO ((CURRENT-PRIORITY -1_20.)	;Priority of CURRENT-PROCESS
       (REMAINING-QUANTUM 0 0)
       (NEXT-PROCESS NIL NIL)
       (THIS-TIME (TIME) (TIME))
       (LAST-TIME (TIME) THIS-TIME)
       (DELTA-TIME)
       (NEXT-WHO-TIME 0))
      (())

    (SETQ DELTA-TIME (TIME-DIFFERENCE THIS-TIME LAST-TIME))

    (AND CURRENT-PROCESS
	 (SETF (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS)
	       (SETQ REMAINING-QUANTUM
		     (- (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS) DELTA-TIME))))

    (COND ((> DELTA-TIME 0)
	   ;; Run clock queue no more often than every 1/60 second.
	   (DOLIST (E CLOCK-FUNCTION-LIST)
	     (CATCH-ERROR (FUNCALL E DELTA-TIME) NIL))
	   (COND ((MINUSP (SETQ NEXT-WHO-TIME (- NEXT-WHO-TIME DELTA-TIME)))
		  (AND (FBOUNDP 'TV:WHO-LINE-UPDATE)
		       (CATCH-ERROR (TV:WHO-LINE-UPDATE) NIL))
		  (SETQ NEXT-WHO-TIME 60.)))))

    (DO ((PROCS ACTIVE-PROCESSES (CDR PROCS))
	 (APE))
	(NIL)
      ;; If no runnable process found, do idle stuff
      (COND ((NULL (FIRST (SETQ APE (CAR PROCS))))
	     (SETQ CURRENT-PRIORITY -1_20.)
	     (OR INHIBIT-IDLE-SCAVENGING-FLAG
		 (%GC-SCAVENGE GC-IDLE-SCAVENGE-QUANTUM))
	     (RETURN))
	    ;; Consider all processes of higher priority than current one.
	    ((> (FOURTH APE) CURRENT-PRIORITY)
	     (AND (LET ((CURRENT-PROCESS (FIRST APE)))
		    (APPLY (SECOND APE) (THIRD APE)))
		  (RETURN (SETQ NEXT-PROCESS (FIRST APE)))))
	    ;; Skip all processes of same priority earlier in the list than
	    ;; the current one, so that we have round-robin.
	    ((EQ (FIRST APE) CURRENT-PROCESS)
	     (AND (PLUSP REMAINING-QUANTUM)
		  (APPLY (SECOND APE) (THIRD APE))
		  (RETURN (SETQ NEXT-PROCESS CURRENT-PROCESS)))
	     (SETF (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS) -1)
	     (SETQ CURRENT-PRIORITY -1_20.))))	;Any process acceptable now.

    (COND (NEXT-PROCESS
	   ;; If old quantum used up, give some more
	   (OR (PLUSP (PROCESS-QUANTUM-REMAINING NEXT-PROCESS))
	       (SETF (PROCESS-QUANTUM-REMAINING NEXT-PROCESS) (PROCESS-QUANTUM NEXT-PROCESS)))
	   (SETQ CURRENT-PRIORITY (PROCESS-PRIORITY NEXT-PROCESS))
	   (SETF (PROCESS-WHOSTATE NEXT-PROCESS) "RUN")
	   (SET-PROCESS-WAIT NEXT-PROCESS #'TRUE NIL)
	   (%XBUS-WRITE TV:WHO-LINE-RUN-LIGHT-LOC 37777777)
	   (LET ((SG (PROCESS-STACK-GROUP (SETQ CURRENT-PROCESS NEXT-PROCESS))))
	     (IF (= (%DATA-TYPE SG) DTP-STACK-GROUP)
		 (STACK-GROUP-RESUME SG NIL)
		 (FUNCALL SG)))
	   (%XBUS-WRITE TV:WHO-LINE-RUN-LIGHT-LOC 0)
	   ;; Remember stack group of process last run
	   (OR (PROCESS-SIMPLE-P CURRENT-PROCESS)
	       (SETF (PROCESS-STACK-GROUP CURRENT-PROCESS)
		     %CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP)))
	  (T (SETQ CURRENT-PROCESS NIL)))

    ;; In case we took a page fault, the microcode will turn the run light on.
    ;; So turn it back off...this is a kind of kludge, but...
    (%XBUS-WRITE TV:WHO-LINE-RUN-LIGHT-LOC 0)))

;;; PROCESS-RUN-FUNCTION and associated hair

;This is a list of processes which may be recycled by PROCESS-RUN-FUNCTION
;It exists to avoid excess consing of stacks and reclaiming of them via
;the ordinary garbage collector.
(DEFVAR PROCESS-RUN-FUNCTION-SPARE-PROCESSES NIL)

;; Run a function in its own process
(DEFUN PROCESS-RUN-FUNCTION (NAME FUNCTION &REST ARGS)
  "Run a function in its own process.  The process is flushed if the machine
is warm booted."
  (PROCESS-RUN-FUNCTION-1 NIL NAME FUNCTION ARGS T))

(DEFUN PROCESS-RUN-TEMPORARY-FUNCTION (NAME FUNCTION &REST ARGS)
  "Run a function in its own process.  The process is reset, and made available for reuse,
when the machine is booted."
  (PROCESS-RUN-FUNCTION-1 #'PROCESS-RUN-FUNCTION-WARM-BOOT-RESET NAME FUNCTION ARGS NIL))

(DEFUN PROCESS-RUN-RESTARTABLE-FUNCTION (NAME FUNCTION &REST ARGS)
  "Run a function in its own process.  The process is reset and restarted when the machine
is warm booted."
  (PROCESS-RUN-FUNCTION-1 #'PROCESS-WARM-BOOT-DELAYED-RESTART NAME FUNCTION ARGS T))

;;; This thing wants a better name
(DEFUN PROCESS-RUN-FUNCTION-EXTENDED (NAME OPTIONS FUNCTION &REST ARGS)
  "Runs a function in its own process.  The caller can specify options regarding the process:
   :RESTART-AFTER-BOOT -- recall function after warm boot
   :RESTART-AFTER-RESET -- recall function after a PROCESS-RESET (possibly Control-ABORT)
   "
  (OR (LISTP OPTIONS) (SETQ OPTIONS (NCONS OPTIONS)))
  (PROCESS-RUN-FUNCTION-1 (IF (MEMQ ':RESTART-AFTER-BOOT OPTIONS)
			      #'PROCESS-WARM-BOOT-DELAYED-RESTART
			      #'PROCESS-RUN-FUNCTION-WARM-BOOT-RESET)
			  NAME FUNCTION ARGS
			  (MEMQ ':RESTART-ON-RESET OPTIONS)))

(DEFUN PROCESS-RUN-FUNCTION-1 (WARM-BOOT-ACTION NAME FUNCTION ARGS RESTART-ON-RESET
			       &AUX PROCESS)
  (SETQ PROCESS (WITHOUT-INTERRUPTS (OR (POP PROCESS-RUN-FUNCTION-SPARE-PROCESSES)
					(MAKE-PROCESS NAME
						      ':SPECIAL-PDL-SIZE 4000
						      ':REGULAR-PDL-SIZE 15000))))
  (SETF (PROCESS-NAME PROCESS) NAME)
  (SETF (PROCESS-WARM-BOOT-ACTION PROCESS) WARM-BOOT-ACTION)
  (SETF (SG-NAME (PROCESS-INITIAL-STACK-GROUP PROCESS)) NAME)
  (LEXPR-FUNCALL #'PROCESS-PRESET PROCESS
                 #'PROCESS-RUN-FUNCTION-INTERNAL RESTART-ON-RESET FUNCTION ARGS)
  (PROCESS-ENABLE PROCESS)
  PROCESS)

(DEFUN PROCESS-RUN-FUNCTION-INTERNAL (RESTART-ON-RESET FUNCTION &REST ARGS)
  (OR RESTART-ON-RESET (PROCESS-PRESET CURRENT-PROCESS
				       #'PROCESS-RUN-FUNCTION-INTERNAL-FLUSH-PROCESS))
  (*CATCH 'SYS:COMMAND-LEVEL (APPLY FUNCTION ARGS))
  ;; When the function returns, disable this process and make it available
  ;; for re-use.
  (PROCESS-RUN-FUNCTION-INTERNAL-FLUSH-PROCESS))

(DEFUN PROCESS-RUN-FUNCTION-INTERNAL-FLUSH-PROCESS ()
  (PROCESS-FLUSH-BACKGROUND-STREAM)
  (WITHOUT-INTERRUPTS
    (OR (MEMQ CURRENT-PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES)
	(PUSH CURRENT-PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES))
    (FUNCALL CURRENT-PROCESS ':KILL)))

(DEFUN PROCESS-RUN-FUNCTION-WARM-BOOT-RESET (PROCESS)
  (PROCESS-WARM-BOOT-RESET PROCESS)
  (OR (MEMQ PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES)
      (PUSH PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES)))

(DEFUN PROCESS-WARM-BOOT-RESET (PROCESS)
  (WITHOUT-INTERRUPTS
    (FUNCALL PROCESS ':PRESET #'(LAMBDA ()
				  (FUNCALL CURRENT-PROCESS ':KILL)
				  (PROCESS-WAIT-FOREVER)))
    (FUNCALL PROCESS ':RESET)
    (PROCESS-ENABLE PROCESS)))

(DEFUN PROCESS-WARM-BOOT-RESTART (PROCESS)
  (PROCESS-RESET PROCESS))

;Like PROCESS-WARM-BOOT-RESTART but doesn't allow it to run until after
;initialization is complete.
(DEFUN PROCESS-WARM-BOOT-DELAYED-RESTART (PROCESS)
  (PUSH (CONS PROCESS (PROCESS-RUN-REASONS PROCESS)) DELAYED-RESTART-PROCESSES)
  (SETF (PROCESS-RUN-REASONS PROCESS) NIL)
  (PROCESS-CONSIDER-RUNNABILITY PROCESS)
  (PROCESS-RESET PROCESS))			;Won't actually unwind until given run reason

(DEFUN SB-ON (&OPTIONAL (WHEN 'JUST-SHOW-CURRENT-STATE)
	      &AUX MASK TEM
	      (ALIST '( (:CALL . 1) (:UNIBUS . 2) (:KEYBOARD . 2) ;old name still supported.
		        (:CHAOS . 4) (:CLOCK . 10) )))
  "Sets the sequence break enable flags:
	The argument can be a keyword, a list of keywords, or a numeric mask.
	Keywords are: :CALL, :UNIBUS, :CHAOS, :CLOCK
	With no argument, just returns a list of keywords for what is enabled.
	Argument of NIL means turn off sequence breaks."
  (COND ((NUMBERP WHEN) (SETQ MASK WHEN))
	((NULL WHEN) (SETQ MASK 0))
	((EQ WHEN 'JUST-SHOW-CURRENT-STATE) (SETQ MASK %SEQUENCE-BREAK-SOURCE-ENABLE))
	((ATOM WHEN)
	 (OR (SETQ MASK (CDR (ASSQ WHEN ALIST)))
	     (FERROR NIL "~S invalid keyword.  Use :CALL, :UNIBUS, :CHAOS, or :CLOCK"
		         WHEN)))
	(T (SETQ MASK 0)
	   (DOLIST (KWD WHEN)
	     (IF (SETQ TEM (CDR (ASSQ KWD ALIST)))
		 (SETQ MASK (LOGIOR MASK TEM))
		 (FERROR NIL "~S invalid keyword.  Use :CALL, :UNIBUS, :CHAOS, or :CLOCK"
			     KWD)))))
  (SETQ %SEQUENCE-BREAK-SOURCE-ENABLE MASK)
  (DO ((L NIL)
       (B 1 (LSH B 1)))
      ((ZEROP MASK) L)
    (AND (BIT-TEST B MASK)
	 (PUSH (IF (SETQ TEM (CAR (RASSOC B ALIST))) TEM B) L))
    (SETQ MASK (BOOLE 2 B MASK))))

;;; Initialization

(DEFUN PROCESS-INITIALIZE ()
 (COND ((NOT SCHEDULER-EXISTS)
	(OR (FBOUNDP 'MOUSE-WAKEUP)
	    (FSET 'MOUSE-WAKEUP #'TRUE))
	(SETQ SCHEDULER-STACK-GROUP (MAKE-STACK-GROUP "Scheduler" ':SAFE 0))
	(SETQ INITIAL-PROCESS
	      (MAKE-PROCESS "Initial Process"
			    ':STACK-GROUP %CURRENT-STACK-GROUP
			    ':INITIAL-STACK-GROUP %CURRENT-STACK-GROUP
			    ':INITIAL-FORM '(LISP-TOP-LEVEL2)
			    ':WARM-BOOT-ACTION 'PROCESS-WARM-BOOT-RESTART))))

 ;; Below is done every time the machine starts up (warm or cold).  Unfortunately,
 ;; the state of the current process has been lost, so it must be reset without
 ;; unwinding it.  This is a total loss, but the only way to prevent this
 ;; is to prevent warm booting.  WARM BOOTING IS STRONGLY DISCOURAGED.
 (COND ((AND (BOUNDP 'CURRENT-PROCESS)
	     CURRENT-PROCESS)
	(SETQ WARM-BOOTED-PROCESS CURRENT-PROCESS)
	(FUNCALL (PROG1 CURRENT-PROCESS (SETQ CURRENT-PROCESS NIL))
		 ':RESET T)  ;T means NOUNWIND
	(PROCESS-DISABLE WARM-BOOTED-PROCESS))
       (T (SETQ WARM-BOOTED-PROCESS NIL)))
 (SETQ CURRENT-PROCESS INITIAL-PROCESS)
 (PROCESS-ENABLE INITIAL-PROCESS)		;enable even if warm-booted out of
 ;; Do to all active processes what they want done to them
 (PROCESS-ORDER-ACTIVE-PROCESSES)
 (DOLIST (P ACTIVE-PROCESSES)
   (AND (NULL (SETQ P (CAR P)))
	(RETURN T))
   (OR (AND (PROCESS-WARM-BOOT-ACTION P)
	    (ERRSET (FUNCALL (PROCESS-WARM-BOOT-ACTION P) P) NIL))
       (FUNCALL P ':FLUSH)))

 (SETQ INHIBIT-IDLE-SCAVENGING-FLAG T)			;Don't scavenge by default

 (SETQ %SCHEDULER-STACK-GROUP SCHEDULER-STACK-GROUP)
 (STACK-GROUP-PRESET SCHEDULER-STACK-GROUP #'PROCESS-SCHEDULER)
 (SETQ SCHEDULER-EXISTS T)
 (FUNCALL SCHEDULER-STACK-GROUP)
 (SETQ INHIBIT-SCHEDULING-FLAG NIL)
 (SB-ON ':CLOCK))

;;; Don't run this the first time, only when the system initializations normally get run
(ADD-INITIALIZATION "Process" '(PROCESS-INITIALIZE) '(SYSTEM NORMAL))

(COMPILE-FLAVOR-METHODS PROCESS SIMPLE-PROCESS)
