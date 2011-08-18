;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.39
;;; Reason: New scheduler (must boot to have this take effect)
;;; Written 12/25/81 18:35:02 by HIC,
;;; while running on Basset from band 2
;;; with System 78.38, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.21, Canon 9.7, microcode 841.



; From file PROCES.LISP >LISPM2 POINTER:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

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
  (DO ((REMAINING-QUANTUM 0 0)
       (NEXT-PROCESS NIL NIL)
       (OLD-CURRENT-PROCESS)
       (THIS-TIME (TIME) (TIME))
       (LAST-TIME (TIME) THIS-TIME)
       (DELTA-TIME)
       (NEXT-WHO-TIME 0))
      (())

    (SETQ DELTA-TIME (TIME-DIFFERENCE THIS-TIME LAST-TIME)
	  OLD-CURRENT-PROCESS CURRENT-PROCESS)

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

    (DO-NAMED FOUND-PROCESS
	      ((PROCS ACTIVE-PROCESSES)
	       (THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED)
	       (FIRST-OF-THIS-PRIORITY)
	       (CURRENT-PRIORITY))
	      ((NULL (FIRST (CAR PROCS))))
      ;; Loop over all process of the current priority
      (SETQ CURRENT-PRIORITY (FOURTH (CAR PROCS))
	    FIRST-OF-THIS-PRIORITY PROCS)
      (DO (APE PRI PROC) (())
	(SETQ APE (CAR PROCS))
	(AND (OR (NULL (SETQ PROC (FIRST APE))) ( (SETQ PRI (FOURTH APE)) CURRENT-PRIORITY))
	     ;; Hit next priority level, or ran out of processes
	     (RETURN))
	(AND (COND ((LET ((CURRENT-PROCESS PROC))
		      (APPLY (SECOND APE) (THIRD APE)))
		    (SETQ THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED PROC)
		    T))
	     (PLUSP (PROCESS-QUANTUM-REMAINING PROC))
	     ;; It is runnable, and it has time remaining
	     (RETURN-FROM FOUND-PROCESS (SETQ NEXT-PROCESS PROC)))
	(SETQ PROCS (CDR PROCS)))
      ;; Ran out of all processes at current priority level.  Reset their quantums.
      (DO ((PS FIRST-OF-THIS-PRIORITY (CDR PS)))
	  ((EQ PS PROCS))
	(SETF (PROCESS-QUANTUM-REMAINING (FIRST (CAR PS)))
	      (PROCESS-QUANTUM (FIRST (CAR PS)))))
      ;; If a process would have run at this priority level, but couldn't becase
      (AND THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED
	   (RETURN-FROM FOUND-PROCESS
	     (SETQ NEXT-PROCESS THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED))))

    (COND ((NULL NEXT-PROCESS)
	   ;; No process to run, do idle time stuff
	   (OR INHIBIT-IDLE-SCAVENGING-FLAG
	       (%GC-SCAVENGE GC-IDLE-SCAVENGE-QUANTUM))))

    (COND (NEXT-PROCESS
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

)
