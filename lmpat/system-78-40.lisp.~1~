;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.40
;;; Reason: Make system processes have meaningful priorities
;;; Written 12/25/81 20:12:24 by HIC,
;;; while running on Basset from band 2
;;; with System 78.38, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.21, Canon 9.7, microcode 841.



; From file BASSTR.LISP >LMWIN POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

;;; This function is called, possibly in the keyboard process, when one of the
;;; standard asynchronous intercepted characters, of the sort that mungs over the
;;; process, is typed.  Scheduling is inhibited.
;;; This does the actual munging of the process in a separate process, in case
;;; it has to wait for the process' stack-group to get out of some weird state.
(DEFUN KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER (CHAR &AUX P)
  (KBD-ESC-CLEAR NIL)  ;Forget chars typed before "CTRL-abort", even those inside window's iob
  (AND (SETQ P SELECTED-WINDOW)			;Find process to be hacked
       (SETQ P (FUNCALL P ':PROCESS))
       (SELECTQ CHAR
	 ((#\ABORT #\ABORT)
	  (PROCESS-RUN-FUNCTION '(:NAME "Abort" :PRIORITY 50.) P ':INTERRUPT
				#'KBD-INTERCEPT-CHARACTER (DPB 0 %%KBD-CONTROL CHAR)))
	 (#\BREAK
	  (PROCESS-RUN-FUNCTION '(:NAME "Break" :PRIORITY 40.) P ':INTERRUPT 'BREAK 'BREAK))
	 (#\BREAK
	  (PROCESS-RUN-FUNCTION '(:NAME "Break" :PRIORITY 40.)
				P ':INTERRUPT %ERROR-HANDLER-STACK-GROUP
				'(:BREAK))))))

)

(FUNCALL TV:MOUSE-PROCESS ':SET-PRIORITY 30.)
(FUNCALL TV:KBD-PROCESS ':SET-PRIORITY 30.)
(FUNCALL CHAOS:RECEIVER ':SET-PRIORITY 35.)
(FUNCALL CHAOS:BACKGROUND ':SET-PRIORITY 25.)

; From file MOUSE.LISP >LMWIN POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFUN MOUSE-CALL-SYSTEM-MENU (&OPTIONAL (SUP MOUSE-SHEET))
  (PROCESS-RUN-FUNCTION '(:NAME "System Menu" :PRIORITY 10.)
			#'(LAMBDA (SUP)
			    (USING-RESOURCE (MENU SYSTEM-MENU SUP)
			      (FUNCALL MENU ':CHOOSE)))
			SUP))

)

; From file MOUSE.LISP >LMWIN POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

;;; Low-level routines

;;; MOUSE-INPUT blocks until the mouse status changes (it moves or a button
;;; is depressed or raised).  It then returns 6 values: delta-X, delta-Y,
;;; buttons-newly-pushed, buttons-newly-raised, and the relevant mouse X and Y
;;; (the current position normally, but if any buttons changed, the position then).
;;; There are 3 coordinate systems involved:
;;;  Table coordinates - physical motion of the mouse
;;;			 Speeds are expressed in terms of these.
;;;  Mouse coordinates - These are the scaled table coordinates.  Only deltas
;;;			 are valid here.
;;;  Screen coordinates - these are where the mouse-blinker appears on
;;;			 the TV screen; the same as mouse coordinates except
;;;			 for non-simple geometry caused by e.g. scroll bars.
;;;			 These appear in the variables MOUSE-X and MOUSE-Y
;;; Note that because of non-simple geometry, the deltas returned by MOUSE-INPUT
;;; are not necessarily equal to the change in MOUSE-X and MOUSE-Y.
(DEFUN MOUSE-INPUT (&OPTIONAL (WAIT-FLAG T))
  ;; Await a change in hardware status from what it was last time
  (COND (WAIT-FLAG
	 ;; This sleep makes it reasonable for the mouse process to be high priority.
	 ;; It insures that moving the mouse does not lock out lower priority
	 ;; processes for a long time.  This constant may want to be tweaked.  A
	 ;; value of 1 (1/60 of a second) does not noticably affect mouse response.
	 (PROCESS-SLEEP 1.)
	 (PROCESS-WAIT "MOUSE" #'(LAMBDA () (OR MOUSE-WAKEUP MOUSE-RECONSIDER)))))
  ;; Clear wakeup flag unless there are buffered mouse button transitions, since we
  ;; might not read all of them before calling MOUSE-INPUT again.
  (SETQ MOUSE-WAKEUP ( MOUSE-BUTTONS-BUFFER-IN-INDEX MOUSE-BUTTONS-BUFFER-OUT-INDEX))
  ;; Compute delta-X and delta-Y in screen coordinates
  (LET ((DELTA-X (- MOUSE-X MOUSE-LAST-X))
	(DELTA-Y (- MOUSE-Y MOUSE-LAST-Y))
	(GLITCH-X NIL) (GLITCH-Y NIL) NEW-BUTTONS CHANGED-BUTTONS)
    (INCF MOUSE-LAST-X DELTA-X)
    (INCF MOUSE-LAST-Y DELTA-Y)
    ;; Compute change in button status
    (MULTIPLE-VALUE (NEW-BUTTONS MOUSE-LAST-BUTTONS-TIME
		     MOUSE-LAST-BUTTONS-X MOUSE-LAST-BUTTONS-Y)
      (MOUSE-BUTTONS))
    (SETQ CHANGED-BUTTONS (LOGXOR NEW-BUTTONS MOUSE-LAST-BUTTONS)
	  MOUSE-LAST-BUTTONS NEW-BUTTONS)
    ;; Force blinker to stay within mouse-sheet.  If the mouse moves during this
    ;; computation, it will glitch back.  So we only SETQ the variables 
    ;; if the mouse position actually needs to be changed, rather than using
    ;; MAX and MIN which would be more readable.
    (IF (> 0 MOUSE-X)
	(SETQ GLITCH-X 0))
    (IF ( (SHEET-WIDTH MOUSE-SHEET) MOUSE-X)
	(SETQ GLITCH-X (1- (SHEET-WIDTH MOUSE-SHEET))))
    (IF (> 0 MOUSE-Y)
	(SETQ GLITCH-Y 0))
    (IF ( (SHEET-HEIGHT MOUSE-SHEET) MOUSE-Y)
	(SETQ GLITCH-Y (1- (SHEET-HEIGHT MOUSE-SHEET))))
    ;; If mouse blinker needs to be glitched, do so
    (IF (OR GLITCH-X GLITCH-Y)
	(WITHOUT-INTERRUPTS
	   (%OPEN-MOUSE-CURSOR)
	   (IF GLITCH-X
	       (SETQ MOUSE-LAST-X (SETQ MOUSE-X GLITCH-X)))
	   (IF GLITCH-Y
	       (SETQ MOUSE-LAST-Y (SETQ MOUSE-Y GLITCH-Y)))
	   (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
		 PREPARED-SHEET NIL)))
    (VALUES DELTA-X
	    DELTA-Y
	    (LOGAND NEW-BUTTONS CHANGED-BUTTONS)
	    (BOOLE 2 NEW-BUTTONS CHANGED-BUTTONS) ;BOOLE 2 is ANDCA
	    (IF (ZEROP CHANGED-BUTTONS) MOUSE-LAST-X MOUSE-LAST-BUTTONS-X)
	    (IF (ZEROP CHANGED-BUTTONS) MOUSE-LAST-Y MOUSE-LAST-BUTTONS-Y))))

)

; From file MOUSE.LISP >LMWIN POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFUN MOUSE-SELECT (WINDOW)
  (IF (EQ CURRENT-PROCESS MOUSE-PROCESS)
      (PROCESS-RUN-FUNCTION '(:NAME "Mouse select" :PRIORITY 20.) #'MOUSE-SELECT WINDOW)
      (FUNCALL WINDOW ':MOUSE-SELECT NIL)))

)

; From file MENU.LISP >LMWIN POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

;;; When no selection, but mouse moved out of range, deexpose menu
(DEFMETHOD (BASIC-MOMENTARY-MENU :AFTER :HANDLE-MOUSE) ()
  (OR CHOSEN-ITEM
      ;; Don't flush if mouse being usurped
      WINDOW-OWNING-MOUSE
      ;; Only flush us if either not explicitly flushing or we don't own mouse
      (AND MOUSE-RECONSIDER (EQ SELF (WINDOW-OWNING-MOUSE)))
      ;; This is called in the mouse process.  We don't want to take the chance that
      ;; we might go blocked, so run in another process.
      (PROCESS-RUN-FUNCTION '(:NAME "Menu Deactivate" :PRIORITY 20.) SELF ':DEACTIVATE)))

)
