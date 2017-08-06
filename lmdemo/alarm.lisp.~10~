;; -*- Mode: Lisp; Base: 8.; Package: Hacks -*-

;; Alarm system for Lispms.  
;; Written by Dave Andre in August 1981.

;; The system is driven by the symbols on ALARM-LIST.
;; Any symbol on the list should have the following properties defined as functions:
;;
;;  CHECK:  This function is called periodically and should return a boolean which
;;  states whether an alarm condition exists.
;;  
;;  NOTIFY &optional dont-notify-p:  This function is called if the CHECK function
;;  has notified the alarm process that an alarm condition exists.  This function
;;  should update its knowledge to state that the user has been notified, and unless
;;  dont-notify-p is true, should notify the user of the occurance.  If it can also
;;  be determined that an alarm condition will no longer exist, this function may
;;  remove its associated alarm from ALARM-LIST.
;;
;;  RESET:  This function should reset the alarm's knowledge to its initial state.
;;  For example, the MAIL alarm checks for mail to various people.  This function
;;  would then remove any people from the alarm's knowledge.
;;
;;  ADD-ALARM new-condition:  This function's argument uniquely specifies a condition
;;  to check for.  Calling this function adds this condition to the alarm's knowledge.

(DEFVAR ALARM-LIST ())
(DEFVAR ALARM-LIST-LOCK NIL)

;; This is a list of alarms which should not signal a notification the next
;; time they are checked.
(DEFVAR ALARM-INHIBIT-NOTIFICATION-LIST ())

(DEFUN SET-ALARM-INHIBIT-NOTIFY (ALARM)
  (WITHOUT-INTERRUPTS
    (OR (MEMQ ALARM ALARM-INHIBIT-NOTIFICATION-LIST)
	(PUSH ALARM ALARM-INHIBIT-NOTIFICATION-LIST))))

;; The alarm process sleeps for this amount of time between checking ALARM-LIST.
;; Defaults to two minutes.
(DEFVAR ALARM-SLEEP-TIME 7200.)

;; This is where the alarm-process lives.  It runs at a lower priority than other processes.
(DEFVAR ALARM-PROCESS NIL)

;;; Entry functions

(DEFUN ADD-ALARM (ALARM)
  (CHECK-ARG ALARM
	     (GET ALARM 'CHECK)
	     "an alarm")
  (ACTIVATE-ALARM-PROCESS)
  (UNWIND-PROTECT
    (PROGN (PROCESS-LOCK (LOCF ALARM-LIST-LOCK) NIL "Alarm Lock")
	   (COND ((NOT (MEMQ ALARM ALARM-LIST))
		  (PUSH ALARM ALARM-LIST))))
    (%STORE-CONDITIONAL (LOCF ALARM-LIST-LOCK) CURRENT-PROCESS NIL)))

(DEFUN DELETE-ALARM (ALARM)
  (UNWIND-PROTECT
    (PROGN (PROCESS-LOCK (LOCF ALARM-LIST-LOCK) NIL "Alarm Lock")
	   (FUNCALL (GET ALARM 'RESET))
	   (SETQ ALARM-LIST (DELQ ALARM ALARM-LIST)))
    (%STORE-CONDITIONAL (LOCF ALARM-LIST-LOCK) CURRENT-PROCESS NIL)))

(DEFUN ACTIVATE-ALARM-PROCESS ()
  (OR ALARM-PROCESS
      (SETQ ALARM-PROCESS (MAKE-PROCESS "Alarm Background" ':PRIORITY -1)))
  (COND ((NULL (SI:PROCESS-RUN-REASONS ALARM-PROCESS))
	 (PROCESS-PRESET ALARM-PROCESS 'ALARM-BACKGROUND-TOP-LEVEL)
	 (PROCESS-ENABLE ALARM-PROCESS))))

(DEFUN DEACTIVATE-ALARM-PROCESS (&OPTIONAL RESET-ALARMS)
  (COND ((NOT (NULL ALARM-PROCESS))
	 (IF (EQ CURRENT-PROCESS ALARM-PROCESS)
	     (PROCESS-RUN-TEMPORARY-FUNCTION "Alarm Temp" 'DEACTIVATE-ALARM-PROCESS)
	     (COND (RESET-ALARMS
		    (SETQ ALARM-INHIBIT-NOTIFICATION-LIST NIL)
		    (DOLIST (ALARM ALARM-LIST)
		      (FUNCALL (GET ALARM 'RESET)))
		    (SETQ ALARM-LIST NIL)))
	     (FUNCALL ALARM-PROCESS ':KILL)))))

(ADD-INITIALIZATION "Deactivate Alarms"
		    '(DEACTIVATE-ALARM-PROCESS T)
		    '(LOGOUT))

;;; Internal workings.

(DEFVAR *CURRENT-ALARM*)
(DEFUN ALARM-BACKGROUND-TOP-LEVEL ()
  (DO () (NIL)
    (*CATCH 'SYS:COMMAND-LEVEL
      (DO ((NOTIFICATION-QUEUE NIL NIL)) (NIL)
	(LET ((USER-ID USER-ID) VAL ERR)
	  (AND (EQUAL USER-ID "")
	       (SETQ USER-ID "Alarm-Background"))
	  (UNWIND-PROTECT
	    (PROGN (PROCESS-LOCK (LOCF ALARM-LIST-LOCK) NIL "Alarm Lock")
		   (CONDITION-BIND ((NIL 'ALARM-CONDITION-HANDLER))
		     (DOLIST (*CURRENT-ALARM* ALARM-LIST)
		       (MULTIPLE-VALUE (VAL ERR)
			 (*CATCH 'ALARM (FUNCALL (GET *CURRENT-ALARM* 'CHECK))))
		       (COND (ERR (SETQ ALARM-LIST
					(DELQ *CURRENT-ALARM* ALARM-LIST))
				  (FUNCALL (GET *CURRENT-ALARM* 'RESET)))
			     (VAL (PUSH *CURRENT-ALARM* NOTIFICATION-QUEUE))))))
	    (%STORE-CONDITIONAL (LOCF ALARM-LIST-LOCK) CURRENT-PROCESS NIL))
	  (DOLIST (ALARM (NREVERSE NOTIFICATION-QUEUE))
	    (COND ((MEMQ ALARM ALARM-INHIBIT-NOTIFICATION-LIST)
		   (SETQ ALARM-INHIBIT-NOTIFICATION-LIST
			 (DELQ ALARM ALARM-INHIBIT-NOTIFICATION-LIST))
		   (FUNCALL (GET ALARM 'NOTIFY) T))
		  (T (FUNCALL (GET ALARM 'NOTIFY)))))
	  (PROCESS-SLEEP ALARM-SLEEP-TIME "Alarm Wait")
	  (OR ALARM-LIST (DEACTIVATE-ALARM-PROCESS)))))))

(DEFVAR WITHIN-ALARM-CONDITION-HANDLER NIL)
(DEFUN ALARM-CONDITION-HANDLER (CONDITION FORMAT-STRING &REST FORMAT-ARGS)
  (IF WITHIN-ALARM-CONDITION-HANDLER
      NIL					;Don't handle.
      (LET ((WITHIN-ALARM-CONDITION-HANDLER T))
	(TV:NOTIFY NIL
		   (WITH-OUTPUT-TO-STRING (S)
		     (FORMAT S "Error in the alarm process while checking ~A alarms.
Removing ~:*~A from the active alarm list.
The error was: " *CURRENT-ALARM*)
		     (IF (STRINGP FORMAT-STRING)
			 (LEXPR-FUNCALL #'FORMAT S FORMAT-STRING FORMAT-ARGS)
			 (FORMAT S "~A" CONDITION))))
	(*THROW 'ALARM NIL))))

;;; Alarm definitions.

;; File checks.  Notifies whenever the INFO of a file on this list changes.
(DEFVAR FILES-TO-BE-MONITORED ())
(DEFVAR FILES-TO-BE-MONITORED-PREVIOUS-PLIST ())
(DEFVAR FILES-TO-BE-NOTIFIED ())

(DEFUN PLIST-INFO (PLIST)
  (CONS (GET PLIST ':TRUENAME) (GET PLIST ':CREATION-DATE)))

(DEFUN (FILE CHECK) ()
  (LET ((PLIST (FS:MULTIPLE-FILE-PLISTS FILES-TO-BE-MONITORED)))
    (DOLIST (ENTRY PLIST)
      (COND ((NOT (EQUAL (PLIST-INFO (ASSOC (CAR ENTRY) FILES-TO-BE-MONITORED-PREVIOUS-PLIST))
			 (PLIST-INFO ENTRY)))
	     (PUSH (LIST (CAR ENTRY) (GET ENTRY ':CREATION-DATE))
		   FILES-TO-BE-NOTIFIED))))
    (SETQ FILES-TO-BE-MONITORED-PREVIOUS-PLIST PLIST)
    FILES-TO-BE-NOTIFIED))

(DEFUN (FILE NOTIFY) (&OPTIONAL DONT-NOTIFY-P)
  (OR DONT-NOTIFY-P
      (DOLIST (ENTRY FILES-TO-BE-NOTIFIED)
	(LEXPR-FUNCALL 'TV:NOTIFY NIL "File ~A modified at ~\TIME\" ENTRY)))
  (SETQ FILES-TO-BE-NOTIFIED NIL))

(DEFUN (FILE RESET) ()
  (SETQ FILES-TO-BE-MONITORED NIL
	FILES-TO-BE-MONITORED-PREVIOUS-PLIST NIL
	FILES-TO-BE-NOTIFIED NIL))

(DEFUN (FILE ADD-ALARM) (ALARM)
  (SETQ ALARM (FS:MERGE-PATHNAME-DEFAULTS ALARM))
  (OR (MEMQ ALARM FILES-TO-BE-MONITORED)
      (PUSH ALARM FILES-TO-BE-MONITORED)))

;; Mail checks.  Notifies whenever the mail file of a user on this list is updated, but not
;; if it's deleted.  Each entry must be of the form (user host filename), because
;; unfortunately there's no way to figure out the filename.
(DEFVAR MAIL-CHECK-USERS ())
(DEFVAR MAIL-CHECK-USERS-CREATION-DATE-ALIST ())
(DEFVAR MAIL-CHECK-USERS-WITH-NEW-MAIL ())

(DEFUN (MAIL CHECK) ()
  (DO ((U MAIL-CHECK-USERS (CDR U))
       (USER) (HOST) (FILENAME) (OLD-ENTRY) (PROBE) (CREATION-DATE))
      ((NULL U))
    (SETQ USER (CAAR U)
	  HOST (CADAR U)
	  FILENAME (CADDAR U)
	  OLD-ENTRY (ASSOC (CAR U) MAIL-CHECK-USERS-CREATION-DATE-ALIST))
    (COND ((NOT (STRINGP (SETQ PROBE (OPEN FILENAME '(:PROBE)))))
	   (SETQ CREATION-DATE (FUNCALL PROBE ':CREATION-DATE))
	   (COND ((OR (NULL (CDR OLD-ENTRY))
		      ( CREATION-DATE (CDR OLD-ENTRY)))
		  (IF OLD-ENTRY
		      (SETF (CDR OLD-ENTRY) CREATION-DATE)
		      (PUSH (SETQ OLD-ENTRY (CONS (CAR U) CREATION-DATE))
			    MAIL-CHECK-USERS-CREATION-DATE-ALIST))
		  (PUSH OLD-ENTRY MAIL-CHECK-USERS-WITH-NEW-MAIL))))
	  (T (IF OLD-ENTRY (SETF (CDR OLD-ENTRY) NIL)))))
  MAIL-CHECK-USERS-WITH-NEW-MAIL)

(DEFUN (MAIL NOTIFY) (&OPTIONAL DONT-NOTIFY-P)
  (OR DONT-NOTIFY-P
      (DOLIST (ENTRY MAIL-CHECK-USERS-WITH-NEW-MAIL)
	(TV:NOTIFY NIL
		   (WITH-OUTPUT-TO-STRING (S)
		     (COND ((AND (EQUAL (CAAR ENTRY) USER-ID)
				 (EQ (CADAR ENTRY) FS:USER-LOGIN-MACHINE)) ;Hosts are EQ.
			    (FUNCALL S ':STRING-OUT "You have new mail at "))
			   (T (FORMAT S "~A~:[@~A~] has new mail at "
				      (CAAR ENTRY)
				      (EQUAL (CADAR ENTRY) FS:USER-LOGIN-MACHINE)
				      (CADAR ENTRY))))
		     (TIME:PRINT-UNIVERSAL-TIME (CDR ENTRY) S)))))
  (SETQ MAIL-CHECK-USERS-WITH-NEW-MAIL NIL))

(DEFUN (MAIL RESET) ()
  (SETQ MAIL-CHECK-USERS NIL
	MAIL-CHECK-USERS-CREATION-DATE-ALIST NIL
	MAIL-CHECK-USERS-WITH-NEW-MAIL NIL))

(DEFUN (MAIL ADD-ALARM) (ALARM)
  (PUSH ALARM MAIL-CHECK-USERS))

;; Make alarm go off at a certain time.
(DEFVAR ALARM-TIMES NIL)			;Alist (ut . message)

(DEFUN (TIME CHECK) ()
  (> (TIME:GET-UNIVERSAL-TIME) (CAAR ALARM-TIMES)))

(DEFUN (TIME NOTIFY) (&OPTIONAL IGNORE
		      &AUX (CURRENT-TIME (TIME:GET-UNIVERSAL-TIME)))
  (DO ((A ALARM-TIMES (CDR A)))
      ((NULL A)
       (SETQ ALARM-TIMES NIL)
       (DELETE-ALARM 'TIME))
    (IF ( CURRENT-TIME (CAAR A))
	(TV:NOTIFY NIL (CDAR A))
	(RETURN (SETQ ALARM-TIMES A)))))

(DEFUN (TIME RESET) ()
  (SETQ ALARM-TIMES NIL))

(DEFUN (TIME ADD-ALARM) (ALARM)
  (WITHOUT-INTERRUPTS
    (SETQ ALARM-TIMES (SORTCAR (CONS ALARM ALARM-TIMES) #'<))))
	     
;; Make alarm go off when the status of a specified host changes.
;; As far as this program is concerned, there are three possible statuses for hosts:
;; UP, DOWN, or going down in a certain amount of time.  The status on the alist is
;; therefore always UP, DOWN, or a universal time of a planned shutdown.
;; Unfortunately no code exists to check for planned shutdowns, but when it does,
;; the only thing which should have to be modified is the CHECK function.

;; This us a user variable, and it overrides the inhibit notification stuff.
(DEFVAR ALWAYS-NOTIFY-IF-HOST-NOT-UP T)

(DEFVAR HOSTS-TO-CHECK NIL)
(DEFVAR HOSTS-TO-CHECK-LOCK NIL)
(DEFVAR HOSTS-CURRENT-STATUS NIL)
(DEFVAR HOSTS-WITH-NEW-STATUS NIL)

(DEFUN (HOSTS CHECK) (&AUX CONNECTIONS CURRENT-STATUS)
  (UNWIND-PROTECT
    (PROGN (PROCESS-LOCK (LOCF HOSTS-TO-CHECK-LOCK))
	   (SETQ CONNECTIONS (MAKE-LIST (LENGTH HOSTS-TO-CHECK)))
	   (CHAOS:ASSURE-ENABLED)
	   (DO ((H HOSTS-TO-CHECK (CDR H))
		(C CONNECTIONS (CDR C)))
	       ((NULL C))
	     (SETF (CAR C) (CHAOS:OPEN-CONNECTION (CAR H) "STATUS" 1)))
	   (SETQ HOSTS-TO-CHECK-LOCK NIL)
	   ;; Wait a maximum of 5 seconds for the replys to come in.
	   (PROCESS-WAIT-WITH-TIMEOUT "Host Status" 600.
	     #'(LAMBDA (CONNS)
		 (DO ((C CONNS (CDR C)))
		     ((NULL C) T)
		   (AND (EQ (CHAOS:STATE (CAR C)) 'CHAOS:RFC-SENT-STATE)
			(RETURN NIL))))
	     CONNECTIONS)
	   (DO ((C CONNECTIONS (CDR C)))
	       ((NULL C))
	     (SETQ CURRENT-STATUS (CDR (ASSQ (CHAOS:FOREIGN-ADDRESS (CAR C))
					     HOSTS-CURRENT-STATUS)))
	     (SELECTQ (CHAOS:STATE (CAR C))
	       ((CHAOS:RFC-SENT-STATE CHAOS:HOST-DOWN-STATE)
		(COND ((NEQ CURRENT-STATUS 'DOWN)
		       (PUSH (CONS (CHAOS:FOREIGN-ADDRESS (CAR C)) 'DOWN)
			     HOSTS-WITH-NEW-STATUS))))
	       (OTHERWISE
		(COND ((NEQ CURRENT-STATUS 'UP)
		       (PUSH (CONS (CHAOS:FOREIGN-ADDRESS (CAR C)) 'UP)
			     HOSTS-WITH-NEW-STATUS)))))))
    (%STORE-CONDITIONAL (LOCF HOSTS-TO-CHECK-LOCK) CURRENT-PROCESS NIL)
    (DOLIST (C CONNECTIONS)
      (CHAOS:REMOVE-CONN C)))
  HOSTS-WITH-NEW-STATUS)

(DEFUN (HOSTS NOTIFY) (&OPTIONAL DONT-NOTIFY-P &AUX TEM)
  (DOLIST (H HOSTS-WITH-NEW-STATUS)
    (AND (OR (NOT DONT-NOTIFY-P)
	     (AND ALWAYS-NOTIFY-IF-HOST-NOT-UP
		  (NEQ (CDR H) 'UP)))
	 (TV:NOTIFY NIL
	   (WITH-OUTPUT-TO-STRING (S)
	     (FUNCALL S ':STRING-OUT (CHAOS:HOST-SHORT-NAME (CAR H)))
	     (COND ((NUMBERP (CDR H))
		    (FUNCALL S ':STRING-OUT " is going down at ")
		    (TIME:PRINT-UNIVERSAL-TIME (CDR H) S))
		   (T (FUNCALL S ':STRING-OUT
			       (COND ((EQ (CDR H) 'UP) " is up.")
				     (T " is down."))))))))
    (COND ((SETQ TEM (ASSOC (CAR H) HOSTS-CURRENT-STATUS))
	   (RPLACD TEM (CDR H)))
	  (T (PUSH H HOSTS-CURRENT-STATUS))))
  (SETQ HOSTS-WITH-NEW-STATUS NIL))

(DEFUN (HOSTS RESET) ()
  (SETQ HOSTS-TO-CHECK NIL
	HOSTS-TO-CHECK-LOCK NIL
	HOSTS-CURRENT-STATUS NIL
	HOSTS-WITH-NEW-STATUS NIL))

(DEFUN (HOSTS ADD-ALARM) (NEW-ALARM &AUX HOST)
  (COND ((NUMBERP NEW-ALARM) (SETQ HOST NEW-ALARM))
	((SETQ HOST (CHAOS:ADDRESS-PARSE NEW-ALARM)))
	(T (FERROR NIL "~S is not a known host." NEW-ALARM)))
  (UNWIND-PROTECT
    (PROGN (PROCESS-LOCK (LOCF HOSTS-TO-CHECK-LOCK))
	   (OR (MEMQ HOST HOSTS-TO-CHECK)
	       (PUSH HOST HOSTS-TO-CHECK)))
    (%STORE-CONDITIONAL (LOCF HOSTS-TO-CHECK-LOCK) CURRENT-PROCESS NIL)))

;; Make alarm go off when a Lispm frees up.
;; Here CHAOS:FINGER-ALL-LMS does all the work for us.
(DEFVAR FREE-LISPMS NIL)			;Good default value.
(DEFVAR NEW-FREE-LISPMS NIL)

(DEFUN (LISPM CHECK) ()
  (LET ((LISPMS (CHAOS:FINGER-ALL-LMS 'IGNORE NIL T)))
    (DOLIST (LISPM LISPMS)
      (COND ((NOT (MEMBER LISPM FREE-LISPMS))
	     (PUSH LISPM NEW-FREE-LISPMS))))
    (SETQ FREE-LISPMS LISPMS)))

(DEFUN (LISPM NOTIFY) (&OPTIONAL DONT-NOTIFY-P)
  (DOLIST (LISPM NEW-FREE-LISPMS)
    (OR DONT-NOTIFY-P
	(TV:NOTIFY NIL "~A is free." LISPM)))
  (SETQ NEW-FREE-LISPMS NIL))

;; These properties are inappropriate, because this alarm only searches
;; for one type of alarm.
(DEFPROP LISPM IGNORE RESET)
(DEFPROP LISPM IGNORE NEW-ALARM)

;; Make alarm go off when the status of somebody logged in somewhere changes.

;; Make alarm go off if the dover status changes.


;;; User interface functions.
;;; These are the functions that a user would normally call in an init file.
;;; Someday write a mouse interface.

(DEFUN BACKGROUND-CHECK-MAIL (USER HOST FILENAME &OPTIONAL NOTIFY-INITIALLY-P)
  (SETQ HOST (SI:PARSE-HOST HOST))
  (FUNCALL (GET 'MAIL 'ADD-ALARM) (LIST USER HOST (FS:MERGE-PATHNAME-DEFAULTS FILENAME)))
  (OR NOTIFY-INITIALLY-P
      (SET-ALARM-INHIBIT-NOTIFY 'MAIL))
  (ADD-ALARM 'MAIL)
  T)

(DEFUN BACKGROUND-CHECK-FILES (NOTIFY-INITIALLY-P &REST FILES)
  (DOLIST (FILE FILES)
    (FUNCALL (GET 'FILE 'ADD-ALARM) FILE))
  (OR NOTIFY-INITIALLY-P
      (SET-ALARM-INHIBIT-NOTIFY 'FILE))
  (ADD-ALARM 'FILE)
  T)

(DEFUN SET-ALARM (TIME MESSAGE)
  (IF (NOT (NUMBERP (SETQ TIME (TIME:PARSE-UNIVERSAL-TIME TIME))))
      (VALUES NIL TIME)
      (FUNCALL (GET 'TIME 'ADD-ALARM) (CONS TIME MESSAGE))
      (ADD-ALARM 'TIME)
      T))

;; By default, the background host stuff will always notify if the host is not up.
;; See the variable ALWAYS-NOTIFY-IF-HOST-NOT-UP.   It's a kludge, but...
(DEFUN BACKGROUND-CHECK-HOSTS (NOTIFY-INITIALLY-P &REST HOSTS)
  (DO ((H HOSTS (CDR H))) ((NULL H))
    (FUNCALL (GET 'HOSTS 'ADD-ALARM) (CAR H)))
  (OR NOTIFY-INITIALLY-P
      (SET-ALARM-INHIBIT-NOTIFY 'HOSTS))
  (ADD-ALARM 'HOSTS)
  T)

;; Note that this also updates the variable FREE-LISPMS
(DEFUN CHECK-FREE-LISPMS (&OPTIONAL NOTIFY-INITIALLY-P)
  (OR NOTIFY-INITIALLY-P
      (SET-ALARM-INHIBIT-NOTIFY 'LISPM))
  (ADD-ALARM 'LISPM))