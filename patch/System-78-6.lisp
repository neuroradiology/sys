;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.6
;;; Reason: Gagging feature for Qsend
;;; Written 12/10/81 04:09:14 by CStacy,
;;; while running on Lisp Machine Six from band 2
;;; with System 78.5, ZMail 38.0, microcode 836.



; From file CHSAUX > LMIO; AI:
#8R CHAOS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))

;Set to non-nil if we don't want to get qsends.
;If this is a string, it is sent as a reply to the user trying to send to us.
(DEFVAR QSEND-GAGGED NIL)

(DEFUN QSENDS-OFF (&OPTIONAL (GAG-MESSAGE T))
  (SETQ QSEND-GAGGED GAG-MESSAGE))

(DEFUN QSENDS-ON ()
  (SETQ QSEND-GAGGED NIL))

(DEFVAR POP-UP-QSEND-WINDOW)
(DEFVAR POP-UP-QSEND-LOCK NIL)	;One guy typing at a time

(DEFUN POP-UP-RECEIVE-SEND-MSG (&AUX CONN RECIPIENT RFC SENDER VISP TEM
				     (START (ARRAY-ACTIVE-LENGTH SAVED-SENDS)))
 (UNWIND-PROTECT
   (PROGN
     (SETQ CONN (LISTEN "SEND"))
     (SETQ RFC (PKT-STRING (READ-PKTS CONN)))
     (SETQ RECIPIENT
	   (COND ((SETQ TEM (STRING-SEARCH " " RFC))
		  (NSUBSTRING RFC (1+ TEM)))
		 (T "anyone")))
     (ACCEPT CONN)
     (COND (QSEND-GAGGED
	    (WITH-OPEN-STREAM (CSTREAM (MAKE-STREAM CONN ':DIRECTION ':INPUT))
	      (SETQ SENDER (FUNCALL CSTREAM ':LINE-IN))
	      (COND ((SETQ TEM (STRING-SEARCH-CHAR #/@ SENDER))
		     (SETQ SENDER (NSUBSTRING SENDER 0 TEM)))
		    ((SETQ TEM (STRING-SEARCH "from " SENDER))
		     (SETQ SENDER (NSUBSTRING SENDER (+ TEM 5)
					      (STRING-SEARCH-SET
						'(#/] #\SP) SENDER (+ TEM 5)))))
		    (T
		     (SETQ SENDER ""))) ;Can generate bogus mail.
	      (SETQ SENDER (STRING-APPEND SENDER #/@
					  (HOST-SHORT-NAME (FOREIGN-ADDRESS CONN)))))
	    (IF (NOT (EQUAL MY-ADDRESS (FOREIGN-ADDRESS CONN))) ;If user is not being silly
		(IF (STRINGP QSEND-GAGGED )                     ;send or mail the reply.
		    (SEND-MSG SENDER QSEND-GAGGED T)
		  (SEND-MSG SENDER "[User not accepting Qsends.]" T))))
	   (T 
	    (WITH-OUTPUT-TO-STRING (SSTREAM SAVED-SENDS)
	      (FORMAT SSTREAM "~%[Message from ~A for ~A]~%"
		      (HOST-DATA (FOREIGN-ADDRESS CONN)) RECIPIENT)
	      (WITH-OPEN-STREAM (CSTREAM (MAKE-STREAM CONN ':DIRECTION ':INPUT))
		(SETQ SENDER (FUNCALL CSTREAM ':LINE-IN))
		(FUNCALL SSTREAM ':LINE-OUT SENDER)
		(COND ((SETQ TEM (STRING-SEARCH-CHAR #/@ SENDER))
		       (SETQ SENDER (NSUBSTRING SENDER 0 TEM)))
		      ((SETQ TEM (STRING-SEARCH "from " SENDER))
		       (SETQ SENDER (NSUBSTRING SENDER (+ TEM 5)
						(STRING-SEARCH-SET
						  '(#/] #\SP) SENDER (+ TEM 5)))))
		      (T
		       (SETQ SENDER "")))
		(SETQ SENDER (STRING-APPEND SENDER #/@
					    (HOST-SHORT-NAME (FOREIGN-ADDRESS CONN))))
		(STREAM-COPY-UNTIL-EOF CSTREAM SSTREAM))
	      (FORMAT SSTREAM "~2%"))
	    (OR (BOUNDP 'POP-UP-QSEND-WINDOW)
		(SETQ POP-UP-QSEND-WINDOW TV:(MAKE-WINDOW 'POP-UP-TEXT-WINDOW
							  ':NAME "QSend"
							  ':HEIGHT 400	;About 17 lines
							  ':SAVE-BITS T)))
	    ;; Ring the bell before the real-time delay of popping it up.
	    ;; Also ring it before locking so he knows another message came in.
	    (DOTIMES (I SEND-BELLCOUNT) (FUNCALL POP-UP-QSEND-WINDOW ':BEEP))
	    ;; This waits until any other message is done
	    ;; then seizes the lock.  The effect is to handle only one message at a time.
	    (DO ((FIRST-TIME T NIL))
		((%STORE-CONDITIONAL (VALUE-CELL-LOCATION 'POP-UP-QSEND-LOCK)
				     NIL CURRENT-PROCESS))
	      ;; If messages coming in with thing hung up, let user know
	      (AND FIRST-TIME (NEQ (FUNCALL POP-UP-QSEND-WINDOW ':STATUS) ':SELECTED)
		   (TV:NOTIFY NIL "Message from ~A waiting for QSend window" SENDER))
	      (PROCESS-WAIT "Lock" #'(LAMBDA (X) (NULL (CDR X)))
			    (VALUE-CELL-LOCATION 'POP-UP-QSEND-LOCK)))
	    (SETQ VISP (TV:SHEET-EXPOSED-P POP-UP-QSEND-WINDOW))
	    (FUNCALL POP-UP-QSEND-WINDOW ':MOUSE-SELECT)
	    ;; If the window was not already visible, erase it.
	    (COND ((NOT VISP)
		   (SETQ START (1+ START))	;Skip the first CR
		   (FUNCALL POP-UP-QSEND-WINDOW ':CLEAR-SCREEN)))
	    (FUNCALL POP-UP-QSEND-WINDOW ':STRING-OUT SAVED-SENDS START)
	    (LET ((TERMINAL-IO POP-UP-QSEND-WINDOW))
	      (FUNCALL POP-UP-QSEND-WINDOW ':CLEAR-INPUT)
	      (COND ((Y-OR-N-P "Reply? " POP-UP-QSEND-WINDOW)
		     (FORMAT POP-UP-QSEND-WINDOW "~&To: ~A" SENDER)
		     (SEND-MSG SENDER))))
	   (COND ((EQ POP-UP-QSEND-LOCK CURRENT-PROCESS)
		  (SETQ POP-UP-QSEND-LOCK NIL)
		  (PROCESS-ALLOW-SCHEDULE)
		  (COND ((NULL POP-UP-QSEND-LOCK)
			 (FUNCALL POP-UP-QSEND-WINDOW ':DESELECT T)
			 (FUNCALL POP-UP-QSEND-WINDOW ':DEACTIVATE))))))))))

(ADD-INITIALIZATION "SEND"
                    '(PROCESS-RUN-TEMPORARY-FUNCTION "SEND Server" #'POP-UP-RECEIVE-SEND-MSG)
                    NIL
                    'SERVER-ALIST)

;; Calling ZWEI:SEND-MESSAGE-STRING with a destination of "anyone@HOST" is bogus!

(DEFUN SEND-MSG (DESTINATION &OPTIONAL MSG (MAIL-P NIL) &AUX HOST PERSON)
  (COND ((AND (NOT (NUMBERP DESTINATION))
	      (SETQ HOST (DO ((@-POS (STRING-SEARCH "@" DESTINATION)
				     (STRING-SEARCH "@" DESTINATION (1+ @-POS)))
			      (LAST-@-POS NIL @-POS))
			     ((NULL @-POS) LAST-@-POS))))
         (SETQ PERSON (STRING-UPCASE (NSUBSTRING DESTINATION 0 HOST))
               HOST (NSUBSTRING DESTINATION (1+ HOST) (STRING-LENGTH DESTINATION))))
        (T (SETQ PERSON "anyone"  HOST DESTINATION)))
  (FS:FORCE-USER-TO-LOGIN)
  (COND ((NULL MSG)
	 (FORMAT T "~%Message: (terminate with ~:@C)~%" #\END)
	 (SETQ MSG (SEND-MSG-GET-MESSAGE))))
  (WITH-OPEN-STREAM (STREAM (OPEN-STREAM HOST (STRING-APPEND "SEND " PERSON)
					 ':ERROR NIL ':DIRECTION ':OUTPUT))
    (COND ((NOT (STRINGP STREAM))
	   (FORMAT STREAM "~A@~A ~\DATIME\~%" USER-ID SI:LOCAL-HOST)
           (FUNCALL STREAM ':STRING-OUT MSG)
	   (FUNCALL STREAM ':CLOSE))
	  ((IF MAIL-P (ZWEI:SEND-MESSAGE-STRING PERSON
						(STRING-APPEND "[This was a failing QSEND]
"
							   MSG))
	     (IF (FQUERY FORMAT:YES-OR-NO-QUIETLY-P-OPTIONS "~A  Mail instead? " STREAM)
		 (ZWEI:SEND-MESSAGE-STRING PERSON
					   (STRING-APPEND "[This was a failing QSEND]
"
							   MSG))))))))


)
