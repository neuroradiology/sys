;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.17
;;; Reason: CHAOS:DUMMY-MAIL-SERVER never responded
;;; Written 12/16/81 04:17:21 by moon,
;;; while running on Lisp Machine Two from band 2
;;; with System 78.8, ZMail 38.1, Local-File 30.3, Experimental DAEDALUS 29.0, microcode 837.



; From file CHSAUX > LMIO; AI:
#8R CHAOS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))

;;; Dummy mail server, rejects all incoming mail
(DEFUN DUMMY-MAIL-SERVER (&AUX CONN STREAM RCPT)
  (SETQ CONN (LISTEN "MAIL"))
  (ACCEPT CONN)
  (SETQ STREAM (STREAM CONN))
  (*CATCH 'DONE
    (CONDITION-BIND (((READ-ON-CLOSED-CONNECTION LOS-RECEIVED-STATE HOST-DOWN)
		      #'(LAMBDA (&REST IGNORE) (*THROW 'DONE NIL))))
      (DO () (NIL)				;Read the rcpts
	(SETQ RCPT (FUNCALL STREAM ':LINE-IN NIL))
	(AND (ZEROP (STRING-LENGTH RCPT))	;Blank line = start text
	     (RETURN))
	(FUNCALL STREAM ':LINE-OUT
		 "-Lisp Machines do not accept mail, maybe you want the :LMSEND command.")
	(FUNCALL STREAM ':FORCE-OUTPUT))))
  (CLOSE CONN "all rcpts read"))

)
