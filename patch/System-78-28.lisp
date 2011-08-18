;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.28
;;; Reason: &REST retention problem in 78.26
;;; Written 12/23/81 11:50:34 by BSG,
;;; while running on Spaniel from band 1
;;; with System 78.27, ZMail 38.4, Symbolics 8.7, Tape 6.2, LMFS 21.17, Canon 9.3, microcode 841.



; From file wholin.lisp >lmwin POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFMETHOD (WHO-LINE-FILE-SHEET :ADD-SERVER) (CONNECTION CONTACT-NAME
					       &OPTIONAL (PROCESS SI:CURRENT-PROCESS)
					       FUNCTION &REST ARGS)
  (PUSH (MAKE-SERVER-DESC
	  CONNECTION CONNECTION
	  HOST-NAME (CHAOS:HOST-SHORT-NAME (CHAOS:FOREIGN-ADDRESS CONNECTION))
	  CONTACT-NAME CONTACT-NAME
	  PROCESS PROCESS
	  FUNCTION FUNCTION
	  ARGS (COPYLIST ARGS))
	SERVERS-LIST))

)
