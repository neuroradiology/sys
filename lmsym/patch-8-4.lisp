;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.4
;;; Reason: PARSE-DIRECTORY-DATE-PROPERTY patched in Symbolics temporarily
;;; Written 12/17/81 09:40:00 by BEE,
;;; while running on Beagle from band 1
;;; with System 78.17, ZMail 38.2, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.6, Canon 9.0, microcode 840.



; From file PATHNM.LISP DSK:<LMIO> SCRC:
#8R FILE-SYSTEM:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))

;;; Fast date parser for simple case of MM/DD/YY HH:MM:SS
(DEFUN PARSE-DIRECTORY-DATE-PROPERTY (STRING START &OPTIONAL END &AUX FLAG)
  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH STRING)))
  (IF (AND (OR (= END (+ START 8))
	       (SETQ FLAG (= END (+ START 17.))))
	   (= (AREF STRING (+ START 2)) #//)
	   (= (AREF STRING (+ START 5)) #//)
	   (OR (NULL FLAG)
	       (AND (= (AREF STRING (+ START 8)) #\SP)
		    (= (AREF STRING (+ START 11.)) #/:)
		    (= (AREF STRING (+ START 14.)) #/:))))
      (LET (DAY MONTH YEAR HOURS MINUTES SECONDS)
	(SETQ MONTH (PARSE-DIRECTORY-DATE-PROPERTY-1 STRING START)
	      DAY (PARSE-DIRECTORY-DATE-PROPERTY-1 STRING (+ START 3))
	      YEAR (PARSE-DIRECTORY-DATE-PROPERTY-1 STRING (+ START 6)))
	(IF FLAG
	    (SETQ HOURS (PARSE-DIRECTORY-DATE-PROPERTY-1 STRING (+ START 9))
		  MINUTES (PARSE-DIRECTORY-DATE-PROPERTY-1 STRING (+ START 12.))
		  SECONDS (PARSE-DIRECTORY-DATE-PROPERTY-1 STRING (+ START 15.)))
	    (SETQ HOURS 0 MINUTES 0 SECONDS 0))
	;; The file job is wont to give dates of the form 00/00/00 for things made by
	;; DSKDMP, e.g..  Avoid errors later.
	(AND (PLUSP MONTH)
	     (TIME:ENCODE-UNIVERSAL-TIME SECONDS MINUTES HOURS DAY MONTH YEAR)))
      ;;Not in simple format, escape to full parser
      (LET ((DATE (TIME:PARSE-UNIVERSAL-TIME STRING START END)))
	(AND (NUMBERP DATE) DATE))))

)

