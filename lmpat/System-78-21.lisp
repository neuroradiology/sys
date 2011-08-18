;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.21
;;; Reason: Fix to PARSE-DIRECTORY-DATE-PROPERTY for non simple date case
;;; Written 12/18/81 11:30:39 by BEE,
;;; while running on Lisp Machine Nine from band 5
;;; with System 78.20, ZMail 38.3, microcode 836, 60.45Hz.



; From file PATHNM > LMIO; AI:
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
