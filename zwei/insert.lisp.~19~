;;; -*- Mode:LISP; Package:ZWEI -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;;  This file contains basic text manipulation functions for ZWEI. 

;;; This file provides the following functions:
;;; INSERT INSERT-INTERVAL DELETE-INTERVAL COPY-INTERVAL

;;; Internal function for inserting and deleting.
(DEFUN SET-LINE-LENGTH (LINE LENGTH)
  (LET ((CURRENT-SIZE (ARRAY-LENGTH LINE)))
    (COND ((> LENGTH CURRENT-SIZE)
	   (ADJUST-ARRAY-SIZE LINE (MAX LENGTH (FIX (* (MAX CURRENT-SIZE 30.) 1.3s0))))))
    (SETF (LINE-LENGTH LINE) LENGTH)))

;;; Change the type of an array, used to turn ordinary lines into 16-bit ones
(DEFUN SET-LINE-ARRAY-TYPE (LINE ARRAY-TYPE)
  (LET ((NEW-LINE (CREATE-LINE ARRAY-TYPE (ARRAY-LENGTH LINE) (LINE-NODE LINE))))
    (COPY-ARRAY-CONTENTS-AND-LEADER LINE NEW-LINE)
    (STRUCTURE-FORWARD LINE NEW-LINE)))

;;; Make something into a string
(DEFUN ASSURE-STRING (STRING)
  (COND ((ARRAYP STRING) STRING)
	((AND (NUMBERP STRING) ( STRING 400))
	 (LET ((NEW-STRING (MAKE-ARRAY NIL 'ART-FAT-STRING 1)))
	   (ASET STRING NEW-STRING 0)
	   NEW-STRING))
	(T (STRING STRING))))

;;; Insert the STRING at the BP.
(DEFUN INSERT (BP STRING &OPTIONAL (STRING-START 0) STRING-LENGTH
	       &AUX LINE INDEX LINE-LENGTH FIRST-NEWLINE FIRST-LINE LAST-LINE)
  (MUNG-BP-INTERVAL BP)
  (SETQ LINE (BP-LINE BP)
	INDEX (BP-INDEX BP)
	LINE-LENGTH (LINE-LENGTH LINE))
  (COND ((STRINGP STRING)
	 (OR STRING-LENGTH (SETQ STRING-LENGTH (ARRAY-ACTIVE-LENGTH STRING)))
	 (SETQ FIRST-NEWLINE (%STRING-SEARCH-CHAR #\CR STRING STRING-START STRING-LENGTH))
	 (COND ((NULL FIRST-NEWLINE)
		;; The string doesn't have any newlines in it.
		(INSERT-WITHIN-LINE LINE INDEX STRING STRING-START STRING-LENGTH))
	       (T
		;; First, construct the "last" line, which is made up of the last
		;; line of the STRING followed by the part of LINE after INDEX.
		;; The copy the first line of STRING into LINE.
		(LET* ((LAST-NEWLINE (STRING-REVERSE-SEARCH-CHAR #\CR STRING
								 STRING-LENGTH STRING-START))
		       (ARRAY-TYPE (IF (EQ (ARRAY-TYPE STRING) 'ART-FAT-STRING)
				       'ART-FAT-STRING (ARRAY-TYPE LINE)))
		       (LCHARS (- STRING-LENGTH LAST-NEWLINE 1)))
		  (COND ((AND (= LAST-NEWLINE (1- STRING-LENGTH))
			      (ZEROP INDEX))
			 ;;Inserting stuff ending with CR at front of line
			 ;;implies we can just shove down the old line
			 (SETQ LAST-LINE LINE)
			 ;; But then we can't use it as the first line.
			 (SETQ FIRST-LINE (CREATE-LINE ARRAY-TYPE
						       (- FIRST-NEWLINE STRING-START)
						       (BP-NODE BP)))
			 (SETF (LINE-PREVIOUS FIRST-LINE) (LINE-PREVIOUS LINE))
			 (AND (LINE-PREVIOUS LINE)
			      (SETF (LINE-NEXT (LINE-PREVIOUS LINE)) FIRST-LINE))
			 (COPY-ARRAY-PORTION STRING STRING-START FIRST-NEWLINE
					     FIRST-LINE 0 (ARRAY-LENGTH FIRST-LINE))
			 ;; Transfer bps from the front of LINE to FIRST-LINE.
			 (DOLIST (BP (LINE-BP-LIST LINE))
			   (AND (ZEROP (BP-INDEX BP))
				(EQ (BP-STATUS BP) ':NORMAL)
				(MOVE-BP BP FIRST-LINE 0))))
			(T
			 ;; Otherwise, keep the beginning of the line we are inserting in,
			 ;; and make a new line for the tail end of the string.
			 (SETQ FIRST-LINE LINE)
			 (SETQ LAST-LINE (CREATE-LINE ARRAY-TYPE
						      (+ LCHARS (- LINE-LENGTH INDEX))
						      (BP-NODE BP)))
			 ;; Copy the last line of STRING into LAST-LINE.
			 (COPY-ARRAY-PORTION STRING (1+ LAST-NEWLINE) STRING-LENGTH
					     LAST-LINE 0 LCHARS)
			 ;; Copy the part of LINE after INDEX into LAST-LINE
			 (COPY-ARRAY-PORTION LINE INDEX LINE-LENGTH
					     LAST-LINE LCHARS (ARRAY-LENGTH LAST-LINE))
			 ;; Figure out whether LINE is being changed at all.
			 (OR (AND (= FIRST-NEWLINE STRING-START)
				  (= INDEX LINE-LENGTH))
			     (MUNG-LINE LINE))
			 ;; Copy the first line of STRING into LINE.
			 (SET-LINE-LENGTH LINE (+ INDEX (- FIRST-NEWLINE STRING-START)))
			 (OR (EQ ARRAY-TYPE (ARRAY-TYPE LINE))
			     (SET-LINE-ARRAY-TYPE LINE 'ART-FAT-STRING))
			 (COPY-ARRAY-PORTION STRING STRING-START FIRST-NEWLINE
					     LINE INDEX (LINE-LENGTH LINE))
			 ;; Relocate buffer pointers.
			 (DOLIST (BP (LINE-BP-LIST LINE))
			   (LET ((I (BP-INDEX BP)))
			     (COND ((OR (> I INDEX)
					(AND (= I INDEX)
					     (EQ (BP-STATUS BP) ':MOVES)))
				    (MOVE-BP BP LAST-LINE (+ (- I INDEX) LCHARS))))))))
		  (DO ((PREV-LINE FIRST-LINE THIS-LINE)
		       (THIS-LINE)
		       (PREV-NEWLINE FIRST-NEWLINE NEWLINE)
		       (NEWLINE)
		       (THE-LINE-BEYOND (LINE-NEXT LINE)))
		      (NIL)
		    (COND ((= PREV-NEWLINE LAST-NEWLINE)
			   ;; We are at the end.
			   (AND THE-LINE-BEYOND
				(SETF (LINE-PREVIOUS THE-LINE-BEYOND) LAST-LINE))
			   (SETF (LINE-NEXT LAST-LINE) THE-LINE-BEYOND)
			   (SETF (LINE-NEXT PREV-LINE) LAST-LINE)
			   (SETF (LINE-PREVIOUS LAST-LINE) PREV-LINE)
			   (RETURN NIL)))
		    (SETQ NEWLINE (%STRING-SEARCH-CHAR #\CR STRING
						       (1+ PREV-NEWLINE) STRING-LENGTH))
		    (LET ((LENGTH (- NEWLINE PREV-NEWLINE 1)))
		      (SETQ THIS-LINE (CREATE-LINE (ARRAY-TYPE STRING) LENGTH (BP-NODE BP)))
		      (COPY-ARRAY-PORTION STRING (1+ PREV-NEWLINE) NEWLINE
					  THIS-LINE 0 LENGTH)
		      (SETF (LINE-NEXT PREV-LINE) THIS-LINE)
		      (SETF (LINE-PREVIOUS THIS-LINE) PREV-LINE)))
		  (CREATE-BP LAST-LINE LCHARS)))))
	;; These are for INSERT of a non-string
	((EQ STRING #\CR)
	 (COND ((ZEROP INDEX)
		;; Shove down the old line if inserting a CR at its front
		(SETQ FIRST-LINE (CREATE-LINE 'ART-STRING 0 (BP-NODE BP)))
		(SETF (LINE-PREVIOUS FIRST-LINE) (LINE-PREVIOUS LINE))
		(AND (LINE-PREVIOUS LINE)
		     (SETF (LINE-NEXT (LINE-PREVIOUS LINE)) FIRST-LINE))
		(SETF (LINE-NEXT FIRST-LINE) LINE)
		(SETF (LINE-PREVIOUS LINE) FIRST-LINE)
		;; Transfer bps from the front of LINE to FIRST-LINE.
		(DOLIST (BP (LINE-BP-LIST LINE))
		  (AND (ZEROP (BP-INDEX BP))
		       (EQ (BP-STATUS BP) ':NORMAL)
		       (MOVE-BP BP FIRST-LINE 0)))
		(CREATE-BP LINE 0))
	       (T
		;; Otherwise, keep the beginning of the line we are inserting in,
		;; and make a new line for the tail end of the string.
		(SETQ LAST-LINE (CREATE-LINE (ARRAY-TYPE LINE) (- LINE-LENGTH INDEX)
					     (BP-NODE BP)))
		(SETF (LINE-NEXT LAST-LINE) (LINE-NEXT LINE))
		(AND (LINE-NEXT LINE)
		     (SETF (LINE-PREVIOUS (LINE-NEXT LINE)) LAST-LINE))
		(SETF (LINE-NEXT LINE) LAST-LINE)
		(SETF (LINE-PREVIOUS LAST-LINE) LINE)
		;; Copy the part of LINE after INDEX into LAST-LINE
		(COPY-ARRAY-PORTION LINE INDEX LINE-LENGTH
				    LAST-LINE 0 (ARRAY-LENGTH LAST-LINE))
		;; Figure out whether LINE is being changed at all.
		(OR (= INDEX LINE-LENGTH)
		    (MUNG-LINE LINE))
		;; Truncate LINE
		(SET-LINE-LENGTH LINE INDEX)
		;; Relocate buffer pointers.
		(DOLIST (BP (LINE-BP-LIST LINE))
		  (LET ((I (BP-INDEX BP)))
		    (COND ((OR (> I INDEX)
			       (AND (= I INDEX)
				    (EQ (BP-STATUS BP) ':MOVES)))
			   (MOVE-BP BP LAST-LINE (- I INDEX))))))
		(CREATE-BP LAST-LINE 0))))
	;; Insert ordinary character -- code simplified from INSERT-WITHIN-LINE
	((FIXP STRING)
	 (AND (GET (LOCF (LINE-PLIST LINE)) ':DIAGRAM) (BARF "Diagram line"))
	 (SET-LINE-LENGTH LINE (1+ LINE-LENGTH))
	 (IF (AND ( STRING 400)
		  (NEQ (ARRAY-TYPE LINE) 'ART-FAT-STRING))
	     (SET-LINE-ARRAY-TYPE LINE 'ART-FAT-STRING))
	 ;; Move the characters ahead of the inserting forward.
	 (DO ((LF (1- LINE-LENGTH) (1- LF))
	      (LT LINE-LENGTH (1- LT)))
	     ((< LF INDEX))
	   (ASET (AREF LINE LF) LINE LT))
	 ;; Insert the new characters into the line.
	 (ASET STRING LINE INDEX)
	 ;; Relocate buffer pointers.
	 (DOLIST (BP (LINE-BP-LIST LINE))
	   (LET ((I (BP-INDEX BP)))
	     (COND ((OR (> I INDEX)
			(AND (= I INDEX)
			     (EQ (BP-STATUS BP) ':MOVES)))
		    (SETF (BP-INDEX BP) (1+ I))))))
	 (MUNG-LINE LINE)
	 (CREATE-BP LINE (1+ INDEX)))
	;; Inserting something random
	(T (INSERT BP (STRING STRING) STRING-START STRING-LENGTH))))

(DEFUN INSERT-MOVING (BP STRING)
  (LET ((NBP (INSERT BP STRING)))
    (MOVE-BP BP NBP)
    NBP))

;;; First arg is a BP.  Second is an interval, or second&third are an ordered range.
;;; Insert the stuff from the interval at the BP.
(DEFUN INSERT-INTERVAL (AT-BP FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  (MUNG-BP-INTERVAL AT-BP)
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (LET ((AT-LINE (BP-LINE AT-BP))
	(AT-INDEX (BP-INDEX AT-BP))
	(FROM-LINE (BP-LINE FROM-BP))
	(FROM-INDEX (BP-INDEX FROM-BP))
	(TO-LINE (BP-LINE TO-BP))
	(TO-INDEX (BP-INDEX TO-BP)))
    (IF (EQ FROM-LINE TO-LINE)
	;; Insert within AT-LINE.
	(INSERT-WITHIN-LINE AT-LINE AT-INDEX FROM-LINE FROM-INDEX TO-INDEX)
	(LET ((AT-LINE-LENGTH (LINE-LENGTH AT-LINE))
	      (FROM-LINE-LENGTH (LINE-LENGTH FROM-LINE))
	      (ARRAY-TYPE (IF (EQ (ARRAY-TYPE TO-LINE) 'ART-FAT-STRING)
			      'ART-FAT-STRING (ARRAY-TYPE AT-LINE)))
	      FIRST-LINE LAST-LINE)
	  (COND ((AND (ZEROP TO-INDEX)
		      (ZEROP AT-INDEX))
		 ;;Inserting stuff ending with CR at front of line
		 ;;implies we can just shove down the old line
		 (SETQ LAST-LINE AT-LINE)
		 ;; But then we can't use it as the first line.
		 (SETQ FIRST-LINE (CREATE-LINE ARRAY-TYPE FROM-LINE-LENGTH
					       (BP-NODE AT-BP)))
		 (SETF (LINE-PREVIOUS FIRST-LINE) (LINE-PREVIOUS AT-LINE))
		 (AND (LINE-PREVIOUS AT-LINE)
		      (SETF (LINE-NEXT (LINE-PREVIOUS AT-LINE)) FIRST-LINE))
		 (DOTIMES (SF FROM-LINE-LENGTH)
		   (ASET (AREF FROM-LINE SF) FIRST-LINE SF))
		 (SETF (LINE-PLIST FIRST-LINE) (LINE-PLIST FROM-LINE))
		 ;; Transfer bps from the front of AT-LINE to FIRST-LINE.
		 (DOLIST (BP (LINE-BP-LIST AT-LINE))
		   (AND (ZEROP (BP-INDEX BP))
			(EQ (BP-STATUS BP) ':NORMAL)
			(MOVE-BP BP FIRST-LINE 0))))
		(T
		 ;; Otherwise, keep the beginning of the line we are inserting in,
		 ;; and make a new line for the tail end of the string.
		 (SETQ FIRST-LINE AT-LINE)
		 (SETQ LAST-LINE
		       (CREATE-LINE ARRAY-TYPE (+ TO-INDEX (- AT-LINE-LENGTH AT-INDEX))
				    (BP-NODE AT-BP)))
		 ;; Copy the first part of TO-LINE into the LAST-LINE.
		 (DO ((TF 0 (1+ TF))
		      (LLT 0 (1+ LLT)))
		     (( TF TO-INDEX))
		   (ASET (AREF TO-LINE TF) LAST-LINE LLT))
		 ;; Figure out whether AT-LINE is being changed at all.
		 (OR (AND (ZEROP FROM-LINE-LENGTH)
			  (= AT-INDEX (LINE-LENGTH AT-LINE)))
		     (MUNG-LINE AT-LINE))
		 ;; Copy the second part of AT-LINE to LAST-LINE.
		 (DO ((AF AT-INDEX (1+ AF))
		      (LLT TO-INDEX (1+ LLT)))
		     (( AF AT-LINE-LENGTH))
		   (ASET (AREF AT-LINE AF) LAST-LINE LLT))
		 ;; Copy FROM-LINE into AT-LINE.
		 (SET-LINE-LENGTH AT-LINE (+ AT-INDEX (- FROM-LINE-LENGTH FROM-INDEX)))
		 (DO ((FF FROM-INDEX (1+ FF))
		      (AT AT-INDEX (1+ AT))
		      (16B-P (EQ (ARRAY-TYPE AT-LINE) 'ART-FAT-STRING))
		      (CH))
		     (( FF FROM-LINE-LENGTH))
		   (COND ((NOT (OR (< (SETQ CH (AREF FROM-LINE FF)) 400) 16B-P))
			  (SET-LINE-ARRAY-TYPE AT-LINE 'ART-FAT-STRING)
			  (SETQ 16B-P T)))
		   (ASET CH AT-LINE AT))
		 ;; Relocate buffer pointers.
		 (DOLIST (BP (LINE-BP-LIST AT-LINE))
		   (LET ((I (BP-INDEX BP)))
		     (COND ((OR (> I AT-INDEX)
				(AND (= I AT-INDEX)
				     (EQ (BP-STATUS BP) ':MOVES)))
			    (MOVE-BP BP LAST-LINE (+ (- I AT-INDEX) TO-INDEX))))))))
	  (DO ((PREV-LINE FIRST-LINE THIS-LINE)
	       (THIS-LINE)
	       (NODE (BP-NODE AT-BP))
	       (THE-LINE-BEYOND (LINE-NEXT AT-LINE))
	       (ORIGINAL-LINE (LINE-NEXT FROM-LINE) (LINE-NEXT ORIGINAL-LINE)))
	      ((EQ ORIGINAL-LINE TO-LINE)
	       (AND THE-LINE-BEYOND
		    (SETF (LINE-PREVIOUS THE-LINE-BEYOND) LAST-LINE))
	       (SETF (LINE-NEXT LAST-LINE) THE-LINE-BEYOND)
	       (SETF (LINE-NEXT PREV-LINE) LAST-LINE)
	       (SETF (LINE-PREVIOUS LAST-LINE) PREV-LINE))
	    (SETQ THIS-LINE (COPY-LINE ORIGINAL-LINE NODE))
	    (SETF (LINE-NEXT PREV-LINE) THIS-LINE)
	    (SETF (LINE-PREVIOUS THIS-LINE) PREV-LINE))
	  (CREATE-BP LAST-LINE TO-INDEX)))))

(DEFUN INSERT-INTERVAL-MOVING (BP FIRST-BP &OPTIONAL LAST-BP IN-ORDER-P)
  (LET ((NBP (INSERT-INTERVAL BP FIRST-BP LAST-BP IN-ORDER-P)))
    (MOVE-BP BP NBP)))

(DEFUN INSERT-THING (BP THING)
  (IF (OR (STRINGP THING) (NUMBERP THING) (SYMBOLP THING))
      (INSERT BP THING)
      ;; This is a kludge, to prevent getting bad data into the mini-buffer
      ;; there may be a more general solution, i am not sure.
      (AND (EQ (BP-TOP-LEVEL-NODE BP) *INTERVAL*)
	   (NULL (WINDOW-FONT-ALIST *WINDOW*))
	   (DO LINE (BP-LINE (INTERVAL-FIRST-BP THING)) (LINE-NEXT LINE) (NULL LINE)
	     (SET-LINE-ARRAY-TYPE LINE 'ART-STRING)))
      (INSERT-INTERVAL BP THING NIL T)))

;;; Delete the text between FROM-BP and TO-BP.  FROM-BP and TO-BP must be in order.
;;; Return a BP to the place from which text was deleted.
(DEFUN DELETE-INTERVAL (FROM-BP &OPTIONAL TO-BP IN-ORDER-P &AUX KEPT-LINE)
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (MUNG-BP-INTERVAL FROM-BP)
  (LET ((FROM-LINE (BP-LINE FROM-BP))
	(FROM-INDEX (BP-INDEX FROM-BP))
	(TO-LINE (BP-LINE TO-BP))
	(TO-INDEX (BP-INDEX TO-BP)))
    (COND ((EQ FROM-LINE TO-LINE)
	   (COND ((= TO-INDEX FROM-INDEX))
		 ((< TO-INDEX FROM-INDEX)
		  (FERROR NIL "The BPs ~S and ~S were not in order." FROM-BP TO-BP))
		 (T
		  (LET ((LINE-LENGTH (LINE-LENGTH FROM-LINE))
			(NDEL (- TO-INDEX FROM-INDEX)))
		    (DO ((FL TO-INDEX (1+ FL))
			 (TL FROM-INDEX (1+ TL)))
			(( FL LINE-LENGTH))
		      (ASET (AREF FROM-LINE FL) FROM-LINE TL))
		    (SET-LINE-LENGTH FROM-LINE (- LINE-LENGTH NDEL))
		    ;; Relocate buffer pointers.
		    (DOLIST (BP (LINE-BP-LIST FROM-LINE))
		      (LET ((I (BP-INDEX BP)))
			(COND (( I TO-INDEX)
			       (SETF (BP-INDEX BP) (- I NDEL)))
			      (( I FROM-INDEX)
			       (SETF (BP-INDEX BP) FROM-INDEX)))))
		    (MUNG-LINE FROM-LINE)))))
	  (T
	   (MUNG-BP-INTERVAL TO-BP)		;May be in another node
	   (COND ((AND (ZEROP TO-INDEX) (ZEROP FROM-INDEX))
		  ;; If deleting all of from-line and none of to-line,
		  ;; we don't need to touch to-line at all.
		  (SETQ KEPT-LINE TO-LINE)
		  (SETF (LINE-TICK FROM-LINE) 'DELETED))
		 (T
		  ;; Copy characters from end of TO-LINE to replace end of FROM-LINE.
		  (SETQ KEPT-LINE FROM-LINE)
		  (SETF (LINE-TICK TO-LINE) 'DELETED)
		  (LET ((TO-LENGTH (LINE-LENGTH TO-LINE)))
		    (OR (AND (= TO-INDEX TO-LENGTH)
			     (= FROM-INDEX (LINE-LENGTH FROM-LINE)))
			(MUNG-LINE FROM-LINE))
		    (SET-LINE-LENGTH FROM-LINE (+ FROM-INDEX (- TO-LENGTH TO-INDEX)))
		    (DO ((FTL TO-INDEX (1+ FTL))
			 (TFL FROM-INDEX (1+ TFL))
			 (16B-P (EQ (ARRAY-TYPE FROM-LINE) 'ART-FAT-STRING))
			 (CH))
			(( FTL TO-LENGTH))
		      (COND ((NOT (OR (< (SETQ CH (AREF TO-LINE FTL)) 400) 16B-P))
			     (SET-LINE-ARRAY-TYPE FROM-LINE 'ART-FAT-STRING)
			     (SETQ 16B-P T)))
		      (ASET CH FROM-LINE TFL)))))
	   ;; Relocate BPs on the FROM-LINE.
	   (DOLIST (BP (LINE-BP-LIST FROM-LINE))
	     (MOVE-BP BP KEPT-LINE (MIN (BP-INDEX BP) FROM-INDEX)))
	   ;; Relocate BPs on the TO-LINE.
	   (DOLIST (BP (LINE-BP-LIST TO-LINE))
	     (MOVE-BP BP KEPT-LINE
		      (+ FROM-INDEX (MAX 0 (- (BP-INDEX BP) TO-INDEX)))))
	   ;; Loop over intermedidiate lines, relocating bps.
	   (DO ((LINE (LINE-NEXT FROM-LINE) (LINE-NEXT LINE)))
	       ((EQ LINE TO-LINE)
		;; We have reached the TO-LINE.
		;; Splice out all lines FROM-LINE to TO-LINE inclusive except KEPT-LINE.
		(LET ((LINE-BEFORE (LINE-PREVIOUS FROM-LINE))
		      (LINE-AFTER (LINE-NEXT TO-LINE)))
		  (SETF (LINE-NEXT KEPT-LINE) LINE-AFTER)
		  (SETF (LINE-PREVIOUS KEPT-LINE) LINE-BEFORE)
		  (AND LINE-BEFORE (SETF (LINE-NEXT LINE-BEFORE) KEPT-LINE))
		  (AND LINE-AFTER (SETF (LINE-PREVIOUS LINE-AFTER) KEPT-LINE))))
	     (OR LINE
		 (FERROR NIL "The BPs ~S and ~S were not in order." FROM-BP TO-BP))
	     (SETF (LINE-TICK LINE) 'DELETED)
	     (DOLIST (BP (LINE-BP-LIST LINE))
	       (MOVE-BP BP KEPT-LINE FROM-INDEX))))))
  (COPY-BP FROM-BP))

;;; This is an internal function of INSERT and INSERT-INTERVAL
(DEFUN INSERT-WITHIN-LINE (LINE INDEX STRING FROM TO)
  (AND (GET (LOCF (LINE-PLIST LINE)) ':DIAGRAM) (BARF "Diagram line"))
  (COND ((EQ STRING LINE)
	 (SETQ STRING (SUBSTRING STRING FROM TO)
	       TO (- TO FROM)
	       FROM 0)))
  (LET ((LINE-LENGTH (LINE-LENGTH LINE))
	(STRING-LENGTH (- TO FROM)))
    (LET ((NEW-LINE-LENGTH (+ LINE-LENGTH STRING-LENGTH)))
      (SET-LINE-LENGTH LINE NEW-LINE-LENGTH)
      (OR (EQ (ARRAY-TYPE STRING) 'ART-STRING) (EQ (ARRAY-TYPE LINE) 'ART-FAT-STRING)
	  (SET-LINE-ARRAY-TYPE LINE 'ART-FAT-STRING))
      ;; Move the characters ahead of the inserting forward.
      (DO ((LF (1- LINE-LENGTH) (1- LF))
	   (LT (1- NEW-LINE-LENGTH) (1- LT)))
	  ((< LF INDEX))
	(ASET (AREF LINE LF) LINE LT))
      ;; Insert the new characters into the line.
      (COPY-ARRAY-PORTION STRING FROM TO LINE INDEX (+ INDEX STRING-LENGTH))
      ;; Relocate buffer pointers.
      (DOLIST (BP (LINE-BP-LIST LINE))
	(LET ((I (BP-INDEX BP)))
	  (COND ((OR (> I INDEX)
		     (AND (= I INDEX)
			  (EQ (BP-STATUS BP) ':MOVES)))
		 (SETF (BP-INDEX BP) (+ I STRING-LENGTH))))))
      (MUNG-LINE LINE)
      (CREATE-BP LINE (+ INDEX STRING-LENGTH)))))

;;; Insert a line BEFORE a line in the interval.
;;; this is used by the interval stream for the case of a line that already looks like
;;; an editor line, such as is gotten from STREAM-COPY-UNTIL-EOF
(DEFUN INSERT-LINE-WITH-LEADER (LINE AT-LINE)
  (LET ((PREV (LINE-PREVIOUS AT-LINE)))
    (COND (PREV
           (SETF (LINE-NEXT PREV) LINE)
           (SETF (LINE-PREVIOUS LINE) PREV))))
  (SETF (LINE-NEXT LINE) AT-LINE)
  (SETF (LINE-PREVIOUS AT-LINE) LINE)
  (SETF (LINE-NODE LINE) (LINE-NODE AT-LINE))
  (SETF (LINE-TICK LINE) *TICK*)
  ;; Now hack the BPs
  (DOLIST (BP (LINE-BP-LIST AT-LINE))
      (COND ((EQ (BP-STATUS BP) ':NORMAL)
             (SETF (LINE-BP-LIST AT-LINE) (DELQ BP (LINE-BP-LIST AT-LINE)))
             (PUSH BP (LINE-BP-LIST LINE))
             (SETF (BP-LINE BP) LINE)))))

;;; This is an internal function of INSERT-INTERVAL
(DEFUN COPY-LINE (LINE NODE)
  (LET ((LEN (LINE-LENGTH LINE)))
    (LET ((NEW-LINE (CREATE-LINE (ARRAY-TYPE LINE) LEN NODE)))
      (COPY-ARRAY-CONTENTS LINE NEW-LINE)
      (SETF (LINE-PLIST NEW-LINE) (LINE-PLIST LINE))
      NEW-LINE)))

;;; This takes either an interval or a pair of BPs, and returns
;;; an interval with the same characters.
(DEFUN COPY-INTERVAL (FROM-BP &OPTIONAL TO-BP IN-ORDER-P &AUX INTERVAL)
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (SETQ INTERVAL (MAKE-NODE NODE-TICK *TICK*))
  (LET ((FROM-LINE (BP-LINE FROM-BP))
	(FROM-INDEX (BP-INDEX FROM-BP))
	(TO-LINE (BP-LINE TO-BP))
	(TO-INDEX (BP-INDEX TO-BP)))
    (COND ((EQ FROM-LINE TO-LINE)
	   (LET ((LEN (- TO-INDEX FROM-INDEX)))
	     (LET ((LINE (CREATE-LINE (ARRAY-TYPE FROM-LINE) LEN INTERVAL)))
	       (DO ((FLF FROM-INDEX (1+ FLF))
		    (LT 0 (1+ LT)))
		   (( LT LEN))
		 (ASET (AREF FROM-LINE FLF) LINE LT))
	       (AND (ZEROP FROM-INDEX) (= TO-INDEX (LINE-LENGTH FROM-LINE))
		    (SETF (LINE-PLIST LINE) (LINE-PLIST FROM-LINE)))
	       (SETF (INTERVAL-FIRST-BP INTERVAL) (CREATE-BP LINE 0 ':NORMAL))
	       (SETF (INTERVAL-LAST-BP INTERVAL) (CREATE-BP LINE LEN ':MOVES)))))
	  (T
	   (LET ((FROM-LINE-LENGTH (LINE-LENGTH FROM-LINE)))
	     (LET ((FIRST-LINE (CREATE-LINE (ARRAY-TYPE FROM-LINE)
					    (- FROM-LINE-LENGTH FROM-INDEX)
					    INTERVAL))
		   (LAST-LINE (CREATE-LINE (ARRAY-TYPE TO-LINE) TO-INDEX INTERVAL)))
	       ;; Copy text from FROM-LINE to FIRST-LINE.
	       (DO ((FRF FROM-INDEX (1+ FRF))
		    (FIT 0 (1+ FIT)))
		   (( FRF FROM-LINE-LENGTH))
		 (ASET (AREF FROM-LINE FRF) FIRST-LINE FIT))
	       (AND (ZEROP FROM-INDEX) (SETF (LINE-PLIST FIRST-LINE) (LINE-PLIST FROM-LINE)))
	       ;; Copy text from TO-LINE to LAST-LINE.
	       (DO ((I 0 (1+ I)))
		   (( I TO-INDEX))
		 (ASET (AREF TO-LINE I) LAST-LINE I))
	       (AND (= TO-INDEX (LINE-LENGTH TO-LINE))
		    (SETF (LINE-PLIST LAST-LINE) (LINE-PLIST TO-LINE)))
	       (DO ((PREV-LINE FIRST-LINE THIS-LINE)
		    (THIS-LINE)
		    (ORIGINAL-LINE (LINE-NEXT FROM-LINE) (LINE-NEXT ORIGINAL-LINE)))
		   ((EQ ORIGINAL-LINE TO-LINE)
		    (SETF (LINE-NEXT PREV-LINE) LAST-LINE)
		    (SETF (LINE-PREVIOUS LAST-LINE) PREV-LINE)
		    (SETF (INTERVAL-FIRST-BP INTERVAL) (CREATE-BP FIRST-LINE 0 ':NORMAL))
		    (SETF (INTERVAL-LAST-BP INTERVAL) (CREATE-BP LAST-LINE TO-INDEX ':MOVES)))
		 (SETQ THIS-LINE (COPY-LINE ORIGINAL-LINE INTERVAL))
		 (SETF (LINE-NEXT PREV-LINE) THIS-LINE)
		 (SETF (LINE-PREVIOUS THIS-LINE) PREV-LINE)))))))
  INTERVAL)

;;; Make a string whose text is that of the interval.
(DEFUN STRING-INTERVAL (FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (LET ((FROM-LINE (BP-LINE FROM-BP))
	(FROM-INDEX (BP-INDEX FROM-BP))
	(TO-LINE (BP-LINE TO-BP))
	(TO-INDEX (BP-INDEX TO-BP))
	STRING)
    (SETQ STRING (MAKE-ARRAY NIL (ARRAY-TYPE FROM-LINE) (COUNT-CHARS FROM-BP TO-BP)))
    (COND ((EQ FROM-LINE TO-LINE)
	   ;; Within a line.  Copy the characters.
	   (DO ((LF FROM-INDEX (1+ LF))
		(ST 0 (1+ ST)))
	       (( LF TO-INDEX))
	     (ASET (AREF FROM-LINE LF) STRING ST)))
	  (T
	   (LET ((ST 0))
	     ;; Copy from the first line.
	     (DO ((FLF FROM-INDEX (1+ FLF))
		  (LEN (LINE-LENGTH FROM-LINE)))
		 (( FLF LEN))
	       (ASET (AREF FROM-LINE FLF) STRING ST)
	       (SETQ ST (1+ ST)))
	     (ASET #\CR STRING ST)
	     (SETQ ST (1+ ST))
	     ;; Copy from intermediate lines.
	     (DO ((LINE (LINE-NEXT FROM-LINE) (LINE-NEXT LINE)))
		 ((EQ LINE TO-LINE))
	       (DO ((LF 0 (1+ LF))
		    (LEN (LINE-LENGTH LINE)))
		   (( LF LEN))
		 (ASET (AREF LINE LF) STRING ST)
		 (SETQ ST (1+ ST)))
	       (ASET #\CR STRING ST)
	       (SETQ ST (1+ ST)))
	     ;; Copy from the last line.
	     (DO ((TLF 0 (1+ TLF)))
		 (( TLF TO-INDEX))
	       (ASET (AREF TO-LINE TLF) STRING ST)
	       (SETQ ST (1+ ST))))))
    STRING))

;;;Insert n copies of a character
(DEFUN INSERT-CHARS (BP CHAR N &AUX STRING)
  (SETQ STRING (MAKE-ARRAY NIL (IF (LDB-TEST %%CH-FONT CHAR) 'ART-FAT-STRING 'ART-STRING) N))
  (DOTIMES (I N) (ASET CHAR STRING I))
  (PROG1 (INSERT BP STRING) (RETURN-ARRAY STRING)))
