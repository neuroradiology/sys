;;; Date and time routines -*- Mode:LISP; Package:TIME -*-
;;; Note: days and months are kept one-based throughout, as much as possible.
;;; Days of the week are zero-based on Monday.

;;; [Maybe this should have a global variable which causes it to use AM/PM in place
;;;  of 24-hour time, in all relevant functions?]

(DEFUN MICROSECOND-TIME (&AUX (INHIBIT-SCHEDULING-FLAG T))
  (LET ((LOW (%UNIBUS-READ 764120))  ;Hardware synchronizes if you read this one first
	(HIGH (%UNIBUS-READ 764122)))
    (DPB HIGH 2020 LOW)))

(DEFUN FIXNUM-MICROSECOND-TIME (&AUX (INHIBIT-SCHEDULING-FLAG T))
  (LET ((LOW (%UNIBUS-READ 764120))
	(HIGH (%UNIBUS-READ 764122)))
    (DPB HIGH 2007 LOW)))

;;; Conversion routines, universal time is seconds since 1-jan-00 00:00-GMT

(DEFINE-SITE-VARIABLE *TIMEZONE* :TIMEZONE)

;;; One-based array of cumulative days per month.
(DEFVAR *CUMULATIVE-MONTH-DAYS-TABLE* (MAKE-ARRAY NIL 'ART-16B 13.))
(FILLARRAY *CUMULATIVE-MONTH-DAYS-TABLE*
	   '(0 0 31. 59. 90. 120. 151. 181. 212. 243. 273. 304. 334.))

;; Takes Univeral Time (seconds since 1/1/1900) as a 32-bit number
;; Algorithm from KLH's TIMRTS.
(DEFUN DECODE-UNIVERSAL-TIME (UNIVERSAL-TIME &OPTIONAL (TIMEZONE *TIMEZONE*)
					    &AUX SECS MINUTES HOURS DAY MONTH
					     YEAR DAY-OF-THE-WEEK DST-P)
  (DECLARE (RETURN-LIST SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P))
  (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
    (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME TIMEZONE))
  (AND (SETQ DST-P (DAYLIGHT-SAVINGS-TIME-P HOURS DAY MONTH YEAR))
       ;; See if it's daylight savings time, time-zone number gets smaller if so.
       (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
	 (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME (1- TIMEZONE))))
  (PROG () (RETURN SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DST-P)))

(DEFUN DECODE-UNIVERSAL-TIME-WITHOUT-DST (UNIVERSAL-TIME &OPTIONAL (TIMEZONE *TIMEZONE*)
							&AUX X SECS MINUTES HOURS
							 DAY MONTH YEAR)
  (DECLARE (RETURN-LIST SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK))
  (SETQ UNIVERSAL-TIME (- UNIVERSAL-TIME (* TIMEZONE 3600.)))
  (SETQ SECS (\ UNIVERSAL-TIME 86400.)		;(* 24. 60. 60.)
	X (// UNIVERSAL-TIME 86400.))		;Days since genesis.
  (LET ((B (\ X 365.))
	(A (// X 365.)))
    (COND ((NOT (ZEROP A))			
	   (SETQ B (- B (LSH (1- A) -2)))
	   (COND ((< B 0)
		  (SETQ A (1- A))
		  (SETQ A (+ A (// B 365.)))	;We must allow for times so far in the future
		  (SETQ B (\ B 365.))		;as to produce >> 365. Feb 29's.
		  (SETQ B (+ B 365.))		;(Of course, this doesn't allow for
						;the year 2100 not being a leap-year.)
		  (AND (NOT (BIT-TEST A 3))
		       (SETQ B (1+ B)))))))
    (DO ((C 12. (1- C)))
	(( B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C))
	 (COND ((AND (NOT (BIT-TEST A 3))
		     (> C 2))
		(SETQ B (1- B))
		(AND (< B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C))
		     (SETQ C (1- C)))
		(AND (= C 2)
		     (SETQ B (1+ B)))))
	 (SETQ B (- B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C)))
	 (SETQ YEAR A)
	 (SETQ MONTH C)
	 (SETQ DAY (1+ B)))))
  (SETQ HOURS (// SECS 3600.)
	MINUTES (// (\ SECS 3600.) 60.)
	SECS (\ SECS 60.))
  (PROG () (RETURN SECS MINUTES HOURS DAY MONTH YEAR (\ X 7))))

(DEFUN DAYLIGHT-SAVINGS-TIME-P (HOURS DAY MONTH YEAR)
  (COND ((OR (< MONTH 4)	;Standard time if before 2 am last Sunday in April
	     (AND (= MONTH 4)
		  (LET ((LSA (LAST-SUNDAY-IN-APRIL YEAR)))
		    (OR (< DAY LSA)
			(AND (= DAY LSA) (< HOURS 2))))))
	 NIL)
	((OR (> MONTH 10.)	;Standard time if after 1 am last Sunday in October
	     (AND (= MONTH 10.)
		  (LET ((LSO (LAST-SUNDAY-IN-OCTOBER YEAR)))
		    (OR (> DAY LSO)
			(AND (= DAY LSO) ( HOURS 1))))))
	 NIL)
	(T T)))

;;; Domain-dependent knowledge
(DEFUN LAST-SUNDAY-IN-OCTOBER (YEAR)
  (LET ((LSA (LAST-SUNDAY-IN-APRIL YEAR)))
    ;; Days between April and October = 31+30+31+31+30 = 153  6 mod 7
    ;; Therefore the last Sunday in October is one less than the last Sunday in April
    ;; unless that gives 24. in which case it is 31.
    (IF (= LSA 25.) 31. (1- LSA))))

(DEFUN LAST-SUNDAY-IN-APRIL (YEAR)
  (IF (> YEAR 100.)
      (SETQ YEAR (- YEAR 1900.)))
  ;; This copied from GDWOBY routine in ITS
  (LET ((DOW-BEG-YEAR
	  (LET ((B (\ (+ YEAR 1899.) 400.)))
	    (\ (- (+ (1+ B) (SETQ B (// B 4))) (// B 25.)) 7)))
	(FEB29 (IF (LEAP-YEAR-P YEAR) 1 0)))
    (LET ((DOW-APRIL-30 (\ (+ DOW-BEG-YEAR 119. FEB29) 7)))
      (- 30. DOW-APRIL-30))))

;;; Returns universal time, as a 32-bit number of seconds since 1/1/00 00:00-GMT
(DEFUN ENCODE-UNIVERSAL-TIME (SECONDS MINUTES HOURS DAY MONTH YEAR
			      &OPTIONAL TIMEZONE &AUX TEM)
  (AND (> YEAR 1900.) (SETQ YEAR (- YEAR 1900.)))
  (OR TIMEZONE
      (SETQ TIMEZONE (IF (DAYLIGHT-SAVINGS-TIME-P HOURS DAY MONTH YEAR)
			 (1- *TIMEZONE*) *TIMEZONE*)))
  (SETQ TEM (+ (1- DAY) (AREF *CUMULATIVE-MONTH-DAYS-TABLE* MONTH)
	       (// (1- YEAR) 4) (* YEAR 365.)))	;Number of days since 1/1/00.
  (AND (> MONTH 2) (LEAP-YEAR-P YEAR)
       (SETQ TEM (1+ TEM)))			;After 29-Feb in a leap year.
  (+ SECONDS (* 60. MINUTES) (* 3600. HOURS) (* TEM 86400.) (* TIMEZONE 3600.)))	;Return number of seconds.

;;; Maintenance functions

(DEFVAR *LAST-TIME-UPDATE-TIME* NIL)
(DEFVAR *LAST-TIME-SECONDS*)
(DEFVAR *LAST-TIME-MINUTES*)
(DEFVAR *LAST-TIME-HOURS*)
(DEFVAR *LAST-TIME-DAY*)
(DEFVAR *LAST-TIME-MONTH*)
(DEFVAR *LAST-TIME-YEAR*)
(DEFVAR *LAST-TIME-DAY-OF-THE-WEEK*)
(DEFVAR *LAST-TIME-DAYLIGHT-SAVINGS-P*)
(DEFVAR *NETWORK-TIME-FUNCTION* NIL)

(DEFUN INITIALIZE-TIMEBASE (&OPTIONAL UT)
  (AND (NULL UT) *NETWORK-TIME-FUNCTION*
       (SETQ UT (FUNCALL *NETWORK-TIME-FUNCTION*)))
  (PROG ()
	(AND (NUMBERP UT) (GO DO-IT))
     STRING
	(FORMAT QUERY-IO "~&Please type the date and time: ")
	(SETQ UT (READLINE QUERY-IO))
	(AND (STRING-EQUAL UT "")
	     (IF (Y-OR-N-P "Do you want to specify the time or not? ")
		 (GO STRING)
		 (RETURN (SETQ *LAST-TIME-UPDATE-TIME* NIL))))
	(SETQ UT (PARSE-UNIVERSAL-TIME UT 0 NIL T 0))
	(COND ((STRINGP UT)
	       (FUNCALL QUERY-IO ':STRING-OUT UT)
	       (GO STRING))
	      ((NOT (Y-OR-N-P (FORMAT NIL "Time is ~A, OK? " (PRINT-UNIVERSAL-DATE UT NIL))))
	       (GO STRING)))
     DO-IT
	(WITHOUT-INTERRUPTS
	  (SETQ *LAST-TIME-UPDATE-TIME* (FIXNUM-MICROSECOND-TIME))
	  (MULTIPLE-VALUE (*LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
			   *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
			   *LAST-TIME-DAY-OF-THE-WEEK* *LAST-TIME-DAYLIGHT-SAVINGS-P*)
	    (DECODE-UNIVERSAL-TIME UT))
	  (RETURN T))))

(DEFUN SET-LOCAL-TIME (&OPTIONAL NEW-TIME)
  (AND (STRINGP NEW-TIME)
       (SETQ NEW-TIME (TIME:PARSE-UNIVERSAL-TIME NEW-TIME)))
  (LET ((*NETWORK-TIME-FUNCTION* NIL))
    (INITIALIZE-TIMEBASE NEW-TIME)))

;; This is so freshly booted machines don't give out an incorrect time until
;; they've found out for themselves what the time *really* is.
(ADD-INITIALIZATION "Forget time" '(SETQ TIME:*LAST-TIME-UPDATE-TIME* NIL) '(BEFORE-COLD))

;This must not process-wait, since it can be called inside the scheduler via the who-line
(DEFUN UPDATE-TIMEBASE (&AUX TIME TICK)
  (COND ((NOT (NULL *LAST-TIME-UPDATE-TIME*))
	 (WITHOUT-INTERRUPTS
	   ;; Put the following code back if the TIME function ever makes any attempt
	   ;; to be even close to 60 cycles.  Also change INITIALIZE-TIMEBASE.
	   ;(SETQ TIME (TIME)
	   ;	 TICK (// (TIME-DIFFERENCE TIME *LAST-TIME-UPDATE-TIME*) 60.)
	   ;	 *LAST-TIME-UPDATE-TIME*
	   ;	    (LDB 0027 (%24-BIT-PLUS (* 60. TICK) *LAST-TIME-UPDATE-TIME*)))
	   (SETQ TIME (FIXNUM-MICROSECOND-TIME)
		 TICK (// (TIME-DIFFERENCE TIME *LAST-TIME-UPDATE-TIME*) 1000000.)
		 *LAST-TIME-UPDATE-TIME*
		    (LDB 0027 (%24-BIT-PLUS (* 1000000. TICK) *LAST-TIME-UPDATE-TIME*)))
	   (OR (ZEROP TICK)
	       (< (SETQ *LAST-TIME-SECONDS* (+ *LAST-TIME-SECONDS* TICK)) 60.)
	       (< (PROG1 (SETQ *LAST-TIME-MINUTES* (+ *LAST-TIME-MINUTES*
						      (// *LAST-TIME-SECONDS* 60.)))
			 (SETQ *LAST-TIME-SECONDS* (\ *LAST-TIME-SECONDS* 60.)))
		  60.)
	       (< (PROG1 (SETQ *LAST-TIME-HOURS* (+ *LAST-TIME-HOURS*
						    (// *LAST-TIME-MINUTES* 60.)))
			 (SETQ *LAST-TIME-MINUTES* (\ *LAST-TIME-MINUTES* 60.)))
		  24.)
	       ( (PROG1 (SETQ *LAST-TIME-DAY* (1+ *LAST-TIME-DAY*))
			 (SETQ *LAST-TIME-DAY-OF-THE-WEEK*
			       (\ (1+ *LAST-TIME-DAY-OF-THE-WEEK*) 7))
			 (SETQ *LAST-TIME-HOURS* 0))
		  (MONTH-LENGTH *LAST-TIME-MONTH* *LAST-TIME-YEAR*))
	       ( (SETQ *LAST-TIME-DAY* 1
			*LAST-TIME-MONTH* (1+ *LAST-TIME-MONTH*))
		  12.)
	       (SETQ *LAST-TIME-MONTH* 1
		     *LAST-TIME-YEAR* (1+ *LAST-TIME-YEAR*)))
	   T))
	;This used to call INITIALIZE-TIMEBASE.  However, since that gets called by
	;an initialization it seems best not to get processes into it at the same time.
	(T NIL)))

;;; One-based lengths of months
(DEFVAR *MONTH-LENGTHS* '(0 31. 28. 31. 30. 31. 30. 31. 31. 30. 31. 30. 31.))

(DEFUN MONTH-LENGTH (MONTH YEAR)
  (IF (= MONTH 2)
      (IF (LEAP-YEAR-P YEAR) 29. 28.)
      (NTH MONTH *MONTH-LENGTHS*)))

(DEFUN LEAP-YEAR-P (YEAR)
  (IF (< YEAR 100.)
      (SETQ YEAR (+ 1900. YEAR)))
  (AND (ZEROP (\ YEAR 4))
       (OR (NOT (ZEROP (\ YEAR 100.)))
	   (ZEROP (\ YEAR 400.)))))

(DEFUN DAYLIGHT-SAVINGS-P ()
  (UPDATE-TIMEBASE)
  *LAST-TIME-DAYLIGHT-SAVINGS-P*)

(DEFUN DEFAULT-YEAR ()
  (UPDATE-TIMEBASE)
  *LAST-TIME-YEAR*)

;;; These are the functions the user should call
;;; If they can't find out what time it is, they return NIL
(DEFUN GET-TIME ()
  (DECLARE (RETURN-LIST SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK
			DAYLIGHT-SAVINGS-P))
  (AND (UPDATE-TIMEBASE)
       (PROG () (RETURN *LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
			*LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
			*LAST-TIME-DAY-OF-THE-WEEK* *LAST-TIME-DAYLIGHT-SAVINGS-P*))))

(DEFUN GET-UNIVERSAL-TIME ()
  (UPDATE-TIMEBASE)
  (ENCODE-UNIVERSAL-TIME *LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
			 *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*))

(DEFUN PRINT-CURRENT-TIME (&OPTIONAL (STREAM STANDARD-OUTPUT))
  (AND (UPDATE-TIMEBASE)
       (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
	   (GET-TIME)
         (PRINT-TIME SECONDS MINUTES HOURS DAY MONTH YEAR STREAM))))

(DEFPROP FORMAT:DATIME FORMAT-CTL-DATIME FORMAT:FORMAT-CTL-NO-ARG)

(DEFUN FORMAT-CTL-DATIME (IGNORE)
  (PRINT-CURRENT-TIME))

(DEFUN PRINT-UNIVERSAL-TIME (UT &OPTIONAL (STREAM STANDARD-OUTPUT) (TIMEZONE *TIMEZONE*))
  (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME UT TIMEZONE)
    (PRINT-TIME SECONDS MINUTES HOURS DAY MONTH YEAR STREAM)))

(DEFPROP FORMAT:TIME FORMAT-CTL-TIME FORMAT:FORMAT-CTL-ONE-ARG)

(DEFUN FORMAT-CTL-TIME (UT IGNORE)
  (PRINT-UNIVERSAL-TIME UT))

(DEFUN PRINT-TIME (SECONDS MINUTES HOURS DAY MONTH YEAR &OPTIONAL (STREAM STANDARD-OUTPUT))
  (FORMAT STREAM
	  '( (D) "//" (D 2 60) "//" (D 2 60) " " (D 2 60) ":" (D 2 60) ":" (D 2 60) )
	      MONTH    DAY           YEAR         HOURS        MINUTES	  SECONDS))

(DEFUN PRINT-CURRENT-DATE (&OPTIONAL (STREAM STANDARD-OUTPUT))
  (AND (UPDATE-TIMEBASE)
       (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
	   (GET-TIME)
         (PRINT-DATE SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK STREAM))))

(DEFUN PRINT-UNIVERSAL-DATE (UT &OPTIONAL (STREAM STANDARD-OUTPUT) (TIMEZONE *TIMEZONE*))
  (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
      (DECODE-UNIVERSAL-TIME UT TIMEZONE)
    (PRINT-DATE SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK STREAM)))

(DEFPROP FORMAT:DATE FORMAT-CTL-DATE FORMAT:FORMAT-CTL-ONE-ARG)

(DEFUN FORMAT-CTL-DATE (UT IGNORE)
  (PRINT-UNIVERSAL-DATE UT))

(DEFUN PRINT-DATE (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK
		   &OPTIONAL (STREAM STANDARD-OUTPUT))
  (SETQ MONTH (MONTH-STRING MONTH)
	DAY-OF-THE-WEEK (DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK))
  (FORMAT STREAM
	  "~A the ~:R of ~A, 19~D/; ~D:~2,48D:~2,48D ~A"
	  DAY-OF-THE-WEEK DAY MONTH YEAR (1+ (\ (+ HOURS 11.) 12.)) MINUTES SECONDS
	  (COND ((AND (ZEROP SECONDS)
		      (ZEROP MINUTES)
		      (MEMQ HOURS '(0 12.)))
		 (IF (= HOURS 0) "midnight" "noon"))
		(( HOURS 12.) "pm")
		(T "am")) ))

(DEFUN PRINT-BRIEF-UNIVERSAL-TIME (UT &OPTIONAL (STREAM STANDARD-OUTPUT)
						(REF-UT (GET-UNIVERSAL-TIME)))
  "Prints only those aspects of the time, UT, that differ from the current time.
Also never prints seconds.  Used by notifications, for example."
  (MULTIPLE-VALUE-BIND (IGNORE MINUTES HOURS DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME UT)
    (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE REF-DAY REF-MONTH REF-YEAR)
	(DECODE-UNIVERSAL-TIME REF-UT)
      ;; If not same day, print month and day numerically
      (IF (OR ( DAY REF-DAY) ( MONTH REF-MONTH) ( YEAR REF-YEAR))
	  (FORMAT STREAM "~D//~D~:[//~2,'0D~] " MONTH DAY (= YEAR REF-YEAR) YEAR))
      ;; Always print hours colon minutes, even if same as now
      (FORMAT STREAM "~2,'0D:~2,'0D" HOURS MINUTES))))

;;; Some useful strings and accessing functions.

;;; Days of the week.  Elements must be (in order):
;;; (1) Three-letter form.
;;; (2) Full spelling.
;;; (3) Middle-length form if any, else NIL.
;;; (4) Francais.
;;; (5) Deutsch.
(DEFVAR *DAYS-OF-THE-WEEK* '(("Mon" "Monday" NIL "Lundi" "Montag")
			     ("Tue" "Tuesday" "Tues" "Mardi" "Dienstag")
			     ("Wed" "Wednesday" NIL "Mercredi" "Mittwoch")
			     ("Thu" "Thursday" "Thurs" "Jeudi" "Donnerstag")
			     ("Fri" "Friday" NIL "Vendredi" "Freitag")
			     ("Sat" "Saturday" NIL "Samedi" "Samstag")
			     ("Sun" "Sunday" NIL "Dimanche" "Sonntag")))

(DEFUN DAY-OF-THE-WEEK-STRING (DAY-OF-THE-WEEK &OPTIONAL (MODE ':LONG) &AUX STRINGS)
  (SETQ STRINGS (NTH DAY-OF-THE-WEEK *DAYS-OF-THE-WEEK*))
  (SELECTQ MODE
    (:SHORT (FIRST STRINGS))
    (:LONG (SECOND STRINGS))
    (:MEDIUM (OR (THIRD STRINGS) (FIRST STRINGS)))
    (:FRENCH (FOURTH STRINGS))
    (:GERMAN (FIFTH STRINGS))
    (OTHERWISE (FERROR NIL "~S is not a known mode" MODE))))


;;; Months of the year:  Elements must be (in order):
;;; (1) Three-letter form.
;;; (2) Full spelling.
;;; (3) Middle-length form if any, else NIL.
;;; (4) Francais.
;;; (5) Roman numberals (used in Europe).
;;; (6) Deutsch.
(DEFVAR *MONTHS* '(("Jan" "January" NIL "Janvier" "I" "Januar")
		   ("Feb" "February" NIL "Fevrier" "II" "Februar")
		   ("Mar" "March" NIL "Mars" "III" "Maerz")
		   ("Apr" "April" NIL "Avril" "IV" "April")
		   ("May" "May" NIL "Mai" "V" "Mai")
		   ("Jun" "June" NIL "Juin" "VI" "Juni")
		   ("Jul" "July" NIL "Juillet" "VII" "Juli")
		   ("Aug" "August" NIL "Aout" "VIII" "August")
		   ("Sep" "September" "Sept" "Septembre" "IX" "September")
		   ("Oct" "October" NIL "Octobre" "X" "Oktober")
		   ("Nov" "November" "Novem" "Novembre" "XI" "November")
		   ("Dec" "December" "Decem" "Decembre" "XII" "Dezember")))

(DEFUN MONTH-STRING (MONTH &OPTIONAL (MODE ':LONG) &AUX STRINGS)
  (SETQ STRINGS (NTH (1- MONTH) *MONTHS*))
  (SELECTQ MODE
    (:SHORT (FIRST STRINGS))
    (:LONG (SECOND STRINGS))
    (:MEDIUM (OR (THIRD STRINGS) (FIRST STRINGS)))
    (:FRENCH (FOURTH STRINGS))
    (:ROMAN (FIFTH STRINGS))
    (:GERMAN (SIXTH STRINGS))
    (OTHERWISE (FERROR NIL "~S is not a known mode" MODE))))

;;; minutes offset from gmt, normal name, daylight name, miltary character
(DEFVAR *TIMEZONES* '((0 "GMT" NIL #/Z)			;Greenwich
		      (1 NIL NIL #/A)
		      (2 NIL NIL #/B)
		      (3 NIL "ADT" #/C)
		      (4 "AST" "EDT" #/D)		;Atlantic
		      (5 "EST" "CDT" #/E)		;Eastern
		      (6 "CST" "MDT" #/F)		;Central
		      (7 "MST" "PDT" #/G)		;Mountain
		      (8 "PST" "YDT" #/H)		;Pacific
		      (9 "YST" "HDT" #/I)		;Yukon
		      (10. "HST" "BDT" #/K)		;Hawaiian
		      (11. "BST" NIL #/L)		;Bering
		      (12. NIL NIL #/M)
		      (-1 NIL NIL #/N)
		      (-2 NIL NIL #/O)
		      (-3 NIL NIL #/P)
		      (-4 NIL NIL #/Q)
		      (-5 NIL NIL #/R)
		      (-6 NIL NIL #/S)
		      (-7 NIL NIL #/T)
		      (-8 NIL NIL #/U)
		      (-9 NIL NIL #/V)
		      (-10. NIL NIL #/W)
		      (-11. NIL NIL #/X)
		      (-12. NIL NIL #/Y)
		      (3.5 "NST" NIL -1)		;Newfoundland
		      ))

(DEFUN TIMEZONE-STRING (&OPTIONAL (TIMEZONE *TIMEZONE*)
				  (DAYLIGHT-SAVINGS-P (DAYLIGHT-SAVINGS-P)))
  (IF DAYLIGHT-SAVINGS-P
      (THIRD (NTH (1- TIMEZONE) *TIMEZONES*))
      (SECOND (NTH TIMEZONE *TIMEZONES*))))

;;; Date and time parsing

(DEFMACRO BAD-DATE-OR-TIME (REASON . ARGS)
  `(*THROW 'BAD-DATE-OR-TIME ,(IF (NULL ARGS) REASON `(FORMAT NIL ,REASON . ,ARGS))))

;;; Check that a date is ok: day is within month; and day-of-week, if specified, is valid
(DEFUN VERIFY-DATE (DAY MONTH YEAR DAY-OF-THE-WEEK)
  (COND ((> DAY (MONTH-LENGTH MONTH YEAR))
	 (FORMAT NIL "~A only has ~D day~:P" (MONTH-STRING MONTH) (MONTH-LENGTH MONTH YEAR)))
	(DAY-OF-THE-WEEK
	 (LET ((UT (ENCODE-UNIVERSAL-TIME 0 0 0 DAY MONTH YEAR)))
	   (MULTIPLE-VALUE-BIND (NIL NIL NIL NIL NIL NIL CORRECT-DAY-OF-THE-WEEK)
	       (DECODE-UNIVERSAL-TIME UT)
	     (AND ( DAY-OF-THE-WEEK CORRECT-DAY-OF-THE-WEEK)
		  (FORMAT NIL "~A the ~:R ~D is a ~A, not a ~A"
			  (MONTH-STRING MONTH) DAY YEAR
			  (DAY-OF-THE-WEEK-STRING CORRECT-DAY-OF-THE-WEEK)
			  (DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK))))))
	(T
	 NIL)))

(COMMENT
(DEFUN TEST ()
  (DO ((LINE)) (NIL)
    (TERPRI)
    (SETQ LINE (READLINE))
    (AND (EQUAL LINE "") (RETURN NIL))
    (MULTIPLE-VALUE-BIND (SECONDS NIL DAY MONTH YEAR NIL NIL ERRMES)
	(PARSE-DATE-AND-TIME LINE)
      (IF ERRMES
	  (PRINC ERRMES)
	  (PRINT-DATE-AND-TIME SECONDS DAY MONTH YEAR)))))

(DEFUN PRINT-DATE-AND-TIME (SECONDS MINUTES HOURS DAY MONTH YEAR &AUX MINUTES HOURS)
  (SETQ	MONTH (MONTH-STRING MONTH ':SHORT))
  (FORMAT T "~D-~A-~D ~D:~2,48D:~2,48D"
	  DAY MONTH YEAR HOURS MINUTES SECONDS))
)

;;;
;;;  the following routines are obsolete!!
;;;

;; Takes Univeral Time (seconds since 1/1/1900) as a 32-bit number
;; Algorithm from KLH's TIMRTS.
(DEFUN TIME-BREAK-UNIVERSAL (UNIVERSAL-TIME &OPTIONAL (TIMEZONE *TIMEZONE*)
					    &AUX SECS DAY MONTH YEAR DAY-OF-THE-WEEK DST-P)
  (DECLARE (RETURN-LIST SECS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P))
  (MULTIPLE-VALUE (SECS DAY MONTH YEAR DAY-OF-THE-WEEK)
    (TIME-BREAK-UNIVERSAL-WITHOUT-DST UNIVERSAL-TIME TIMEZONE))
  (AND (SETQ DST-P (DAYLIGHT-SAVINGS-TIME-P (\ SECS 3600.) DAY MONTH YEAR))
       ;; See if it's daylight savings time, time-zone number gets smaller if so.
       (MULTIPLE-VALUE (SECS DAY MONTH YEAR DAY-OF-THE-WEEK)
	 (TIME-BREAK-UNIVERSAL-WITHOUT-DST UNIVERSAL-TIME (1- TIMEZONE))))
  (PROG () (RETURN SECS DAY MONTH YEAR DAY-OF-THE-WEEK DST-P)))

(DEFUN TIME-BREAK-UNIVERSAL-WITHOUT-DST (UNIVERSAL-TIME &OPTIONAL (TIMEZONE *TIMEZONE*)
							&AUX X SECS DAY MONTH YEAR)
  (DECLARE (RETURN-LIST SECS DAY MONTH YEAR DAY-OF-THE-WEEK))
  (SETQ UNIVERSAL-TIME (- UNIVERSAL-TIME (* TIMEZONE 3600.)))
  (SETQ SECS (\ UNIVERSAL-TIME 86400.)		;(* 24. 60. 60.)
	X (// UNIVERSAL-TIME 86400.))		;Days since genesis.
  (LET ((B (\ X 365.))
	(A (// X 365.)))
    (COND ((NOT (ZEROP A))
	   (SETQ B (- B (LSH (1- A) -2)))
	   (COND ((< B 0)
		  (SETQ A (1- A))
		  (SETQ B (+ B 365.))
		  (AND (NOT (BIT-TEST A 3))
		       (SETQ B (1+ B)))))))
    (DO ((C 12. (1- C)))
	(( B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C))
	 (COND ((AND (NOT (BIT-TEST A 3))
		     (> C 2))
		(SETQ B (1- B))
		(AND (< B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C))
		     (SETQ C (1- C)))
		(AND (= C 2)
		     (SETQ B (1+ B)))))
	 (SETQ B (- B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C)))
	 (SETQ YEAR A)
	 (SETQ MONTH C)
	 (SETQ DAY (1+ B)))))
  (PROG () (RETURN SECS DAY MONTH YEAR (\ X 7))))



;;; Returns universal time, as a 32-bit number of seconds since 1/1/00 00:00-GMT
(DEFUN TIME-UNBREAK-UNIVERSAL (SECONDS DAY MONTH YEAR &OPTIONAL TIMEZONE &AUX TEM)
  (AND (> YEAR 1900.) (SETQ YEAR (- YEAR 1900.)))
  (OR TIMEZONE
      (SETQ TIMEZONE (IF (DAYLIGHT-SAVINGS-TIME-P (\ SECONDS 3600.) DAY MONTH YEAR)
			 (1- *TIMEZONE*) *TIMEZONE*)))
  (SETQ TEM (+ (1- DAY) (AREF *CUMULATIVE-MONTH-DAYS-TABLE* MONTH)
	       (// (1- YEAR) 4) (* YEAR 365.)))	;Number of days since 1/1/00.
  (AND (> MONTH 2) (LEAP-YEAR-P YEAR)
       (SETQ TEM (1+ TEM)))			;After 29-Feb in a leap year.
  (+ SECONDS (* TEM 86400.) (* TIMEZONE 3600.)))	;Return number of seconds.


;;; These are the functions the user should call
;;; If they can't find out what time it is, they return NIL

(DEFUN WHAT-TIME (&OPTIONAL (STREAM STANDARD-OUTPUT))
  (AND (UPDATE-TIMEBASE)
       (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
	   (GET-TIME)
         (AND STREAM (TERPRI STREAM))
         (TIME-PRINT STREAM SECONDS MINUTES HOURS DAY MONTH YEAR))))


(DEFUN TIME-PRINT-UNIVERSAL (UT &OPTIONAL (STREAM STANDARD-OUTPUT) (TIMEZONE *TIMEZONE*))
  (MULTIPLE-VALUE-BIND (SECONDS DAY MONTH YEAR)
      (TIME-BREAK-UNIVERSAL UT TIMEZONE)
    (LET (MINUTES HOURS)
      (SETQ MINUTES (// SECONDS 60.)
	    SECONDS (\ SECONDS 60.))
      (SETQ HOURS (// MINUTES 60.)
	    MINUTES (\ MINUTES 60.))
      (TIME-PRINT STREAM SECONDS MINUTES HOURS DAY MONTH YEAR))))

(DEFUN TIME-PRINT (STREAM SECONDS MINUTES HOURS DAY MONTH YEAR)
  (FORMAT STREAM
	  '( (D) "//" (D 2 60) "//" (D 2 60) " " (D 2 60) ":" (D 2 60) ":" (D 2 60) )
	       MONTH DAY           YEAR         HOURS        MINUTES	  SECONDS))

(ADD-INITIALIZATION "Initialize Timebase" '(INITIALIZE-TIMEBASE) '(:WARM :NOW))
(ADD-INITIALIZATION "Forget Time" '(SETQ *LAST-TIME-UPDATE-TIME* NIL) '(:BEFORE-COLD))
