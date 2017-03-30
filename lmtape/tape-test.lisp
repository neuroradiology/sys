;-*- Mode:LISP; Package:TAPE; Base:8 -*-

; Tape diagnostic

; Parameters user changeable

(DEFVAR *TEST-RECORD-LENGTH* 4096.)
(DEFVAR *TEST-COUNTING-PATTERN* T)
(DEFVAR *TEST-PATTERN* 0)
(DEFVAR *TEST-N-RECORDS* 100.)
(DEFVAR *TEST-READ-RETRY* NIL)
(DEFVAR *TEST-WRITE-RETRY* NIL)
(DEFVAR *TEST-READ-RETRY-COUNT* 5)
(DEFVAR *TEST-WRITE-RETRY-COUNT* 5)
(DEFVAR *TEST-BACKGROUND* 0)
(DEFVAR *TEST-BUFFER*)

(DEFCONST *TEST-CHOICES*
      '((*TEST-RECORD-LENGTH* "Bytes per record" :NUMBER)
	(*TEST-N-RECORDS* "Number of records before rewind" :NUMBER)
	(*TEST-COUNTING-PATTERN* "Use counting byte values" :BOOLEAN)
	(*TEST-PATTERN* "Fixed value for every byte" :OCTAL-NUMBER)
	"	Used only if not counting"
	(*TEST-READ-RETRY* "Retry read errors" :BOOLEAN)
	(*TEST-READ-RETRY-COUNT* "Number of read retries" :NUMBER)
	(*TEST-WRITE-RETRY* "Retry write errors" :BOOLEAN)
	(*TEST-WRITE-RETRY-COUNT* "Number of write retries" :NUMBER)
	(*TEST-BACKGROUND* "Initial contents of buffer before read" :OCTAL-NUMBER)))

(DEFPROP :OCTAL-NUMBER
	 (PRINT-OCTAL-NUMBER READ-OCTAL-NUMBER NIL NIL NIL
	  "Click left to input a new octal number from the keyboard.")
	 TV:CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFUN PRINT-OCTAL-NUMBER (NUM STREAM)
  (FORMAT STREAM "~O" NUM))

(DEFUN READ-OCTAL-NUMBER (STREAM)
  (LET ((IBASE 8)) (TV:READ-NUMBER STREAM)))

(DEFUN TEST ()
  ;; Let user pick parameters
  (LET ((BASE 10.) (IBASE 10.) (*NOPOINT NIL))
    (TV:CHOOSE-VARIABLE-VALUES *TEST-CHOICES*
			       ':LABEL "Tape test parameters"
			       ':MARGIN-CHOICES '("Run Test")))
  (OR (ZEROP (\ *TEST-RECORD-LENGTH* 4))
      (FERROR NIL "Record length must be a multiple of 4 for some reason"))
  ;; Set up test buffer with record to be written
  (SETQ *TEST-BUFFER* 
	(IF (BOUNDP '*TEST-BUFFER*)
	    (ADJUST-ARRAY-SIZE *TEST-BUFFER* *TEST-RECORD-LENGTH*)
	    (MAKE-ARRAY *TEST-RECORD-LENGTH* ':TYPE 'ART-8B)))
  (IF *TEST-COUNTING-PATTERN*
      (LOOP FOR I FROM 0 BELOW *TEST-RECORD-LENGTH*
	    DO (ASET I *TEST-BUFFER* I))
      (LOOP FOR I FROM 0 BELOW *TEST-RECORD-LENGTH*
	    DO (ASET *TEST-PATTERN* *TEST-BUFFER* I)))
    ;Should there be a pattern which is different in each record,
    ;to detect problems with missing records or backspacing?
  ;; Wire down buffer
  (UNWIND-PROTECT
    (LET ((BUFADR (+ (%POINTER *TEST-BUFFER*)
		     (IF (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG *TEST-BUFFER* 0) 2 1))))
      (SI:WIRE-ARRAY *TEST-BUFFER* NIL NIL T)
      (PRINT-AND-RESET-BUSINT-STATUS)
      ;; Rewind tape and write test records
      (TAPE-COMMAND 0 ':REWIND)
      (TAPE-WAIT-REWIND-COMPLETE)
      (LOOP FOR RECORD-NUMBER FROM 0 BELOW *TEST-N-RECORDS*
	    DO (TAPE-COMMAND 0 ':WRITE BUFADR *TEST-RECORD-LENGTH*)
	       (TAPE-WAIT-OP-COMPLETE)
	       (PRINT-AND-RESET-BUSINT-STATUS)
	    WHEN (TAPE-ERROR-P)
	      DO (FORMAT T "~&Error writing record ~D:~%" RECORD-NUMBER)
		 (TSTAT)
	      AND WHEN *TEST-WRITE-RETRY*
	        DO (LOOP FOR RETRY-COUNT FROM 1 TO *TEST-WRITE-RETRY-COUNT*
			 DO (TAPE-COMMAND 0 ':SPACE-REVERSE NIL 1)
			    (TAPE-WAIT-OP-COMPLETE)
			    (TAPE-COMMAND 0 ':WRITE-ERG BUFADR *TEST-RECORD-LENGTH*)
			    (TAPE-WAIT-OP-COMPLETE)
			    (PRINT-AND-RESET-BUSINT-STATUS)
			 WHEN (TAPE-ERROR-P)
			   DO (FORMAT T "~&Error in retry ~D of writing record ~D:~%"
				        RETRY-COUNT RECORD-NUMBER)
			      (TSTAT)
			 ELSE RETURN
			        (FORMAT T "~&Record ~D written successfully on ~:R try.~%"
					  RECORD-NUMBER (1+ RETRY-COUNT))))
      (FORMAT T "~&~D records written." *TEST-N-RECORDS*)
      ;; Rewind tape and read back the test records
      (TAPE-COMMAND 0 ':REWIND)
      (TAPE-WAIT-REWIND-COMPLETE)
      (LOOP FOR RECORD-NUMBER FROM 0 BELOW *TEST-N-RECORDS* DO
	(LOOP FOR RETRY-COUNT FROM 0 TO *TEST-READ-RETRY-COUNT* AS LOSE = NIL
	      DO (LOOP FOR I FROM 0 BELOW *TEST-RECORD-LENGTH*
		       DO (ASET *TEST-BACKGROUND* *TEST-BUFFER* I))
		 (TAPE-COMMAND 0 ':READ BUFADR *TEST-RECORD-LENGTH*)
		 (TAPE-WAIT-OP-COMPLETE)
		 (PRINT-AND-RESET-BUSINT-STATUS)
	      WHEN (TAPE-ERROR-P)
	        DO (FORMAT T "~&Error reading record ~D ~:[(~:R retry)~]:~%"
			     RECORD-NUMBER (ZEROP RETRY-COUNT) RETRY-COUNT)
		   (TSTAT)
		   (SETQ LOSE T)
	      UNLESS (CHECK-TEST-PATTERN RECORD-NUMBER RETRY-COUNT LOSE)
	        DO (SETQ LOSE T)
	      UNTIL (OR (NOT LOSE) (NOT *TEST-READ-RETRY*))
	      DO (TAPE-COMMAND 0 ':SPACE-REVERSE NIL 1)
		 (TAPE-WAIT-OP-COMPLETE)
	      FINALLY
	        (AND (NOT LOSE)
		     (> RETRY-COUNT 0)
		     (FORMAT T "~&Record ~D read successfully on ~:R try.~%"
			       RECORD-NUMBER RETRY-COUNT))))
      (FORMAT T "~&~D records read." *TEST-N-RECORDS*)
      ;; Rewind tape again
      (TAPE-COMMAND 0 ':REWIND))
    (SI:UNWIRE-ARRAY *TEST-BUFFER*)))

;Prints error messages and returns NIL if lose, or returns T
(DEFUN CHECK-TEST-PATTERN (RECORD-NUMBER RETRY-COUNT HEADER-PRINTED?)
  (LOOP FOR I FROM 0 BELOW *TEST-RECORD-LENGTH* AS WIN = T
	AS GOOD = (IF *TEST-COUNTING-PATTERN* (LOGAND 377 I) *TEST-PATTERN*)
	AS BAD = (AREF *TEST-BUFFER* I)
	UNLESS (= GOOD BAD)
	  DO (COND ((NOT HEADER-PRINTED?)
		    (FORMAT T "~&Unflagged error reading record ~D ~:[(~:R retry)~]:~%"
			      RECORD-NUMBER (ZEROP RETRY-COUNT) RETRY-COUNT)
		    (TSTAT)
		    (SETQ HEADER-PRINTED? T)))
	     (FORMAT T "Byte ~O (~D.), good ~O, bad ~O~%" I I GOOD BAD)
	     (SETQ WIN NIL)
        FINALLY (RETURN WIN)))

(DEFUN PRINT-AND-RESET-BUSINT-STATUS ()
  (LET ((STATUS (%UNIBUS-READ 766104)))
    (COND ((BIT-TEST 77 STATUS)
	   (FORMAT T "~&Bus interface error status: ~O  " STATUS)
	   (LOOP FOR NAME IN '(XBUS-NXM-ERR XBUS-PARITY-ERR CADR-ADDRESS-PARITY-ERR
			       UNIBUS-NXM-ERR CADR-DATA-PARITY-ERR UNIBUS-MAP-ERR)
		 AS BIT = 1 THEN (LSH BIT 1)
		 WHEN (BIT-TEST BIT STATUS) DO (PRINC NAME) (TYO #\SP))
	   (TERPRI)
	   ;; Reset the error status
	   (%UNIBUS-WRITE 766044 0)))))

(DEFCONST *STREAM-TEST-CHOICES*
      '((*TEST-RECORD-LENGTH* "Bytes per record" :NUMBER)
	(*TEST-N-RECORDS* "Number of records before rewind" :NUMBER)))

;; Write records containing bytes counting from 1 to 69 then read them back
(DEFUN STREAM-TEST ()
  ;; Let user pick parameters
  (LET ((BASE 10.) (IBASE 10.) (*NOPOINT NIL))
    (TV:CHOOSE-VARIABLE-VALUES *STREAM-TEST-CHOICES*
			       ':LABEL "Stream test parameters"
			       ':MARGIN-CHOICES '("Run Test")))
  (OR (ZEROP (\ *TEST-RECORD-LENGTH* 4))
      (FERROR NIL "Record length must be a multiple of 4 for some reason"))
  (LET ((STREAM NIL))
    (UNWIND-PROTECT
      (LET ((BUF (MAKE-ARRAY 69. ':TYPE 'ART-8B)))
	;; Set up test buffer
	(LOOP FOR I FROM 1 TO (ARRAY-LENGTH BUF)
	      DO (ASET I BUF (1- I)))
	;; Rewind tape
	(TAPE-COMMAND 0 ':REWIND)
	(TAPE-WAIT-REWIND-COMPLETE)
	;; Write the records
	(SETQ STREAM (TAPE:OPEN-TAPE 0 ':MODE ':WRITE ':RECORD-LENGTH *TEST-RECORD-LENGTH*))
	(LOOP REPEAT (// (* *TEST-RECORD-LENGTH* *TEST-N-RECORDS*) (ARRAY-LENGTH BUF))
	      DO (FUNCALL STREAM ':STRING-OUT BUF))
	(FUNCALL STREAM ':EOF)
	(FUNCALL STREAM ':CLOSE)
	;; Rewind tape
	(TAPE-COMMAND 0 ':REWIND)
	(TAPE-WAIT-REWIND-COMPLETE)
	;; Read back and compare
	(SETQ STREAM (TAPE:OPEN-TAPE 0 ':MODE ':READ ':RECORD-LENGTH *TEST-RECORD-LENGTH*))
;	(LOOP FOR I FROM 0 BELOW (* (// (* *TEST-RECORD-LENGTH* *TEST-N-RECORDS*)
;					(ARRAY-LENGTH BUF))
;				    (ARRAY-LENGTH BUF))
;	      AS CH = (FUNCALL STREAM ':TYI "Unexpected EOF")
;	      UNLESS (= CH (1+ (\ I 69.)))
;		DO (FORMAT T "~&Byte number ~D. (~O*69.+~O) has ~O not ~O"
;			     I (// I 69.) (\ I 69.) CH (1+ (\ I 69.))))
	(LOOP FOR R FROM 0 BELOW (// (* *TEST-RECORD-LENGTH* *TEST-N-RECORDS*)
				     (ARRAY-LENGTH BUF))
	      DO (SI:FILL-ARRAY-FROM-STREAM STREAM BUF (ARRAY-LENGTH BUF) "Unexpected EOF")
	      DO (LOOP FOR I FROM 0 BELOW 69.
		       UNLESS (= (AREF BUF I) (1+ I))
		         DO (FORMAT T "~&Byte number ~D. (~O*69.+~O) has ~O not ~O"
				      (+ (* R 69.) I) R I (AREF BUF I) (1+ I))))
	(FUNCALL STREAM ':CLOSE))
      (AND STREAM (FUNCALL STREAM ':CLOSE ':ABORT))))
  (TAPE-COMMAND 0 ':REWIND))
