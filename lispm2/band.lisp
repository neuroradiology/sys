;-*- Mode:LISP; Package:System-Internals; Base:8 -*-

(DEFUN GET-PARTITION-SIZE (PART &AUX PART-BASE PART-SIZE RQB BUF)
  (MULTIPLE-VALUE (PART-BASE PART-SIZE) (SYS:FIND-DISK-PARTITION PART))
  (AND (STRING-EQUAL PART "LOD" 0 0 3 3)
       (UNWIND-PROTECT
	 (PROGN (SETQ RQB (SYS:GET-DISK-RQB 1))
		(SETQ BUF (SYS:RQB-BUFFER RQB))
		(SYS:DISK-READ RQB 0 (1+ PART-BASE))
		(LET ((SIZE (DPB (AREF BUF (1+ (* 2 SYS:%SYS-COM-VALID-SIZE)))
				 1010		;Knows page-size is 2^8
				 (LDB 1010 (AREF BUF (* 2 SYS:%SYS-COM-VALID-SIZE))))))
		  (COND ((AND (> SIZE 10) ( SIZE PART-SIZE))
			 (SETQ PART-SIZE SIZE)))))
	 (SYS:RETURN-DISK-RQB RQB)))
  PART-SIZE)

(DEFUNP BAND-TRANSFER-SERVER (&AUX CONN PKT STR TEM RQB BUF WRITE-P (QUANTUM 17.)
				   PART-BASE PART-SIZE PART-COMMENT SUB-START SUB-N)
 (UNWIND-PROTECT (PROGN
  (SETQ CONN (CHAOS:LISTEN "BAND-TRANSFER" 25.))
  (PROCESS-WAIT "Listen" #'(LAMBDA (CONN) (EQ (CHAOS:STATE CONN) 'CHAOS:RFC-RECEIVED-STATE))
			 CONN)
  (SETQ STR (CHAOS:PKT-STRING (CHAOS:READ-PKTS CONN)))	;Look at the RFC
  (LET ((IBASE 10.))	;RFC is BAND-TRANSFER READ/WRITE band subset size comment
			;subset is NIL or list of rel start and n-blocks
    (SETQ TEM (READ-FROM-STRING (STRING-APPEND "(" STR ")"))))
  (MULTIPLE-VALUE (PART-BASE PART-SIZE) (SYS:FIND-DISK-PARTITION (THIRD TEM)))
  (OR PART-BASE
      (RETURN (CHAOS:REJECT CONN (FORMAT NIL "No /"~A/" partition here." (THIRD TEM)))))
  (AND (FOURTH TEM) (SETQ SUB-START (FIRST (FOURTH TEM)) SUB-N (SECOND (FOURTH TEM))))
  (COND ((STRING-EQUAL (SECOND TEM) "READ")
	 (SETQ WRITE-P NIL)
	 (SETQ PART-COMMENT (SI:PARTITION-COMMENT (THIRD TEM) 0)))
	((STRING-EQUAL (SECOND TEM) "WRITE")
	 (SETQ WRITE-P T)
	 (OR ( (FIFTH TEM) PART-SIZE)
	     (RETURN (CHAOS:REJECT CONN "Partition too small")))
	 (SETQ PART-SIZE (FIFTH TEM))
	 (SETQ PART-COMMENT (STRING (SIXTH TEM))))	;Comment to store later
	(T (RETURN (CHAOS:REJECT CONN "Illegal operation, must be READ or WRITE"))))
  (AND SUB-START (OR (MINUSP SUB-START) (MINUSP SUB-N) (> (+ SUB-START SUB-N) PART-SIZE))
       (CHAOS:REJECT CONN "Subset outside of partition"))
  (CHAOS:ACCEPT CONN)
  (BEEP)
  (FORMAT (TV:GET-NOTIFICATION-STREAM)
	  "~&[BAND-TRANSFER-SERVER: ~:[READ~;WRITE~] of ~A partition by ~A]~%"
	  WRITE-P (THIRD TEM) (CHAOS:HOST-DATA (CHAOS:FOREIGN-ADDRESS CONN)))
  (SETQ QUANTUM (GCD PART-SIZE QUANTUM))
  (COND ((NOT WRITE-P)				;Send packet containing size, comment
	 (SETQ PART-SIZE (GET-PARTITION-SIZE (THIRD TEM)))
	 (SETQ PKT (CHAOS:GET-PKT))
	 (CHAOS:SET-PKT-STRING PKT (FORMAT NIL "~D ~S" PART-SIZE PART-COMMENT))
	 (CHAOS:SEND-PKT CONN PKT)))
  (AND SUB-START (SETQ PART-BASE (+ PART-BASE SUB-START)
		       PART-SIZE SUB-N
		       QUANTUM (GCD PART-SIZE QUANTUM)))
  (SETQ RQB (SYS:GET-DISK-RQB QUANTUM)
	BUF (SYS:RQB-BUFFER RQB))
  (SI:WIRE-DISK-RQB RQB)
  (DO BLOCK PART-BASE (+ BLOCK QUANTUM) ( BLOCK (+ PART-BASE PART-SIZE))
    (COND ((NOT WRITE-P)
	   (SI:DISK-READ-WIRED RQB 0 BLOCK)
	   (ARRAY-TO-NET BUF CONN))
	  (T (ARRAY-FROM-NET BUF CONN)
	     (SI:DISK-WRITE-WIRED RQB 0 BLOCK))))
  (CHAOS:FINISH CONN)
  (CHAOS:CLOSE CONN "Done")
  (AND WRITE-P (SI:UPDATE-PARTITION-COMMENT (THIRD TEM) PART-COMMENT 0)))
 (AND RQB (SYS:RETURN-DISK-RQB RQB))
 (AND CONN (CHAOS:REMOVE-CONN CONN))))

(DEFUN ARRAY-TO-NET (BUF CONN &AUX PKT (NHWDS (ARRAY-LENGTH BUF))
				    (N (// CHAOS:MAX-DATA-BYTES-PER-PKT 2)))
  (DO I 0 (+ I N) ( I NHWDS)
    (SETQ N (MIN (- NHWDS I) N))
    (SETQ PKT (CHAOS:GET-PKT))
    (COPY-ARRAY-PORTION BUF I (+ I N)
			PKT CHAOS:FIRST-DATA-WORD-IN-PKT (+ CHAOS:FIRST-DATA-WORD-IN-PKT N))
    (SETF (CHAOS:PKT-NBYTES PKT) (* N 2))
    (CHAOS:SEND-PKT CONN PKT 300)))

(DEFUN ARRAY-FROM-NET (BUF CONN &AUX PKT (NHWDS (ARRAY-LENGTH BUF)) N)
  (DO I 0 (+ I N) ( I NHWDS)
    (SETQ PKT (CHAOS:GET-NEXT-PKT CONN))
    (SETQ N (// (CHAOS:PKT-NBYTES PKT) 2))
    (COPY-ARRAY-PORTION PKT CHAOS:FIRST-DATA-WORD-IN-PKT (+ CHAOS:FIRST-DATA-WORD-IN-PKT N)
			BUF I (+ I N))
    (CHAOS:RETURN-PKT PKT)))

(DEFUNP RECEIVE-BAND (FROM-MACHINE FROM-PART TO-PART
		      &OPTIONAL SUBSET-START SUBSET-N-BLOCKS
		      &AUX CONN PKT STR TEM RQB BUF (QUANTUM 17.)
			   PART-BASE PART-SIZE PART-COMMENT (N-HUNDRED 0))
 (UNWIND-PROTECT (PROGN
  (MULTIPLE-VALUE (PART-BASE PART-SIZE) (SYS:FIND-DISK-PARTITION TO-PART))
  (OR PART-BASE
      (RETURN (FORMAT NIL "No /"~A/" partition here." TO-PART)))
  (SETQ QUANTUM (GCD PART-SIZE QUANTUM))
  (SETQ CONN (CHAOS:CONNECT FROM-MACHINE
			    (FORMAT NIL "BAND-TRANSFER READ ~A ~D"
				        FROM-PART (AND SUBSET-N-BLOCKS
						       (LIST SUBSET-START SUBSET-N-BLOCKS)))
			    25.))
  (AND (STRINGP CONN) (RETURN CONN))	;Error message
  ;; Receive packet containing size and comment
  (SETQ PKT (CHAOS:GET-NEXT-PKT CONN)
	STR (CHAOS:PKT-STRING PKT))
  (SETQ TEM (LET ((IBASE 10.)) (READ-FROM-STRING STR)))
  (OR ( TEM PART-SIZE) (RETURN "Does not fit in local partition"))
  (SETQ PART-SIZE TEM)
  (SETQ TEM (STRING-SEARCH-CHAR #\SP STR))
  (SETQ PART-COMMENT (READ-FROM-STRING STR NIL (1+ TEM)))
  (FORMAT T "~&Receiving ~A from ~A: ~D blocks, ~A~%"
	    FROM-PART FROM-MACHINE PART-SIZE PART-COMMENT)
  (CHAOS:RETURN-PKT PKT)
  (AND SUBSET-N-BLOCKS (SETQ PART-BASE (+ PART-BASE SUBSET-START)
			     PART-SIZE SUBSET-N-BLOCKS
			     QUANTUM (GCD PART-SIZE QUANTUM)))
  (SETQ RQB (SYS:GET-DISK-RQB QUANTUM)
	BUF (SYS:RQB-BUFFER RQB))
  (SI:WIRE-DISK-RQB RQB)
  (DO BLOCK PART-BASE (+ BLOCK QUANTUM) ( BLOCK (+ PART-BASE PART-SIZE))
    (AND ( (SETQ TEM (// (- BLOCK PART-BASE) 100.)) N-HUNDRED)
	 (FORMAT T "~D " (SETQ N-HUNDRED TEM)))
    (ARRAY-FROM-NET BUF CONN)
    (SI:DISK-WRITE-WIRED RQB 0 BLOCK))
  (CHAOS:CLOSE CONN "Done")
  (OR SUBSET-N-BLOCKS (SI:UPDATE-PARTITION-COMMENT TO-PART PART-COMMENT 0)))
 (AND RQB (SYS:RETURN-DISK-RQB RQB))
 (AND CONN (NOT (STRINGP CONN)) (CHAOS:REMOVE-CONN CONN))))

(DEFUNP COMPARE-BAND (FROM-MACHINE FROM-PART TO-PART	
		      &OPTIONAL SUBSET-START SUBSET-N-BLOCKS
		      &AUX CONN PKT STR TEM RQB BUF BUF1 (QUANTUM 17.)
			   PART-BASE PART-SIZE PART-COMMENT (N-HUNDRED 0))
 (UNWIND-PROTECT (PROGN
  (MULTIPLE-VALUE (PART-BASE PART-SIZE) (SYS:FIND-DISK-PARTITION TO-PART))
  (OR PART-BASE
      (RETURN (FORMAT NIL "No /"~A/" partition here." TO-PART)))
  (SETQ CONN (CHAOS:CONNECT FROM-MACHINE
			    (FORMAT NIL "BAND-TRANSFER READ ~A ~D"
				        FROM-PART (AND SUBSET-N-BLOCKS
						       (LIST SUBSET-START SUBSET-N-BLOCKS)))
			    25.))
  (AND (STRINGP CONN) (RETURN CONN))	;Error message
  ;; Receive packet containing size and comment
  (SETQ PKT (CHAOS:GET-NEXT-PKT CONN)
	STR (CHAOS:PKT-STRING PKT))
  (SETQ TEM (LET ((IBASE 10.)) (READ-FROM-STRING STR)))
  (OR ( TEM PART-SIZE) (RETURN "Does not fit in local partition"))
  (SETQ PART-SIZE TEM)
  (SETQ TEM (STRING-SEARCH-CHAR #\SP STR))
  (SETQ PART-COMMENT (READ-FROM-STRING STR NIL (1+ TEM)))
  (FORMAT T "~&Comparing ~A with ~A from ~A: ~D blocks, ~A~%"
	    TO-PART FROM-PART FROM-MACHINE PART-SIZE PART-COMMENT)
  (CHAOS:RETURN-PKT PKT)
  (AND SUBSET-N-BLOCKS (SETQ PART-BASE (+ PART-BASE SUBSET-START)
			     PART-SIZE SUBSET-N-BLOCKS))
  (SETQ QUANTUM (GCD PART-SIZE QUANTUM))
  (SETQ RQB (SYS:GET-DISK-RQB QUANTUM)
	BUF (SYS:RQB-BUFFER RQB)
	BUF1 (MAKE-ARRAY NIL 'ART-16B (ARRAY-LENGTH BUF)))
  (SI:WIRE-DISK-RQB RQB)
  (DO BLOCK PART-BASE (+ BLOCK QUANTUM) ( BLOCK (+ PART-BASE PART-SIZE))
    (AND ( (SETQ TEM (// (- BLOCK PART-BASE) 100.)) N-HUNDRED)
	 (FORMAT T "~D " (SETQ N-HUNDRED TEM)))
    (ARRAY-FROM-NET BUF CONN)
    (COND ((SI:DISK-READ-COMPARE-WIRED RQB 0 BLOCK)
	   (COPY-ARRAY-CONTENTS BUF BUF1)
	   (SI:DISK-READ-WIRED RQB 0 BLOCK)
	   (DO B BLOCK (1+ B) (= B (+ BLOCK QUANTUM))
	     (DO ((I (* (- B BLOCK) 1000) (1+ I))
		  (N (* (1+ (- B BLOCK)) 1000))
		  (NDIFFS 0))
		 ((= I N)
		  (OR (ZEROP NDIFFS)
		      (LET ((SPT (AREF SI:DISK-SECTORS-PER-TRACK-ARRAY 0))
			    (HPC (AREF SI:DISK-HEADS-PER-CYLINDER-ARRAY 0)))
			(FORMAT T "~&Block ~S (cyl ~O surf ~O sec ~O here) differs in ~D halfwords~%"
			          B (// B (* HPC SPT)) (// (\ B (* HPC SPT)) SPT)
				  (\ B SPT) NDIFFS))))
	       (OR (= (AREF BUF I) (AREF BUF1 I)) (SETQ NDIFFS (1+ NDIFFS))))))))
  (CHAOS:CLOSE CONN "Done"))
 (AND RQB (SYS:RETURN-DISK-RQB RQB))
 (AND CONN (NOT (STRINGP CONN)) (CHAOS:REMOVE-CONN CONN))))

(DEFUNP TRANSMIT-BAND (FROM-PART TO-MACHINE TO-PART
		       &OPTIONAL SUBSET-START SUBSET-N-BLOCKS
		       &AUX CONN TEM RQB BUF (QUANTUM 17.)
			    PART-BASE PART-SIZE PART-COMMENT (N-HUNDRED 0))
 (UNWIND-PROTECT (PROGN
  (MULTIPLE-VALUE (PART-BASE PART-SIZE) (SYS:FIND-DISK-PARTITION FROM-PART))
  (OR PART-BASE
      (RETURN (FORMAT NIL "No /"~A/" partition here." TO-PART)))
  (SETQ PART-SIZE (GET-PARTITION-SIZE FROM-PART)
	PART-COMMENT (SI:PARTITION-COMMENT FROM-PART 0))
  (SETQ QUANTUM (GCD PART-SIZE QUANTUM))
  (SETQ CONN (CHAOS:CONNECT TO-MACHINE
			    (FORMAT NIL "BAND-TRANSFER WRITE ~A ~D ~D ~S"
				        FROM-PART
					(AND SUBSET-N-BLOCKS
					     (LIST SUBSET-START SUBSET-N-BLOCKS))
					PART-SIZE PART-COMMENT)))
  (AND (STRINGP CONN) (RETURN CONN))	;Error message
  (FORMAT T "~&Transmitting ~A to ~A on ~A: ~D blocks, ~A~%"
	    FROM-PART TO-PART TO-MACHINE PART-SIZE PART-COMMENT)
  (AND SUBSET-N-BLOCKS (SETQ PART-BASE (+ PART-BASE SUBSET-START)
			     PART-SIZE SUBSET-N-BLOCKS))
  (SETQ RQB (SYS:GET-DISK-RQB QUANTUM)
	BUF (SYS:RQB-BUFFER RQB))
  (SI:WIRE-DISK-RQB RQB)
  (DO BLOCK PART-BASE (+ BLOCK QUANTUM) ( BLOCK (+ PART-BASE PART-SIZE))
    (AND ( (SETQ TEM (// (- BLOCK PART-BASE) 100.)) N-HUNDRED)
	 (FORMAT T "~D " (SETQ N-HUNDRED TEM)))
    (SI:DISK-READ-WIRED RQB 0 BLOCK)
    (ARRAY-TO-NET BUF CONN))
  (CHAOS:FINISH CONN)
  (CHAOS:CLOSE CONN "Done"))
 (AND RQB (SYS:RETURN-DISK-RQB RQB))
 (AND CONN (NOT (STRINGP CONN)) (CHAOS:REMOVE-CONN CONN))))

(ADD-INITIALIZATION "BAND-TRANSFER"
		    '(PROCESS-RUN-FUNCTION "Band-Transfer" 'BAND-TRANSFER-SERVER)
		    NIL 'CHAOS:SERVER-ALIST)
