;;; -*- Mode: LISP; Package: SYSTEM-INTERNALS; Base:8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; "User" Disk I/O routines for Lisp machine -- macrocode portion
;;; See QCOM for documentation on disk rqb's and symbol definitions.
;;; The label-editor and related routines have been moved out of here into DLEDIT

;;;*** Errors should be metered, do this after the microcode is revised ***

;;; The following routines are likely to be of general interest:
;;; POWER-UP-DISK - makes sure a drive is powered up
;;; CLEAR-DISK-FAULT - attempts to reset select-lock (fault, unsafe, device-check)
;;; SET-CURRENT-MICROLOAD - choose microload to be booted from  [this is in the DLEDIT file]
;;; SET-CURRENT-BAND - choose world load to be booted from  [this is in the DLEDIT file]
;;; PRINT-DISK-LABEL - print the pack label of a drive  [this is in the DLEDIT file]
;;; PRINT-LOADED-BAND - print information about what system is running
;;; PAGE-{IN,OUT}-{STRUCTURE,WORDS,REGION,AREA,ARRAY} - fast paging: multiple pages at a time
;;; EDIT-DISK-LABEL - edit the pack label of a drive  [this is in the DLEDIT file]
;;; RECEIVE-PARTITION - receive a disk partition over the Chaosnet
;;; TRANSMIT-PARTITION - transmit a disk partition over the Chaosnet
;;; COMPARE-RECEIVED-PARTITION - like receive, but compare to existing contents of partition.
;;; LOAD-MCR-FILE - load microcode from the file-computer onto the disk
;;; COPY-DISK-PARTITION - copy a partition from disk to disk
;;; COPY-DISK-PARTITION-BACKGROUND - same but in separate process and artificially slowed down
;;; COMPARE-DISK-PARTITION - similar to copy, but compares and prints differences.
;;; MEASURED-SIZE-OF-PARTITION - Returns how much of LOD is actually used.

;;; These are interesting if you really want to do I/O
;;; GET-DISK-RQB
;;; RETURN-DISK-RQB
;;; PRINT-RQB
;;; DISK-READ
;;; DISK-WRITE

(DEFVAR DISK-BUFFER-AREA)	;Area containing wirable buffers and RQBs

;Vector of free disk RQB threads, indexed by RQB size in pages
(DEFVAR DISK-FREE-RQB-THREAD-VECTOR (MAKE-ARRAY (- PAGE-SIZE 14)))

(DEFVAR PAGE-OFFSET)		;Disk address of start of PAGE partition
				; The microcode knows this, but it's not telling
(DEFVAR VIRTUAL-MEMORY-SIZE)	;Size of paging partition in words

(DEFVAR CURRENT-LOADED-BAND)	;Remembers %LOADED-BAND through warm-booting
(DEFVAR DISK-PACK-NAME)		;Remembers name of pack for PRINT-LOADED-BAND

(DEFVAR CC-REMOTE-DISK-WRITE-CHECK NIL)  ;CC remote disk handler does read after write
(DEFVAR DISK-SHOULD-READ-COMPARE NIL) ;Unfortunately there is a hardware bug with
				      ;read compares on transfers longer than 1 block.
				      ;This didn't find any problems while it was on anyway.
				      ;(Fixed by DC ECO#1)
(DEFVAR DISK-ERROR-RETRY-COUNT 5)	;Retry this many times before CERRORing
(DEFVAR LET-MICROCODE-HANDLE-DISK-ERRORS T)  ;Better be UCADR 795 or later for this to work

(DEFSUBST RQB-BUFFER (RQB) (ARRAY-LEADER RQB %DISK-RQ-LEADER-BUFFER))
(DEFSUBST RQB-8-BIT-BUFFER (RQB) (ARRAY-LEADER RQB %DISK-RQ-LEADER-8-BIT-BUFFER))
(DEFSUBST RQB-NPAGES (RQB) (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES))

;;; The next three routines are the simple versions intended to be called
;;; by moderately naive people (i.e. clowns).
;;; Note that if UNIT is not a number, it is a function called to
;;; perform the operation.  This allows the console program to
;;; access other machines' disks in a compatible fashion.

(DEFUN DISK-READ (RQB UNIT ADDRESS
		  &OPTIONAL (MICROCODE-ERROR-RECOVERY LET-MICROCODE-HANDLE-DISK-ERRORS))
  (COND ((NUMBERP UNIT)
	 (WIRE-DISK-RQB RQB (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES) T T) ;set modified bits
	 (DISK-READ-WIRED RQB UNIT ADDRESS MICROCODE-ERROR-RECOVERY)
	 (UNWIRE-DISK-RQB RQB)
	 RQB)
	((FUNCALL UNIT ':READ RQB ADDRESS))))

(DEFUN DISK-WRITE (RQB UNIT ADDRESS
		  &OPTIONAL (MICROCODE-ERROR-RECOVERY LET-MICROCODE-HANDLE-DISK-ERRORS))
  (COND ((NUMBERP UNIT)
	 (WIRE-DISK-RQB RQB)
	 (DISK-WRITE-WIRED RQB UNIT ADDRESS MICROCODE-ERROR-RECOVERY)
	 (UNWIRE-DISK-RQB RQB)
	 RQB)
	((FUNCALL UNIT ':WRITE RQB ADDRESS))))

;A hardware bug causes this to lose if xfer > 1 page  (Fixed by DC ECO#1)
;Returns T if they match and NIL if they don't
(DEFUN DISK-READ-COMPARE (RQB UNIT ADDRESS
		  &OPTIONAL (MICROCODE-ERROR-RECOVERY LET-MICROCODE-HANDLE-DISK-ERRORS))
  (COND ((NUMBERP UNIT)
	 (WIRE-DISK-RQB RQB)
	 (DISK-READ-COMPARE-WIRED RQB UNIT ADDRESS MICROCODE-ERROR-RECOVERY)
	 (UNWIRE-DISK-RQB RQB)
	 (ZEROP (LDB %%DISK-STATUS-HIGH-READ-COMPARE-DIFFERENCE
		     (AREF RQB %DISK-RQ-STATUS-HIGH))))
	((FUNCALL UNIT ':READ-COMPARE RQB ADDRESS))))

;;; Get STATUS of a unit by doing OFFSET-CLEAR (nebbish command) to it
;;; Leaves the status in the rqb
(DEFUN GET-DISK-STATUS (RQB UNIT)
  (DISK-RUN RQB UNIT 0 1 1 %DISK-COMMAND-OFFSET-CLEAR "Offset Clear" T))

;;; Power up a drive, return T if successful, NIL if timed out
(DEFUN POWER-UP-DISK (UNIT &AUX RQB)
  (UNWIND-PROTECT
     (PROGN (WITHOUT-INTERRUPTS
	     (SETQ RQB (GET-DISK-RQB)))
	    (DO ((START-TIME (TIME)))
		((OR (> (TIME-DIFFERENCE (TIME) START-TIME) (* 30. 60.))
		     (NOT (LDB-TEST %%DISK-STATUS-LOW-OFF-CYLINDER
				    (PROGN (GET-DISK-STATUS RQB UNIT)
					   (AREF RQB %DISK-RQ-STATUS-LOW)))))
		 (NOT (LDB-TEST %%DISK-STATUS-LOW-OFF-CYLINDER
				(AREF RQB %DISK-RQ-STATUS-LOW))))
	      (PROCESS-SLEEP 60.)))
     (RETURN-DISK-RQB RQB)))

(DEFUN CLEAR-DISK-FAULT (UNIT &AUX RQB)
  (UNWIND-PROTECT
     (PROGN (WITHOUT-INTERRUPTS
	     (SETQ RQB (GET-DISK-RQB)))
	    (DISK-RUN RQB UNIT 0 1 1 %DISK-COMMAND-FAULT-CLEAR "Fault Clear" T))
     (RETURN-DISK-RQB RQB)))

;A debugging function
(DEFUN PRINT-RQB (RQB)
  (DO ((I 0 (1+ I))
       (L DISK-RQ-HWDS (CDR L)))
      ((NULL (CDR L))
       (PRINT (CAR L)) ;CCW list
       (DO I I (+ I 2) NIL
	 (FORMAT T "	~O" (DPB (AREF RQB (1+ I)) 2020 (AREF RQB I)))
	 (OR (BIT-TEST 1 (AREF RQB I)) (RETURN NIL)))
       (TERPRI))
    (FORMAT T "~%~S  ~O" (CAR L) (AREF RQB I))))

;Move all existing rqbs from the old freelist to the new one.
(DEFUN XFER-FREE-RQBS ()
  (LOCAL-DECLARE ((SPECIAL DISK-FIRST-FREE-RQB))
    (DO (NEXT) ((NULL DISK-FIRST-FREE-RQB))
      (SETQ NEXT (ARRAY-LEADER DISK-FIRST-FREE-RQB %DISK-RQ-LEADER-THREAD))
      (SETF (ARRAY-LEADER DISK-FIRST-FREE-RQB %DISK-RQ-LEADER-THREAD) NIL)
      (RETURN-DISK-RQB DISK-FIRST-FREE-RQB)
      (SETQ DISK-FIRST-FREE-RQB NEXT))))

(DEFUN GET-DISK-RQB (&OPTIONAL (N-PAGES 1) (LEADER-LENGTH (LENGTH DISK-RQ-LEADER-QS))
		     &AUX OVERHEAD ARRAY-LENGTH)
  ;; Create static area to contain disk buffer.  We assume everything
  ;; allocated in here is on a page boundary
  (OR (BOUNDP 'DISK-BUFFER-AREA)
      (MAKE-AREA ':NAME 'DISK-BUFFER-AREA
		 ':GC ':STATIC))
  ;; Compute how much overhead there is in the RQB-BUFFER, RQB-8-BIT-BUFFER, and in the
  ;; RQB's leader and header.  4 for the RQB-BUFFER indirect-offset array,
  ;; 4 for the RQB-8-BIT-BUFFER indirect-offset array,
  ;; 3 for the RQB's header, plus the RQB's leader.  Then set the length
  ;; (in halfwords) of the array to be sufficient so that it plus the overhead
  ;; is a multiple of the page size, making it possible to wire down RQB's.
  (SETQ OVERHEAD (+ 4 4 3 LEADER-LENGTH)
	ARRAY-LENGTH (* (- (* (1+ N-PAGES) PAGE-SIZE) OVERHEAD) 2))
  (COND ((> ARRAY-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
	 (SETQ OVERHEAD (1+ OVERHEAD) ARRAY-LENGTH (- ARRAY-LENGTH 2))
	 (OR (> ARRAY-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
	     (FERROR NIL "Impossible to make this RQB array fit"))))
  ;; See if the CCW list will run off the end of the first page, and hence
  ;; not be stored in consecutive physical addresses.
  (IF (> (+ OVERHEAD (// %DISK-RQ-CCW-LIST 2) N-PAGES) PAGE-SIZE)
      (FERROR NIL "CCW list doesn't fit on first RQB page, ~S pages is too many" N-PAGES))
  ;; See if there is already a suitable free RQB.  If not, make one.
  (COND ((DO ((INHIBIT-SCHEDULING-FLAG T)
	      (B (AREF DISK-FREE-RQB-THREAD-VECTOR N-PAGES)
		 (ARRAY-LEADER B %DISK-RQ-LEADER-THREAD))
	      (PREV-LOC (LOCF (AREF DISK-FREE-RQB-THREAD-VECTOR N-PAGES))
			(LOCF (ARRAY-LEADER B %DISK-RQ-LEADER-THREAD))))
	     ((NULL B))
	   (AND (= (ARRAY-LENGTH B) ARRAY-LENGTH)
		(= (ARRAY-LEADER-LENGTH B) LEADER-LENGTH)
		(RETURN (PROGN (RPLACD PREV-LOC (ARRAY-LEADER B %DISK-RQ-LEADER-THREAD))
			       (SETF (ARRAY-LEADER B %DISK-RQ-LEADER-THREAD) NIL)
			       B)))))
	(T 
	 (PROG (RQB-BUFFER RQB RQB-8-BIT-BUFFER)
	   L  (SETQ RQB-BUFFER (MAKE-ARRAY (* PAGE-SIZE 2 N-PAGES)
				       ':TYPE ART-16B
				       ':AREA DISK-BUFFER-AREA
				       ':DISPLACED-TO ""
				       ':DISPLACED-INDEX-OFFSET    ;To second page of RQB
				           (- ARRAY-LENGTH (* PAGE-SIZE 2 N-PAGES)))
		    RQB-8-BIT-BUFFER (MAKE-ARRAY (* PAGE-SIZE 4 N-PAGES)
						 ':TYPE ART-STRING
						 ':AREA DISK-BUFFER-AREA
						 ':DISPLACED-TO ""
						 ':DISPLACED-INDEX-OFFSET
						 (* 2 (- ARRAY-LENGTH
							 (* PAGE-SIZE 2 N-PAGES)))))
	      (SETQ RQB (MAKE-ARRAY ARRAY-LENGTH
				':AREA DISK-BUFFER-AREA
				':TYPE ART-16B
				':LEADER-LENGTH LEADER-LENGTH))
	      (COND ((NOT (= (%REGION-NUMBER RQB-BUFFER)   ;make sure a new region
			     (%REGION-NUMBER RQB)))	   ;didnt screw us completely
		 ;Screwwed! Try again.  Make sure don't lose same way again by
		 ; using up region that didnt hold it.
		     (RETURN-ARRAY RQB)
		     (LET ((RN (%REGION-NUMBER RQB-BUFFER)))
		       (RETURN-ARRAY RQB-BUFFER)
		       (%USE-UP-REGION RN))
		     (GO L)))
	      (MAKE-SURE-FREE-POINTER-OF-REGION-IS-AT-PAGE-BOUNDARY
		'DISK-BUFFER-AREA (%REGION-NUMBER RQB))
	      (%P-STORE-CONTENTS-OFFSET RQB RQB-BUFFER 1)	;Displace RQB-BUFFER to RQB
	      (%P-STORE-CONTENTS-OFFSET RQB RQB-8-BIT-BUFFER 1)
	      (STORE-ARRAY-LEADER (+ %DISK-RQ-CCW-LIST (* 2 N-PAGES))
				  RQB
				  %DISK-RQ-LEADER-N-HWDS)
	      (STORE-ARRAY-LEADER N-PAGES RQB %DISK-RQ-LEADER-N-PAGES)
	      (STORE-ARRAY-LEADER RQB-BUFFER RQB %DISK-RQ-LEADER-BUFFER)
	      (STORE-ARRAY-LEADER RQB-8-BIT-BUFFER RQB %DISK-RQ-LEADER-8-BIT-BUFFER)
	      (RETURN RQB)))))

;Use this to recover if the free pointer is off a page boundary.
(DEFUN %USE-UP-REGION (REGION-NUMBER)
  (AS-1 (AR-1 #'SYSTEM:REGION-FREE-POINTER REGION-NUMBER)
	#'SYSTEM:REGION-LENGTH
	REGION-NUMBER))

;This used to check all the regions of the area, but that loses,
;because exhausted regions always have free pointers 10 past the page boundary
;(because the rqb-buffer and rqb-8-bit-buffer had been consed before exhausting it).
(DEFUN MAKE-SURE-FREE-POINTER-OF-REGION-IS-AT-PAGE-BOUNDARY (AREA REGION-NUMBER)
  (OR (ZEROP (LOGAND (1- PAGE-SIZE) (REGION-FREE-POINTER REGION-NUMBER)))
      (FERROR NIL
	      "~%Area ~A(#~O), region ~O has free pointer ~O, which is not on a page boundary"
	      AREA (SYMEVAL AREA) REGION-NUMBER (REGION-FREE-POINTER REGION-NUMBER))))

;; Return a buffer to the free list
(DEFUN RETURN-DISK-RQB (RQB &AUX (INHIBIT-SCHEDULING-FLAG T))
  (COND ((NOT (NULL RQB))	;Allow NIL's to be handed to the function just in case
	 (UNWIRE-DISK-RQB RQB)
	 (LET ((N-PAGES (// (ARRAY-LENGTH (RQB-BUFFER RQB)) 2 PAGE-SIZE)))
	   (DO ((B (AREF DISK-FREE-RQB-THREAD-VECTOR N-PAGES)
		   (ARRAY-LEADER B %DISK-RQ-LEADER-THREAD)))
	       ((NULL B)
		(SETF (ARRAY-LEADER RQB %DISK-RQ-LEADER-THREAD)
		      (AREF DISK-FREE-RQB-THREAD-VECTOR N-PAGES))
		(SETF (AREF DISK-FREE-RQB-THREAD-VECTOR N-PAGES) RQB))
	     (AND (EQ B RQB) (RETURN))))))	;Don't get it on the list twice
  NIL)

(DEFUN COUNT-FREE-RQBS (N-PAGES)
  (DO ((B (AREF DISK-FREE-RQB-THREAD-VECTOR N-PAGES) (ARRAY-LEADER B %DISK-RQ-LEADER-THREAD))
       (I 0 (1+ I)))
      ((NULL B) I)))

;; Return a buffer to the free list, without error checking.
(DEFUN RISKY-RETURN-DISK-RQB (RQB &AUX (INHIBIT-SCHEDULING-FLAG T))
  (COND ((NOT (NULL RQB))	;Allow NIL's to be handed to the function just in case
	 (UNWIRE-DISK-RQB RQB)
	 (LET ((N-PAGES (// (ARRAY-LENGTH (RQB-BUFFER RQB)) 2 PAGE-SIZE)))
	   (SETF (ARRAY-LEADER RQB %DISK-RQ-LEADER-THREAD)
		 (AREF DISK-FREE-RQB-THREAD-VECTOR N-PAGES))
	   (SETF (AREF DISK-FREE-RQB-THREAD-VECTOR N-PAGES) RQB))))
  NIL)

;; Set up ccw list, wire down pages
(DEFUN WIRE-DISK-RQB (RQB &OPTIONAL (N-PAGES (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES))
				       (WIRE-P T) SET-MODIFIED
			 &AUX (LONG-ARRAY-FLAG (%P-LDB %%ARRAY-LONG-LENGTH-FLAG RQB))
			      (LOW (- (%POINTER RQB) (ARRAY-DIMENSION-N 0 RQB) 2))
			      (HIGH (+ (%POINTER RQB) 1 LONG-ARRAY-FLAG
				       (// (ARRAY-LENGTH RQB) 2))))
  (DO LOC (LOGAND LOW (- PAGE-SIZE)) (+ LOC PAGE-SIZE) (>= LOC HIGH)
    (WIRE-PAGE LOC WIRE-P SET-MODIFIED))
  ;; Having wired the rqb, if really wiring set up CCW-list N-PAGES long
  ;; and CLP to it, but if really unwiring make CLP point to NXM as err check
  (COND ((NOT WIRE-P) (ASET 177777 RQB %DISK-RQ-CCW-LIST-POINTER-LOW) ;Just below TV buffer
		      (ASET 76 RQB %DISK-RQ-CCW-LIST-POINTER-HIGH))
	(T (DO ((CCWX 0 (1+ CCWX))
		(VADR (+ LOW PAGE-SIZE) (+ VADR PAGE-SIZE)) ;Start with 2nd page of rqb array
		(PADR))
	       ((>= CCWX N-PAGES)	;Done, set END in last CCW
		(SETQ PADR (%PHYSICAL-ADDRESS (+ (%POINTER RQB)
						 1
						 LONG-ARRAY-FLAG
						 (// %DISK-RQ-CCW-LIST 2))))
		(ASET PADR RQB %DISK-RQ-CCW-LIST-POINTER-LOW)
		(ASET (LSH PADR -16.) RQB %DISK-RQ-CCW-LIST-POINTER-HIGH))
	     (SETQ PADR (%PHYSICAL-ADDRESS VADR))
	     (ASET (+ (LOGAND (- PAGE-SIZE) PADR)	;Low 16 bits
		      (COND ((= CCWX (1- N-PAGES)) 0) (T 1))) ;Chain bit
		   RQB (+ %DISK-RQ-CCW-LIST (* 2 CCWX)))
	     (ASET (LSH PADR -16.)		;High 6 bits
		   RQB (+ %DISK-RQ-CCW-LIST 1 (* 2 CCWX)))))))

(DEFUN UNWIRE-DISK-RQB (RQB)
  (WIRE-DISK-RQB RQB NIL NIL))

;;; Disk geometry is remembered in the following arrays.
;;; The DISK-READ-WIRED function contains a kludge that it notices if you
;;; are reading the label, and automatically adjusts the geometry
;;; for that unit from the label.  You can also store explicitly
;;; in the arrays if you like.

(DEFVAR DISK-SECTORS-PER-TRACK-ARRAY)
(DEFVAR DISK-HEADS-PER-CYLINDER-ARRAY)

(COND ((NOT (BOUNDP 'DISK-SECTORS-PER-TRACK-ARRAY))
       (SETQ DISK-SECTORS-PER-TRACK-ARRAY (MAKE-ARRAY NIL 'ART-8B 20)
	     DISK-HEADS-PER-CYLINDER-ARRAY (MAKE-ARRAY NIL 'ART-8B 20))
       (FILLARRAY DISK-SECTORS-PER-TRACK-ARRAY '(17.))
       (FILLARRAY DISK-HEADS-PER-CYLINDER-ARRAY '(5))))

;These must be called with the buffer already wired, which specifies the
;number of pages implicitly (usually 1 of course)
;For now, error-handling is rudimentary, fix later
;Note!! If you call this directly, you better make sure the modified bits for the
; pages transferred get set!!!
(DEFUN DISK-READ-WIRED (RQB UNIT ADDRESS
			&OPTIONAL (MICROCODE-ERROR-RECOVERY LET-MICROCODE-HANDLE-DISK-ERRORS)
			&AUX (SECTORS-PER-TRACK (AREF DISK-SECTORS-PER-TRACK-ARRAY UNIT))
			     (HEADS-PER-CYLINDER (AREF DISK-HEADS-PER-CYLINDER-ARRAY UNIT)))
  
  (DISK-RUN RQB UNIT ADDRESS SECTORS-PER-TRACK HEADS-PER-CYLINDER
	    (LOGIOR %DISK-COMMAND-READ
		    (IF MICROCODE-ERROR-RECOVERY %DISK-COMMAND-DONE-INTERRUPT-ENABLE 0))
	    "read")
  (LET ((BFR (ARRAY-LEADER RQB %DISK-RQ-LEADER-BUFFER)))
    (COND ((AND (ZEROP ADDRESS)
		(= (AREF BFR 0) (+ (LSH #/A 8) #/L))
		(= (AREF BFR 1) (+ (LSH #/L 8) #/B)))
	   (ASET (AREF BFR 6) DISK-HEADS-PER-CYLINDER-ARRAY UNIT)
	   (ASET (AREF BFR 10) DISK-SECTORS-PER-TRACK-ARRAY UNIT))))
  NIL)

(DEFUN DISK-WRITE-WIRED (RQB UNIT ADDRESS
		   &OPTIONAL (MICROCODE-ERROR-RECOVERY LET-MICROCODE-HANDLE-DISK-ERRORS)
		   &AUX (SECTORS-PER-TRACK (AREF DISK-SECTORS-PER-TRACK-ARRAY UNIT))
			(HEADS-PER-CYLINDER (AREF DISK-HEADS-PER-CYLINDER-ARRAY UNIT)))
  (DISK-RUN RQB UNIT ADDRESS SECTORS-PER-TRACK HEADS-PER-CYLINDER
	    (LOGIOR %DISK-COMMAND-WRITE
		    (IF MICROCODE-ERROR-RECOVERY %DISK-COMMAND-DONE-INTERRUPT-ENABLE 0))
	    "write"))


;A hardware bug causes this to lose if xfer > 1 page  (Fixed by DC ECO#1)
;Returns T if read-compare difference detected
(DEFUN DISK-READ-COMPARE-WIRED (RQB UNIT ADDRESS
		   &OPTIONAL (MICROCODE-ERROR-RECOVERY LET-MICROCODE-HANDLE-DISK-ERRORS)
		   &AUX (SECTORS-PER-TRACK (AREF DISK-SECTORS-PER-TRACK-ARRAY UNIT))
			(HEADS-PER-CYLINDER (AREF DISK-HEADS-PER-CYLINDER-ARRAY UNIT)))
  (DISK-RUN RQB UNIT ADDRESS SECTORS-PER-TRACK HEADS-PER-CYLINDER
	    (LOGIOR %DISK-COMMAND-READ-COMPARE
		    (IF MICROCODE-ERROR-RECOVERY %DISK-COMMAND-DONE-INTERRUPT-ENABLE 0))
	    "read-compare")
  (LDB-TEST %%DISK-STATUS-HIGH-READ-COMPARE-DIFFERENCE
	    (AREF RQB %DISK-RQ-STATUS-HIGH)))

(DEFUN DISK-RUN (RQB UNIT ADDRESS SECTORS-PER-TRACK HEADS-PER-CYLINDER CMD CMD-NAME
		 &OPTIONAL NO-ERROR-CHECKING
		 &AUX ADR CYLINDER SURFACE SECTOR ERROR-COUNT ER-H ER-L)
  (PROG (FINAL-ADDRESS FINAL-CYLINDER FINAL-SURFACE FINAL-SECTOR MICROCODE-ERROR-RECOVERY)
    (SETQ FINAL-ADDRESS (+ ADDRESS (1- (DISK-TRANSFER-SIZE RQB)))   ;count length of CCW.
	  FINAL-SECTOR (\ FINAL-ADDRESS SECTORS-PER-TRACK)
	  ADR (// FINAL-ADDRESS SECTORS-PER-TRACK)
	  FINAL-SURFACE (\ ADR HEADS-PER-CYLINDER)
	  FINAL-CYLINDER (// ADR  HEADS-PER-CYLINDER)
	  MICROCODE-ERROR-RECOVERY (BIT-TEST %DISK-COMMAND-DONE-INTERRUPT-ENABLE CMD))
    ;; Temporary error check
    (AND MICROCODE-ERROR-RECOVERY
	 (< %MICROCODE-VERSION-NUMBER 795.)
	 (SETQ MICROCODE-ERROR-RECOVERY NIL
	       CMD (LOGXOR %DISK-COMMAND-DONE-INTERRUPT-ENABLE CMD)))
 FULL-RETRY
    (SETQ ERROR-COUNT DISK-ERROR-RETRY-COUNT)
 PARTIAL-RETRY
    (SETQ SECTOR (\ ADDRESS SECTORS-PER-TRACK) ADR (// ADDRESS SECTORS-PER-TRACK)
	  SURFACE (\ ADR HEADS-PER-CYLINDER)
	  CYLINDER (// ADR  HEADS-PER-CYLINDER))
    (ASET CMD RQB %DISK-RQ-COMMAND)
    (ASET (+ (LSH SURFACE 8) SECTOR) RQB %DISK-RQ-SURFACE-SECTOR)
    (ASET (+ (LSH UNIT 12.) CYLINDER) RQB %DISK-RQ-UNIT-CYLINDER)
    (AS-1 0 RQB %DISK-RQ-FINAL-UNIT-CYLINDER)
    (AS-1 0 RQB %DISK-RQ-FINAL-SURFACE-SECTOR)
    (DISK-RUN-1 RQB UNIT)
    (AND NO-ERROR-CHECKING (RETURN NIL))
    (SETQ ER-H (AREF RQB %DISK-RQ-STATUS-HIGH)
	  ER-L (AREF RQB %DISK-RQ-STATUS-LOW))
    (AND (= CMD %DISK-COMMAND-READ-COMPARE)
	 (LDB-TEST %%DISK-STATUS-HIGH-READ-COMPARE-DIFFERENCE ER-H)
	 (SETQ ER-H (DPB 0 %%DISK-STATUS-HIGH-INTERNAL-PARITY ER-H)))
    (COND ((OR (BIT-TEST %DISK-STATUS-HIGH-ERROR ER-H)
	       (BIT-TEST %DISK-STATUS-LOW-ERROR ER-L))
	   (OR (ZEROP (SETQ ERROR-COUNT (1- ERROR-COUNT)))
	       MICROCODE-ERROR-RECOVERY
	       (GO PARTIAL-RETRY))
	   (CERROR 0 NIL ':DISK-ERROR
 "Disk ~A error unit ~D, cyl ~D., surf ~D., sec ~D.,~%  status ~A
 Type control-C to retry."
		   CMD-NAME UNIT
		   (LDB 0014 (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER))
		   (LDB 1010 (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
		   (LDB 0010 (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
		   (DECODE-DISK-STATUS (AREF RQB %DISK-RQ-STATUS-LOW)
				       (AREF RQB %DISK-RQ-STATUS-HIGH)))
	   (GO FULL-RETRY))
	  ((OR ( FINAL-CYLINDER (LDB 0014 (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER)))
	       ( FINAL-SURFACE (LDB 1010 (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR)))
	       ( FINAL-SECTOR (LDB 0010 (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))))
	   (CERROR 0 NIL ':DISK-ERROR
 "Disk ~A error unit ~D, cyl ~D., surf ~D., sec ~D.,~%  status ~A
 Failed to complete operation, final disk address should be ~D, ~D, ~D.
 Type control-C to retry."
		   CMD-NAME UNIT
		   (LDB 0014 (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER))
		   (LDB 1010 (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
		   (LDB 0010 (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
		   (DECODE-DISK-STATUS (AREF RQB %DISK-RQ-STATUS-LOW)
				       (AREF RQB %DISK-RQ-STATUS-HIGH))
		   FINAL-CYLINDER FINAL-SURFACE FINAL-SECTOR)
	   (GO FULL-RETRY))
	  ((AND DISK-SHOULD-READ-COMPARE
		(OR (= CMD %DISK-COMMAND-READ) (= CMD %DISK-COMMAND-WRITE)))
	   (ASET %DISK-COMMAND-READ-COMPARE RQB %DISK-RQ-COMMAND)
	   (DISK-RUN-1 RQB UNIT)
	   (COND ((OR (BIT-TEST %DISK-STATUS-HIGH-ERROR (AREF RQB %DISK-RQ-STATUS-HIGH))
		      (BIT-TEST %DISK-STATUS-LOW-ERROR (AREF RQB %DISK-RQ-STATUS-LOW)))
		  (OR (ZEROP (SETQ ERROR-COUNT (1- ERROR-COUNT)))
		      (GO PARTIAL-RETRY))
		  (CERROR 0 NIL ':DISK-ERROR
 "Disk error during read//compare after ~A unit ~D, cyl ~D., surf ~D., sec ~D.,~%  status ~A
 Type control-C to retry."
			  CMD-NAME UNIT
			  (LDB 0014 (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER))
			  (LDB 1010 (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
			  (LDB 0010 (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
			  (DECODE-DISK-STATUS (AREF RQB %DISK-RQ-STATUS-LOW)
					      (AREF RQB %DISK-RQ-STATUS-HIGH)))
		  (GO FULL-RETRY))
		 ((LDB-TEST %%DISK-STATUS-HIGH-READ-COMPARE-DIFFERENCE
			    (AREF RQB %DISK-RQ-STATUS-HIGH))
		  ;; A true read/compare difference really shouldn't happen, complain
		  (CERROR 0 NIL ':DISK-ERROR
			  "Disk read//compare error unit ~D, cyl ~D., surf ~D., sec ~D.
 Type control-C to retry."
			  UNIT
			  (LDB 0014 (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER))
			  (LDB 1010 (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
			  (LDB 0010 (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR)))
		  (GO FULL-RETRY)))))))

;This knows about a second disk controller, containing units 10-17,
;which is at an XBUS address 4 less than the address of the first controller.
(DEFUN DISK-RUN-1 (RQB UNIT)
  (COND ((< UNIT 8)
	 (%DISK-OP RQB)
	 (DO () ((NOT (ZEROP (AREF RQB %DISK-RQ-DONE-FLAG)))))) ;Loop until disk op complete
	(T ;; Await disk control ready
	   (DO () ((BIT-TEST 1 (%XBUS-READ 377770))))
	   ;; Write 4 words into disk control
	   (DO ((I %DISK-RQ-COMMAND (+ I 2))
		(N 4 (1- N))
		(A (+ (LSH 77 18.) 377770) (1+ A)))
	       ((ZEROP N))
	     (%P-STORE-TAG-AND-POINTER A (LDB 1010 (AREF RQB (1+ I)))
				         (%24-BIT-PLUS (LSH (AREF RQB (1+ I)) 16.)
						       (AREF RQB I))))
	   ;; Await disk control done
	   (DO () ((BIT-TEST 1 (%XBUS-READ 377770))))
	   ;; Read 4 words (8 halfwords) from disk control
	   (DO ((I %DISK-RQ-STATUS-LOW (+ I 2))
		(N 4 (1- N))
		(A (+ (LSH 77 18.) 377770) (1+ A)))
	       ((ZEROP N))
	     (ASET (%P-LDB 0020 A) RQB I)
	     (ASET (%P-LDB 2020 A) RQB (1+ I))))))

(DEFUN DISK-TRANSFER-SIZE (RQB)
  (DO ((CCWP %DISK-RQ-CCW-LIST (+ CCWP 2))
       (COUNT 1 (1+ COUNT)))
      ((ZEROP (LOGAND (AREF RQB CCWP) 1)) COUNT)))

(DEFUN (:DISK-ERROR EH:PROCEED) (IGNORE IGNORE)
  (FORMAT T "~&Retrying disk operation.~%"))

;Return a string representation of the disk status register (pair of halfwords)
;Put the most relevant error condition bit first, followed by all other 1 bits
;Except for Idle, Interrupt, Read Compare Difference (and block-counter) which
;are not interesting as errors.
;Also if the transfer is aborted, leave out Internal Parity which is always on.
(DEFUN DECODE-DISK-STATUS (LOW HIGH)
  (WITH-OUTPUT-TO-STRING (S)
    (LOOP FOR (NAME PPSS HALF) IN '(("Nonexistent-Memory" %%DISK-STATUS-HIGH-NXM T)
				    ("Memory-Parity" %%DISK-STATUS-HIGH-MEM-PARITY T)
				    ("Multiple-Select" %%DISK-STATUS-LOW-MULTIPLE-SELECT)
				    ("No-Select" %%DISK-STATUS-LOW-NO-SELECT)
				    ("Fault" %%DISK-STATUS-LOW-FAULT)
				    ("Off-line"  %%DISK-STATUS-LOW-OFF-LINE)
				    ("Off-Cylinder" %%DISK-STATUS-LOW-OFF-CYLINDER)
				    ("Seek-Error" %%DISK-STATUS-LOW-SEEK-ERROR)
				    ("Start-Block-Error" %%DISK-STATUS-LOW-START-BLOCK-ERROR)
				    ("Overrun" %%DISK-STATUS-LOW-OVERRUN)
				    ("Header-Compare" %%DISK-STATUS-HIGH-HEADER-COMPARE T)
				    ("Header-ECC" %%DISK-STATUS-HIGH-HEADER-ECC T)
				    ("ECC-Hard" %%DISK-STATUS-HIGH-ECC-HARD T)
				    ("ECC-Soft" %%DISK-STATUS-LOW-ECC-SOFT)
				    ("Timeout" %%DISK-STATUS-LOW-TIMEOUT)
				    ("Internal-Parity" %%DISK-STATUS-HIGH-INTERNAL-PARITY T)
				    ("Transfer-Aborted" %%DISK-STATUS-LOW-TRANSFER-ABORTED)
				    ("CCW-Cycle" %%DISK-STATUS-HIGH-CCW-CYCLE T)
				    ("Read-Only" %%DISK-STATUS-LOW-READ-ONLY)
				    ("Sel-Unit-Attention"
				        %%DISK-STATUS-LOW-SEL-UNIT-ATTENTION)
				    ("Any-Unit-Attention" %%DISK-STATUS-LOW-ATTENTION))
	  WITH FLAG = NIL
	  WHEN (AND (LDB-TEST (SYMEVAL PPSS) (IF (NULL HALF) LOW HIGH))
		    (OR (NEQ PPSS '%%DISK-STATUS-HIGH-INTERNAL-PARITY)
			(NOT (LDB-TEST %%DISK-STATUS-LOW-TRANSFER-ABORTED LOW))))
	    DO (IF FLAG (FUNCALL S ':STRING-OUT "  "))
	       (FUNCALL S ':STRING-OUT NAME)
	       (SETQ FLAG T))))

;;; Unit is unit number on local disk controller or a string.
;;; If a string, CC means hack over debug interface
;;;    		 TEST is a source of test data
;;;		 MT is magtape
;;;	  otherwise it is assumed to be the chaosnet name of a remote machine.
(DECLARE (SPECIAL REMOTE-DISK-CONN REMOTE-DISK-STREAM REMOTE-DISK-UNIT
		  CC-DISK-UNIT CC-DISK-INIT-P CADR:CC-DISK-LOWCORE CADR:CC-DISK-TYPE))

(DEFUN DECODE-UNIT-ARGUMENT (UNIT USE &OPTIONAL (CC-DISK-INIT-P NIL) (WRITE-P NIL)
			     &AUX TEM)
  (COND ((NUMBERP UNIT) UNIT)			;Local disk
	((AND (STRINGP UNIT) 
	      (STRING-EQUAL UNIT "CC" 0 0 2))
	 (SETQ TEM (STRING-SEARCH-CHAR #\SP UNIT))
	 (LET ((CC-DISK-UNIT (IF (NULL TEM) 0 (READ-FROM-STRING UNIT NIL (1+ TEM)))))
	   (COND ((NOT (ZEROP CC-DISK-UNIT))
		  (FERROR NIL "CC can only talk to unit zero")))
	   (COND ((NULL CC-DISK-INIT-P)
		  (CADR:CC-DISK-INIT)
		  (CADR:CC-DISK-WRITE 1 CADR:CC-DISK-LOWCORE 2)) ;Save on block 1,2
		 (T (SETQ CADR:CC-DISK-TYPE T)))   ;Dont try to read garbage label, etc.
	   (CLOSURE '(CC-DISK-UNIT CC-DISK-INIT-P)
		    'CC-DISK-HANDLER)))
	((AND (STRINGP UNIT)			;This makes test data.
	      (STRING-EQUAL UNIT "TEST" 0 0 4))
	 (SETQ TEM (STRING-SEARCH-CHAR #\SP UNIT))
	 (LET ((CC-DISK-UNIT (IF (NULL TEM) 0 (READ-FROM-STRING UNIT NIL (1+ TEM)))))
	   (CLOSURE '(CC-DISK-UNIT)
		    'CC-TEST-HANDLER)))
	((AND (STRINGP UNIT)			;Magtape interface.
	      (STRING-EQUAL UNIT "MT" 0 0 2))
	 (SETQ TEM (STRING-SEARCH-CHAR #\SP UNIT))
	 (LET ((CC-DISK-UNIT (IF (NULL TEM) 0 (READ-FROM-STRING UNIT NIL (1+ TEM)))))
	   (COND ((NOT (ZEROP CC-DISK-UNIT))
		  (FERROR NIL "MT can only talk to unit zero")))
	   (FS:MAKE-BAND-MAGTAPE-HANDLER WRITE-P)))	;Temporarily, this fctn in RG;MT.
	((STRINGP UNIT)				;Open connection to foreign disk
	 (LET ((REMOTE-DISK-CONN
		 (CHAOS:CONNECT (SUBSTRING UNIT 0 (SETQ TEM (STRING-SEARCH-CHAR #\SP UNIT)))
				"REMOTE-DISK" 25.))
	       (REMOTE-DISK-STREAM)
	       (REMOTE-DISK-UNIT (IF (NULL TEM) 0 (READ-FROM-STRING UNIT NIL (1+ TEM)))))
	   (AND (STRINGP REMOTE-DISK-CONN)
		(FERROR NIL "Cannot connect to ~S: ~A" UNIT REMOTE-DISK-CONN))
	   (SETQ REMOTE-DISK-STREAM (CHAOS:STREAM REMOTE-DISK-CONN))
	   (FORMAT REMOTE-DISK-STREAM "SAY Disk being hacked remotely by ~A@~A -- ~A~%"
		   USER-ID (SYMBOLIC-CHAOS-ADDRESS CHAOS:MY-ADDRESS) USE)
	   (FUNCALL REMOTE-DISK-STREAM ':FORCE-OUTPUT)
	   (CLOSURE '(REMOTE-DISK-CONN REMOTE-DISK-STREAM REMOTE-DISK-UNIT)
		    'REMOTE-DISK-HANDLER)))
	(T UNIT)))				;Probably remote disk at higher level

(DEFUN DISPOSE-OF-UNIT (UNIT)
  (OR (NUMBERP UNIT) (FUNCALL UNIT ':DISPOSE)))

;;; :READ-COMPARE not supported, nothing uses it.
(DEFUN REMOTE-DISK-HANDLER (OP &REST ARGS)
  (SELECTQ OP
    (:READ (LET ((RQB (CAR ARGS)))
	     (LET ((BLOCK (CADR ARGS))
		   (N-BLOCKS (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES)))
	       (FORMAT REMOTE-DISK-STREAM "READ ~D ~D ~D~%" REMOTE-DISK-UNIT BLOCK N-BLOCKS)
	       (FUNCALL REMOTE-DISK-STREAM ':FORCE-OUTPUT)
	       (DO ((BLOCK BLOCK (1+ BLOCK))
		    (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
		    (BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484. T))
		    (BLOCK-PKT-2 (GET-DISK-STRING RQB 121. 484. T))
		    (BLOCK-PKT-3 (GET-DISK-STRING RQB 242. 56. T)))
		   ((ZEROP N-BLOCKS)
		    (RETURN-ARRAY BLOCK-PKT-3)
		    (RETURN-ARRAY BLOCK-PKT-2)
		    (RETURN-ARRAY BLOCK-PKT-1))
		 ;; Get 3 packets and form a block in the buffer
		 ;; RECEIVE-PARTITION-PACKET will throw if it gets to eof.
		 (RECEIVE-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-1)
		 (RECEIVE-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-2)
		 (RECEIVE-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-3)
		 ;; Advance magic strings to next block
		 (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-1 3)
					      (* 4 PAGE-SIZE))
					   BLOCK-PKT-1 3)
		 (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-2 3)
					      (* 4 PAGE-SIZE))
					   BLOCK-PKT-2 3)
		 (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-3 3)
					      (* 4 PAGE-SIZE))
					   BLOCK-PKT-3 3)))))
    (:WRITE (LET ((RQB (CAR ARGS)))
	      (LET ((BLOCK (CADR ARGS))
		    (N-BLOCKS (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES)))
		(FORMAT REMOTE-DISK-STREAM "WRITE ~D ~D ~D~%" REMOTE-DISK-UNIT BLOCK N-BLOCKS)
		(FUNCALL REMOTE-DISK-STREAM ':FORCE-OUTPUT)
		(DO ((BLOCK BLOCK (1+ BLOCK))
		     (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
		     (BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484. T))
		     (BLOCK-PKT-2 (GET-DISK-STRING RQB 121. 484. T))
		     (BLOCK-PKT-3 (GET-DISK-STRING RQB 242. 56. T)))
		    ((ZEROP N-BLOCKS)
		     (RETURN-ARRAY BLOCK-PKT-3)
		     (RETURN-ARRAY BLOCK-PKT-2)
		     (RETURN-ARRAY BLOCK-PKT-1))
		  ;; Transmit three packets from block in buffer
		  (TRANSMIT-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-1)
		  (TRANSMIT-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-2)
		  (TRANSMIT-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-3)
		  ;; Advance magic strings to next block
		  (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-1 3)
					       (* 4 PAGE-SIZE))
					    BLOCK-PKT-1 3)
		  (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-2 3)
					       (* 4 PAGE-SIZE))
					    BLOCK-PKT-2 3)
		  (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-3 3)
					       (* 4 PAGE-SIZE))
					    BLOCK-PKT-3 3)))))
    (:DISPOSE (CHAOS:CLOSE REMOTE-DISK-CONN))
    (:UNIT-NUMBER REMOTE-DISK-UNIT)
    (:MACHINE-NAME (SYMBOLIC-CHAOS-ADDRESS (CHAOS:FOREIGN-ADDRESS REMOTE-DISK-CONN)))
    (:SAY (FORMAT REMOTE-DISK-STREAM "SAY ~A~%" (CAR ARGS))
	  (FUNCALL REMOTE-DISK-STREAM ':FORCE-OUTPUT))
    (:HANDLES-LABEL NIL)
    ))

(DEFUN CC-DISK-HANDLER (OP &REST ARGS)
  (SELECTQ OP
    (:READ (LET ((RQB (CAR ARGS)))
	     (LET ((BLOCK (CADR ARGS))
		   (N-BLOCKS (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES)))
	       (DO ((BLOCK BLOCK (1+ BLOCK))
		    (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
		    (BUF (RQB-BUFFER RQB))
		    (BUF-IDX -1))
		   ((ZEROP N-BLOCKS))
		 (CADR:CC-DISK-READ BLOCK CADR:CC-DISK-LOWCORE 2)
		 ;Following code transmogrified from DBG-READ-XBUS and DBG-READ.
		 ; You dont really think this would be reasonable via DL11 or debug kludge,
		 ; do you?
		 ;; Yes it is reasonable, you total fool!  Hacking the disk label is one of 
		 ;; the most useful things to do.  You really made me do alot of work, and
		 ;; it was not appreciated one bit.  --HIC
		 (IF (EQ CADR:DBG-ACCESS-PATH 'CADR:BUSINT)
		     (LET ((UBUS-WD-LOC
			     (LSH (CADR:DBG-SETUP-UNIBUS-MAP 17
						   (ASH CADR:CC-DISK-LOWCORE 8)) -1)))
		       (%UNIBUS-WRITE 766110 0)  ;high unibus adr bit and DBG-NXM-INHIBIT
		       (DOTIMES (W 400)
			 (%UNIBUS-WRITE 766114 UBUS-WD-LOC)
			 (AS-1 (%UNIBUS-READ 766100)
			       BUF
			       (SETQ BUF-IDX (1+ BUF-IDX)))
			 (%UNIBUS-WRITE 766114 (SETQ UBUS-WD-LOC (1+ UBUS-WD-LOC)))
			 (AS-1 (%UNIBUS-READ 766100)
			       BUF
			       (SETQ BUF-IDX (1+ BUF-IDX)))
			 (SETQ UBUS-WD-LOC (1+ UBUS-WD-LOC))))
		     (DO ((ADR (ASH CADR:CC-DISK-LOWCORE 8) (1+ ADR))
			  (WORD)
			  (W 0 (1+ W)))
			 (( W 400))
		       (SETQ WORD (CADR:PHYS-MEM-READ ADR))
		       (AS-1 (LOGAND 177777 WORD) BUF (SETQ BUF-IDX (1+ BUF-IDX)))
		       (AS-1 (LDB 2020 WORD) BUF (SETQ BUF-IDX (1+ BUF-IDX)))))))))
		 
    (:WRITE (LET ((RQB (CAR ARGS)))
	      (LET ((BLOCK (CADR ARGS))
		    (N-BLOCKS (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES)))
		(DO ((BLOCK BLOCK (1+ BLOCK))
		     (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
		     (BUF (RQB-BUFFER RQB))
		     (BUF-IDX -1))
		    ((ZEROP N-BLOCKS))
		  (IF (EQ CADR:DBG-ACCESS-PATH 'CADR:BUSINT)
		      (LET ((UBUS-WD-LOC
			      (LSH (CADR:DBG-SETUP-UNIBUS-MAP 17
						    (ASH CADR:CC-DISK-LOWCORE 8)) -1)))
			(%UNIBUS-WRITE 766110 0)  ;high unibus adr bit and DBG-NXM-INHIBIT
			(DOTIMES (W 400)
			  (%UNIBUS-WRITE 766114 UBUS-WD-LOC)
			  (%UNIBUS-WRITE 766100 (AR-1 BUF (SETQ BUF-IDX (1+ BUF-IDX))))
			  (%UNIBUS-WRITE 766114 (SETQ UBUS-WD-LOC (1+ UBUS-WD-LOC)))
			  (%UNIBUS-WRITE 766100 (AR-1 BUF (SETQ BUF-IDX (1+ BUF-IDX))))
			  (SETQ UBUS-WD-LOC (1+ UBUS-WD-LOC))))
		      (DO ((ADR (ASH CADR:CC-DISK-LOWCORE 8) (1+ ADR))
			   (WORD)
			   (W 0 (1+ W)))
			  (( W 400))
			(SETQ WORD (DPB (AR-1 BUF (SETQ BUF-IDX (+ 2 BUF-IDX)))
					2020
					(AR-1 BUF (1- BUF-IDX))))
			(CADR:PHYS-MEM-WRITE ADR WORD)))
	RETRY	  (CADR:CC-DISK-WRITE BLOCK CADR:CC-DISK-LOWCORE 1)
		  (COND ((AND CC-REMOTE-DISK-WRITE-CHECK 
			      (NULL (CADR:CC-DISK-READ BLOCK
						       (1+ CADR:CC-DISK-LOWCORE)
						       1)))
			 (GO RETRY)))  ;read it back to let hardware check ECC, etc.
		  ))))

    (:DISPOSE (COND ((NULL CC-DISK-INIT-P)
		     (CADR:CC-DISK-READ 1 CADR:CC-DISK-LOWCORE 1)) ;Restore saved core
		    (T (SETQ CADR:CC-DISK-TYPE NIL))))	;Otherwise read label now that it
							; maybe isnt garbage
    (:UNIT-NUMBER 0)
    (:MACHINE-NAME "via CC")
    (:SAY (FORMAT T "CC-SAY ~A~%" (CAR ARGS)))
    (:HANDLES-LABEL NIL)))

(DEFUN CC-TEST-HANDLER (OP &REST ARGS)
  (SELECTQ OP
    (:READ (LET ((RQB (CAR ARGS)))
	     (LET ((BLOCK (CADR ARGS))
		   (N-BLOCKS (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES)))
	       (DO ((BLOCK BLOCK (1+ BLOCK))
		    (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
		    (BUF (RQB-BUFFER RQB))
		    (BUF-IDX -1))
		   ((ZEROP N-BLOCKS))
		 (SELECTQ CC-DISK-UNIT
		   (0 (DOTIMES (W 400)	;Unit 0 makes block,,word
			(AS-1 BLOCK BUF (SETQ BUF-IDX (1+ BUF-IDX)))
			(AS-1 W BUF (SETQ BUF-IDX (1+ BUF-IDX)))))
		   (1 (DOTIMES (W 400)  ;Unit 1 makes block#7777,,word
			(AS-1 (LOGXOR 7777 BLOCK) BUF (SETQ BUF-IDX (1+ BUF-IDX)))
			(AS-1 W BUF (SETQ BUF-IDX (1+ BUF-IDX))))))))))
    (:WRITE (LET ((RQB (CAR ARGS)))
	      (LET ((BLOCK (CADR ARGS))
		    (N-BLOCKS (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES)))
		(DO ((BLOCK BLOCK (1+ BLOCK))
		     (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
		    (BUF (RQB-BUFFER RQB))
;FOLLOWING CODE IS TRANSMOGRIFIED FROM DBG-WRITE-XBUS
		    (BUF-IDX -1)
		    (W1) (W2) (ERRS 0) (MAX-ERRS 3))
		    ((ZEROP N-BLOCKS))
		 (SELECTQ CC-DISK-UNIT
		   (0 (DOTIMES (W 400)	;Unit 0 should be block,,word
			(SETQ W1 (AR-1 BUF (SETQ BUF-IDX (1+ BUF-IDX)))
			      W2 (AR-1 BUF (SETQ BUF-IDX (1+ BUF-IDX))))
			(COND ((OR (NOT (= BLOCK W1))
				   (NOT (= W2 W)))
			       (FORMAT T "~%Block ~O WD ~O should be ~O,,~O is ~O,,~O"
				       BLOCK W BLOCK W W1 W2)
			       (COND ((> (SETQ ERRS (1+ ERRS)) MAX-ERRS)
				      (RETURN NIL)))))))
		   (1 (DOTIMES (W 400)  ;Unit 1 should be block#7777,,word
			(SETQ W1 (AR-1 BUF (SETQ BUF-IDX (1+ BUF-IDX)))
			      W2 (AR-1 BUF (SETQ BUF-IDX (1+ BUF-IDX))))
			(COND ((OR (NOT (= (LOGXOR 7777 BLOCK) W1))
				   (NOT (= W2 W)))
			       (FORMAT T "~%Block ~O WD ~O should be ~O,,~O is ~O,,~O"
				       BLOCK W (LOGXOR BLOCK 7777) W W1 W2)
			       (COND ((> (SETQ ERRS (1+ ERRS)) MAX-ERRS)
				      (RETURN NIL))))))))))))
    (:DISPOSE NIL)
    (:UNIT-NUMBER CC-DISK-UNIT)
    (:MACHINE-NAME "TEST")
    (:SAY (FORMAT T "CC-TEST-SAY ~A~%" (CAR ARGS)))
    (:HANDLES-LABEL T)
    (:FIND-DISK-PARTITION (VALUES  0 100000 NIL))
    (:PARTITION-COMMENT (FORMAT NIL "TEST ~D" CC-DISK-UNIT))
    (:UPDATE-PARTITION-COMMENT NIL)
    ))


;;; These are internal

(DEFUN READ-DISK-LABEL (RQB UNIT)
  (DISK-READ RQB UNIT 0))

(DEFUN WRITE-DISK-LABEL (RQB UNIT)
  (OR (STRING-EQUAL (GET-DISK-STRING RQB 0 4) "LABL")
      (FERROR NIL "Attempt to write garbage label"))
  (DISK-WRITE RQB UNIT 0))

; The "amazing kludge" is because of double indirect-arrays with differing byte-size

(DEFUN GET-DISK-STRING (RQB WORD-ADDRESS N-CHARACTERS &OPTIONAL (SHARE-P NIL)
			&AUX (AMAZING-KLUDGE (%P-CONTENTS-OFFSET (RQB-BUFFER RQB) 3)))
  (COND (SHARE-P (MAKE-ARRAY N-CHARACTERS ':TYPE 'ART-STRING
			     ':DISPLACED-TO (RQB-BUFFER RQB)
			     ':DISPLACED-INDEX-OFFSET (+ AMAZING-KLUDGE (* 4 WORD-ADDRESS))))
	((LET ((STR (MAKE-ARRAY N-CHARACTERS ':TYPE 'ART-STRING))
	       (BSTR (MAKE-ARRAY N-CHARACTERS ':TYPE 'ART-STRING
				 ':DISPLACED-TO (RQB-BUFFER RQB)
				 ':DISPLACED-INDEX-OFFSET
				      (+ AMAZING-KLUDGE (* 4 WORD-ADDRESS)))))
	   (COPY-ARRAY-CONTENTS BSTR STR)
	   (RETURN-ARRAY BSTR)
	   (DO ((I 0 (1+ I)))
	       ((= I N-CHARACTERS))
	     (COND ((= (AREF STR I) 0)	;Seems padded with 0's rather than 200's
		    (ADJUST-ARRAY-SIZE STR I)
		    (RETURN))))
	   STR))))

(DEFUN PUT-DISK-STRING (RQB STR WORD-ADDRESS N-CHARACTERS
			&AUX (AMAZING-KLUDGE (%P-CONTENTS-OFFSET (RQB-BUFFER RQB) 3)))
  (DO ((TEM (MAKE-ARRAY N-CHARACTERS ':TYPE 'ART-STRING
			':DISPLACED-TO (RQB-BUFFER RQB)
                        ':DISPLACED-INDEX-OFFSET (+ AMAZING-KLUDGE (* 4 WORD-ADDRESS))))
       (I 0 (1+ I))
       (N (MIN (STRING-LENGTH STR) N-CHARACTERS)))
      ((>= I N)
       (DO I I (1+ I) (>= I N-CHARACTERS)
	 (ASET 0 TEM I))
       (RETURN-ARRAY TEM)
       NIL)
   (ASET (AREF STR I) TEM I)))

(DEFUN GET-DISK-FIXNUM (RQB WORD-ADDRESS)
  (DPB (AREF (RQB-BUFFER RQB) (1+ (* 2 WORD-ADDRESS)))
       2020
       (AREF (RQB-BUFFER RQB) (* 2 WORD-ADDRESS))))

(DEFUN PUT-DISK-FIXNUM (RQB VAL WORD-ADDRESS)
  (ASET (LDB 0020 VAL) (RQB-BUFFER RQB) (* 2 WORD-ADDRESS))
  (ASET (LDB 2020 VAL) (RQB-BUFFER RQB) (1+ (* 2 WORD-ADDRESS))))

;Returns NIL if no such partition, or 3 values (FIRST-BLOCK N-BLOCKS LABEL-LOC) if it exists
(DEFUN FIND-DISK-PARTITION (NAME &OPTIONAL RQB (UNIT 0) (ALREADY-READ-P NIL) CONFIRM-WRITE
			    &AUX (RETURN-RQB NIL))
  (DECLARE (RETURN-LIST FIRST-BLOCK N-BLOCKS LABEL-LOC))
  (IF (AND (CLOSUREP UNIT)
	   (FUNCALL UNIT ':HANDLES-LABEL))
      (FUNCALL UNIT ':FIND-DISK-PARTITION NAME)
      (PROG FIND-DISK-PARTITION ()
	(UNWIND-PROTECT
	  (PROGN
	    (COND ((NULL RQB)
		   (WITHOUT-INTERRUPTS
		     (SETQ RETURN-RQB T
			   RQB (GET-DISK-RQB)))))
	    (OR ALREADY-READ-P (READ-DISK-LABEL RQB UNIT))
	    (DO ((N-PARTITIONS (GET-DISK-FIXNUM RQB 200))
		 (WORDS-PER-PART (GET-DISK-FIXNUM RQB 201))
		 (I 0 (1+ I))
		 (LOC 202 (+ LOC WORDS-PER-PART)))
		((= I N-PARTITIONS) NIL)
	      (COND ((STRING-EQUAL (GET-DISK-STRING RQB LOC 4) NAME)
		     (AND CONFIRM-WRITE
			  (NOT (FQUERY FORMAT:YES-OR-NO-QUIETLY-P-OPTIONS
				"Do you really want to clobber partition ~A ~
				 ~:[~*~;on unit ~D ~](~A)? "
				NAME (NUMBERP UNIT) UNIT				
				(GET-DISK-STRING RQB (+ LOC 3) 16.)))
			  (RETURN-FROM FIND-DISK-PARTITION NIL T))
		     (RETURN-FROM FIND-DISK-PARTITION
				  (GET-DISK-FIXNUM RQB (+ LOC 1))
				  (GET-DISK-FIXNUM RQB (+ LOC 2))
				  LOC)))))
	  (AND RETURN-RQB (RETURN-DISK-RQB RQB))))))

;Useful interfaces to FIND-DISK-PARTITION which do your error checking for you
(DEFUN FIND-DISK-PARTITION-FOR-READ (NAME &OPTIONAL RQB (UNIT 0) (ALREADY-READ-P NIL)
						    (NUMBER-PREFIX "LOD"))
  (DECLARE (RETURN-LIST FIRST-BLOCK N-BLOCKS LABEL-LOC NAME))
  (COND ((NUMBERP NAME) (SETQ NAME (FORMAT NIL "~A~D" NUMBER-PREFIX NAME)))
	((SYMBOLP NAME) (SETQ NAME (GET-PNAME NAME)))
	((NOT (STRINGP NAME)) (FERROR NIL "~S is not a valid partition name" NAME)))
  (MULTIPLE-VALUE-BIND (FIRST-BLOCK N-BLOCKS LABEL-LOC)
      (FIND-DISK-PARTITION NAME RQB UNIT ALREADY-READ-P)
    (IF (NOT (NULL FIRST-BLOCK))
	(VALUES FIRST-BLOCK N-BLOCKS LABEL-LOC NAME)
	(FERROR NIL "No partition named /"~A/" exists on disk unit ~D." NAME UNIT))))

;Signals error if no partition exists, but returns NIL if it does exist
;but the user says not to use it.  The caller may check for NIL or blow
;out trying to do arithmetic on it, as he prefers.
(DEFUN FIND-DISK-PARTITION-FOR-WRITE (NAME &OPTIONAL RQB (UNIT 0) (ALREADY-READ-P NIL)
						    (NUMBER-PREFIX "LOD"))
  (DECLARE (RETURN-LIST FIRST-BLOCK N-BLOCKS LABEL-LOC NAME))
  (COND ((NUMBERP NAME) (SETQ NAME (FORMAT NIL "~A~D" NUMBER-PREFIX NAME)))
	((SYMBOLP NAME) (SETQ NAME (GET-PNAME NAME)))
	((NOT (STRINGP NAME)) (FERROR NIL "~S is not a valid partition name" NAME)))
  (MULTIPLE-VALUE-BIND (FIRST-BLOCK N-BLOCKS LABEL-LOC)
      (FIND-DISK-PARTITION NAME RQB UNIT ALREADY-READ-P T)
    (IF (NOT (NULL FIRST-BLOCK))
	(VALUES FIRST-BLOCK N-BLOCKS LABEL-LOC NAME)
	(IF (NULL N-BLOCKS)
	    (FERROR NIL "No partition named /"~A/" exists on disk unit ~D." NAME UNIT)
	    NIL))))

;; Returns a list with elements (<name> <base> <size> <comment> <desc-loc>)
(DEFUN PARTITION-LIST (&OPTIONAL RQB (UNIT 0) ALREADY-READ-P &AUX RETURN-RQB)
  (UNWIND-PROTECT
    (PROGN (COND ((NULL RQB)
		  (WITHOUT-INTERRUPTS
		    (SETQ RETURN-RQB T
			  RQB (GET-DISK-RQB)))))
	   (OR ALREADY-READ-P (READ-DISK-LABEL RQB UNIT))
	   (LET ((RESULT (MAKE-LIST (GET-DISK-FIXNUM RQB 200)))
		 (WORDS-PER-PART (GET-DISK-FIXNUM RQB 201)))
	     (DO ((LOC 202 (+ LOC WORDS-PER-PART))
		  (R RESULT (CDR R)))
		 ((NULL R) RESULT)
	       (SETF (CAR R)
		     (LIST (GET-DISK-STRING RQB LOC 4)
			   (GET-DISK-FIXNUM RQB (1+ LOC))
			   (GET-DISK-FIXNUM RQB (+ LOC 2))
			   (GET-DISK-STRING RQB (+ LOC 3) 16.)
			   LOC)))))
    (AND RETURN-RQB (RETURN-DISK-RQB RQB))))

;; This is a hack to allow one to easily find if a partition he wants is available.
(DEFUN PRINT-AVAILABLE-BANDS (&OPTIONAL (WHICH "LOD")
					(MACHINES (CHAOS:FINGER-ALL-LMS 'IGNORE NIL T))
			      &AUX (WL (AND (STRINGP WHICH)
					    (MIN (ARRAY-ACTIVE-LENGTH WHICH) 4)))
				   RQB UNIT TEM PARTITION-LIST PARTITION-LIST-ALIST)
  (CHECK-ARG WHICH (OR (STRINGP WHICH) (EQ WHICH T)) "a string or T")
  (UNWIND-PROTECT
    (PROGN (SETQ RQB (GET-DISK-RQB))
	   (DOLIST (M MACHINES)
	     (UNWIND-PROTECT
	       (SETQ UNIT (DECODE-UNIT-ARGUMENT M "Examining Label")
		     PARTITION-LIST (PARTITION-LIST RQB UNIT))
	       (DISPOSE-OF-UNIT UNIT))
	     (DOLIST (PARTITION PARTITION-LIST)
	       (AND (OR (EQ WHICH T)
			(STRING-EQUAL (CAR PARTITION) WHICH 0 0 WL WL))
		    (PLUSP (STRING-LENGTH (FOURTH PARTITION)))
		    (IF (SETQ TEM (ASSOC (FOURTH PARTITION) PARTITION-LIST-ALIST))
			(RPLACD (LAST TEM) (NCONS (LIST M (FIRST PARTITION))))
			(PUSH (LIST* (FOURTH PARTITION)
				     (LIST M (FIRST PARTITION))
				     NIL)
			      PARTITION-LIST-ALIST))))))
    (RETURN-DISK-RQB RQB))
  (SETQ PARTITION-LIST-ALIST (SORTCAR PARTITION-LIST-ALIST #'STRING-LESSP))
  (DOLIST (P PARTITION-LIST-ALIST)
    (FORMAT T "~%~A:~20T~:{~<~%~20T~2:;~A ~A~>~:^, ~}" (CAR P) (CDR P))))

;;; Functions for shipping partitions across the network

;This should be installed
(DEFUN SYMBOLIC-CHAOS-ADDRESS (NUM)
  (GET-HOST-FROM-ADDRESS NUM ':CHAOS))

(DEFUN RECEIVE-PARTITION (PART &OPTIONAL (STARTING-HUNDRED 0) (STREAM T) (UNIT 0)
			       &AUX PART-BASE PART-SIZE RQB
			            RFC-ARG-STRING)
  (UNWIND-PROTECT
   (PROGN
    (WITHOUT-INTERRUPTS
     (SETQ RQB (GET-DISK-RQB)))
    (MULTIPLE-VALUE (PART-BASE PART-SIZE NIL PART)
      (FIND-DISK-PARTITION-FOR-WRITE PART RQB UNIT))
    (LET ((CONN (CHAOS:LISTEN "PARTITION-TRANSFER" 25.))) ;Window 25, # int buffers 32
      (SETQ RFC-ARG-STRING
	    (SUBSTRING-AFTER-CHAR #\SP (CHAOS:PKT-STRING (CHAOS:READ-PKTS CONN))))
      (CHAOS:ACCEPT CONN)
      (FORMAT STREAM "~&Begin transfer of ~A on unit ~D from ~A~%"
	      PART UNIT (SYMBOLIC-CHAOS-ADDRESS (CHAOS:FOREIGN-ADDRESS CONN)))
      (*CATCH 'EOF
	 (DO ((BLOCK (+ PART-BASE (* STARTING-HUNDRED 100.)) (1+ BLOCK))
	      (N (- PART-SIZE (* STARTING-HUNDRED 100.)) (1- N))
	      (BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484. T))
	      (BLOCK-PKT-2 (GET-DISK-STRING RQB 121. 484. T))
	      (BLOCK-PKT-3 (GET-DISK-STRING RQB 242. 56. T)))
	     (NIL)
	     ;Get 3 packets and form a block in the buffer
	     ;RECEIVE-PARTITION-PACKET will throw if it gets to eof.
	     (RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-1)
	     (COND ((ZEROP N) ;Transmitter sent more than we have room for,
		    (FERROR NIL "Received-partition too big")  ; just close on him
		    (RETURN NIL)))
	     (RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-2)
	     (RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-3)
	     (DISK-WRITE RQB UNIT BLOCK)
	     (COND ((ZEROP (\ (- BLOCK PART-BASE) 100.))
		    (FORMAT STREAM "~D " (// (- BLOCK PART-BASE) 100.))))))
      (CHAOS:CLOSE CONN)
      (UPDATE-PARTITION-COMMENT PART RFC-ARG-STRING UNIT)
      (FORMAT STREAM "~&Receive done~%")))
   (RETURN-DISK-RQB RQB)))

(DEFUN TRANSMIT-PARTITION (HOST PART &OPTIONAL (STARTING-HUNDRED 0) (STREAM T) (UNIT 0)
				     &AUX PART-BASE PART-SIZE DESC-LOC RQB)
  (UNWIND-PROTECT
   (PROGN
    (WITHOUT-INTERRUPTS
     (SETQ RQB (GET-DISK-RQB)))
    (MULTIPLE-VALUE (PART-BASE PART-SIZE DESC-LOC)
      (FIND-DISK-PARTITION-FOR-READ PART RQB UNIT))
    (LET ((CONN (CHAOS:CONNECT HOST (STRING-APPEND "PARTITION-TRANSFER "
						   (PARTITION-COMMENT PART UNIT)))))
      (COND ((STRINGP CONN) CONN) ;Error message
            (T
             (FORMAT STREAM "~&Begin transfer of ~A on unit ~D to ~A"
                     PART UNIT (SYMBOLIC-CHAOS-ADDRESS (CHAOS:FOREIGN-ADDRESS CONN)))
             (COND ((STRING-EQUAL PART "LOD" 0 0 3 3)
		    (DISK-READ RQB UNIT (1+ PART-BASE))
		    (LET* ((BUF (RQB-BUFFER RQB))
			   (SIZE (DPB (AREF BUF (1+ (* 2 %SYS-COM-VALID-SIZE)))
				      1010	;Knows page-size is 2^8
				      (LDB 1010 (AREF BUF (* 2 %SYS-COM-VALID-SIZE))))))
		      (COND ((AND (> SIZE 10) ( SIZE PART-SIZE))
			     (SETQ PART-SIZE SIZE)
			     (FORMAT STREAM
				     "... using measured size of ~D. blocks." SIZE))))))
             (TERPRI)
             (DO ((BLOCK (+ PART-BASE (* STARTING-HUNDRED 100.)) (1+ BLOCK))
                  (N (- PART-SIZE (* STARTING-HUNDRED 100.)) (1- N))
                  (BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484. T))
                  (BLOCK-PKT-2 (GET-DISK-STRING RQB 121. 484. T))
                  (BLOCK-PKT-3 (GET-DISK-STRING RQB 242. 56. T)))
                 ((ZEROP N)
		  (CHAOS:SEND-PKT CONN (CHAOS:GET-PKT) CHAOS:EOF-OP)
		  (CHAOS:FINISH CONN)
		  (CHAOS:CLOSE CONN))
                ;Get a block in the buffer and send 3 packets
                (DISK-READ RQB UNIT BLOCK)
                (TRANSMIT-PARTITION-PACKET CONN BLOCK-PKT-1)
                (TRANSMIT-PARTITION-PACKET CONN BLOCK-PKT-2)
                (TRANSMIT-PARTITION-PACKET CONN BLOCK-PKT-3)
                (COND ((ZEROP (\ (- BLOCK PART-BASE) 100.))
                       (FORMAT STREAM "~D " (// (- BLOCK PART-BASE) 100.)))))
             (FORMAT STREAM "~&Transmit done~%")))))
   (RETURN-DISK-RQB RQB)))

(DEFUN COMPARE-RECEIVED-PARTITION (PART &OPTIONAL (STARTING-HUNDRED 0) (STREAM T) (UNIT 0)
					&AUX PART-BASE PART-SIZE RQB RQB2)
  (UNWIND-PROTECT
   (PROGN
    (WITHOUT-INTERRUPTS
     (SETQ RQB (GET-DISK-RQB))
     (SETQ RQB2 (GET-DISK-RQB)))
    (MULTIPLE-VALUE (PART-BASE PART-SIZE) (FIND-DISK-PARTITION-FOR-READ PART RQB UNIT))
    (LET ((CONN (CHAOS:LISTEN "PARTITION-TRANSFER")))
      (CHAOS:ACCEPT CONN)
      (FORMAT STREAM "~&Begin compare of ~A on unit ~D from ~A~%"
	      PART UNIT (SYMBOLIC-CHAOS-ADDRESS (CHAOS:FOREIGN-ADDRESS CONN)))
      (*CATCH 'EOF
	 (DO ((BLOCK (+ PART-BASE (* STARTING-HUNDRED 100.)) (1+ BLOCK))
	      (N (- PART-SIZE (* STARTING-HUNDRED 100.)) (1- N))
	      (BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484. T))
	      (BLOCK-PKT-2 (GET-DISK-STRING RQB 121. 484. T))
	      (BLOCK-PKT-3 (GET-DISK-STRING RQB 242. 56. T))
	      (BUF (RQB-BUFFER RQB))
	      (BUF2 (RQB-BUFFER RQB2)))
	     (NIL)
	     ;Get 3 packets and form a block in the buffer
	     ;RECEIVE-PARTITION-PACKET will throw if it gets to eof.
	     (RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-1)
	     (COND ((ZEROP N) ;Transmitter sent more than we have room for,
		    (FERROR NIL "Received-partition too big")  ; just close on him
		    (RETURN NIL)))
	     (RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-2)
	     (RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-3)
	     (DISK-READ RQB2 UNIT BLOCK)
	     (DO ((C 0 (1+ C))
		  (ERRS 0))
		 ((OR (= C 1000) (= ERRS 3)))
	       (COND ((NOT (= (AR-1 BUF C) (AR-1 BUF2 C)))
		      (FORMAT T "~%ERR Block ~O Halfword ~O, Part: ~O Rcvd: ~O "
			      (- BLOCK (+ PART-BASE (* STARTING-HUNDRED 100.)))
			      C
			      (AR-1 BUF2 C)
			      (AR-1 BUF C))
		      (SETQ ERRS (1+ ERRS)))))
	     (COND ((ZEROP (\ (- BLOCK PART-BASE) 100.))
		    (FORMAT STREAM "~D " (// (- BLOCK PART-BASE) 100.))))))
      (CHAOS:CLOSE CONN)
      (FORMAT STREAM "~&Receive done~%")))
   (RETURN-DISK-RQB RQB)
   (RETURN-DISK-RQB RQB2)))

(DEFUN RECEIVE-PARTITION-PACKET (CONN INTO)
  (LET ((PKT (CHAOS:GET-NEXT-PKT CONN)))
    (AND (NULL PKT) (FERROR NIL "Connection ~S broken" CONN))
    (SELECT (CHAOS:PKT-OPCODE PKT)
      (CHAOS:DAT-OP
       (COPY-ARRAY-CONTENTS (CHAOS:PKT-STRING PKT) INTO)
       (LET ((CORRECT (AREF PKT (+ (// (ARRAY-LENGTH INTO) 2) 10)))
	     (ACTUAL (CHECKSUM-STRING INTO)))
	 (OR (= CORRECT ACTUAL)
	     (FORMAT T "~&Checksum error, correct=~O, actual=~O~%" CORRECT ACTUAL)))
       (CHAOS:RETURN-PKT PKT))
      (CHAOS:EOF-OP
       (CHAOS:RETURN-PKT PKT)
       (*THROW 'EOF NIL))
      (OTHERWISE
        (FERROR NIL "~S is illegal packet opcode, pkt ~S, received for connection ~S"
                    (CHAOS:PKT-OPCODE PKT) PKT CONN)))))

(DEFUN TRANSMIT-PARTITION-PACKET (CONN OUTOF)
  (LET ((PKT (CHAOS:GET-PKT)))
    (COPY-ARRAY-CONTENTS OUTOF (CHAOS:PKT-STRING PKT))
    (ASET (CHECKSUM-STRING OUTOF) PKT (+ (// (ARRAY-LENGTH OUTOF) 2) 10))
    (SETF (CHAOS:PKT-NBYTES PKT) (+ (ARRAY-LENGTH OUTOF) 2))
    (CHAOS:SEND-PKT CONN PKT)))

(DEFUN CHECKSUM-STRING (STR)
  (DO ((CKSM 0 (+ (AREF STR I) CKSM))
       (I 0 (1+ I))
       (N (ARRAY-LENGTH STR)))
      (( I N) (LOGAND 177777 CKSM))))

;Return number of pages occupied by saved image stored on band.
;This really ought to be in the label, like an array-active-length
(DEFUN MEASURED-SIZE-OF-PARTITION (PART &OPTIONAL (UNIT 0)
				     &AUX PART-BASE PART-SIZE RQB)
  (SETQ UNIT (DECODE-UNIT-ARGUMENT UNIT
				   (FORMAT NIL "sizing ~A partition" PART)))
  (UNWIND-PROTECT
    (PROGN
      (WITHOUT-INTERRUPTS
        (SETQ RQB (GET-DISK-RQB)))
      (MULTIPLE-VALUE (PART-BASE PART-SIZE) (FIND-DISK-PARTITION-FOR-READ PART RQB UNIT))
      (COND ((OR (NUMBERP PART) (STRING-EQUAL PART "LOD" 0 0 3 3))
	     (DISK-READ RQB UNIT (1+ PART-BASE))
	     (LET ((BUF (RQB-BUFFER RQB)))
	       (LET ((SIZE (DPB (AREF BUF (1+ (* 2 %SYS-COM-VALID-SIZE)))
				1010		;Knows page-size is 2^8
				(LDB 1010 (AREF BUF (* 2 %SYS-COM-VALID-SIZE))))))
		 (IF (AND (> SIZE 10) ( SIZE PART-SIZE)) SIZE PART-SIZE))))
	    (T PART-SIZE)))
    (RETURN-DISK-RQB RQB)))

(DEFUN DISK-INIT (&AUX RQB SIZE)
  (UNWIND-PROTECT
   (PROGN (WITHOUT-INTERRUPTS
	     (SETQ RQB (GET-DISK-RQB)))
	  (READ-DISK-LABEL RQB 0)
	  ;; Update things which depend on the location and size of the paging area
	  (MULTIPLE-VALUE (PAGE-OFFSET SIZE)
	    (FIND-DISK-PARTITION "PAGE" RQB 0 T))
	  (SETQ VIRTUAL-MEMORY-SIZE (* (MIN (LDB 1020 A-MEMORY-VIRTUAL-ADDRESS) SIZE)
				       PAGE-SIZE))
	  (SETQ DISK-PACK-NAME (GET-DISK-STRING RQB 20 32.)))
   (RETURN-DISK-RQB RQB)))

(DEFUN PRINT-LOADED-BAND (&OPTIONAL (STREAM T)) ;Can be NIL to return a string
    (OR (ZEROP %LOADED-BAND) (SETQ CURRENT-LOADED-BAND %LOADED-BAND))
    (OR (BOUNDP 'CURRENT-LOADED-BAND) (SETQ CURRENT-LOADED-BAND 0))
    (PROG2  ;If STREAM is NIL, want to return a string with no carriage returns in it
      (FORMAT STREAM "~&")
      (FORMAT STREAM "This is band ~C of ~A, with ~A"
          (LDB 2010 CURRENT-LOADED-BAND) ;4th character in char string (only high 3 stored)
	  DISK-PACK-NAME
	  (IF (FBOUNDP 'SYSTEM-VERSION-INFO)	;For the cold load
	      (SYSTEM-VERSION-INFO)
              "[fresh cold load]"))
      (FORMAT STREAM "~%")))

;; This is called explicitly by LISP-REINITIALIZE.
(DEFUN PRINT-HERALD (&OPTIONAL (S STANDARD-OUTPUT))
  (OR (ZEROP %LOADED-BAND) (SETQ CURRENT-LOADED-BAND %LOADED-BAND))
  (OR (BOUNDP 'CURRENT-LOADED-BAND) (SETQ CURRENT-LOADED-BAND 0))
  (FORMAT S "~&This is band ~C of ~A."
	  (LDB 2010 CURRENT-LOADED-BAND)
	  DISK-PACK-NAME)
  (AND (BOUNDP 'SYSTEM-ADDITIONAL-INFO)
       (PLUSP (ARRAY-ACTIVE-LENGTH SYSTEM-ADDITIONAL-INFO))
       (FORMAT S " (~A)" SYSTEM-ADDITIONAL-INFO))
  (IF (NOT (FBOUNDP 'DESCRIBE-SYSTEM-VERSIONS))
      (FORMAT S "~%Fresh Cold Load~%")
      (DESCRIBE-SYSTEM-VERSIONS S)
      (FORMAT S "~%~A ~A, with associated machine ~A.~%"
	      (OR (GET-SITE-OPTION ':SITE-PRETTY-NAME) SITE-NAME)
	      LOCAL-PRETTY-HOST-NAME
	      ASSOCIATED-MACHINE)))

;Must be defined before initialization below
(DEFUN WIRE-PAGE (ADDRESS &OPTIONAL (WIRE-P T) SET-MODIFIED DONT-BOTHER-PAGING-IN)
  (IF WIRE-P
      (DO ()
	  ((%CHANGE-PAGE-STATUS ADDRESS %PHT-SWAP-STATUS-WIRED NIL)
	   (IF SET-MODIFIED			;Set modified bit without changing anything
	       (%P-STORE-DATA-TYPE ADDRESS (%P-DATA-TYPE ADDRESS))))
	(COND ((NOT DONT-BOTHER-PAGING-IN)
	       (%P-LDB 1 (%POINTER ADDRESS)))	;Haul it in
	      ((NULL (%PAGE-STATUS ADDRESS))
	       (WITHOUT-INTERRUPTS		;Try not to get aborted
		 (LET ((PFN (%FINDCORE)))
		   (OR (%PAGE-IN PFN (LSH ADDRESS -8))
		       ;Page already got in somehow, free up the PFN
		       (%CREATE-PHYSICAL-PAGE (LSH PFN 8))))))))
      (UNWIRE-PAGE ADDRESS)))

(DEFUN UNWIRE-PAGE (ADDRESS)
  (%CHANGE-PAGE-STATUS ADDRESS %PHT-SWAP-STATUS-NORMAL NIL))

(DEFUN WIRE-WORDS (FROM SIZE &OPTIONAL (WIRE-P T) SET-MODIFIED DONT-BOTHER-PAGING-IN)
  (DO ((ADR (- FROM (LOGAND FROM (1- PAGE-SIZE))) (+ ADR PAGE-SIZE))
       (N   (- PAGE-SIZE (LOGAND FROM (1- PAGE-SIZE))) (+ N PAGE-SIZE)))
      ((>= N SIZE))
    (WIRE-PAGE ADR WIRE-P SET-MODIFIED DONT-BOTHER-PAGING-IN)))

(DEFUN UNWIRE-WORDS (FROM SIZE)
  (WIRE-WORDS FROM SIZE NIL))
      
(DEFUN WIRE-ARRAY  (ARRAY &OPTIONAL FROM TO SET-MODIFIED DONT-BOTHER-PAGING-IN &AUX SIZE)
  (WITHOUT-INTERRUPTS
    (MULTIPLE-VALUE (ARRAY FROM SIZE)
      (PAGE-ARRAY-CALCULATE-BOUNDS ARRAY FROM TO))
    (AND ARRAY
	 ;; Have starting word and number of words. 
	 (WIRE-WORDS FROM SIZE T SET-MODIFIED DONT-BOTHER-PAGING-IN))))

(DEFUN UNWIRE-ARRAY (ARRAY &OPTIONAL FROM TO &AUX SIZE)
  (WITHOUT-INTERRUPTS
    (MULTIPLE-VALUE (ARRAY FROM SIZE)
      (PAGE-ARRAY-CALCULATE-BOUNDS ARRAY FROM TO))
    (AND ARRAY
	 ;; Have starting word and number of words. 
	 (UNWIRE-WORDS FROM SIZE))))

;;; Takes the number of an area and wires down all the allocated
;;; pages of it, or un-wires, depending on the second argument.
;;; The area had better have only one region.
;;; Also doesn't work on downwards-consed list regions (which no longer exist).
(DEFUN WIRE-AREA (AREA WIRE-P)
  (LET ((REGION (AREA-REGION-LIST AREA)))
    (OR (MINUSP (REGION-LIST-THREAD REGION)) ;last region in area
	(FERROR NIL "Area ~A has more than one region" (AREA-NAME AREA)))
    (DO ((LOC (REGION-ORIGIN REGION) (%24-BIT-PLUS LOC PAGE-SIZE))
	 (COUNT (// (+ (REGION-FREE-POINTER REGION) (1- PAGE-SIZE)) PAGE-SIZE) (1- COUNT)))
	((ZEROP COUNT))
      (WIRE-PAGE LOC WIRE-P))))

;This must be after main definitions above, but before initialization below!
(ADD-INITIALIZATION "DISK-INIT" '(DISK-INIT) '(SYSTEM))

;;; Put a microcode file onto my own disk.
;;; Note that the cretinous halfwords are out of order
(DEFUN LOAD-MCR-FILE (FILENAME PART &OPTIONAL (UNIT 0)
                                    &AUX PART-BASE PART-SIZE RQB)
  (SETQ FILENAME (IF (NUMBERP FILENAME)
		     (FUNCALL (FS:PARSE-PATHNAME "SYS:UBIN;UCADR")
			      ':NEW-TYPE-AND-VERSION "MCR" FILENAME)
		     (FS:MERGE-PATHNAME-DEFAULTS FILENAME)))
  (OR (EQUAL (FUNCALL FILENAME ':TYPE-AND-VERSION) "MCR")
      (FERROR NIL "~A is not a MCR file." FILENAME))
  (SETQ UNIT (DECODE-UNIT-ARGUMENT UNIT
		(FORMAT NIL "Loading ~A into ~A partition" FILENAME PART)
		NIL
		T))
  (UNWIND-PROTECT
    (PROGN
      (WITHOUT-INTERRUPTS
	(SETQ RQB (GET-DISK-RQB)))
      (MULTIPLE-VALUE (PART-BASE PART-SIZE NIL PART)
	(FIND-DISK-PARTITION-FOR-WRITE PART RQB UNIT NIL "MCR"))
    (WITH-OPEN-FILE (FILE FILENAME '(:READ :FIXNUM))
       (DO-NAMED DONE
	   ((BUF16 (ARRAY-LEADER RQB %DISK-RQ-LEADER-BUFFER))
            (BLOCK PART-BASE (1+ BLOCK))
            (N PART-SIZE (1- N)))
           ((ZEROP N) (FERROR NIL "Failed to fit in partition"))
         (DO ((LH) (RH)
              (I 0 (+ I 2)))
             ((= I 1000)
              (DISK-WRITE RQB UNIT BLOCK))
           (SETQ LH (FUNCALL FILE ':TYI)
                 RH (FUNCALL FILE ':TYI))
           (COND ((OR (NULL LH) (NULL RH))
		  (UPDATE-PARTITION-COMMENT
		       PART
		       (LET ((PATHNAME (FUNCALL FILE ':TRUENAME)))
			 (MULTIPLE-VALUE-BIND (NIL VERSION)
			     (FUNCALL PATHNAME ':TYPE-AND-VERSION)
			   (FORMAT NIL "~A ~D" (FUNCALL PATHNAME ':NAME) VERSION)))
		       UNIT)
		  (RETURN-FROM DONE NIL)))
           (ASET RH BUF16 I)
           (ASET LH BUF16 (1+ I))))))
   (RETURN-DISK-RQB RQB)))

(DEFUN PARTITION-COMMENT (PART UNIT &AUX RQB DESC-LOC)
  (IF (AND (CLOSUREP UNIT)
	   (FUNCALL UNIT ':HANDLES-LABEL))
      (FUNCALL UNIT ':PARTITION-COMMENT PART)
      (UNWIND-PROTECT
	(PROGN (WITHOUT-INTERRUPTS
		 (SETQ RQB (GET-DISK-RQB)))
	       (MULTIPLE-VALUE (NIL NIL DESC-LOC) (FIND-DISK-PARTITION PART RQB UNIT))
	       (COND ((NULL DESC-LOC) NIL)
		     ((>= (GET-DISK-FIXNUM RQB 201) 7)
		      (GET-DISK-STRING RQB (+ DESC-LOC 3) 16.))
		     (T "")))
	(RETURN-DISK-RQB RQB))))

;;; Change the comment on a partition
(DEFUN UPDATE-PARTITION-COMMENT (PART STRING UNIT &AUX RQB DESC-LOC)
  (IF (AND (CLOSUREP UNIT)
	   (FUNCALL UNIT ':HANDLES-LABEL))
      (FUNCALL UNIT ':UPDATE-PARTITION-COMMENT PART STRING)
      (UNWIND-PROTECT
	(PROGN (WITHOUT-INTERRUPTS
		 (SETQ RQB (GET-DISK-RQB)))
	       (MULTIPLE-VALUE (NIL NIL DESC-LOC)
		 (FIND-DISK-PARTITION-FOR-READ PART RQB UNIT NIL NIL))
	       (AND (>= (GET-DISK-FIXNUM RQB 201) 7)
		    (PUT-DISK-STRING RQB STRING (+ DESC-LOC 3) 16.))
	       (WRITE-DISK-LABEL RQB UNIT))
	(RETURN-DISK-RQB RQB))))

(DEFUN COPY-DISK-PARTITION-BACKGROUND (FROM-UNIT FROM-PART TO-UNIT TO-PART STREAM
				       STARTING-HUNDRED)
  (PROCESS-RUN-FUNCTION "copy partition"
	#'(LAMBDA (FU FP TU TP TERMINAL-IO SH)
	    (COPY-DISK-PARTITION FU FP TU TP 10. 300. SH))
	FROM-UNIT FROM-PART TO-UNIT TO-PART STREAM STARTING-HUNDRED))

;;; Copying a partition from one unit to another
(DEFUN COPY-DISK-PARTITION (FROM-UNIT FROM-PART TO-UNIT TO-PART
			    &OPTIONAL (N-PAGES-AT-A-TIME 85.) (DELAY NIL)
				      (STARTING-HUNDRED 0) (WHOLE-THING-P NIL)
			    &AUX FROM-PART-BASE FROM-PART-SIZE TO-PART-BASE TO-PART-SIZE RQB
			         PART-COMMENT)
  (SETQ FROM-UNIT (DECODE-UNIT-ARGUMENT FROM-UNIT
					(FORMAT NIL "reading ~A partition" FROM-PART))
	TO-UNIT (DECODE-UNIT-ARGUMENT TO-UNIT
				      (FORMAT NIL "writing ~A partition" TO-PART)
				      NIL
				      T))
  (UNWIND-PROTECT
   (PROGN
    (WITHOUT-INTERRUPTS
     (SETQ RQB (GET-DISK-RQB N-PAGES-AT-A-TIME)))
    (MULTIPLE-VALUE (FROM-PART-BASE FROM-PART-SIZE NIL FROM-PART)
	(FIND-DISK-PARTITION-FOR-READ FROM-PART NIL FROM-UNIT))
    (MULTIPLE-VALUE (TO-PART-BASE TO-PART-SIZE NIL TO-PART)
	(FIND-DISK-PARTITION-FOR-WRITE TO-PART NIL TO-UNIT))
    (SETQ PART-COMMENT (PARTITION-COMMENT FROM-PART FROM-UNIT))
    (FORMAT T "~&Copying ~S" PART-COMMENT)
    (AND (OR (NUMBERP FROM-PART) (STRING-EQUAL FROM-PART "LOD" 0 0 3 3))
	 (NOT WHOLE-THING-P)
	 (LET ((RQB NIL) (BUF NIL))
	   (UNWIND-PROTECT
	     (PROGN (SETQ RQB (GET-DISK-RQB 1))
		    (SETQ BUF (RQB-BUFFER RQB))
		    (DISK-READ RQB FROM-UNIT (1+ FROM-PART-BASE))
		    (LET ((SIZE (DPB (AREF BUF (1+ (* 2 %SYS-COM-VALID-SIZE)))
				     1010   ;Knows page-size is 2^8
				     (LDB 1010 (AREF BUF (* 2 %SYS-COM-VALID-SIZE))))))
		      (COND ((AND (> SIZE 10) ( SIZE FROM-PART-SIZE))
			     (SETQ FROM-PART-SIZE SIZE)
			     (FORMAT T "... using measured size of ~D. blocks." SIZE)))))
	     (RETURN-DISK-RQB RQB))))
    (FORMAT T "~%")
    (UPDATE-PARTITION-COMMENT TO-PART "Incomplete Copy" TO-UNIT)
    (COND ((AND (CLOSUREP TO-UNIT)	;magtape needs to know this stuff before
		(FUNCALL TO-UNIT ':HANDLES-LABEL))  ;writing file.
	   (FUNCALL TO-UNIT ':PUT PART-COMMENT ':COMMENT)
	   (FUNCALL TO-UNIT ':PUT FROM-PART-SIZE ':SIZE)))
  ;Old hack which used to move WIRE-DISK-RQB outside loop flushed because
  ; DISK-READ sets modified bits during WIRE-DISK-RQB, which we may need to do.
    (DO ((FROM-ADR (+ FROM-PART-BASE (* 100. STARTING-HUNDRED)) (+ FROM-ADR AMT))
	 (TO-ADR (+ TO-PART-BASE (* 100. STARTING-HUNDRED)) (+ TO-ADR AMT))
	 (FROM-HIGH (+ FROM-PART-BASE FROM-PART-SIZE))
	 (TO-HIGH (+ TO-PART-BASE TO-PART-SIZE))
	 (N-BLOCKS (* 100. STARTING-HUNDRED) (+ N-BLOCKS AMT))
	 (N-HUNDRED STARTING-HUNDRED)
	 (AMT))
	((OR (>= FROM-ADR FROM-HIGH) (>= TO-ADR TO-HIGH)))
      (SETQ AMT (MIN (- FROM-HIGH FROM-ADR) (- TO-HIGH TO-ADR) N-PAGES-AT-A-TIME))
      (COND ((NOT (= AMT N-PAGES-AT-A-TIME))
	     (RETURN-DISK-RQB RQB)
	     (SETQ RQB (GET-DISK-RQB AMT))))
      (DISK-READ RQB FROM-UNIT FROM-ADR)
      (DISK-WRITE RQB TO-UNIT TO-ADR)
      (COND ((NOT (= (// (+ N-BLOCKS AMT) 100.) N-HUNDRED))
	     (SETQ N-HUNDRED (1+ N-HUNDRED))
	     (FORMAT T "~D " N-HUNDRED)))
      (IF DELAY (PROCESS-SLEEP DELAY)
	  (PROCESS-ALLOW-SCHEDULE))) ;kludge
    (UPDATE-PARTITION-COMMENT TO-PART PART-COMMENT TO-UNIT))
   ;;Unwind-protect forms
   (RETURN-DISK-RQB RQB))
  (DISPOSE-OF-UNIT FROM-UNIT)
  (DISPOSE-OF-UNIT TO-UNIT))

;PRINTS DIFFERENCES
(DEFUN COMPARE-DISK-PARTITION (FROM-UNIT FROM-PART TO-UNIT TO-PART
			    &OPTIONAL (N-PAGES-AT-A-TIME 85.) (DELAY NIL)
				      (STARTING-HUNDRED 0) (WHOLE-THING-P NIL)
			    &AUX FROM-PART-BASE FROM-PART-SIZE TO-PART-BASE TO-PART-SIZE
			         RQB RQB2)
  (SETQ FROM-UNIT (DECODE-UNIT-ARGUMENT FROM-UNIT
					(FORMAT NIL "reading ~A partition" FROM-PART))
	TO-UNIT (DECODE-UNIT-ARGUMENT TO-UNIT (FORMAT NIL "reading ~A partition" TO-PART)))
  (UNWIND-PROTECT
   (PROGN
    (WITHOUT-INTERRUPTS
     (SETQ RQB (GET-DISK-RQB N-PAGES-AT-A-TIME))
     (SETQ RQB2 (GET-DISK-RQB N-PAGES-AT-A-TIME)))
    (MULTIPLE-VALUE (FROM-PART-BASE FROM-PART-SIZE)
	(FIND-DISK-PARTITION-FOR-READ FROM-PART NIL FROM-UNIT))
    (MULTIPLE-VALUE (TO-PART-BASE TO-PART-SIZE)
	(FIND-DISK-PARTITION-FOR-READ TO-PART NIL TO-UNIT))
    (FORMAT T "~&Comparing ~S and ~S"
	    (PARTITION-COMMENT FROM-PART FROM-UNIT)
	    (PARTITION-COMMENT TO-PART TO-UNIT))
    (AND (STRING-EQUAL FROM-PART "LOD" 0 0 3 3)
	 (NOT WHOLE-THING-P)
	 (LET ((RQB NIL) (BUF NIL))
	   (UNWIND-PROTECT
	     (PROGN (SETQ RQB (GET-DISK-RQB 1))
		    (SETQ BUF (RQB-BUFFER RQB))
		    (DISK-READ RQB FROM-UNIT (1+ FROM-PART-BASE))
		    (LET ((SIZE (DPB (AREF BUF (1+ (* 2 %SYS-COM-VALID-SIZE)))
				     1010   ;Knows page-size is 2^8
				     (LDB 1010 (AREF BUF (* 2 %SYS-COM-VALID-SIZE))))))
		      (COND ((AND (> SIZE 10) ( SIZE FROM-PART-SIZE))
			     (SETQ FROM-PART-SIZE SIZE)
			     (FORMAT T "... using measured size of ~D. blocks." SIZE)))))
	     (RETURN-DISK-RQB RQB))))
    (DO ((FROM-ADR (+ FROM-PART-BASE (* 100. STARTING-HUNDRED)) (+ FROM-ADR AMT))
	 (TO-ADR (+ TO-PART-BASE (* 100. STARTING-HUNDRED)) (+ TO-ADR AMT))
	 (FROM-HIGH (+ FROM-PART-BASE FROM-PART-SIZE))
	 (TO-HIGH (+ TO-PART-BASE TO-PART-SIZE))
	 (N-BLOCKS (* 100. STARTING-HUNDRED) (+ N-BLOCKS AMT))
	 (N-HUNDRED STARTING-HUNDRED)
	 (AMT)
	 (BUF (RQB-BUFFER RQB))
	 (BUF2 (RQB-BUFFER RQB2)))
	((OR (>= FROM-ADR FROM-HIGH) (>= TO-ADR TO-HIGH)))
      (SETQ AMT (MIN (- FROM-HIGH FROM-ADR) (- TO-HIGH TO-ADR) N-PAGES-AT-A-TIME))
      (COND ((NOT (= AMT N-PAGES-AT-A-TIME))
	     (RETURN-DISK-RQB RQB)
	     (RETURN-DISK-RQB RQB2)
	     (SETQ RQB (GET-DISK-RQB AMT))
	     (SETQ RQB2 (GET-DISK-RQB AMT))
	     (SETQ BUF (RQB-BUFFER RQB))
	     (SETQ BUF2 (RQB-BUFFER RQB2))))
      (DISK-READ RQB FROM-UNIT FROM-ADR)
      (DISK-READ RQB2 TO-UNIT TO-ADR)
      (DO ((C 0 (1+ C))
	   (ERRS 0)
	   (LIM (* 1000 AMT)))
	  ((OR (= C LIM) (= ERRS 3)))
	(COND ((NOT (= (AR-1 BUF C) (AR-1 BUF2 C)))
	       (FORMAT T "~%ERR Block ~O Halfword ~O, S1: ~O S2: ~O "
		       (+ (- FROM-ADR (+ FROM-PART-BASE (* STARTING-HUNDRED 100.)))
			  (// C 1000))
		       (\ C 1000)
		       (AR-1 BUF C)
		       (AR-1 BUF2 C))
	       (SETQ ERRS (1+ ERRS)))))
      (COND ((NOT (= (// N-BLOCKS 100.) N-HUNDRED))
	     (SETQ N-HUNDRED (// N-BLOCKS 100.))
	     (FORMAT T "~D " N-HUNDRED)))
      (IF DELAY (PROCESS-SLEEP DELAY)
	  (PROCESS-ALLOW-SCHEDULE))) ;kludge
    )
   ;;Unwind-protect forms
   (RETURN-DISK-RQB RQB)
   (RETURN-DISK-RQB RQB2)
   (DISPOSE-OF-UNIT FROM-UNIT)
   (DISPOSE-OF-UNIT TO-UNIT)))

;;; User-controlled paging code

;;; We have a special RQB which is not like a normal disk RQB in that
;;; it doesn't contain any buffer.  It is exactly one page long since
;;; everything in DISK-BUFFER-AREA has to be a multiple of a page.
;;; This defines the number of CCWs.

(DEFVAR PAGE-RQB-SIZE (- PAGE-SIZE 1 (// %DISK-RQ-CCW-LIST 2))) ;NUMBER OF CCWS
(DEFVAR PAGE-RQB (MAKE-ARRAY DISK-BUFFER-AREA 'ART-16B (* 2 (1- PAGE-SIZE))))

(DEFUN WIRE-PAGE-RQB () 
  (WIRE-PAGE (%POINTER PAGE-RQB))
  (LET ((PADR (+ (%PHYSICAL-ADDRESS PAGE-RQB)
		 1
		 (// %DISK-RQ-CCW-LIST 2))))
    (ASET PADR PAGE-RQB %DISK-RQ-CCW-LIST-POINTER-LOW)
    (ASET (LSH PADR -16.) PAGE-RQB %DISK-RQ-CCW-LIST-POINTER-HIGH)))

(DEFUN UNWIRE-PAGE-RQB ()
  (UNWIRE-PAGE (%POINTER PAGE-RQB)))

(DEFUN PAGE-IN-AREA (AREA)
  (DO REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION) (MINUSP REGION)
    (PAGE-IN-REGION REGION)))

(DEFUN PAGE-OUT-AREA (AREA)
  (DO REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION) (MINUSP REGION)
    (PAGE-OUT-REGION REGION)))

(DEFUN PAGE-IN-REGION (REGION)
  (PAGE-IN-WORDS (REGION-ORIGIN REGION) (REGION-FREE-POINTER REGION)))

(DEFUN PAGE-OUT-REGION (REGION)
  (PAGE-OUT-WORDS (REGION-ORIGIN REGION) (REGION-FREE-POINTER REGION)))

(DEFUN PAGE-IN-STRUCTURE (OBJ)
  (SETQ OBJ (FOLLOW-STRUCTURE-FORWARDING OBJ))
  (PAGE-IN-WORDS (%FIND-STRUCTURE-LEADER OBJ)
		 (%STRUCTURE-TOTAL-SIZE OBJ)))

(DEFUN PAGE-OUT-STRUCTURE (OBJ)
  (SETQ OBJ (FOLLOW-STRUCTURE-FORWARDING OBJ))
  (PAGE-OUT-WORDS (%FIND-STRUCTURE-LEADER OBJ)
		  (%STRUCTURE-TOTAL-SIZE OBJ)))

;FROM and TO are lists of subscripts.  If too short, zeros are appended.
;Returns array, starting address of data, number of Q's of data.
;First value is NIL if displaced to an absolute address (probably TV buffer).
(DEFUN PAGE-ARRAY-CALCULATE-BOUNDS (ARRAY FROM TO)
  (SETQ ARRAY (FOLLOW-STRUCTURE-FORWARDING ARRAY))
  (PROG DONE (NDIMS TYPE START END SIZE ELTS-PER-Q)
    (SETQ NDIMS (ARRAY-/#-DIMS ARRAY)
	  TYPE (ARRAY-TYPE ARRAY))
    (OR ( (LENGTH FROM) NDIMS)
	(FERROR NIL "Too many dimensions in starting index ~S" FROM))
    (OR ( (LENGTH TO) NDIMS)
	(FERROR NIL "Too many dimensions in ending index ~S" TO))
    (SETQ START (OR (NTH (1- NDIMS) FROM) 0)
	  END (1- (OR (NTH (1- NDIMS) TO) (ARRAY-DIMENSION-N NDIMS ARRAY))))
    (DO ((I (1- NDIMS) (1- I))
	 (DIM))
	((ZEROP I))
      (SETQ START (+ (* START (SETQ DIM (ARRAY-DIMENSION-N I ARRAY)))
		     (OR (NTH (1- I) FROM) 0))
	    END (+ (* END DIM)
		   (1- (OR (NTH (1- I) TO) DIM)))))
    (SETQ END (1+ END))		;Convert from inclusive upper bound to exclusive
    (SETQ SIZE (- END START))
    (DO ((P))
	((ZEROP (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0)))
      (SETQ NDIMS (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0))
      (SETQ P
	    (%MAKE-POINTER-OFFSET DTP-LOCATIVE ARRAY
				  (+ NDIMS (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))))
      (AND (NOT (ZEROP (%P-FLAG-BIT P)))		;Index offset
	   (SETQ START (+ START (%P-CONTENTS-OFFSET P 2))))
      (SETQ ARRAY (%P-CONTENTS-OFFSET P 0))
      (OR (ARRAYP ARRAY)
	  (RETURN-FROM DONE NIL)))
    (SETQ ELTS-PER-Q (CDR (ASSOC TYPE ARRAY-ELEMENTS-PER-Q)))
    (SETQ START (+ (IF (PLUSP ELTS-PER-Q) (// START ELTS-PER-Q) (* START (MINUS ELTS-PER-Q)))
		   (%MAKE-POINTER-OFFSET DTP-FIX ARRAY
		     (+ NDIMS (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))))
	  SIZE (IF (PLUSP ELTS-PER-Q) (// (+ SIZE ELTS-PER-Q -1) ELTS-PER-Q)
		   (* SIZE (MINUS ELTS-PER-Q))))
    (RETURN-FROM DONE ARRAY START SIZE)))

(DEFUN PAGE-IN-ARRAY  (ARRAY &OPTIONAL FROM TO &AUX SIZE)
  (WITHOUT-INTERRUPTS
    (MULTIPLE-VALUE (ARRAY FROM SIZE)
      (PAGE-ARRAY-CALCULATE-BOUNDS ARRAY FROM TO))
    (AND ARRAY
	 ;; Have starting word and number of words.  Page dem words in.
	 (PAGE-IN-WORDS FROM SIZE))))

(DEFUN PAGE-OUT-ARRAY (ARRAY &OPTIONAL FROM TO &AUX SIZE)
  (WITHOUT-INTERRUPTS
    (MULTIPLE-VALUE (ARRAY FROM SIZE)
      (PAGE-ARRAY-CALCULATE-BOUNDS ARRAY FROM TO))
    (AND ARRAY
	 ;; Have starting word and number of words.  Page dem words out.
	 (PAGE-OUT-WORDS FROM SIZE))))

;;;*** Writes pages to disk even if they are not modified.
(DEFUN PAGE-OUT-WORDS (ADDRESS NWDS &AUX (CCWX 0) CCWP BASE-ADDR STS)
  (WITHOUT-INTERRUPTS
    (SETQ ADDRESS (%POINTER ADDRESS))
    (UNWIND-PROTECT
      (PROGN (WIRE-PAGE-RQB)
	     ;; This DO is over the whole frob
	     (DO ((ADDR (LOGAND (- PAGE-SIZE) ADDRESS) (%24-BIT-PLUS ADDR PAGE-SIZE))
		  (N (+ NWDS (LOGAND (1- PAGE-SIZE) ADDRESS)) (- N PAGE-SIZE)))
		 ((NOT (PLUSP N)))
	       (SETQ CCWX 0
		     CCWP %DISK-RQ-CCW-LIST
		     BASE-ADDR ADDR)
	       ;; This DO is over pages to go in a single I/O operation
	       ;; We wire them down and put their physical addresses into CCWs
	       (DO () (NIL)
		 (IF (OR (NULL (SETQ STS (%PAGE-STATUS ADDR)))	;Swapped out
			 ( STS %PHT-SWAP-STATUS-WIRED))	;Wired
		     (RETURN NIL))		;In either case, leave page alone
		 (SETQ CCWX (1+ CCWX))
		 (WIRE-PAGE ADDR)
		 (LET ((PADR (%PHYSICAL-ADDRESS ADDR)))
		   (ASET (1+ (LOGAND (- PAGE-SIZE) PADR))
			 PAGE-RQB CCWP)
		   (ASET (LSH PADR -16.) PAGE-RQB (1+ CCWP)))
		 (SETQ CCWP (+ 2 CCWP))
		 (OR (< CCWX PAGE-RQB-SIZE) (RETURN NIL))
		 (SETQ ADDR (%24-BIT-PLUS ADDR PAGE-SIZE)
		       N (- N PAGE-SIZE))
		 (OR (PLUSP N) (RETURN NIL)))
	       (COND ((PLUSP CCWX)	;We have something to do, run the I/O op
		      (ASET (LOGAND (AREF PAGE-RQB (- CCWP 2)) -2) ;Turn off chain bit
			    PAGE-RQB (- CCWP 2))
		      (DISK-RUN PAGE-RQB 0 (+ (LSH BASE-ADDR -8) PAGE-OFFSET)
				(AREF DISK-SECTORS-PER-TRACK-ARRAY 0)
				(AREF DISK-HEADS-PER-CYLINDER-ARRAY 0)
				%DISK-COMMAND-WRITE "write")
		      ;Make these pages not modified, flushable, and not wired
		      (DO ((I 0 (1+ I))
			   (ADDRESS BASE-ADDR (%24-BIT-PLUS ADDRESS PAGE-SIZE)))
			  ((= I CCWX))
			(DO ((PHTX (%COMPUTE-PAGE-HASH ADDRESS) (+ PHTX 2))
			     (PHT-LIMIT (SYSTEM-COMMUNICATION-AREA %SYS-COM-PAGE-TABLE-SIZE))
			     (PHT1))
			    (NIL)
			  (AND ( PHTX PHT-LIMIT) (SETQ PHTX (- PHTX PHT-LIMIT)))
			  (SETQ PHT1 (PAGE-TABLE-AREA PHTX))
			  (COND ((NOT (BIT-TEST 100 PHT1)) (RETURN NIL))	;Not found
				((= (LDB 1020 PHT1) (LDB 1020 ADDRESS))	;Address match
				 (STORE (PAGE-TABLE-AREA PHTX)
					(%LOGDPB 0 %%PHT1-MODIFIED-BIT PHT1))
				 (RETURN NIL))))
			(%CHANGE-PAGE-STATUS ADDRESS %PHT-SWAP-STATUS-FLUSHABLE
					     (LDB %%REGION-MAP-BITS
						  (REGION-BITS (%REGION-NUMBER ADDRESS)))))
		      (SETQ CCWX 0)))))
      ;; UNWIND-PROTECT forms
      (UNWIRE-PAGE-RQB)
      (DOTIMES (I CCWX)
	(UNWIRE-PAGE (+ (* I PAGE-SIZE) BASE-ADDR))))))

(DEFUN PAGE-IN-WORDS (ADDRESS NWDS &AUX (CCWX 0) CCWP BASE-ADDR)
  (WITHOUT-INTERRUPTS
    (SETQ ADDRESS (%POINTER ADDRESS))
    (UNWIND-PROTECT
      (PROGN (WIRE-PAGE-RQB)
	     ;; This DO is over the whole frob
	     (DO ((ADDR (LOGAND (- PAGE-SIZE) ADDRESS) (%24-BIT-PLUS ADDR PAGE-SIZE))
		  (N (+ NWDS (LOGAND (1- PAGE-SIZE) ADDRESS)) (- N PAGE-SIZE)))
		 ((NOT (PLUSP N)))
	       (SETQ CCWX 0
		     CCWP %DISK-RQ-CCW-LIST
		     BASE-ADDR ADDR)
	       ;; This DO is over pages to go in a single I/O operation.
	       ;; We collect some page frames to put them in, remembering the
	       ;; PFNs as CCWs.
	       (DO () (NIL)
		 (OR (EQ (%PAGE-STATUS ADDR) NIL) (RETURN NIL))
		 (LET ((PFN (%FINDCORE)))
		   (ASET (1+ (LSH PFN 8)) PAGE-RQB CCWP)
		   (ASET (LSH PFN -8) PAGE-RQB (1+ CCWP)))
		 (SETQ CCWX (1+ CCWX)
		       CCWP (+ 2 CCWP))
		 (OR (< CCWX PAGE-RQB-SIZE) (RETURN NIL))
		 (SETQ ADDR (%24-BIT-PLUS ADDR PAGE-SIZE)
		       N (- N PAGE-SIZE))
		 (OR (PLUSP N) (RETURN NIL)))
	       (COND ((PLUSP CCWX)	;We have something to do, run the I/O op
		      (ASET (LOGAND (AREF PAGE-RQB (- CCWP 2)) -2) ;Turn off chain bit
			    PAGE-RQB (- CCWP 2))
		      (DISK-RUN PAGE-RQB 0 (+ (LSH BASE-ADDR -8) PAGE-OFFSET)
				(AREF DISK-SECTORS-PER-TRACK-ARRAY 0)
				(AREF DISK-HEADS-PER-CYLINDER-ARRAY 0)
				%DISK-COMMAND-READ "read")
		      ;Make these pages in
		      (DO ((I 0 (1+ I))
			   (CCWP %DISK-RQ-CCW-LIST (+ 2 CCWP))
			   (VPN (LSH BASE-ADDR -8) (1+ VPN))
			   (PFN))
			  ((= I CCWX))
			(SETQ PFN (DPB (AREF PAGE-RQB (1+ CCWP))
				       1010 (LDB 1010 (AREF PAGE-RQB CCWP))))
			(OR (%PAGE-IN PFN VPN)
			    ;Page already got in somehow, free up the PFN
			    (%CREATE-PHYSICAL-PAGE (LSH PFN 8))))
		      (SETQ CCWX 0)))))
      ;; UNWIND-PROTECT forms
      (UNWIRE-PAGE-RQB)
;I guess it's better to lose some physical memory than to get two pages
;swapped into the same address, in the event that we bomb out.
;     (DO ((CCWP %DISK-RQ-CCW-LIST (+ CCWP 2))
;	   (N CCWX (1- N)))
;	  ((ZEROP N))
;	(%CREATE-PHYSICAL-PAGE (DPB (AREF PAGE-RQB (1+ CCWP))
;				    2006
;				    (AREF PAGE-RQB CCWP))))
      )))

;This is a test function.
(DEFUN READ-ALL-BLOCKS (&OPTIONAL (UNIT 0) &AUX RQB BUF BLOCKS-PER-TRACK N-CYLS N-HEADS)
  (SETQ UNIT (DECODE-UNIT-ARGUMENT UNIT "reading all"))
  (UNWIND-PROTECT
    (PROGN (UNWIND-PROTECT (PROGN (SETQ RQB (GET-DISK-RQB 1))
			   (DISK-READ RQB UNIT 0)	;Get label
			   (SETQ BUF (RQB-BUFFER RQB))
			   (SETQ N-CYLS (AREF BUF 4)
				 N-HEADS (AREF BUF 6)
				 BLOCKS-PER-TRACK (AREF BUF 8)))
		    (RETURN-DISK-RQB RQB))
	   (SETQ RQB NIL)
	   (UNWIND-PROTECT (PROGN (SETQ RQB (GET-DISK-RQB BLOCKS-PER-TRACK))
				  (DO CYL 0 (1+ CYL) (= CYL N-CYLS)
				      (DO HEAD 0 (1+ HEAD) (= HEAD N-HEADS)
					  (DISK-READ RQB UNIT (* (+ (* CYL N-HEADS) HEAD)
								 BLOCKS-PER-TRACK)))
				      (FORMAT T "~D " CYL)))
			   (RETURN-DISK-RQB RQB)))
    (DISPOSE-OF-UNIT UNIT)))

