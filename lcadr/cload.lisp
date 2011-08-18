;;-*- MODE: LISP; PACKAGE: CADR -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;NOTE!! This file has been converted on run on the LISPM, <<BUT>>
;;  this file MUST be maintained in MACLISP runnable form.
;;  Reason is, the conversion does not achieve "decoupling" between
;;  the environment loaded and the environment loading.  The cold loader will
;;  have to be completely rewritten to do this.  (Probably, it will load QFASL files
;;  directly onto a save-band, thus eliminating the need for two machines and
;;  the cold load file itself.)
;;  Anyway, the current conversion has as its sole purpose SOMETIMES avoiding
;;  bletcherous PDP-10 response.
;COLD-LOAD LOADER FOR REAL -*-LISP-*- MACHINE, CADR VERSION

;FUNCTIONS
;(0) DETERMINE HOW MUCH MAIN MEMORY IS PRESENT
;(1) GET THE COLD LOAD INTO THE MACHINE
;(2) ALLOCATE THE AREAS.  THIS KNOWS THE NAMES, ORDER, AND SIZES
;    OF THE "INITIAL" AREAS.
;(3) SET UP CERTAIN STUFF WHICH IS PECULIAR TO THE REAL MACHINE:
;    THE PAGE HASH TABLE
;    THE REGION-BITS FIELD
;    THE REGION-SORTED-BY-ORIGIN AREA
;    THE SYSTEM-COMMUNICATION-AREA
;      (THIS IS PARTLY SET UP IN THE COLD LOAD AND PARTLY HERE)

;WHAT THIS HAS TO DO IS EACH TIME AN AREA REACHES THE END OF A PAGE,
;ALLOCATE THE NEXT PHYSICAL PAGE TO THAT ADDRESS, STORE INTO THE
;PAGE HASH TABLE BASED ON THE PROPERTIES OF THAT AREA (INCLUDING
;SPECIAL TEMPORARY HAIR FOR THE LINEAR PDL AREA), AND CONTINUE.
;THE MAP IS NOW TOTALLY UNSTRAIGHT; NOTE: WIRED AREAS (AT LEAST THE
;FIRST FEW) BETTER BE LOADED "STRAIGHT".
;THERE SHOULD BE A PAGE MAP IN THE 10 WHICH IS AN ARRAY HAVING
;THE PHYSICAL PAGE NUMBER OR -1 IF NOT YET ALLOCATED.
;THIS THEN WOULD ALLOW THE AREAS TO BE SPACED OUT IN VIRTUAL MEMORY.

;NOTE THAT IN THIS PROGRAM EACH AREA CAN HAVE ONLY ONE REGION

;IF ANYTHING RELATED TO THIS STUFF IS CHANGED, THIS FILE
;WILL PROBABLY NEED TO BE CHANGED, OR AT LEAST CHECKED.

(INCLUDE |LMDOC;.COMPL PRELUD|);DEFINE DEFMACRO, `, LET, ETC.
(IF-FOR-MACLISP
  (INCLUDE |LMCONS;QFMAC >|))
(IF-FOR-LISPM
(DEFF LOGLDB* (FUNCTION LDB))
(DEFF LOGDPB* (FUNCTION DPB)) )

;Really wants to be a bignum LSH.  On LISPM, LSH doesnt win for bignums, ASH does.
; In MACLISP, LSH wins sufficiently.
(DEFMACRO CC-SHIFT (QUAN AMT)
  #`(#Q ASH #M LSH ,QUAN ,AMT))

(SETQ MAX-NUMBER-VIRTUAL-PAGES 20000)		;1024K
(SETQ SYS-COM-AREA-BASE 400)		;BASE OF SYSTEM-COMMUNICATION-AREA

(DECLARE (SPECIAL COLD-RESIDENT-SYMBOL-AREA COLD-SYSTEM-COMMUNICATION-AREA
		  COLD-SCRATCH-PAD-INIT-AREA COLD-MICRO-CODE-SYMBOL-AREA
		  COLD-PAGE-TABLE-AREA COLD-PHYSICAL-PAGE-DATA
		  COLD-REGION-ORIGIN COLD-REGION-LENGTH COLD-REGION-BITS
		  COLD-REGION-SORTED-BY-ORIGIN COLD-REGION-FREE-POINTER
		  COLD-REGION-GC-POINTER COLD-REGION-LIST-THREAD
		  COLD-AREA-NAME COLD-AREA-REGION-LIST COLD-AREA-REGION-SIZE
		  COLD-AREA-MAXIMUM-SIZE COLD-SUPPORT-ENTRY-VECTOR COLD-CONSTANTS-AREA
		  COLD-MICRO-CODE-ENTRY-AREA COLD-MICRO-CODE-ENTRY-NAME-AREA 
		  COLD-MICRO-CODE-ENTRY-ARGS-INFO-AREA
		  COLD-MICRO-CODE-ENTRY-MAX-PDL-USAGE COLD-MICRO-CODE-EXIT-AREA
		  CC-MAIN-MEMORY-SIZE TOTAL-/#-AREAS SIZE-OF-AREA-ARRAYS
		LIST-STRUCTURED-AREAS STATIC-AREAS
		DTP-FIX DTP-LOCATIVE DTP-U-ENTRY 
		RACMWD RACVMW NANPG ;SYMEVAL'ED
		AREA-LIST PDL-BUFFER-AREA-LIST WIRED-AREA-LIST
		READ-ONLY-AREA-LIST FSM-LINEAR-ADVANCING
		MAX-NUMBER-VIRTUAL-PAGES SIZE-OF-PAGE-TABLE
		%%PHT1-VIRTUAL-PAGE-NUMBER %%PHT1-SWAP-STATUS-CODE
		%PHT-SWAP-STATUS-NORMAL %PHT-SWAP-STATUS-FLUSHABLE
		%PHT-SWAP-STATUS-WIRED
		%%PHT1-VALID-BIT %%PHT2-ACCESS-STATUS-AND-META-BITS
		%%PHT2-PHYSICAL-PAGE-NUMBER
		%SYS-COM-FREE-AREA/#-LIST %SYS-COM-FREE-REGION/#-LIST
		%SYS-COM-MEMORY-SIZE %SYS-COM-WIRED-SIZE
		CTALK-BARF-AT-WRITE-ERRORS LAST-PHT-FREE-ENTRY
		SYS-COM-AREA-BASE 
))

(DECLARE (FIXNUM %PHT-SWAP-STATUS-NORMAL %PHT-SWAP-STATUS-FLUSHABLE  ;TO AVOID NCOMPL BARF
		 %PHT-SWAP-STATUS-WIRED
		 %%PHT1-VIRTUAL-PAGE-NUMBER %%PHT1-SWAP-STATUS-CODE
		 %%PHT1-VALID-BIT %%PHT2-ACCESS-STATUS-AND-META-BITS
		 %%PHT2-PHYSICAL-PAGE-NUMBER LAST-PHT-FREE-ENTRY))

(DECLARE (ARRAY* (FIXNUM FSP 1 LOC 1 LEN 1 LIM 1 PHYS-PAGE 1)))

(DECLARE (FIXNUM (CL-NIBBLE))
	 (FIXNUM (PHYS-MEM-READ FIXNUM))
	 (NOTYPE (PHYS-MEM-WRITE FIXNUM FIXNUM))
	 (FIXNUM (CL-MAP-ADR FIXNUM))
	 (NOTYPE (CL-CREATE-PAGE NOTYPE FIXNUM FIXNUM)))

(IF-FOR-MACLISP (ARRAY CL-NIBBLE-BUFFER FIXNUM 1))

(IF-FOR-MACLISP 
  (DECLARE (LOAD '((LISPM) UTIL FASL))  ;NEEDED FOR QCOM TO WIN
	   (VALRET '|:SL/
/P|)
	   (LOAD '((LISPM) UTIL1 FASL)) ;ALSO ...
	   (LOAD '((LISPM)QCOM >))))  ;GET DEFINITIONS FOR PHT1,2 ETC NEEDED AT COMPILE-TIME
				     ; FOR LOGLDB* LOGDPB*


(DECLARE (SPECIAL CL-NIBBLE-FILE))

(IF-FOR-MACLISP
(DEFUN CL-NIBBLE ()
  ((LAMBDA (TEM)
     (DECLARE (FIXNUM TEM))
     (COND ((< (SETQ TEM (CL-NIBBLE-BUFFER 0)) 0)
	    (SETQ TEM (IN CL-NIBBLE-FILE))
	    (STORE (CL-NIBBLE-BUFFER 0) (BOOLE 1 177777 (LSH TEM -4)))
	    (LSH TEM -24))
	   (T (STORE (CL-NIBBLE-BUFFER 0) -1)
	      TEM)))
   0))

(DEFUN CL-OPEN (FILE)
  (SETQ CL-NIBBLE-FILE (OPEN FILE '(IN FIXNUM BLOCK)))
  (STORE (CL-NIBBLE-BUFFER 0) -1)
  (OR (= (IN CL-NIBBLE-FILE) -30387857152.)
      (ERROR '|FILE TYPE NOT CLOAD| FILE 'FAIL-ACT))
  NIL)
)

(IF-FOR-LISPM
(DEFUN CL-NIBBLE ()
  (FUNCALL CL-NIBBLE-FILE ':TYI))

(DEFUN CL-OPEN (FILE)
  (SETQ CL-NIBBLE-FILE (OPEN FILE '(IN FIXNUM BLOCK)))
  (OR (AND (= (CL-NIBBLE) 36555.)
	   (= (CL-NIBBLE) 57744.))
      (ERROR '|FILE TYPE NOT CLOAD| FILE 'FAIL-ACT))
  NIL)
)

(DEFUN CL-CLOSE ()
  (CLOSE CL-NIBBLE-FILE)
  (SETQ CL-NIBBLE-FILE NIL))

(DEFMACRO QPUT (ADR DATA)	;THIS VERSION WRITES PHYSICAL MEM THROUGH MEMTERFACE
  `(PHYS-MEM-WRITE (CL-MAP-ADR ,ADR) ,DATA))

(DEFMACRO QGET (ADR)
  `(PHYS-MEM-READ (CL-MAP-ADR ,ADR)))

(DEFUN CC-CLEAR-CORE (SIZE)
  (DO I 0 (1+ I) (= I SIZE)
    (DECLARE (FIXNUM I))
    (COND ((OR (< I 1400) (> I 2377)) ;KLUDGILY AVOID CLEARING MICROCODE-SYMBOL-AREA
	   (PHYS-MEM-WRITE I 0)))))

(DECLARE (FIXNUM I))

(DEFUN LOAD-COLD ()
  (PROG (NEXT-FREE-VIR-PAGE NEXT-FREE-PHY-PAGE NEXT-AREA-NUMBER INITIAL-AREAS
	 QS-PER-PAGE NANPG NAREAS NMDTP #M CDR-NEXT #M CDR-NIL QNIL
	 TEM SAR RAR OP ADR)

    (DECLARE (FIXNUM NEXT-FREE-VIR-PAGE NEXT-FREE-PHY-PAGE NEXT-AREA-NUMBER QS-PER-PAGE
		     NANPG NAREAS NMDTP CDR-NEXT CDR-NIL QNIL
		     TEM SAR RAR OP ADR))

#Q  (COND ((NOT (BOUNDP 'PDL-BUFFER-AREA-LIST))
	   (FORMAT T 
"~%Reading in QCOM, this will screw you if incompatible changes have been made!")
	   (READFILE "LISPM;QCOM")))  ;Necessary for PDL-BUFFER-AREA-LIST
    (QF-CLEAR-CACHE T)	;EVERYTHING YOU KNOW IS WRONG
    (SETQ QS-PER-PAGE 400)
    (CC-DISCOVER-MAIN-MEMORY-SIZE)
    (READ-LABEL)
    (SETQ NANPG 1)	;NUMBER OF PAGES IN EACH PER-AREA AREA
    (SETQ NAREAS (1- (* NANPG QS-PER-PAGE))) ;MAX # AREAS
    (OR (= NAREAS SIZE-OF-AREA-ARRAYS) (BREAK SOMETHING-NOT-KOSHER-WITH-SIZE-OF-AREA-ARRAYS))
#M  (SETQ CDR-NEXT 3 CDR-NIL 2)
    (SETQ NMDTP (+ (CC-SHIFT DTP-FIX 24.) (CC-SHIFT CDR-NEXT 30.)))
    (SETQ QNIL (CC-SHIFT DTP-SYMBOL 24.))
    (SETQ NEXT-FREE-VIR-PAGE 0 NEXT-AREA-NUMBER 0)

    (SETQ INITIAL-AREAS '(	;NAME WHOSE VALUE WILL BE SET TO ADDRESS OF AREA,
			; AND WHOSE COLD-AREA-LENGTH, AREA-NUMBER PROPERTIES WILL GET SET,
			;AND S-EXP FOR NUMBER OF PAGES TO ASSIGN TO AREA
	  		;INITIAL-AREAS ARE STRAIGHT-MAPPED!
	COLD-RESIDENT-SYMBOL-AREA 1		;HIGHLY USED ATOMS
	COLD-SYSTEM-COMMUNICATION-AREA 1
	COLD-SCRATCH-PAD-INIT-AREA 1
	COLD-MICRO-CODE-SYMBOL-AREA 2	;FIRST 600 LOCNS HAVE UCODE LOCNS TO XFER
					; TO ON MISC-INSTS 200-600
	COLD-PAGE-TABLE-AREA 20 		;PAGE HASH TABLE	******* 256K
	COLD-PHYSICAL-PAGE-DATA 4      		;			******* 256K
    ;FOLLOWING HAVE ONE Q FOR EACH REGION OR AREA IN THE WORLD
	COLD-REGION-ORIGIN	NANPG		;ADDRESS OF START OF AREA
	COLD-REGION-LENGTH	NANPG		;# OF QS IN AREA
	COLD-REGION-BITS	NANPG		;FREE STG MODE, PAGE MAP BITS, ETC.
	COLD-REGION-SORTED-BY-ORIGIN NANPG
    ;AREAS BEFORE THIS ARE WIRED.  CODE KNOWS THAT REGION-FREE-POINTER IS FIRST NON-WIRED AREA.
	COLD-REGION-FREE-POINTER NANPG
	COLD-REGION-GC-POINTER NANPG
	COLD-REGION-LIST-THREAD NANPG
	COLD-AREA-NAME	NANPG		;AREA NAME STRING
	COLD-AREA-REGION-LIST NANPG
	COLD-AREA-REGION-SIZE NANPG
	COLD-AREA-MAXIMUM-SIZE NANPG
	COLD-FREE-AREA 0			;ALTERED LATER TO HAVE REST OF PAGE PARTITION
	COLD-SUPPORT-ENTRY-VECTOR 1	;HOLDS Q QUANTITIES NEEDED BY UCONS ASSEMBLED
					; UCODE. ORDER IS IMPORTANT.  THE QUANTITIES
					; AND ORDER ARE BOTH SPECIFIED IN THE
					; SUPPORT-VECTOR-CONTENTS LIST IN QCOM,
					; AND THIS LIST HAD BETTER BE THE SAME WHEN
					; WHEN THE UCODE IS ASSEMBLED AS WHEN THE
					; COLD LOAD IS MADE.
	COLD-CONSTANTS-AREA 1		;"CONSTANTS PAGE"
   ;OTHER AREAS
	COLD-EXTRA-PDL-AREA 10
	COLD-MICRO-CODE-ENTRY-AREA 1
	COLD-MICRO-CODE-ENTRY-NAME-AREA 1
	COLD-MICRO-CODE-ENTRY-ARGS-INFO-AREA 1
	COLD-MICRO-CODE-ENTRY-MAX-PDL-USAGE 1
	COLD-MICRO-CODE-EXIT-AREA 0
    ))

	;SET UP INITIAL AREAS
    (DO ((X INITIAL-AREAS (CDDR X))
	 (AN 0 (1+ AN)))
	((NULL X))
      (LET ((COLD-AREA (CAR X))			;FUNNY SYMBOL USED BY COLD LOAD
	    (NUMBER-OF-PAGES (EVAL (CADR X)))
	    (AREA-SYMBOL (NTH AN AREA-LIST)))	;REGULAR AREA SYMBOL
	(PUTPROP AREA-SYMBOL
		 (SET COLD-AREA (* NEXT-FREE-VIR-PAGE QS-PER-PAGE))
		 'COLD-AREA-ORIGIN)
	(PUTPROP AREA-SYMBOL
		 (PUTPROP COLD-AREA (* NUMBER-OF-PAGES QS-PER-PAGE) 'COLD-AREA-LENGTH)
		 'COLD-AREA-LENGTH)
	(SETQ NEXT-FREE-VIR-PAGE (+ NEXT-FREE-VIR-PAGE NUMBER-OF-PAGES))
	(PUTPROP COLD-AREA NEXT-AREA-NUMBER 'AREA-NUMBER)
	(SETQ NEXT-AREA-NUMBER (1+ NEXT-AREA-NUMBER))))
    (SETQ TOTAL-/#-AREAS NEXT-AREA-NUMBER)

	;INITIALIZE PAGING STUFF
    (CL-INIT-PAGE-HASH-TABLE)

	;ALLOCATE STORAGE TO INITIAL AREAS
    (DO ((X INITIAL-AREAS (CDDR X))
	 (AN 0 (1+ AN))
	 (VIRADR 0))
	((NULL X))
      (LET ((AREA (CAR X))
	    (NUMBER-OF-PAGES (EVAL (CADR X))))
	(DO I 0 (1+ I) (= I NUMBER-OF-PAGES)
	    (CL-CREATE-PAGE (NTH AN AREA-LIST) (GET AREA 'AREA-NUMBER) VIRADR VIRADR)
	    (SETQ VIRADR (1+ VIRADR)))
	    ))

    (SETQ NEXT-FREE-PHY-PAGE NEXT-FREE-VIR-PAGE)
    (ARRAY FSP FIXNUM NAREAS)	;COPY OF FREE-SPACE POINTERS
    (ARRAY LOC FIXNUM NAREAS)	;COPY OF AREA-ORIGIN ARRAY
    (ARRAY LEN FIXNUM NAREAS)	;COPY OF AREA-LENGTH ARRAY
    (ARRAY LIM FIXNUM NAREAS)	;HIGHEST RELATIVE LOC PAGE CREATED FOR
    (FILLARRAY 'LOC '(0))
    (FILLARRAY 'LEN '(0))
    (FILLARRAY 'FSP '(0))
    (FILLARRAY 'LIM '(0))

	;FILL VARIOUS AREAS WITH DEFAULT STUFF
    (DO I 0 (1+ I) (> I NAREAS)
      (QPUT (+ I COLD-AREA-NAME) QNIL)
      (QPUT (+ I COLD-AREA-REGION-SIZE) (+ NMDTP 40000))
      (QPUT (+ I COLD-AREA-MAXIMUM-SIZE) (+ NMDTP (1- 1_23.))) ;MAX POSITIVE NUMBER
      (QPUT (+ I COLD-REGION-ORIGIN) (+ NMDTP 0))  ;So good type in free region#'s
      (QPUT (+ I COLD-REGION-LENGTH) (+ NMDTP 0))  ;..
      (QPUT (+ I COLD-REGION-FREE-POINTER) (+ NMDTP 0))
      (QPUT (+ I COLD-REGION-GC-POINTER) (+ NMDTP 0))
      (QPUT (+ I COLD-REGION-BITS) (+ NMDTP 0))) ;Suitable for free region

	;SET UP CONTENTS OF INITIAL AREAS
    (DO ((I 0 (1+ I))
	 (AL AREA-LIST (CDR AL))
	 (FIXED-P T))
	((NULL AL))
      (AND (EQ (CAR AL) 'WORKING-STORAGE-AREA) (SETQ FIXED-P NIL))
      (QPUT (+ I COLD-AREA-REGION-LIST) (+ NMDTP I))
      (QPUT (+ I COLD-REGION-LIST-THREAD) (+ NMDTP I 1_23.))
      (QPUT (+ I COLD-REGION-BITS)
	    (+ NMDTP
	       (LOGDPB* 
		 (COND ((MEMQ (CAR AL) READ-ONLY-AREA-LIST) 1200)	;RO
		       ((MEMQ (CAR AL) WIRED-AREA-LIST) 1400)	;RW
		       ((MEMQ (CAR AL) PDL-BUFFER-AREA-LIST)
			500)				;MAY BE IN PDL-BUFFER, NO ACCESS.
		       (T 1300))				;RWF
		 %%REGION-MAP-BITS
		 0)
	       (LOGDPB* 1 %%REGION-OLDSPACE-META-BIT 0)
	       (COND ((EQ (CAR AL) 'EXTRA-PDL-AREA)
		      (LOGDPB* 0 %%REGION-EXTRA-PDL-META-BIT 0))
		     ((LOGDPB* 1 %%REGION-EXTRA-PDL-META-BIT 0)))
	       (LOGDPB* (COND ((MEMQ (CAR AL) LIST-STRUCTURED-AREAS) 0) (T 1))
			%%REGION-REPRESENTATION-TYPE 0)
	       (LOGDPB* 0 %%REGION-COMPACT-CONS-FLAG 0) ;MUST BE 0 IN ALL COLD-LOAD AREAS
	       (LOGDPB* (COND ((EQ (CAR AL) 'EXTRA-PDL-AREA) %REGION-SPACE-EXTRA-PDL)
			      (FIXED-P %REGION-SPACE-FIXED)
			      ((MEMQ (CAR AL) STATIC-AREAS) %REGION-SPACE-STATIC)
			      (T %REGION-SPACE-NEW))
			%%REGION-SPACE-TYPE 0)
	       ))
      (LET ((ORG (COND ((GET (CAR AL) 'COLD-AREA-ORIGIN))
		       (T 0))))
	(QPUT (+ I COLD-REGION-ORIGIN) (+ NMDTP ORG)) 
	(STORE (LOC I) ORG))
      (STORE (LIM I) (STORE (LEN I)
			    (COND ((GET (CAR AL) 'COLD-AREA-LENGTH))
				  (T 0))))
      (QPUT (+ I COLD-REGION-LENGTH)
	    (+ NMDTP (COND ((GET (CAR AL) 'COLD-AREA-LENGTH))  ;"NON INITIAL" AREAS WONT
			   (T 0)))))		;HAVE THIS PROPERTY, WILL "CREATE" THEM LATER
    (CL-OPEN '((LISPM1)CADR CLOAD))
    (PRINT 'BEGIN-COLD-LOAD-FILE)

	;PROCESS NEXT FROB IN COLD LOAD FILE
L   (SETQ TEM (CL-NIBBLE))
    (SETQ OP (LOGLDB* 1404 TEM))		;OP CODE
    (COND ((= OP 1) (GO EOF))
	  ((= OP 2) (GO RDI))
	  ((= OP 3) (GO HWDS))
	  ((= OP 4) (GO INITA))		;CREATE AREA
	  (T (ERROR '|RANDOM NIBBLE IN COLDLOAD FILE| TEM 'FAIL-ACT)))
RDI (SETQ SAR (LOGLDB* 0006 TEM)	;AREA TO STORE INTO
	  RAR (LOGLDB* 0606 TEM))	;AREA TO RELOCATE BY
    (SETQ TEM (+ (CL-NIBBLE) (CC-SHIFT (CL-NIBBLE) 16.))) ;DATA Q
    (SETQ ADR (FSP SAR))		;STORE IN NEXT LOC IN SAR
    (AND (> ADR (LEN SAR)) (PRINT SAR) (BREAK AREA-FULL T))
CPG (COND ((NOT (< ADR (LIM SAR)))
	   (CL-CREATE-PAGE (NTH SAR AREA-LIST)
			   SAR
			   (// (+ ADR (LOC SAR)) QS-PER-PAGE)
			   NEXT-FREE-PHY-PAGE)
	   (SETQ NEXT-FREE-PHY-PAGE (1+ NEXT-FREE-PHY-PAGE))
	   (STORE (LIM SAR) (+ QS-PER-PAGE (LIM SAR)))
	   (GO CPG)))
    (STORE (FSP SAR) (1+ ADR))    
    (SETQ ADR (+ ADR (LOC SAR)))
    (SETQ TEM (+ TEM (LOC RAR)))
    (QPUT ADR TEM)
    (GO L)

HWDS
    (SETQ SAR (LOGLDB* 0006 TEM))	;AREA TO STORE INTO
    (SETQ ADR (FSP SAR))		;STORE IN NEXT LOCS IN SAR
    (SETQ TEM (+ ADR (CL-NIBBLE)))	;END ADDRESS FOR TRANSFER
    (AND (> TEM (LEN SAR)) (PRINT SAR) (BREAK AREA-FULL T))
HCP (COND ((> TEM (LIM SAR))
	   (CL-CREATE-PAGE (NTH SAR AREA-LIST)
			   SAR
			   (// (+ (LIM SAR) (LOC SAR)) QS-PER-PAGE)
			   NEXT-FREE-PHY-PAGE)
	   (SETQ NEXT-FREE-PHY-PAGE (1+ NEXT-FREE-PHY-PAGE))
	   (STORE (LIM SAR) (+ QS-PER-PAGE (LIM SAR)))
	   (GO HCP)))
    (STORE (FSP SAR) TEM)
    (SETQ ADR (+ ADR (LOC SAR)))
    (SETQ TEM (+ TEM (LOC SAR)))	;NOW STORE HALFWORDS FROM ADR TO TEM
CPS (COND ((= ADR TEM) (GO L)))
    (QPUT ADR (+ (CL-NIBBLE) (CC-SHIFT (CL-NIBBLE) 16.)))
    (SETQ ADR (1+ ADR))
    (GO CPS)

INITA 
    (SETQ SAR (LOGLDB* 0006 TEM)	;AREA NUMBER TO MAKE
	  RAR (CL-NIBBLE))		;NUMBER OF PAGES
    (AND (< SAR NEXT-AREA-NUMBER)
	 (ERROR SAR 'REDEFINING-INITIAL-AREA 'FAIL-ACT))
    (OR (< SAR (LENGTH AREA-LIST))
	(ERROR SAR 'NOT-IN-AREA-LIST 'FAIL-ACT))
    (SETQ TOTAL-/#-AREAS (MAX TOTAL-/#-AREAS (1+ SAR)))
    (STORE (LOC SAR) (SETQ TEM (* NEXT-FREE-VIR-PAGE QS-PER-PAGE)))
    (QPUT (+ SAR COLD-REGION-ORIGIN) (+ NMDTP TEM))
    (SETQ NEXT-FREE-VIR-PAGE (+ NEXT-FREE-VIR-PAGE RAR))
    (SETQ RAR (* RAR QS-PER-PAGE))	  ;SIZE OF AREA TO MAKE
    (STORE (LEN SAR) RAR)
    (QPUT (+ SAR COLD-REGION-LENGTH) (+ NMDTP RAR))
    (GO L)

EOF (PRINT 'END-COLD-LOAD-FILE)
    (CL-CLOSE)
    (PRIN1 NEXT-FREE-PHY-PAGE)
    (PRINC '//)
    (PRIN1 (// CC-MAIN-MEMORY-SIZE QS-PER-PAGE))
    (PRINC '| PHYSICAL CORE PAGES USED|)
    (PHYS-MEM-WRITE (+ SYS-COM-AREA-BASE %SYS-COM-MEMORY-SIZE) ;TELL MACHINE HOW MUCH
		    (+ NMDTP CC-MAIN-MEMORY-SIZE))      ; MEMORY TO USE
    (PHYS-MEM-WRITE (+ SYS-COM-AREA-BASE %SYS-COM-WIRED-SIZE)	;AND HOW MUCH IS WIRED DOWN
		    (+ NMDTP COLD-REGION-FREE-POINTER))	      ;*** FIRST UNWIRED AREA ***
    (PRINT (// COLD-REGION-FREE-POINTER QS-PER-PAGE))
    (PRINC '|WIRED DOWN PAGES|)
    ;; Transfer over free pointers
    (DO ((I 0 (1+ I))
	 (AL AREA-LIST (CDR AL))
	 (TEM))
	((= I NAREAS))
      (SETQ TEM (+ NMDTP (FSP I)))
      (QPUT (+ I COLD-REGION-FREE-POINTER) TEM)
      (QPUT (+ I COLD-REGION-GC-POINTER) TEM))
    ;; Set up FREE-AREA, and AREA# and REGION# free lists.
    (LET ((I (FIND-POSITION-IN-LIST 'FREE-AREA AREA-LIST))
	  (FREE-START (* NEXT-FREE-VIR-PAGE QS-PER-PAGE))
	  (FREE-END (* (CDR (GET-PARTITION-START-AND-SIZE 'PAGE))
		       QS-PER-PAGE)))
       (DECLARE (FIXNUM FREE-START FREE-END))
       (OR (ZEROP (LEN I)) (BREAK FREE-AREA-SCREWED))
       (QPUT (+ I COLD-REGION-ORIGIN) (+ NMDTP FREE-START))
       (QPUT (+ I COLD-REGION-LENGTH) (+ NMDTP (- FREE-END FREE-START)))
       (QPUT (+ I COLD-REGION-BITS) (+ NMDTP 0))) ;FREE

    (PHYS-MEM-WRITE (+ SYS-COM-AREA-BASE %SYS-COM-FREE-AREA/#-LIST)
		    (+ NMDTP TOTAL-/#-AREAS))
    (PHYS-MEM-WRITE (+ SYS-COM-AREA-BASE %SYS-COM-FREE-REGION/#-LIST)
		    (+ NMDTP TOTAL-/#-AREAS))
    (DO I TOTAL-/#-AREAS (1+ I) (= I NAREAS)
      (QPUT (+ I COLD-REGION-LIST-THREAD) (+ NMDTP (1+ I)))
      (QPUT (+ I COLD-AREA-REGION-LIST) (+ NMDTP (1+ I))))
    (QPUT (+ NAREAS COLD-REGION-LIST-THREAD) (+ NMDTP 0))
    (QPUT (+ NAREAS COLD-AREA-REGION-LIST) (+ NMDTP 0))

    ;; Make certain areas look full
    (MAPC '(LAMBDA (AR)
	     (LET ((ARN (FIND-POSITION-IN-LIST AR AREA-LIST)))
	       (AND (NULL ARN) (ERROR '|AREA DOESN'T EXIST| AR 'FAIL-ACT))
	       (QPUT (+ COLD-REGION-FREE-POINTER ARN)
		     (+ NMDTP (LEN ARN)))))
	  '(REGION-ORIGIN REGION-LENGTH REGION-FREE-POINTER REGION-GC-POINTER
	    REGION-BITS REGION-LIST-THREAD AREA-NAME AREA-REGION-LIST
	    AREA-REGION-SIZE AREA-MAXIMUM-SIZE
	    LINEAR-PDL-AREA LINEAR-BIND-PDL-AREA))

    (DO ((PG NEXT-FREE-PHY-PAGE (1+ PG))		;MARK FREE CORE FREE
	 (LIM (// CC-MAIN-MEMORY-SIZE QS-PER-PAGE)))
	((= PG LIM))					;DIFF VIR ADRS SO SPREAD IN HSH TBL?
      (DECLARE (FIXNUM PG LIM))
      (CL-CREATE-PAGE '*FREE* 0 -1 PG))

    (INIT-REGION-SORTED-BY-ORIGIN)

    (PRINT 'CC-DISK-WRITE-OUT-CORE)
    (CC-DISK-WRITE-OUT-CORE 'PAGE)

    #M (*REARRAY 'FSP)
    #M (*REARRAY 'LOC)
    #M (*REARRAY 'LEN)
    #M (*REARRAY 'LIM)
    #M (*REARRAY 'PHYS-PAGE)
    (RETURN '(DONE NOW DO A WARM BOOT TO START IT UP))
))

;SET UP REGION-SORTED-BY-ORIGIN ENTRY.  SPECIAL NOTES:
;(1) ZERO LENGTH REGIONS HAVE TO COME BEFORE OTHER REGIONS AT THE SAME ADDRESS
;    [ALTERNATIVELY THEY COULD NOT APPEAR AT ALL]
(DEFUN INIT-REGION-SORTED-BY-ORIGIN ()
  (DO ((MUM (MAKE-SORTED-REGION-LIST)
	    (OR (CDR MUM) MUM)) ;REPLICATE LAST ENTRY
       (I 0 (1+ I)))
      ((> I SIZE-OF-AREA-ARRAYS))
    (QPUT (+ I COLD-REGION-SORTED-BY-ORIGIN)
	  (+ (CC-SHIFT DTP-FIX 24.)
	     (CDAR MUM)))))

(DEFUN MAKE-SORTED-REGION-LIST ()
  (SORT (DO ((I 0 (1+ I))
	     (AL AREA-LIST (CDR AL))
	     (L NIL))
	    ((= I TOTAL-/#-AREAS)
	     (NREVERSE L))
	  (OR (EQ (CAR AL) 'FREE-AREA)
	      (SETQ L (CONS (CONS (LOC I) I) L))))
	(FUNCTION (LAMBDA (X Y)
	   (COND ((= (CAR X) (CAR Y))		;ONE IS ZERO LENGTH, IT -MUST- GO FIRST
		  (COND ((= 0 (LEN (CDR X))) T)
			((= 0 (LEN (CDR Y))) NIL)
			((ERROR (LIST X Y)
				'|TWO AREAS AT SAME ADDRESS AND NEITHER ZERO-LENGTH - MAKE-SORTED-REGION-LIST|
				'FAIL-ACT))))
		 ((< (CAR X) (CAR Y))))))))

;THIS CREATES AN INITIALLY ZERO PAGE HASH TABLE IN THE MACHINE
;THE SPECIAL VARIABLE PAGE-TABLE-AREA HAS ITS ADDRESS AND THE
;SPECIAL VARIABLE SIZE-OF-PAGE-TABLE HAS ITS SIZE IN QS.
;ALSO THE PHYS-PAGE ARRAY (KEPT IN THE PDP10) IS INITIALIZED TO ALL INVALID
(DEFUN CL-INIT-PAGE-HASH-TABLE ()
  (SETQ LAST-PHT-FREE-ENTRY 0)
  (SETQ SIZE-OF-PAGE-TABLE (GET 'COLD-PAGE-TABLE-AREA 'COLD-AREA-LENGTH))
  (DO I (1- SIZE-OF-PAGE-TABLE) (1- I) (< I 0)
    (PHYS-MEM-WRITE (+ COLD-PAGE-TABLE-AREA I) (CC-SHIFT DTP-FIX 24.)))
  (ARRAY PHYS-PAGE FIXNUM MAX-NUMBER-VIRTUAL-PAGES)
  (FILLARRAY 'PHYS-PAGE '(-1)))

;CALL HERE TO SET UP A PAGE  Area must be "real" area name (not COLD-xxx)
(DEFUN CL-CREATE-PAGE (AREA AREA-NUMBER VIRPAGE PHYPAGE)
  (OR (< PHYPAGE (CC-SHIFT CC-MAIN-MEMORY-SIZE -8))
      (ERROR PHYPAGE '|PHYSICAL PAGE OUT OF BOUNDS - CL-CREATE-PAGE| 'FAIL-ACT))
  (OR (EQ AREA '*FREE*)
      (< VIRPAGE MAX-NUMBER-VIRTUAL-PAGES)
      (ERROR VIRPAGE '|VIRTUAL PAGE OUT OF BOUNDS - CL-CREATE-PAGE| 'FAIL-ACT))
  (OR (EQ AREA '*FREE*)
      (< (PHYS-PAGE VIRPAGE) 0)
      (ERROR VIRPAGE '|VIRTUAL PAGE ALREADY CREATED - CL-CREATE-PAGE| 'FAIL-ACT))
  (DO ((PHT-MASK (- SIZE-OF-PAGE-TABLE 2))
       (ACCESS-AND-STATUS-CODE
	  (COND ((MEMQ AREA PDL-BUFFER-AREA-LIST) 5)
		((EQ AREA '*FREE*) 12)			;SO NO SWAP OUT
		((MEMQ AREA READ-ONLY-AREA-LIST) 12)
		(T 14)))
       (META-BITS (COND ((MEMQ AREA LIST-STRUCTURED-AREAS) 60)
			((EQ AREA 'EXTRA-PDL-AREA) 44)
			(T 64))) ;SYMBOLIC?????
       (SWAP-STATUS-CODE 
	  (COND ;((MEMQ AREA PDL-BUFFER-AREA-LIST) %PHT-SWAP-STATUS-PDL-BUFFER)
		((EQ AREA '*FREE*) %PHT-SWAP-STATUS-FLUSHABLE)	;FREE CORE, NOT A REAL AREA
		((MEMQ AREA WIRED-AREA-LIST) %PHT-SWAP-STATUS-WIRED)
		(T %PHT-SWAP-STATUS-NORMAL)))
       (HASH (COND ((EQ AREA '*FREE*) LAST-PHT-FREE-ENTRY)
		   ((LOGXOR (CC-SHIFT VIRPAGE 2) (CC-SHIFT VIRPAGE -6))))
	     (+ HASH 2)))
      (NIL)
    (DECLARE (FIXNUM PHT-MASK ACCESS-AND-STATUS-CODE META-BITS SWAP-STATUS-CODE HASH))
    (COND ((= 0 (LOGLDB* %%PHT1-VALID-BIT
			 (PHYS-MEM-READ (+ COLD-PAGE-TABLE-AREA
					   (SETQ HASH (LOGAND HASH PHT-MASK))))))
	   (PHYS-MEM-WRITE (+ COLD-PAGE-TABLE-AREA HASH)
		   (LOGDPB* VIRPAGE %%PHT1-VIRTUAL-PAGE-NUMBER
			    (LOGDPB* SWAP-STATUS-CODE %%PHT1-SWAP-STATUS-CODE
				     (LOGDPB* 1 %%PHT1-VALID-BIT
					      (CC-SHIFT DTP-FIX 24.)))))
	   (PHYS-MEM-WRITE (+ COLD-PAGE-TABLE-AREA HASH 1)
		   (LOGDPB* ACCESS-AND-STATUS-CODE
			    %%PHT2-ACCESS-AND-STATUS-BITS
			    (LOGDPB* META-BITS %%PHT2-META-BITS 
				     (LOGDPB* PHYPAGE %%PHT2-PHYSICAL-PAGE-NUMBER
					      (CC-SHIFT DTP-FIX 24.)))))
	   (PHYS-MEM-WRITE (+ PHYPAGE COLD-PHYSICAL-PAGE-DATA)
			   (+ (CC-SHIFT (COND ((EQ AREA '*FREE*) 0)
					      (T AREA-NUMBER))
				  16.)
			      HASH))

	   (COND ((EQ AREA '*FREE*) (SETQ LAST-PHT-FREE-ENTRY HASH))
		 (T (STORE (PHYS-PAGE VIRPAGE) PHYPAGE)))
	   (RETURN T)))))

;CONVERT A VIRTUAL ADDRESS TO PHYSICAL
(DEFUN CL-MAP-ADR (VIRADR)
  ((LAMBDA (TEM)
     (DECLARE (FIXNUM TEM))
     (COND ((< TEM 0)
	    (ERROR VIRADR '|REFERENCE TO PAGE NOT YET CREATED - CL-MAP-ADR| 'FAIL-ACT))
	   (T (+ (CC-SHIFT TEM 8) (LOGAND VIRADR 377)))))
   (PHYS-PAGE (CC-SHIFT VIRADR -8))))

;DISCOVER THE AMOUNT OF MAIN MEMORY, SETQ THE VARIABLE CC-MAIN-MEMORY-SIZE
(IF-FOR-MACLISP  ;This function in LCADR;DMON for LISPM
(DEFUN CC-DISCOVER-MAIN-MEMORY-SIZE ()
  (SETQ CC-MAIN-MEMORY-SIZE
	(DO ((ADR 0 (+ ADR 40000))		;MEMORY COMES IN 16K CHUNKS
	     (CTALK-BARF-AT-WRITE-ERRORS NIL)	;WE'RE COUNTING ON ERRORS, DON'T COMPLAIN
	     (ZEROS 0)
	     (ONES 37777777777))
	    (())
	  (DECLARE (FIXNUM ADR ZEROS ONES))
	  (PHYS-MEM-WRITE ADR ZEROS)
	  (OR (= ZEROS (PHYS-MEM-READ ADR)) (RETURN ADR))
	  (PHYS-MEM-WRITE ADR ONES)
	  (OR (= ONES (PHYS-MEM-READ ADR)) (RETURN ADR))))
  (TERPRI)
  (PRINC '|Main memory |)
  ((LAMBDA (BASE *NOPOINT) (PRIN1 (// CC-MAIN-MEMORY-SIZE 2000)))
   10. T)
  (PRINC '|K|)) )

