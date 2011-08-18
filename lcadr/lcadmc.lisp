;;; CADR DIANGOSTIC PROGRAM MACRO DEFINITIONS AND DECLARATIONS		-*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; USE (INCLUDE ((LMCONS) CADMAC >))

(COMMENT FIXNUM AND SPECIAL DECLARATIONS)

(DECLARE (SPECIAL RAPC RASIR RAOBS RAREALOBUS RAABUS RAMBUS RANOOPF RASTS RASTAT 
                  RACMO RACME RADME RAPBE RAM1E RAM2E RAAME RAUSE RAMME RAFSE RAFDE 
                  RARGE RACSWE RARDRE RACIBE RAGO RASTOP RARDRO RAFDO RAOPCE
                  RAORG RAFSO RAM2O RADMO RAM1O RAMD RAUBMO RAUBME 
                  RAPI RAPP RAUSP RAIR RAQ RALC RADC RAMOD RAOPCO RARSET
                  RARS RASTEP RASA RAAMO RAMMO RARCON RAPBO RAUSO))

(COMMENT MACROS)

;BUILD UP A WORD OUT OF A BUNCH OF FIELDS
(DEFUN BUILD MACRO (X)
  (DO ((X (CDR X) (CDDR X))
       (EXP 0))
      ((NULL X) EXP)
    (SETQ EXP `(LOGDPB ,(CADR X) ,(CAR X) ,EXP))))

;BUILD AND EXECUTE A MICRO INSTRUCTION.  WORKS HARD TO AVOID BIGNUM+NUMBER CONSING.
;ALSO RECOGNIZES COMPILE TIME CONSTANTS.  MOST MICRO INSTRUCTIONS EXECUTED BY
;THIS STUFF ARE COMPLETELY CONSTANT AT COMPILE TIME.
;FEATURES:
; (CC-EXECUTE FIELD VALUE FIELD VALUE ...)	;PUT IN IR, DON'T CLOCK
; (CC-EXECUTE (WRITE) FIELD VALUE ...)		;CLOCK IT
; (CC-EXECUTE (W-C-MEM ADR) FIELD VALUE ...)	;WRITE INTO C-MEM LOCATION ADR (BASH 1@A,0@M)
; (CC-EXECUTE (EXECUTOR <user's executor fctn>) FIELD VALUE ...) 
;note!! does not preserve order of evaluation!! Can evaluate non-constant arg more than
; once if field overlaps 16 bit boundary!!
(DEFUN CC-EXECUTE MACRO (X)
  (LET ((HIGH 0)		;BUILD INSTRUCTION WORD IN THREE PIECES
	(MIDDLE 0)
	(LOW 0)
	(FIELD NIL)
	(P NIL)
	(P+S NIL)
	(C-MEM-W-ADR NIL)
	(ARGUMENT NIL)
	(EXECUTOR NIL))
     ;DECODE OPERATION TYPE
     (SETQ EXECUTOR
	   (COND ((EQUAL (CADR X) '(WRITE))
		  (SETQ X (CDR X))
		  'CC-EXECUTE-W)
		 ((AND (NOT (ATOM (CADR X)))
		       (EQ (CAADR X) 'W-C-MEM))
		  (SETQ C-MEM-W-ADR (CADADR X) X (CDR X))
		  'CC-WRITE-C-MEM-3-16BIT-WORDS)
		 ((AND (NOT (ATOM (CADR X)))
		       (EQ (CAADR X) 'EXECUTOR))
		  (PROG1 (CADADR X) (SETQ X (CDR X))))
		 (T 'CC-EXECUTE-R)))
     ;FIRST PASS DOES ALL THE CONSTANT ONES
     (DO X (CDR X) (CDDR X) (NULL X)
       (SETQ FIELD (SYMEVAL (CAR X)) ARGUMENT (CADR X)
	     P (LSH FIELD -6) P+S (+ P (LOGAND 77 FIELD)))
       (COND ((OR (NUMBERP ARGUMENT)			;CONSTANT ARG, DO AT COMPILE TIME
		  (AND (SYMBOLP ARGUMENT) (GET ARGUMENT 'CONSTANT)))
	      (AND (SYMBOLP ARGUMENT) (SETQ ARGUMENT (SYMEVAL ARGUMENT)))
	      (AND (< P 20)	;OVERLAPS LOW WORD
		   (SETQ LOW (LOGDPB-INTO-FIXNUM ARGUMENT FIELD LOW)))
	      (AND (< P 40)
		   (>= P+S 20)	;OVERLAPS MIDDLE WORD
		   (SETQ MIDDLE
			 (COND ((>= P 20) (LOGDPB-INTO-FIXNUM ARGUMENT (- FIELD 2000) MIDDLE))
			       (T (LOGDPB-INTO-FIXNUM (LSH ARGUMENT (- P 20))
						      (- P+S 20)
						      MIDDLE)))))
	      (AND (>= P+S 40)	;OVERLAPS HIGH WORD
		   (SETQ HIGH
			 (COND ((>= P 40) (LOGDPB-INTO-FIXNUM ARGUMENT (- FIELD 4000) HIGH))
			       (T (LOGDPB-INTO-FIXNUM (LSH ARGUMENT (- P 40))
						      (- P+S 40)
						      HIGH))))))))
     ;SECOND PASS FILLS IN THE NON-CONSTANT ONES
     (DO X (CDR X) (CDDR X) (NULL X)
       (SETQ FIELD (SYMEVAL (CAR X)) ARGUMENT (CADR X)
	     P (LSH FIELD -6) P+S (+ P (LOGAND 77 FIELD)))
       (COND ((NOT (OR (NUMBERP ARGUMENT) (AND (SYMBOLP ARGUMENT) (GET ARGUMENT 'CONSTANT))))
	      (AND (< P 20)	;OVERLAPS LOW WORD
		   (SETQ LOW `(LOGDPB-INTO-FIXNUM ,ARGUMENT ,FIELD ,LOW)))
	      (AND (< P 40)
		   (>= P+S 20)	;OVERLAPS MIDDLE WORD
		   (SETQ MIDDLE
			 (COND ((>= P 20) `(LOGDPB-INTO-FIXNUM ,ARGUMENT ,(- FIELD 2000) ,MIDDLE))
			       (T `(LOGDPB-INTO-FIXNUM (LSH ,ARGUMENT ,(- P 20))
						       ,(- P+S 20)
						       ,MIDDLE)))))
	      (AND (>= P+S 40)	;OVERLAPS HIGH WORD
		   (SETQ HIGH
			 (COND ((>= P 40) `(LOGDPB-INTO-FIXNUM ,ARGUMENT ,(- FIELD 4000) ,HIGH))
			       (T `(LOGDPB-INTO-FIXNUM (LSH ,ARGUMENT ,(- P 40))
						       ,(- P+S 40)
						       ,HIGH))))))))
     (COND (C-MEM-W-ADR `(,EXECUTOR ,C-MEM-W-ADR ,HIGH ,MIDDLE ,LOW)) ;ARG ORDER DIFFERS
	   (T `(,EXECUTOR ,LOW ,MIDDLE ,HIGH)))))

(COMMENT CONS MICROINSTRUCTION FIELD DEFINITIONS)

;MAKE SYMBOLS AVAILABLE AT BOTH COMPILE TIME & RUN TIME
;AND DECLARE THEM SPECIAL

(DEFUN DEFINE-SYMBOL MACRO (X)
  (SETQ X
        (DO ((LIST (CADR X) (CDDR LIST))
             (SYM-LIST NIL))
            ((NULL LIST)
             `(PROGN 'COMPILE
                     (SETQ . ,(CADR X))
                     (SPECIAL . ,SYM-LIST)
                     (MAPC (FUNCTION (LAMBDA (X) (PUTPROP X T 'CONSTANT)))
                           ',SYM-LIST)) )
          (PUSH (CAR LIST) SYM-LIST)))
  (EVAL X)
  X)

(DEFINE-SYMBOL
  (
   ;IR FIELDS
   CONS-IR-OP 5302
     CONS-OP-ALU 0 ;ASSUMED 0 AND OMITTED IN MANY PLACES FOR BREVITY
     CONS-OP-JUMP 1
     CONS-OP-DISPATCH 2
     CONS-OP-BYTE 3
   CONS-IR-POPJ 5201
   CONS-IR-ILONG 5501
   CONS-IR-STAT-BIT 5601
   CONS-IR-SPARE-BIT 5701
   CONS-IR-A-SRC 4012
   CONS-IR-M-SRC 3206
    CONS-FUNC-SRC-INDICATOR 40 ;ADD IN FOR FUNCTIONAL SOURCES
    CONS-M-SRC-DISP-CONST 40
    CONS-M-SRC-MICRO-STACK 41 ;USP BITS 28-24, SPCn BITS 18-0
    CONS-M-SRC-PDL-BUFFER-POINTER 42
    CONS-M-SRC-PDL-BUFFER-INDEX 43
    CONS-M-SRC-C-PDL-BUFFER-INDEX 45
    CONS-M-SRC-OPC 46 
    CONS-M-SRC-Q 47
    CONS-M-SRC-VMA 50
    CONS-M-SRC-MAP 51		;ADDRESSED BY MD, NOT VMA
      CONS-MAP-LEVEL-1-BYTE 3005
      CONS-MAP-LEVEL-2-BYTE 0030
      CONS-MAP-PFR-BIT 1_30.
      CONS-MAP-PFW-BIT 1_31.
    CONS-M-SRC-MD 52
    CONS-M-SRC-LC 53 
    CONS-M-SRC-MICRO-STACK-POP 54 ;SAME AS MICRO-STACK, BUT ALSO POPS USP
      CONS-US-POINTER-BYTE 3005
      CONS-US-DATA-BYTE 0023
    CONS-M-SRC-C-PDL-BUFFER-POINTER-POP 64
    CONS-M-SRC-C-PDL-BUFFER-POINTER 65
   CONS-IR-A-MEM-DEST 1614
     CONS-A-MEM-DEST-INDICATOR 4000  ;ADD THIS TO A MEM ADDRESS
     CONS-A-MEM-DEST-1777 5777
   CONS-IR-M-MEM-DEST 1605
   CONS-IR-FUNC-DEST 2305
    CONS-FUNC-DEST-LC 1 
    CONS-FUNC-DEST-INT-CNTRL 2 
    CONS-FUNC-DEST-C-PP 10
    CONS-FUNC-DEST-PDL-BUFFER-PUSH 11
    CONS-FUNC-DEST-C-PI 12
    CONS-FUNC-DEST-PDL-BUFFER-INDEX 13
    CONS-FUNC-DEST-PDL-BUFFER-POINTER 14
    CONS-FUNC-DEST-MICRO-STACK-PUSH 15
    CONS-FUNC-DEST-OA-LOW 16
    CONS-FUNC-DEST-OA-HIGH 17
    CONS-FUNC-DEST-VMA 20
      CONS-VMA-LEVEL-1-BYTE 1513
      CONS-VMA-LEVEL-2-BYTE 1005
    CONS-FUNC-DEST-VMA-START-READ 21
    CONS-FUNC-DEST-VMA-START-WRITE 22
    CONS-FUNC-DEST-VMA-WRITE-MAP 23
      CONS-MAP-LEVEL-1-BYTE-FOR-WRITING 3305 ;NOT IDENTICAL TO CONS-MAP-LEVEL-1-BYTE
      CONS-VMA-WRITE-LEVEL-1-MAP-BIT 1_26.
      CONS-VMA-WRITE-LEVEL-2-MAP-BIT 1_25.
    CONS-FUNC-DEST-MD 30
    CONS-FUNC-DEST-MD-START-READ 31
    CONS-FUNC-DEST-MD-START-WRITE 32
    CONS-FUNC-DEST-MD-WRITE-MAP 33
   CONS-IR-OB 1402
    CONS-OB-MSK 0 ;DEPENDS ON THIS =0 FOR BREVITY
    CONS-OB-ALU 1
    CONS-OB-ALU-RIGHT-1 2
    CONS-OB-ALU-LEFT-1 3
  CONS-IR-MF 1202 ;MISCELLANEOUS FUNCTION
    CONS-MF-HALT 1
  CONS-IR-ALUF 0207  ;INCLUDING CARRY
    CONS-ALU-SETZ 0_1
    CONS-ALU-SETO 17_1
    CONS-ALU-SETA 5_1
    CONS-ALU-SETM 3_1
    CONS-ALU-SUB 55	      ;includes ALU-CARRY-IN-ONE
    CONS-ALU-ADD 31_1
    CONS-ALU-M+M 37_1
    CONS-ALU-M+M+1 77
    CONS-ALU-M+1 71
  CONS-IR-Q 0002
    CONS-Q-LEFT 1
    CONS-Q-RIGHT 2
    CONS-Q-LOAD 3
  CONS-IR-DISP-LPC 3101
  CONS-IR-DISP-ADVANCE-INSTRUCTION-STREAM 3001
  CONS-IR-DISP-CONST 4012
  CONS-IR-DISP-ADDR 1413
  CONS-IR-BYTL-1 0505
  CONS-IR-DISP-BYTL 0503
  CONS-IR-MROT 0005
  CONS-IR-JUMP-ADDR 1416
  CONS-IR-JUMP-COND 0007
    CONS-JUMP-COND-M<A 41
    CONS-JUMP-COND-M<=A 42
    CONS-JUMP-COND-M=A 43
    CONS-JUMP-COND-PAGE-FAULT 44
    CONS-JUMP-COND-PAGE-FAULT-OR-INTERRUPT 45
    CONS-JUMP-COND-PAGE-FAULT-OR-INTERRUPT-OR-SEQUENCE-BREAK 46
    CONS-JUMP-COND-UNC 47
    CONS-JUMP-COND-M>=A 141
    CONS-JUMP-COND-M>A 142
    CONS-JUMP-COND-M-NEQ-A 143
    CONS-JUMP-COND-NO-PAGE-FAULT 144
  CONS-IR-R 1101
  CONS-IR-P 1001
  CONS-IR-N 0701
  CONS-IR-BYTE-FUNC 1402
    CONS-BYTE-FUNC-LDB 1
    CONS-BYTE-FUNC-SELECTIVE-DEPOSIT 2
    CONS-BYTE-FUNC-DPB 3

  ;DISPATCH MEMORY BITS
  CONS-DISP-R-BIT 2001
  CONS-DISP-P-BIT 1701
  CONS-DISP-N-BIT 1601
  CONS-DISP-RPN-BITS 1603
  CONS-DISP-PARITY-BIT 2101
))
