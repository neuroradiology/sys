;LAP FOR FEFS				-*-LISP-*- 

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;READ LISPM;MACROS > IN BEFORE TRYING TO RUN THIS INTERPRETIVELY

;Available info on variables in function being lapped:
;ALLVARS is the list, reversed from what it was in the compiler,
; so that arguments come first, in order, and so that the order of appearance
; of the bound special variables matches their order in SPECVARS
; (except that a special may appear more than once in ALLVARS).
;SPECVARS is the list of names of all special variables, bound or free.
; These are in the order that their value cell pointers should go in the fef.
; Vars bound at function entry must come first, and duplicates among them
; must not be eliminated.  SPECVARS-BIND-COUNT is the number of them
; which are bound at function entry.
;FREEVARS is the list of all free variables.

;At the moment, to avoid having to change the compiler at the same time,
;  SPECVARS is generated by LAP out of ALLVARS and FREEVARS.

;This is the interface from the compiler to LAP:
;The format of ALLVARS is described in LISPM;QCDEFS.
;FREEVARS is just a list of all free variables.
;ALLVARS and FREEVARS are contained in the first element of a list
;which contains the full description of the code.
;The list describing the code, called QCMP-OUTPUT in the compiler,
;contains these things:
;
;(MFEF functionname specialflag allvars freevars)
;(CONSTRUCT-MACRO)		;This, if present, means that lap should cons MACRO
;				;onto the fef before outputting the definition.
;(QTAG S-V-BASE)		;This defines a symbol usable for referring to value cell ptrs
;(S-V-BLOCK)			;This outputs the value cell pointers.
;(QTAG DESC-LIST-ORG)		;This defines a symbol pointing at the start of the ADL.
;(A-D-L)			;This outputs the ADL.
;(A-D-L)			;For historic reasons, there can be extra of these.
;...				;They do nothing.
;(ENDLIST)			;This puts CDR-NIL in the last Q of the ADL.
;				;It is not actually necessary, now.
;(PARAM LLOCBLOCK n)		;This specifies the length of the function's local block
;(QTAG QUOTE-BASE)		;This defines a symbol usable for referring to quoted constants
				;pointers to which live in the FEF starting here.
;Lap pass 1 inserts things to define the quoted constants in the list here.
;(ENDLIST)			;Put CDR-NIL in last constant pointer.
;(BREAKOFFS ('(:INTERNAL fnname 0) '(:INTERNAL fnname 1) ...))
				;List quoted constants that ought to be
				;replaced by pointers to FEFs somehow.
				;On pass 2, each '(:internal ...) is rplaca'd
				;with the fef index of where the internal fef ptr will go.
				;The list structure is shared with the debugging-info
				;entry INTERNAL-FEF-OFFSETS; this is how that entry
				;gets the data it is supposed to have.
;(DEBUG-INFO debugging info)	;Optionally, specify the debugging information ALIST.
;				;The defined entry type now is (ARGLIST <arglist>), as in
;				;(DEBUG-INFO (ARGLIST (X &OPTIONAL Y)))
;				;Sets %%FEFHI-MS-DEBUG-INFO-PRESENT bit in the fef misc wd.
;PROGSA				;This identifies the start of the unboxed part of the FEF.
;macro instructions follow.
;(PARAM MXPDL n)		;This specifies the maximum stack frame size this function needs.

;A macro instruction has one of these formats:

;(BRANCH condition state pop-flag tag)
;  condition is ALWAYS, NILIND or ATOMIND.
;  state is which way the branch should go.  For ALWAYS, state should be NIL.
;  NILIND T means branch if NIL, whereas NILIND NIL means branch if not NIL.
;  pop-flag is T to mean pop one object off the pdl if the branch is not taken.

;(MOVE destination source)
;  destination is D-IGNORE (or 0), D-PDL, D-INDS, D-NEXT, D-LAST, D-NEXT-LIST, D-RETURN.
;  source is an operand address.
;  This format applies to all 2-operand instructions.
;(+ source)
;  This format applies to all non-destination instructions.
;(MISC destination name)
;  name is the name of the miscellaneous instruction, such as CADDDR.

;A source operand has one of these formats:

;(LOCBLOCK n)   address n relative to the local block on the stack.
;(ARG n)	address n relative to the argument block on the stack.
;(LPDL 77)	pop the stack and use the value popped.
;(SPECIAL sym)	the value cell of sym, actually relative to the
;		invisible pointer stored in the FEF.
;(SPECIAL n)	similar, except that the index in the list of special variables
;		is specified instead of the symbol name.  This number is the
;		offset of the invisible pointer in the FEF with respect to
;		the first such invisible pointer.
;(QUOTE-VECTOR <s-exp>)   s-exp  placed in quote vector of FEF, and operand ref's it.
;		s-exp should have one of these forms:
;		  (QUOTE object)            The object is stored in the FEF
;		  (FUNCTION symbol)	    A fwding ptr to the fn cell is stored
;		  (BREAKOFF-FUNCTION name)  The name is stored,
;					    but the offset of this q is put into
;					    the INTERNAL-FEF-OFFSETS debugging info item.
;					    When (:INTERNAL thisfn n) is defined,
;					    its definition replaces the name.

(DECLARE (SPECIAL ADR SYMPTR SYMTAB QLP-A-D-L-DONE ADL-ORIGIN A-D-L-NEEDED-P
  SPECVARS SPECVARS-BIND-COUNT LOW-HALF-Q BREAKOFF-FUNCTION-OFFSETS ;N-SVS
  MAX-ARGS MIN-ARGS SM-ARGS-NOT-EVALD REST-ARG HAIRY-INIT-FLAG S-V-BITMAP-ACTIVE 
  DATA-TYPE-CHECKING-FLAG LENGTH-OF-PROG PROG-ORG FCTN-NAME LAP-OUTPUT-AREA 
  BIND-CONS-AREA %FEF-HEADER-LENGTH FEF-NAME-PRESENT FEF-SV-BIT FEF-DES-DT 
  FEF-DES-EVALAGE FEF-ARG-SYNTAX FEF-INIT-OPTION LAP-MODE FASD-GROUP-LENGTH 
  %FEFH-NO-ADL %FEFH-FAST-ARG %FEFH-SV-BIND LAP-ADL-NOSTORE %FEFHI-SVM-ACTIVE
  LAP-NO-ADL LAP-LASTQ-MODIFIER FASL-OP-FRAME FASL-OP-STOREIN-FUNCTION-CELL 
  LAP-FASD-NIBBLE-COUNT QUOTE-LIST QUOTE-COUNT CONSTANTS-PAGE QFEFHI-FAST-ARG-OPT-OPERATIVE
  %ARG-DESC-FEF-QUOTE-HAIR %ARG-DESC-FEF-BIND-HAIR
  %ARG-DESC-QUOTED-REST %ARG-DESC-EVALED-REST 
  LAP-OUTPUT-BLOCK LAP-OUTPUT-BLOCK-LENGTH LAP-STORE-POINTER LAP-MACRO-FLAG
  %Q-FLAG-BIT %HEADER-TYPE-FEF %%HEADER-TYPE-FIELD ALLVARS FREEVARS
  %FEF-NAME-PRESENT %%FEFHI-MS-DEBUG-INFO-PRESENT QLP-DEBUG-INFO-PRESENT QCMP-OUTPUT)) 

(DECLARE (SPECIAL DTP-FEF-POINTER DTP-FIX DTP-SYMBOL DTP-LOCATIVE 
	DTP-EXTERNAL-VALUE-CELL-POINTER))

(DECLARE (SPECIAL FUNCTIONS-REFERENCED))  ;LIST OF ALL FUNCTIONS REFERENCED & NOT DEFINED

;LAP-MODE MAY BE QFASL, QFASL-NO-FDEFINE, REL, OR COMPILE-TO-CORE.
;FOR QFASL-NO-FDEFINE, RETURNS FASL-TABLE INDEX OF FEF
(DEFUN QLAPP (FCTN LAP-MODE)
  (PROG (SYMTAB ADR NBR SYMPTR QLP-A-D-L-DONE SPECVARS SPECVARS-BIND-COUNT LOW-HALF-Q
	 MAX-ARGS MIN-ARGS SM-ARGS-NOT-EVALD REST-ARG HAIRY-INIT-FLAG
	 DATA-TYPE-CHECKING-FLAG LENGTH-OF-PROG PROG-ORG FCTN-NAME 
	 LAP-OUTPUT-AREA TEM LAP-NO-ADL LAP-LASTQ-MODIFIER ADL-ORIGIN A-D-L-NEEDED-P
	 QUOTE-LIST QUOTE-COUNT S-V-BITMAP-ACTIVE ALLVARS FREEVARS QLP-DEBUG-INFO-PRESENT
	 LAP-OUTPUT-BLOCK LAP-OUTPUT-BLOCK-LENGTH LAP-STORE-POINTER LAP-MACRO-FLAG
	 BREAKOFF-FUNCTION-OFFSETS)
	(SETQ LAP-OUTPUT-AREA 'MACRO-COMPILED-PROGRAM)
	(SETQ MAX-ARGS (SETQ MIN-ARGS 0))
	(SETQ SYMTAB (LIST NIL))
	(SETQ QUOTE-COUNT 0)
	(SETQ ADR 0)
	(SETQ QLP-DEBUG-INFO-PRESENT 0)
	(SETQ ALLVARS (CADDDR (CAR QCMP-OUTPUT))
	      FREEVARS (CADDDR (CDAR QCMP-OUTPUT)))
	(SETQ SPECVARS (EXTRACT-SPECVARS))
	(SCAN-ARGS)
	(COMPUTE-A-D-L-NEEDED-P)
	(QLAP-PASS1 FCTN)
	(RPLACD SYMTAB (NREVERSE (CDR SYMTAB)))
	(SETQ QUOTE-LIST (NREVERSE QUOTE-LIST))	;JUST SO FIRST ONES WILL BE FIRST
	(SETQ TEM (LAP-SYMTAB-PLACE 'QUOTE-BASE))
	(LAP-SYMTAB-RELOC (CADDAR TEM)		;VALUE OF QUOTE-BASE
			  (* 2 (LENGTH QUOTE-LIST))
			  (CDR TEM))
	(SETQ NBR (QLAP-ADJUST-SYMTAB))	;NUMBER BRANCHES TAKING EXTRA WD
	(SETQ LENGTH-OF-PROG (+ ADR (+ NBR (* 2 (LENGTH QUOTE-LIST)))))
	(SETQ SYMPTR SYMTAB)
	(SETQ QUOTE-COUNT 0)
	(SETQ ADR 0)
	(SETQ ADL-ORIGIN (OR QLP-A-D-L-DONE 0))
	(SETQ QLP-A-D-L-DONE NIL)
	(QLAP-PASS2 FCTN)
	;Don't call FASD with the temporary area in effect
	(LET-IF QC-FILE-IN-PROGRESS ((DEFAULT-CONS-AREA QC-FILE-OLD-DEFAULT-CONS-AREA))
	  (COND ((OR LOW-HALF-Q 
		     (AND (OR (EQ LAP-MODE 'QFASL) (EQ LAP-MODE 'QFASL-NO-FDEFINE))
			  (NOT (= 0 (LOGAND ADR 1)))))
		 (LAP-OUTPUT-WORD 0)))
	  (COND ((EQ LAP-MODE 'QFASL)
		 (SETQ TEM (FASD-TABLE-ADD (NCONS NIL)))
		 (COND ((NOT (= 0 LAP-FASD-NIBBLE-COUNT))
			(BARF LAP-FASD-NIBBLE-COUNT 
			      'LAP-FASD-NIBBLE-COUNT 
			      'BARF)))
		  ;; If this function is supposed to be a macro,
		  ;; dump directions to cons MACRO onto the fef.
		 (COND (LAP-MACRO-FLAG
			 (FASD-START-GROUP T 1 FASL-OP-LIST)
			 (FASD-NIBBLE 2)
			 (FASD-CONSTANT 'MACRO)
			 (FASD-START-GROUP NIL 1 FASL-OP-INDEX)
			 (FASD-NIBBLE TEM)
			 (SETQ TEM (FASD-TABLE-ADD (NCONS NIL)))))
		 (FASD-STOREIN-FUNCTION-CELL FCTN-NAME TEM)
		 (FASD-FUNCTION-END)
		 (RETURN NIL))
		((EQ LAP-MODE 'QFASL-NO-FDEFINE)
		 (SETQ TEM (FASD-TABLE-ADD (NCONS NIL)))
		 (COND ((NOT (= 0 LAP-FASD-NIBBLE-COUNT))
			(BARF LAP-FASD-NIBBLE-COUNT 
			      'LAP-FASD-NIBBLE-COUNT 
			      'BARF)))
		  ;; If this function is supposed to be a macro,
		  ;; dump directions to cons MACRO onto the fef.
		 (COND (LAP-MACRO-FLAG
			 (FASD-START-GROUP T 1 FASL-OP-LIST)
			 (FASD-NIBBLE 2)
			 (FASD-CONSTANT 'MACRO)
			 (FASD-START-GROUP NIL 1 FASL-OP-INDEX)
			 (FASD-NIBBLE TEM)
			 (SETQ TEM (FASD-TABLE-ADD (NCONS NIL)))))
		 (RETURN TEM))
		((EQ LAP-MODE 'COMPILE-TO-CORE)
		 (FSET-CAREFULLY FCTN-NAME
				 (COND (LAP-MACRO-FLAG (CONS 'MACRO LAP-OUTPUT-BLOCK))
				       (T LAP-OUTPUT-BLOCK))))
		((EQ LAP-MODE 'REL)
		 (QFASL-REL:DUMP-LAP-FSET FCTN-NAME LAP-OUTPUT-BLOCK))
	     (T (FERROR NIL "~S is a bad lap mode" LAP-MODE)) ))
)) 

(DEFUN QLAP-PASS1 (PNTR)
  (PROG NIL 
     P1 (COND ((NULL PNTR) (RETURN NIL)))	;PASS 1
	(QLP1 (CAR PNTR))
	(SETQ PNTR (CDR PNTR))
	(GO P1) ))

(DEFUN QLAP-ADJUST-SYMTAB NIL 
  (PROG (T1 NBR)
	(SETQ NBR 0)
	(SETQ T1 SYMTAB)
   P2A  (COND ((NULL (CDR T1)) (RETURN NBR))	;FINALIZE SYM DEFS
	      ((EQ (CADADR T1) 'BRANCH) (GO P2B))
	      ((EQ (CADADR T1) 'TDEF) (GO P2C)))
   P2A1 (SETQ T1 (CDR T1))
	(GO P2A)
   P2B  (QLRLC (CADR T1) NBR)	;THIS IS ONLY ADR AT WHICH TO HACK THIS.
	(SETQ NBR (1+ NBR))	;DOESNT AFFECT VALUE OF EVENTUAL BRANCH
	(GO P2A1)
   P2C  (QLRLC (CADR T1) NBR)
	(GO P2A1) ))

(DEFUN QLAP-PASS2 (PNTR)
  (PROG NIL 
   P3A (COND ((NULL PNTR) (RETURN NIL))		;PASS 2
	     ((QLP2-Q (CAR PNTR)) (GO P3C)))	;XFER ON ADVANCE TO UNBOXED AREA
       (SETQ PNTR (CDR PNTR))
       (GO P3A)
   P3C (COND (LAP-LASTQ-MODIFIER (LAP-MODIFY-LASTQ LAP-LASTQ-MODIFIER)))
       (DO ((P PNTR (CDR P))) ((NULL P))
	 (QLP2-U (CAR P)))))

(DEFUN LAP-D-OUT (S-EXP)
  (LAP-Q-OUT NIL NIL NIL S-EXP))

;On pass 2, output a Q, specified by components.
;S-EXP is the contents of the Q.
;FLAG is %Q-FLAG-BIT, to turn on that bit, if desired.
;INVZ-P is non-NIL to modify the data type of the Q:
;  QZEVCP for an external value cell pointer, or
;  QZLOC for a locative.
;OFFSET is added to the Q.  It is useful for making pointers to
;  value cells or function cells of symbols.
(DEFUN LAP-Q-OUT (FLAG INVZ-P OFFSET S-EXP)
  (COND (LAP-LASTQ-MODIFIER (LAP-MODIFY-LASTQ LAP-LASTQ-MODIFIER)))
  (COND ((OR (EQ LAP-MODE 'QFASL) (EQ LAP-MODE 'QFASL-NO-FDEFINE))
	 ;Don't call FASD with the temporary area in effect
	 (LET-IF QC-FILE-IN-PROGRESS ((DEFAULT-CONS-AREA QC-FILE-OLD-DEFAULT-CONS-AREA))
	   (FASD-CONSTANT S-EXP)))
	((OR (EQ LAP-MODE 'COMPILE-TO-CORE) (EQ LAP-MODE 'REL))
	 (COND ((>= LAP-STORE-POINTER LAP-OUTPUT-BLOCK-LENGTH)
		(BARF S-EXP 'DOESNT-FIT-IN-ALLOCATED-BLOCK 'BARF)))
	 ;QC-TRANSLATE-FUNCTION may have consed some lists which end up here,
	 ;such as the function's debug info, in the temporary area even though
	 ;QC-FILE-LOAD-FLAG is set, so copy them out.
	 (IF (EQ (%AREA-NUMBER S-EXP) QCOMPILE-TEMPORARY-AREA)
	     (SETQ S-EXP (COPYTREE S-EXP)))
	 (%P-STORE-CONTENTS-OFFSET S-EXP LAP-OUTPUT-BLOCK LAP-STORE-POINTER)
	 (SETQ LAP-STORE-POINTER (1+ LAP-STORE-POINTER))))
  (SETQ LAP-LASTQ-MODIFIER 
	(+ 300					;NXTCDR
	   (+ (COND (FLAG 40) (T 0))
	      (+ (COND ((NULL INVZ-P) 0)
		       ((EQ INVZ-P 'QZEVCP) 20)
		       ((EQ INVZ-P 'QZLOC) 400)
		       (T (BARF INVZ-P 'LAP-Q-OUT 'BARF)))
		 (COND (OFFSET OFFSET) (T 0)) )))))

(DEFUN LAP-MODIFY-LASTQ (CODE)
  (COND ((OR (EQ LAP-MODE 'QFASL) (EQ LAP-MODE 'QFASL-NO-FDEFINE))
	 (LAP-FASD-NIBBLE CODE))
	(T
	 (LET ((OFFSET (LOGAND CODE 17))
	       (IDX (1- LAP-STORE-POINTER)))
	   (%P-DPB-OFFSET (LSH CODE -6) %%Q-CDR-CODE LAP-OUTPUT-BLOCK IDX)
	   (%P-DPB-OFFSET (LSH CODE -5) %%Q-FLAG-BIT LAP-OUTPUT-BLOCK IDX)
	   (COND ((NOT (ZEROP OFFSET))
		  (%P-STORE-CONTENTS-OFFSET
		    (%MAKE-POINTER-OFFSET
		      DTP-LOCATIVE
		      (%P-CONTENTS-OFFSET LAP-OUTPUT-BLOCK IDX)
		      OFFSET)
		    LAP-OUTPUT-BLOCK
		    IDX)))
	   (COND ((BIT-TEST 20 CODE)
		  (%P-DPB-OFFSET DTP-EXTERNAL-VALUE-CELL-POINTER %%Q-DATA-TYPE
				 LAP-OUTPUT-BLOCK IDX))
		 ((BIT-TEST 400 CODE)
		  (%P-DPB-OFFSET DTP-LOCATIVE %%Q-DATA-TYPE
				 LAP-OUTPUT-BLOCK IDX))) ))))

(DEFUN LAP-OUTPUT-WORD (WD)
  (COND ((OR (EQ LAP-MODE 'QFASL) (EQ LAP-MODE 'QFASL-NO-FDEFINE))
	 (LAP-FASD-NIBBLE WD))
	((NULL LOW-HALF-Q) (SETQ LOW-HALF-Q WD))
	(T
	 (COND ((>= LAP-STORE-POINTER LAP-OUTPUT-BLOCK-LENGTH)
		(BARF WD 'DOESNT-FIT-IN-ALLOCATED-BLOCK 'BARF)))
	 (%P-DPB-OFFSET WD %%Q-HIGH-HALF LAP-OUTPUT-BLOCK LAP-STORE-POINTER)
	 (%P-DPB-OFFSET LOW-HALF-Q %%Q-LOW-HALF LAP-OUTPUT-BLOCK LAP-STORE-POINTER)
	 (SETQ LOW-HALF-Q NIL)
	 (SETQ LAP-STORE-POINTER (1+ LAP-STORE-POINTER)))))

(DEFUN LAP-STORE-NXTNIL-CDR-CODE NIL 
  (SETQ LAP-LASTQ-MODIFIER (+ 200 (BOOLE 4 LAP-LASTQ-MODIFIER 300))))

(DEFUN LAP-HEADER (Q-LENGTH UNBOXED-LENGTH)
  (AND (> Q-LENGTH 400)
       (BARF (- Q-LENGTH 400) '|You have been screwed!/
Constants area of FEF is excessively long by| 'DATA))
  (COND ((OR (EQ LAP-MODE 'QFASL) (EQ LAP-MODE 'QFASL-NO-FDEFINE))
	 ;Don't call FASD with the temporary area in effect
	 (LET-IF QC-FILE-IN-PROGRESS ((DEFAULT-CONS-AREA QC-FILE-OLD-DEFAULT-CONS-AREA))
	   (IF (EQ LAP-MODE 'QFASL) (FASD-FUNCTION-HEADER FCTN-NAME))
	   (FASD-START-GROUP NIL 3 FASL-OP-FRAME)
	   (FASD-NIBBLE Q-LENGTH)
	   (FASD-NIBBLE UNBOXED-LENGTH)
	   (SETQ LAP-FASD-NIBBLE-COUNT 
		 (+ Q-LENGTH (* 2 UNBOXED-LENGTH)))
	   (FASD-NIBBLE LAP-FASD-NIBBLE-COUNT)
	   (SETQ FASD-GROUP-LENGTH LAP-FASD-NIBBLE-COUNT)))
	(T
	  (SETQ LAP-OUTPUT-BLOCK			;CREATE THE FEF
		(%ALLOCATE-AND-INITIALIZE
		  DTP-FEF-POINTER		;DATA TYPE OF RETURNED POINTER
		  DTP-HEADER			;HEADER (1ST WORD OF FEF)
		  (%LOGDPB %HEADER-TYPE-FEF %%HEADER-TYPE-FIELD 0)
		  (SETQ LAP-OUTPUT-BLOCK-LENGTH	;TOTAL SIZE Q (2ND WORD OF FEF)
			(+ Q-LENGTH UNBOXED-LENGTH))
		  (COND ((EQ LAP-MODE 'COMPILE-TO-CORE)	;AREA TO ALLOCATE IN
			  MACRO-COMPILED-PROGRAM)
			(T QFASL-REL:DUMP-TEMP-AREA))
		  LAP-OUTPUT-BLOCK-LENGTH))	;AMOUNT TO ALLOCATE
	  (SETQ LAP-STORE-POINTER 2)) ))	;1ST TWO WDS DONE EXCEPT REST OF HEADER
						;Q WILL BE FILLED IN LATER.

(DEFUN LAP-FASD-NIBBLE (N)
  ;Don't call FASD with the temporary area in effect
  (LET-IF QC-FILE-IN-PROGRESS ((DEFAULT-CONS-AREA QC-FILE-OLD-DEFAULT-CONS-AREA))
    (SETQ LAP-FASD-NIBBLE-COUNT (1- LAP-FASD-NIBBLE-COUNT))
    (FASD-NIBBLE N)))

(DEFUN LAP-ARGP (VARHOME)
  (MEMQ (VAR-KIND VARHOME) '(FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST FEF-ARG-AUX)))

;This function is called before pass 1 and duplicates some of the work done in
;pass 2 by LAP-MFEF, in order to determine whether the A-D-L will be required.
;This organization is somewhat poor...
(DEFUN COMPUTE-A-D-L-NEEDED-P ()
  (PROG (QFEFHI-FAST-ARG-OPT-OPERATIVE S-V-BITMAP-ACTIVE FA)
	(COMPUTE-S-V-MAP)			;Compute S-V-BITMAP-ACTIVE
	(SETQ FA (COMPUTE-FAST-OPT-Q))	;Compute QFEFHI-FAST-ARG-OPT-OPERATIVE
	(SETQ A-D-L-NEEDED-P
	      (OR (BIT-TEST %ARG-DESC-FEF-QUOTE-HAIR FA)	;Needed by interpreter
		  (NOT QFEFHI-FAST-ARG-OPT-OPERATIVE)	;Needed by microcode
		  (DOLIST (V ALLVARS)			;Needed for extra info on args
		    (OR (LAP-ARGP V) (RETURN NIL))	;(such as &functional)
		    (AND (VAR-MISC V) (RETURN T)))))))

;At the start of pass 2, when the MFEF pseudo is encountered,
;output the fixed header Qs of the fef.
(DEFUN LAP-MFEF (WD)
  (PROG (QFEFHI-IPC QFEFHI-FCTN-NAME QFEFHI-FAST-ARG-OPT QFEFHI-SV-BITMAP QFEFHI-MISC 
	 QFEFHI-STORAGE-LENGTH UNBOXED-ORG QFEFHI-FAST-ARG-OPT-OPERATIVE LOCAL-BLOCK-LENGTH)
     (SETQ QFEFHI-IPC (LIST 'HEADER-TYPE-FEF
			    (SETQ UNBOXED-ORG (QLEVAL 'PROGSA 'T))))
     (COND ((SPECIAL-BIND-NEEDED-P) (SETQ QFEFHI-IPC (CONS '%FEFH-SV-BIND QFEFHI-IPC))))
     (SETQ QFEFHI-FCTN-NAME (SETQ FCTN-NAME (CADR WD)))
     (SETQ QFEFHI-SV-BITMAP (COMPUTE-S-V-MAP))       ;DO THIS FIRST, SO S-V-BITMAP-ACTIVE
     (SETQ QFEFHI-FAST-ARG-OPT (COMPUTE-FAST-OPT-Q)) ; IS SET FOR COMPUTE-FAST-OPT-Q
     (COND (QFEFHI-FAST-ARG-OPT-OPERATIVE
	    (SETQ QFEFHI-IPC (CONS '%FEFH-FAST-ARG QFEFHI-IPC))))
     (OR A-D-L-NEEDED-P
	 (SETQ QFEFHI-IPC (CONS '%FEFH-NO-ADL QFEFHI-IPC)))
     (SETQ QFEFHI-MISC (+ (LSH ADL-ORIGIN 15.)
			  QLP-DEBUG-INFO-PRESENT
			  (+ (COND ((NOT (ZEROP ADL-ORIGIN))
                                     (LSH (QLEVAL 'DESC-LIST-ORG 'T) 7))
				    (T 0))
			     (SETQ LOCAL-BLOCK-LENGTH (QLEVAL 'LLOCBLOCK 'NIL)))))
     (AND (> LOCAL-BLOCK-LENGTH 100)
	  (BARF (- LOCAL-BLOCK-LENGTH 100) '|You have been screwed!/
Local-variables block exceeds maximum length by| 'DATA))
;    (SETQ QFEFHI-MISC1(+ (LSH (QLEVAL 'MXPDL 'NIL) 15.)
;			  0 
;			  0))
     (SETQ QFEFHI-STORAGE-LENGTH (LSH (1+ LENGTH-OF-PROG) -1))
     (SETQ ADR (+ ADR (* 2 %FEF-HEADER-LENGTH)))
     (LAP-HEADER (// UNBOXED-ORG 2)	;Q PART LENGTH
		 (- QFEFHI-STORAGE-LENGTH (// UNBOXED-ORG 2)))
						;UNBOXED PART LENGTH
     (COND ((OR (EQ LAP-MODE 'QFASL) (EQ LAP-MODE 'QFASL-NO-FDEFINE))
	     (SETQ PROG-ORG
		   (LAP-D-OUT (DPB %HEADER-TYPE-FEF %%HEADER-TYPE-FIELD
				   (LIST-SUM QFEFHI-IPC))))
	     (LAP-D-OUT QFEFHI-STORAGE-LENGTH))
	   (T
	    (%P-DPB (LIST-SUM QFEFHI-IPC) %%HEADER-REST-FIELD LAP-OUTPUT-BLOCK)))
	(LAP-D-OUT QFEFHI-FCTN-NAME)
	(LAP-D-OUT QFEFHI-FAST-ARG-OPT)
	(LAP-D-OUT QFEFHI-SV-BITMAP)
	(LAP-D-OUT QFEFHI-MISC)
	(LAP-STORE-NXTNIL-CDR-CODE)
))

;Looking at ALLVARS, compute these quantities:
;MIN-ARGS, the minimum number of args required by the function.
;MAX-ARGS, the maximum number of args accepted by the function, not including a rest arg.
;HAIRY-INIT-FLAG, T if any variable is initialized at function entry other than to NIL.
;SM-VARS-NOT-EVALD, T if any arguments are not evaluated.
(DEFUN SCAN-ARGS NIL
  (DO ((VS ALLVARS (CDR VS))) ((NULL VS))
    (SELECTQ (VAR-KIND (CAR VS))
      (FEF-ARG-REQ
	(SETQ MAX-ARGS (1+ MAX-ARGS))
	(SETQ MIN-ARGS (1+ MIN-ARGS)))
      (FEF-ARG-OPT
	(SETQ MAX-ARGS (1+ MAX-ARGS)))
      (FEF-ARG-REST
	(SETQ REST-ARG (CAR VS))))
    (SELECTQ (CAR (VAR-INIT (CAR VS)))
      ((FEF-INI-NONE FEF-INI-NIL))
      (FEF-INI-COMP-C
	(OR (EQ (VAR-KIND (CAR VS)) 'FEF-ARG-INTERNAL-AUX)
	    (SETQ HAIRY-INIT-FLAG T)))
      (OTHERWISE (SETQ HAIRY-INIT-FLAG T)))
    (AND (EQ (VAR-EVAL (CAR VS)) 'FEF-QT-QT)
	 (SETQ SM-ARGS-NOT-EVALD T))))

(DEFUN COMPUTE-FAST-OPT-Q NIL	;SETS SPEC VAR QFEFHI-FAST-ARG-OPT-OPERATIVE
  (SETQ QFEFHI-FAST-ARG-OPT-OPERATIVE NIL)	;ASSUME INOPERATIVE
  (OR HAIRY-INIT-FLAG	;CHECK REASONS NOT TO HAVE FAST OPT OPERATIVE
      DATA-TYPE-CHECKING-FLAG    ;FOR LINEAR ENTER
      (NULL S-V-BITMAP-ACTIVE)   ;MICRO-CODE DOESNT FEEL LIKE HANDLING THIS CASE..
      ; (GOING TO HAVE TO GRUBBLE THRU A-D-L ANYWAY, SO
      ; MIGHT AS WELL DO SLOW ENTER).
      (SETQ QFEFHI-FAST-ARG-OPT-OPERATIVE T))
  (+  (COND ((NULL REST-ARG) 0)
	    ((EQ (VAR-EVAL REST-ARG) 'FEF-QT-QT)
	     %ARG-DESC-QUOTED-REST)
	    (T %ARG-DESC-EVALED-REST))
      (+ (COND ((AND SM-ARGS-NOT-EVALD (> MAX-ARGS 0))	;IF QUOTED REG ARGS,
		%ARG-DESC-FEF-QUOTE-HAIR)	;FAST ARG NOT OPERATIVE FOR CALLER
	       (T 0))
	 (+ (COND (QFEFHI-FAST-ARG-OPT-OPERATIVE 0)
		  (T %ARG-DESC-FEF-BIND-HAIR))
	    (+ (LSH MIN-ARGS 6)
	       MAX-ARGS)))))

;Return T if any special variables must be bound at entry to this function.
(DEFUN SPECIAL-BIND-NEEDED-P NIL
  (DO ((VS ALLVARS (CDR VS))) ((NULL VS) NIL)
    (AND (LAP-ARGP (CAR VS))
	 (NEQ (VAR-TYPE (CAR VS)) 'FEF-LOCAL)
	 (RETURN T))))

;Compute and return the special-variable bitmap for the function.
;The bit saying whether the map is active is correctly set in the value returned.
;In addition, S-V-BITMAP-ACTIVE is left T if the bitmap is active.
;The map is active if the AP-relative addresses of all the values
;to be bound to specials are constant, and if the addresses are not
;too large to be expressed in a 1-word bit map.
(DEFUN COMPUTE-S-V-MAP NIL
  (PROG (S-MAP)
	(COND ((NOT (SPECIAL-BIND-NEEDED-P))
	       (SETQ S-V-BITMAP-ACTIVE T)
	       (RETURN %FEFHI-SVM-ACTIVE))  ;Null bitmap, no specials
	      (REST-ARG (RETURN 0)))	    ;Can't predict addresses
	(SETQ S-MAP 0 S-V-BITMAP-ACTIVE T)  ;Assume will use bitmap, unless too many to fit
	(DO ((BIT (LSH %FEFHI-SVM-ACTIVE -1) (LSH BIT -1))
	     (ENDARG)
	     (VS ALLVARS (CDR VS)))
	    ((NULL VS))
	  (COND ((LAP-ARGP (CAR VS))
		 (AND ENDARG (BARF NIL '|Arg-types out of order| 'BARF))
		 (OR (EQ (VAR-TYPE (CAR VS)) 'FEF-LOCAL)
		     (COND ((ZEROP BIT)		;Special past the end of the bit map
			    (SETQ S-V-BITMAP-ACTIVE NIL) ;so give up on using bit map
			    (RETURN NIL))
			   (T (SETQ S-MAP (+ S-MAP BIT))))))
		(T (SETQ ENDARG T))))
	(COND (S-V-BITMAP-ACTIVE
		(RETURN (+ S-MAP %FEFHI-SVM-ACTIVE)))
	      (T (RETURN 0)))))			;Couldn't use bit map after all

;Get a list of all special variables referred to by the function,
;either free or bound, suitable for constructing the indirect pointers
;to their value cells.
;Specials bound at entry to the function must come first, one for one,
;even if there are duplicates.  SPECVARS-BIND-COUNT is the number of such.
;Specials bound internally or used free can have duplicates removed.
(DEFUN EXTRACT-SPECVARS NIL
  (PROG (SVS)
	(SETQ SPECVARS-BIND-COUNT 0)
	(DO ((VS ALLVARS (CDR VS))) ((NULL VS))
	  (AND (NEQ (VAR-TYPE (CAR VS)) 'FEF-LOCAL)
	       (OR (COND ((LAP-ARGP (CAR VS))
			  (SETQ SPECVARS-BIND-COUNT (1+ SPECVARS-BIND-COUNT))
			  T))
		   (NOT (MEMQ (VAR-NAME (CAR VS)) SVS)))
	       (PUSH (VAR-NAME (CAR VS)) SVS)))
	(DO ((VS FREEVARS (CDR VS))) ((NULL VS))
	  (OR (MEMQ (CAR VS) SVS)
	      (PUSH (CAR VS) SVS)))
	(RETURN (REVERSE SVS))))

(DEFUN QLP2-DEFSYM (SYM VAL)
  (PROG NIL 
     S1 (COND ((NULL (CDR SYMPTR)) (GO S1E))	;SYMBOL
	      ((NOT (EQ (CADADR SYMPTR) 'TDEF))
	       (SETQ SYMPTR (CDR SYMPTR))
	       (GO S1))
	      ((OR (NOT (EQ SYM (CAADR SYMPTR)))	;SHOULD BE IN SAME ORDER AS PASS 1
		   (NOT (= VAL (CADDR (CADR SYMPTR)))))
	       (GO S1E)))
	(RETURN (SETQ SYMPTR (CDR SYMPTR)))
     S1E(RETURN (BARF (LIST (CAR SYMPTR) SYM VAL)
		      'SYMPTR-LOSES
		      'BARF))
	))

(DEFUN QLP2-U (WD)	;PASS2 FOR UNBOXED AREA
  (PROG (TEM)
	(COND ((NULL WD) (RETURN NIL))
	      ((ATOM WD)(GO S1))
	      ((EQ (CAR WD) 'RESTART-TAG) (SETQ WD (CADR WD)) (GO S1))
	      ((EQ (CAR WD) 'BRANCH) (GO B1))
	      ((MEMQ (CAR WD) '(COMMENT NO-DROP-THROUGH PARAM)) (RETURN NIL))
	      ((EQ (CAR WD) 'ADI-CALL)
	       (LAP-P2-ADI (CDR WD))
	       (RETURN NIL))
	      ((EQ (CAR WD) 'MISC)		;(MISC destination function)
	       (AND (SETQ TEM (ASSQ (CADDR WD) MISC-INSTRUCTION-REQUIRED-DESTINATION-ALIST))
		    (NOT (MEMQ (CADR WD) (CDR TEM)))
		    (BARF WD '|Illegal destination for this misc instruction| 'BARF))
	       (LAP-OUTPUT-WORD (LAP-WORD-EVAL WD))
	       (GO X1))
	      (T (LAP-OUTPUT-WORD (LAP-WORD-EVAL WD))
		 (GO X1)))
     B1 (QB2 (LIST (CADR WD) (CADDR WD) (CADDDR WD))	;BRANCH
	     (CAR (LAST WD)))
     X1 (SETQ ADR (1+ ADR))
	(RETURN NIL)
     S1 (QLP2-DEFSYM WD ADR) 
	(RETURN NIL)
	))

(DEFUN QLP2-Q (WD) 	;PASS2 FOR Q AREA
  (PROG NIL
	(COND ((ATOM WD)(GO A1))	;TAG HAD BETTER BE PROGSA
	      ((EQ (CAR WD) 'QTAG)
	       (QLP2-DEFSYM (CADR WD) (// ADR 2))
	       (COND ((EQ (CADR WD) 'QUOTE-BASE)
		      (MAPC #'(LAMBDA (CONST-ELT) (QLP2-Q (CAR CONST-ELT)))
			    QUOTE-LIST))) ;DUMP QUOTE TABLE
	       (RETURN NIL))
	      ((EQ (CAR WD) 'PARAM) (RETURN NIL))
	      ((EQ (CAR WD) 'ENDLIST) 			;TERMINATE LIST THAT HAS JUST
	       (LAP-STORE-NXTNIL-CDR-CODE)		;BEEN ASSEMBLED
	       (RETURN NIL))
	      ((EQ (CAR WD) 'MFEF) (LAP-MFEF WD)
	       (RETURN NIL))
	      ((EQ (CAR WD) 'S-V-BLOCK)
	       (SETQ ADR (QLP2-S-V-BLOCK ADR))
	       (RETURN NIL))
	      ((EQ (CAR WD) 'CONSTRUCT-MACRO)
	       (SETQ LAP-MACRO-FLAG T)
	       (RETURN NIL))
	      ((EQ (CAR WD) 'A-D-L)
	       (SETQ ADR (QLP-A-D-L ADR T))
	       (RETURN NIL))
	      ((EQ (CAR WD) 'DEBUG-INFO)
	       (LAP-D-OUT (CDR WD))
	       (LAP-STORE-NXTNIL-CDR-CODE)
	       (GO X2))
	      ((EQ (CAR WD) 'BREAKOFFS)
	       ;; When we see the BREAKOFFS command,
	       ;; we copy the fef offsets of where the ptrs to broken-off fns should go
	       ;; into the cars of the list which is the cadr of the breakoffs command.
	       ;; That list is shared with a debug-info item
	       ;; which is supposed to contain a list of those offsets.
	       (OR (= (LENGTH BREAKOFF-FUNCTION-OFFSETS) (LENGTH (CADR WD)))
		   (BARF NIL '|wrong number of broken-off functions seen by Lap| 'BARF))
		   ;Error check avoids writing a garbaged FEF which will cause things
		   ;to die later.
	       (DO ((OFFSETS (REVERSE BREAKOFF-FUNCTION-OFFSETS) (CDR OFFSETS))
		    (L (CADR WD) (CDR L)))
		   ((NULL L))
		 (SETF (CAR L) (CAR OFFSETS)))
	       (RETURN NIL))
	      ((EQ (CAR WD) 'QUOTE)
	       (LAP-D-OUT (CADR WD))
	       (GO X2))
	      ((EQ (CAR WD) 'LOCATIVE-TO-S-V-CELL)
	       (LAP-Q-OUT NIL 'QZLOC '1 (CADR WD))
	       (GO X2))
	      ((EQ (CAR WD) 'FUNCTION)
	       (IF (SYMBOLP (CADR WD))
		   (LAP-Q-OUT NIL 'QZEVCP '2 (CADR WD))
		   (LAP-Q-OUT NIL 'QZEVCP NIL 
			      (IF (EQ LAP-MODE 'COMPILE-TO-CORE)
				  (FDEFINITION-LOCATION (CADR WD))
				  (CONS EVAL-AT-LOAD-TIME-MARKER
					`(FDEFINITION-LOCATION ',(CADR WD))))))
	       (FUNCTION-REFERENCED (CADR WD) FCTN-NAME)
	       (GO X2))
	      ((EQ (CAR WD) 'BREAKOFF-FUNCTION)
	       (PUSH (// ADR 2) BREAKOFF-FUNCTION-OFFSETS)
	       (LAP-D-OUT (CADR WD))
	       (GO X2))
	      ((EQ (CAR WD) 'TAG)
	       (LAP-D-OUT (QLEVAL (CADR WD) T))
	       (GO X2))
	      ((EQ (CAR WD) 'FIXE)
	       (LAP-D-OUT (QLEVAL (CADR WD) NIL))
	       (GO X2))
	      (T (BARF WD 'UNKNOWN-OP-IN-Q-AREA-LAP 'BARF)
		 (RETURN NIL)))
	
     A1   (COND ((NOT (EQ WD 'PROGSA))
		 (BARF WD 'TAG-IN-Q-AREA 'BARF)
		 (RETURN NIL))
		(T (RETURN T)))	;ADVANCE TO UNBOXED AREA
     X2   (SETQ ADR (+ 2 ADR))))

(DEFUN FUNCTION-REFERENCED (WHAT BY)
  ;; Collect functions referenced
  (OR (FUNCTION-P WHAT)				;defined in QCP1
      (LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)
	    (ENTRY (ASSOC WHAT FUNCTIONS-REFERENCED)))
	(SETQ BY (COPYTREE BY))			;Could be (:METHOD ...)
	(IF ENTRY
	    (RPLACD ENTRY (CONS BY (CDR ENTRY)))
	    (PUSH (LIST (COPYTREE WHAT) BY) FUNCTIONS-REFERENCED)))))

;Output the block of forwarding pointers to value cells of special variables.
;The flag bit is set in each one which is not bound at function entry.
;We make one forwarding pointer for each entry in SPECVARS,
;and assume that the first SPECVARS-BIND-COUNT of them are bound at function entry.
;The argument of this function is the location counter (in half-Qs) in the fef,
;and the updated location counter is returned.
(DEFUN QLP2-S-V-BLOCK (ADR)
   (DO ((SVS SPECVARS (CDR SVS))
	(ADR ADR (+ ADR 2))
	(NUMARGS SPECVARS-BIND-COUNT (1- NUMARGS)))
       ((NULL SVS)
	(LAP-STORE-NXTNIL-CDR-CODE)
	ADR)
      (LAP-Q-OUT (AND (<= NUMARGS 0) '%Q-FLAG-BIT)
		 'QZEVCP
		 1
		 (CAR SVS))))

;Output the argument descriptor list, based entirely on ALLVARS.
;Only bound variables go in the ADL.
;On pass 1, PASS2-FLAG is NIL and all we do is advance ADR.
;Since, at the moment, there are many A-D-L lists output in the lap code,
;QLP-A-D-L-DONE is used to make sure that QLP-A-D-L does its work only once.
;It starts out as NIL, and is set to the number of variables in the ADL.
(DEFUN QLP-A-D-L (ADR PASS2-FLAG)
 (COND ((AND A-D-L-NEEDED-P (NOT QLP-A-D-L-DONE))
	(SETQ QLP-A-D-L-DONE 0)
	(DO ((VS ALLVARS (CDR VS)) (V) (KIND) (INTL)) ((NULL VS))
	   (SETQ V (CAR VS))
	   (SETQ KIND (VAR-KIND V))
	   (SETQ QLP-A-D-L-DONE (1+ QLP-A-D-L-DONE))
	   ;; First, output the word of bits.
	   (AND PASS2-FLAG
		(LAP-D-OUT (+ (SYMEVAL KIND)
			      (SYMEVAL (VAR-TYPE V))
			      (SYMEVAL (VAR-EVAL V))
			      (SYMEVAL (CAR (VAR-INIT V)))
			      (LIST-SUM (VAR-MISC V)))))
	   (SETQ ADR (+ 2 ADR))
	   ;; Now output the initialization data, if any.
	   (SETQ INTL (VAR-INIT V))
	   (COND ((NOT (LAP-ARGP V)))
		 ((EQ (CAR INTL) 'FEF-INI-COMP-C))
		 ((EQ (CAR INTL) 'FEF-INI-OPT-SA)
		   ;; optional arg with alternate starting address: output the tag to jump to.
		  (COND (PASS2-FLAG
			 (QLP2-Q (LIST 'TAG (CADR INTL))))
			(T (SETQ ADR (+ 2 ADR)))))
		 ((CADR INTL)
		  (COND (PASS2-FLAG (QLP2-Q (CADR INTL)))
			(T (SETQ ADR (+ 2 ADR)))))))
	(AND PASS2-FLAG (LAP-STORE-NXTNIL-CDR-CODE))))
 ADR)

(DEFUN QLP1 (WD) 
 (PROG NIL 
     (COND ((NULL WD) (RETURN NIL))
           ((ATOM WD) (GO S1))
	   ((EQ (CAR WD) 'RESTART-TAG)
		(SETQ WD (CADR WD))
		(GO S1))
	   ((EQ (CAR WD) 'QTAG) (GO S1A))
	   ((EQ (CAR WD) 'BRANCH) (GO B1))
	   ((EQ (CAR WD) 'PARAM) (GO P1))
	   ((MEMQ (CAR WD) '(ENDLIST COMMENT NO-DROP-THROUGH)) (RETURN NIL))
	   ((EQ (CAR WD) 'MFEF) (GO MFEF1))
	   ((EQ (CAR WD) 'S-V-BLOCK)
	    (SETQ ADR (+ ADR (* 2 (LENGTH SPECVARS))))
	    (RETURN NIL))
	   ((EQ (CAR WD) 'CONSTRUCT-MACRO) (RETURN NIL))
	   ((EQ (CAR WD) 'A-D-L)
	    (SETQ ADR (QLP-A-D-L ADR NIL))
	    (RETURN NIL))
	   ((EQ (CAR WD) 'DEBUG-INFO)
	    (SETQ QLP-DEBUG-INFO-PRESENT (%LOGDPB 1 %%FEFHI-MS-DEBUG-INFO-PRESENT 0))
	    (RETURN (SETQ ADR (+ 2 ADR))))
	   ((MEMQ (CAR WD)
		  '(QUOTE LOCATIVE-TO-S-V-CELL FIXE TAG))
	    (RETURN (SETQ ADR (+ 2 ADR))))
	   ((EQ (CAR WD) 'BREAKOFFS) 
	    (RETURN NIL))
	   ((EQ (CAR WD) 'ADI-CALL)
		(LAP-P1-ADI (CDR WD))
		(RETURN NIL))
	   (T (LAP-ADR-P1 (CADDR WD))
	      (GO X1)))
B1   (DEFLAPSYM (CAR (LAST WD)) ADR 'BRANCH)
X1   (RETURN (SETQ ADR (1+ ADR)))
S1   (RETURN (DEFLAPSYM WD ADR 'TDEF))
S1A  (RETURN (DEFLAPSYM (CADR WD) (// ADR 2) 'TDEF))
P1   (RETURN (PUTPROP (CADR WD)
		      (QLEVAL (CADDR WD) T)
		      'QLVAL))
MFEF1(SETQ ADR (+ ADR (* 2 %FEF-HEADER-LENGTH)))
     (RETURN NIL)))

(DEFUN LAP-P1-ADI (X)
  (PROG (L ADI)
	(SETQ ADI (CADDDR X))
	(MAPC-ALTERNATE (FUNCTION LAP-ADR-P1)	;ODD POSITIONS OF ADI LIST
			(CDR ADI))
	(SETQ L 1)	;A MISC (OF SOME SORT)
	(COND ((NOT (AND (EQ (CAR (CADDR X)) 'QUOTE-VECTOR)
			 (EQUAL (CADR (CADDR X)) '(FUNCTION *CATCH))))
	       (LAP-ADR-P1 (CADDR X))	;FCTN TO CALL
	       (SETQ L (1+ L))))	;MOVE D-PDL <FUNCTION ..>
	(COND ((MEMQ-ALTERNATE 'RESTART-PC ADI)
	       (SETQ L (1+ L))))	;MOVE D-PDL <TAG XXX>
 	(COND ((MEMQ-ALTERNATE 'MULTIPLE-VALUE ADI)
	       (SETQ L (1+ L))))
	(SETQ ADR (+ ADR L))
	(RETURN L)))

(DEFUN LAP-P2-ADI (X)
  (PROG (ADI TM MISC-TYPE)
	(SETQ MISC-TYPE (CAR X))	;TYPE CALL INST WOULD HAVE USED
	(SETQ ADI (CADDDR X))
	(COND ((NOT (AND (EQ (CAR (CADDR X)) 'QUOTE-VECTOR)
			 (EQUAL (SETQ TM (CADR (CADDR X)))
				'(FUNCTION *CATCH))))
	       (QLP2-U (LIST 'MOVE 'D-PDL (CADDR X))))
	      (T (SETQ MISC-TYPE '%CATCH-OPEN)))
	(COND ((SETQ TM (MEMQ-ALTERNATE 'RESTART-PC ADI))
	       (QLP2-U (LIST 'MOVE 'D-PDL (CADR TM)))
	       (COND ((NOT (MEMQ MISC-TYPE '(%CATCH-OPEN)))
		      (BARF TM 'BAD-ADI-CALL-WITH-RESTART-PC 'BARF))) ))
	(COND ((MEMQ-ALTERNATE 'FEXPR-CALL ADI)
	       (COND ((NOT (EQ MISC-TYPE 'CALL))
		      (BARF MISC-TYPE 'BAD-FEXPR-ADI 'BARF)))
	       (SETQ MISC-TYPE '%FEXPR-CALL)))
	(COND ((MEMQ-ALTERNATE 'LEXPR-CALL ADI)
	       (COND ((NOT (EQ MISC-TYPE 'CALL))
		      (BARF MISC-TYPE 'BAD-LEXPR-ADI 'BARF)))
	       (SETQ MISC-TYPE '%LEXPR-CALL)))
	(COND ((SETQ TM (MEMQ-ALTERNATE 'MULTIPLE-VALUE ADI))
	       (QLP2-U (LIST 'MOVE 'D-PDL (CADR TM)))
	       (SETQ MISC-TYPE (CDR (ASSQ MISC-TYPE '( (CALL . %CALL-MULT-VALUE)
						      (CALL0 . %CALL0-MULT-VALUE)
						      (%FEXPR-CALL . %FEXPR-CALL-MV)
						      (%LEXPR-CALL . %LEXPR-CALL-MV) 
						      (%CATCH-OPEN . %CATCH-OPEN-MV) ) ))) ))
	(COND ((MEMQ-ALTERNATE 'MULTIPLE-VALUE-LIST ADI)
	       (SETQ MISC-TYPE (CDR (OR (ASSQ MISC-TYPE
					      '((CALL . %CALL-MULT-VALUE-LIST)
						(CALL0 . %CALL0-MULT-VALUE-LIST)))
					(BARF MISC-TYPE
					      'CALL-TYPE-ILLEGAL-WITH-MULTIPLE-VALUE-LIST
					      'BARF))))))
	(COND ((MEMQ MISC-TYPE '(NIL CALL CALL0))
	       (BARF X 'BAD-ADI 'BARF)))
	(QLP2-U (LIST 'MISC (CADR X) MISC-TYPE))
	(RETURN NIL)))

(DEFUN MAPC-ALTERNATE (FN LST)
  (PROG NIL
     L	(COND ((NULL LST) (RETURN NIL)))
	(FUNCALL FN (CAR LST))
	(COND ((NULL (SETQ LST (CDR LST)))
	       (RETURN NIL)))
	(SETQ LST (CDR LST))
	(GO L)))

(DEFUN LAP-ADR-P1 (ADR)
  (COND ((ATOM ADR) NIL)
	((EQ (CAR ADR) 'QUOTE-VECTOR)
	 (QADD (CADR ADR)))))

;; On pass 1, add an entry for the constant X to the quote vector if necessary.
;; It is necessary if X is not in the constants page, and not already in the
;; quote vector,
;; or if X is a load-time eval.
(DEFUN QADD (X) 
  (LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
    (OR (NULL X)
	(QFIND-CONSTANTS-PAGE X)
	(PROGN (SETQ QUOTE-COUNT (1+ QUOTE-COUNT))
	       (AND (NOT (CONTAINS-LOAD-TIME-EVAL X)) (ASSOC X QUOTE-LIST)))
	(PUSH (CONS X QUOTE-COUNT) QUOTE-LIST))
    X))

;; Return position of ITEM in constants page, or NIL if it doesn't appear there.
(DEFUN QFIND-CONSTANTS-PAGE (ITEM)
  (AND (EQ (CAR ITEM) 'QUOTE)
       (FIND-POSITION-IN-LIST-EQUAL (CADR ITEM) CONSTANTS-PAGE)))

;; Return T if FORM contains a load-time eval (#,) or other special
;; marker that means it should not be made EQ to things that look equal.
(DEFUN CONTAINS-LOAD-TIME-EVAL (FORM)
  (DO F FORM (CDR F) (ATOM F)
    (AND (OR (AND FASD-MAGIC-AREAS-ALIST (ASSQ (%AREA-NUMBER F) FASD-MAGIC-AREAS-ALIST))
	     (IF (ATOM (CAR F)) (ASSQ (CAR F) FASD-MARKERS-ALIST)
		 (CONTAINS-LOAD-TIME-EVAL (CAR F))))
	 (RETURN T))))

(DEFUN LAP-QUOTE-ADR (ITEM)
  (LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T)
	TM)
    (COND ((SETQ TM (QFIND-CONSTANTS-PAGE ITEM))
	   (+ TM (GET 'CONST-PAGE 'QLVAL)))
	  ((PROGN (SETQ QUOTE-COUNT (1+ QUOTE-COUNT))
		  (DO ((IDX 0 (1+ IDX)) (QUOTE-LIST QUOTE-LIST (CDR QUOTE-LIST)))
		      ((NULL QUOTE-LIST) (SETQ TM NIL))
		    (AND (OR (NOT (CONTAINS-LOAD-TIME-EVAL ITEM))
			     (= QUOTE-COUNT (CDAR QUOTE-LIST)))
			 (EQUAL ITEM (CAAR QUOTE-LIST))
			 (RETURN (SETQ TM IDX)))))
	   (+ TM (QLEVAL 'QUOTE-BASE T)))
	  (T (BARF ITEM 'NOT-ON-QUOTE-LIST 'BARF)
	     0))))

;Var is either the name or the index of a special variable.
(DEFUN LAP-SPECIAL-ADR (VAR)
  (PROG (TM)
	(COND ((NUMBERP VAR)
	       (RETURN (+ VAR (QLEVAL 'S-V-BASE T))))
	      ((SETQ TM (FIND-POSITION-IN-LIST VAR SPECVARS))
		(RETURN (+ TM (QLEVAL 'S-V-BASE T))))
	      (T (BARF VAR 'NOT-ON-SPECIAL-VAR-LIST 'BARF)
		(RETURN 0)))))

;QLAP SYMBOL TABLE..
; IS A LIST, STARTING FROM (CDR SYMTAB)
; ORDER IS IMPORTANT. ON PASS 1 IT IS IN REVERSE ORDER FROM THAT IN WHICH
; ENTIRES WHERE MADE. IT IS NREVERSE D PRIOR TO PASS2.
;ENTRIES ARE OF TWO TYPES, DEFINITIONS OF SYMBOLS AND NOTATIONS THAT A
; BRANCH WHICH MIGHT TAKE TWO "WORDS" OCCURRED.  THESE LATER ARE REMOVED AS
; SOON AS IT CAN BE DETERMINED THAT THE BRANCH CAN DEFINITELY "MAKE IT" IN
; ONE WORD (IE MAGNITUDE OF DELTA IS < OR = 377).
;EACH ENTRY IS A 3 LIST, SYM TYPE VAL.  TYPE IS EITHER TDEF OR BRANCH.
; VAL IS VALUE IF TYPE IS SYM, OR THE ADR OF THE BRANCH IF TYPE IS BRANCH.

(DEFUN LAP-SYMTAB-PLACE (SYM)
  (PROG (STP)
	(SETQ STP (CDR SYMTAB))
  L	(COND ((NULL STP) (BARF SYM 'CANT-FIND-PLACE 'BARF))
	      ((EQ (CAAR STP) SYM) (RETURN STP)))
	(SETQ STP (CDR STP))
	(GO L)))

(DEFUN LAP-SYMTAB-RELOC (BOTTOM AMT STP)
				       ;RELOCATE SYMTAB ITEMS IN SYMTAB SEGMENT POINTED
				       ;TO BY STP BY AMOUNT AMT
 (PROG (TEM)			       ;IF THEY ARE .GE. BOTTOM
	(SETQ TEM STP)
  A	(COND ((NULL TEM) (RETURN NIL))
	      ((NOT (< (CADDAR TEM) BOTTOM))
		(RPLACA (CDDAR TEM) (+ AMT (CADDAR TEM)))))
	(SETQ TEM (CDR TEM))
	(GO A)))

(DEFUN DEFLAPSYM (SYM VAL TYPE) 
  (PROG (STP NBR TM) 
	(SETQ STP SYMTAB)
	(SETQ NBR 0)
     L  (COND ((NULL (CDR STP)) (GO L1))
	      ((EQ (CAADR STP) SYM) (GO L2))
	      ((EQ (CADADR STP) 'BRANCH)
	       (SETQ NBR (1+ NBR))))
     L3 (SETQ STP (CDR STP))
	(GO L)
     L1 (RETURN (RPLACD SYMTAB (CONS (LIST SYM TYPE VAL) (CDR SYMTAB))))
     L2 (COND ((EQ TYPE 'BRANCH) (GO L2C))
	      ((EQ (CADADR STP) 'BRANCH) (GO L2A))	;NOW DEFINING SYM BRANCHED TO THEN
	   ;  ((AND (EQ (CADADR STP) 'TDEF)
	   ;        (EQ TYPE 'TDEF))
	   ;	    (RETURN (RPLACA (CDDADR STP) VAL)))		  ;REDEFINING
	      (T (BARF (LIST SYM VAL TYPE)
		       'MULT-DEF
		       'DATA)))
     L2A(COND ((NOT (EQ TYPE 'TDEF))
	       (BARF TYPE 'BAD-TYPE 'BARF)))
	(SETQ TM (+ VAL NBR))				  ;HIGHEST POSSIBLE VALUE
     L2B(COND ((< (- TM (CADDR (CADR STP))) 377)
	       (RPLACD STP (CDDR STP))
	       (GO L)))					  ;THAT BRANCH WILL MAKE IT
	(GO L3)						  ;MAYBE IT WONT
     L2C(COND ((EQ (CADADR STP) 'BRANCH) (GO L1))	  ;THAT BRANCH DIDNT MAKE IT
	      						  ;SO THIS ONE WONT
	      ((= VAL (CADDR (CADR STP))) (GO L1))	  ;EITHER JMP . LOSES!
	      ((< (- (+ VAL NBR) (CADDR (CADR STP))) 377)
	       (RETURN NIL)))				  ;THIS ONE DEFINITELY MAKES IT
	(GO L1))) 					  ;MIGHT OR MIGHT NOT HMMM

(DEFUN LAP-WORD-EVAL (WD)
  (PROG (VL TM)
	(SETQ VL 0)
	(COND ((EQ (CAR WD) 'SETE)
	       (SETQ VL
		     (+ 112000
			(CDR (ASSQ (CADR WD)
				   '((CDR . 0)
				     (CDDR . 20000)
				     (1+ . 40000)
				     (1- . 60000))))))
	       (SETQ WD (CDDR WD))))
     L	(COND ((NULL WD) (RETURN VL))
	      ((NUMBERP (SETQ TM (CAR WD))))
	      ((ATOM (CAR WD) )
	       (COND ((NULL (SETQ TM (GET (CAR WD) 'QLVAL)))
		      (BARF WD 'UNDEFINED-IN-WORD 'BARF)
		      (SETQ TM 0))))
	      ((EQ (CAAR WD) 'QUOTE-VECTOR)
	       (SETQ TM (LAP-QUOTE-ADR (CADAR WD))))
	      ((EQ (CAAR WD) 'SPECIAL)
	       (SETQ TM (LAP-SPECIAL-ADR (CADAR WD))))
	      (T (SETQ TM (QLEVAL (CAR WD) NIL ))))
	(SETQ VL (+ VL TM))
	(SETQ WD (CDR WD))
	(GO L)))

(DEFUN QLEVAL (X FLAG) 			;FLAG ->T, USE SYMTAB, NIL-> QLVAL PROPS 
  (PROG (VL) 
	(SETQ VL 0)
	(COND ((NUMBERP X) (RETURN X)) ((ATOM X) (GO S1)))
     L1 (SETQ VL (+ (QLEVAL (CAR X) FLAG) VL))
	(COND ((NULL (SETQ X (CDR X))) (RETURN VL)))
	(GO L1)
     S1 (COND (FLAG (GO S1A))
	      ((NULL (SETQ VL (GET X 'QLVAL))) (GO S1A))
	      (T (RETURN VL)))
     S1A(SETQ VL SYMTAB)
     S2 (COND ((NULL (CDR VL)) (GO E1))
	      ((AND (EQ (CAADR VL) X)
		    (NOT (EQ (CADADR VL) 'BRANCH)))
	       (RETURN (CADDR (CADR VL)))))
	(SETQ VL (CDR VL))
	(GO S2)
     E1 (BARF X 'UNDEFINED 'DATA))) 

(DEFUN QLRLC (ENTRY AMT) 
  (COND ((NOT (= 0 AMT))
	 (RPLACA (CDDR ENTRY) (+ AMT (CADDR ENTRY)))))) 

(DEFUN QB2 (CONDITION TAG) 
  (PROG (TM TM2) 
	(COND
	  ((NULL (SETQ TM
		       (ASSOC CONDITION
			      '(((ALWAYS NIL NIL) . 0)
				((NILIND TRUE NIL) . 20000)
				((NILIND FALSE NIL) . 40000)
				((NILIND TRUE T) . 60000)
				((NILIND FALSE T) . 100000)
				((ATOMIND TRUE NIL) . 120000)
				((ATOMIND FALSE NIL) . 140000)))))
	   (BARF CONDITION
		 'NON-EXISTANT-CONDITION
		 'BARF)))
	(SETQ TM2 (- (QLEVAL TAG T) ADR))
     L1 (COND ((NULL (CDR SYMPTR)) (GO L2))
  ;	      ((EQ (CADADR SYMPTR) 'TDEF)
  ;	       (SETQ SYMPTR (CDR SYMPTR))
  ;	       (GO L1))
	      ((AND (EQ (CAADR SYMPTR) TAG)
		    (EQ (CADADR SYMPTR) 'BRANCH)
		    (= ADR (CADDR (CADR SYMPTR))))
	       (SETQ SYMPTR (CDR SYMPTR))		;COMMITTED TO 2 WD BRANCH
	       (LAP-OUTPUT-WORD (+ 14777 (CDR TM)))
	       (SETQ ADR (1+ ADR))
	       (LAP-OUTPUT-WORD (BOOLE 1 177777 (- TM2 2)))  ;- NUMBERS DONT WIN!
	       (RETURN NIL))				       ;-1 BECAUSE PC IS INCREMENTED
	      ((OR (= 0 TM2) (> (ABS TM2) 376))  ; ANOTHER -1 BECAUSE ADR IS 1 MORE NOW
	       (BARF (LIST TAG TM2) 'NOT-IN-RANGE 'BARF)))
     L2 (RETURN (LAP-OUTPUT-WORD (+ 14000
				    (+ (CDR TM) (BOOLE 1 777 (1- TM2)))))))) 

(DEFUN MEMQ-ALTERNATE (X Y)
  (PROG NIL 
    L	(COND ((NULL Y) (RETURN NIL))
	      ((EQ X (CAR Y)) (RETURN Y)))
	(SETQ Y (CDDR Y))
	(GO L)))