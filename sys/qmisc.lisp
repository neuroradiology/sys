; -*- Package:SYSTEM-INTERNALS; Mode:LISP; Base:8 -*-
; MACHINE MISCELLANEOUS FUNCTIONS NOT WORTHY OF BEING IN QFCTNS
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(DECLARE (SETQ RETAIN-VARIABLE-NAMES-SWITCH 'ALL))
(DECLARE (SPECIAL ROOM))

(SETQ ROOM '(WORKING-STORAGE-AREA MACRO-COMPILED-PROGRAM))

(DEFUN ROOM-GET-AREA-LENGTH-USED (AREA)
  (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION))
       (N-REGIONS 0 (1+ N-REGIONS))
       (LENGTH 0 (+ LENGTH (24-BIT-UNSIGNED (REGION-LENGTH REGION))))
       (USED 0 (+ USED (24-BIT-UNSIGNED (REGION-FREE-POINTER REGION)))))
      ((MINUSP REGION)
       (RETURN LENGTH USED N-REGIONS))))

(DEFUN 24-BIT-UNSIGNED (N)
  "Given an unsigned 24-bit fixnum, returns a positive number.
If the argument is negative, it is expanded into a bignum."
  (IF (MINUSP N) (+ N (ASH 1 24.)) N))

(DEFUN MAKE-24-BIT-UNSIGNED (N)
  "If arg a bignum, its low 24 bits are returned as a fixnum, possibly
   negative.  A fixnum arg is just returned."
  (COND ((= (%DATA-TYPE N) DTP-FIX) N)
	(T (LOGIOR (LSH (LDB 2701 N) 27) (LDB 27 N)))))

(DEFUN ROOM-PRINT-AREA (AREA &AUX LENGTH USED N-REGIONS (PACKAGE (PKG-FIND-PACKAGE "SYSTEM")))
  (COND ((NOT (NULL (AREA-NAME AREA)))
	 (MULTIPLE-VALUE (LENGTH USED N-REGIONS) (ROOM-GET-AREA-LENGTH-USED AREA))
	 (IF (= (LDB %%REGION-SPACE-TYPE (REGION-BITS (AREA-REGION-LIST AREA)))
		%REGION-SPACE-FIXED)
	     (FORMAT T "~51,1,1,'.<~S~;(~D region~:P)~> ~O//~O used.  ~D% free.~%"
		     (AREA-NAME AREA) N-REGIONS USED LENGTH
		     (COND ((ZEROP LENGTH)
			    0)
			   ((< LENGTH 40000)
			    (// (* 100. (- LENGTH USED)) LENGTH))
			   (T
			    (// (- LENGTH USED) (// LENGTH 100.))) ))
	     (FORMAT T "~51,1,1,'.<~S~;(~D region~:P)~> ~DK allocated, ~DK used.~%"
		     (AREA-NAME AREA) N-REGIONS
		     (// (+ LENGTH 1777) 2000) (// (+ USED 1777) 2000)))))
  T)

;(ROOM) tells about the default areas
;(ROOM area1 area2...) tells about those areas
;(ROOM T) tells about all areas
;(ROOM NIL) prints only the header, does not do any areas
(DEFUN ROOM (&REST ARGS)
  (LET ((FREE-SIZE (GET-FREE-SPACE-SIZE))
	(PHYS-SIZE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)))
    (FORMAT T "~&Physical memory: ~O (~DK), Free space: ~O (~DK)"
	      PHYS-SIZE (// PHYS-SIZE 2000) FREE-SIZE (// FREE-SIZE 2000)))
  (MULTIPLE-VALUE-BIND (N-WIRED-PAGES N-FIXED-WIRED-PAGES)
      (COUNT-WIRED-PAGES)
    (FORMAT T ", Wired pages ~D+~D (~D~[~;.25~;.5~;.75~]K)~%"
	      N-FIXED-WIRED-PAGES (- N-WIRED-PAGES N-FIXED-WIRED-PAGES)
	      (// N-WIRED-PAGES (// 2000 PAGE-SIZE))
	      (\ N-WIRED-PAGES (// 2000 PAGE-SIZE))))
  (COND ((NULL ARGS)
	 (SETQ ARGS ROOM))
	((EQUAL ARGS '(T))
	 (FORMAT T "Unless otherwise noted, area names are in the SYSTEM package~%")
	 (SETQ ARGS AREA-LIST)))
  (COND ((NOT (EQUAL ARGS '(NIL)))
	 (DOLIST (AREA ARGS)
	   (ROOM-PRINT-AREA (IF (SYMBOLP AREA) (SYMEVAL AREA) AREA))))))

;First value is total number of wired pages.  Second is number of fixed-wired pages.
(DEFUN COUNT-WIRED-PAGES ()
  (DO ((ADR (REGION-ORIGIN PAGE-TABLE-AREA) (+ ADR 2))
       (N (// (SYSTEM-COMMUNICATION-AREA %SYS-COM-PAGE-TABLE-SIZE) 2) (1- N))
       (N-WIRED 0))
      ((ZEROP N)
       (DO ((ADR (REGION-ORIGIN PHYSICAL-PAGE-DATA) (1+ ADR))
	    (N (// (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE) PAGE-SIZE) (1- N))
	    (N-FIXED-WIRED 0))
	   ((ZEROP N)
	    (RETURN (+ N-WIRED N-FIXED-WIRED) N-FIXED-WIRED))
	 (AND (= (%P-LDB 0020 ADR) 177777)
	      ( (%P-LDB 2020 ADR) 177777)
	      (SETQ N-FIXED-WIRED (1+ N-FIXED-WIRED)))))
    (AND (NOT (ZEROP (%P-LDB %%PHT1-VALID-BIT ADR)))
	 (= (%P-LDB %%PHT1-SWAP-STATUS-CODE ADR) %PHT-SWAP-STATUS-WIRED)
	 (SETQ N-WIRED (1+ N-WIRED)))))

(DEFUN STREAM-COPY-UNTIL-EOF (FROM-STREAM TO-STREAM &OPTIONAL (LEADER-SIZE NIL))
  "Copy from one stream to another, until EOF on the from-stream.
  The default is to use the most efficient mode, but the third argument
  may be used to force use of :LINE-IN//:LINE-OUT mode, especially useful
  when the to-stream is an editor interval stream.  If you use this to
  copy binary files, note that you had better open the streams with
  appropriate host-dependent byte sizes, and that if the from-stream
  supports :LINE-IN but not :READ-INPUT-BUFFER you will probably lose."
  (LET ((FWO (FUNCALL FROM-STREAM ':WHICH-OPERATIONS))
	(TWO (FUNCALL TO-STREAM ':WHICH-OPERATIONS)))
    (COND ((AND (NOT LEADER-SIZE)
		(MEMQ ':READ-INPUT-BUFFER FWO)
		(MEMQ ':STRING-OUT TWO))
	   ;; If it can go, this mode is the most efficient by far.
	   (DO ((BUF) (OFFSET) (LIMIT))
	       (())
	     (MULTIPLE-VALUE (BUF OFFSET LIMIT)
	       (FUNCALL FROM-STREAM ':READ-INPUT-BUFFER))
	     (COND ((NULL BUF) (RETURN NIL)))
	     (FUNCALL TO-STREAM ':STRING-OUT BUF OFFSET LIMIT)
	     (FUNCALL FROM-STREAM ':ADVANCE-INPUT-BUFFER)))

	  ((AND (MEMQ ':LINE-IN FWO)
                (MEMQ ':LINE-OUT TWO))
	   ;; Not as good, but better than :TYI/:TYO
           (DO ((LINE) (EOF))
	       (())
	     (MULTIPLE-VALUE (LINE EOF)
	       (FUNCALL FROM-STREAM ':LINE-IN LEADER-SIZE))
	     (COND ((NOT EOF)
		    (FUNCALL TO-STREAM ':LINE-OUT LINE))
		   (T (FUNCALL TO-STREAM ':STRING-OUT LINE)
		      (RETURN NIL)))))
	  ;; This always wins, but is incredibly slow.
	  (T (DO ((CHAR))
		 ((NULL (SETQ CHAR (FUNCALL FROM-STREAM ':TYI))))
	       (FUNCALL TO-STREAM ':TYO CHAR))))))

(DEFUN DESCRIBE-ADL (ADL)
  (PROG (OPT-Q INIT-OPTION)
    L	(COND ((NULL ADL) (RETURN NIL)))
    	(SETQ OPT-Q (CAR ADL) ADL (CDR ADL))
	(TERPRI)
	(COND ((NOT (ZEROP (LOGAND OPT-Q %FEF-NAME-PRESENT)))
	       (PRINC "NAME ")
	       (PRIN1-THEN-SPACE (CAR ADL))
	       (SETQ ADL (CDR ADL))))
	(PRIN1-THEN-SPACE (NTH (LDB %%FEF-SPECIALNESS OPT-Q)
			       FEF-SPECIALNESS))
	(PRIN1-THEN-SPACE (NTH (LDB %%FEF-DES-DT OPT-Q)
			       FEF-DES-DT))
	(PRIN1-THEN-SPACE (NTH (LDB %%FEF-QUOTE-STATUS OPT-Q)
			       FEF-QUOTE-STATUS))
	(PRIN1-THEN-SPACE (NTH (LDB %%FEF-ARG-SYNTAX OPT-Q)
			       FEF-ARG-SYNTAX))
	(PRIN1-THEN-SPACE (SETQ INIT-OPTION (NTH (LDB %%FEF-INIT-OPTION OPT-Q)
						 FEF-INIT-OPTION)))
	(COND ((MEMQ INIT-OPTION '(FEF-INI-PNTR FEF-INI-C-PNTR 
				   FEF-INI-OPT-SA FEF-INI-EFF-ADR))
	       (PRINC "ARG ")
	       (PRIN1 (CAR ADL))
	       (SETQ ADL (CDR ADL))))
	(GO L)
))

(DEFUN DESCRIBE-STACK-GROUP (SG &AUX TEM)
  (FORMAT T "~%Stack Group; name is ~S, current state ~S"
	  (SG-NAME SG)
	  (NTH (SG-CURRENT-STATE SG) SG-STATES))
  (COND ((NOT (ZEROP (SG-IN-SWAPPED-STATE SG)))
	 (FORMAT T "~%  Variables currently swapped out")))
  (COND ((NOT (ZEROP (SG-FOOTHOLD-EXECUTING-FLAG SG)))
	 (FORMAT T "~%  Foothold currently executing")))
  (COND ((NOT (ZEROP (SG-PROCESSING-ERROR-FLAG SG)))
	 (FORMAT T "~% Currently processing an error")))
  (COND ((NOT (ZEROP (SG-PROCESSING-INTERRUPT-FLAG SG)))
	 (FORMAT T "~% Currently processing an interrupt")))
  (FORMAT T "~%ERROR-MODE:")
     (PRINT-ERROR-MODE (SG-SAVED-M-FLAGS SG))
  (FORMAT T "~%SG-SAFE ~D, SG-SWAP-SV-ON-CALL-OUT ~D, SG-SWAP-SV-OF-SG-THAT-CALLS-ME ~D"
	  (SG-SAFE SG)
	  (SG-SWAP-SV-ON-CALL-OUT SG)
	  (SG-SWAP-SV-OF-SG-THAT-CALLS-ME SG))
  (FORMAT T "~%SG-INST-DISP: ~D (~:*~[Normal~;Debug~;Single-step~;Single-step done~])"
	    (SG-INST-DISP SG))
  (FORMAT T 
      "~%SG-PREVIOUS-STACK-GROUP ~S, SG-CALLING-ARGS-NUMBER ~S, SG-CALLING-ARGS-POINTER ~S"
          (SG-PREVIOUS-STACK-GROUP SG)
	  (SG-CALLING-ARGS-NUMBER SG)
	  (SG-CALLING-ARGS-POINTER SG))
  (FORMAT T "~%Regular PDL pointer ~D, ~D available, ~D limit"
          (SG-REGULAR-PDL-POINTER SG)
	  (ARRAY-LENGTH (SG-REGULAR-PDL SG))
	  (SG-REGULAR-PDL-LIMIT SG))
  (FORMAT T "~%Special PDL pointer ~D, ~D available, ~D limit"
	  (SG-SPECIAL-PDL-POINTER SG)
	  (ARRAY-LENGTH (SG-SPECIAL-PDL SG))
	  (SG-SPECIAL-PDL-LIMIT SG))
  (COND ((SETQ TEM (SG-RECOVERY-HISTORY SG))
	 (FORMAT T "~%Recovery history ~S" TEM)))
  (COND ((SETQ TEM (SG-UCODE SG))
	 (FORMAT T "~%SG-UCODE ~S" TEM)))
)

(DEFUN DESCRIBE-FEF (FEF &AUX HEADER NAME FAST-ARG SV MISC LENGTH DBI)
   (COND ((SYMBOLP FEF)
	  (DESCRIBE-FEF (CAR (FUNCTION-CELL-LOCATION FEF))))
	 ((NEQ (%DATA-TYPE FEF) DTP-FEF-POINTER)
	  (FERROR NIL "~S is not a FEF" FEF))
	 (T
	  (SETQ HEADER (%P-LDB-OFFSET %%HEADER-REST-FIELD FEF %FEFHI-IPC))
	  (SETQ LENGTH (%P-CONTENTS-OFFSET FEF %FEFHI-STORAGE-LENGTH))
	  (SETQ NAME (%P-CONTENTS-OFFSET FEF %FEFHI-FCTN-NAME))
	  (SETQ FAST-ARG (%P-CONTENTS-OFFSET FEF %FEFHI-FAST-ARG-OPT))
	  (SETQ SV (%P-CONTENTS-OFFSET FEF %FEFHI-SV-BITMAP))
	  (SETQ MISC (%P-CONTENTS-OFFSET FEF %FEFHI-MISC))
	  
	  (FORMAT T "~%FEF for function ~S~%" NAME)
	  (FORMAT T "Initial relative PC: ~S halfwords.~%" (LDB %%FEFH-PC HEADER))
; -- Print out the fast arg option
	  (FORMAT T "The Fast Argument Option is ~A"
		    (IF (ZEROP (LDB %%FEFH-FAST-ARG HEADER))
			"not active, but here it is anyway:"
			"active:"))
	  (DESCRIBE-NUMERIC-DESCRIPTOR-WORD FAST-ARG)
; -- Randomness.
	  (FORMAT T "~%The length of the local block is ~S~%"
		    (LDB %%FEFHI-MS-LOCAL-BLOCK-LENGTH MISC))
	  (FORMAT T "The total storage length of the FEF is ~S~%"
		    LENGTH)
; -- Special variables
	  (COND ((ZEROP (LDB %%FEFH-SV-BIND HEADER))
		 (PRINC "There are no special variables present."))
		(T (PRINC "There are special variables, ")
		   (TERPRI)
		   (COND ((ZEROP (LDB %%FEFHI-SVM-ACTIVE SV))
			  (PRINC "but the S-V bit map is not active. "))
			 (T (FORMAT T "and the S-V bit map is active and contains: ~O"
				      (LDB %%FEFHI-SVM-BITS SV))))))
          (TERPRI)
; -- ADL.
	  (COND ((ZEROP (LDB %%FEFH-NO-ADL HEADER))
		 (FORMAT T "There is an ADL:  It is ~S long, and starts at ~S"
			   (LDB %%FEFHI-MS-BIND-DESC-LENGTH MISC)
			   (LDB %%FEFHI-MS-ARG-DESC-ORG MISC))
		 (DESCRIBE-ADL (GET-MACRO-ARG-DESC-POINTER FEF))
		 )
		(T (PRINC "There is no ADL.")))
	  (TERPRI)
	  (COND ((SETQ DBI (FUNCTION-DEBUGGING-INFO FEF))
		 (FORMAT T "Debugging info:~%")
		 (DOLIST (ITEM DBI)
		   (FORMAT T "  ~S~%" ITEM))))
	  )))

(DEFUN DESCRIBE-NUMERIC-DESCRIPTOR-WORD (N &AUX MIN MAX)
    (TERPRI)
    (PRINC "   ")
    (AND (BIT-TEST %ARG-DESC-QUOTED-REST N)
	 (PRINC "Quoted rest arg, "))
    (AND (BIT-TEST %ARG-DESC-EVALED-REST N)
	 (PRINC "Evaluated rest arg, "))
    (AND (BIT-TEST %ARG-DESC-FEF-QUOTE-HAIR N)
	 (PRINC "Some args quoted, "))
    (AND (BIT-TEST %ARG-DESC-INTERPRETED N)
	 (PRINC "Interpreted function, "))
    (AND (BIT-TEST %ARG-DESC-FEF-BIND-HAIR N)
	 (PRINC "Linear enter must check ADL, "))
    (SETQ MAX (LDB %%ARG-DESC-MAX-ARGS N))
    (SETQ MIN (LDB %%ARG-DESC-MIN-ARGS N))
    (COND ((= MAX MIN)
	   (PRINC MAX) (PRINC " args."))
	  (T
	   (PRINC "Takes between ") (PRINC MIN) (PRINC " and ") (PRINC MAX) (PRINC " args."))))


(DEFUN DESCRIBE-ARRAY (ARRAY &AUX ARRAYDIMS NDIMS LONG-LENGTH-FLAG)
    (COND ((SYMBOLP ARRAY)
	   (COND ((AND (BOUNDP ARRAY)
		       (ARRAYP (SYMEVAL ARRAY)))
		  (DESCRIBE-ARRAY (SYMEVAL ARRAY)))
		 ((AND (FBOUNDP ARRAY)
		       (ARRAYP (FSYMEVAL ARRAY)))
		  (DESCRIBE-ARRAY (FSYMEVAL ARRAY)))
		 (T NIL)))
	  ((ARRAYP ARRAY)
	   (FORMAT STANDARD-OUTPUT "~%This is a ~S type array." (ARRAY-TYPE ARRAY))
	   (SETQ ARRAYDIMS (ARRAY-DIMENSIONS ARRAY))
	   (SETQ NDIMS (LENGTH ARRAYDIMS))
	   (SETQ LONG-LENGTH-FLAG (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))
	   (COND ((> NDIMS 1)
		  (FORMAT STANDARD-OUTPUT "~%It is ~D-dimensional, with dimensions "
			  NDIMS)
		  (DO L ARRAYDIMS (CDR L) (NULL L)
		    (FORMAT STANDARD-OUTPUT "~S " (CAR L))))
		 (T (FORMAT STANDARD-OUTPUT "~%It is ~S long." (CAR ARRAYDIMS))
		    (AND (< (ARRAY-ACTIVE-LENGTH ARRAY) (CAR ARRAYDIMS))
			 (FORMAT STANDARD-OUTPUT "  Active length is ~S"
				 (ARRAY-ACTIVE-LENGTH ARRAY)))))
	   (AND (ARRAY-HAS-LEADER-P ARRAY)
		(FORMAT STANDARD-OUTPUT "~%It has a leader, of length ~S"
			(ARRAY-LEADER-LENGTH ARRAY)))
	   (COND ((ARRAY-DISPLACED-P ARRAY)
		  (COND ((ARRAY-INDIRECT-P ARRAY)
			 (FORMAT STANDARD-OUTPUT "~%The array is indirected to ~S"
				 (%P-CONTENTS-OFFSET ARRAY (+ NDIMS LONG-LENGTH-FLAG)))
			 (AND (ARRAY-INDEXED-P ARRAY)
			      (FORMAT STANDARD-OUTPUT ", with index-offset ~S"
				    (%P-CONTENTS-OFFSET ARRAY (+ NDIMS LONG-LENGTH-FLAG 2))))
			 (FORMAT STANDARD-OUTPUT "~%Description:")
			 (DESCRIBE-ARRAY (%P-CONTENTS-OFFSET ARRAY
							     (+ NDIMS LONG-LENGTH-FLAG))))
			(T (FORMAT STANDARD-OUTPUT "~%The array is displaced to ~S"
				   (%P-CONTENTS-OFFSET ARRAY (+ NDIMS LONG-LENGTH-FLAG))))))))
	  (T (FERROR NIL "~S is not an array" ARRAY))))

;DESCRIBE ANYTHING
(DEFUN DESCRIBE (ANYTHING &OPTIONAL NO-COMPLAINTS &AUX TYPE)
  (COND	((AND (NAMED-STRUCTURE-P ANYTHING)
	      (COND ((AND (GET (NAMED-STRUCTURE-SYMBOL ANYTHING) 'NAMED-STRUCTURE-INVOKE)
			  (MEMQ ':DESCRIBE
				(NAMED-STRUCTURE-INVOKE ANYTHING ':WHICH-OPERATIONS)))
		     (NAMED-STRUCTURE-INVOKE ANYTHING ':DESCRIBE))
		    ((GET (SETQ TYPE (NAMED-STRUCTURE-SYMBOL ANYTHING)) 'DEFSTRUCT-ITEMS)
		     (DESCRIBE-OLD-DEFSTRUCT TYPE ANYTHING))
		    ((GET (NAMED-STRUCTURE-SYMBOL ANYTHING) 'DEFSTRUCT-DESCRIPTION)
		     (DESCRIBE-DEFSTRUCT ANYTHING)))))
	((OR (ENTITYP ANYTHING) (= (%DATA-TYPE ANYTHING) DTP-INSTANCE))
	 (FUNCALL ANYTHING ':DESCRIBE))
	((ARRAYP ANYTHING)
	 (DESCRIBE-ARRAY ANYTHING))
	((CLOSUREP ANYTHING)
	 (DESCRIBE-CLOSURE ANYTHING))
	((= (%DATA-TYPE ANYTHING) DTP-FEF-POINTER)
	 (DESCRIBE-FEF ANYTHING))
	((SYMBOLP ANYTHING)
	 (DESCRIBE-SYMBOL ANYTHING))
	((LISTP ANYTHING)
	 (DESCRIBE-LIST ANYTHING))
	((= (%DATA-TYPE ANYTHING) DTP-STACK-GROUP)
	 (DESCRIBE-STACK-GROUP ANYTHING))
	((SMALL-FLOATP ANYTHING)
	 (DESCRIBE-SMALL-FLONUM ANYTHING))
	((FLOATP ANYTHING)
	 (DESCRIBE-FLONUM ANYTHING))
        ((= (%DATA-TYPE ANYTHING) DTP-SELECT-METHOD)
         (DESCRIBE-SELECT-METHOD ANYTHING))
	((bigp anything)
	 (describe-bignum anything))
	((FIXP ANYTHING)
	 (FORMAT T "~%~R is ~[even~;odd~]" ANYTHING (LDB 0001 ANYTHING)))
	((NOT NO-COMPLAINTS)
	 (FORMAT STANDARD-OUTPUT "~%I don't know how to describe ~S" ANYTHING)))
  (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
  ANYTHING)

(DEFUN DESCRIBE-1 (THING)	;AN INTERNAL SUBROUTINE
  (COND ((OR (NULL THING) ;Don't recursively describe relatively boring things
	     (NUMBERP THING) (SYMBOLP THING) (STRINGP THING))
	 NIL)
	(T (LET ((STANDARD-OUTPUT	;Arrange for indentation by 4 spaces
		   (CLOSURE '(STANDARD-OUTPUT)
		     #'(LAMBDA (&REST ARGS)
			  ;; Have to do it this way rather than with PROG1
			  ;; due to multiple-values not getting passed back
			  ;; This vile kludgery seems to be the only way to get it to work
			  ;; due to various things shafting me left and right.
			  (PROG (X1 X2 X3 X4 X5)
			    (MULTIPLE-VALUE (X1 X2 X3 X4 X5)
			       (APPLY STANDARD-OUTPUT ARGS))
			    (AND (EQ (CAR ARGS) ':TYO) (= (CADR ARGS) #\CR)
				 (FUNCALL STANDARD-OUTPUT ':STRING-OUT "    "))
			    (RETURN X1 X2 X3 X4 X5))))))
	     (DESCRIBE THING T))
	   (FUNCALL STANDARD-OUTPUT ':FRESH-LINE))))

(DEFUN DESCRIBE-SYMBOL (SYM)
  (COND ((BOUNDP SYM)
	 (LET ((PRINLEVEL 2) (PRINLENGTH 3))
	   (FORMAT STANDARD-OUTPUT "~%The value of ~S is ~S" SYM (SYMEVAL SYM)))
	 (DESCRIBE-1 (SYMEVAL SYM))))
  (COND ((FBOUNDP SYM)
	 (LET ((PRINLEVEL 2) (PRINLENGTH 3))
	   (FORMAT STANDARD-OUTPUT "~%~S is the function ~S: ~S"
		   SYM (FSYMEVAL SYM) (ARGLIST SYM)))
	 (DESCRIBE-1 (FSYMEVAL SYM))))
  (DO ((PL (PLIST SYM) (CDDR PL))
       (PRINLEVEL 2)
       (PRINLENGTH 3))
      ((NULL PL))
    (FORMAT STANDARD-OUTPUT "~%~S has property ~S: ~S"
	    SYM (CAR PL) (CADR PL))
    (DESCRIBE-1 (CADR PL)))
  NIL)

(DEFUN DESCRIBE-LIST (L)
  (FORMAT STANDARD-OUTPUT "~%~S is a list" L))

(DEFUN DESCRIBE-OLD-DEFSTRUCT (SYMBOL X)
  (FORMAT T "~%~S is a ~S~%" X SYMBOL)
  (DO L (GET SYMBOL 'DEFSTRUCT-ITEMS) (CDR L) (NULL L)
      (FORMAT T "   ~30A~S~%"
	      (STRING-APPEND (CAR L) ":")
	      (EVAL `(,(CAR L) ',X)))))

(DEFUN DESCRIBE-DEFSTRUCT (X &OPTIONAL DEFSTRUCT-TYPE &AUX DESCRIPTION)
  (SETQ DESCRIPTION (GET (OR DEFSTRUCT-TYPE (NAMED-STRUCTURE-SYMBOL X))
			 'DEFSTRUCT-DESCRIPTION))
  (FORMAT T "~%~S is a ~S~%" X (DEFSTRUCT-DESCRIPTION-NAME))
  (DO L (DEFSTRUCT-DESCRIPTION-SLOT-ALIST) (CDR L) (NULL L)
      (FORMAT T "   ~30A~S~%"
	      (STRING-APPEND (CAAR L) ":")
	      (EVAL `(,(DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDAR L)) ',X)))))

(DEFUN DESCRIBE-CLOSURE (CL)
  (LET ((C (%MAKE-POINTER DTP-LIST CL))
	(SYM NIL) (OFFSET NIL))
    (FORMAT T "~%~S is a closure of ~S:~%" CL (CAR C))
    (DO L (CDR C) (CDDR L) (NULL L)
	(SETQ SYM (%FIND-STRUCTURE-HEADER (CAR L))
	      OFFSET (%POINTER-DIFFERENCE (CAR L) SYM))
	(FORMAT T
		"   ~A cell of ~S:        ~32,7S~%"
		(SELECTQ OFFSET
		  (0 "Print name") (1 "Value") (2 "Function")
		  (3 "Property list") (4 "Package"))
		SYM
		(COND ((= (%P-DATA-TYPE (CADR L)) DTP-NULL)
		       "unbound.")
		      (T (CAADR L)))))
    (DESCRIBE-1 (CAR C))
    ))

(DEFUN DESCRIBE-SELECT-METHOD (M)
  (FORMAT T "~%~S handles:" M)
  (DO ((ML (%MAKE-POINTER DTP-LIST M) (CDR ML)))
      ((ATOM ML)
       (COND (ML
	      (FORMAT T "~%   anything else to ~S" ML)
	      (COND ((SYMBOLP ML)
		     (AND (BOUNDP ML) (FORMAT T "  -> ~S" (SYMEVAL ML))) ;probably a class
		     )))))
    (COND ((ATOM (CAR ML)) (FORMAT T "~%   subroutine ~S" (CAR ML)))
          (T (FORMAT T "~%   ~S: ~34T~S" (CAAR ML)
		     (IF (= (%DATA-TYPE (CDAR ML)) DTP-FEF-POINTER)
			 (%P-CONTENTS-OFFSET (CDAR ML) %FEFHI-FCTN-NAME)
			 (CDAR ML)))))))

(DEFUN DESCRIBE-SMALL-FLONUM (X)
  (FORMAT T "~%~S is a small flonum.~%  " X)
  (FORMAT T "Excess-100 exponent ~O, 17-bit mantissa ~O (with sign bit deleted)"
	    (LDB 2107 (%POINTER X)) (LDB 0021 (%POINTER X))))

(DEFUN DESCRIBE-FLONUM (X)
  (FORMAT T "~%~S is a flonum.~%  " X)
  (FORMAT T "Excess-2000 exponent ~O, 32-bit mantissa ~O~4,48O~4,48O (including sign)"
	       (%P-LDB-OFFSET 1013 X 0)
	       (%P-LDB-OFFSET 0010 X 0)
	       (%P-LDB-OFFSET 1414 X 1)
	       (%P-LDB-OFFSET 0014 X 1)))

(defun describe-bignum (x)
  (let ((len (%p-ldb-offset #o0022 x 0))
	(barf nil))
    (format t "~&~S is a bignum.~&It is ~R word~:P long.  It is ~[positive~;negative~].  ~
                 It is stored starting at location: ~O~&Its contents:~2%"
	    x len (%p-ldb-offset #o2201 x 0) (%pointer x))
    (do ((i 1 (1+ i)))
	((> i len))
      (or (zerop (%p-ldb-offset #o3701 x i))
	  (setq barf t))
      (format t "~&~3O: ~[ ~;*~]"
	      i (%p-ldb-offset #o3701 x i))
      (do ((ppss #o3601 (- ppss #o0100)))
	  ((< ppss #o0001))
	(tyo (+ #/0 (%p-ldb-offset ppss x i))))
      (format t "  ~O," (%p-ldb-offset #o3601 x i))
      (do ((ppss #o3303 (- ppss #o0300)))
	  ((< ppss #o0003))
	(tyo (+ #/0 (%p-ldb-offset ppss x i))))
      (princ "  ")
      (do ((ppss #o3403 (- ppss #o0300)))
	  ((< ppss #o0103))
	(tyo (+ #/0 (%p-ldb-offset ppss x i))))
      (format t ",~O  ~O," (%p-ldb-offset #o0001 x i) (%p-ldb-offset #o3502 x i))
      (do ((ppss #o3203 (- ppss #o0300)))
	  ((< ppss #o0203))
	(tyo (+ #/0 (%p-ldb-offset ppss x i))))
      (format t ",~O" (%p-ldb-offset #o0002 x i)))
    (if barf
	(format t "~2&* = high order bit illegally 1, bug in bignum microcode?"))
    (terpri))
  x)

(DEFUN DESCRIBE-AREA (AREA &AUX LENGTH USED N-REGIONS)
  (AND (NUMBERP AREA) (SETQ AREA (AREA-NAME AREA)))
  (DO AREA-NUMBER 0 (1+ AREA-NUMBER) (> AREA-NUMBER SIZE-OF-AREA-ARRAYS)
    (COND ((EQ AREA (AREA-NAME AREA-NUMBER))
	   (MULTIPLE-VALUE (LENGTH USED N-REGIONS) (ROOM-GET-AREA-LENGTH-USED AREA-NUMBER))
	   (FORMAT T "~%Area #~O: ~S has ~D region~P, max size ~O, region size ~O (octal):~%"
		     AREA-NUMBER AREA N-REGIONS N-REGIONS
		     (AREA-MAXIMUM-SIZE AREA-NUMBER) (AREA-REGION-SIZE AREA-NUMBER))
	   (DO ((REGION (AREA-REGION-LIST AREA-NUMBER) (REGION-LIST-THREAD REGION))
		(BITS))
	       ((MINUSP REGION))
	     (SETQ BITS (REGION-BITS REGION))
	     (FORMAT T "  Region #~O: Origin ~O, Length ~O, Free ~O, GC ~O, Type ~A ~A, Map ~O,~[NoScav~;Scav~]~%"
		     REGION (REGION-ORIGIN-TRUE-VALUE REGION) (REGION-LENGTH REGION)
		     (REGION-FREE-POINTER REGION) (REGION-GC-POINTER REGION)
		     (NTH (LDB %%REGION-REPRESENTATION-TYPE BITS)
			  '(LIST STRUC "REP=2" "REP=3"))
		     (NTH (LDB %%REGION-SPACE-TYPE BITS)
			  '(FREE OLD NEW NEW1 NEW2 NEW3 NEW4 NEW5 NEW6
			    STATIC FIXED EXTRA-PDL COPY "TYPE=15" "TYPE=16" "TYPE=17"))
		     (LDB %%REGION-MAP-BITS BITS)
                     (LDB %%REGION-SCAVENGE-ENABLE BITS)))
	   (RETURN T)))))

(DEFVAR RANDOM-ARRAY)

(DEFSTRUCT (RANDOM-NUMBER-TABLE ARRAY-LEADER)
    RANDOM-FILL-POINTER
    RANDOM-SEED
    RANDOM-POINTER-1
    RANDOM-POINTER-2)

(DEFUN RANDOM-CREATE-ARRAY (SIZE OFFSET SEED &OPTIONAL (AREA NIL))
    (LET ((ARRAY (MAKE-RANDOM-NUMBER-TABLE
		   MAKE-ARRAY (:AREA AREA
			       :TYPE 'ART-Q-LIST
			       :LENGTH SIZE)
		   RANDOM-FILL-POINTER SIZE
		   RANDOM-SEED SEED
		   RANDOM-POINTER-1 0
		   RANDOM-POINTER-2 OFFSET)))
      (RANDOM-INITIALIZE ARRAY)
      ARRAY))

(DEFUN RANDOM-INITIALIZE (ARRAY &OPTIONAL NEW-SEED &AUX SIZE X BYTE-SPEC POINTER)
   (IF (NOT (NULL NEW-SEED))
       (SETF (RANDOM-SEED ARRAY) NEW-SEED))
   (SETQ SIZE (RANDOM-FILL-POINTER ARRAY)
	 POINTER (AP-1 ARRAY 0))
   (SETF (RANDOM-POINTER-2 ARRAY) (\ (+ SIZE (- (RANDOM-POINTER-2 ARRAY)
						(RANDOM-POINTER-1 ARRAY)))
				     SIZE))
   (SETF (RANDOM-POINTER-1 ARRAY) 0)
   (DO I 0 (1+ I) (= I SIZE)
     (ASET 0 ARRAY I))
   (SETQ X (RANDOM-SEED ARRAY))
   (DO L '(1414 0014) (CDR L) (NULL L)
     (SETQ BYTE-SPEC (CAR L))
     (DO I 0 (1+ I) (= I SIZE)
       (SETQ X (%24-BIT-TIMES X 4093.))			;4093. is a prime number.
       (%P-DPB-OFFSET (LDB 1314 X) BYTE-SPEC POINTER I))))

(DEFUN RANDOM (&OPTIONAL ARG ARRAY &AUX PTR1 PTR2 SIZE ANS)
    (COND ((NULL ARRAY)
	   (OR (BOUNDP 'RANDOM-ARRAY)
	       (SETQ RANDOM-ARRAY (RANDOM-CREATE-ARRAY 71. 35. 105)))
	   (SETQ ARRAY RANDOM-ARRAY)))	   ;INITIALIZATION AS OPT ARG LOSES ON BOUNDP.
    (WITHOUT-INTERRUPTS
      (SETQ PTR1 (RANDOM-POINTER-1 ARRAY)
	    PTR2 (RANDOM-POINTER-2 ARRAY)
	    SIZE (RANDOM-FILL-POINTER ARRAY))
      (OR (< (SETQ PTR1 (1+ PTR1)) SIZE)
	  (SETQ PTR1 0))
      (OR (< (SETQ PTR2 (1+ PTR2)) SIZE)
	  (SETQ PTR2 0))
      (SETF (RANDOM-POINTER-1 ARRAY) PTR1)
      (SETF (RANDOM-POINTER-2 ARRAY) PTR2)
      (SETQ ANS (%24-BIT-PLUS (AR-1 ARRAY PTR1) (AR-1 ARRAY PTR2)))
      (ASET ANS ARRAY PTR2))
    (COND (ARG (\ (LOGAND ANS 37777777) ARG))   ;ASSURE POSITIVE ANSWER
	  (T ANS)))

;; Return a randomly chosen number at least LOW and less than HIGH.
(DEFUN RANDOM-IN-RANGE (LOW HIGH)
  (PROG* ((R (RANDOM))
	  (RNORM (// (LOGAND R 777777) (FLOAT 1000000))))
     (RETURN (+ LOW (* RNORM (- HIGH LOW))))))

(DEFUN SET-MEMORY-SIZE (NEW-SIZE)
  (PROG (OLD-SIZE NEWP OLDP)
	(COND ((< NEW-SIZE (+ (SYSTEM-COMMUNICATION-AREA %SYS-COM-WIRED-SIZE) 20000)) ;8K MIN
	       (FERROR NIL "~O is smaller than wired + 8K"  NEW-SIZE)))
    L   (SETQ OLD-SIZE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE))
        (SETQ OLDP (// (+ OLD-SIZE (1- PAGE-SIZE)) PAGE-SIZE))
        (SETQ NEWP (// (+ NEW-SIZE (1- PAGE-SIZE)) PAGE-SIZE))
	(COND ((OR (> NEWP (REGION-LENGTH PHYSICAL-PAGE-DATA))
		   (> NEWP (// (* 4 (REGION-LENGTH PAGE-TABLE-AREA)) 9)))
	       (FERROR NIL "~O is bigger than page tables allow"  NEW-SIZE))
	      ((= NEWP OLDP) (RETURN T))
              ((< NEWP OLDP) (GO FLUSH)))
  MORE  (COND ((%DELETE-PHYSICAL-PAGE OLD-SIZE)
               (PRINT (LIST OLD-SIZE "EXISTED"))))
        (%CREATE-PHYSICAL-PAGE OLD-SIZE)
	(STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)
	       (+ OLD-SIZE PAGE-SIZE))
        (GO L)

  FLUSH (COND ((NULL (%DELETE-PHYSICAL-PAGE (- OLD-SIZE PAGE-SIZE)))
               (PRINT (LIST (- OLD-SIZE PAGE-SIZE) "DID-NOT-EXIST"))))
	(STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)
	       (- OLD-SIZE PAGE-SIZE))
        (GO L)))

(DEFUN SET-ERROR-MODE (&OPTIONAL (CAR-SYM-MODE 1) (CDR-SYM-MODE 1)
			         (CAR-NUM-MODE 0) (CDR-NUM-MODE 0))
       (SETQ %MODE-FLAGS (%LOGDPB CAR-SYM-MODE %%M-FLAGS-CAR-SYM-MODE %MODE-FLAGS))
       (SETQ %MODE-FLAGS (%LOGDPB CDR-SYM-MODE %%M-FLAGS-CDR-SYM-MODE %MODE-FLAGS))
       (SETQ %MODE-FLAGS (%LOGDPB CAR-NUM-MODE %%M-FLAGS-CAR-NUM-MODE %MODE-FLAGS))
       (SETQ %MODE-FLAGS (%LOGDPB CDR-NUM-MODE %%M-FLAGS-CDR-NUM-MODE %MODE-FLAGS)))

(DEFUN PRINT-ERROR-MODE (&OPTIONAL (EM %MODE-FLAGS) (STREAM STANDARD-OUTPUT))
  (FORMAT STREAM
	  "CAR of a number is ~A.~%CDR of a number is ~A.~%CAR of a symbol is ~A.~%CDR of a symbol is a ~A.~%Trapping is ~A.~%"
	  (SELECTQ (LDB %%M-FLAGS-CAR-NUM-MODE EM)
	      (0 "an error")
	      (1 "NIL")
	      (OTHERWISE "in an unknown state"))
	  (SELECTQ (LDB %%M-FLAGS-CDR-NUM-MODE EM)
	      (0 "an error")
	      (1 "NIL")
	      (OTHERWISE "in an unknown state"))
	  (SELECTQ (LDB %%M-FLAGS-CAR-SYM-MODE EM)
	      (0 "an error")
	      (1 "NIL if the symbol is NIL, otherwise an error")
	      (2 "NIL")
	      (3 "its print-name"))
	  (SELECTQ (LDB %%M-FLAGS-CDR-SYM-MODE EM)
	      (0 "an error")
	      (1 "NIL if the symbol is NIL, otherwise an error")
	      (2 "NIL")
	      (3 "its property list"))
	  (SELECTQ (LDB %%M-FLAGS-TRAP-ENABLE EM)
	      (0 "disabled")
	      (1 "enabled"))
	  ))

(DECLARE (SPECIAL APROPOS-SUBSTRING))

(LOCAL-DECLARE ((SPECIAL RETURN-LIST))
(DEFUN APROPOS (APROPOS-SUBSTRING &OPTIONAL PKG (DO-INFERIORS T) DO-SUPERIORS
		&AUX RETURN-LIST)
  (SETQ PKG (IF (NULL PKG) PKG-GLOBAL-PACKAGE (PKG-FIND-PACKAGE PKG)))
  (MAPATOMS #'APROPOS-1 PKG DO-SUPERIORS)
  (AND DO-INFERIORS
       (DOLIST (P (PKG-SUBPACKAGES PKG))
	 (MAPATOMS-ALL #'APROPOS-1 P)))
  RETURN-LIST)

(DEFUN APROPOS-1 (SYMBOL)
  (COND ((STRING-SEARCH APROPOS-SUBSTRING (GET-PNAME SYMBOL))
	 (PUSH SYMBOL RETURN-LIST)
	 ;; Binding the package to NIL forces the package to be printed.
	 ;; This is better than explicitly printing the package, because
	 ;; this way you get the "short" version.
	 (LET ((PACKAGE NIL))
	   (FORMAT T "~%~S" SYMBOL))
	 (AND (FBOUNDP SYMBOL)
	      (FORMAT T " - Function ~:S" (ARGLIST SYMBOL)))
	 (AND (BOUNDP SYMBOL)
	      (COND ((FBOUNDP SYMBOL) (PRINC ", Bound"))
		    (T (PRINC " - Bound")))))))
);End of LOCAL-DECLARE

(DEFUN SYMEVAL-IN-CLOSURE (CLOSURE PTR)
  (CHECK-ARG CLOSURE (OR (CLOSUREP CLOSURE) (ENTITYP CLOSURE)) "a closure or an entity")
  (CHECK-ARG PTR (COND ((SYMBOLP PTR) (SETQ PTR (VALUE-CELL-LOCATION PTR)))
		       ((= (%DATA-TYPE PTR) DTP-LOCATIVE)))
	         "a symbol or a locative")
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       (CAR PTR))
    (AND (EQ (CAR L) PTR)
	 (RETURN (CAADR L)))))

(DEFUN LOCATE-IN-CLOSURE (CLOSURE PTR)
  (CHECK-ARG CLOSURE (OR (CLOSUREP CLOSURE) (ENTITYP CLOSURE)) "a closure or an entity")
  (CHECK-ARG PTR (COND ((SYMBOLP PTR) (SETQ PTR (VALUE-CELL-LOCATION PTR)))
		       ((= (%DATA-TYPE PTR) DTP-LOCATIVE)))
	         "a symbol or a locative")
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       PTR)
    (AND (EQ (CAR L) PTR)
	 (RETURN (CADR L)))))

(DEFUN SET-IN-CLOSURE (CLOSURE PTR VAL)
  (CHECK-ARG CLOSURE (OR (CLOSUREP CLOSURE) (ENTITYP CLOSURE)) "a closure or an entity")
  (CHECK-ARG PTR (COND ((SYMBOLP PTR) (SETQ PTR (VALUE-CELL-LOCATION PTR)))
		       ((= (%DATA-TYPE PTR) DTP-LOCATIVE)))
	         "a symbol or a locative")
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       (RPLACA PTR VAL))
    (COND ((EQ (CAR L) PTR)
	   (RETURN (RPLACA (CADR L) VAL)))))
  VAL)

(DEFUN CLOSUREP (X)
    (= (%DATA-TYPE X) DTP-CLOSURE))

(DEFUN ENTITYP (X)
    (= (%DATA-TYPE X) DTP-ENTITY))

;ARRAY-POP, eventually to be micro-coded
;UNDOES (ARRAY-PUSH ARRAY <DATA>) AND RETURNS <DATA>
(DEFUN ARRAY-POP (ARRAY)
  (PROG (IDX VAL ARRAY-TYPE (INHIBIT-SCHEDULING-FLAG T))
	(COND ((ZEROP (SETQ IDX (ARRAY-LEADER ARRAY 0)))
	       (FERROR NIL "~S Overpopped" ARRAY)))
	(SETQ ARRAY-TYPE (AR-1 (FUNCTION ARRAY-TYPES)
			       (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0)))
	(SETQ VAL (AR-1 ARRAY (SETQ IDX (1- IDX))))	;1- BECAUSE IDX IS # ACTIVE ELEMENTS
	(COND ((MEMQ ARRAY-TYPE '(ART-Q ART-Q-LIST))
	       (AS-1 NIL ARRAY IDX)))   ;FLUSH SO NOT THERE FOR GC (HA HA)
	(STORE-ARRAY-LEADER IDX ARRAY 0)
	(COND ((AND (EQ ARRAY-TYPE 'ART-Q-LIST)
		    (NOT (ZEROP IDX)))
	       (%P-DPB CDR-NIL %%Q-CDR-CODE (AP-1 ARRAY (1- IDX)))))
	(RETURN VAL)))

;;; The following definitions of FILLARRAY and LISTARRAY should be completely
;;; compatible with Maclisp.  Slow, maybe, but compatible.

;;; When filling from an array, extra elements in the destination get the default initial
;;; value for the array type.  When filling from a list it sticks at the last element.
;;; Extra elements in the source are ignored.  copy-array-contents
;;; does the right thing for one-d arrays, but for multi-dimensional arrays
;;; uses column-major rather than row-major order.

(DEFRESOURCE FILLARRAY-INDEX-ARRAYS ()
	:CONSTRUCTOR (MAKE-ARRAY 10)
	:INITIAL-COPIES 2)

(DEFUN FILLARRAY (ARRAY SOURCE)
  (LET ((DEST (IF (SYMBOLP ARRAY) (FSYMEVAL ARRAY) ARRAY)))
    (CHECK-ARG ARRAY (ARRAYP DEST) "an array or a symbol FBOUND to an array")
    ;; Note, I really mean LISTP here -- Maclisp does not allow NIL, and that is right.
    ;; Well, there is code in the system that depends on the empty list working as a source,
    ;; at least for zero-length arrays.  This code says filling from () means fill
    ;; with the default initial value for the destination array type.
    (CHECK-ARG SOURCE (OR (ARRAYP SOURCE) (LISTP SOURCE) (NULL SOURCE)) "an array or a list")
    (LET ((DEST-NDIMS (ARRAY-#-DIMS DEST))
	  (SOURCE-IS-AN-ARRAY-P (ARRAYP SOURCE)))
      (COND (SOURCE-IS-AN-ARRAY-P
	     (LET ((SOURCE-NDIMS (ARRAY-#-DIMS SOURCE)))
	       (COND ((AND (= DEST-NDIMS 1)
			   (= SOURCE-NDIMS 1))
		      ;; One-D array into a one-D array is in microcode!
		      (LET ((N-ELEMENTS (MIN (ARRAY-LENGTH SOURCE)
					     (ARRAY-LENGTH DEST))))
			(COPY-ARRAY-PORTION SOURCE 0 N-ELEMENTS DEST 0 N-ELEMENTS)))
		     (T
		      ;; Hairy case, some array is multi-dimensional.
		      (USING-RESOURCE (SOURCE-INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
			(USING-RESOURCE (DEST-INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
			  (DOTIMES (I 10)
			    (ASET 0 SOURCE-INDEX-ARRAY I)
			    (ASET 0 DEST-INDEX-ARRAY I))
			  (LET ((SOURCE-ELEMENTS (ARRAY-LENGTH SOURCE))
				(DEST-ELEMENTS (ARRAY-LENGTH DEST)))
			    (DOTIMES (I (MIN SOURCE-ELEMENTS DEST-ELEMENTS))
			      (FILLARRAY-PUT (FILLARRAY-GET SOURCE
							    SOURCE-INDEX-ARRAY
							    SOURCE-NDIMS)
					     DEST DEST-INDEX-ARRAY DEST-NDIMS)))))))))
	    ((NULL SOURCE) (COPY-ARRAY-PORTION DEST 0 0 DEST 0 (ARRAY-LENGTH DEST)))
	    (T
	     ;; Source is a list.
	     (COND ((= DEST-NDIMS 1)
		    (DOTIMES (X (ARRAY-DIMENSION-N 1 DEST))
		      (ASET (CAR SOURCE) DEST X)
		      (IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE)))))
		   ((= DEST-NDIMS 2)
		    (DOTIMES (X (ARRAY-DIMENSION-N 1 DEST))
		      (DOTIMES (Y (ARRAY-DIMENSION-N 2 DEST))
			(ASET (CAR SOURCE) DEST X Y)
			(IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE))))))
		   ((= DEST-NDIMS 3)
		    (DOTIMES (X (ARRAY-DIMENSION-N 1 DEST))
		      (DOTIMES (Y (ARRAY-DIMENSION-N 2 DEST))
			(DOTIMES (Z (ARRAY-DIMENSION-N 3 DEST))
			  (ASET (CAR SOURCE) DEST X Y Z)
			  (IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE)))))))
		   (T
		    (USING-RESOURCE (DEST-INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
		      (DOTIMES (I 10)
			(ASET 0 DEST-INDEX-ARRAY I))
		      (DOTIMES (I (ARRAY-LENGTH DEST))
			(FILLARRAY-PUT (CAR SOURCE) DEST DEST-INDEX-ARRAY DEST-NDIMS)
			(IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE)))))))))))
  ARRAY)

(DEFUN FILLARRAY-GET (ARRAY INDEX-ARRAY NDIMS)
  (%OPEN-CALL-BLOCK ARRAY 0 1)			;d-stack
  (%ASSURE-PDL-ROOM NDIMS)
  (DOTIMES (I NDIMS)
    (%PUSH (AREF INDEX-ARRAY I)))
  (%ACTIVATE-OPEN-CALL-BLOCK)
  (FILLARRAY-INCREMENT-INDEX ARRAY INDEX-ARRAY NDIMS)
  (%POP))

(DEFUN FILLARRAY-PUT (VALUE ARRAY INDEX-ARRAY NDIMS)
  (%OPEN-CALL-BLOCK ARRAY 0 0)			;d-ignore
  (%ASSURE-PDL-ROOM NDIMS)
  (DOTIMES (I NDIMS)
    (%PUSH (AREF INDEX-ARRAY I)))
  (%ACTIVATE-OPEN-CALL-BLOCK)
  (STORE NIL VALUE)
  (FILLARRAY-INCREMENT-INDEX ARRAY INDEX-ARRAY NDIMS))

(DEFUN FILLARRAY-INCREMENT-INDEX (ARRAY INDEX-ARRAY NDIMS)
  (DO ((DIM NDIMS (1- DIM)))
      (( DIM 0))
    (LET ((VAL (1+ (AREF INDEX-ARRAY (1- DIM)))))
      (COND ((< VAL (ARRAY-DIMENSION-N DIM ARRAY))
	     (ASET VAL INDEX-ARRAY (1- DIM))
	     (RETURN))
	    (T
	     (ASET 0 INDEX-ARRAY (1- DIM)))))))

;;; LISTARRAY of a one-dimensional array respects the fill pointer, but
;;; for multi-dimensional arrays it ignores the fill pointer.
(DEFUN LISTARRAY (ARRAY &OPTIONAL LIMIT)
  (IF (SYMBOLP ARRAY)
      (SETQ ARRAY (FSYMEVAL ARRAY)))
  (CHECK-ARG ARRAY ARRAYP "an array or a symbol FBOUND to an array")
  (CHECK-ARG LIMIT (OR (NULL LIMIT) (FIXP LIMIT)) "NIL or a fixnum")
  (LET* ((NDIMS (ARRAY-#-DIMS ARRAY))
	 (ELEMENTS (IF (= NDIMS 1)
		       (ARRAY-ACTIVE-LENGTH ARRAY)
		       (ARRAY-LENGTH ARRAY)))
	 (TIMES (IF (NULL LIMIT)
		    ELEMENTS
		    (MIN LIMIT ELEMENTS)))
	 (LIST (MAKE-LIST TIMES))
	 (L LIST)
	 (COUNT 0))
    (COND ((= NDIMS 1)
	   (DOTIMES (X (ARRAY-ACTIVE-LENGTH ARRAY))
	     (SETQ COUNT (1+ COUNT))
	     (IF (> COUNT TIMES)
		 (RETURN))
	     (RPLACA L (AREF ARRAY X))
	     (SETQ L (CDR L))))
	  ((= NDIMS 2)
	   (DOTIMES (X (ARRAY-DIMENSION-N 1 ARRAY))
	     (DOTIMES (Y (ARRAY-DIMENSION-N 2 ARRAY))
	       (SETQ COUNT (1+ COUNT))
	       (IF (> COUNT TIMES)
		   (RETURN))
	       (RPLACA L (AREF ARRAY X Y))
	       (SETQ L (CDR L)))))
	  ((= NDIMS 3)
	   (DOTIMES (X (ARRAY-DIMENSION-N 1 ARRAY))
	     (DOTIMES (Y (ARRAY-DIMENSION-N 2 ARRAY))
	       (DOTIMES (Z (ARRAY-DIMENSION-N 3 ARRAY))
		 (SETQ COUNT (1+ COUNT))
		 (IF (> COUNT TIMES)
		     (RETURN))
		 (RPLACA L (AREF ARRAY X Y Z))
		 (SETQ L (CDR L))))))
	  (T
	   (USING-RESOURCE (INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
	     (DOTIMES (I 10) (ASET 0 INDEX-ARRAY I))
	     (DOTIMES (I TIMES)
	       (RPLACA L (FILLARRAY-GET ARRAY INDEX-ARRAY NDIMS))
	       (SETQ L (CDR L))))))
    LIST))

(DEFUN LIST-ARRAY-LEADER (ARRAY &OPTIONAL LIMIT &AUX LST)
       (AND (SYMBOLP ARRAY) (SETQ ARRAY (FSYMEVAL ARRAY)))
       (OR LIMIT
	   (SETQ LIMIT (OR (ARRAY-LEADER-LENGTH ARRAY) 0)))
       (SETQ LST (MAKE-LIST DEFAULT-CONS-AREA LIMIT))
       (DO ((I 0 (1+ I))
	    (L LST (CDR L)))
	   ((>= I LIMIT)
	    LST)
	   (RPLACA L (ARRAY-LEADER ARRAY I))))

(DEFUN *RSET (&OPTIONAL (NEW-MODE T))
    (SETQ *RSET NEW-MODE))

(DEFUN ARRAY-/#-DIMS (ARRAY)
    (CHECK-ARG ARRAY ARRAYP "an array")
    (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0))

(DEFUN DATA-TYPE (X)
    (AR-1 (FSYMEVAL 'Q-DATA-TYPES) (%DATA-TYPE X)))

;;; Facilities for looking through all functions in the world
;;; and finding out what they do.

(LOCAL-DECLARE ((SPECIAL RETURN-LIST))
(DEFUN WHO-CALLS (SYMBOL &OPTIONAL PKG (DO-INFERIORS T) (DO-SUPERIORS T) &AUX RETURN-LIST)
  (OR PKG (SETQ PKG PKG-GLOBAL-PACKAGE))
  (FIND-CALLERS-OF-SYMBOLS SYMBOL PKG
	#'(LAMBDA (CALLER CALLEE HOW)
	    (FORMAT T "~&~S" CALLER)
	    (FORMAT T (SELECTQ HOW
			(:VARIABLE " uses ~S as a variable.")
			(:FUNCTION " calls ~S as a function.")
			(:MISC-FUNCTION " calls ~S via a 'misc' instruction.")
			(:CONSTANT " uses ~S as a constant.")
			(:UNBOUND-FUNCTION " calls ~S, an undefined function.")
			(NIL ", an interpreted function, uses ~S somehow."))
		    CALLEE)
	    (PUSH CALLER RETURN-LIST)) DO-INFERIORS DO-SUPERIORS)
  RETURN-LIST)
)

(DEFF WHO-USES 'WHO-CALLS)

(DEFUN WHAT-FILES-CALL (SYMBOL-OR-SYMBOLS &OPTIONAL PKG (DO-INFERIORS T) (DO-SUPERIORS T))
  (OR PKG (SETQ PKG PKG-GLOBAL-PACKAGE))
  (LOCAL-DECLARE ((SPECIAL L))
    (LET ((L NIL))
      (FIND-CALLERS-OF-SYMBOLS SYMBOL-OR-SYMBOLS PKG
	#'(LAMBDA (CALLER IGNORE IGNORE)
	    (AND (SETQ CALLER (GET-SOURCE-FILE-NAME CALLER 'DEFUN))
		 (NOT (MEMQ CALLER L))
		 (PUSH CALLER L)))
	DO-INFERIORS DO-SUPERIORS)
      L)))

(LOCAL-DECLARE ((SPECIAL SYMBOL FUNCTION))
(DEFUN FIND-CALLERS-OF-SYMBOLS (SYMBOL PKG FUNCTION
				&OPTIONAL (DO-INFERIORS T) (DO-SUPERIORS T))
  "This is the main driving function for WHO-CALLS and friends.
   Looks at all symbols in PKG and its inferiors and its superiors (does not
   do its sisters, cousins, and aunts).
   Looks at each symbol's function definition and if it
   refers to SYMBOL calls FUNCTION with the function name, the symbol used,
   and the type of use (:VARIABLE, :FUNCTION, :MISC-FUNCTION, :CONSTANT, :UNBOUND-FUNCTION,
   or NIL if used in an unknown way in an interpreted function.)
   SYMBOL can be a single symbol or a list of symbols.
   The symbol :UNBOUND-FUNCTION is treated specially."
  ;; Sorting first, in order of function definitions, didn't help much when
  ;; tried in the previous generation of this function.
  (SETQ PKG (PKG-FIND-PACKAGE PKG))
  (CHECK-ARG SYMBOL
	     (OR (SYMBOLP SYMBOL)
		 (LOOP FOR SYM IN SYMBOL ALWAYS (SYMBOLP SYM)))
	     "a symbol or a list of symbols")
  (IF (SYMBOLP SYMBOL)
      (SETQ SYMBOL (ADD-SYMBOLS-OPTIMIZED-INTO SYMBOL SYMBOL))
      (DOLIST (SYM SYMBOL)
	(SETQ SYMBOL (ADD-SYMBOLS-OPTIMIZED-INTO SYM SYMBOL))))
  (MAPATOMS #'FIND-CALLERS-OF-SYMBOLS-AUX PKG DO-SUPERIORS)
  (AND DO-INFERIORS
       (DOLIST (P (PKG-SUBPACKAGES PKG))
	 (MAPATOMS-ALL #'FIND-CALLERS-OF-SYMBOLS-AUX P)))
  NIL)

(DEFUN ADD-SYMBOLS-OPTIMIZED-INTO (SYM LIST)
  (DOLIST (SYM1 (GET SYM 'COMPILER:OPTIMIZED-INTO))
    (IF (SYMBOLP LIST) (SETQ LIST (LIST LIST)))
    (OR (MEMQ SYM1 LIST)
	(SETQ LIST (ADD-SYMBOLS-OPTIMIZED-INTO SYM1 (CONS SYM1 LIST)))))
  LIST)

(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX (CALLER &AUX FL)
  ;; Ignore all symbols which are forwarded to others, to avoid duplication.
  (AND ( (%P-LDB-OFFSET %%Q-DATA-TYPE CALLER 2) DTP-ONE-Q-FORWARD)
       (FBOUNDP CALLER)
       (FIND-CALLERS-OF-SYMBOLS-AUX1 CALLER (FSYMEVAL CALLER)))
  (COND (( (%P-LDB-OFFSET %%Q-DATA-TYPE CALLER 3) DTP-ONE-Q-FORWARD)
	 ;; Also look for properties
	 (DO ((L (PLIST CALLER) (CDDR L)))
	     ((NULL L))
	   (COND ((= (%DATA-TYPE (CADR L)) DTP-FEF-POINTER)
		  (FIND-CALLERS-OF-SYMBOLS-AUX-FEF
		    (LIST ':PROPERTY CALLER (CAR L)) (CADR L)))))
	 ;; Also look for flavor methods
	 (AND (SETQ FL (GET CALLER 'FLAVOR))
	      (ARRAYP FL)		;Could be T
	      (DOLIST (MTE (FLAVOR-METHOD-TABLE FL))
		(DOLIST (METH (CDDDR MTE))
		  (IF (METH-DEFINEDP METH)
		      (FIND-CALLERS-OF-SYMBOLS-AUX1 (METH-FUNCTION-SPEC METH)
						    (METH-DEFINITION METH))))))
	 ;; Also look for initializations
	 (IF (GET CALLER 'INITIALIZATION-LIST)
	     ;; It is an initialization list.
	     (DOLIST (INIT-LIST-ENTRY (SYMEVAL CALLER))
	       (FIND-CALLERS-OF-SYMBOLS-AUX-LIST CALLER (INIT-FORM INIT-LIST-ENTRY)))))))

(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX1 (CALLER DEFN)
  ;; Don't be fooled by macros, interpreted or compiled.
  (AND (LISTP DEFN) (EQ (CAR DEFN) 'MACRO) (SETQ DEFN (CDR DEFN)))
  (COND ((LISTP DEFN)
	 (FIND-CALLERS-OF-SYMBOLS-AUX-LIST CALLER DEFN))
	((= (%DATA-TYPE DEFN) DTP-FEF-POINTER)
	 (FIND-CALLERS-OF-SYMBOLS-AUX-FEF CALLER DEFN)))
  ;; If this function is traced, advised, etc.
  ;; then look through the actual definition.
  (LET* ((DEBUG-INFO (FUNCTION-DEBUGGING-INFO DEFN))
	 (INNER (ASSQ 'SI:ENCAPSULATED-DEFINITION DEBUG-INFO)))
    (AND INNER (FIND-CALLERS-OF-SYMBOLS-AUX (CADR INNER)))))
		 
(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX-FEF (CALLER DEFN &AUX TEM OFFSET SYM)
  (DO ((I %FEF-HEADER-LENGTH (1+ I))
       (LIM (// (FEF-INITIAL-PC DEFN) 2)))
      ((>= I LIM) NIL)
    (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE DEFN I) DTP-EXTERNAL-VALUE-CELL-POINTER)
	   (SETQ TEM (%P-CONTENTS-AS-LOCATIVE-OFFSET DEFN I)
		 SYM (%FIND-STRUCTURE-HEADER TEM)
		 OFFSET (%POINTER-DIFFERENCE TEM SYM))
	   (COND ((NOT (SYMBOLP SYM)))
		 ((= OFFSET 2)			;Function cell reference
		  (IF (IF (ATOM SYMBOL) (EQ SYM SYMBOL) (MEMQ SYM SYMBOL))
		      (FUNCALL FUNCTION CALLER SYM ':FUNCTION)
		      (AND (IF (ATOM SYMBOL) (EQ ':UNBOUND-FUNCTION SYMBOL)
			       (MEMQ ':UNBOUND-FUNCTION SYMBOL))
			   (NOT (FBOUNDP SYM))
			   (FUNCALL FUNCTION CALLER SYM ':UNBOUND-FUNCTION))))
		 (T				;Value reference presumably
		  (IF (IF (ATOM SYMBOL) (EQ SYM SYMBOL) (MEMQ SYM SYMBOL))
		      (FUNCALL FUNCTION CALLER SYM ':VARIABLE)))))
	  ((SYMBOLP (SETQ SYM (%P-CONTENTS-OFFSET DEFN I)))
	   (IF (IF (ATOM SYMBOL) (EQ SYM SYMBOL) (MEMQ SYM SYMBOL))
	       (FUNCALL FUNCTION CALLER SYM ':CONSTANT)))))
  ;; See if we have a function reference compiled into a misc instruction
  ;; This won't work for LIST and LIST-IN-AREA
  (IF (SYMBOLP SYMBOL)
      (IF (FEF-CALLS-MISC-FUNCTION DEFN SYMBOL)
	  (FUNCALL FUNCTION CALLER SYMBOL ':MISC-FUNCTION))
      (DOLIST (SYM SYMBOL)
	(IF (FEF-CALLS-MISC-FUNCTION DEFN SYM)
	    (FUNCALL FUNCTION CALLER SYM ':MISC-FUNCTION))))
  (AND (LDB-TEST %%FEFHI-MS-DEBUG-INFO-PRESENT
		 (%P-CONTENTS-OFFSET DEFN %FEFHI-MISC))
       (SETQ TEM (CDR (ASSQ ':INTERNAL-FEF-OFFSETS
			    (%P-CONTENTS-OFFSET DEFN (1- (%P-LDB %%FEFH-PC-IN-WORDS DEFN))))))
       (LOOP FOR OFFSET IN TEM
	     FOR I FROM 0
	     DO (FIND-CALLERS-OF-SYMBOLS-AUX-FEF `(:INTERNAL ,CALLER ,I)
						 (%P-CONTENTS-OFFSET DEFN OFFSET)))))

;;; See if this FEF uses a certain MISC instruction
(DEFUN FEF-CALLS-MISC-FUNCTION (FEF SYM &AUX TEM INST)
  (AND (GET SYM 'COMPILER:QINTCMP)
       (SETQ TEM (GET SYM 'COMPILER:QLVAL))
       (DO ((MISCINST (+ 15_11 TEM))	;Misc instruction sought
	    (MISCMASK 17777)		;Masks out destination
	    (LONGJUMP 14777)		;First word of 2-word jump instruction
	    (PC (FEF-INITIAL-PC FEF) (1+ PC))
	    (MAXPC (* (FEF-LENGTH FEF) 2)))
	   ((>= PC MAXPC) NIL)
	 (SETQ INST (LOGAND (%P-LDB-OFFSET (IF (ODDP PC) %%Q-HIGH-HALF %%Q-LOW-HALF)
					   FEF (// PC 2))
			    MISCMASK))
	 (COND ((= INST MISCINST) (RETURN T))
	       ((= INST LONGJUMP) (SETQ PC (1+ PC)))))))

;;; Tree-walk CALLER looking for FUNCTION.  CALLER should be the function name,
;;; and DEFN should be its definition.  Avoids listing symbols twice.
(LOCAL-DECLARE ((SPECIAL SUPPRESS))
(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX-LIST (CALLER DEFN)
  (LET ((SUPPRESS NIL))
    (FIND-CALLERS-OF-SYMBOLS-AUX-LIST1 CALLER DEFN)))

(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX-LIST1 (CALLER DEFN)
  (DO ((L DEFN (CDR L)))
      ((ATOM L))
    (COND ((AND (SYMBOLP (CAR L))
		(NOT (MEMQ (CAR L) SUPPRESS))
		(IF (ATOM SYMBOL) (EQ (CAR L) SYMBOL) (MEMQ (CAR L) SYMBOL)))
	   (PUSH (CAR L) SUPPRESS)
	   (FUNCALL FUNCTION CALLER (CAR L) NIL))
	  ((LISTP (CAR L))
	   (FIND-CALLERS-OF-SYMBOLS-AUX-LIST1 CALLER (CAR L))))))
);close inner LOCAL-DECLARE
);close LOCAL-DECLARE

(DEFUN %MAKE-PAGE-READ-ONLY (P)
  (%CHANGE-PAGE-STATUS P NIL (DPB 2 0603 (LDB %%REGION-MAP-BITS  ;CHANGE MAP-STATUS
                                              (REGION-BITS (%REGION-NUMBER P))))))
;MAR-HACKING FUNCTIONS

(DEFUN CLEAR-MAR ()
  (DO ((P %MAR-LOW (+ P 200)))
      ((> P %MAR-HIGH)) ;TROUBLE WITH NEGATIVE NUMBERS HERE!
    (%CHANGE-PAGE-STATUS P NIL (LDB %%REGION-MAP-BITS
				    (REGION-BITS (%REGION-NUMBER P)))))
  (SETQ %MAR-LOW -1
	%MAR-HIGH -2
	%MODE-FLAGS (%LOGDPB 0 %%M-FLAGS-MAR-MODE %MODE-FLAGS))
  NIL)

;NOT GC-SAFE, ADDITIONAL HAIR REQUIRED, ALSO NEGATIVE NUMBER TROUBLE
(DEFUN SET-MAR (LOCATION CYCLE-TYPE &OPTIONAL (N-WORDS 1))
					;N-WORDS SHOULD DEFAULT TO (SIZE LOCATION)
  (SETQ CYCLE-TYPE
	(SELECTQ CYCLE-TYPE
	   (:READ 1)
	   (:WRITE 2)
	   ((T) 3)  ;Parens around the T so it doesn't look like an otherwise
	   (OTHERWISE (FERROR NIL "~S is not a valid CYCLE-TYPE" CYCLE-TYPE))))
  (CLEAR-MAR) ;CLEAR OLD MAR
  (SETQ %MAR-HIGH (+ (1- N-WORDS) (SETQ %MAR-LOW (%POINTER LOCATION))))
  ;IF MAR'ED PAGES ARE IN CORE, SET UP THEIR TRAPS
  (DO P %MAR-LOW (+ P 200) (> P %MAR-HIGH)
    (%CHANGE-PAGE-STATUS P NIL (DPB 6 0604 (LDB %%REGION-MAP-BITS  ;CHANGE MAP-STATUS
						(REGION-BITS (%REGION-NUMBER P))))))
  (SETQ %MODE-FLAGS (%LOGDPB CYCLE-TYPE %%M-FLAGS-MAR-MODE %MODE-FLAGS))	;ENERGIZE
  T)

(DEFUN MAR-MODE ()
   (LET ((MODE (LDB %%M-FLAGS-MAR-MODE %MODE-FLAGS)))
     (SELECTQ MODE
	(0 'NIL)
	(1 ':READ)
	(2 ':WRITE)
	(3 'T)
	(OTHERWISE (FERROR NIL "The MAR mode, ~O, is invalid." MODE)))))

(DEFUN PAIRLIS (VARS VALS &AUX ALST)
       (SETQ ALST (MAKE-LIST DEFAULT-CONS-AREA (LENGTH VARS)))
       (DO ((VARS VARS (CDR VARS))
            (VALS VALS (CDR VALS))
            (TEM ALST (CDR TEM)))
           ((NULL VARS) ALST)
           (RPLACA TEM (CONS (CAR VARS) (CAR VALS)))))

(DEFUN DEL-IF-NOT (PRED LIST)
       (PROG (LST OLST)
        A    (COND ((ATOM LIST) (RETURN LIST))
                   ((FUNCALL PRED (CAR LIST)))
                   (T
                    (SETQ LIST (CDR LIST))
                    (GO A)))
             (SETQ OLST (SETQ LST LIST))
        B    (SETQ LST (CDR LST))
             (COND ((ATOM LST) (RETURN LIST))
                   ((FUNCALL PRED (CAR LST))
                    (SETQ OLST LST))
                   (T
                    (RPLACD OLST (CDR LST))))
             (GO B)))

(DEFUN DEL-IF (PRED LIST)
       (PROG (LST OLST)
        A    (COND ((ATOM LIST) (RETURN LIST))
                   ((FUNCALL PRED (CAR LIST))
                    (SETQ LIST (CDR LIST))
                    (GO A)))
             (SETQ OLST (SETQ LST LIST))
        B    (SETQ LST (CDR LST))
             (COND ((ATOM LST) (RETURN LIST))
                   ((FUNCALL PRED (CAR LST))
                    (RPLACD OLST (CDR LST)))
                   (T
                    (SETQ OLST LST)))
             (GO B)))

(DEFUN HAIPART (X N &AUX TEM)
  ;; Get number of significant bits
  (SETQ TEM (HAULONG (SETQ X (ABS X))))
  (COND ;; Positive N means get high N bits, or as many as there are
	((> N 0) (SETQ TEM (- N TEM))	;minus number of low bits to discard
		 (COND ((< TEM 0) (ASH X TEM))
		       (T X)))
	;; Zero N means return no bits
	((= N 0) 0)
	;; Negative N means get low -N bits, or as many as there are
	((< (SETQ N (MINUS N)) TEM)
	 (\ X (ASH 1 N)))
	(T X)))

(DEFUN PROGV (VARS VALS &QUOTE &REST STUFF)
  (DO-NAMED PROGV
	((VARS VARS (CDR VARS))
	 (VALS VALS (CDR VALS)))
	((NULL VARS)
	 (DO ((STUFF STUFF (CDR STUFF)))
	     (NIL)
	   (COND ((NULL (CDR STUFF))			;Pass multiple values (compiled anyway)
		  (RETURN-FROM PROGV (EVAL (CAR STUFF))))
		 ((EVAL (CAR STUFF))))))
    (BIND (VALUE-CELL-LOCATION (CAR VARS)) (CAR VALS))))

;;; (PROGW '((VAR-1 VAL-1) (VAR-2 VAL-2) ... (VAR-N VAL-N)) &BODY BODY)
;;; Binds VAR-I to VAL-I (evaluated) during execution of BODY
(DEFUN PROGW (VARS-AND-VALS &QUOTE &REST STUFF)
  (DO-NAMED PROGW
      ((VARS-AND-VALS VARS-AND-VALS (CDR VARS-AND-VALS)))
      ((NULL VARS-AND-VALS)
       (DO ((STUFF STUFF (CDR STUFF)))
	   (NIL)
	 (IF (NULL (CDR STUFF))
	     (RETURN-FROM PROGW (EVAL (CAR STUFF)))
	     (EVAL (CAR STUFF)))))
    (BIND (VALUE-CELL-LOCATION (CAAR VARS-AND-VALS))
	  (EVAL (CADAR VARS-AND-VALS)))))

;;; (LET-IF <COND> ((VAR-1 VAL-1) (VAR-2 VAL-2) ... (VAR-N VAL-N)) &BODY BODY)
;;; If <COND> is not nil, binds VAR-I to VAL-I (evaluated) during execution of BODY,
;;; otherwise just evaluates BODY.
(DEFUN LET-IF (COND &QUOTE VAR-LIST &QUOTE &REST STUFF)
  (PROGW (AND COND VAR-LIST)
     (DO ((STUFF STUFF (CDR STUFF)))
	 (NIL)
       (IF (NULL (CDR STUFF))
	   (RETURN (EVAL (CAR STUFF)))
	   (EVAL (CAR STUFF))))))

;Interpreter version of UNWIND-PROTECT
;(UNWIND-PROTECT risky-stuff forms-to-do-when-unwinding-this-frame...)
;If risky-stuff returns, we return what it returns, doing forms-to-do
;(just as PROG1 would do).  If risky-stuff does a throw, we let the throw
;function as specified, but make sure that forms-to-do get done as well.
(DEFUN UNWIND-PROTECT (&QUOTE BODY-FORM &REST CLEANUP-FORMS)
  (UNWIND-PROTECT (EVAL BODY-FORM)
    (DOLIST (FORM CLEANUP-FORMS)
      (EVAL FORM))))

;;; This should really be fixed to expand more than just top level functions.
(DEFUN MEXP ()
    (DO ((TEM))
	(())
      (FORMAT T "~2%Macro form ")
      (FUNCALL STANDARD-INPUT ':UNTYI (FUNCALL STANDARD-INPUT ':TYI))	;Allow abort to exit
      (*CATCH 'COMMAND-LEVEL		;Stay in mexp if abort out of input, **more**
	(SETQ TEM (READ-FOR-TOP-LEVEL))
	(AND (SYMBOLP TEM) (RETURN NIL))
	(DO EXP (MACROEXPAND-1 TEM) (MACROEXPAND-1 EXP) (EQ EXP TEM)
	  ;(FORMAT T "  ~S" (SETQ TEM EXP))
	  (PRINC "  ")
	  (GRIND-TOP-LEVEL (SETQ TEM EXP))
	  ))))


;; STATUS and SSTATUS 
;; Note that these have to be Maclisp compatible and therefore have to work
;; independent of packages.  All symbols on feature lists are in the keyword package.

(DEFVAR STATUS-FEATURE-LIST
	'(:SORT :FASLOAD :STRING :NEWIO :ROMAN :TRACE :GRINDEF :GRIND :LISPM))

(DEFVAR STATUS-STATUS-LIST '(:FEATURE :FEATURES :NOFEATURE :STATUS :SSTATUS :TABSIZE
			     :USERID :SITE :OPSYS))

(DEFVAR STATUS-SSTATUS-LIST '(:FEATURE :NOFEATURE))

(DEFUN RETURN-STATUS (STATUS-LIST ITEM ITEM-P)
       (COND ((NOT ITEM-P) STATUS-LIST)
	     (T (NOT (NULL (MEM #'STRING-EQUAL ITEM STATUS-LIST))))))

(DEFUN STATUS (&QUOTE STATUS-FUNCTION &OPTIONAL (ITEM NIL ITEM-P))
  (SELECTOR STATUS-FUNCTION STRING-EQUAL
    (('FEATURE 'FEATURES) (RETURN-STATUS STATUS-FEATURE-LIST ITEM ITEM-P))
    (('NOFEATURE) (COND ((NOT ITEM-P)
			 (FERROR NIL "Too few args to STATUS NOFEATURE"))
			(T (NOT (RETURN-STATUS STATUS-FEATURE-LIST ITEM ITEM-P)))))
    (('STATUS) (RETURN-STATUS STATUS-STATUS-LIST ITEM ITEM-P))
    (('SSTATUS) (RETURN-STATUS STATUS-SSTATUS-LIST ITEM ITEM-P))
    (('TABSIZE) 8)
    (('USERID) USER-ID)
    (('SITE) LOCAL-HOST-NAME)
    (('OPSYS) ':LISPM)
    (OTHERWISE (FERROR NIL "~S is not a legal STATUS request" STATUS-FUNCTION))))

(DEFUN SSTATUS (&QUOTE STATUS-FUNCTION ITEM
		&AUX (DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
  (SETQ ITEM (INTERN (STRING ITEM) ""))		;These are all keywords
  (SELECTOR STATUS-FUNCTION STRING-EQUAL
    (('FEATURE) (COND ((NOT (MEMQ ITEM STATUS-FEATURE-LIST))
		       (SETQ STATUS-FEATURE-LIST
			     (CONS ITEM STATUS-FEATURE-LIST))))
		ITEM)
    (('NOFEATURE) (COND ((MEMQ ITEM STATUS-FEATURE-LIST)
			 (SETQ STATUS-FEATURE-LIST
			       (DELQ ITEM STATUS-FEATURE-LIST))))
		  ITEM)
    (OTHERWISE (FERROR NIL "~S is not a legal SSTATUS request" STATUS-FUNCTION))))

;The ADD-OPTIMIZER for this is in QCOPT, so that things work in the cold-load
(DEFUN STATUS-OPTIMIZER (FORM)
  (LET ((STATUS-FUNCTION (CADR FORM))
	;(ITEM (CADDR FORM))
	(ITEM-P (CDDR FORM)))
    (SELECTOR STATUS-FUNCTION STRING-EQUAL
      (('FEATURE 'FEATURES) (IF ITEM-P FORM `STATUS-FEATURE-LIST))
      (('TABSIZE) `8)
      (('USERID) `USER-ID)
      (('SITE) `LOCAL-HOST-NAME)
      (('OPSYS) `':LISPM)
      (OTHERWISE (OR (MEM #'STRING-EQUAL STATUS-FUNCTION STATUS-STATUS-LIST)
		     (COMPILER:BARF FORM "Unknown STATUS function" 'COMPILER:WARN))
		 FORM))))

;;; Site stuff
(DEFVAR SITE-NAME)				;Setup by the cold load generator
(DEFVAR SITE-OPTION-ALIST NIL)

;;; This function is used to change the site in an already build world load.
;;; NEW-SITE is the site keyword, such as :MIT.
;;; SYS-HOST is the host that should be used as SYS: for loading the new site declaration.
;;; SYS-DIRECTORY is the directory to be used to getting the SITE file if this system
;;; doesn't follow the same directory naming convention as the original.
;;; HOST-TABLE-BOOTSTRAP is a filename to be loaded.  This is necessary either
;;; if SYS: is to point at a host not currently in the host table, or if there
;;; are file server hosts which are not SYS:.
(DEFUN SET-SITE (NEW-SITE &OPTIONAL SYS-HOST SYS-DIRECTORY HOST-TABLE-BOOTSTRAP)
  (SETQ STATUS-FEATURE-LIST (CONS NEW-SITE (DELQ SITE-NAME STATUS-FEATURE-LIST)))
  (SETQ SITE-NAME NEW-SITE)
  (AND HOST-TABLE-BOOTSTRAP (LOAD HOST-TABLE-BOOTSTRAP))
  (COND (SYS-HOST
	 (FS:CHANGE-LOGICAL-PATHNAME-HOST "SYS" SYS-HOST)
	 (SETQ SYS-HOST (FS:GET-PATHNAME-HOST SYS-HOST)))
	(T
	 (LET ((SYS-LOGICAL-HOST (FS:GET-PATHNAME-HOST "SYS")))
	   (AND SYS-LOGICAL-HOST (SETQ SYS-HOST (FUNCALL SYS-LOGICAL-HOST ':HOST))))))
  (AND SYS-HOST (FUNCALL SYS-HOST ':SET-SITE NEW-SITE))
  (AND SYS-DIRECTORY (FS:CHANGE-LOGICAL-PATHNAME-DIRECTORY "SYS" "SYS" SYS-DIRECTORY))
  (MAYBE-MINI-LOAD-FILE-ALIST SITE-FILE-ALIST)
  (INITIALIZATIONS 'SITE-INITIALIZATION-LIST T))

(DEFMACRO DEFSITE (SITE &BODY OPTIONS)
  `(DEFSITE-1 ',SITE ',OPTIONS))

(DEFUN DEFSITE-1 (NEW-SITE OPTIONS)
  (SETQ SITE-NAME NEW-SITE)
  (SETQ SITE-OPTION-ALIST (LOOP FOR (KEY EXP) IN OPTIONS
				COLLECT `(,KEY . ,(EVAL EXP)))))

(DEFUN GET-SITE-OPTION (KEY)
  (CDR (ASSQ KEY SITE-OPTION-ALIST)))

(DEFMACRO DEFINE-SITE-VARIABLE (VAR KEY)
  `(PROGN 'COMPILE
     (DEFVAR ,VAR)
     (ADD-INITIALIZATION ,(FORMAT NIL "SITE:~A" VAR)
			 `(SETQ ,',VAR (GET-SITE-OPTION ',',KEY))
			 '(SITE))))

(DEFMACRO DEFINE-SITE-HOST-LIST (VAR KEY)
  `(PROGN 'COMPILE
     (DEFVAR ,VAR)
     (ADD-INITIALIZATION ,(FORMAT NIL "SITE:~A" VAR)
			 `(SETQ ,',VAR (MAPCAR 'PARSE-HOST (GET-SITE-OPTION ',',KEY)))
			 '(SITE))))

;;; This NORMAL is so that it doesn't happen right away before enough is loaded to work.
(ADD-INITIALIZATION "HOST-TABLE-INITIALIZATION"
		    '(MAYBE-MINI-LOAD-FILE-ALIST HOST-TABLE-FILE-ALIST) '(SITE NORMAL))

;;; Interfaces to chaosnet physical support facilities
(DEFUN CALL-ELEVATOR ()
  (COND ((TECH-SQUARE-FLOOR-P 8)
	 (CHAOS:HACK-DOOR "8"))
	((TECH-SQUARE-FLOOR-P 9)
	 (CHAOS:HACK-DOOR "9"))
	(T (TV:NOTIFY NIL "I don't know how to get an elevator to your location."))))

(DEFUN BUZZ-DOOR ()
  (COND ((TECH-SQUARE-FLOOR-P 9) (CHAOS:HACK-DOOR "D"))
	(T (TV:NOTIFY NIL "I can only open the 9th floor door at Tech square"))))

(DEFUN TECH-SQUARE-FLOOR-P (FLOOR)
  (AND LOCAL-FLOOR-LOCATION
       (EQ (FIRST LOCAL-FLOOR-LOCATION) 'MIT-NE43)
       (= (SECOND LOCAL-FLOOR-LOCATION) FLOOR)))

;;; Stuff for function specs

;These are here because they must be loaded after the package system is operational
;(or maybe only because they aren't needed in the cold load?)

;This is useful for sorting function specs
(DEFUN FUNCTION-SPEC-LESSP (FS1 FS2)
  (STRING-LESSP (IF (SYMBOLP FS1) FS1 (SECOND FS1))
		(IF (SYMBOLP FS2) FS2 (SECOND FS2))))

(DEFUN FUNDEFINE (FUNCTION-SPEC &AUX TYPE)
  "Makes a function spec not have a function definition"
  ;; First, validate the function spec and determine its type
  (CHECK-ARG FUNCTION-SPEC
	     (SETQ TYPE (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC))
	     "a symbol or a function-spec list")
  (IF (SYMBOLP FUNCTION-SPEC) (FMAKUNBOUND FUNCTION-SPEC)
      (FUNCALL (GET TYPE 'FUNCTION-SPEC-HANDLER) 'FUNDEFINE FUNCTION-SPEC)))

(DEFUN FDEFINITION-LOCATION (FUNCTION-SPEC &AUX TYPE)
  "Returns a locative pointer to the cell containing the function spec's definition"
  ;; First, validate the function spec and determine its type
  (CHECK-ARG FUNCTION-SPEC
	     (SETQ TYPE (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC))
	     "a symbol or a function-spec list")
  (IF (SYMBOLP FUNCTION-SPEC) (LOCF (FSYMEVAL FUNCTION-SPEC))
      (FUNCALL (GET TYPE 'FUNCTION-SPEC-HANDLER) 'FDEFINITION-LOCATION FUNCTION-SPEC)))

(DEFUN FUNCTION-PARENT (FUNCTION-SPEC &AUX TYPE DEF TEM)
  (DECLARE (RETURN-LIST NAME TYPE))
  "Returns NIL or the name of another definition which has the same source code.
The second value is the type of that definition (which can be NIL).
This is used for things like internal functions, methods automatically
created by a defflavor, and macros automatically created by a defstruct."
  ;; First, validate the function spec and determine its type
  (CHECK-ARG FUNCTION-SPEC
	     (SETQ TYPE (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC))
	     "a symbol or a function-spec list")
  (COND ((AND (FDEFINEDP FUNCTION-SPEC)
	      (SETQ TEM (CDR (ASSQ 'FUNCTION-PARENT
				   (DEBUGGING-INFO (SETQ DEF (FDEFINITION FUNCTION-SPEC)))))))
	 (VALUES (CAR TEM) (CADR TEM)))
	((AND (LISTP DEF) (EQ (CAR DEF) 'MACRO) (SYMBOLP (CDR DEF))  ;for DEFSTRUCT
	      (SETQ DEF (GET (CDR DEF) 'MACROEXPANDER-FUNCTION-PARENT)))
	 (FUNCALL DEF FUNCTION-SPEC))
	((NOT (SYMBOLP FUNCTION-SPEC))
	 (FUNCALL (GET TYPE 'FUNCTION-SPEC-HANDLER) 'FUNCTION-PARENT FUNCTION-SPEC))))

;; (:LOCATION locative-or-list-pointer) refers to the CDR of the pointer.
;; This is for pointing at an arbitrary place which there is no special
;; way to describe.
(DEFPROP :LOCATION LOCATION-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN LOCATION-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((LOC (SECOND FUNCTION-SPEC)))
    (SELECTQ FUNCTION
      (VALIDATE-FUNCTION-SPEC (AND (= (LENGTH FUNCTION-SPEC) 2)
				   (OR (= (%DATA-TYPE LOC) DTP-LOCATIVE)
				       (= (%DATA-TYPE LOC) DTP-LIST))))
      (FDEFINE (RPLACD LOC ARG1))
      (FDEFINITION (CDR LOC))
      (FDEFINEDP (AND ( (%P-DATA-TYPE LOC) DTP-NULL) (NOT (NULL (CDR LOC)))))
      (FDEFINITION-LOCATION LOC)
	;FUNDEFINE could store DTP-NULL, which would only be right sometimes
      (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))

;Convert old Maclisp-style property function specs
(DEFUN STANDARDIZE-FUNCTION-SPEC (FUNCTION-SPEC)
  (AND (LISTP FUNCTION-SPEC)
       (= (LENGTH FUNCTION-SPEC) 2)
       (SYMBOLP (CAR FUNCTION-SPEC))
       (NOT (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER))
       (SETQ FUNCTION-SPEC (CONS ':PROPERTY FUNCTION-SPEC)))
  (OR (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC)
      (FERROR NIL "~S is not a valid function spec" FUNCTION-SPEC))
  FUNCTION-SPEC)

(DEFPROP DEFUN "Function" DEFINITION-TYPE-NAME)
(DEFPROP DEFVAR "Variable" DEFINITION-TYPE-NAME)

;; Query about any irregularities about redefining the given function symbol now.
;; Return T to tell caller to go ahead and redefine the symbol
;; (no problems or user says ok), NIL to leave it unchanged.
(DEFUN QUERY-ABOUT-REDEFINITION (FUNCTION-SPEC NEW-PATHNAME TYPE OLD-PATHNAME)
  ;; Detect any cross-file redefinition worth complaining about.
  (COND ((OR (EQ (IF (STRINGP OLD-PATHNAME) OLD-PATHNAME
		     (FUNCALL OLD-PATHNAME ':TRANSLATED-PATHNAME))
		 (IF (STRINGP NEW-PATHNAME) NEW-PATHNAME
		     (FUNCALL NEW-PATHNAME ':TRANSLATED-PATHNAME)))
	     (MEMQ OLD-PATHNAME (FUNCALL NEW-PATHNAME ':GET ':REDEFINES-FILES)))
	 T)
	(T
	 (FORMAT QUERY-IO
"~&WARNING: ~A ~S being illegally redefined by file ~A.
It was previously defined by file ~A."
		 (OR (GET TYPE 'DEFINITION-TYPE-NAME) TYPE) FUNCTION-SPEC
		 NEW-PATHNAME OLD-PATHNAME)
	 (COND ((EQ INHIBIT-FDEFINE-WARNINGS ':JUST-WARN))
	       (T
		(FORMAT QUERY-IO " OK? (type Y, N, E, P or [HELP]) ")
		(FUNCALL QUERY-IO ':CLEAR-INPUT)
		(DO () (NIL)
		  (SELECTQ (CHAR-UPCASE (FUNCALL QUERY-IO ':TYI))
		    ((#/Y #/T #\SP)
		     (PRINC "Yes." QUERY-IO)
		     (RETURN T))
		    ((#/E)
		     (PRINC "Error." QUERY-IO)
		     (CERROR T NIL ':ILLEGAL-FUNCTION-DEFINITION
		       "~A ~S being illegally redefined by file ~A.
It was previously defined by file ~A."
			     (OR (GET TYPE 'DEFINITION-TYPE-NAME) TYPE) FUNCTION-SPEC
			     NEW-PATHNAME OLD-PATHNAME)
		     (RETURN T))
		    ((#/N #\RUBOUT)
		     (PRINC "No." QUERY-IO)
		     (RETURN NIL))
		    (#/P
		     (PRINC "Proceed." QUERY-IO)
		     (PUSH OLD-PATHNAME (FUNCALL NEW-PATHNAME ':GET ':REDEFINES-FILES))
		     (RETURN T))
		    ((#/? #\HELP)
		     (PRINC "
Type Y to proceed to redefine the function, N to not redefine it, E to go into the
error handler, or P to proceed and not ask in the future (for this pair of files): "
			    QUERY-IO))
		    (OTHERWISE (FORMAT QUERY-IO "~& Type Y, N, E, or [HELP]: ")))))))))

;Restore the saved previous function definition of a symbol.
(DEFUN UNDEFUN (FUNCTION-SPEC &AUX TEM)
    (SETQ TEM (FUNCTION-SPEC-GET FUNCTION-SPEC ':PREVIOUS-DEFINITION))
    (OR TEM (FERROR NIL "~S has no previous function definition" FUNCTION-SPEC))
    (FSET-CAREFULLY FUNCTION-SPEC TEM T))

;;; Some source file stuff that does not need to be in QRAND
(DEFUN GET-SOURCE-FILE-NAME (FUNCTION-SPEC &OPTIONAL TYPE)
  (DECLARE (RETURN-LIST PATHNAME TYPE))
  (LET ((PROPERTY (FUNCTION-SPEC-GET FUNCTION-SPEC ':SOURCE-FILE-NAME)))
    (COND ((NULL PROPERTY) NIL)
	  ((NLISTP PROPERTY)
	   (AND (MEMQ TYPE '(DEFUN NIL))
		(VALUES PROPERTY 'DEFUN)))
	  (T
	   (LET ((LIST (IF TYPE (ASSQ TYPE PROPERTY) (CAR PROPERTY))))
	     (LOOP FOR FILE IN (CDR LIST)
		   WHEN (NOT (FUNCALL FILE ':GET ':PATCH-FILE))
		   RETURN (VALUES FILE (CAR LIST))))))))

(DEFUN GET-ALL-SOURCE-FILE-NAMES (FUNCTION-SPEC)
  (LET ((PROPERTY (FUNCTION-SPEC-GET FUNCTION-SPEC ':SOURCE-FILE-NAME)))
    (COND ((NULL PROPERTY) NIL)
	  ((NLISTP PROPERTY)
	   (SETQ PROPERTY `((DEFUN ,PROPERTY)))
	   ;; May as well save this consing.
	   (FUNCTION-SPEC-PUTPROP FUNCTION-SPEC PROPERTY ':SOURCE-FILE-NAME)
	   PROPERTY)
	  (T PROPERTY))))

;Get the documentation string for a function or function spec.
;NIL if not defined or no documentation
(DEFUN DOCUMENTATION (FCN)
  (COND ((SYMBOLP FCN)
	 (OR (AND (FBOUNDP FCN) (DOCUMENTATION (FSYMEVAL FCN)))
	     (GET FCN ':DOCUMENTATION)))
	((LISTP FCN)
	 (COND ((MEMQ (CAR FCN) '(LAMBDA NAMED-LAMBDA SUBST NAMED-SUBST))
		(AND (MEMQ (CAR FCN) '(NAMED-LAMBDA NAMED-SUBST))
		     (SETQ FCN (CDR FCN)))
		(SETQ FCN (CDDR FCN))
		(AND (LISTP (CAR FCN))
		     (EQ (CAAR FCN) 'DECLARE)
		     (SETQ FCN (CDR FCN)))
		(AND (CDR FCN)
		     (STRINGP (CAR FCN))
		     (CAR FCN)))
	       ((EQ (CAR FCN) 'MACRO)
		(DOCUMENTATION (CDR FCN)))
	       (T
		(AND (FDEFINEDP FCN) (DOCUMENTATION (FDEFINITION FCN))))))
	((= (%DATA-TYPE FCN) DTP-FEF-POINTER)
	 (CADR (ASSQ ':DOCUMENTATION (FUNCTION-DEBUGGING-INFO FCN))))))

;Old name.
(DEFF FUNCTION-DOCUMENTATION 'DOCUMENTATION)

;These are for reading in QCOM, and the like
(DEFUN ASSIGN-ALTERNATE (X)
   (PROG NIL 
    L	(COND ((NULL X)(RETURN NIL)))
	(SET (INTERN-LOCAL (GET-PNAME (CAR X)) PACKAGE) (CADR X))
	(SETQ X (CDDR X))
	(GO L)))

(DEFUN GET-ALTERNATE (X)
   (PROG (Y)
    L	(COND ((NULL X)(RETURN (REVERSE Y))))
	(SETQ Y (CONS (CAR X) Y))
	(SETQ X (CDDR X))
	(GO L)))

(DEFUN ASSIGN-VALUES (INPUT-LIST &OPTIONAL (SHIFT 0) (INIT 0) (DELTA 1))
   (PROG ()
LP	(COND ((NULL INPUT-LIST)(RETURN INIT)))
	(SET (CAR INPUT-LIST) (LSH INIT SHIFT))
	(SETQ INPUT-LIST (CDR INPUT-LIST))
	(SETQ INIT (+ INIT DELTA))
	(GO LP)))

(DEFUN ASSIGN-VALUES-INIT-DELTA (INPUT-LIST SHIFT INIT DELTA)
    (PROG NIL 
LP	(COND ((NULL INPUT-LIST) (RETURN INIT)))
	(SET (CAR INPUT-LIST) (LSH INIT SHIFT))
	(SETQ INPUT-LIST (CDR INPUT-LIST))
	(SETQ INIT (+ INIT DELTA))
	(GO LP)))

;(CALL function arg-desc-1 arg-data-1 arg-desc-2 arg-data-2 ...)
;The first argument is a function to call.
;The remaining arguments are in pairs, consisting of
;a descriptor arg and a data arg.
;The descriptor arg says what to do with the data arg.
;The descriptor arg value should be either a keyword or
;a list of keywords, the allowed keywords being :SPREAD and :OPTIONAL.
;:SPREAD means that the data argument is a list of arguments
;rather than a single argument.
;:OPTIONAL means that the data argument can be ignored if
;the function being called doesn't ask for it.
;After the first :OPTIONAL, all args supplied are considered optional.

(DEFUN CALL (FN &REST ALTERNATES
		&AUX (MAX-ARGS 100) (ARGS-INF (ARGS-INFO FN)))
    (AND (ZEROP (LDB %%ARG-DESC-QUOTED-REST ARGS-INF))
         (ZEROP (LDB %%ARG-DESC-EVALED-REST ARGS-INF))
         (SETQ MAX-ARGS (LDB %%ARG-DESC-MAX-ARGS ARGS-INF)))
    (%OPEN-CALL-BLOCK FN 0 4)
    (DO ((Y ALTERNATES (CDDR Y)) (OPTIONAL-FLAG) (SPREAD-FLAG NIL NIL)) ((NULL Y))
	(COND ((AND (SYMBOLP (CAR Y)) (NOT (NULL (CAR Y))))
	       (SELECTQ (CAR Y)
		   (:SPREAD (SETQ SPREAD-FLAG T))
		   (:OPTIONAL (SETQ OPTIONAL-FLAG T))
		   (OTHERWISE (FERROR NIL "Invalid CALL keyword ~S" (CAR Y)))))
	      (T (DO X (CAR Y) (CDR X) (NULL X)
		     (SELECTQ (CAR X)
			 (:SPREAD (SETQ SPREAD-FLAG T))
			 (:OPTIONAL (SETQ OPTIONAL-FLAG T))
			 (OTHERWISE (FERROR NIL "Invalid CALL keyword ~S" (CAR X)))))))
	(AND OPTIONAL-FLAG (<= MAX-ARGS 0)
	     (RETURN NIL))
	(COND (SPREAD-FLAG
	       (DO X (CADR Y) (CDR X) (OR (NULL X) (AND OPTIONAL-FLAG (<= MAX-ARGS 0)))
		   (%ASSURE-PDL-ROOM 1)
		   (%PUSH (CAR X))
		   (SETQ MAX-ARGS (1- MAX-ARGS))))
	      (T (%ASSURE-PDL-ROOM 1)
		 (%PUSH (CADR Y))
		 (SETQ MAX-ARGS (1- MAX-ARGS)))))
    (%ACTIVATE-OPEN-CALL-BLOCK))

;COMPILER-LET is just like LET when interpreted.
;But when compiled, it binds at compile time (on both passes).
;It is not a macro, for the sake of the compiler's definition of it,
;and for the sake of COMPILE-DRIVER.
(DEFUN COMPILER-LET (&QUOTE BINDLIST &REST BODY)
    (EVAL `(LET ,BINDLIST . ,BODY)))

(DEFUN DISK-RESTORE (&OPTIONAL PARTITION &AUX NAME COMMENT DESIRED-UCODE)
  (LET ((L (DISK-RESTORE-DECODE PARTITION)) (RQB NIL) BLOCK)
    (UNWIND-PROTECT
      (PROGN (SETQ RQB (GET-DISK-RQB))
	     (READ-DISK-LABEL RQB 0)
	     (SETQ NAME (IF PARTITION
			    (STRING-APPEND (LDB 0010 (CADR L)) (LDB 1010 (CADR L))
					   (LDB 0010 (CAR L)) (LDB 1010 (CAR L)))
			    (GET-DISK-STRING RQB 7 4)))
	     (SETQ BLOCK (FIND-DISK-PARTITION-FOR-READ NAME RQB)
		   COMMENT (PARTITION-COMMENT NAME 0))
	     (DISK-READ RQB 0 (1+ BLOCK))
	     (SETQ DESIRED-UCODE (AREF (RQB-BUFFER RQB)
				       (* 2 %SYS-COM-DESIRED-MICROCODE-VERSION))))
      (RETURN-DISK-RQB RQB))
    (AND ( DESIRED-UCODE %MICROCODE-VERSION-NUMBER)
	 (NOT (ZEROP DESIRED-UCODE))		;Not stored yet
	 (FORMAT QUERY-IO
		 "~&That band prefers microcode ~D but the running microcode is ~D.~%"
		 DESIRED-UCODE %MICROCODE-VERSION-NUMBER))
    (COND ((FQUERY FORMAT:YES-OR-NO-QUIETLY-P-OPTIONS
		   "Do you really want to reload ~A (~A)? " NAME COMMENT)
	   (TV:CLOSE-ALL-SERVERS "Disk-Restoring")
	   (%DISK-RESTORE (CAR L) (CADR L))))))

(DEFVAR WHO-LINE-JUST-COLD-BOOTED-P NIL) ;Set to T upon cold boot for who-line's benefit

(DEFUN DISK-SAVE (PARTITION)
  (LET* ((L (DISK-RESTORE-DECODE PARTITION))
	 (PART-NAME (STRING-APPEND (LDB 0010 (CADR L)) (LDB 1010 (CADR L))
				   (LDB 0010 (CAR L)) (LDB 1010 (CAR L))))
	 PART-SIZE)
    (MULTIPLE-VALUE (NIL PART-SIZE) (FIND-DISK-PARTITION-FOR-WRITE PART-NAME))
    (ASSURE-CC-SYMBOLS-LOADED)
    (IF (> (FIND-MAX-ADDR) PART-SIZE)
	;; This test is not necessarily accurate, since we have not
	;; yet shut off the world.  However, it should catch most cases,
	;; so that this error will be detected before the partition comment
	;; gets clobbered.
	(FERROR NIL "Cannot save, partition too small"))
    (UPDATE-PARTITION-COMMENT PART-NAME (GET-NEW-SYSTEM-VERSION) 0)
    (LOGOUT)
    
    ;; Cause cold boot initializations to happen when rebooted
    ;; and do the BEFORE-COLD initializations now
    (INITIALIZATIONS 'BEFORE-COLD-INITIALIZATION-LIST T)
    (RESET-INITIALIZATIONS 'COLD-INITIALIZATION-LIST)
    (SETQ WHO-LINE-JUST-COLD-BOOTED-P T)
    
    ;; Now shut down the world and check the partition size for real, just
    ;; to make sure that we didn't exceed the size very recently.
    (DOLIST (S TV:ALL-THE-SCREENS) (TV:SHEET-GET-LOCK S))
    (TV:WITH-MOUSE-USURPED
      (WITHOUT-INTERRUPTS
	(SETQ TV:MOUSE-SHEET NIL)
	(DOLIST (S TV:ALL-THE-SCREENS)
	  (FUNCALL S ':DEEXPOSE)
	  (TV:SHEET-RELEASE-LOCK S))
	(SETQ CURRENT-PROCESS NIL)	;Prevent error message upon coming up
	(LET ((MAX-ADDR (FIND-MAX-ADDR)))
	  (COND ((> MAX-ADDR PART-SIZE)
		 (FUNCALL TV:MAIN-SCREEN ':EXPOSE)
		 (FERROR NIL "Cannot save, partition too small.  Warm Boot please.")))
	  ;; Store the size in words rather than pages.  But don't get a bignum!
	  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-VALID-SIZE) (LSH MAX-ADDR 8))
	  (DO I 600 (1+ I) (= I 640)	;Clear the disk error log
	      (%P-STORE-TAG-AND-POINTER I 0 0))
	  (%DISK-SAVE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)
		      (CAR L) (CADR L)))))))

(DEFUN ASSURE-CC-SYMBOLS-LOADED ()
  (MULTIPLE-VALUE-BIND (NIL CURRENT-VERSION)
      (AND CADR:CC-FILE-SYMBOLS-LOADED-FROM
	   (FUNCALL CADR:CC-FILE-SYMBOLS-LOADED-FROM ':TYPE-AND-VERSION))
    (COND ((NEQ CURRENT-VERSION %MICROCODE-VERSION-NUMBER)
	   (FORMAT T "~%Loading CC symbols for UCADR version ~D~%" %MICROCODE-VERSION-NUMBER)
	   (LET ((IBASE 8))
	     (PKG-BIND "CADR"
	       (CADR:CC-LOAD-UCODE-SYMBOLS-FOR-VERSION %MICROCODE-VERSION-NUMBER)))))))

;;; Find the highest address in the virtual memory.  If you call this without
;;; inhibiting interrupts, the result is not strictly correct since some
;;; other process could invalidate it at any time by CONSing.  However,
;;; it gives you a good idea and a lower bound.  The answer is in number
;;; of pages.
(DEFUN FIND-MAX-ADDR ()
  (DO ((REGION 0 (1+ REGION))
       (MAX-ADDR 0))
      ((= REGION (REGION-LENGTH REGION-LENGTH))
       (// MAX-ADDR PAGE-SIZE))
    ;; Check each region.  If it is free, ignore it.  Otherwise,
    ;; find the highest address of that region, and get the
    ;; highest such address.
    (COND ((NOT (= (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION))
		   %REGION-SPACE-FREE))	   
	   (SETQ MAX-ADDR (MAX MAX-ADDR (+ (REGION-ORIGIN-TRUE-VALUE REGION)
					   (REGION-LENGTH REGION))))))))

(DEFUN REGION-ORIGIN-TRUE-VALUE (REGION)
  	;below crock avoids returning a negative number if region starts above
	; half way point in address space.  It can make a bignum so be careful!
  (MAKE-24-BIT-UNSIGNED (REGION-ORIGIN REGION)))


(DEFUN DISK-RESTORE-DECODE (PARTITION &AUX LOW-16-BITS HI-16-BITS)
    (COND ((NULL PARTITION)
	   (SETQ LOW-16-BITS 0 HI-16-BITS 0))
	  ((NUMBERP PARTITION)
	   (SETQ LOW-16-BITS (+ #/L (LSH #/O 8)))
	   (SETQ HI-16-BITS (+ #/D (LSH (+ #/0 PARTITION) 8))))
	  ((STRINGP PARTITION)
	   (SETQ LOW-16-BITS (+ (CHAR-UPCASE (AR-1 PARTITION 0))
				(LSH (CHAR-UPCASE (AR-1 PARTITION 1)) 8)))
	   (SETQ HI-16-BITS (+ (CHAR-UPCASE (AR-1 PARTITION 2))
			       (LSH (CHAR-UPCASE (AR-1 PARTITION 3)) 8))))
	  (T (FERROR NIL "~S is not a valid partition name" PARTITION)))
    (LIST HI-16-BITS LOW-16-BITS))

(DEFUN GET-FROM-ALTERNATING-LIST (L KEY) 
"Retreive associated item from an alternating list
Like GET, but no initial CAR"
  (GET (LOCF L) KEY))

(DEFUN PUT-ON-ALTERNATING-LIST (ITEM L KEY)
"Put an item on an alternating association list
Modifies the current association, if any.
Otherwise adds one to the head of the list.  
Returns the augmented list as value.
The user should alway use this value unless he is
certain there is a current association"
  (PROG (PNTR)
	(SETQ PNTR L)
     L  (COND ((NULL L) (RETURN (CONS KEY (CONS ITEM L))))
	      ((EQ KEY (CAR L))
	       (RPLACA (CDR L) ITEM)
	       (RETURN L)))
	(SETQ L (CDDR L))
	(GO L)))

(DEFUN READ-METER (NAME)
"Read the value of the A Memory metering location
specified by the argument"
   (LET ((A-OFF (+ %COUNTER-BLOCK-A-MEM-ADDRESS
		   (OR (FIND-POSITION-IN-LIST NAME A-MEMORY-COUNTER-BLOCK-NAMES)
		       (FERROR NIL "~S is not a valid counter name" NAME)))))
      (WITHOUT-INTERRUPTS	;Try not to get inconsistent numbers
	  (DPB (%P-LDB 2020 (+ A-MEMORY-VIRTUAL-ADDRESS A-OFF))
	       2020
	       (%P-LDB 0020 (+ A-MEMORY-VIRTUAL-ADDRESS A-OFF))))))

(DEFUN WRITE-METER (NAME VAL)
"Set  the value of the A Memory metering location
specified by the first argument to the second argument"
    (LET ((A-OFF (+ %COUNTER-BLOCK-A-MEM-ADDRESS
		   (OR (FIND-POSITION-IN-LIST NAME A-MEMORY-COUNTER-BLOCK-NAMES)
		       (FERROR NIL "~S is not a valid counter name" NAME)))))
     (WITHOUT-INTERRUPTS
	 (%P-DPB (LDB 2020 VAL)
		 2020
		 (+ A-MEMORY-VIRTUAL-ADDRESS A-OFF))
	 (%P-DPB VAL
		 0020
		 (+ A-MEMORY-VIRTUAL-ADDRESS A-OFF)))))

;;; Change what an indirect array points at, or what its offset is.
(DEFUN CHANGE-INDIRECT-ARRAY (ARRAY TYPE DIMLIST DISPLACED-P INDEX-OFFSET
			      &AUX INDEX-LENGTH NDIMS INDIRECT-LENGTH TEM
				   OLD-NDIMS OLD-INDIRECT-LENGTH)
  (CHECK-ARG ARRAY ARRAYP "an array")
  (OR (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1)
      (FERROR NIL "~S is not a displaced array" ARRAY))
  (CHECK-ARG DISPLACED-P (OR (ARRAYP DISPLACED-P) (FIXP DISPLACED-P))
	     "an array or physical address to indirect to")
  (CHECK-ARG TYPE		;TEM gets the numeric array type
	     (SETQ TEM (COND ((NUMBERP TYPE) (LDB %%ARRAY-TYPE-FIELD TYPE))
			     ((FIND-POSITION-IN-LIST TYPE ARRAY-TYPES))))
	     "an array type")
  (SETQ TYPE TEM)
  (COND ((NLISTP DIMLIST)
	 (SETQ NDIMS 1 INDEX-LENGTH (ATOMEVAL DIMLIST)))
	(T (SETQ NDIMS (LENGTH DIMLIST)
		 INDEX-LENGTH (LIST-PRODUCT DIMLIST))))
  (SETQ INDIRECT-LENGTH (IF INDEX-OFFSET 3 2)
	OLD-NDIMS (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)
	OLD-INDIRECT-LENGTH (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0))
  (OR (= NDIMS OLD-NDIMS)
      (FERROR NIL "Illegal attempt to change the number of dimensions from ~D to ~D"
	          OLD-NDIMS NDIMS))
  (OR (= INDIRECT-LENGTH OLD-INDIRECT-LENGTH)
      (FERROR NIL "Illegal attempt to add or remove index-offset"))
  (%P-DPB-OFFSET TYPE %%ARRAY-TYPE-FIELD ARRAY 0)
  (AND (LISTP DIMLIST)
       (DO ((I 1 (1+ I))
	    (N NDIMS (1- N)))
	   ((< N 2))
	 (%P-STORE-CONTENTS-OFFSET (ATOMEVAL (CAR DIMLIST)) ARRAY I)
	 (SETQ DIMLIST (CDR DIMLIST))))
  (%P-STORE-CONTENTS-OFFSET DISPLACED-P ARRAY NDIMS)
  (%P-STORE-CONTENTS-OFFSET INDEX-LENGTH ARRAY (1+ NDIMS))
  (WHEN INDEX-OFFSET
     (%P-STORE-CONTENTS-OFFSET INDEX-OFFSET ARRAY (+ NDIMS 2)))
  ARRAY)

(DEFUN IGNORE (&REST IGNORE) NIL)

;;; Read a number out of a string (starting at FROM, in the given RADIX).
;;; Returns the number, or NIL if no number was seen.
;;; Second value returned is where in the string the number ended
;;; (index of first non-digit).
(DEFUN PARSE-NUMBER (STRING &OPTIONAL (FROM 0) TO (RADIX 10.))
  (DO ((I FROM (1+ I))
       (CH)
       (NUM 0)
       (FIRSTP T NIL)
       (LIM (OR TO (STRING-LENGTH STRING))))
      (NIL)
    (AND ( I LIM)
	 (RETURN (AND (NOT FIRSTP) NUM) I))
    (SETQ CH (AREF STRING I))
    (COND ((OR (< CH #/0)
	       (> CH #/9))
	   (RETURN (AND (NOT FIRSTP) NUM) I)))
    (SETQ NUM (+ (* NUM RADIX) (- CH #/0)))))

;;; "Print" a number into an array the fast way
(DEFUN NUMBER-INTO-ARRAY (ARRAY N &OPTIONAL (RADIX BASE) (AT-INDEX 0) (MIN-COLUMNS 0)
				  &AUX QUOT)
  (IF (ZEROP (SETQ QUOT (// N RADIX)))
      (DOTIMES (I (1- MIN-COLUMNS))
	(ASET #\SP ARRAY AT-INDEX)
	(SETQ AT-INDEX (1+ AT-INDEX)))
      (SETQ AT-INDEX (NUMBER-INTO-ARRAY ARRAY QUOT RADIX AT-INDEX (1- MIN-COLUMNS))))
  (ASET (+ #/0 (\ N RADIX)) ARRAY AT-INDEX)
  (1+ AT-INDEX))

;;; Add an array to the end of another
(DEFUN APPEND-TO-ARRAY (TO-ARRAY FROM-ARRAY &OPTIONAL (FROM-START 0) FROM-END
					    &AUX OLD-LENGTH NEW-LENGTH)
  (OR FROM-END (SETQ FROM-END (ARRAY-ACTIVE-LENGTH FROM-ARRAY)))
  (SETQ NEW-LENGTH (+ (SETQ OLD-LENGTH (ARRAY-LEADER TO-ARRAY 0)) (- FROM-END FROM-START)))
  (AND (< (ARRAY-LENGTH TO-ARRAY) NEW-LENGTH) (ADJUST-ARRAY-SIZE TO-ARRAY NEW-LENGTH))
  (COPY-ARRAY-PORTION FROM-ARRAY FROM-START FROM-END TO-ARRAY OLD-LENGTH NEW-LENGTH)
  (STORE-ARRAY-LEADER NEW-LENGTH TO-ARRAY 0))
