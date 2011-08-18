;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.23
;;; Reason: Disk: incorrect calling of disk-run.  Flavor: generalize wrappers, fix undefined methods
;;; Written 12/19/81 23:56:50 by Moon,
;;; while running on Lisp Machine Seven from band 4
;;; with System 78.22, ZMail 38.4, microcode 841.



; From file DISK > LMIO; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;;; These are internal

;Read the label from specified unit into an RQB, and set up the
;disk configuration table if it is a local unit.
(DEFUN READ-DISK-LABEL (RQB UNIT)
  (DISK-READ RQB UNIT 0)
  (IF (NUMBERP UNIT)
      (LET ((BFR (RQB-BUFFER RQB)))
	(COND ((AND (= (AREF BFR 0) (+ (LSH #/A 8) #/L))
		    (= (AREF BFR 1) (+ (LSH #/L 8) #/B))
		    (= (AREF BFR 2) 1))
	       (ASET (AREF BFR 6) DISK-HEADS-PER-CYLINDER-ARRAY UNIT)
	       (ASET (AREF BFR 10) DISK-SECTORS-PER-TRACK-ARRAY UNIT))))))

)

; From file DISK > LMIO; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

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
	    "read"))

)

; From file DISK > LMIO; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

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
		      (DISK-READ-WIRED PAGE-RQB 0 (+ (LSH BASE-ADDR -8) PAGE-OFFSET))
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

)

; From file DISK > LMIO; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

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
		      (DISK-WRITE-WIRED PAGE-RQB 0 (+ (LSH BASE-ADDR -8) PAGE-OFFSET))
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

)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;This is an a-list from method type to function to write the code to go
;in the combined method.  Users can add to this.
(DEFVAR *SPECIALLY-COMBINED-METHOD-TYPES*
	'((:WRAPPER PUT-WRAPPER-INTO-COMBINED-METHOD)))

)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(LOCAL-DECLARE ((SPECIAL *FL*))
(DEFUN GET-SPECIALLY-COMBINED-METHODS (MLE *FL*)
  (SORT (LOOP FOR (TYPE . FSPECS) IN (CDDDR MLE)
	      WHEN (ASSQ TYPE *SPECIALLY-COMBINED-METHOD-TYPES*)
	        APPEND FSPECS)
	#'(LAMBDA (FS1 FS2)
	    (LOOP WITH FL1 = (CADR FS1) AND FL2 = (CADR FS2)
		  FOR SUP IN (FLAVOR-DEPENDS-ON-ALL *FL*)
		  WHEN (EQ SUP FL2) RETURN T	;Base flavor earlier in list
		  WHEN (EQ SUP FL1) RETURN NIL)))))

(DEFUN PUT-WRAPPER-INTO-COMBINED-METHOD (WRAPPER-NAME FORM)
  ;; Here we just put the wrapper in as a macro.  It will be expanded by the compiler.
  (OR (AND (FDEFINEDP WRAPPER-NAME)
	   (LET ((DEF (FDEFINITION WRAPPER-NAME)))
	     (AND (LISTP DEF) (EQ (CAR DEF) 'MACRO))))
      (FERROR NIL "~S supposed to be a wrapper macro, but missing!" WRAPPER-NAME))
  `(MACROCALL #',WRAPPER-NAME .DAEMON-CALLER-ARGS. ,FORM))
)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

; This function does most of the analysis of the magic-list-entry needed by
; method-combination functions, including most error checking.
(DEFUN GET-CERTAIN-METHODS (MAGIC-LIST-ENTRY METHOD-TYPE OTHER-METHODS-ALLOWED NO-METHODS-OK
			    ORDERING-DECLARATION &AUX (METHODS NIL))
  "Perform analysis needed by method-combination functions.
   Returns a list of the method symbols for METHOD-TYPE extracted from MAGIC-LIST-ENTRY.
   This value is shared with the data structure, don't bash it.
   OTHER-METHODS-ALLOWED is a list of method types not to complain about (T = allow all).
   NO-METHODS-OK = NIL means to complain if the returned value would be NIL.
   ORDERING-DECLARATION is :BASE-FLAVOR-FIRST, :BASE-FLAVOR-LAST, or NIL meaning
     take one of those symbols from the MAGIC-LIST-ENTRY."
  ;; Find the methods of the desired type, and barf at any extraneous methods
  (DOLIST (X (CDDDR MAGIC-LIST-ENTRY))
    (COND ((EQ (CAR X) METHOD-TYPE) (SETQ METHODS (CDR X)))
	  ((ASSQ (CAR X) *SPECIALLY-COMBINED-METHOD-TYPES*) ) ;Wrappers ignored at this level
	  ((OR (EQ OTHER-METHODS-ALLOWED T) (MEMQ (CAR X) OTHER-METHODS-ALLOWED)) )
	  (T (FERROR NIL "~S ~S method(s) illegal when using :~A method-combination"
		         (CAR X) (CAR MAGIC-LIST-ENTRY) (CADR MAGIC-LIST-ENTRY)))))
  ;; Complain if no methods supplied
  (AND (NULL METHODS) (NOT NO-METHODS-OK)
       (FERROR NIL "No ~S ~S method(s) supplied to :~A method-combination"
	           METHOD-TYPE (CAR MAGIC-LIST-ENTRY) (CADR MAGIC-LIST-ENTRY)))
  ;; Get methods into proper order.  Don't use NREVERSE!
  (SELECTQ (OR ORDERING-DECLARATION (SETQ ORDERING-DECLARATION (CADDR MAGIC-LIST-ENTRY)))
    (:BASE-FLAVOR-FIRST )
    (:BASE-FLAVOR-LAST (SETQ METHODS (REVERSE METHODS)))
    (OTHERWISE (FERROR NIL "~S invalid method combination order;
 must be :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST"
		           ORDERING-DECLARATION)))
  METHODS)

)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFUN SPECIALLY-COMBINED-METHODS-PRESENT (MLE)
  (LOOP FOR (TYPE) IN (CDDDR MLE)
	THEREIS (ASSQ TYPE *SPECIALLY-COMBINED-METHOD-TYPES*)))

)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;; This function creates a combined-method, and returns the appropriate function spec.
;; Its main job in life is to take care of wrappers.  Note the combined method
;; always takes a single &REST argument named .DAEMON-CALLER-ARGS.
;; FORM is a single form to be used as the body.
(DEFUN MAKE-COMBINED-METHOD (FL MAGIC-LIST-ENTRY FORM &AUX FSPEC)
  ;; Get the function spec which will name the combined-method
  (SETQ FSPEC `(:METHOD ,(FLAVOR-NAME FL) :COMBINED ,(CAR MAGIC-LIST-ENTRY)))
  ;; Put the wrappers around the form.  The base-flavor wrapper goes on the inside.
  (DOLIST (METHOD (GET-SPECIALLY-COMBINED-METHODS MAGIC-LIST-ENTRY FL))
    (SETQ FORM (FUNCALL (CADR (ASSQ (CADDR METHOD) *SPECIALLY-COMBINED-METHOD-TYPES*))
			METHOD FORM)))
  ;; Remember that it's going to be there, for HAVE-COMBINED-METHOD
  (FLAVOR-NOTICE-METHOD FSPEC)
  (IF *JUST-COMPILING*
      (FUNCTION-SPEC-PUTPROP FSPEC MAGIC-LIST-ENTRY 'FUTURE-COMBINED-METHOD-DERIVATION))
  ;; Compile the function.  It will be inserted into the flavor's tables either
  ;; now or when the QFASL file is loaded.
  ;; Declare the instance variables special in case wrappers use them
  (LET ((LOCAL-DECLARATIONS (CONS (FLAVOR-SPECIAL-DECLARATION (FLAVOR-NAME FL))
				  LOCAL-DECLARATIONS)))
    (COMPILE-AT-APPROPRIATE-TIME
	FL
	FSPEC
	`(LAMBDA (&REST .DAEMON-CALLER-ARGS.)
	   ,FORM)
	`(FUNCTION-SPEC-PUTPROP ',FSPEC
				',MAGIC-LIST-ENTRY
				'COMBINED-METHOD-DERIVATION)))
  FSPEC)

)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFUN (:DAEMON METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:BEFORE :AFTER) T
						  ':BASE-FLAVOR-LAST)))
	(BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
					     ':BASE-FLAVOR-LAST))
	(AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
					    ':BASE-FLAVOR-FIRST))
	(WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
	   (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS) PRIMARY-METHOD)
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	   `(PROGN 
	      ,@(MAPCAR #'(LAMBDA (X) `(LEXPR-FUNCALL #',X .DAEMON-CALLER-ARGS.))
			BEFORE-METHODS)
	      ,(IF AFTER-METHODS
		   ;; Kludge to return a few multiple values
		   `(PROG (.VAL1. .VAL2. .VAL3.)
		       ,(AND PRIMARY-METHOD
			     `(MULTIPLE-VALUE (.VAL1. .VAL2. .VAL3.)
				(LEXPR-FUNCALL #',PRIMARY-METHOD .DAEMON-CALLER-ARGS.)))
		       ,@(MAPCAR #'(LAMBDA (X) `(LEXPR-FUNCALL #',X .DAEMON-CALLER-ARGS.))
				 AFTER-METHODS)
		       (RETURN .VAL1. .VAL2. .VAL3.))
		   ;; No :AFTER methods, hair not required
		   ;; You are allowed to not have a primary method
		   (AND PRIMARY-METHOD
			`(LEXPR-FUNCALL #',PRIMARY-METHOD .DAEMON-CALLER-ARGS.))))))))

; :DAEMON-WITH-OVERRIDE combination
; This is the same as :DAEMON (the default), except that :OVERRIDE type methods
; are combined with the :BEFORE-primary-:AFTER methods in an OR.  This allows
; overriding of the main methods function.  For example, a combined method as follows
; might be generated: (OR (FOO-OVERRIDE-BAR-METHOD) (PROGN (FOO-BEFORE-BAR-METHOD)))
(DEFUN (:DAEMON-WITH-OVERRIDE METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL
						  '(:BEFORE :AFTER :OVERRIDE) T
						  ':BASE-FLAVOR-LAST)))
	(BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
					     ':BASE-FLAVOR-LAST))
	(AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
					    ':BASE-FLAVOR-FIRST))
	(WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY))
	(OVERRIDE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY
					       ':OVERRIDE T T ':BASE-FLAVOR-LAST)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
	   (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS)
	     (NULL OVERRIDE-METHODS)
	     PRIMARY-METHOD)
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	  `(OR ,@(MAPCAR #'(LAMBDA (X) `(LEXPR-FUNCALL #',X .DAEMON-CALLER-ARGS.))
			 OVERRIDE-METHODS)
	     (PROGN 
	      ,@(MAPCAR #'(LAMBDA (X) `(LEXPR-FUNCALL #',X .DAEMON-CALLER-ARGS.))
			BEFORE-METHODS)
	      ,(IF AFTER-METHODS
		   ;; Kludge to return a few multiple values
		   `(PROG (.VAL1. .VAL2. .VAL3.)
		       ,(AND PRIMARY-METHOD
			     `(MULTIPLE-VALUE (.VAL1. .VAL2. .VAL3.)
				(LEXPR-FUNCALL #',PRIMARY-METHOD .DAEMON-CALLER-ARGS.)))
		       ,@(MAPCAR #'(LAMBDA (X) `(LEXPR-FUNCALL #',X .DAEMON-CALLER-ARGS.))
				 AFTER-METHODS)
		       (RETURN .VAL1. .VAL2. .VAL3.))
		   ;; No :AFTER methods, hair not required
		   ;; You are allowed to not have a primary method
		   (AND PRIMARY-METHOD
			`(LEXPR-FUNCALL #',PRIMARY-METHOD .DAEMON-CALLER-ARGS.)))))))))
)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFUN SIMPLE-METHOD-COMBINATION (FL MAGIC-LIST-ENTRY)
  (LET ((METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL NIL NIL NIL))
	(WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY)))
    (OR (AND (NOT WRAPPERS-P) (NULL (CDR METHODS)) (CAR METHODS))
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	   (CONS (CADR MAGIC-LIST-ENTRY)
		 (MAPCAR #'(LAMBDA (M) `(LEXPR-FUNCALL #',M .DAEMON-CALLER-ARGS.))
			 METHODS))))))

)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;This associates from function-specs to METHs for methods that exist,
;because they have been putprop'ed or because someone has a pointer to
;their function cell, but which are not known by the flavor system, i.e.
;are not to be called when a message is sent.
(DEFVAR *UNDEFINED-METHOD-HASH-TABLE*
	(MAKE-EQUAL-HASH-TABLE))

)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;This just exists to be called at compile-time from the DEFMETHOD macro,
;so that any combined methods generated by COMPILE-FLAVOR-METHODS will
;know that this method will be around at run time and should be called.
(DEFUN FLAVOR-NOTICE-METHOD (FUNCTION-SPEC)
  (AND (BOUNDP 'COMPILER:FUNCTIONS-DEFINED)
       (PUSH FUNCTION-SPEC COMPILER:FUNCTIONS-DEFINED))
  (FLAVOR-METHOD-ENTRY FUNCTION-SPEC NIL))

)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;Find or create a method-table entry for the specified method.
;DONT-CREATE is NIL if method is to be fully known if not already.
;		T not to create at all.
;		HASH to create in the hash table if not known.
(DEFUN FLAVOR-METHOD-ENTRY (FUNCTION-SPEC DONT-CREATE)
  (LET ((FLAVOR-NAME (SECOND FUNCTION-SPEC))
	(TYPE (THIRD FUNCTION-SPEC))
	(MESSAGE (FOURTH FUNCTION-SPEC)))
    (IF (NULL MESSAGE) (SETQ MESSAGE TYPE TYPE NIL))	;If no type
    (IF (OR (NULL MESSAGE) (NEQ (FIRST FUNCTION-SPEC) ':METHOD) (> (LENGTH FUNCTION-SPEC) 4)
	    (NOT (SYMBOLP FLAVOR-NAME)) (NOT (SYMBOLP TYPE)) (NOT (SYMBOLP MESSAGE)))
	(FERROR NIL "~S is not a valid function-spec" FUNCTION-SPEC))
    (LET* ((FL (GET FLAVOR-NAME 'FLAVOR))
	   (MTE (AND FL (ASSQ MESSAGE (FLAVOR-METHOD-TABLE FL))))
	   METH)
      (COND ((AND (NULL MTE) (NOT DONT-CREATE))
	     ;; Message not previously known about, put into table
	     (AND FL
		  (PUSH (SETQ MTE (LIST* MESSAGE NIL NIL NIL)) (FLAVOR-METHOD-TABLE FL)))))
      ;; Message known, search for the type entry
      (COND ((METH-LOOKUP TYPE (CDDDR MTE)))	;Known by flavor
	    ((SETQ METH (GETHASH-EQUAL FUNCTION-SPEC *UNDEFINED-METHOD-HASH-TABLE*))
	     (COND ((AND (NULL DONT-CREATE) FL)	;Move from hash table to flavor
		    (PUSH METH (CDDDR MTE))
		    (REMHASH-EQUAL FUNCTION-SPEC *UNDEFINED-METHOD-HASH-TABLE*)))
	     METH)
	    ((EQ DONT-CREATE T) NIL)		;Not to be created
	    ((AND (NOT DONT-CREATE) (NULL FL)) NIL)	;Create, but no flavor defined
	    (T ;; Type not known, create a new meth with an unbound definition cell
	     (LET ((METH (LIST-IN-AREA PERMANENT-STORAGE-AREA FUNCTION-SPEC NIL NIL)))
	       (NULLIFY-METHOD-DEFINITION METH)
	       (IF DONT-CREATE			;Put in hash table or flavor as desired
		   (PUTHASH-EQUAL FUNCTION-SPEC METH *UNDEFINED-METHOD-HASH-TABLE*)
		   (PUSH METH (CDDDR MTE)))
	       METH))))))

)

; From file FLAVOR > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFUN METHOD-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((FLAVOR (SECOND FUNCTION-SPEC))
	(METHOD-TYPE (THIRD FUNCTION-SPEC))
	(MESSAGE (FOURTH FUNCTION-SPEC)))
    (IF (NULL (CDDDR FUNCTION-SPEC))
	(SETQ MESSAGE (THIRD FUNCTION-SPEC) METHOD-TYPE NIL))
    (COND ((NOT (SYMBOLP FLAVOR))
	   (FERROR NIL "In the function spec ~S, the flavor name ~S is not a symbol."
		       FUNCTION-SPEC FLAVOR))
	  ((OR (GET FLAVOR 'FLAVOR) (NOT (CLASS-SYMBOLP FLAVOR)))
	   (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	       (AND (SYMBOLP METHOD-TYPE) (SYMBOLP MESSAGE) ( 3 (LENGTH FUNCTION-SPEC) 4))
	       (LET ((FL (GET FLAVOR 'FLAVOR))
		     (METH (FLAVOR-METHOD-ENTRY FUNCTION-SPEC 
						(SELECTQ FUNCTION
						  (FDEFINE NIL)		;Create in flavor
						  (PUTPROP 'HASH)	;Create in hash table
						  (OTHERWISE T)))))	;Don't create
		 (OR (NOT (NULL METH))
		     (MEMQ FUNCTION '(FDEFINEDP COMPILER-FDEFINEDP GET FUNCTION-PARENT))
		     (IF FL
			 (FERROR NIL "~S is not a defined method; it is not possible to ~S it"
			         FUNCTION-SPEC FUNCTION)
		         (FERROR NIL "~S is neither the name of a flavor nor the name ~
				      of a class;~% it is not possible to ~S ~S."
				 FLAVOR FUNCTION FUNCTION-SPEC)))
		 (SELECTQ FUNCTION
		   (FDEFINE
		     (OR FL
			 (FERROR NIL "~S is neither the name of a flavor nor the name ~
				      of a class;~% it is not possible to ~S ~S."
				 FLAVOR FUNCTION FUNCTION-SPEC))
		     (LET ((NEW-DEFINITION (NOT (METH-DEFINEDP METH))))
		       (SETF (METH-DEFINITION METH) ARG1)
		       ;; Incrementally recompile the flavor if this is a new method, unless
		       ;; it is a :COMBINED method, which is the result of compilation,
		       ;; not a client of it.
		       (AND NEW-DEFINITION
			    (NEQ METHOD-TYPE ':COMBINED)
			    (RECOMPILE-FLAVOR FLAVOR MESSAGE))))
		   (FDEFINITION (METH-DEFINITION METH))
		   (FDEFINEDP (AND METH (METH-DEFINEDP METH)))
		   (FDEFINITION-LOCATION (LOCF (METH-DEFINITION METH)))
		   (FUNDEFINE
		     (LET ((MTE (ASSQ MESSAGE (FLAVOR-METHOD-TABLE FL))) TEM)
		       (PUTHASH-EQUAL FUNCTION-SPEC METH *UNDEFINED-METHOD-HASH-TABLE*)
		       (SETF (CDDDR MTE) (DELQ METH (CDDDR MTE)))	;Remove this method
		       (IF (OR (NULL (CDDDR MTE))	;No methods left for this operation?
							;or just a worthless combined method?
			       (AND (= (LENGTH (CDDDR MTE)) 1)
				    (EQ (METH-METHOD-TYPE (CADDDR MTE)) ':COMBINED)
				    (= (LENGTH (SETQ TEM
						 (CDDDR (FUNCTION-SPEC-GET
							  (METH-FUNCTION-SPEC (CADDDR MTE))
							  'COMBINED-METHOD-DERIVATION))))
				       1)
				    (IF METHOD-TYPE (EQ (CADDAR TEM) METHOD-TYPE)
					(NULL (CDDDAR TEM)))))
			   (SETF (FLAVOR-METHOD-TABLE FL)
				 (DELQ MTE (FLAVOR-METHOD-TABLE FL))))
		       (RECOMPILE-FLAVOR (FLAVOR-NAME FL) MESSAGE)	;Propagate the change
		       ;; In case anyone has their paws on the function cell
		       (NULLIFY-METHOD-DEFINITION METH)))
		   (FUNCTION-PARENT (VALUES FLAVOR 'DEFFLAVOR))	;Useful for automatic methods
		   (COMPILER-FDEFINEDP METH)
		   (GET (AND METH (GET (LOCF (METH-PLIST METH)) ARG1)))
		   (PUTPROP (PUTPROP (LOCF (METH-PLIST METH)) ARG1 ARG2))
		   (OTHERWISE
		     (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))))))
	  (T
	   (CLASS-METHOD-FUNCTION-SPEC-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))

)
