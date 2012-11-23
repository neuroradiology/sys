;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.3
;;; Reason: Improve PROCESS-UNLOCK, Fix simultaneous compilation screw.
;;; Written 12/09/81 00:26:52 by DLA,
;;; while running on Lisp Machine Three from band 1
;;; with Experimental System 78.2, Experimental ZMail 38.0, Experimental Local-File 30.1, microcode 836.



; From file PROCES > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;; Unlock the given lock.  The unlocker must be the same as the locker.
(DEFUN PROCESS-UNLOCK (LOCATIVE-POINTER &OPTIONAL LOCK-VALUE (ERROR-P T))
  (OR LOCK-VALUE (SETQ LOCK-VALUE CURRENT-PROCESS))
  (OR (%STORE-CONDITIONAL LOCATIVE-POINTER LOCK-VALUE NIL)
      (AND ERROR-P
	   (FERROR NIL "Attempt to unlock ~S, which you don't have locked"
		   LOCATIVE-POINTER))))

)

; From file QCDEFS > LISPM; AI:
#8R COMPILER:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))

;Lock so compiler doesn't lose entering itself recursively.
;This lock is set by COMPILER-WARNINGS-CONTEXT-BIND....
(DEFVAR COMPILER-PROCESS-LOCK NIL)

)

; From file QCDEFS > LISPM; AI:
#8R COMPILER:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))

(DEFMACRO COMPILER-WARNINGS-CONTEXT-BIND (&BODY BODY)
  (LET ((TOP-LEVEL-P-VAR (GENSYM)))
    `(LET ((,TOP-LEVEL-P-VAR (NOT COMPILER-WARNINGS-CONTEXT)))
       (LET-IF ,TOP-LEVEL-P-VAR
	       ((COMPILER-WARNINGS-CONTEXT T)
		(STANDARD-OUTPUT STANDARD-OUTPUT)
		(COMPILER-WARNINGS-INTERVAL-STREAM NIL)
		(FUNCTIONS-REFERENCED NIL)
		(FUNCTIONS-DEFINED NIL)
		(BARF-SPECIAL-LIST NIL))
	 (UNWIND-PROTECT
	   (PROG2 (COND (,TOP-LEVEL-P-VAR
			 (ENTER-COMPILER-WARNINGS-CONTEXT)
			 (PROCESS-LOCK (LOCF COMPILER-PROCESS-LOCK) NIL "Compiler")))
		  (PROGN . ,BODY)
		  (COND (,TOP-LEVEL-P-VAR
			 (PRINT-FUNCTIONS-REFERENCED-BUT-NOT-DEFINED))))
	   (COND (,TOP-LEVEL-P-VAR
		  ;; Don't bomb if user aborts due to lock.
		  (PROCESS-UNLOCK (LOCF COMPILER-PROCESS-LOCK) NIL NIL))))))))

)

; From file QCFILE > LISPM2; AI:
#8R COMPILER:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))

;This function does all the "outer loop" of the compiler.  It is called
;by the editor as well as the compiler.
;INPUT-STREAM is what to compile.  GENERIC-PATHNAME is for the corresponding file.
;FASD-FLAG is NIL if not making a QFASL file.
;PROCESS-FN is called on each form.
;QC-FILE-LOAD-FLAG, QC-FILE-IN-CORE-FLAG, and PACKAGE-SPEC are options.
;FILE-LOCAL-DECLARATIONS is normally initialized to NIL,
;but you can optionally pass in an initializations for it.
;READ-THEN-PROCESS-FLAG means do all reading first, thus minimizing thrashing.
(DEFUN COMPILE-STREAM (INPUT-STREAM GENERIC-PATHNAME FASD-FLAG PROCESS-FN
		       QC-FILE-LOAD-FLAG QC-FILE-IN-CORE-FLAG PACKAGE-SPEC
		       &OPTIONAL (FILE-LOCAL-DECLARATIONS NIL) READ-THEN-PROCESS-FLAG
		       &AUX LAST-ERROR-FUNCTION COMPILING-WHOLE-FILE-P
		            (PACKAGE PACKAGE)
			    ;;QC-FILE-RESET uses this to unbind this area after a boot
			    (QC-FILE-OLD-DEFAULT-CONS-AREA DEFAULT-CONS-AREA)
			    FDEFINE-FILE-PATHNAME)
  (COMPILER-WARNINGS-CONTEXT-BIND
       (SETQ COMPILING-WHOLE-FILE-P
	     (OR (NULL COMPILER-WARNINGS-INTERVAL-STREAM)
		 (ZWEI:SETUP-COMPILER-WARNINGS INPUT-STREAM)))
    ;; Bind all the variables required by the file property list.
    (MULTIPLE-VALUE-BIND (VARS VALS) (FS:FILE-PROPERTY-BINDINGS GENERIC-PATHNAME)
      (PROGV VARS VALS
	;; Override the package if required.  It has been bound in any case.
	(AND PACKAGE-SPEC (SETQ PACKAGE (PKG-FIND-PACKAGE PACKAGE-SPEC)))
	;; Override the generic pathname
	(SETQ FDEFINE-FILE-PATHNAME
	      (LET ((PATHNAME (AND (MEMQ ':PATHNAME (FUNCALL INPUT-STREAM ':WHICH-OPERATIONS))
				   (FUNCALL INPUT-STREAM ':PATHNAME))))
		(AND PATHNAME (FUNCALL PATHNAME ':GENERIC-PATHNAME))))
	;; Having bound the variables, process the file.
	(DO ((QC-FILE-IN-PROGRESS T)
	     (UNDO-DECLARATIONS-FLAG (NOT QC-FILE-LOAD-FLAG))
	     (LOCAL-DECLARATIONS NIL)
	     (OPEN-CODE-MAP-SWITCH OPEN-CODE-MAP-SWITCH)
	     (RUN-IN-MACLISP-SWITCH RUN-IN-MACLISP-SWITCH)
	     (OBSOLETE-FUNCTION-WARNING-SWITCH OBSOLETE-FUNCTION-WARNING-SWITCH)
	     (ALL-SPECIAL-SWITCH ALL-SPECIAL-SWITCH)
	     (SOURCE-FILE-UNIQUE-ID)
	     (FORM-LIST))
	    ()
	  (COND (FASD-FLAG
		 ;; Copy all suitable file properties into the fasl file
		 ;; Suitable means those that are lambda-bound when you read in a file.
		 (LET ((PLIST (COPYLIST (FUNCALL GENERIC-PATHNAME ':PLIST))))
		   ;; Remove unsuitable properties
		   (DO ((L (LOCF PLIST)))
		       ((NULL (CDR L)))
		     (IF (NOT (NULL (GET (CADR L) 'FS:FILE-PROPERTY-BINDINGS)))
			 (SETQ L (CDDR L))
			 (RPLACD L (CDDDR L))))
		   ;; Make sure the package property is really the package compiled in
		   ;; Must load QFASL file into same package compiled in
		   (PUTPROP (LOCF PLIST)
			    (INTERN (PKG-NAME PACKAGE) SI:PKG-USER-PACKAGE)  ;keyword package
			    ':PACKAGE)
		   (AND INPUT-STREAM
			(MEMQ ':TRUENAME (FUNCALL INPUT-STREAM ':WHICH-OPERATIONS))
			(SETQ SOURCE-FILE-UNIQUE-ID (FUNCALL INPUT-STREAM ':TRUENAME))
			(PUTPROP (LOCF PLIST)
				 SOURCE-FILE-UNIQUE-ID
				 ':QFASL-SOURCE-FILE-UNIQUE-ID))
		   ;; If a file is being compiled across directories, remember where the
		   ;; source really came from.
		   (AND FDEFINE-FILE-PATHNAME FASD-STREAM
			(LET ((OUTFILE (AND (MEMQ ':PATHNAME
						  (FUNCALL FASD-STREAM ':WHICH-OPERATIONS))
					    (FUNCALL FASD-STREAM ':PATHNAME))))
			  (COND (OUTFILE
				 (SETQ OUTFILE (FUNCALL OUTFILE ':GENERIC-PATHNAME))
				 (AND (NEQ OUTFILE FDEFINE-FILE-PATHNAME)
				      (PUTPROP (LOCF PLIST) FDEFINE-FILE-PATHNAME
					       ':SOURCE-FILE-GENERIC-PATHNAME))))))
		   (MULTIPLE-VALUE-BIND (MAJOR MINOR)
		       (SI:GET-SYSTEM-VERSION "System")
		     (PUTPROP (LOCF PLIST)
			      (LIST USER-ID
				    SI:LOCAL-PRETTY-HOST-NAME
				    (TIME:GET-UNIVERSAL-TIME)
				    MAJOR MINOR)
			      ':COMPILE-DATA))
		   ;; First thing in QFASL file must be property list
		   ;; These properties wind up on the GENERIC-PATHNAME.
		   (COND (QC-FILE-REL-FORMAT
			  (QFASL-REL:DUMP-FILE-PROPERTY-LIST
			     GENERIC-PATHNAME
			     PLIST))
			 (T
			  (FASD-FILE-PROPERTY-LIST PLIST))))))
	  (QC-PROCESS-INITIALIZE)
	  (IF READ-THEN-PROCESS-FLAG
	  ;; Read in all the forms and make a list of them (reversed) so that we
	  ;; don't have the reader and obarrays fighting the compiler for core.
	  ;; This is suboptimal in Maclisp but who cares about the Maclisp version.
	      (PROGN
		(DO ((EOF (NCONS NIL))
		     (FORM)
		     (READ-AREA (IF QC-FILE-LOAD-FLAG DEFAULT-CONS-AREA
				    QC-FILE-TEMPORARY-AREA))
		     (QC-FILE-READ-IN-PROGRESS FASD-FLAG))	;looked at by XR-#,-MACRO
		    (NIL)
		  (SETQ FORM (READ INPUT-STREAM EOF))
		  (AND (EQ FORM EOF) (RETURN))
		  (SETQ FORM-LIST (CONS-IN-AREA FORM FORM-LIST READ-AREA)))
		(CLOSE INPUT-STREAM)	;Mainly for the sake of the who-line
		;; Now scan down the list compiling.
		(DOLIST (FORM (NREVERSE FORM-LIST))
		  ;; Start a new whack if FASD-TABLE is getting too big.
		  (AND FASD-FLAG
		       ( (FASD-TABLE-LENGTH) QC-FILE-WHACK-THRESHOLD)
		       (FASD-END-WHACK))
		  (FUNCALL PROCESS-FN FORM)))
	      ;Do things in order.  This may cause more thrashing, but sometimes the other
	      ; thing loses (particularily in case of #,).
	      (PROGN
		(DO ((EOF (NCONS NIL))
		     (FORM))
		    (NIL)
		  (SETQ FORM
			(LET ((READ-AREA (IF QC-FILE-LOAD-FLAG DEFAULT-CONS-AREA
					     QC-FILE-TEMPORARY-AREA))
			      (QC-FILE-READ-IN-PROGRESS FASD-FLAG)) ;looked at by XR-#,-MACRO
			  (READ INPUT-STREAM EOF)))
		  (AND (EQ FORM EOF) (RETURN))
		  ;; Start a new whack if FASD-TABLE is getting too big.
		  (AND FASD-FLAG
		       ( (FASD-TABLE-LENGTH) QC-FILE-WHACK-THRESHOLD)
		       (FASD-END-WHACK))
		  (FUNCALL PROCESS-FN FORM)))))))))

)

; From file QCP1 > LISPM; AI:
#8R COMPILER:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))

;; Compile a function which already has an interpreted definition,
;; or define it to a newly supplied definition's compilation.
;; If the definition is one which is legal but cannot meaningfully
;; be compiled, we just leave it unchanged.
(IF-FOR-LISPM
(DEFUN COMPILE (NAME &OPTIONAL LAMBDA-EXP)
  (AND QC-FILE-IN-PROGRESS	;Check for condition likely to cause temporary area lossage
       (FORMAT ERROR-OUTPUT "~&COMPILE: Compiler recursively entered, you may lose.~%"))
  (COMPILER-WARNINGS-CONTEXT-BIND
    (PROG (TEM LAST-ERROR-FUNCTION COMPILING-WHOLE-FILE-P)
      (QC-PROCESS-INITIALIZE)
      (RESET-TEMPORARY-AREA FASD-TEMPORARY-AREA)
      (SETQ COMPILING-WHOLE-FILE-P (ZWEI:SETUP-COMPILER-WARNINGS))
      (COND (LAMBDA-EXP)
	    ((AND (FDEFINEDP NAME)
		  (LISTP (SETQ TEM (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC NAME)))))
	     (SETQ LAMBDA-EXP TEM))
	    ((AND (SYMBOLP NAME)
		  (SETQ TEM (GET NAME ':PREVIOUS-EXPR-DEFINITION)))
	     (SETQ LAMBDA-EXP TEM))
	    (T (FERROR NIL "Can't find LAMBDA expression for ~S" NAME)))
      (COMPILE-1 NAME LAMBDA-EXP)
      (RETURN NAME))))
)

)
