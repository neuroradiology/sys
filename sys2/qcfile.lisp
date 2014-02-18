;-*- Mode:LISP; Package:COMPILER; Base:8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;Compile a LISP source file into a QFASL file.

(DECLARE (SPECIAL QC-FILE-LOAD-FLAG QC-FILE-IN-CORE-FLAG QC-FILE-IN-PROGRESS
		  QC-FILE-READ-IN-PROGRESS QC-BARF-P 
		  QC-FILE-OLD-DEFAULT-CONS-AREA
		  LOCAL-DECLARATIONS UNDO-DECLARATIONS-FLAG BARF-SPECIAL-LIST
		  QC-FILE-TEMPORARY-AREA  ;USUALLY FASD-TEMPORARY-AREA, SET TO 
					       ; WORKING-STORAGE-AREA TO DISABLE TEMP
					       ; AREA HACK.
		  QC-FILE-WHACK-THRESHOLD FASD-TEMPORARY-AREA
		  OPEN-CODE-MAP-SWITCH
		  RETAIN-VARIABLE-NAMES-SWITCH RUN-IN-MACLISP-SWITCH
		  OBSOLETE-FUNCTION-WARNING-SWITCH ALL-SPECIAL-SWITCH
		  QC-ERROR-OUTPUT-FILE FUNCTIONS-REFERENCED FUNCTIONS-DEFINED
		  QC-TF-ELEMENT QC-TF-PROCESSING-MODE QC-TF-OUTPUT-MODE
		  CPROG QCOMPILE-POST-PROC LAST-ERROR-FUNCTION))

(SETQ QC-FILE-TEMPORARY-AREA FASD-TEMPORARY-AREA)

(OR (BOUNDP 'QC-FILE-IN-PROGRESS)
    (SETQ QC-FILE-IN-PROGRESS NIL))

(OR (BOUNDP 'QC-FILE-READ-IN-PROGRESS)
    (SETQ QC-FILE-READ-IN-PROGRESS NIL))

(OR (BOUNDP 'LOCAL-DECLARATIONS)
    (SETQ LOCAL-DECLARATIONS NIL))

(OR (BOUNDP 'UNDO-DECLARATIONS-FLAG)
    (SETQ UNDO-DECLARATIONS-FLAG NIL))

(SETQ QC-ERROR-OUTPUT-FILE NIL)

;;; User options to the compiler
(SETQ RETAIN-VARIABLE-NAMES-SWITCH 'ARGS	;What variable names to save for debugging
      OPEN-CODE-MAP-SWITCH T			;Turn MAPC, etc. into DO
      RUN-IN-MACLISP-SWITCH NIL			;Barf at functions only on Lisp machine
      OBSOLETE-FUNCTION-WARNING-SWITCH NIL	;Barf at functions which have better versions
      ALL-SPECIAL-SWITCH NIL)			;Make all variables special

(SETQ QC-FILE-WHACK-THRESHOLD 4000)

;Non-NIL means write an relocatable format QFASL file.
;The global value of this variable is the default
;to be used if the file property list doesn't say.
;If the :FASL property is :REL, make a REL file;
;if it is :FASL, make an old QFASL file.
;If no :FASL property, leave the variable with its global value.
(DEFVAR QC-FILE-REL-FORMAT NIL)

;Non-NIL means ignore the file property list and
;do whatever the global value of QC-FILE-REL-FORMAT says.
(DEFVAR QC-FILE-REL-FORMAT-OVERRIDE NIL)

;This holds the file-group symbol for the input file
;so that QFASL-REL:DUMP-LAP-FSET can find it.
(DEFVAR QC-FILE-FILE-GROUP-SYMBOL)

;The package we compiled in is left here by COMPILE-STREAM.
(DEFVAR QC-FILE-PACKAGE)

(DEFUN QC-FILE-LOAD (&REST QC-FILE-ARGS)
  (LOAD (APPLY #'QC-FILE QC-FILE-ARGS)))

;Compile a source file, producing a QFASL file in the binary format.
;If QC-FILE-LOAD-FLAG is T, the stuff in the source file is left defined
;as well as written into the QFASL file.  If QC-FILE-IN-CORE-FLAG is T,
;then rather than recompiling anything, the definitions currently in core
;are written out into the QFASL file.

;While a QC-FILE is in progress the default CONS area is sometimes FASD-TEMPORARY-AREA,
;which will be flushed by the end of the next FASL-WHACK.  Note this can happen
;in the middle of single QC-FILE, (between function boundaries).
;**no** by the end of the QC-FILE or by the start of another.
;Therefore, if a breakpoint occurs during a QC-FILE, you must call (QC-FILE-RESET).
;This also clears out QC-FILE-IN-PROGRESS, which is T while a QC-FILE is being done.

;Note that macros and specials are put on LOCAL-DECLARATIONS to make them temporary.
;They are also sent over into the QFASL file.

(DEFUN FASD-UPDATE-FILE (INFILE &OPTIONAL OUTFILE)
    (QC-FILE INFILE OUTFILE NIL T))

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

(DEFUN ENTER-COMPILER-WARNINGS-CONTEXT ()
  (COND (COMPILER-WARNINGS-BUFFER
	 (MULTIPLE-VALUE (STANDARD-OUTPUT COMPILER-WARNINGS-INTERVAL-STREAM)
	   (ZWEI:MAKE-BUFFER-WINDOW-OR-BROADCAST-STREAM
	     COMPILER-WARNINGS-BUFFER CONCATENATE-COMPILER-WARNINGS-P)))
	(T
	 (SETQ COMPILER-WARNINGS-INTERVAL-STREAM NIL))))

(DEFUN PRINT-FUNCTIONS-REFERENCED-BUT-NOT-DEFINED ()
  ;; Report functions referenced but not defined, which may be spelling errors
  (SETQ FUNCTIONS-REFERENCED
	(DEL-IF #'(LAMBDA (X)
		    (OR (MEMQ (CAR X) FUNCTIONS-DEFINED)
			(FDEFINEDP (CAR X))))
		FUNCTIONS-REFERENCED))
  (COND (FUNCTIONS-REFERENCED
	 (FORMAT T
		 "~&The following functions were referenced but don't seem defined:")
	 (DOLIST (X FUNCTIONS-REFERENCED)
	   (FORMAT T "~& ~S referenced by " (CAR X))
	   (FORMAT:PRINT-LIST T "~S" (CDR X))
	   (FORMAT T "~&")))))

(DEFUN QC-FILE (INFILE &OPTIONAL OUTFILE LOAD-FLAG IN-CORE-FLAG PACKAGE-SPEC
				 FILE-LOCAL-DECLARATIONS
				 DONT-SET-DEFAULT-P
				 READ-THEN-PROCESS-FLAG
		       &AUX GENERIC-PATHNAME QC-FILE-PACKAGE
			    (QC-FILE-REL-FORMAT QC-FILE-REL-FORMAT))
  ;; Default the specified input and output file names.  Open files.
  (SETQ INFILE (FS:MERGE-PATHNAME-DEFAULTS INFILE FS:LOAD-PATHNAME-DEFAULTS "LISP"))
  (WITH-OPEN-FILE (INPUT-STREAM INFILE '(:READ))
    ;; The input pathname might have been changed by the user in response to an error
    (SETQ INFILE (FUNCALL INPUT-STREAM ':PATHNAME))
    (OR DONT-SET-DEFAULT-P (FS:SET-DEFAULT-PATHNAME INFILE FS:LOAD-PATHNAME-DEFAULTS))
    (SETQ GENERIC-PATHNAME (FUNCALL INFILE ':GENERIC-PATHNAME))
    (SETQ OUTFILE
	  (IF OUTFILE
	      (FS:MERGE-PATHNAME-DEFAULTS OUTFILE INFILE "QFASL")
	      (FUNCALL INFILE ':NEW-PATHNAME
		       ':TYPE "QFASL"
		       ':VERSION (FUNCALL (FUNCALL INPUT-STREAM ':TRUENAME) ':VERSION))))
    ;; Get the file property list again, in case we don't have it already or it changed
    (FS:FILE-READ-PROPERTY-LIST GENERIC-PATHNAME INPUT-STREAM)
    (OR QC-FILE-REL-FORMAT-OVERRIDE
	(SELECTQ (FUNCALL GENERIC-PATHNAME ':GET ':FASL)
	  (:REL (SETQ QC-FILE-REL-FORMAT T))
	  (:FASL (SETQ QC-FILE-REL-FORMAT NIL))
	  (NIL)
	  (T (FERROR NIL "File property FASL value not FASL or REL in file ~A"
		     GENERIC-PATHNAME))))
    (COND (QC-FILE-REL-FORMAT
	   (LET ((FASD-STREAM NIL))	;REL compiling doesn't work the same way
	     (QFASL-REL:DUMP-START)
	     (COMPILE-STREAM INPUT-STREAM GENERIC-PATHNAME FASD-STREAM 'QC-FILE-WORK-COMPILE
			     LOAD-FLAG IN-CORE-FLAG PACKAGE-SPEC
			     FILE-LOCAL-DECLARATIONS READ-THEN-PROCESS-FLAG)
	     (LET ((PACKAGE QC-FILE-PACKAGE))
	       (QFASL-REL:WRITE-REL-FILE OUTFILE))))
	  (T
	   (WITH-OPEN-FILE (FASD-STREAM OUTFILE '(:WRITE :FIXNUM))
	     (SETQ OUTFILE (FUNCALL FASD-STREAM ':PATHNAME))
	     (FASD-START-FILE)
	     (FASD-INITIALIZE)
	     (COMPILE-STREAM INPUT-STREAM GENERIC-PATHNAME FASD-STREAM 'QC-FILE-WORK-COMPILE
			     LOAD-FLAG IN-CORE-FLAG PACKAGE-SPEC
			     FILE-LOCAL-DECLARATIONS READ-THEN-PROCESS-FLAG)
	     (FASD-END-WHACK)
	     (FASD-END-FILE)))))
  OUTFILE)

;;; COMPILE-STREAM when called by QC-FILE calls this on each form in the file
(DEFUN QC-FILE-WORK-COMPILE (FORM)
  ;; Maybe macroexpand in temp area.
  (LET-IF (NOT QC-FILE-LOAD-FLAG) ((DEFAULT-CONS-AREA QC-FILE-TEMPORARY-AREA))
    ;; Macro-expand and output this form in the appropriate way.
    (COMPILE-DRIVER FORM #'QC-FILE-COMMON NIL)))

;Common processing of each form, for both QC-FILE and FASD-UPDATE-FILE.
(DEFUN QC-FILE-COMMON (FORM TYPE)
    (COND ((MEMQ TYPE '(SPECIAL DECLARE MACRO))
	   ;; While evaluating the thing, turn off the temporary area, and
	   ;; if this is an EVAL-WHEN (COMPILE), turn off the undo-declarations
	   ;; flag so that macro definitions will really happen.
	   (LET ((DEFAULT-CONS-AREA QC-FILE-OLD-DEFAULT-CONS-AREA)
		 (UNDO-DECLARATIONS-FLAG (AND UNDO-DECLARATIONS-FLAG
					      (NOT (EQ TYPE 'DECLARE)))))
	     (OR QC-FILE-IN-CORE-FLAG (EVAL (COPYTREE FORM))))
	   ;; If supposed to compile or fasdump as well as eval, do so.
	   (COND ((EQ TYPE 'SPECIAL) (QC-FILE-FASD-FORM FORM NIL))
		 ((EQ TYPE 'MACRO)
		  (QC-TRANSLATE-FUNCTION (CADR FORM)
					 `(MACRO LAMBDA . ,(CDDR FORM))
					 'MACRO-COMPILE
					 (IF QC-FILE-REL-FORMAT 'REL 'QFASL)))))
	  ((EQ TYPE 'BEGF))
	  (QC-FILE-IN-CORE-FLAG (QC-FILE-FASD-FORM FORM T))
          (T (QC-FILE-FORM FORM))))

;Handle one form from the source file, in a QC-FILE which is actually recompiling.
;Only DEFUNs and random forms to be evaluated come here.
;We assume that DEFUNs have already been munged into the standard (DEFUN fn args . body) form.
(DEFUN QC-FILE-FORM (FORM)
  (PROG (TEM FV)
    (COND ((EQ (CAR FORM) 'COMMENT)) ;Delete comments entirely
	  ((EQ (CAR FORM) 'DEFUN)
	   (SETQ TEM (CADR FORM))
	   (COND (QC-FILE-LOAD-FLAG
		     (RPLACA (FUNCTION-CELL-LOCATION TEM)	;In case used interpreted
			     (SETQ FV (CONS 'LAMBDA (CDDR FORM))))
		     (COMPILE-1 TEM FV)
		     (RETURN (QC-FILE-FASD-FORM FORM T))))
           (QC-TRANSLATE-FUNCTION TEM (CONS 'LAMBDA (CDDR FORM))
				  'MACRO-COMPILE
				  (COND (QC-FILE-REL-FORMAT 'REL)
					(T 'QFASL)))
	   (IF (AND (ATOM TEM)	 ;Crufty restriction, otherwise it doesnt have a property list
		    (QC-FILE-SHOULD-MICROCOMPILE TEM))
	       (QC-TRANSLATE-FUNCTION TEM (CONS 'LAMBDA (CDDR FORM))  ;Once more, with feeling
				      'MICRO-COMPILE
				      (COND (QC-FILE-REL-FORMAT 'REL)
					    (T 'QFASL))))
	   (RETURN NIL)
           )
	  (QC-FILE-LOAD-FLAG (EVAL FORM)))
    (RETURN (QC-FILE-FASD-FORM FORM T))))

(DEFUN QC-FILE-SHOULD-MICROCOMPILE (FUNCTION-NAME)
    (OR (GET FUNCTION-NAME ':DEPEND-ON-BEING-MICROCOMPILED)
	(DOLIST (D LOCAL-DECLARATIONS)
	  (COND ((AND (STRING-EQUAL (STRING (CAR D)) "MICROCOMPILE")
		      (MEMQ FUNCTION-NAME (CDR D)))
		 (RETURN T))))
	(DOLIST (D FILE-LOCAL-DECLARATIONS)
	  (COND ((AND (STRING-EQUAL (STRING (CAR D)) "MICROCOMPILE")
		      (MEMQ FUNCTION-NAME (CDR D)))
		 (RETURN T))))))
		      
;Dump out a form to be evaluated at load time.
;Method of dumping depends on format of file being written.
(DEFUN QC-FILE-FASD-FORM (FORM &OPTIONAL OPTIMIZE)
  (LET ((DEFAULT-CONS-AREA QC-FILE-OLD-DEFAULT-CONS-AREA))
    (COND (QC-FILE-REL-FORMAT (QFASL-REL:DUMP-FORM FORM OPTIMIZE))
	  (T (FASD-FORM FORM OPTIMIZE)))))

;Call this function if the machine bombs out inside a QC-FILE.
;We do merely what stack unwinding ought to do.
(DEFUN QC-FILE-RESET ()
    (SETQ QC-FILE-IN-PROGRESS NIL)
    (SETQ UNDO-DECLARATIONS-FLAG NIL)
    (SETQ QC-FILE-READ-IN-PROGRESS NIL)
    (SETQ LOCAL-DECLARATIONS NIL)
    (SETQ FILE-LOCAL-DECLARATIONS NIL)
    (COND ((BOUNDP 'QC-FILE-OLD-DEFAULT-CONS-AREA)
	   (SETQ DEFAULT-CONS-AREA QC-FILE-OLD-DEFAULT-CONS-AREA))))

;(COMPILE-DRIVER form processing-function override-fn) should be used by anyone
;trying to do compilation of forms from source files, or any similar operation.
;It knows how to decipher DECLAREs, EVAL-WHENs, DEFUNs, macro calls, etc.
;It doesn't actually compile or evaluate anything,
;but instead calls the processing-function with two args:
; a form to process, and a flag which is one of these atoms:
;  SPECIAL  -  QC-FILE should eval this and put it in the FASL file.
;  DECLARE  -  QC-FILE should eval this.
;  DEFUN    -  QC-FILE should compile this and put the result in the FASL file.
;  MACRO    -  This defines a macro.  QC-FILE should eval this and also
;               put it in the FASL file.
;  BEGF     -  This is a BEGF or ENDF, which delimits a function definition in
;		the source file.  QC-FILE will someday want to put suitable
;		information inthe FASL file.
;  RANDOM   -  QC-FILE should just put this in the FASL file to be evalled.
;Of course, operations other than QC-FILE will want to do different things
;in each case, but they will probably want to distinguish the same cases.
;That's why COMPILE-DRIVER will be useful to them.

;override-fn gets to look at each form just after macro expansion.
;If it returns T, nothing more is done to the form.  If it returns NIL,
;the form is processed as usual (given to process-fn, etc.).
;override-fn may be NIL.

(DEFUN COMPILE-DRIVER (FORM PROCESS-FN OVERRIDE-FN)
  (PROG (FN TEM TEM1 TEM2)
    (SETQ FORM (MACROEXPAND FORM T))
    (RETURN
     (COND ((AND OVERRIDE-FN
		 (FUNCALL OVERRIDE-FN FORM)))
	   ((ATOM FORM))
	   ((EQ (CAR FORM) 'EVAL-WHEN)
	    (AND (CADR FORM) (OR (ATOM (CADR FORM)) (EQ (CAADR FORM) 'QUOTE))
		 (ERROR 'INVALID-EVAL-WHEN FORM))	;Must run in MACLISP.  No strings.
	    (SETQ TEM (MEMQ 'COMPILE (CADR FORM))
		  TEM1 (MEMQ 'LOAD (CADR FORM))
		  TEM2 (CDDR FORM))
	    (COND ((AND TEM TEM1)
		   (MAPC (FUNCTION (LAMBDA (DECL)
			     ;; Note: this may get things evaluated twice at compile time if
			     ;; they would get evaluated at compile time if not inside an
			     ;; EVAL-WHEN.  Fixing that would required changing the
			     ;; COMPILE-DRIVER interface.  This code at least gets everything
			     ;; compiled that should be, which the old code here didn't.
			     (FUNCALL PROCESS-FN DECL 'DECLARE)
			     (COMPILE-DRIVER DECL PROCESS-FN OVERRIDE-FN)))
			 TEM2))
		  (TEM (MAPC (FUNCTION (LAMBDA (FORM)
					 (FUNCALL PROCESS-FN FORM 'DECLARE)))
			     TEM2))
		  (TEM1 (MAPC (FUNCTION (LAMBDA (FORM)
				  (COND ((EQ (CAR FORM) 'DEFUN)
					 (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN))
					(T (FUNCALL PROCESS-FN FORM 'RANDOM)))))
			      TEM2))))
	   ((OR (EQ (SETQ FN (CAR FORM)) 'DEF)
		(AND (EQ FN 'PROGN)
		     (EQUAL (CADR FORM) ''COMPILE)))
	    (MAPC (FUNCTION (LAMBDA (FORM)
		      (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN)))
		  (CDDR FORM)))
	   ((MEMQ FN '(MACRO DEFSTRUCT))
	    (FUNCALL PROCESS-FN FORM 'MACRO))
	   ((MEMQ FN '(SPECIAL UNSPECIAL))
	    (FUNCALL PROCESS-FN FORM 'SPECIAL))
	   ((EQ FN 'DECLARE)
	    (COMPILE-DECLARE (CDR FORM) PROCESS-FN))
	   ((EQ FN 'COMMENT) NIL)
	   ((MEMQ FN '(BEGF ENDF))
	    (FUNCALL PROCESS-FN FORM 'BEGF))
	   ((EQ FN 'COMPILER-LET)
	    (EVAL `(LET ,(CADR FORM) (COMPILE-DRIVER '(PROGN 'COMPILE . ,(CDDR FORM))
						     ',PROCESS-FN ',OVERRIDE-FN))))
	   ((EQ FN 'DEFUN)
	    (SETQ TEM (DEFUN-COMPATIBILITY (CDR FORM)))
	    (COND ((EQ (CDR TEM) (CDR FORM))
		   (FUNCALL PROCESS-FN FORM 'DEFUN))
		  (T (COMPILE-DRIVER TEM PROCESS-FN OVERRIDE-FN))))
	   (T (FUNCALL PROCESS-FN FORM 'RANDOM))))))

(DEFUN COMPILE-DECLARE (DECL-LIST PROCESS-FN)
    (MAPC (FUNCTION (LAMBDA (DECL)
	      (FUNCALL PROCESS-FN DECL
		       (COND ((MEMQ (CAR DECL) '(SPECIAL UNSPECIAL)) 'SPECIAL)
			     (T 'DECLARE)))))
	  DECL-LIST))

;Handle SPECIAL and UNSPECIAL declarations.

;When not compiling a file, etc., or in Maclisp,
; we simply put on or remove a SPECIAL property.
;When compiling a file (COMPILE-NO-LOAD-FLAG is T)
; we just use FILE-LOCAL-DECLARATIONS to make the change.
;SPECIAL just pushes one big entry on FILE-LOCAL-DECLARATIONS, to save consing.
;UNSPECIAL, for each symbol, tries to avoid lossage in the case where a symbol
;is repeatedly made special and then unspecial again, by removing any existing
;unshadowed SPECIALs from FILE-LOCAL-DECLARATIONS, and then putting on an UNSPECIAL
;only if there isn't already one.  This way, FILE-LOCAL-DECLARATIONS doesn't keep growing.

;SPECIAL-1 and UNSPECIAL-1 can be used to make a computed symbol special or unspecial.

(REMPROP 'SPECIAL ':SOURCE-FILE-NAME)	;Avoid function redefinition message
(REMPROP 'UNSPECIAL ':SOURCE-FILE-NAME)

(DEFUN SPECIAL FEXPR (SYMBOLS)		;FEXPR so runs in MACLISP.
   (COND 
	 (UNDO-DECLARATIONS-FLAG
	  (PUSH (CONS 'SPECIAL (COPYLIST SYMBOLS)) FILE-LOCAL-DECLARATIONS))
	 (T
	  (MAPC #'(LAMBDA (X)
		    (PUTPROP X (OR FDEFINE-FILE-PATHNAME T) 'SPECIAL))
		SYMBOLS)))
   T)

(DEFUN SPECIAL-1 (SYMBOL)
    (COND (UNDO-DECLARATIONS-FLAG
	   (PUSH (CONS 'SPECIAL SYMBOL) FILE-LOCAL-DECLARATIONS))
	  (T (PUTPROP SYMBOL (OR FDEFINE-FILE-PATHNAME T) 'SPECIAL))))

(DEFUN UNSPECIAL FEXPR (SYMBOLS)	;FEXPR so runs in MACLISP.
	(MAPC (FUNCTION UNSPECIAL-1) SYMBOLS)
	T)

(DEFUN UNSPECIAL-1 (SYMBOL)
     (COND (UNDO-DECLARATIONS-FLAG
	    (AND (DO ((LDECLS FILE-LOCAL-DECLARATIONS (CDR LDECLS)))
		     ((NULL LDECLS) (OR (GET SYMBOL 'SPECIAL) (GET SYMBOL 'SYSTEM-CONSTANT)))
		    (LET ((LDECL (CAR LDECLS)))
			 (COND ((EQ (CAR LDECL) 'SPECIAL)
				(RPLACD LDECL (DELQ SYMBOL (CDR LDECL))))
			       ((AND (EQ (CAR LDECL) 'UNSPECIAL)
				     (MEMQ SYMBOL (CDR LDECL)))
				(RETURN NIL)))))
		 (PUSH (LIST 'UNSPECIAL SYMBOL) FILE-LOCAL-DECLARATIONS)))
	   (T (REMPROP SYMBOL 'SPECIAL)
	      (REMPROP SYMBOL 'SYSTEM-CONSTANT))))

;Process a DEFUN arglist, converting old Maclisp types of function
;such as Fexprs, Lexprs, etc. into Lisp-machine style definitions.
;This must be done before the name of the function can be determined with certainty.
;Actually, the argument is the arglist of the DEFUN, sans the DEFUN itself.
;The value is an entire form, whose car will be DEFUN or MACRO.
;The cdr of the value must be EQ to the argument, exp, if nothing has to be done.
(DEFUN DEFUN-COMPATIBILITY (EXP)
  (PROG (FCTN-NAME LL BODY TYPE)
	(SETQ TYPE 'EXPR)
	(SETQ FCTN-NAME (CAR EXP))
	(COND ((NOT (ATOM FCTN-NAME))		;Convert list function specs
	       (COND ((AND (= (LENGTH FCTN-NAME) 2)	;(DEFUN (FOO MACRO) ...)
			   (EQ (SECOND FCTN-NAME) 'MACRO))
		      (SETQ TYPE 'MACRO FCTN-NAME (CAR FCTN-NAME)))
		     ((EQ FCTN-NAME (SETQ FCTN-NAME (STANDARDIZE-FUNCTION-SPEC FCTN-NAME)))
		      (RETURN (CONS 'DEFUN EXP)))))	;Return if no conversion required
	      ((OR (NOT (ATOM (CADR EXP))) (NULL (CADR EXP))) ;Detect a valid DEFUN.
	       (RETURN (CONS 'DEFUN EXP)))
	      ((MEMQ (CADR EXP) '(FEXPR EXPR MACRO))
	       (SETQ TYPE (CADR EXP) EXP (CDR EXP)))
	      ((MEMQ FCTN-NAME '(FEXPR EXPR MACRO))
	       (SETQ TYPE FCTN-NAME FCTN-NAME (CADR EXP) EXP (CDR EXP))))
	;; Here if a new DEFUN has to be constructed
	(SETQ LL (CADR EXP))
	(SETQ BODY (CDDR EXP))
;WEIRD CONVERSION HACK TO UNCONVERT INTERLISP NLAMBDAS THAT WERE PREVIOUSLY CONVERTED
; BY HOLLOWAY'S RANDOM HACKER TO KLUDGY FEXPR'S
	(COND ((AND (EQ TYPE 'FEXPR)
		    (EQUAL LL '(*ARGS*)))
		(SETQ TYPE 'EXPR)
		(SETQ LL (CONS '&QUOTE (CADAAR BODY)))	;LAMBDA LIST OF INTERNAL LAMBDA
		(SETQ BODY (CDDAAR BODY)) ))	;BODY OF INTERNAL LAMBDA
; **END OF THAT HACK**
	(COND ((EQ TYPE 'FEXPR)
	       (SETQ LL (CONS '&QUOTE (CONS '&REST LL))))
	      ((EQ TYPE 'MACRO)
	       (RETURN (CONS 'MACRO (CONS FCTN-NAME (CONS LL BODY)))))
	      ((AND LL (ATOM LL))
	        (SETQ TYPE 'LEXPR
		     LL `(&EVAL &REST *LEXPR-ARGLIST* &AUX (,LL (LENGTH *LEXPR-ARGLIST*))))))
	(RETURN (CONS 'DEFUN (CONS FCTN-NAME (CONS LL BODY))))))

;BARF and MEMQL are used by the cold load generator, and are in QCFILE
;  so that the cold load generator can get to them without loading
;  either of QCP1 or QCP2 (which are big).

;BARF is how the compiler prints an error message.

;SEVERITY should be WARN for a warning (no break),
;DATA for something certainly very wrong in the user's input
;(something which can't be recovered from),
;BARF for an inconsistency in the compiler's data structures (not  the user's fault).

(DECLARE (SPECIAL FUNCTION-BEING-PROCESSED))

(DEFUN BARF (EXP REASON SEVERITY)
  (LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA))	;Stream may cons
    (COND ((EQ FUNCTION-BEING-PROCESSED LAST-ERROR-FUNCTION))
          (T (SETQ LAST-ERROR-FUNCTION FUNCTION-BEING-PROCESSED)
             (FORMAT T "~%<< While compiling ~S >>" LAST-ERROR-FUNCTION)))
    (COND ((EQ SEVERITY 'WARN)
	   (FORMAT T "~%Warning: ~S ~A." EXP REASON))
	  (T (FERROR NIL "~S ~A"
		     EXP REASON)))))

(DEFUN MEMQL (A B)
  (PROG NIL
   L	(COND ((NULL A) (RETURN NIL))
	      ((MEMQ (CAR A) B) (RETURN A)))
	(SETQ A (CDR A))
	(GO L)))

;World-load version of DEFMIC.
;Store into MICRO-CODE-ENTRY-ARGLIST-AREA
;Put on QLVAL and QINTCMP properties
(DEFUN DEFMIC (&QUOTE NAME OPCODE ARGLIST LISP-FUNCTION-P &OPTIONAL (NO-QINTCMP NIL)
	       &AUX FUNCTION-NAME INSTRUCTION-NAME MICRO-CODE-ENTRY-INDEX)
  (COND ((ATOM NAME)
	 (SETQ FUNCTION-NAME NAME INSTRUCTION-NAME NAME))
	((SETQ FUNCTION-NAME (CAR NAME) INSTRUCTION-NAME (CDR NAME))))
  (COND ((AND LISP-FUNCTION-P
	      (FBOUNDP FUNCTION-NAME) ;In case DEFMIC file edited after cold-load made
	      (= (%DATA-TYPE (FSYMEVAL FUNCTION-NAME)) DTP-U-ENTRY))
	 (SETQ MICRO-CODE-ENTRY-INDEX (%POINTER (FSYMEVAL FUNCTION-NAME)))
	 (STORE (MICRO-CODE-ENTRY-ARGLIST-AREA MICRO-CODE-ENTRY-INDEX) ARGLIST)))
  (COND ((NOT NO-QINTCMP)
	 (PUTPROP INSTRUCTION-NAME (LENGTH ARGLIST) 'QINTCMP)
	 (OR (EQ FUNCTION-NAME INSTRUCTION-NAME)
	     (PUTPROP FUNCTION-NAME (LENGTH ARGLIST) 'QINTCMP))))
  (PUTPROP INSTRUCTION-NAME OPCODE 'QLVAL))
