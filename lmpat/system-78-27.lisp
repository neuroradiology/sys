;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.27
;;; Reason: (PATHNAME :WRAPPER :NEW-PATHNAME), QSEND, Compiler lock temporary fix.
;;; Written 12/21/81 22:05:57 by DLA,
;;; while running on Lisp Machine Filecomputer from band 1
;;; with System 78.26, ZMail 38.4, Experimental Local-File 31.0, microcode 841.



; From file PATHNM > LMIO; AI:
#8R FILE-SYSTEM:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))

;;; Make sure that a :NEW-PATHNAME which specifies a new host
;;; is processed by the flavor of pathname for that host.
(DEFWRAPPER (PATHNAME :NEW-PATHNAME) (OPTIONS . BODY)
  `(LET ((NEW-HOST (GET (LOCF OPTIONS) ':HOST))
	 (NEW-PATHNAME-HOST NIL))
     (IF (AND NEW-HOST
	      (SETQ NEW-PATHNAME-HOST (GET-PATHNAME-HOST NEW-HOST))
	      (NEQ HOST NEW-PATHNAME-HOST))
	 (LEXPR-FUNCALL (DEFAULT-PATHNAME NIL NEW-PATHNAME-HOST NIL NIL T)
			':NEW-PATHNAME
			':DEVICE DEVICE
			(IF (LISTP DIRECTORY) ':STRUCTURED-DIRECTORY ':DIRECTORY)
			DIRECTORY
			(IF (LISTP NAME) ':STRUCTURED-NAME ':NAME) NAME
			':TYPE TYPE
			':VERSION VERSION
			OPTIONS)
	 . ,BODY)))

;; Propogate the change.
(RECOMPILE-FLAVOR 'PATHNAME NIL NIL)

)



; From file CHSAUX > LMIO; AI:
#8R CHAOS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))

(DEFUN SEND-MSG (DESTINATION &OPTIONAL MSG (MAIL-P NIL)  &AUX @-POS HOST PERSON)
  (SETQ DESTINATION (STRING DESTINATION))
  (SETQ @-POS (STRING-SEARCH-CHAR #/@ DESTINATION))
  (COND ((NULL @-POS)
	 (FORMAT QUERY-IO "~%At host: ")
	 (SETQ HOST (READLINE QUERY-IO))
	 (IF (EQUAL HOST "")
	     (SETQ HOST (CAR (FUNCALL FS:USER-LOGIN-MACHINE ':HOST-NAMES))))
	 (SETQ PERSON DESTINATION))
	((ZEROP @-POS)
	 (SETQ HOST (SUBSTRING DESTINATION (+ 1 @-POS) (STRING-LENGTH DESTINATION)))
	 (SETQ PERSON "anyone"))
	(T
	 (SETQ HOST (SUBSTRING DESTINATION (+ 1 @-POS) (STRING-LENGTH DESTINATION)))
	 (SETQ PERSON (SUBSTRING DESTINATION 0 (- @-POS 0)))))
  (FS:FORCE-USER-TO-LOGIN)
  (COND ((NULL MSG)
	 (FORMAT T "~%Message: (terminate with ~:@C)~%" #\END)
	 (SETQ MSG (SEND-MSG-GET-MESSAGE))))
  (WITH-OPEN-STREAM (STREAM (OPEN-STREAM HOST (STRING-APPEND "SEND " PERSON)
					 ':ERROR NIL ':DIRECTION ':OUTPUT))
    (COND ((NOT (STRINGP STREAM))
	   (FORMAT STREAM "~A@~A ~\DATIME\~%" USER-ID SI:LOCAL-HOST)
           (FUNCALL STREAM ':STRING-OUT MSG)
	   (FUNCALL STREAM ':CLOSE))
	  ((IF MAIL-P (ZWEI:SEND-MESSAGE-STRING PERSON
						(STRING-APPEND "[This was a failing QSEND]
"
							   MSG))
	     (IF (FQUERY FORMAT:YES-OR-NO-QUIETLY-P-OPTIONS "~A  Mail instead? " STREAM)
		 (ZWEI:SEND-MESSAGE-STRING PERSON
					   (STRING-APPEND "[This was a failing QSEND]
"
							   MSG))))))))

)

; From file QCDEFS > LISPM; AI:
#8R COMPILER:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "COMPILER")))

(IF-FOR-MACLISP-ELSE-LISPM

 (DEFMACRO COMPILER-WARNINGS-CONTEXT-BIND (&BODY BODY)
   `(LET ((FUNCTIONS-REFERENCED NIL)
	  (FUNCTIONS-DEFINED NIL)
	  (BARF-SPECIAL-LIST NIL))
      . ,BODY))

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
	 (PROG2 (COND (,TOP-LEVEL-P-VAR
		       (ENTER-COMPILER-WARNINGS-CONTEXT)))
		(PROGN . ,BODY)
		(COND (,TOP-LEVEL-P-VAR
		       (PRINT-FUNCTIONS-REFERENCED-BUT-NOT-DEFINED))))))))

  )

)

; From file MAKSYS > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFUN MAKE-SYSTEM (SYSTEM &REST KEYWORDS)
  ;; First check whether there is a new system declaration that can be loaded
  (MAYBE-RELOAD-SYSTEM-DECLARATION SYSTEM KEYWORDS)
  (PROGW *MAKE-SYSTEM-SPECIAL-VARIABLES*
    (SETQ *SYSTEM-BEING-MADE* (FIND-SYSTEM-NAMED SYSTEM))
    ;; Do all the keywords
    (DOLIST (KEYWORD KEYWORDS)
      (LET ((FUNCTION (GET KEYWORD 'MAKE-SYSTEM-KEYWORD)))
	(OR FUNCTION
	    (FERROR NIL "~S is not a recognized option" KEYWORD))
	(FUNCALL FUNCTION)))
    ;; Put all compiler messages together for this run
    (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
      ;; Process forms with compiler context
      (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-BEFORE*)
	(EVAL FORM))
      ;; Do the work of the transformations
      (PERFORM-TRANSFORMATIONS (COLLECT-TOP-LEVEL-TRANSFORMATIONS *SYSTEM-BEING-MADE*))
      ;; Finally process any forms queued by the keywords with compiler context
      (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-AFTER*)
	(EVAL FORM)))
    ;; Now forms outside of compiler context
    (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-FINALLY*)
      (EVAL FORM)))
  T)

)

; From file COMC > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFCOM COM-COMPILE-BUFFER-CHANGED-FUNCTIONS "Compile any sections which have been edited"
	()
  (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
    (COMPILE-BUFFER-CHANGED-FUNCTIONS *INTERVAL* *NUMERIC-ARG-P*))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

(DEFCOM COM-COMPILE-CHANGED-FUNCTIONS "Compile any sections which have been edited" ()
  (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
    (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
      (AND (EQ (IF (EQ BUFFER *INTERVAL*) *MAJOR-MODE*
		   (BUFFER-SAVED-MAJOR-MODE BUFFER))
	       'LISP-MODE)
	   (COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*))))
  (FORMAT T "~&Done.~%")
  DIS-NONE)
)

; From file COMC > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFCOM COM-COMPILE-BUFFER-CHANGED-FUNCTIONS "Compile any sections which have been edited"
	()
  (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
    (COMPILE-BUFFER-CHANGED-FUNCTIONS *INTERVAL* *NUMERIC-ARG-P*))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

(DEFCOM COM-COMPILE-CHANGED-FUNCTIONS "Compile any sections which have been edited" ()
  (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
    (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
      (AND (EQ (IF (EQ BUFFER *INTERVAL*) *MAJOR-MODE*
		   (BUFFER-SAVED-MAJOR-MODE BUFFER))
	       'LISP-MODE)
	   (COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*))))
  (FORMAT T "~&Done.~%")
  DIS-NONE)
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
