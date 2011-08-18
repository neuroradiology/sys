;;; -*- Mode:LISP; Package:ZWEI -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; This file provides the section specific code for ZMACS
;;; It uses the utility file stuff in ZWEI; FILES.
;;; The simple stuff is in ZWEI; ZMACS

(DEFCOM COM-EDIT-CALLERS "Edit functions that call the specified one.
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's." ()
  (WHO-CALLS-INTERNAL "Edit")
  (COM-NEXT-CALLER))

(DEFCOM COM-LIST-CALLERS "List functions that use the specified function.
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's." ()
  (LIST-ZMACS-CALLERS-TO-BE-EDITED "Callers of" (WHO-CALLS-INTERNAL "List"))
  DIS-NONE)

(DEFCOM COM-EDIT-MULTIPLE-CALLERS "Edit functions that use the specified functions.
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's.
This is the same as Edit Callers except it keeps asking for callees until
you type just a carriage return." ()
  (MULTIPLE-WHO-CALLS-INTERNAL "Edit")
  (COM-NEXT-CALLER))

(DEFCOM COM-LIST-MULTIPLE-CALLERS "List functions that use the specified functions.
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's.
This is the same as List Callers except it keeps asking for callees until
you type just a carriage return." ()
  (LIST-ZMACS-CALLERS-TO-BE-EDITED "Callers of" (MULTIPLE-WHO-CALLS-INTERNAL "List"))
  DIS-NONE)

;;Get a package and the name of it to go in the prompt string,
;;based on numeric argument.
(DEFUN GET-PACKAGE-TO-SEARCH ()
  (LET ((PKG (COND ((< *NUMERIC-ARG* 4) PACKAGE)
		   ((< *NUMERIC-ARG* 16.) PKG-GLOBAL-PACKAGE)
		   (T (LET ((X (TYPEIN-LINE-READLINE "Package to search (default ~A):"
						     PACKAGE)))
			(PKG-FIND-PACKAGE (IF (EQUAL X "") PACKAGE X)))))))
    (VALUES PKG (IF (= *NUMERIC-ARG* 4) "all packages" (FORMAT NIL "package ~A" PKG)))))

;;; Setqs *ZMACS-CALLERS-TO-BE-EDITED*.  Returns the function name.
(DEFUN WHO-CALLS-INTERNAL (PROMPT &OPTIONAL MUST-BE-DEFINED-FLAG &AUX TEM)
  (MULTIPLE-VALUE-BIND (PKG PKG-NAME) (GET-PACKAGE-TO-SEARCH)
    (LET ((FUNCTION (READ-FUNCTION-NAME
		      (FORMAT NIL "~A callers in ~A of" PROMPT PKG-NAME)
		      (RELEVANT-FUNCTION-NAME (POINT))
		      MUST-BE-DEFINED-FLAG
		      'ALWAYS-READ)))
      (AND (FBOUNDP FUNCTION)
	   (LISTP (SETQ TEM (FSYMEVAL FUNCTION)))
	   (EQ (CAR TEM) 'MACRO)
	   (TYPEIN-LINE "By the way, ~S is a macro; there may be other /"callers/" of it."
			FUNCTION))
      (TYPEIN-LINE "~Aing callers in ~A of ~S" PROMPT PKG-NAME FUNCTION)
      (SETUP-ZMACS-CALLERS-TO-BE-EDITED (LIST-CALLERS FUNCTION PKG))
      FUNCTION)))

(DEFUN MULTIPLE-WHO-CALLS-INTERNAL (PROMPT)
  (MULTIPLE-VALUE-BIND (PKG PKG-NAME) (GET-PACKAGE-TO-SEARCH)
    (DO ((FUNCTIONS NIL) (FUNCTION)  (END "Stop")) (NIL)
      (SETQ FUNCTION (READ-FUNCTION-NAME
		       (FORMAT NIL "~A callers in ~A of" PROMPT PKG-NAME)
		       (IF (NULL FUNCTIONS) (RELEVANT-FUNCTION-NAME (POINT)) END)
		       NIL
		       'ALWAYS-READ))
      (COND ((NEQ FUNCTION END) (PUSH FUNCTION FUNCTIONS))
	    (T (SETQ FUNCTIONS (NREVERSE FUNCTIONS))
	       (SETUP-ZMACS-CALLERS-TO-BE-EDITED (LIST-CALLERS FUNCTIONS PKG))
	       (RETURN FUNCTIONS))))))

(DEFUN SETUP-ZMACS-CALLERS-TO-BE-EDITED (CALLERS)
  (SETQ CALLERS (DEL-IF #'(LAMBDA (X) (AND (LISTP X) (EQ (CAR X) ':PROPERTY)
					   (EQ (CADDR X) ':PREVIOUS-DEFINITION)))
			CALLERS))
  (SETQ *ZMACS-CALLERS-TO-BE-EDITED*
	(SORT CALLERS #'(LAMBDA (X Y)
			  (LET ((XNAME (GET-EDITOR-SOURCE-FILE-NAME X))
				(YNAME (GET-EDITOR-SOURCE-FILE-NAME Y)))
			    (AND (EQ XNAME YNAME) (SETQ XNAME X YNAME Y))
			    (OR (SYMBOLP XNAME) (TYPEP XNAME 'FS:PATHNAME)
				(SETQ XNAME ""))
			    (OR (SYMBOLP YNAME) (TYPEP YNAME 'FS:PATHNAME)
				(SETQ YNAME ""))
			    (STRING-LESSP XNAME YNAME)))))
  (COMMAND-STORE 'COM-NEXT-CALLER #/. *ZMACS-COMTAB*))

;;; This traces through logical hosts for the sake of editing
(DEFUN GET-EDITOR-SOURCE-FILE-NAME (FUNCTION-SPEC &OPTIONAL PRINT-P TYPE &AUX LIST)
  (SETQ LIST (SI:GET-ALL-SOURCE-FILE-NAMES FUNCTION-SPEC))
  (SETQ LIST (COND (TYPE (ASSQ TYPE LIST))
		   ((ASSQ 'DEFUN LIST))
		   (T (CAR LIST))))
  ;; All patch files that have defined the function since the real file should be noted.
  (LOOP FOR FILE IN (CDR LIST)
	AS PATCH-FILE-P = (FUNCALL FILE ':GET ':PATCH-FILE)
	UNTIL (NOT PATCH-FILE-P)
	COLLECT FILE INTO INTERMEDIATE-PATCHES
	FINALLY (AND PRINT-P INTERMEDIATE-PATCHES
		     (TYPEIN-LINE "~A ~S ~:[also~;only~] defined by patch file~P ~{~A~^, ~}"
				  (OR (GET (CAR LIST) 'SI:DEFINITION-TYPE-NAME)
					   (CAR LIST))
				  FUNCTION-SPEC PATCH-FILE-P
				  (LENGTH INTERMEDIATE-PATCHES)
				  INTERMEDIATE-PATCHES))
		(AND PATCH-FILE-P (SETQ FILE (CAR INTERMEDIATE-PATCHES)))
		(RETURN (AND FILE
			     (FUNCALL (FUNCALL FILE ':TRANSLATED-PATHNAME)
				      ':NEW-PATHNAME ':TYPE "LISP" ':VERSION ':NEWEST)))))

(DEFUN LIST-ZMACS-CALLERS-TO-BE-EDITED (TYPE &OPTIONAL FUNCTION)
  (EDIT-FUNCTIONS-DISPLAY ;; We want the symbols to show with their proper package prefixes
			  ;; For hack value we will also sort it alphabetically.
			  (SORTCAR (MAPCAR #'(LAMBDA (X)
					       (CONS (FORMAT NIL "~S" X) X))
					   *ZMACS-CALLERS-TO-BE-EDITED*)
				   #'STRING-LESSP)
			  "~A~@[ ~S~]:"
			  "No ~A~@[ ~S~] found."
			  TYPE FUNCTION))

;; Set up the mouse-sensitive display for edit group of functions type operations
;; and offer to start editing these with control-.
;; Caller is responsible for calling SETUP-ZMACS-CALLERS-TO-BE-EDITED 
(DEFUN EDIT-FUNCTIONS-DISPLAY (ITEM-LIST HEADING-STRING NOT-FOUND-STRING &REST FORMAT-ARGS)
  (FUNCALL *TYPEOUT-WINDOW* ':FRESH-LINE)
  (COND ((NULL ITEM-LIST)
	 (LEXPR-FUNCALL #'FORMAT *TYPEOUT-WINDOW* NOT-FOUND-STRING FORMAT-ARGS))
	(T (LEXPR-FUNCALL #'FORMAT *TYPEOUT-WINDOW* HEADING-STRING FORMAT-ARGS)
	   (FUNCALL *TYPEOUT-WINDOW* ':FRESH-LINE)
	   (FUNCALL *TYPEOUT-WINDOW* ':TYO #\CR)	;Blank line after heading
	   (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FUNCTION-NAME ITEM-LIST)
	   (FORMAT *TYPEOUT-WINDOW*
		   "~&Type control-. to ~:[start editing these~;edit this~].~%"
		   (= (LENGTH ITEM-LIST) 1)))))

(DEFCOM COM-FUNCTION-APROPOS "List functions containing the given substring
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's." ()
  (MULTIPLE-VALUE-BIND (PKG PKG-NAME) (GET-PACKAGE-TO-SEARCH)
    (MULTIPLE-VALUE-BIND (FUNCTION KEY STR)
       (GET-EXTENDED-SEARCH-STRINGS
	 (FORMAT NIL "List functions in ~A containing substring:" PKG-NAME))
      (SETUP-ZMACS-CALLERS-TO-BE-EDITED
	(LOCAL-DECLARE ((SPECIAL *FUNCTION* *KEY* *LIST*))
	  (LET ((*FUNCTION* FUNCTION) (*KEY* KEY) (*LIST* NIL))
	    (FUNCALL (IF (EQ PKG PKG-GLOBAL-PACKAGE) #'MAPATOMS-ALL #'MAPATOMS)
		     #'(LAMBDA (SYM)
			 (AND (FUNCALL *FUNCTION* *KEY* (STRING SYM))
			      (FBOUNDP SYM)
			      (PUSH SYM *LIST*)))
		     PKG)
	    *LIST*)))
      (LIST-ZMACS-CALLERS-TO-BE-EDITED "Functions matching" STR)))
  DIS-NONE)

(DEFCOM COM-LIST-MATCHING-FUNCTIONS "List symbols satisfying the given predicate
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's." ()
  (MULTIPLE-VALUE-BIND (PKG PKG-NAME) (GET-PACKAGE-TO-SEARCH)
    (LET ((FUNCTION (READ-EVALUATED-MINI-BUFFER
		      "'(LAMBDA (SYMBOL) )" 18.
		      "List functions in ~A satisfying: (end with Control-Return)" PKG-NAME))
	  (SYMBOL (GENSYM)))
      (FSET SYMBOL FUNCTION)
      (COMPILE SYMBOL)
      (SETUP-ZMACS-CALLERS-TO-BE-EDITED (LIST-MATCHING-SYMBOLS SYMBOL PKG))
      (LET ((PRINLENGTH 3) (PRINLEVEL 3))
	(LIST-ZMACS-CALLERS-TO-BE-EDITED "Symbols satisfying" FUNCTION))))
  DIS-NONE)

(DEFUN READ-EVALUATED-MINI-BUFFER (&OPTIONAL INITIAL-CONTENTS INITIAL-CHAR-POS
				   FORMAT-STRING &REST FORMAT-ARGS &AUX INTERVAL PROMPT)
  (SETQ PROMPT (IF (NULL FORMAT-ARGS) FORMAT-STRING
		   (LEXPR-FUNCALL #'FORMAT NIL FORMAT-STRING FORMAT-ARGS)))
  (MULTIPLE-VALUE (NIL NIL INTERVAL)
    (EDIT-IN-MINI-BUFFER *MINI-BUFFER-MULTI-LINE-COMTAB* INITIAL-CONTENTS INITIAL-CHAR-POS
			 (AND PROMPT (NCONS PROMPT))))
  (LET ((FORM-STRING (STRING-INTERVAL INTERVAL))
	(FORM)
	(EOF '(())))
    (SETQ FORM (READ-FROM-STRING FORM-STRING EOF 0))
    (AND (EQ FORM EOF) (BARF "Unbalanced parentheses."))
    (EVAL FORM)))

(DEFCOM COM-NEXT-CALLER "Edit the next caller of the function.
With a numeric argument, displays the list of remaining callers and allows mousing." ()
  (OR *ZMACS-CALLERS-TO-BE-EDITED* (BARF "No more callers"))
  (IF *NUMERIC-ARG-P* (LIST-ZMACS-CALLERS-TO-BE-EDITED "Functions remaining to be edited")
      (EDIT-DEFINITION (POP *ZMACS-CALLERS-TO-BE-EDITED*)))
  DIS-NONE)

(DEFCOM COM-EDIT-DEFINITION "Go to the definition of a specified function.
The name of the function is read from the mini-buffer." ()
  (LET (SYM NAME)
    (COND ((NOT *NUMERIC-ARG-P*)
	   (MULTIPLE-VALUE (SYM NAME)
	     (READ-FUNCTION-NAME "Edit function" (RELEVANT-FUNCTION-NAME (POINT))
				 'AARRAY-OK 'MULTIPLE-OK))))
    (EDIT-DEFINITION-INTERNAL SYM NAME *NUMERIC-ARG-P*))
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FUNCTION-NAME "Edit" EDIT-DEFINITION T
			  "Edit this function.")

(DEFUN EDIT-DEFINITION (SYMBOL &OPTIONAL OK-TO-ASK)
  (COND ((NOT (SYMBOLP SYMBOL))
	 (SETQ SYMBOL (SYMBOL-FROM-STRING SYMBOL NIL OK-TO-ASK))
	 (OR (SYMBOLP SYMBOL) (SETQ SYMBOL (NCONS SYMBOL)))))
  (EDIT-DEFINITION-INTERNAL SYMBOL))

;; Move point to the definition of a specified function, switching buffers if nec.
;; CHOOSE-NEXT means move to the next definition of the same function as last time,
;; assuming it has more than one.  Barf if it has no more definitions.
(DEFUN EDIT-DEFINITION-INTERNAL (SYMBOL &OPTIONAL NAME CHOOSE-NEXT
					&AUX BUFFER LINE TEM FORCE-SOURCE)
  (POINT-PDL-PUSH (POINT) *WINDOW* T)
  (COND ((NULL SYMBOL)
	 (OR (BOUNDP '*LAST-DEFINITION-SYMBOL*) (BARF))
	 (SETQ SYMBOL *LAST-DEFINITION-SYMBOL*))
	((LISTP SYMBOL)
	 (SETQ *LOOK-ALIKE-DEFINITION-SYMBOLS* (CDR SYMBOL)
	       SYMBOL (CAR SYMBOL)))
	(T
	 (SETQ *LOOK-ALIKE-DEFINITION-SYMBOLS* NIL)))
  (COND ((AND CHOOSE-NEXT *DEFINITIONS-TO-GO*
	      (DO () ((NULL *DEFINITIONS-TO-GO*) NIL)
		(POP *DEFINITIONS-TO-GO* TEM)
		(COND ((MEMQ (CAR TEM) *ZMACS-BUFFER-LIST*)
		       (AND (DEFINITION-STILL-REAL-P (CDR TEM) NAME)
			    (RETURN T))
		       (LET ((PACKAGE PACKAGE)	;Implicit args to subroutines about to call
			     (*INTERVAL* (CAR TEM)))
			 (COMPUTE-BUFFER-PACKAGE *INTERVAL*)
			 (SECTIONIZE-BUFFER (CAR TEM))
			 (SETQ TEM (ASSQ (CAR TEM) (SI:FUNCTION-SPEC-GET SYMBOL
									 'ZMACS-BUFFERS)))
			 (AND TEM (DEFINITION-STILL-REAL-P (CDR TEM) NAME)
			      (RETURN T)))))))
	 (SETQ LINE (CDR TEM) BUFFER (CAR TEM))
	 (MAKE-BUFFER-CURRENT BUFFER))
	(T
	 (COND ((NOT CHOOSE-NEXT))
	       (*LOOK-ALIKE-DEFINITION-SYMBOLS*
		(POP *LOOK-ALIKE-DEFINITION-SYMBOLS* SYMBOL))
	       ;; If the source file for this function has not been read in, can get it from
	       ;; there.
	       ((AND (SETQ TEM (GET-EDITOR-SOURCE-FILE-NAME SYMBOL))
		     (NOT (MEM #'(LAMBDA (FILE BUFFER)
				   (EQ FILE (BUFFER-PATHNAME (CAR BUFFER))))
			       TEM (SI:FUNCTION-SPEC-GET SYMBOL 'ZMACS-BUFFERS))))
		(SETQ FORCE-SOURCE T))
	       ((AND (SYMBOLP SYMBOL)
		     (EQ (SYMBOL-PACKAGE SYMBOL) *UTILITY-PACKAGE*))	;Fake definition
		(SETQ SYMBOL (SYMBOL-FROM-STRING (GET-PNAME SYMBOL) NIL T)))
	       (T
		(BARF "No more definitions.")))
	 (LET (BP)
	   (MULTIPLE-VALUE (BP BUFFER)
	     (DEFINITION-TEXT-LOCATION SYMBOL NAME T FORCE-SOURCE))
	   (SETQ LINE (BP-LINE BP)))
	 (MAKE-BUFFER-CURRENT BUFFER)
	 (SETQ *DEFINITIONS-TO-GO* (REM #'(LAMBDA (LINE X)
					    (AND (EQ (CAR X) *INTERVAL*)
						 (EQ (CDR X) LINE)))
					LINE (SI:FUNCTION-SPEC-GET SYMBOL 'ZMACS-BUFFERS)
					1))))
  (SETQ *LAST-DEFINITION-SYMBOL* SYMBOL)
  (AND (OR *DEFINITIONS-TO-GO* *LOOK-ALIKE-DEFINITION-SYMBOLS*)
       (LET ((N-MORE-DEFS (DO ((I (LENGTH *DEFINITIONS-TO-GO*)
				  (+ I (LENGTH (SI:FUNCTION-SPEC-GET SYMBOL 'ZMACS-BUFFERS))))
			       (L *LOOK-ALIKE-DEFINITION-SYMBOLS* (CDR L))
			       (SYMBOL))
			      ((NULL L) I)
			    (SETQ SYMBOL (CAR L)))))
	 (OR (ZEROP N-MORE-DEFS) ;This can happen!
	     (TYPEIN-LINE "~D more definition~:P as well~@[;~:
 give an argument to ~A to edit the next one~]"
			  N-MORE-DEFS
			  (KEY-FOR-COMMAND 'COM-EDIT-DEFINITION)))))
  ;; Position at top of window
  (RECENTER-WINDOW *WINDOW* ':START (BACKWARD-OVER-COMMENT-LINES (CREATE-BP LINE 0) NIL))
  (MOVE-BP (POINT) LINE 0)
  NIL)

;; Return a bp pointing to the start of the specified function,
;; reading it in if necessary but not selecting the buffer
(DEFUN DEFINITION-TEXT-LOCATION (SYMBOL &OPTIONAL NAME NO-ERROR-P FORCE-SOURCE-FILE
					&AUX BUFFER SFILE FILE LINE TEM
					     (PACKAGE PACKAGE))
  (PROG ()
    (SETQ SFILE (GET-EDITOR-SOURCE-FILE-NAME SYMBOL T))	;Always print message first
    (COND ((AND (NOT FORCE-SOURCE-FILE)
		(SETQ TEM (SI:FUNCTION-SPEC-GET SYMBOL 'ZMACS-BUFFERS)))
	   (SETQ BUFFER (OR (ASSQ *INTERVAL* TEM) (CAR TEM)))
	   (SETQ LINE (CDR BUFFER) BUFFER (CAR BUFFER))))
    (COND (BUFFER
	   (AND (MEMQ BUFFER *ZMACS-BUFFER-LIST*)
		(DEFINITION-STILL-REAL-P LINE NAME)
		(RETURN (CREATE-BP LINE 0) BUFFER))
	   ;; If a definition points at a non-ZMACS buffer,
	   ;; or isn't really in the buffer any more, rescan that file.
	   (SETQ FILE (BUFFER-NAME BUFFER) BUFFER NIL)))
    (OR NAME (SETQ NAME SYMBOL))
    (COND (FILE)
	  ((SETQ FILE SFILE))
	  ((AND (SETQ TEM (SYS:FUNCTION-PARENT SYMBOL))	;Mainly for :INTERNAL here
		(OR (GET-EDITOR-SOURCE-FILE-NAME TEM)
		    (SI:FUNCTION-SPEC-GET TEM 'ZMACS-BUFFERS)))
	   (TYPEIN-LINE "~S is inside of ~S's definition" SYMBOL TEM)
	   (RETURN (DEFINITION-TEXT-LOCATION TEM NIL NO-ERROR-P)))
	  ((SETQ TEM (DOLIST (SYMBOL (AND (SYMBOLP SYMBOL) (PACKAGE-LOOKALIKE-SYMBOLS NAME)))
		       (AND (FQUERY '(:SELECT T)
				    ;; Don't leave PACKAGE in GLOBAL during query.
				    (LET ((PACKAGE SI:PKG-GLOBAL-PACKAGE))
				      (FORMAT NIL "Do you mean ~S? " SYMBOL)))
			    (RETURN SYMBOL))))
	   (RETURN (DEFINITION-TEXT-LOCATION TEM NAME NO-ERROR-P FORCE-SOURCE-FILE)))
	  (T
	   (OR *MINI-BUFFER-COMMAND*
	       (MINI-BUFFER-RING-PUSH (SETQ *MINI-BUFFER-COMMAND*
					    `((COM-EDIT-DEFINITION NIL 1)
					      ,(STRING-FROM-NAME NAME)))))
	   (SETQ FILE (READ-DEFAULTED-PATHNAME (FORMAT NIL "Read ~S from what file:" NAME)
					       (PATHNAME-DEFAULTS)))))
    (OR BUFFER (SETQ BUFFER (FIND-BUFFER-NAMED FILE)))
    (COND ((NULL BUFFER)
	   (MULTIPLE-VALUE (NIL BUFFER) (FIND-FILE FILE NIL))
	   (COMPUTE-BUFFER-PACKAGE BUFFER))
	  (T	;It's in this buffer's file, but we don't seem to know where in the buffer.
		;Resectionize (even if the function-parent is going to work later).
	   (TYPEIN-LINE "Re-sectionizing buffer in search of ~S" SYMBOL)
	   (COMPUTE-BUFFER-PACKAGE BUFFER)	;SECTIONIZE-BUFFER needs this to work
	   (SECTIONIZE-BUFFER BUFFER)))
    ;; BUFFER is now the right buffer, we think, and its sectionization is up to date.
    (COND ;; Did we find a definition when we rescanned it?
          ((SETQ LINE (CDR (ASSQ BUFFER (SI:FUNCTION-SPEC-GET SYMBOL 'ZMACS-BUFFERS)))))
	  ;; Does the definition go by some other name (e.g. a defstruct macro or
	  ;; an automatically-created flavor method)?
	  ((AND (SETQ TEM (SYS:FUNCTION-PARENT SYMBOL))
		(SETQ LINE (CDR (ASSQ BUFFER (SI:FUNCTION-SPEC-GET TEM 'ZMACS-BUFFERS)))))
	   (TYPEIN-LINE "~S is inside of ~S's definition" SYMBOL TEM))
	  ;; Maybe we have the wrong symbol (due to DEFMETHOD?  Can this still happen?)
	  ((AND (NEQ NAME SYMBOL)
		(SETQ TEM (CDR (ASSOC NAME (G-L-P *ZMACS-COMPLETION-AARRAY*))))
		(SETQ LINE (CDR (ASSQ BUFFER (SI:FUNCTION-SPEC-GET TEM 'ZMACS-BUFFERS)))))))
    (COND (LINE (RETURN (CREATE-BP LINE 0) BUFFER))
	  ;; Get here if we just scanned a file and didn't find a definition in it.
	  ;; If so, if there were previously several definitions and it's only
	  ;; the first one that lost, try the next.
	  ((SI:FUNCTION-SPEC-GET SYMBOL 'ZMACS-BUFFERS)
	   (RETURN (DEFINITION-TEXT-LOCATION SYMBOL NAME NO-ERROR-P)))
	  (T (FUNCALL (IF NO-ERROR-P #'TYPEIN-LINE #'BARF)
		      "Can't find a definition of ~S in ~A" SYMBOL (BUFFER-NAME BUFFER))
	     (RETURN (LET ((*INTERVAL* BUFFER)) ;The primitives depend on this!
		       (DEFINITION-LIKELY-POSITION BUFFER NIL T NAME)))))))

;; Return T if LINE is not deleted and really appears to contain a definition of NAME.
(DEFUN DEFINITION-STILL-REAL-P (LINE NAME &AUX TEM)
  (AND LINE
       (NEQ (LINE-TICK LINE) 'DELETED)			;Assure not deleted
       (OR (NULL NAME)				;User did not really give the name
	   (AND (SETQ TEM (GET-DEFUN-NAME (CREATE-BP LINE 0)))	;else has the right stuff.
		(STRING-EQUAL (STRING NAME) TEM)))))

;;; Parse a file, finding the defuns, adding the names to the completion Aarray and the
;;; symbols' property lists
;;; Note that this must be called with PACKAGE bound to the file's package,
;;; which is typically done by making the buffer current.
;;; If a stream is specified, we read that stream into the buffer until eof,
;;; a line at a time, and sectionize the stuff as we go.
;;; In that case, HACK-FONTS is passed on to INTERVAL-STREAM.

;;; The data base is stored as: a ZMACS-BUFFERS property on each symbol, whose value
;;; is a list of definitions each of the form (buffer . line), and
;;; a ZMACS-SECTION-LIST property on each file group symbol
;;; whose value is a list of the symbols defined in the file, in order of occurrence.
(DEFUN SECTIONIZE-BUFFER (BUFFER &OPTIONAL STREAM HACK-FONTS
                                 &AUX (PACKAGE (PKG-FIND-PACKAGE
						 (OR (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER)
							      ':GET ':PACKAGE)
						     PACKAGE)
						 ':ASK))
				      (*INTERVAL* BUFFER)
				      MODE INT-STREAM FIRST-BP LAST-BP ADDED-COMPLETIONS
				      BUFFER-TICK OLD-CHANGED-SECTIONS)
  (SETQ FIRST-BP (INTERVAL-FIRST-BP BUFFER)
	LAST-BP (INTERVAL-LAST-BP BUFFER))
  (AND STREAM
       (SETQ INT-STREAM (INTERVAL-STREAM-INTO-BP LAST-BP HACK-FONTS)))
  ;;Buffer must be a FILE-BUFFER, but need not be a real ZMACS BUFFER.
  (COND ((TYPEP BUFFER 'BUFFER)
	 (SETQ ADDED-COMPLETIONS (MAKE-ARRAY 100 ':TYPE 'ART-Q-LIST
					         ':LEADER-LENGTH 2 ':LEADER-LIST '(0)))
	 (SETQ MODE (BUFFER-SAVED-MAJOR-MODE BUFFER)))
	(T
	 (SETQ MODE 'LISP-MODE)))
  ;; Make sure the buffer ends with an empty line.
  (OR (ZEROP (BP-INDEX LAST-BP))
      (INSERT LAST-BP #\CR))
  ;; Flush any old definition pointers into this buffer.
  (LOCAL-DECLARE ((SPECIAL THE-BUFFER))
    (LET ((THE-BUFFER BUFFER))
      (DOLIST (SYM (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER) ':GET 'ZMACS-SECTION-LIST))
	(SI:FUNCTION-SPEC-PUTPROP SYM
				  (DEL-IF #'(LAMBDA (DEFN) (EQ (CAR DEFN) THE-BUFFER))
					  (SI:FUNCTION-SPEC-GET SYM 'ZMACS-BUFFERS))
				  'ZMACS-BUFFERS))))
  (SETQ BUFFER-TICK (BUFFER-TICK BUFFER))
  (TICK)
  ;; Flush old section nodes.  Also collect the names of those that are modified, they are
  ;; the ones that will be modified again after a revert buffer.
  (DOLIST (NODE (NODE-INFERIORS BUFFER))
    (AND (> (NODE-TICK NODE) BUFFER-TICK)
	 (PUSH (SECTION-NODE-NAME NODE) OLD-CHANGED-SECTIONS))
    (FLUSH-BP (INTERVAL-FIRST-BP NODE))
    (FLUSH-BP (INTERVAL-LAST-BP NODE)))
  ;; Now scan the buffer and record the definitions.
  (DO ((LINE (BP-LINE FIRST-BP) (LINE-NEXT LINE))
       (LIMIT (BP-LINE LAST-BP))
       (EOFFLG)
       (BP (COPY-BP FIRST-BP))
       (SYM)
       (PROP)
       (STR)
       (SYM-LIST)
       (INT-LINE)
       (PREV-NODE-START-BP FIRST-BP)
       (PREV-NODE-DEFUN-LINE NIL)
       (PREV-NODE-NAME "Buffer header")
       (PREVIOUS-NODE NIL)
       (NODE-LIST NIL)
       (ADD-SECTIONS (MEMQ MODE '(LISP-MODE TEXT-MODE BOLIO-MODE))))
      (NIL)
    ;; If we have a stream, read another line.
    (COND ((AND STREAM (NOT EOFFLG))
	   (MULTIPLE-VALUE (LINE EOFFLG)
			   (FUNCALL STREAM ':LINE-IN LINE-LEADER-SIZE))
	   (IF LINE (SETQ INT-LINE (FUNCALL INT-STREAM ':LINE-OUT LINE))))
	  (T
	   (SETQ INT-LINE LINE)))
    ;; See if the line is the start of a defun.
    (COND ((AND LINE
		ADD-SECTIONS
		(LET (ERR)
		  (MULTIPLE-VALUE (SYM STR ERR)
		    (GET-SECTION-NAME MODE INT-LINE BP))
		  (NOT ERR)))
	   (SETQ PROP (SI:FUNCTION-SPEC-GET SYM 'ZMACS-BUFFERS))
	   (SI:FUNCTION-SPEC-PUTPROP SYM (NCONC PROP (NCONS (CONS BUFFER INT-LINE)))
				     'ZMACS-BUFFERS)
	   (PUSH SYM SYM-LIST)
	   (AND ADDED-COMPLETIONS (SECTION-COMPLETION SYM STR ADDED-COMPLETIONS))
	   (SETQ PREVIOUS-NODE
		 (ADD-SECTION-NODE PREV-NODE-START-BP
				   (SETQ PREV-NODE-START-BP (BACKWARD-OVER-COMMENT-LINES BP))
				   PREV-NODE-NAME PREV-NODE-DEFUN-LINE BUFFER PREVIOUS-NODE
				   (IF (MEMQ PREV-NODE-NAME OLD-CHANGED-SECTIONS)
				       *TICK* BUFFER-TICK)
				   BUFFER-TICK))
	   (PUSH PREVIOUS-NODE NODE-LIST)
	   (SETQ PREV-NODE-NAME SYM
		 PREV-NODE-DEFUN-LINE INT-LINE)))
    ;; After processing the last line, exit.
    (COND ((OR EOFFLG (AND (NULL STREAM) (EQ LINE LIMIT)))
	   ;; If reading a stream, we should not have inserted a CR
	   ;; after the eof line.
	   (AND STREAM
		(DELETE-INTERVAL (FORWARD-CHAR (INTERVAL-LAST-BP BUFFER) -1 T)
				 (INTERVAL-LAST-BP BUFFER)
				 T))
	   (AND ADD-SECTIONS
		(PUSH (ADD-SECTION-NODE PREV-NODE-START-BP LAST-BP PREV-NODE-NAME
					PREV-NODE-DEFUN-LINE BUFFER PREVIOUS-NODE
					(IF (MEMQ PREV-NODE-NAME OLD-CHANGED-SECTIONS)
					    *TICK* BUFFER-TICK)
					BUFFER-TICK)
		      NODE-LIST))
	   (SETF (NODE-INFERIORS BUFFER) (NREVERSE NODE-LIST))
	   (RETURN
	     (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER)
		      ':PUTPROP (NREVERSE SYM-LIST) 'ZMACS-SECTION-LIST)))))
  (COND (ADDED-COMPLETIONS
	 ;; Copy all the completion entries now, so they all go on one page.
	 (LET ((I (ARRAY-LEADER ADDED-COMPLETIONS 0)))
	   (DOTIMES (J I)
	     (SETF (AREF ADDED-COMPLETIONS J)
		   (CONS (STRING-APPEND (CAR (AREF ADDED-COMPLETIONS J)))
			 (CDR (AREF ADDED-COMPLETIONS J))))))
	 ;; Sort them and merge them into the main list.
	 (SORT-COMPLETION-AARRAY ADDED-COMPLETIONS)
	 (MERGE-COMPLETION-AARRAY *ZMACS-COMPLETION-AARRAY* ADDED-COMPLETIONS))))

;;; Add a symbol to the completion aarray
(DEFUN SECTION-COMPLETION (THING &OPTIONAL STRING MERGING-AARRAY (EXTEND-BY 100) &AUX TEM SYM)
  (OR STRING (SETQ STRING (STRING THING)))
  (COND ((OR MERGING-AARRAY
	     (NOT (SETQ TEM (ASSOC STRING (G-L-P *ZMACS-COMPLETION-AARRAY*)))))
	 (ARRAY-PUSH-EXTEND (OR MERGING-AARRAY *ZMACS-COMPLETION-AARRAY*)
			    (CONS STRING (IF (LISTP THING) (NCONS THING) THING))
			    EXTEND-BY)
	 (OR MERGING-AARRAY
	     (STORE-ARRAY-LEADER NIL *ZMACS-COMPLETION-AARRAY* 1)))	;No longer sorted
	((LISTP (SETQ SYM (CDR TEM)))
	 (OR (MEMBER THING SYM) (PUSH THING (CDR TEM))))
	((NEQ THING SYM)
	 (SETF (CDR TEM) (LIST TEM THING)))))

;;; Add a new node with one section in it
(DEFUN ADD-SECTION-NODE (START-BP END-BP NAME DEFUN-LINE BUFFER PREVIOUS-NODE TICK TICK1
			 &AUX NODE)
  (SETQ NODE (MAKE-SECTION-NODE INTERVAL-FIRST-BP (COPY-BP START-BP ':NORMAL)
				INTERVAL-LAST-BP (COPY-BP END-BP ':MOVES)
				NODE-TICK TICK
				NODE-PREVIOUS PREVIOUS-NODE
				NODE-SUPERIOR BUFFER
				SECTION-NODE-NAME NAME
				SECTION-NODE-DEFUN-LINE DEFUN-LINE
				SECTION-NODE-COMPILE-TICK TICK1))
  (AND PREVIOUS-NODE
       (SETF (NODE-NEXT PREVIOUS-NODE) NODE))
  (DO ((LINE (BP-LINE START-BP) (LINE-NEXT LINE))
       (LIMIT (BP-LINE END-BP)))
      ((EQ LINE LIMIT))
    (SETF (LINE-NODE LINE) NODE))
  NODE)

;;; See if any new functions have been added to an interval
(DEFUN CHECK-INTERVAL-SECTIONS (START-BP &OPTIONAL END-BP IN-ORDER-P &AUX BUFFER)
  (GET-INTERVAL START-BP END-BP IN-ORDER-P)
  (SETQ BUFFER (BP-TOP-LEVEL-NODE START-BP))
  (OR (BEG-LINE-P END-BP)
      (SETQ END-BP (BEG-LINE END-BP 1 T)))
  (AND (TYPEP BUFFER 'FILE-BUFFER)
       (DO ((LINE (BP-LINE START-BP) (LINE-NEXT LINE))
	    (END-LINE (BP-LINE END-BP))
	    (MODE (IF (TYPEP BUFFER 'BUFFER) (BUFFER-SAVED-MAJOR-MODE BUFFER) *MAJOR-MODE*))
	    (BP (COPY-BP START-BP))
	    (PREV-NODE-DEFUN-LINE NIL)
	    (PREV-NODE-NAME NIL)
	    (PREV-NODE-SYM NIL)
	    (NODE) (NAME) (SYM) (ERR) (OSYM))
	   (NIL)
	 (COND ((OR (EQ LINE END-LINE)
		    (PROGN (MULTIPLE-VALUE (SYM NAME ERR)
			     (GET-SECTION-NAME MODE LINE BP))
			   (NOT ERR)))
		(COND ((OR (NULL PREV-NODE-DEFUN-LINE)
			   (EQ PREV-NODE-SYM
			       (SETQ OSYM (SECTION-NODE-NAME
					    (SETQ NODE (BP-NODE
							 (MOVE-BP BP PREV-NODE-DEFUN-LINE
								  0))))))))
		      ;; Weird case where package was changed or flavor not defined when first
		      ;; doing sectionization.  Just change the name on the section.
		      ((AND (STRING-EQUAL PREV-NODE-NAME (IF (SYMBOLP OSYM)
							     (STRING OSYM)
							     (MULTIPLE-VALUE-BIND (NIL NAME)
								 (SYMBOL-FROM-STRING OSYM)
							       NAME)))
			    (NEQ NODE BUFFER))	;Don't be confused by a buffer and function
						;with the same name.
		       (SETF (SECTION-NODE-NAME NODE) PREV-NODE-SYM)
		       (SETF (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER)
				      ':GET 'ZMACS-SECTION-LIST)
			     (CONS PREV-NODE-SYM
				   (DELQ OSYM (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER)
						       ':GET 'ZMACS-SECTION-LIST)))))
		      (T
		       ;; Here is a section we didn't know about, make a new one
		       (AND (EQ NODE BUFFER)	;If at very end of buffer
			    (SETQ NODE (CAR (LAST (NODE-INFERIORS BUFFER)))))
		       (LET ((PREV-NODE NODE) (NEXT-NODE NIL) LAST-BP)
			 (IF (NOT (NULL PREV-NODE))
			     (SETQ LAST-BP (INTERVAL-LAST-BP PREV-NODE)
				   NEXT-NODE (NODE-NEXT PREV-NODE))
			     (SETQ LAST-BP (COPY-BP (INTERVAL-FIRST-BP BUFFER) ':MOVES)))
			 (MOVE-BP LAST-BP LINE 0)
			 (SETQ NODE (ADD-SECTION-NODE (BACKWARD-OVER-COMMENT-LINES BP)
						      LAST-BP PREV-NODE-SYM
						      PREV-NODE-DEFUN-LINE BUFFER PREV-NODE
						      (NODE-TICK (OR PREV-NODE BUFFER))
						      (IF PREV-NODE
							 (SECTION-NODE-COMPILE-TICK PREV-NODE)
							 (NODE-TICK BUFFER))))
			 (COND (NEXT-NODE
				(SETF (NODE-NEXT NODE) NEXT-NODE)
				(SETF (NODE-PREVIOUS NEXT-NODE) NODE)))
			 (PUSH* PREV-NODE-SYM (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER)
							':GET 'ZMACS-SECTION-LIST))
			 (LET ((POSITION (MEMQ PREV-NODE (NODE-INFERIORS BUFFER))))
			   (IF POSITION
			       (PUSH NODE (CDR POSITION))
			       (SETF (NODE-INFERIORS BUFFER) (NCONC (NODE-INFERIORS BUFFER)
								    (NCONS NODE))))))))
		(SETQ PREV-NODE-DEFUN-LINE LINE
		      PREV-NODE-SYM SYM
		      PREV-NODE-NAME NAME)))
	 (AND (EQ LINE END-LINE) (RETURN NIL)))))

;;; Given a string that's supposed to be a defun-name, convert it to a symbol
;;; return the symbol and a string suitable for completing with.
;;;--- This comment is wrong, this function doesn't necessarily return a symbol ---
;;; This is not used when constructing the completions, but only when finding them.
;;;--- This comment isn't true either, SYMBOL-FROM-STRING is called by GET-SECTION-NAME
;;;--- which is called by SECTIONIZE-BUFFER.  NO, this comment is correct, the string
;;;returned is NOT put into the completion table.
;;; LINE is the line we got it from
(DEFUN SYMBOL-FROM-STRING (STR &OPTIONAL LINE OK-TO-ASK &AUX (EOF '(())) SYM ERROR-P)
  (DECLARE (RETURN-LIST SYM STR ERROR-P))
  (IF (ARRAYP STR)
      (MULTIPLE-VALUE (SYM ERROR-P)
	(CATCH-ERROR (READ-FROM-STRING STR EOF) NIL))
      (SETQ SYM STR
	    STR (FORMAT NIL "~S" STR)))
  (COND (ERROR-P
	 (VALUES NIL NIL ERROR-P))
	((SYMBOLP SYM)
	 (VALUES SYM (GET-PNAME SYM)))
	((OR (NLISTP SYM) (EQ SYM EOF))
	 (VALUES NIL NIL T))
	(T
	 ;; Here SYM is a list.  Certain types of function specs have two ways to
	 ;; type them, with and without the leading type keyword.  Also certain types
	 ;; of functions and other definitions do not follow the standard form
	 ;; of (DEFxxx name options...).  What we do here is to recognize and
	 ;; standardize those cases.  The variables are:
	 ;;	TYPE - the type of function spec or non-function definition
	 ;;	SYM - the function spec or definition name
	 ;;	SPEC - the variant of SYM which appears in the source code
	 ;;	STR - SPEC converted to a string
	 ;; :HANDLER doesn't appear in source files, but gets translated into
	 ;; an appropriate :METHOD here, by analyzing the combined method.
	 ;; :INTERNAL doesn't appear in source files, but might be given as the argument
	 ;; to M-X Disassemble.  The code here just tries not to destory it.
	 (LET ((TYPE (CAR SYM))
	       SPEC)
	   (IF (MEMQ TYPE '(:METHOD :PROPERTY :HANDLER :INSTANCE-METHOD :INTERNAL))
	       (SETQ SPEC (CDR SYM)
		     STR (DEFINITION-NAME-AS-STRING TYPE SPEC))
	       (SETQ SPEC SYM
		     TYPE (COND ((NULL LINE)
				 ':MAYBE-METHOD)
				((%STRING-EQUAL LINE 0 "(DEFMETHOD" 0 12)
				 ':ALWAYS-METHOD)
				((%STRING-EQUAL LINE 0 "(DEFWRAPPER" 0 13)
				 (RPLACD SPEC (CONS ':WRAPPER (CDR SPEC)))
				 ':METHOD)
				((%STRING-EQUAL LINE 0 "(DEFSTRUCT" 0 12)
				 ':DEFSTRUCT)
				((%STRING-EQUAL LINE 0 "(DEFSELECT" 0 12)
				 ':DEFSELECT)
				(T ':PROPERTY))))
	   (OR (SELECTQ TYPE
		 (:INSTANCE-METHOD
		  (AND (BOUNDP (CAR SPEC))
		       (SETQ SYM (FUNCALL (CLASS (SYMEVAL (CAR SPEC)))
					  ':METHOD-FOR (CADR SPEC)))))
		 ;; This would be a nice idea, but you cannot record any information on
		 ;; a :METHOD that isn't a method yet, such as the 'ZMACS-BUFFERS property.
;		 (:ALWAYS-METHOD
;		  (SETQ SYM (CONS ':METHOD SPEC)))
		 ((:METHOD :HANDLER :MAYBE-METHOD :ALWAYS-METHOD)
		  (LET ((FLAVOR (CAR SPEC))
			(MTYPE (IF (CDDR SPEC) (CADR SPEC)))
			(MESSAGE (IF (CDDR SPEC) (CADDR SPEC) (CADR SPEC)))
			FL)
		    (COND ((SETQ FL (GET FLAVOR 'SI:FLAVOR)))
			  ((AND (= (LENGTH SPEC) 2) (SYMBOLP FLAVOR) (CLASS-SYMBOLP FLAVOR))
			   (SETQ SYM (FUNCALL (SYMEVAL FLAVOR) ':METHOD-FOR (CADR SPEC))
				 FL T))
			  (OK-TO-ASK
			   (DOLIST (SYMBOL (PACKAGE-LOOKALIKE-SYMBOLS FLAVOR
								      SI:PKG-GLOBAL-PACKAGE
								      '(SI:FLAVOR)))
			     (IF (FQUERY '(:SELECT T) "Do you mean ~S? "
						      `(:METHOD ,SYMBOL . ,(CDR SPEC)))
				 (RETURN (SETQ FLAVOR SYMBOL
					       SPEC (CONS FLAVOR (CDR SPEC))
					       FL (GET FLAVOR 'SI:FLAVOR)))))))
		    (COND ((SYMBOLP FL)		;T or NIL
			   (AND (EQ TYPE ':MAYBE-METHOD)
				(= (LENGTH SPEC) 2)
				(SETQ SYM (CONS ':PROPERTY SPEC))))
			  ((SI:FLAVOR-METHOD-EXISTS FL MTYPE MESSAGE)
			   (SETQ SYM (CONS ':METHOD SPEC)))
			  (OK-TO-ASK
			   (DOLIST (SYMBOL (FIND-COMBINED-METHODS FLAVOR MESSAGE))
			     (IF (FQUERY '(:SELECT T) "Do you mean ~S? " SYMBOL)
				 (RETURN (SETQ SYM SYMBOL))))))))
		 (:DEFSTRUCT
		  (SETQ SYM (CAR SPEC)
			STR (GET-PNAME SYM)))
		 (:DEFSELECT
		  (SETQ SYM (CAR SPEC))
		  (IF (SYMBOLP SYM)
		      (SETQ STR (GET-PNAME SYM))
		      (MULTIPLE-VALUE (SYM STR)
			(SYMBOL-FROM-STRING (CAR SYM)))))
		 ((:PROPERTY :INTERNAL)
		  (AND (= (LENGTH SPEC) 2)
		       (SETQ SYM (CONS TYPE SPEC)))))
	       ;; Something we don't understand, make a bogus symbol to use as a property
	       ;; list to remember the location of this definition
	       (SETQ SYM (INTERN STR *UTILITY-PACKAGE*))))
	 (VALUES SYM STR))))

;Convert SPEC to a string the way it is likely to be spelled in the source file
;i.e. so that it will match what GET-DEFUN-NAME will return.
(DEFUN DEFINITION-NAME-AS-STRING (TYPE SPEC)
  TYPE ;not used now, but might be in the future
  (WITH-OUTPUT-TO-STRING (S)
    (IF (LOOP FOR X IN SPEC ALWAYS (SYMBOLP X))
	;; Try to guess what the package prefixes are in the source file.
	;; This doesn't always work, but works more often than just PRIN1.
	(LET ((PACKAGE (SYMBOL-PACKAGE (CAR SPEC))))
	  (FUNCALL S ':TYO #/()
	  (PRIN1 (CAR SPEC) S)
	  (LOOP FOR X IN (CDR SPEC)
		AS P = (SYMBOL-PACKAGE X)
		DO (FUNCALL S ':TYO #\SP)
		   (AND (EQ P SI:PKG-GLOBAL-PACKAGE)	;Probably really a keyword
			(FUNCALL S ':TYO #/:))
		   (PRIN1 X S))
	  (FUNCALL S ':TYO #/)))
	;; Not all symbols, stay on the safe side
	(PRIN1 SPEC S))))

(DEFUN GET-SECTION-NAME (MODE LINE BP &AUX STR SYM ERROR-P)
  (DECLARE (RETURN-LIST SYM STR ERROR-P))
  (SELECTQ MODE
    (LISP-MODE
     (IF (AND (%STRING-EQUAL LINE 0 "(DEF" 0 4)
	      (NOT (%STRING-EQUAL LINE 0 "(DEFPROP " 0 9))
	      (SETQ STR (GET-DEFUN-NAME (MOVE-BP BP LINE 0))))
	 (MULTIPLE-VALUE (SYM NIL ERROR-P)
	   (SYMBOL-FROM-STRING STR LINE))
	 (SETQ ERROR-P T)))
    ((TEXT-MODE BOLIO-MODE)
     (IF (AND (%STRING-EQUAL LINE 0 ".DEF" 0 4)
	      (SETQ STR (GET-TEXT-DEFUN-NAME (MOVE-BP BP LINE 0))))
	 (SETQ SYM (INTERN STR *UTILITY-PACKAGE*))
	 (SETQ ERROR-P T)))
    (OTHERWISE
     (SETQ ERROR-P T)))
  (VALUES SYM STR ERROR-P))

(DEFUN GET-TEXT-DEFUN-NAME (BP &AUX BP1)
  (ATOM-WORD-SYNTAX-BIND
    ;; Now get the second word after BP.
    (AND (SETQ BP (FORWARD-WORD BP))
	 (SETQ BP (FORWARD-OVER *BLANKS* BP))
	 (SETQ BP1 (FORWARD-WORD BP))
	 (STRING-INTERVAL BP BP1))))

;;; Return a BP within INTERVAL likely to define SYMBOL,
;;; Priority is: line starting with (, non-comment line, comment line, start of buffer
(DEFUN DEFINITION-LIKELY-POSITION (BP1 &OPTIONAL BP2 IN-ORDER-P SYMBOL
				       &AUX DEFUN-LINE NON-DEFUN-LINE COMMENT-LINE BUFFER)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (DO ((LINE (BP-LINE BP1) (LINE-NEXT LINE))
       (FROM-INDEX (BP-INDEX BP1) 0)
       (END-LINE (BP-LINE BP2))
       (KEY (STRING-FROM-NAME SYMBOL))
       (INDEX) (TEM))
      (NIL)
    (AND (SETQ INDEX (STRING-SEARCH KEY LINE FROM-INDEX
				    (AND (EQ LINE END-LINE) (BP-INDEX BP2))))
	 (COND ((CHAR-EQUAL #/( (AREF LINE 0))
		(SETQ DEFUN-LINE LINE)
		(RETURN T))
	       ((AND (NULL COMMENT-LINE) (SETQ TEM (FIND-COMMENT-START LINE)) (< TEM INDEX))
		(SETQ COMMENT-LINE LINE))
	       ((NULL NON-DEFUN-LINE)
		(SETQ NON-DEFUN-LINE LINE))))
    (AND (EQ LINE END-LINE)
	 (RETURN NIL)))
  (SETQ BUFFER (BP-TOP-LEVEL-NODE BP1))  
  (VALUES (CREATE-BP (OR DEFUN-LINE NON-DEFUN-LINE COMMENT-LINE (BP-LINE BP1))
		     0)
	  BUFFER))

(DEFUN STRING-FROM-NAME (NAME)
  (COND ((STRINGP NAME) NAME)
	((SYMBOLP NAME) (GET-PNAME NAME))
	(T (FORMAT NIL "~S" NAME))))

(DEFUN PACKAGE-LOOKALIKE-SYMBOLS (PNAME
				  &OPTIONAL (TOP-PACKAGE SI:PKG-GLOBAL-PACKAGE)
					    (PROPERTIES '(:SOURCE-FILE-NAME ZMACS-BUFFERS)))
  (PACKAGE-LOOKALIKE-SYMBOLS-1 (STRING PNAME) (PKG-FIND-PACKAGE TOP-PACKAGE) NIL PROPERTIES))

(DEFUN PACKAGE-LOOKALIKE-SYMBOLS-1 (PNAME PKG LIST PROPERTIES &AUX TEM)
  (AND (SETQ TEM (INTERN-LOCAL-SOFT PNAME PKG))
       (NOT (MEMQ TEM LIST))
       ;; Used to be GETL, but that was fooled by a property of NIL
       (LOOP FOR PROP IN PROPERTIES
	     THEREIS (GET TEM PROP))
       (PUSH TEM LIST))
  (DOLIST (PKG (SI:PKG-SUBPACKAGES PKG))
    (SETQ LIST (PACKAGE-LOOKALIKE-SYMBOLS-1 PNAME PKG LIST PROPERTIES)))
  LIST)

;;; Tag table stuff
(DEFCOM COM-VISIT-TAG-TABLE "Read in the specified tag table file.
Go through the tag table, and mark the name of each tag as being
a possible section of its file.  Later, the Edit Definition command
will see these marks and figure out which file to use.
Get the name of the file from the mini-buffer." ()
  (READ-TAG-TABLE (READ-DEFAULTED-PATHNAME "Tag Table:" (PATHNAME-DEFAULTS) "TAGS"))
  DIS-NONE)

(DEFUN READ-TAG-TABLE (FILE &AUX (ADDED-COMPLETIONS (MAKE-ARRAY 1000 ':TYPE 'ART-Q-LIST
								     ':LEADER-LENGTH 2)))
  (STORE-ARRAY-LEADER 0 ADDED-COMPLETIONS 0)
  (WITH-OPEN-FILE (STREAM FILE '(:READ :SUPER-IMAGE))
    (DO ((LINE) (EOF)
	 (FILE-LIST) (PATHNAME) (MODE))
	(NIL)
      (MULTIPLE-VALUE (LINE EOF)
	(FUNCALL STREAM ':LINE-IN))
      (COND (EOF
	     (FUNCALL FILE ':PUTPROP (NREVERSE FILE-LIST) 'ZMACS-TAG-TABLE-FILE-SYMBOLS)
	     (OR (RASSQ FILE *ZMACS-TAG-TABLE-ALIST*)
		 (PUSH (CONS (STRING FILE) FILE) *ZMACS-TAG-TABLE-ALIST*))
	     (RETURN)))
      (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS LINE *PATHNAME-DEFAULTS*))
      (PUSH PATHNAME FILE-LIST)
      (SETQ LINE (FUNCALL STREAM ':LINE-IN))	;Length,Mode
      (SETQ MODE (SUBSTRING LINE (1+ (STRING-SEARCH-CHAR #/, LINE))))
      (DO ((PACKAGE (PKG-FIND-PACKAGE (OR (FUNCALL (FUNCALL PATHNAME ':GENERIC-PATHNAME)
						   ':GET ':PACKAGE)
					  PACKAGE)))
	   (SPACE-POS) (RUBOUT-POS)
	   (STR) (SNAME))
	  ((= (AREF (SETQ LINE (FUNCALL STREAM ':LINE-IN)) 0) #/))
	(COND ((SETQ SPACE-POS (STRING-SEARCH-SET '(#\SP #\TAB) LINE))
	       (SETQ SPACE-POS (1+ SPACE-POS)
		     RUBOUT-POS (COND ((STRING-SEARCH-CHAR 177 LINE SPACE-POS))
				      (T (FUNCALL STREAM ':LINE-IN)
					 (1+ (STRING-LENGTH LINE))))
		     STR (SUBSTRING LINE SPACE-POS (1- RUBOUT-POS)))
	       (COND ((SELECTOR MODE STRING-EQUAL
			("LISP"
			 (AND (%STRING-EQUAL LINE 0 "(DEF" 0 4)
			      (NOT (%STRING-EQUAL LINE 0 "(DEFPROP " 0 9))
			      (SETQ SNAME (SYMBOL-FROM-STRING STR))))
			(("TEXT" "BOLIO" "R")
			 (AND (%STRING-EQUAL LINE 0 ".DEF" 0 4)
			      (SETQ SNAME (INTERN STR *UTILITY-PACKAGE*))))
			(OTHERWISE NIL))
		      (SECTION-COMPLETION SNAME STR ADDED-COMPLETIONS 1000)
		      (OR (GET SNAME ':SOURCE-FILE-NAME)
			  (PUTPROP SNAME PATHNAME ':SOURCE-FILE-NAME))
		      (PUSH* PATHNAME (GET SNAME 'ZMACS-TAG-FILE-SYMBOLS)))))))))
  (SORT-COMPLETION-AARRAY ADDED-COMPLETIONS)
  (MERGE-COMPLETION-AARRAY *ZMACS-COMPLETION-AARRAY* ADDED-COMPLETIONS))

(DEFCOM COM-LIST-TAG-TABLES "List the names of all the tag table files read in" ()
  (DOLIST (TAG-TABLE *ZMACS-TAG-TABLE-ALIST*)
    (FORMAT *TYPEOUT-WINDOW* "~&~4TFiles in tag table ~A:~%" (CAR TAG-TABLE))
    (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FILE
	     (FUNCALL (CDR TAG-TABLE) ':GET 'ZMACS-TAG-TABLE-FILE-SYMBOLS)))
  (FORMAT *TYPEOUT-WINDOW* "~&Done.~%")
  DIS-NONE)

(DEFCOM COM-NEXT-FILE "Move to the next file in the tags table" ()
  (NEXT-FILE *NUMERIC-ARG-P*)
  DIS-TEXT)

(DEFVAR *ZMACS-LAST-TAGS-FILE-LIST* NIL)

(DEFUN NEXT-FILE (RESTART &AUX PATHNAME BUFFER)
  (AND RESTART
       (SETQ *ZMACS-LAST-TAGS-FILE-LIST* (FUNCALL (SELECT-TAG-TABLE) ':GET
						  'ZMACS-TAG-TABLE-FILE-SYMBOLS)))
  (OR *ZMACS-LAST-TAGS-FILE-LIST* (BARF "No more files"))
  (POP *ZMACS-LAST-TAGS-FILE-LIST* PATHNAME)
  (COND ((SETQ BUFFER (FIND-BUFFER-NAMED PATHNAME))
	 (TYPEIN-LINE "~A~%" PATHNAME)
	 (MAKE-BUFFER-CURRENT BUFFER)
	 (MOVE-BP (POINT) (INTERVAL-FIRST-BP *INTERVAL*)))
	(T
	 (FIND-FILE PATHNAME))))

(DEFVAR *ZMACS-TAGS-SEARCH-KEY-STRING* "FOO")
(DEFVAR *ZMACS-TAGS-SEARCH-KEY*)
(DEFVAR *ZMACS-TAGS-SEARCH-FUNCTION*)

(DEFCOM COM-TAGS-SEARCH "Search for the specified string within files of the tags table" ()
  (TEMP-KILL-RING *ZMACS-TAGS-SEARCH-KEY-STRING*
    (MULTIPLE-VALUE (*ZMACS-TAGS-SEARCH-FUNCTION* *ZMACS-TAGS-SEARCH-KEY*)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "Tags search:" *SEARCH-MINI-BUFFER-COMTAB*)))
  (SETQ *ZMACS-TAGS-SEARCH-KEY-STRING*
	(STRING-INTERVAL (WINDOW-INTERVAL (GET-SEARCH-MINI-BUFFER-WINDOW))))
  (NEXT-FILE T)
  (COMMAND-STORE 'COM-TAGS-SEARCH-NEXT-OCCURRENCE #/. *ZMACS-COMTAB*)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (COM-TAGS-SEARCH-NEXT-OCCURRENCE))

(DEFCOM COM-TAGS-SEARCH-NEXT-OCCURRENCE "Search for the next occurrence of search string" ()
  (DO ((BP))
      (NIL)
    (SETQ BP (FUNCALL *ZMACS-TAGS-SEARCH-FUNCTION* (POINT) *ZMACS-TAGS-SEARCH-KEY*))
    (COND (BP
	   (POINT-PDL-PUSH (POINT) *WINDOW*)
	   (MOVE-BP (POINT) BP)
	   (RETURN DIS-BPS))
	  (T
	   (NEXT-FILE NIL)
	   (MUST-REDISPLAY *WINDOW* DIS-TEXT)))))

(DEFVAR *TAGS-QUERY-REPLACE-FROM*)
(DEFVAR *TAGS-QUERY-REPLACE-TO*)
(DEFVAR *TAGS-QUERY-REPLACE-DELIMITED*)

(DEFCOM COM-TAGS-QUERY-REPLACE "Perform a query replace within the tags table files" ()
  (MULTIPLE-VALUE (*TAGS-QUERY-REPLACE-FROM* *TAGS-QUERY-REPLACE-TO*)
    (QUERY-REPLACE-STRINGS NIL))
  (SETQ *TAGS-QUERY-REPLACE-DELIMITED* (AND *NUMERIC-ARG-P* *NUMERIC-ARG*))
  (NEXT-FILE T)
  (COMMAND-STORE 'COM-CONTINUE-TAGS-QUERY-REPLACE #/. *ZMACS-COMTAB*)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (COM-CONTINUE-TAGS-QUERY-REPLACE))

(DEFCOM COM-CONTINUE-TAGS-QUERY-REPLACE "Continue the last tags query replace" ()
  (DO ((VAL)) (NIL)
    (SETQ VAL (QUERY-REPLACE (POINT) *TAGS-QUERY-REPLACE-FROM* *TAGS-QUERY-REPLACE-TO*
			     *TAGS-QUERY-REPLACE-DELIMITED*))
    (AND (EQ VAL 'ABORTED) (RETURN DIS-TEXT))
    (NEXT-FILE NIL)
    (MUST-REDISPLAY *WINDOW* DIS-TEXT)))

(DEFCOM COM-TAGS-MULTIPLE-QUERY-REPLACE "Perform a query replace within the tags table files"
	()
  (MULTIPLE-VALUE-BIND (FROM-LIST TO-LIST)
      (MULTIPLE-QUERY-REPLACE-STRINGS NIL)
    (TAGS-MULTIPLE-QUERY-REPLACE FROM-LIST TO-LIST (AND *NUMERIC-ARG-P* *NUMERIC-ARG*))))

(DEFCOM COM-TAGS-MULTIPLE-QUERY-REPLACE-FROM-BUFFER
	"Perform a multiple query replace from the contents of the specified buffer" ()
  (MULTIPLE-VALUE-BIND (FROM-LIST TO-LIST)
      (PARSE-BUFFER-REPLACE-PAIRS *INTERVAL*)
    (TAGS-MULTIPLE-QUERY-REPLACE FROM-LIST TO-LIST (AND *NUMERIC-ARG-P* *NUMERIC-ARG*))))

(DEFCOM COM-MULTIPLE-QUERY-REPLACE-FROM-BUFFER
	"Perform a multiple query replace from the contents of the specified buffer" ()
  (WITH-QUERY-REPLACE-INTERVAL (REGION-P)
    (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
					*ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*))
	  FROM-LIST TO-LIST)
      (MULTIPLE-VALUE (FROM-LIST TO-LIST)
	(PARSE-BUFFER-REPLACE-PAIRS T))
      (QUERY-REPLACE-LIST (POINT) FROM-LIST TO-LIST *NUMERIC-ARG-P*)))
  DIS-TEXT)

(DEFUN TAGS-MULTIPLE-QUERY-REPLACE (FROM-LIST TO-LIST ARG)
  (SETQ *TAGS-QUERY-REPLACE-FROM* FROM-LIST
	*TAGS-QUERY-REPLACE-TO* TO-LIST
	*TAGS-QUERY-REPLACE-DELIMITED* ARG)
  (NEXT-FILE T)
  (COMMAND-STORE 'COM-CONTINUE-TAGS-MULTIPLE-QUERY-REPLACE #/. *ZMACS-COMTAB*)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (COM-CONTINUE-TAGS-MULTIPLE-QUERY-REPLACE))

(DEFCOM COM-CONTINUE-TAGS-MULTIPLE-QUERY-REPLACE "Continue the last tags query replace" ()
  (DO ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *TAGS-QUERY-REPLACE-DELIMITED*
					  (MINUSP *TAGS-QUERY-REPLACE-DELIMITED*))
				*ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*))
       (VAL))
      (NIL)
    (SETQ VAL (QUERY-REPLACE-LIST (POINT) *TAGS-QUERY-REPLACE-FROM* *TAGS-QUERY-REPLACE-TO*
				  *TAGS-QUERY-REPLACE-DELIMITED*))
    (AND (EQ VAL 'ABORTED) (RETURN DIS-TEXT))
    (NEXT-FILE NIL)
    (MUST-REDISPLAY *WINDOW* DIS-TEXT)))

(DEFVAR *ZMACS-CURRENT-TAG-TABLE* NIL)

(DEFCOM COM-SELECT-TAG-TABLE "Make a tag table current for commands like tags search" ()
  (SETQ *ZMACS-CURRENT-TAG-TABLE* (SELECT-TAG-TABLE NIL))
  DIS-NONE)

(DEFCOM COM-SELECT-SYSTEM-AS-TAG-TABLE "Make the files in a system behave like a tags file"
	()
  (LET ((SYSTEM-NAME (COMPLETING-READ-FROM-MINI-BUFFER
		  "Select system:" (SI:ALL-SYSTEMS-NAME-ALIST))))
    (AND (LISTP SYSTEM-NAME) (SETQ SYSTEM-NAME (CAR SYSTEM-NAME)))	;A string
    (SELECT-FILE-LIST-AS-TAG-TABLE (SI:SYSTEM-SOURCE-FILES SYSTEM-NAME) SYSTEM-NAME))
  DIS-NONE)

(DEFCOM COM-SELECT-ALL-BUFFERS-AS-TAG-TABLE "Select all buffers currently read in" ()
  (SELECT-FILE-LIST-AS-TAG-TABLE (LOOP FOR BUFFER IN *ZMACS-BUFFER-LIST*
				       WHEN (BUFFER-FILE-ID BUFFER)
				       COLLECT (BUFFER-PATHNAME BUFFER))
				 "All buffers")
  DIS-NONE)

(DEFUN SELECT-FILE-LIST-AS-TAG-TABLE (FILE-LIST NAME)
  (SETQ *ZMACS-CURRENT-TAG-TABLE* (FS:MAKE-DUMMY-PATHNAME NAME))
  (FUNCALL *ZMACS-CURRENT-TAG-TABLE* ':PUTPROP
	   (MAPCAR #'(LAMBDA (X) (FS:MERGE-PATHNAME-DEFAULTS X *PATHNAME-DEFAULTS*))
		   FILE-LIST)
	   'ZMACS-TAG-TABLE-FILE-SYMBOLS)
  (PUSH (CONS NAME *ZMACS-CURRENT-TAG-TABLE*) *ZMACS-TAG-TABLE-ALIST*))

;;; Assure that we have only one tags table to worry about
(DEFUN SELECT-TAG-TABLE (&OPTIONAL (DEFAULT-P T))
  (COND ((NULL *ZMACS-TAG-TABLE-ALIST*)
	 (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Tag table:"
						    (PATHNAME-DEFAULTS) ':TAGS)))
	   (READ-TAG-TABLE PATHNAME)
	   PATHNAME))
	((AND DEFAULT-P *ZMACS-CURRENT-TAG-TABLE*)
	 *ZMACS-CURRENT-TAG-TABLE*)
	((AND DEFAULT-P (NULL (CDR *ZMACS-TAG-TABLE-ALIST*)))
	 (CDAR *ZMACS-TAG-TABLE-ALIST*))
	(T
	 (LET ((TABLE (COMPLETING-READ-FROM-MINI-BUFFER "Tag table:"
							*ZMACS-TAG-TABLE-ALIST*)))
	   (COND ((EQUAL TABLE "")
		  (COND (*ZMACS-CURRENT-TAG-TABLE* *ZMACS-CURRENT-TAG-TABLE*)
			(T (BARF))))
		 (T
		  (CDR TABLE)))))))

;;; Return two lists from a given buffer
(DEFUN PARSE-BUFFER-REPLACE-PAIRS (DEFAULT &AUX *INTERVAL*)
  (SETQ *INTERVAL* (READ-BUFFER-NAME "Use replacements in buffer:" DEFAULT))
  (DO ((BP (INTERVAL-FIRST-BP *INTERVAL*))
       (END-BP (INTERVAL-LAST-BP *INTERVAL*))
       (FROM-LIST) (TO-LIST) (TEM))
      (NIL)
    (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS* BP))
    (AND (BP-= BP END-BP) (RETURN (NREVERSE FROM-LIST) (NREVERSE TO-LIST)))
    (IF (= (BP-CH-CHAR BP) #/;)
	(SETQ BP (BEG-LINE BP 1))
	(MULTIPLE-VALUE (TEM BP)
	  (PARSE-BUFFER-REPLACE-PAIRS-1 BP))
	(PUSH TEM FROM-LIST)
	(SETQ BP (FORWARD-OVER *BLANKS* BP))
	(AND (END-LINE-P BP) (BARF "Only one item on line ~S" (BP-LINE BP)))
	(MULTIPLE-VALUE (TEM BP)
	  (PARSE-BUFFER-REPLACE-PAIRS-1 BP))
	(PUSH TEM TO-LIST))))

(DEFUN PARSE-BUFFER-REPLACE-PAIRS-1 (BP &AUX BP1 STR)
  (OR (SETQ BP1 (FORWARD-SEXP BP)) (BARF "Premature eof on line ~S" (BP-LINE BP)))
  (SETQ STR (STRING-INTERVAL BP BP1 T))
  (AND (= (AREF STR 0) #/")
       (SETQ STR (READ-FROM-STRING STR)))
  (VALUES STR BP1))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FUNCTION-NAME "Arglist"
			  TYPEOUT-MENU-ARGLIST NIL
			  "Print arglist for this function.")

(DEFUN TYPEOUT-MENU-ARGLIST (FUNCTION)
  (TYPEIN-LINE "~S: ~A" FUNCTION (ARGLIST FUNCTION))
  T)                                            ;Leave the typeout window there

(DEFUN TYPEOUT-YES-OR-NO-P (&REST FORMAT-ARGS)
  (LET ((QUERY-IO *TYPEOUT-WINDOW*))
    (LEXPR-FUNCALL #'FQUERY '#,`(:SELECT T
				 :TYPE :READLINE
				 :CHOICES ,FORMAT:YES-OR-NO-P-CHOICES)
		   FORMAT-ARGS)))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* BP "Move" MOVE-TO-BP T
			  "Move to this line.")

(DEFUN MOVE-TO-BP (BP &AUX INTERVAL)
  (AND (SETQ INTERVAL (BP-TOP-LEVEL-NODE BP))
       (NEQ INTERVAL *INTERVAL*)
       (MAKE-BUFFER-CURRENT INTERVAL))
  (MOVE-BP (POINT) BP)
  NIL)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FILE "Find" FIND-DEFAULTED-FILE T
			  "Find file this file.")

(DEFUN FIND-DEFAULTED-FILE (STRING &AUX PATHNAME VERSION)
  (SETQ PATHNAME (MAKE-DEFAULTED-PATHNAME (STRING STRING) (PATHNAME-DEFAULTS)))
  ;;It we get a specific file, see if that was the newest and if so, use that instead
  (AND (NOT (MEMQ (SETQ VERSION (FUNCALL PATHNAME ':VERSION)) '(:NEWEST :UNSPECIFIC)))
       (= VERSION (FUNCALL (FUNCALL (FUNCALL PATHNAME ':NEW-VERSION ':NEWEST) ':TRUENAME)
			   ':VERSION))
       (SETQ PATHNAME (FUNCALL PATHNAME ':NEW-VERSION ':NEWEST)))
  (FIND-FILE PATHNAME))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FILE "Load" LOAD-DEFAULTED-FILE NIL
			  "LOAD this file.")

(DEFUN LOAD-DEFAULTED-FILE (STRING)
  (LOAD (MAKE-DEFAULTED-PATHNAME (STRING STRING) (PATHNAME-DEFAULTS))))

;;; Multiple-window commands.

(DEFCOM COM-TWO-WINDOWS "Select two windows.
Split the frame into two editor windows and select the bottom one.
With a numeric argument, make the second window point to the same
buffer that the first window does." ()
  (SWITCH-WINDOWS *NUMERIC-ARG-P* 2)
  DIS-NONE)

(DEFCOM COM-VIEW-TWO-WINDOWS "Select two windows, but stay in the first one.
Split the frame into two editor windows and select the top one.
With a numeric argument, make the second window point to the same
buffer that the first window does." ()
  (SWITCH-WINDOWS *NUMERIC-ARG-P* 1)
  DIS-NONE)

;;; Note that this is designed to ask the question before doing any display
(DEFCOM COM-MODIFIED-TWO-WINDOWS "Find a buffer, file or tag in the other window." ()
  (TYPEIN-LINE "Buffer, file or tag (B, F, or T): ")
  (SELECTQ (CHAR-UPCASE (LDB %%CH-CHAR (FUNCALL STANDARD-INPUT ':TYI)))
       (#/B (LET ((BUFFER (READ-BUFFER-NAME "Select buffer:" T 'MAYBE)))
	      (SWITCH-WINDOWS)
	      (MAKE-BUFFER-CURRENT BUFFER)
	      DIS-TEXT))
	(#/F (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Find file:" (PATHNAME-DEFAULTS)
						      NIL NIL ':NEW-OK)))
	       (SWITCH-WINDOWS)
	       (FIND-FILE PATHNAME))
	     (MAYBE-DISPLAY-DIRECTORY ':READ)
	     DIS-TEXT)
       ((#/T #/.) (MULTIPLE-VALUE-BIND (SYM NAME)
		      (READ-FUNCTION-NAME "Edit function" (RELEVANT-FUNCTION-NAME (POINT))
					  'AARRAY-OK)
		    (SWITCH-WINDOWS)
		    (EDIT-DEFINITION-INTERNAL SYM NAME *NUMERIC-ARG-P*))
		  DIS-NONE)
       (OTHERWISE (BARF))))

(DEFUN SWITCH-WINDOWS (&OPTIONAL CHANGE-INTERVAL (ONE-TO-SELECT 2)
		       &AUX TOP-WINDOW BOTTOM-WINDOW)
  (MULTIPLE-VALUE (TOP-WINDOW BOTTOM-WINDOW)
    (FUNCALL (WINDOW-FRAME *WINDOW*) ':TWO-EDITOR-WINDOWS))
  (COND (CHANGE-INTERVAL
	 (SET-WINDOW-INTERVAL BOTTOM-WINDOW *INTERVAL*)
	 (MOVE-BP (WINDOW-POINT BOTTOM-WINDOW) (POINT))))
  (COND ((AND (WINDOW-EXPOSED-P TOP-WINDOW) (WINDOW-EXPOSED-P BOTTOM-WINDOW))
	 (COM-OTHER-WINDOW))
	(T
	 (TWO-WINDOWS TOP-WINDOW BOTTOM-WINDOW)
	 (LET ((WINDOW (IF (= ONE-TO-SELECT 1) TOP-WINDOW BOTTOM-WINDOW)))
	   (IF (EQ WINDOW *WINDOW*) (SELECT-WINDOW WINDOW)
	       (MAKE-WINDOW-CURRENT WINDOW))))))

(DEFCOM COM-ONE-WINDOW "Select only one window." ()
  (LET ((WINDOW (IF *NUMERIC-ARG-P*
		    *WINDOW*
		    (FUNCALL (WINDOW-FRAME *WINDOW*) ':TWO-EDITOR-WINDOWS))))
    (PREPARE-WINDOW-FOR-REDISPLAY WINDOW)
    (MAKE-WINDOW-FULL-SCREEN WINDOW)
    (OR (EQ WINDOW *WINDOW*) (MAKE-WINDOW-CURRENT WINDOW)))
  DIS-NONE)
                                               
(DEFCOM COM-OTHER-WINDOW "Move to the other window" ()
  (LET ((WINDOW (OTHER-WINDOW)))
    (IF WINDOW
	(ROTATE-TO-OTHER-WINDOW WINDOW)
	(MULTIPLE-VALUE (NIL WINDOW)
	  (FUNCALL (WINDOW-FRAME *WINDOW*) ':TWO-EDITOR-WINDOWS))
	(OR (WINDOW-INTERVAL WINDOW) (BARF "Only one window"))
	(MAKE-WINDOW-FULL-SCREEN WINDOW)
	(MAKE-WINDOW-CURRENT WINDOW)))
  DIS-BPS)

(DEFUN ROTATE-TO-OTHER-WINDOW (WINDOW)
  (SETQ *WINDOW-LIST* (NCONC (DELQ *WINDOW* *WINDOW-LIST*) (NCONS *WINDOW*)))
  (MAKE-WINDOW-CURRENT WINDOW))

(DEFCOM COM-SCROLL-OTHER-WINDOW "Scroll other window up several lines.
Specify the number as a numeric argument, negative for down.
The default is a whole screenful up." (KM)
  (LET ((WINDOW (OTHER-WINDOW)))
    (OR WINDOW (BARF "There is only one window"))
    (PREPARE-WINDOW-FOR-REDISPLAY WINDOW)
    (RECENTER-WINDOW-RELATIVE WINDOW (IF (MEMQ *NUMERIC-ARG-P* '(:DIGITS :CONTROL-U))
					 *NUMERIC-ARG*
					 (* (1- (WINDOW-N-PLINES WINDOW)) *NUMERIC-ARG*))))
  DIS-NONE)

(DEFCOM COM-GROW-WINDOW "Make this window large by argument number of lines." (KM)
  (MULTIPLE-VALUE-BIND (TOP-WINDOW BOTTOM-WINDOW)
      (FUNCALL (WINDOW-FRAME *WINDOW*) ':TWO-EDITOR-WINDOWS)
    (OR (WINDOW-EXPOSED-P BOTTOM-WINDOW) (BARF "There is only one window"))
    (PREPARE-WINDOW-FOR-REDISPLAY TOP-WINDOW)
    (PREPARE-WINDOW-FOR-REDISPLAY BOTTOM-WINDOW)
    (LET ((NLINES (IF (EQ *WINDOW* TOP-WINDOW) *NUMERIC-ARG* (- *NUMERIC-ARG*))))
      (GROW-WINDOW TOP-WINDOW BOTTOM-WINDOW NLINES)))
  DIS-NONE)

;;; This tries to find another window to use
(DEFUN OTHER-WINDOW ()
  (DOLIST (WINDOW *WINDOW-LIST*)
    (AND (NEQ WINDOW *WINDOW*)
	 (WINDOW-EXPOSED-P WINDOW)
	 (RETURN WINDOW))))

(DEFCOM COM-TWO-WINDOWS-SHOWING-REGION "Make two windows on the same buffer.
The top one showing the current region." ()
  (REGION (BP1 BP2)
    (MULTIPLE-VALUE-BIND (TOP-WINDOW BOTTOM-WINDOW)
	(FUNCALL (WINDOW-FRAME *WINDOW*) ':TWO-EDITOR-WINDOWS)
      (SPLIT-SCREEN-BETWEEN-TWO-WINDOWS TOP-WINDOW BOTTOM-WINDOW (COUNT-LINES BP1 BP2 T))
      (SET-WINDOW-INTERVAL TOP-WINDOW *INTERVAL*)
      (RECENTER-WINDOW TOP-WINDOW ':START BP1)
      (MOVE-BP (WINDOW-POINT TOP-WINDOW) BP2)
      (SET-WINDOW-INTERVAL BOTTOM-WINDOW *INTERVAL*)
      (MOVE-BP (WINDOW-POINT BOTTOM-WINDOW) BP1)
      (MAKE-WINDOW-CURRENT BOTTOM-WINDOW)))
  DIS-TEXT)

(DEFVAR *SPLIT-SCREEN-WINDOW-LIST*)

(DEFCOM COM-SPLIT-SCREEN "Make several windows split among the buffers as specified." ()
  (LET* ((FRAME (WINDOW-FRAME *WINDOW*))
	 (BUFFER-LIST (SPLIT-SCREEN-AMONG-BUFFERS-VIA-MENUS FRAME
							    *ZMACS-BUFFER-NAME-ALIST*))
	 WINDOW-LIST)
    (COND (BUFFER-LIST
	   (SETQ WINDOW-LIST (SPLIT-SCREEN-AMONG-BUFFERS-DO-IT FRAME BUFFER-LIST))
	   (DO ((BL BUFFER-LIST (CDR BL)))
	       ((NULL BL))
	     (AND (TYPEP (CAR BL) 'FS:PATHNAME)
		  (MULTIPLE-VALUE-BIND (NIL BUF)
		      (FIND-FILE (CAR BL) NIL)
		    (SETF (CAR BL) BUF))))
	   (MAKE-WINDOW-CURRENT (CAR WINDOW-LIST))
	   (MAKE-BUFFER-CURRENT (CAR BUFFER-LIST))
	   (DO ((BL BUFFER-LIST (CDR BL))
		(WL WINDOW-LIST (CDR WL)))
	       ((NULL BL))
	     (SET-WINDOW-BUFFER (CAR WL) (CAR BL))))))
  DIS-TEXT)

(DEFCOM COM-EDIT-METHODS "Edit all methods for specified message." ()
  (LIST-METHODS-INTERNAL "Edit methods for")
  (COM-NEXT-CALLER))

(DEFCOM COM-LIST-METHODS "List all classes and flavors with methods for specified message." ()
  (MULTIPLE-VALUE-BIND (CLASSES-AND-FUNCTION-SYMBOLS MESSAGE)
      (LIST-METHODS-INTERNAL "List classes and flavors with methods for")
    (EDIT-FUNCTIONS-DISPLAY ;; The printed-representation of each item is the flavor name,
			    ;; including package prefix if necessary, and the method type
			    ;; in parentheses.  The cdr (function-name)
			    ;; of each item is the mumble-class-mumble-method symbol.
			    (MAPCAR #'(LAMBDA (X)
					(CONS (FORMAT NIL "~S~@[ (:~A)~]" (CAR X) (CADR X))
					      (CADDR X)))
				    CLASSES-AND-FUNCTION-SYMBOLS)
			    "Classes and flavors with ~S methods:"
			    "No methods for ~S found."
			    MESSAGE))
  DIS-NONE)

(DEFUN LIST-METHODS-INTERNAL (PROMPT &AUX FL TEM MESSAGE)
  (SETQ MESSAGE (GET-MESSAGE-NAME PROMPT))
  (LET ((CLASSES-AND-FUNCTION-SYMBOLS
	  (DO ((L (CONS OBJECT-CLASS (SI:ALL-SUBCLASSES-OF-CLASS OBJECT-CLASS)) (CDR L))
	       (R NIL)
	       (SYM))
	      ((NULL L) R)
	    (COND ((SETQ SYM (<- (CAR L) ':METHOD-FOR MESSAGE NIL))
		   (PUSH (LIST (<- (CAR L) ':CLASS-SYMBOL) NIL SYM) R))))))
    (DOLIST (FLAVOR SI:*ALL-FLAVOR-NAMES*)
      (AND (SETQ FL (GET FLAVOR 'SI:FLAVOR))
	   (SETQ TEM (ASSQ MESSAGE (SI:FLAVOR-METHOD-TABLE FL)))
	   (DOLIST (METH (CDDDR TEM))
	     (OR (EQ (SI:METH-METHOD-TYPE METH) ':COMBINED)
		 (PUSH (LIST FLAVOR (SI:METH-METHOD-TYPE METH) (SI:METH-FUNCTION-SPEC METH))
		       CLASSES-AND-FUNCTION-SYMBOLS)))))
    (SETUP-ZMACS-CALLERS-TO-BE-EDITED (MAPCAR #'CADDR CLASSES-AND-FUNCTION-SYMBOLS))
    (PROG () (RETURN CLASSES-AND-FUNCTION-SYMBOLS MESSAGE))))

(DEFUN GET-MESSAGE-NAME (PROMPT)
  (PKG-BIND "USER"	;So the colon can be omitted
    (MULTIPLE-VALUE-BIND (SYM STR)
	(READ-FUNCTION-NAME PROMPT
	  (LET ((FUN (RELEVANT-FUNCTION-NAME (POINT))))
	    (AND (MEMQ FUN '(FUNCALL LEXPR-FUNCALL <- FUNCALL-SELF LEXPR-FUNCALL-SELF))
		 (RELEVANT-METHOD-NAME (POINT)
				       (IF (MEMQ FUN '(FUNCALL-SELF LEXPR-FUNCALL-SELF)) 1 2))))
	  NIL T)
      ;; Kludge around to not get screwed by completions to funny symbols
      ;; while still working if user points with the mouse
      (IF STR (READ-FROM-STRING STR) SYM))))

(DEFCOM COM-EDIT-COMBINED-METHODS
	"Edit all methods for specified message to specified flavor." ()
  (LIST-COMBINED-METHODS-INTERNAL "Edit")
  (COM-NEXT-CALLER))

(DEFCOM COM-LIST-COMBINED-METHODS
	"List all methods for specified message to specified flavor." ()
  (MULTIPLE-VALUE-BIND (FLAVOR MESSAGE)
      (LIST-COMBINED-METHODS-INTERNAL "List")
    ;Duplicates code from LIST-ZMACS-CALLERS-TO-BE-EDITED in order to
    ;put the methods in execution order rather than alphabetical order
    (EDIT-FUNCTIONS-DISPLAY ;; Do package prefixes and function specs correctly
			    (MAPCAR #'(LAMBDA (X) (CONS (FORMAT NIL "~S" X) X))
				    *ZMACS-CALLERS-TO-BE-EDITED*)
			    "Combined methods for ~S message to ~S flavor:"
			    NIL
			    MESSAGE FLAVOR))
  DIS-NONE)

(DEFUN LIST-COMBINED-METHODS-INTERNAL (OP &AUX MESSAGE FLAVOR)
  (SETQ MESSAGE (GET-MESSAGE-NAME (FORMAT NIL "~A combined methods for message" OP)))
  (SETQ FLAVOR (READ-FLAVOR-NAME (FORMAT NIL "~A combined methods for ~S message to flavor"
					       OP MESSAGE)
			"You are typing the name of a flavor, to see its combined methods"))
  ;Duplicates code from SETUP-ZMACS-CALLERS-TO-BE-EDITED in order to
  ;put the methods in execution order rather than alphabetical order
  (SETQ *ZMACS-CALLERS-TO-BE-EDITED* (FIND-COMBINED-METHODS FLAVOR MESSAGE))
  (COMMAND-STORE 'COM-NEXT-CALLER #/. *ZMACS-COMTAB*)
  (PROG () (RETURN FLAVOR MESSAGE)))

(DEFUN FIND-COMBINED-METHODS (FLAVOR MESSAGE &AUX FL SM METHOD)
  "Return a list of the non-combined methods involved in handling MESSAGE to FLAVOR"
  (OR (SETQ FL (GET FLAVOR 'SI:FLAVOR))
      (BARF "~S not DEFFLAVOR'ed" FLAVOR))
  (OR (SETQ SM (SI:FLAVOR-SELECT-METHOD FL))
      (BARF "~S not a composed, instantiated flavor" FLAVOR))
  (OR (SETQ METHOD (CDR (SI:ASSQ-CAREFUL MESSAGE (%MAKE-POINTER DTP-LIST SM))))
      (BARF "Flavor ~S does not handle message ~S" FLAVOR MESSAGE))
  (SETQ METHOD (EH:FUNCTION-NAME METHOD))
  (COND ((SETQ SM (CDDDR (SI:FUNCTION-SPEC-GET METHOD 'SI:COMBINED-METHOD-DERIVATION)))
	 (NCONC (REVERSE (CDR (ASSQ ':WRAPPER SM)))	;Try to approximate the order
		(REVERSE (CDR (ASSQ ':BEFORE SM)))	;in which they're called
		(REVERSE (CDR (ASSQ NIL SM)))
		(COPYLIST (CDR (ASSQ ':AFTER SM)))
		(MAPCAN #'(LAMBDA (X)
			    (AND (NOT (MEMQ (CAR X) '(:WRAPPER :BEFORE NIL :AFTER)))
				 (REVERSE (CDR X))))
			SM)))
	(T (LIST METHOD))))

(DEFCOM COM-EDIT-COMPILER-WARNINGS "Edit compiler warning messages in two window mode.
C-. will move to the next function in error." ()
  (EDIT-COMPILER-WARNINGS T))

(DEFCOM COM-CONTINUE-EDITING-COMPILER-WARNINGS "Move to the next erring function." ()
  (EDIT-COMPILER-WARNINGS NIL))

(DEFVAR *EDIT-COMPILER-WARNINGS-MINIMUM-TOP-WINDOW-SIZE* 10.)
(DEFVAR *EDIT-COMPILER-WARNINGS-MAXIMUM-TOP-WINDOW-SIZE* 20.)
(DEFVAR *EDIT-COMPILER-WARNINGS-BP*)
(DEFUN EDIT-COMPILER-WARNINGS (RE-INIT-P &AUX BUFFER START-BP END-BP FILE-LINE FUNCTION
					      TOP-WINDOW BOTTOM-WINDOW)
  (OR (SETQ BUFFER (FIND-BUFFER-NAMED COMPILER:COMPILER-WARNINGS-BUFFER))
      (BARF "No compiler warnings buffer"))
  (COND (RE-INIT-P
	 (MULTIPLE-VALUE-BIND (START-BP END-BP)
	     (FIND-WARNINGS-FOR-FILE BUFFER (IF (BUFFER-FILE-ID *INTERVAL*)
						(STRING (BUFFER-PATHNAME *INTERVAL*))
						(BUFFER-NAME *INTERVAL*)))
	   (COND (START-BP
		  (SETQ START-BP (BEG-LINE START-BP -1 T))
		  (LET ((COPY (COPY-INTERVAL START-BP END-BP T)))
		    (DELETE-INTERVAL START-BP END-BP T)
		    (INSERT-INTERVAL (INTERVAL-FIRST-BP BUFFER) COPY)))))
	 (COMMAND-STORE 'COM-CONTINUE-EDITING-COMPILER-WARNINGS #/. *ZMACS-COMTAB*)
	 (SETQ START-BP (COPY-BP (INTERVAL-FIRST-BP BUFFER) ':MOVES)))
	(T
	 (SETQ START-BP *EDIT-COMPILER-WARNINGS-BP*)))
  (DO ((LINE (BP-LINE START-BP) (LINE-NEXT LINE))
       (SECOND-P NIL)
       (TEM)
       (FIRST-P RE-INIT-P PAGE-P)
       (PAGE-P))
      ((NULL LINE)
       (COND ((NOT SECOND-P)
	      (MAKE-WINDOW-FULL-SCREEN *WINDOW*)
	      (BARF "No more compiler warnings"))))
    (COND ((SETQ PAGE-P (STRING-MATCH "" LINE))
	   (AND SECOND-P (RETURN (SETQ END-BP LINE))))
	  (FIRST-P
	   (AND (STRING-MATCH "Warnings for " LINE)
		(SETQ FILE-LINE LINE)))
	  ((SETQ TEM (STRING-MATCH "<< While compiling " LINE))
	   (IF SECOND-P (RETURN (SETQ END-BP LINE))
	       (SETQ FUNCTION (SUBSTRING LINE TEM
					 (- (LINE-LENGTH LINE) 3))
		     SECOND-P T)
	       (MOVE-BP START-BP LINE 0)))))
  (SETQ END-BP (IF END-BP (CREATE-BP END-BP 0) (INTERVAL-LAST-BP BUFFER)))
  (MULTIPLE-VALUE (TOP-WINDOW BOTTOM-WINDOW)
    (FUNCALL (WINDOW-FRAME *WINDOW*) ':TWO-EDITOR-WINDOWS))
  (AND (EQ TOP-WINDOW *WINDOW*)
       (PSETQ TOP-WINDOW BOTTOM-WINDOW BOTTOM-WINDOW TOP-WINDOW))
  (FUNCALL *TYPEOUT-WINDOW* ':MAKE-COMPLETE)	;Flush copy of warnings
  (SPLIT-SCREEN-BETWEEN-TWO-WINDOWS TOP-WINDOW BOTTOM-WINDOW
				    (RANGE (COUNT-LINES START-BP END-BP T)
					   *EDIT-COMPILER-WARNINGS-MINIMUM-TOP-WINDOW-SIZE*
					   *EDIT-COMPILER-WARNINGS-MAXIMUM-TOP-WINDOW-SIZE*))
  (SET-WINDOW-BUFFER TOP-WINDOW BUFFER)
  (MOVE-BP (WINDOW-POINT TOP-WINDOW) END-BP)
  (RECENTER-WINDOW TOP-WINDOW ':START START-BP)
  (MAKE-WINDOW-CURRENT BOTTOM-WINDOW)
  (AND FILE-LINE
       (COND ((%STRING-EQUAL FILE-LINE 13. "file " 0 5)
	      (FIND-DEFAULTED-FILE (SUBSTRING FILE-LINE 18.)))
	     ((%STRING-EQUAL FILE-LINE 13. "buffer " 0 7)
	      (AND (SETQ BUFFER (FIND-BUFFER-NAMED (SUBSTRING FILE-LINE 20.)))
		   (MAKE-BUFFER-CURRENT BUFFER)))))
  (EDIT-DEFINITION FUNCTION)
  (MOVE-BP START-BP END-BP)
  (SETQ *EDIT-COMPILER-WARNINGS-BP* START-BP)
  DIS-TEXT)
