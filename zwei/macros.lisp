;;; Macros for ZWEI.   -*- Mode:LISP; Package:ZWEI -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFMACRO CHARMAP ((FROM-BP-FORM TO-BP-FORM . RETURN-FORMS) &BODY BODY)
  `(CHARMAP-PER-LINE (,FROM-BP-FORM ,TO-BP-FORM . ,RETURN-FORMS) (NIL) . ,BODY))

(DEFMACRO CHARMAP-PER-LINE ((FROM-BP-FORM TO-BP-FORM . RETURN-FORMS) LINE-FORMS &BODY BODY)
  `(LET ((*FROM-BP* ,FROM-BP-FORM)
         (*TO-BP* ,TO-BP-FORM))
     (DO-NAMED *CHARMAP*
        ((LINE (BP-LINE *FROM-BP*) (LINE-NEXT LINE))
         (*FIRST-INDEX* (BP-INDEX *FROM-BP*) 0)
         (*LAST-LINE* (BP-LINE *TO-BP*))
	 (*THIS-IS-THE-LAST-LINE*))
        (NIL)
      (SETQ *THIS-IS-THE-LAST-LINE* (EQ LINE *LAST-LINE*))
      ,@LINE-FORMS
      (DO ((INDEX *FIRST-INDEX* (1+ INDEX))
           (*LAST-INDEX* (IF *THIS-IS-THE-LAST-LINE* (1- (BP-INDEX *TO-BP*))
			     (LINE-LENGTH LINE))))
          ((> INDEX *LAST-INDEX*)
           (IF *THIS-IS-THE-LAST-LINE*
               (RETURN-FROM *CHARMAP* . ,RETURN-FORMS)))
        . ,BODY))))

(DEFMACRO CHARMAP-RETURN RETURN-FORMS
  `(RETURN-FROM *CHARMAP* . ,RETURN-FORMS))

(DEFMACRO CHARMAP-CHAR ()
  '(COND ((AND (= INDEX *LAST-INDEX*)
	       (NOT *THIS-IS-THE-LAST-LINE*))
	  #\CR)
	 (T (AREF LINE INDEX))))

(DEFMACRO CHARMAP-CH-CHAR ()
  '(LDB %%CH-CHAR (CHARMAP-CHAR)))

(DEFMACRO CHARMAP-SET-CHAR (CHAR)
  `(PROGN
    (MUNG-BP-INTERVAL *FROM-BP*)
    (MUNG-LINE LINE)
    (COND ((AND (= INDEX *LAST-INDEX*)
		(NOT *THIS-IS-THE-LAST-LINE*))
	   (INSERT (DELETE-INTERVAL (END-LINE LINE) (BEG-LINE LINE 1) T) ,CHAR))
	  (T (ASET ,CHAR LINE INDEX)))))

(DEFMACRO CHARMAP-BP-BEFORE ()
  '(CREATE-BP LINE INDEX))

(DEFMACRO CHARMAP-BP-AFTER ()
  '(COND ((AND (= INDEX *LAST-INDEX*)
	       (NOT *THIS-IS-THE-LAST-LINE*))
	  (CREATE-BP (LINE-NEXT LINE) 0))
	 (T
	  (CREATE-BP LINE (1+ INDEX)))))

(DEFMACRO CHARMAP-INCREMENT RETURN-FORMS
  `(COND ((> (SETQ INDEX (1+ INDEX)) *LAST-INDEX*)
	  (COND (*THIS-IS-THE-LAST-LINE*
		 (RETURN-FROM *CHARMAP* . ,RETURN-FORMS))
		(T (SETQ INDEX 0
			 LINE (LINE-NEXT LINE)
			 *THIS-IS-THE-LAST-LINE* (EQ LINE *LAST-LINE*)
			 *LAST-INDEX* (IF *THIS-IS-THE-LAST-LINE* (1- (BP-INDEX *TO-BP*))
					  (LINE-LENGTH LINE))))))))

(DEFMACRO CHARMAP-LINE ()
  'LINE)

(DEFMACRO RCHARMAP ((FROM-BP-FORM TO-BP-FORM . RETURN-FORMS) &BODY BODY)
  `(RCHARMAP-PER-LINE (,FROM-BP-FORM ,TO-BP-FORM . ,RETURN-FORMS) (NIL) . ,BODY))

(DEFMACRO RCHARMAP-PER-LINE ((FROM-BP-FORM TO-BP-FORM . RETURN-FORMS) LINE-FORMS &BODY BODY)
  `(LET ((*FROM-BP* ,FROM-BP-FORM)
         (*TO-BP* ,TO-BP-FORM))
     (DO-NAMED *RCHARMAP*
        ((LINE (BP-LINE *FROM-BP*) (LINE-PREVIOUS LINE))
         (*FIRST-LINE-P* T NIL)
         (*LAST-LINE* (BP-LINE *TO-BP*))
         (*THIS-IS-THE-LAST-LINE*))
        (NIL)
      (SETQ *THIS-IS-THE-LAST-LINE* (EQ LINE *LAST-LINE*))
      ,@LINE-FORMS
      ;; Note that index can take on the value of the length of a line, which means the CR
      (DO ((INDEX (IF *FIRST-LINE-P* (1- (BP-INDEX *FROM-BP*)) (LINE-LENGTH LINE))
                  (1- INDEX))
           (*LAST-INDEX* (IF *THIS-IS-THE-LAST-LINE*
                             (BP-INDEX *TO-BP*)
                             0)))
          ((< INDEX *LAST-INDEX*)
           (IF *THIS-IS-THE-LAST-LINE*
               (RETURN-FROM *RCHARMAP* . ,RETURN-FORMS)))
        . ,BODY))))

(DEFMACRO RCHARMAP-RETURN RETURN-FORMS
  `(RETURN-FROM *RCHARMAP* . ,RETURN-FORMS))

(DEFMACRO RCHARMAP-CHAR ()
  '(COND ((= INDEX (LINE-LENGTH LINE)) #\CR)
         (T (AREF LINE INDEX))))

(DEFMACRO RCHARMAP-CH-CHAR ()
  '(LDB %%CH-CHAR (RCHARMAP-CHAR)))

(DEFMACRO RCHARMAP-CHAR-BEFORE ()
  '(IF (ZEROP INDEX) #\CR (AREF LINE (1- INDEX))))

(DEFMACRO RCHARMAP-CH-CHAR-BEFORE ()
  '(LDB %%CH-CHAR (RCHARMAP-CHAR-BEFORE)))

(DEFMACRO RCHARMAP-SET-CHAR (CHAR)
  `(PROGN
    (MUNG-BP-INTERVAL *FROM-BP*)
    (MUNG-LINE LINE)
    (COND ((= INDEX (LINE-LENGTH LINE))
	   (INSERT (DELETE-INTERVAL (END-LINE LINE) (BEG-LINE LINE 1) T) ,CHAR))
	  (T (ASET ,CHAR LINE INDEX)))))

(DEFMACRO RCHARMAP-BP-BEFORE ()
  '(CREATE-BP LINE INDEX))

(DEFMACRO RCHARMAP-BP-AFTER ()
  '(COND ((= INDEX (LINE-LENGTH LINE))
	  (CREATE-BP (LINE-NEXT LINE) 0))
	 (T (CREATE-BP LINE (1+ INDEX)))))

(DEFMACRO RCHARMAP-DECREMENT RETURN-FORMS
  `(COND ((< (SETQ INDEX (1- INDEX)) *LAST-INDEX*)
	  (COND (*THIS-IS-THE-LAST-LINE*
		 (RETURN-FROM *RCHARMAP* . ,RETURN-FORMS))
		(T (SETQ LINE (LINE-PREVIOUS LINE)
			 *FIRST-LINE-P* NIL
			 INDEX (1- (LINE-LENGTH LINE))
			 *THIS-IS-THE-LAST-LINE* (EQ LINE *LAST-LINE*)
			 *LAST-INDEX* (IF *THIS-IS-THE-LAST-LINE* (BP-INDEX *TO-BP*) 0)))))))

(DEFMACRO RCHARMAP-LINE ()
  'LINE)

(DEFMACRO PUSH* (I R)
  `(OR (MEMQ ,I ,R) (PUSH ,I ,R)))

(DEFMACRO ORDER-BPS (BP1 BP2)
  `(COND ((BP-< ,BP2 ,BP1)
	  (PSETQ ,BP1 ,BP2 ,BP2 ,BP1))))

(DEFMACRO WORD-SYNTAX (CHAR)
  `(CHAR-SYNTAX ,CHAR *MODE-WORD-SYNTAX-TABLE*))

(DEFMACRO ATOM-WORD-SYNTAX (CHAR)
  `(CHAR-SYNTAX ,CHAR *ATOM-WORD-SYNTAX-TABLE*))

(DEFMACRO LIST-SYNTAX (CHAR)
  `(CHAR-SYNTAX ,CHAR *LIST-SYNTAX-TABLE*))

(DEFMACRO ATOM-WORD-SYNTAX-BIND (&BODY BODY)
  `(LET ((*MODE-WORD-SYNTAX-TABLE* *ATOM-WORD-SYNTAX-TABLE*))
     . ,BODY))

(DEFMACRO BP-CH-CHAR (BP)
  `(LDB %%CH-CHAR (BP-CHAR ,BP)))

(DEFMACRO PRESERVE-POINT (&BODY BODY)
  `(LET ((NLINES (1- (COUNT-LINES (INTERVAL-FIRST-BP *INTERVAL*) (POINT) T)))
	 (NCHARS (BP-INDEX (POINT))))
     (PROGN . ,BODY)
     (MOVE-BP (POINT)
	      (FORWARD-CHAR (FORWARD-LINE (INTERVAL-FIRST-BP *INTERVAL*)
					  NLINES T)
			    NCHARS T))))

(DEFMACRO PRESERVE-BUFFER-POINT ((BUFFER) &BODY BODY)
  `(LET ((BUFFER-P (TYPEP ,BUFFER 'BUFFER))
	 SAVED-POINT NLINES NCHARS)
     (IF (NOT BUFFER-P)
	 (SETQ SAVED-POINT (POINT))
	 (SETQ SAVED-POINT (BUFFER-SAVED-POINT ,BUFFER))
	 (AND (EQ ,BUFFER *INTERVAL*) (MOVE-BP SAVED-POINT (POINT))))
     (SETQ NLINES (COUNT-LINES (INTERVAL-FIRST-BP ,BUFFER) SAVED-POINT T)
	   NCHARS (BP-INDEX SAVED-POINT))
     (PROGN . ,BODY)
     (MOVE-BP SAVED-POINT
	      (LET ((*INTERVAL* ,BUFFER))	;Range check right
		(FORWARD-CHAR (FORWARD-LINE (INTERVAL-FIRST-BP ,BUFFER)
					    (1- NLINES) T)
			      NCHARS T)))
     (AND (EQ ,BUFFER *INTERVAL*) BUFFER-P (MOVE-BP (POINT) SAVED-POINT))))

(DEFMACRO DPRINT LIST
  (DO ((L LIST (CDR L))
       (RET NIL (CONS `(FORMAT T "~S = ~S; " ',(CAR L) ,(CAR L)) RET)))
      ((NULL L)
       `(PROGN (TERPRI) . ,(NREVERSE RET)))))

(DEFMACRO CURRENT-FONT (WINDOW &OPTIONAL (NUMBER '*FONT*))
  `(AREF (TV:SHEET-FONT-MAP (WINDOW-SHEET ,WINDOW)) ,NUMBER))

(DEFMACRO TYPEIN-LINE-ACTIVATE (&BODY BODY)
  `(LET ((*EDITOR-ALREADY-KNOWS* T))
     (TV:WINDOW-CALL (*TYPEIN-WINDOW*)
       . ,BODY)))

(DEFMACRO PROMPT-LINE-ACTIVATE (&BODY BODY)
  `(LET ((*EDITOR-ALREADY-KNOWS* T))
     (TV:WINDOW-CALL (*MODE-LINE-WINDOW*)
       . ,BODY)))

(DEFMACRO TEMPORARY-WINDOW-SELECT ((ZWEI-WINDOW) &BODY BODY)
  `(LET ((*EDITOR-ALREADY-KNOWS* T)
	 (.SHEET. (WINDOW-SHEET ,ZWEI-WINDOW)))
     (TV:WINDOW-CALL (.SHEET. :DEACTIVATE)
       . ,BODY)))

(DEFMACRO WITHOUT-IO-BUFFER-OUTPUT-FUNCTION (&BODY BODY)
  `(LOCAL-DECLARE ((SPECIAL TV:IO-BUFFER))
     (LET ()
       (BIND TV:(LOCF (IO-BUFFER-OUTPUT-FUNCTION IO-BUFFER)) NIL)
       . ,BODY)))

;;; Macros used to make command easy to write.
(DEFMACRO POINT ()
  '(WINDOW-POINT *WINDOW*))

(DEFMACRO MARK ()
  '(WINDOW-MARK *WINDOW*))

(DEFMACRO REGION ((BP1 BP2) &BODY BODY)
  `(LET ((,BP1 (POINT)) (,BP2 (MARK)))
     (COND ((NOT (WINDOW-MARK-P *WINDOW*))
	    (BARF "There is no region."))
	   ((BP-< ,BP2 ,BP1)
	    (PSETQ ,BP1 ,BP2 ,BP2 ,BP1)))
     . ,BODY))

(DEFMACRO REGION-LINES ((START-LINE STOP-LINE) &BODY BODY)
  `(LET ((REGION-LINES-BP1 (POINT)) (REGION-LINES-BP2 (MARK)))
     (COND ((NOT (WINDOW-MARK-P *WINDOW*))
	    (BARF "There is no region."))
	   ((BP-< REGION-LINES-BP2 REGION-LINES-BP1)
	    (PSETQ REGION-LINES-BP1 REGION-LINES-BP2
		   REGION-LINES-BP2 REGION-LINES-BP1)))
     (INTERVAL-LINES (REGION-LINES-BP1 REGION-LINES-BP2)
       (,START-LINE ,STOP-LINE)
       . ,BODY)))

(DEFMACRO INTERVAL-LINES ((BP1 BP2) (START-LINE STOP-LINE) &BODY BODY)
  `(LET ((,START-LINE
	  (COND ((ZEROP (BP-INDEX ,BP1))
		 (BP-LINE ,BP1))
		(T (LINE-NEXT (BP-LINE ,BP1)))))
	 (,STOP-LINE
	  (COND ((ZEROP (BP-INDEX ,BP2))
		 (BP-LINE ,BP2))
		(T (LINE-NEXT (BP-LINE ,BP2))))))
     . ,BODY))

(DEFMACRO TEMP-KILL-RING (THING &BODY BODY)
  `(LET ((*KILL-RING* (CONS ,THING *KILL-RING*)))
     . ,BODY))

;;; PRESERVE-POINT

(DEFMACRO WITH-BP ((VARIABLE BP TYPE) &BODY BODY)
  `(LET ((,VARIABLE (COPY-BP ,BP ,TYPE)))
     (UNWIND-PROTECT
       (PROGN . ,BODY)
       (FLUSH-BP ,VARIABLE))))

(DEFMACRO BIND-MODE-LINE (LIST &BODY BODY)
  `(LET ((*MODE-LINE-LIST* ',LIST))
     . ,BODY))

;;; This is for things that take an interval as an argument, or two bps, maybe in order,
;;; it canonicalises them into two ordered bps
(DEFMACRO GET-INTERVAL (START-BP END-BP IN-ORDER-P)
  `(COND ((NULL ,END-BP)
	  (SETQ ,END-BP (INTERVAL-LAST-BP ,START-BP)
		,START-BP (INTERVAL-FIRST-BP ,START-BP)))
	 ((NOT ,IN-ORDER-P)
	  (ORDER-BPS ,START-BP ,END-BP))))

;;; Bind off the read-only attribute of the specified interval temporarily.
(DEFMACRO WITH-READ-ONLY-SUPPRESSED ((INTERVAL) &BODY BODY)
  (ONCE-ONLY (INTERVAL)
    `(LET* ((.SAVED-TICK. (NODE-TICK ,INTERVAL))	;':READ-ONLY presumably
	    (.FILE-ID. (AND (TYPEP ,INTERVAL 'FILE-BUFFER)
			    (BUFFER-FILE-ID ,INTERVAL)))
	    (.SPECIAL-P. (AND .FILE-ID. (LISTP .FILE-ID.)
			      (EQ (FIRST .FILE-ID.) ':SPECIAL-BUFFER)))
	    (.OLD-TICK. (IF .SPECIAL-P. (THIRD .FILE-ID.) 0)))
       (UNWIND-PROTECT
	 (PROGN
	   (SETF (NODE-TICK ,INTERVAL) .OLD-TICK.)
	   . ,BODY)
	 (AND .SPECIAL-P. (SETF (THIRD .FILE-ID.) (NODE-TICK ,INTERVAL)))
	 (SETF (NODE-TICK ,INTERVAL) .SAVED-TICK.)))))

;;; This command reads from the mini-buffer with completion
(DEFMACRO WITH-MINI-BUFFER-COMPLETION ((MINI-BUFFER-WINDOW) &BODY BODY)
  (ONCE-ONLY (MINI-BUFFER-WINDOW)
    `(LET ((.SET-P. NIL))
       (UNWIND-PROTECT
	 (PROGN
	   (SETQ .SET-P. (TURN-ON-MINI-BUFFER-COMPLETION-BLINKER ,MINI-BUFFER-WINDOW T))
	   . ,BODY)
	 (AND .SET-P. (TURN-ON-MINI-BUFFER-COMPLETION-BLINKER ,MINI-BUFFER-WINDOW NIL))))))

;;; Defines a command.  Form is:
;;; (DEFCOM COM-foo "Documentation." OPTIONS-LIST . BODY)
;;; Note: unlike EINE, there is no lambda-list.
;;; Options are:  (KM) -- This command always preserves MARK.

(DEFMACRO DEFCOM (FN DOC OPTIONS &BODY DEF)
  `(PROGN 'COMPILE
     (COMMAND-DEFINE ',FN ',DOC ',OPTIONS)
     (DEFUN ,FN ()
       ,@(PROCESS-COMMAND-OPTIONS OPTIONS)
       . ,DEF)))

(DEFVAR *COMMAND-ALIST* NIL)		;Associates command names with commands.

(DEFUN COMMAND-DEFINE (COMMAND DOC IGNORE)
  (COND ((STRINGP DOC)
	 (PUTPROP COMMAND DOC 'DOCUMENTATION))
	((OR (SYMBOLP DOC)
	     (AND (NOT (ATOM DOC))
		  (MEMQ (CAR DOC) '(FUNCTION LAMBDA))))
	 (PUTPROP COMMAND DOC 'DOCUMENTATION-FUNCTION))
	(T
	 (FERROR NIL "The command ~S has invalid self-documentation ~S" COMMAND DOC)))
  (LET ((NAME (MAKE-COMMAND-NAME COMMAND)))
    (PUTPROP COMMAND NAME 'COMMAND-NAME)
    (OR (ASSOC NAME *COMMAND-ALIST*)
	(PUSH (CONS NAME COMMAND) *COMMAND-ALIST*))))

(DEFUN PROCESS-COMMAND-OPTIONS (OPTIONS)
  (DO ((L OPTIONS (CDR L))
       (RET NIL (APPEND (CDR (ASSQ (CAR L)
				  '((NM (SETF (WINDOW-MARK-P *WINDOW*) NIL))
				    (SM (SETF (WINDOW-MARK-P *WINDOW*) T)
					(SETQ *MARK-STAYS* T))
				    (KM (SETQ *MARK-STAYS* T))
				    (R (SETQ *CENTERING-FRACTION*
					     (IF (PLUSP *NUMERIC-ARG*)
						 *MIN-RESET-FRACTION*
						 *MAX-RESET-FRACTION*)))
				    (-R (SETQ *CENTERING-FRACTION*
					      (IF (PLUSP *NUMERIC-ARG*)
						  *MAX-RESET-FRACTION*
						  *MIN-RESET-FRACTION*)))
				    (PUSH (POINT-PDL-PUSH (POINT) *WINDOW*))
				    (OTHERWISE (FERROR NIL "Unknown DEFCOM option ~S"
						       (CAR L))))
				   )) RET)))
      ((NULL L) RET)))

;;; Convert a string into human-readable form.  Remove leading COM-, or leading
;;; and trailing *'s.  Conver hyphens into spaces, and capitalize each word.
;;; This is used both for command names and variable names.
(DEFUN MAKE-COMMAND-NAME (COMMAND)
  (SETQ COMMAND (STRING COMMAND))
  (LET ((CLEN (STRING-LENGTH COMMAND)))
    (LET ((STR (SUBSTRING COMMAND
			  (COND ((STRING-EQUAL "COM-MOUSE-" COMMAND 0 0 12 12) 12)
				((STRING-EQUAL "COM-" COMMAND 0 0 4 4) 4)
				((STRING-EQUAL "*" COMMAND 0 0 1 1) 1)
				(T 0))
			  (COND ((CHAR-EQUAL #/* (AREF COMMAND (1- CLEN))) (1- CLEN))
				(T CLEN)))))
       (PRETTY-COMMAND-NAME STR))))

(DEFUN PRETTY-COMMAND-NAME (STR)
  (DO ((I 0 (1+ I))
       (FLAG T)
       (CHAR)
       (LIM (STRING-LENGTH STR)))
      (( I LIM) STR)
    (SETQ CHAR (AREF STR I))
    (COND ((= CHAR #/-)
	   (ASET #\SP STR I)
	   (SETQ FLAG T))
	  (FLAG
	   (SETQ FLAG NIL))
	  ((AND ( CHAR #/A)
		( CHAR #/Z))
	   (ASET (+ 40 CHAR) STR I)))))

;;; A variable is a symbol, whose print name starts and ends with "*".
;;; The value of the variable is the value of the symbol.
;;; It has the following  properties:
;;; VARIABLE-NAME             The name, a string derived from the print-name of the symbol.
;;; VARIABLE-INIT             The initial value.
;;; VARIABLE-TYPE             One of the type symbols below.
;;; VARIABLE-DOCUMENTATION    A string documenting the variable.  The first line
;;;                             is the "short form.

;;; The value of *VARIABLE-ALIST* associates names of variables with their
;;; corresponding symbols.

;;; The following types are meaningful:
;;; :BOOLEAN    T or NIL.
;;; :KEYWORD    A symbol on the user package.
;;; :STRING     A string.
;;; :CHAR       A character as a fixnum.
;;; :CHAR-LIST  A list of characters as fixnums.
;;; :FIXNUM     A fixnum.
;;; :FIXNUM-OR-NIL     A fixnum or NIL.
;;; :SMALL-FRACTION A small flonum between 0.0s0 and 1.0s0, inclusively.
;;; :ANYTHING   Any Lisp object.

;;; Variables are defined by:
;;; (DEFVARIABLE <name> <init> <type> <documentation>)

(DEFMACRO DEFVARIABLE (VAR INIT TYPE DOC)
  `(PROGN 'COMPILE
     (DEFINE-VARIABLE ',VAR ,INIT ',TYPE ,DOC)
     (SPECIAL ,VAR)))

(DEFVAR *VARIABLE-ALIST* NIL)		;Associates variable names with variables.

(DEFUN DEFINE-VARIABLE (VAR INIT TYPE DOC)
  (CHECK-ARG TYPE (MEMQ TYPE '(:BOOLEAN :KEYWORD :STRING :FIXNUM-OR-NIL :SMALL-FRACTION
					:CHAR :CHAR-LIST :FIXNUM :ANYTHING))
	     "a valid ZWEI variable type")
  (LET ((NAME (MAKE-COMMAND-NAME VAR)))
    (PUTPROP VAR NAME 'VARIABLE-NAME)
    (OR (ASSOC NAME *VARIABLE-ALIST*)
	(PUSH (CONS NAME VAR) *VARIABLE-ALIST*)))
  (PUTPROP VAR INIT 'VARIABLE-INIT)
  (PUTPROP VAR TYPE 'VARIABLE-TYPE)
  (PUTPROP VAR DOC 'VARIABLE-DOCUMENTATION)
  (SI:RECORD-SOURCE-FILE-NAME VAR 'DEFVAR))

(DEFUN SETQ-ZWEI-VARIABLES ()
  (DO L *VARIABLE-ALIST* (CDR L) (NULL L)
    (LET ((V (CDAR L)))
      (SET V (GET V 'VARIABLE-INIT)))))

(SETQ *INITIAL-LISP-INDENT-OFFSET-ALIST*
      (NCONC *INITIAL-LISP-INDENT-OFFSET-ALIST*
	     '((LAMBDA 1 1) (*CATCH 1 1)
	       (LET 1 1) (LET* 1 1) (LET-IF 2 1) (IF 2 1)
	       (DO 2 1) (DO-NAMED 3 1) (RETURN-FROM 1 1)
	       (PROG . INDENT-PROG) (PROG* . INDENT-PROG)
	       (MULTIPLE-VALUE 1 1) (MULTIPLE-VALUE-BIND 1 3 2 1)
	       (DEFFLAVOR 1 7 3 1) (DEFPROP 0 0))))

(DEFVARIABLE *FILL-COLUMN* 576. :FIXNUM
   "Width in pixels used for filling text.")
(DEFVARIABLE *PARAGRAPH-DELIMITER-LIST* '(#/. #\SP #\TAB) :CHAR-LIST
   "Characters to be followed by two spaces.")
(DEFVARIABLE *PAGE-DELIMITER-LIST* '(#\FF) :CHAR-LIST
   "Characters which separate pages.")
(DEFVARIABLE *STICKY-MINOR-MODES* '(ATOM-WORD-MODE WORD-ABBREV-MODE EMACS-MODE) :ANYTHING
   "Minor modes to carry from current buffer to new ones.")
(DEFVARIABLE *UNSTICKY-MINOR-MODES* '(ELECTRIC-SHIFT-LOCK-MODE ELECTRIC-FONT-LOCK-MODE)
  :ANYTHING
   "Minor modes that are turned off when the mode is changed explicitly")
(DEFVARIABLE *INITIAL-MINOR-MODES* NIL :ANYTHING
   "Minor modes turned on in any major mode")
(DEFVARIABLE *DEFAULT-SAVE-MODE* ':ASK :KEYWORD
   "Default save mode for new buffers (NIL, :ASK, :ALWAYS).")
(DEFVARIABLE *FIND-FILE-SAVE-MODE* ':ASK :KEYWORD
   "Default save mode for new buffers create by Find File (NIL, :ASK, :ALWAYS).")
(DEFVARIABLE *DIRECTORY-LISTER* 'DEFAULT-DIRECTORY-LISTER :ANYTHING
   "Function used by Display Directory and auto directory display option.")
(DEFVARIABLE *DIRECTORY-SINGLE-FILE-LISTER* 'DEFAULT-LIST-ONE-FILE :ANYTHING
   "Function normally called to display each file")
(DEFVARIABLE *AUTO-PUSH-POINT-OPTION* 12 :FIXNUM-OR-NIL
   "Searches push point if it moves more than this many lines.")
(DEFVARIABLE *AUTO-PUSH-POINT-NOTIFICATION* "Point pushed" :STRING
   "This is typed in the echo area when point is automatically pushed.")
(DEFVARIABLE *AUTO-DIRECTORY-DISPLAY* NIL :KEYWORD
   "Tells on which kind of file commands to display directory (NIL, :READ, :WRITE, T).")
(DEFVARIABLE *TAB-BLINKER-FLAG* T :BOOLEAN
   "If a blinker is placed over a tab, make the blinker the width of a space.")
(DEFVARIABLE *FILL-PREFIX* "" :STRING
   "String to put before each line when filling.")
(DEFVARIABLE *FILL-EXTRA-SPACE-LIST* '(#/. #/! #/?) :CHAR-LIST
   "Characters that must be followed by two spaces.")
(DEFVARIABLE *FLASH-MATCHING-PAREN* T :BOOLEAN
   "When point is to the right of a close paren, flash the matching open paren.")
(DEFVARIABLE *COMMENT-START* NIL :STRING
   "String that indicates the start of a comment.")
(DEFVARIABLE *COMMENT-BEGIN* ";" :STRING
   "String for beginning new comments.")
(DEFVARIABLE *COMMENT-END* "" :STRING
   "String for ending comments.")
(DEFVARIABLE *COMMENT-COLUMN* (* 48. 8) FIXNUM
   "Column (in pixels) in which to start new comments.")
(DEFVARIABLE *CASE-REPLACE-P* T :BOOLEAN
   "Replacing commands try to preserve case.")
(DEFVARIABLE *PERMANENT-REAL-LINE-GOAL-XPOS* NIL :FIXNUM-OR-NIL
   "If non-NIL, goal for Up and Down Real Line commands.")
(DEFVARIABLE *SPACE-INDENT-FLAG* NIL :BOOLEAN
   "If true, Auto Fill mode will indent new lines.")
(DEFVARIABLE *POINT-PDL-MAX* 10 :FIXNUM
   "The maximum number of elements on the point PDL.
The point PDL is the push-down-list of saved places in the buffer
where the POINT has been.")
(DEFVARIABLE *KILL-RING-MAX* 10 :FIXNUM
   "The maximum number of elements on the kill ring.
The kill ring is the ring buffer of pieces of text saved by command
that delete text and brought back by commands that yank text.")
(DEFVARIABLE *SEARCH-RING-MAX* 3 :FIXNUM
   "The maximum number of elements on the search ring.
The search ring is the ring buffer of default search strings.")
(DEFVARIABLE *CENTER-FRACTION* 0.5s0 :SMALL-FRACTION
   "Where to recenter the window.
This is how far down in the window the point should be placed when ZWEI
recenters POINT in the window, as a fraction from 0.0 to 1.0.")
(DEFVARIABLE *MIN-RESET-FRACTION* 0.8s0 :SMALL-FRACTION
   "Where to recenter the window when you go off the bottom.
This is how far down in the window the point should be placed when ZWEI
moves the text in the window because you moved off the bottom.
It should be a fraction from 0.0 to 1.0.")
(DEFVARIABLE *MAX-RESET-FRACTION* 0.2s0 :SMALL-FRACTION
   "Where to recenter the window when you go off the top.
This is how far down in the window the point should be placed when ZWEI
moves the text in the window because you moved off the top.
It should be a fraction from 0.0 to 1.0.")
(DEFVARIABLE *BLANKS* '(#\SP #\TAB #\BS) :CHAR-LIST
   "List of characters that ZWEI thinks of as blanks.
The initial contents of this variable are the characters BLANK, TAB, and BACKSPACE.")
(DEFVARIABLE *WHITESPACE-CHARS* '(#\SP #\TAB #\CR #\BS) :CHAR-LIST
   "List of characters that ZWEI thinks of as blanks.
The initial contents of this variable are the characters BLANK, TAB, CR, and BACKSPACE.")
(DEFVARIABLE *REGION-MARKING-MODE* ':UNDERLINE :KEYWORD
   "How to mark the region.
This variable tells ZWEI how to denote the region between POINT and MARK.
It should be a symbol, either :UNDERLINE or :REVERSE-VIDEO.")
(DEFVARIABLE *DEFAULT-MAJOR-MODE* 'LISP-MODE :ANYTHING
   "The major mode in which new buffers are placed by default.")
(DEFVARIABLE *DEFAULT-PACKAGE* NIL :ANYTHING
   "The package new buffers are initially in.")
(DEFVARIABLE *LISP-INDENT-OFFSET* NIL :FIXNUM-OR-NIL
   "Same as QLisp Indent Offset in EMACS.  Good luck trying to use it. - DLW & MMcM")
(DEFVARIABLE *COMMENT-ROUND-FUNCTION* 'ROUND-FOR-COMMENT :ANYTHING
   "Function used to round up column when comments cannot be aligned to comment column.")
(DEFVARIABLE *LISP-DEFUN-INDENTATION* '(2 1) :ANYTHING
   "Amount to indent the second line of a defun.")
(DEFVARIABLE *LISP-INDENT-OFFSET-ALIST* *INITIAL-LISP-INDENT-OFFSET-ALIST* :ANYTHING
   "Describe this someday when all figured out.")
(DEFVARIABLE *FLASH-MATCHING-PAREN* T :BOOLEAN
   "Flash the ( that matches the ) we are to the right of.")
(DEFVARIABLE *LISP-INDENT-LONE-FUNCTION-OFFSET* 1 :FIXNUM
   "Amount to offset indentation of car of list.")
(DEFVARIABLE *FILE-VERSIONS-KEPT* 2 :FIXNUM
   "Number of non-superfluous versions of a file in Dired.")
(DEFVARIABLE *TEMP-FILE-TYPE-LIST* '("MEMO" "XGP" "@XGP" "UNFASL" "OUTPUT" "OLREC" "PRESS")
	     :ANYTHING
   "List of strings which are file types to be automatically 
marked for deletion in Dired.")
(DEFVARIABLE *TEXT-JUSTIFIER-ESCAPE-LIST* '(#/. #/@ #/- #/\ #/') :CHAR-LIST
   "List of characters that start text justifier commands when at the start of the line.")
(DEFVARIABLE *TEXT-JUSTIFIER-UNDERLINE-BEGIN* #/ :CHAR
   "Character to start an underlining.")
(DEFVARIABLE *TEXT-JUSTIFIER-UNDERLINE-END* #/ :CHAR
   "Character to end an underlining.")
(DEFVARIABLE *PL1-INDING-STYLE* 1 :FIXNUM
   "Pl1 indentation style.")
(DEFVARIABLE *ELECTRIC-SHIFT-LOCK-XORS* T :BOOLEAN
   "Shift key acts as xor of shift in electric shift-lock mode")
(DEFVARIABLE *KILL-INTERVAL-SMARTS* NIL :BOOLEAN
   "Kill and yank commands try to optimize whitespace")

	;:PACKAGE should be in here too except it causes lossage with
	;REVERT-BUFFER and REPARSE-MODE-LINE remprop'ing it when it didn't
	;come from the file mode line in the first place.  This needs redesigning.
(DEFVARIABLE *MODE-LINE-PROPERTIES* '(:MODE :LOWERCASE :FONTS :BACKSPACE :TAB-WIDTH) :ANYTHING
   "Known properties in the -*- line")
