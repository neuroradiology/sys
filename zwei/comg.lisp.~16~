;;; Zwei commands, see ZWEI;COMA for comments -*-Mode:LISP; Package:ZWEI-*-
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **

(DEFCOM COM-INSTALL-COMMAND "Install a specified function on a specified key.
The name of the function is read from the mini-buffer (the top of the kill ring
contains the name of the current defun), and a character from the echo area.
If the key is currently holding a command prefix (like Control-X), it will ask
you for another character, so that you can redefine Control-X commands.  However,
with a numeric argument, it will assume you want to redefine Control-X itself,
and will not ask for another character." ()
    (DO (NAME) (NIL)
      (SETQ NAME (READ-FUNCTION-NAME "Name of function to install"
				     (RELEVANT-FUNCTION-NAME (POINT)) NIL 'ALWAYS-READ))
      (AND (OR (FBOUNDP NAME)
	       (FQUERY '(:SELECT T) "~A is not defined, ok to install anyway? " NAME))
	   (RETURN (INSTALL-COMMAND-INTERNAL NAME)))))

(DEFCOM COM-INSTALL-MACRO "Install a specified user macro on a specifed key.
The macro should be a /"permanent/" macro, that has a name.
The name of the macro is read from the mini-buffer, and the keystroke on which
to install it is read in the echo area.
If the key is currently holding a command prefix (like Control-X), it will ask
you for another character, so that you can redefine Control-X commands.  However,
with a numeric argument, it will assume you want to redefine Control-X itself,
and will not ask for another character." ()
  (COM-INSTALL-MACRO-INTERNAL NIL))

(DEFCOM COM-INSTALL-MOUSE-MACRO
	"Like Install Macro, but moves the mouse to where clicked first." ()
  (COM-INSTALL-MACRO-INTERNAL T))

(DEFCOM COM-DEINSTALL-MACRO "Deinstall a keyboard macro" ()
  (INSTALL-COMMAND-INTERNAL NIL NIL T))

(DEFUN COM-INSTALL-MACRO-INTERNAL (MOUSE-P)
  (OR (MEMQ ':MACRO-PREVIOUS-ARRAY (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream does not support macros"))
  (LET ((PACKAGE SI:PKG-USER-PACKAGE)
	NAME MAC)
    (SETQ NAME (TYPEIN-LINE-READ "Name of macro to install (CR for last macro defined):"))
    (COND ((EQ NAME '*EOF*)
	   (SETQ MAC (FUNCALL STANDARD-INPUT ':MACRO-PREVIOUS-ARRAY)
		 NAME (GENSYM))
	   (PUTPROP NAME MAC 'MACRO-STREAM-MACRO))
	  ((NOT (SETQ MAC (GET NAME 'MACRO-STREAM-MACRO)))
	   (BARF "~A is not a defined macro." NAME)))
    (INSTALL-COMMAND-INTERNAL (MAKE-MACRO-COMMAND NAME MOUSE-P) T)))

(DEFUN INSTALL-COMMAND-INTERNAL (COMMAND &OPTIONAL REMEMBER-OLD-P DEINSTALL-P
				 &AUX KEY-LIST)
  (PROMPT-LINE (IF DEINSTALL-P "Key to deinstall:" "Key to get it:"))
  (PROMPT-LINE-ACTIVATE
    (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
      (DO ((COMTAB *COMTAB*)
	   (KEY (FUNCALL STANDARD-INPUT ':MOUSE-OR-KBD-TYI)
		(FUNCALL STANDARD-INPUT ':TYI)))
	  (NIL)
	(PUSH KEY KEY-LIST)
	(PROMPT-LINE-MORE " ~:@C" KEY)
	(LET ((OLD-COMMAND (COMMAND-LOOKUP KEY COMTAB)))
	  (COND ((AND (PREFIX-COMMAND-P OLD-COMMAND)
		      (NOT *NUMERIC-ARG-P*))
		 (SETQ COMTAB (SYMEVAL-IN-CLOSURE OLD-COMMAND 'COMTAB)))
		(T (AND DEINSTALL-P
			(SETQ COMMAND (MOUSE-MACRO-COMMAND-LAST-COMMAND
					(COMMAND-LOOKUP KEY COMTAB))))
		   (AND REMEMBER-OLD-P
			(SET-MOUSE-MACRO-COMMAND-LAST-COMMAND COMMAND
							      (COMMAND-LOOKUP KEY COMTAB)))
		   (COMMAND-STORE COMMAND KEY COMTAB)
		   (RETURN NIL)))))))
  (TYPEIN-LINE "Command ~S installed on" COMMAND)
  (DOLIST (KEY (NREVERSE KEY-LIST))
    (TYPEIN-LINE-MORE " ~:@C" KEY))
  (TYPEIN-LINE-MORE ".")
  DIS-NONE)

;;;Multics EMACS compatible macro commands
(DEFCOM COM-START-KBD-MACRO "Begin defining a keyboard macro.
A numeric argument means append to the prevous keyboard macro." ()
  (OR (MEMQ ':MACRO-PUSH (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream doesnt support macros"))
  (FUNCALL STANDARD-INPUT ':MACRO-PUSH (+ 2 *NUMERIC-ARG-N-DIGITS*)
	   (AND *NUMERIC-ARG-P* (FUNCALL STANDARD-INPUT ':MACRO-PREVIOUS-ARRAY)))
  DIS-NONE)

(DEFCOM COM-END-KBD-MACRO "Terminate the definition of a keyboard macro" ()
  (OR (MEMQ ':MACRO-POP (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream doesnt support macros"))
  (*CATCH 'MACRO-LOOP				;In case no macro running
     (FUNCALL STANDARD-INPUT ':MACRO-POP (+ 2 *NUMERIC-ARG-N-DIGITS*)
	                                 (AND (NOT (ZEROP *NUMERIC-ARG*)) *NUMERIC-ARG*)))
  DIS-NONE)

(DEFCOM COM-CALL-LAST-KBD-MACRO "Repeat the last keyboard macro" ()
  (OR (MEMQ ':MACRO-EXECUTE (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream doesnt support macros"))
  (FUNCALL STANDARD-INPUT ':MACRO-EXECUTE NIL (AND (NOT (ZEROP *NUMERIC-ARG*)) *NUMERIC-ARG*))
  DIS-NONE)

(DEFCOM COM-KBD-MACRO-QUERY "Interactive keyboard macro" ()
  (OR (MEMQ ':MACRO-QUERY (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream doesnt support macros"))
  (FUNCALL STANDARD-INPUT ':MACRO-QUERY (+ 2 *NUMERIC-ARG-N-DIGITS*))
  DIS-NONE)

(DEFCOM COM-VIEW-KBD-MACRO "Typeout the specified keyboard macro.
The macro should be a /"permanent/" macro, that has a name.
The name of the macro is read from the mini-buffer, just cr means the last
one defined, which can also be temporary." ()
  (OR (MEMQ ':MACRO-PREVIOUS-ARRAY (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream does not support macros"))
  (LET ((PACKAGE SI:PKG-USER-PACKAGE)
	NAME MAC)
    (SETQ NAME (TYPEIN-LINE-READ "Name of macro to view (CR for last macro defined):"))
    (COND ((EQ NAME '*EOF*)
	   (SETQ MAC (FUNCALL STANDARD-INPUT ':MACRO-PREVIOUS-ARRAY)))
	  ((NOT (SETQ MAC (GET NAME 'MACRO-STREAM-MACRO)))
	   (BARF "~A is not a defined macro." NAME)))
    (DO ((I 0 (1+ I))
	 (LEN (MACRO-LENGTH MAC))
	 (CH))
	((> I LEN))
      (FORMAT T (SELECTQ (SETQ CH (AREF MAC I))
		  (*MOUSE* "Mouse command ~*")
		  (*SPACE* "Macro query ~*")
		  (*RUN* "Repeat ~*")
		  (NIL "Input ~*")
		  (OTHERWISE "~:C "))
	      CH)))
  DIS-NONE)

(DEFCOM COM-NAME-LAST-KBD-MACRO "Make the last temporary macro permanent.
The new name for the macro is read from the mini-buffer." ()
  (OR (MEMQ ':MACRO-PREVIOUS-ARRAY (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream does not support macros"))
  (LET* ((MAC (OR (FUNCALL STANDARD-INPUT ':MACRO-PREVIOUS-ARRAY)
		  (BARF "There is no previous keyboard macro")))
	 (PACKAGE SI:PKG-USER-PACKAGE)
	 (NAME (TYPEIN-LINE-READ "Name for macro:")))
    (PUTPROP NAME MAC 'MACRO-STREAM-MACRO))
  DIS-NONE)

;;; Sorting commands
(DEFCOM COM-SORT-LINES "Sort the region alphabetically by lines" ()
  (REGION (BP1 BP2)
    (SORT-LINES-INTERVAL #'STRING-LESSP BP1 BP2 T))
  DIS-TEXT)

(DEFCOM COM-SORT-PARAGRAPHS "Sort the region alphabetically by paragraphs" ()
  (REGION (BP1 BP2)
    (SORT-INTERVAL-FUNCTIONS #'FORWARD-OVER-BLANK-OR-TEXT-JUSTIFIER-LINES
			     #'(LAMBDA (BP) (FORWARD-PARAGRAPH BP 1 T))
			     #'(LAMBDA (BP) BP)
			     #'INTERVAL-WITH-SORT-INTERVAL-LESSP
			     BP1 BP2 T))
  DIS-TEXT)

(DEFVAR *MAKE-KBD-MACRO-MOVER-COMTAB*)

;;; This returns a function which takes a BP and returns a resultant BP after performing
;;; the given kbd-macro operation.
(DEFUN MAKE-KBD-MACRO-MOVER (PROMPT)
  (COM-START-KBD-MACRO)
  (TYPEIN-LINE "Defining a keyboard macro to ~A~@[; type ~A to finish it~]"
	       PROMPT (KEY-FOR-COMMAND 'COM-END-KBD-MACRO))
  (LET ((STANDARD-INPUT (LET-CLOSED ((OLD-STANDARD-INPUT STANDARD-INPUT))
			  #'(LAMBDA (OP &REST REST)
			      (PROG1 (LEXPR-FUNCALL OLD-STANDARD-INPUT OP REST)
				     (COND ;;When done recording, exit the recursive edit.
					   ((EQ OP ':MACRO-POP)
					    (*THROW 'EXIT-MAKE-KBD-MACRO-MOVER T))
					   ;;If there is an error, exit, it will throw up
					   ;;further.
					   ((EQ OP ':MACRO-ERROR)
					    (*THROW 'EXIT-MAKE-KBD-MACRO-MOVER
						    ':MACRO-ERROR))))))))
    (AND (EQ (*CATCH 'EXIT-MAKE-KBD-MACRO-MOVER
	       (FUNCALL-SELF ':EDIT))
	     ':MACRO-ERROR)
	 (*THROW 'ZWEI-COMMAND-LOOP T)))
  (COND ((NOT (BOUNDP '*MAKE-KBD-MACRO-MOVER-COMTAB*))
	 (SETQ *MAKE-KBD-MACRO-MOVER-COMTAB* (CREATE-SPARSE-COMTAB))
	 (SETF (COMTAB-KEYBOARD-ARRAY *MAKE-KBD-MACRO-MOVER-COMTAB*)
	       '((-1 . COM-EXIT-KBD-MACRO-MOVER)))))
  (SET-COMTAB-INDIRECTION *MAKE-KBD-MACRO-MOVER-COMTAB* *COMTAB*)
  (LET-CLOSED ((OLD-MACRO-PREVIOUS-ARRAY (FUNCALL STANDARD-INPUT ':MACRO-PREVIOUS-ARRAY))
	       (STANDARD-INPUT (LET-CLOSED ((OLD-STANDARD-INPUT STANDARD-INPUT))
				 #'(LAMBDA (OP &REST REST)
				     (PROG1 (LEXPR-FUNCALL OLD-STANDARD-INPUT OP REST)
					    (COND ((EQ OP ':MACRO-ERROR)
						   (*THROW 'EXIT-KBD-MACRO-MOVER
							   ':MACRO-ERROR))))))))
    (ARRAY-PUSH-EXTEND OLD-MACRO-PREVIOUS-ARRAY -1)
    (SETF (MACRO-LENGTH OLD-MACRO-PREVIOUS-ARRAY)
	  (1- (MACRO-POSITION OLD-MACRO-PREVIOUS-ARRAY)))
    #'(LAMBDA (BP &AUX (POINT (POINT)) OLD-POINT)
	(SETQ OLD-POINT (COPY-BP POINT ':NORMAL))
	(MOVE-BP (POINT) BP)
	(UNWIND-PROTECT
	  (LET ((*COMTAB* *MAKE-KBD-MACRO-MOVER-COMTAB*))
	    (FUNCALL STANDARD-INPUT ':MACRO-EXECUTE OLD-MACRO-PREVIOUS-ARRAY 1)
	    (AND (EQ (*CATCH 'EXIT-KBD-MACRO-MOVER
		       (FUNCALL-SELF ':EDIT))
		     ':MACRO-ERROR)
		 (*THROW 'ZWEI-COMMAND-LOOP T))
	    (COPY-BP POINT))
	  (MOVE-BP (POINT) OLD-POINT)
	  (FLUSH-BP OLD-POINT)))))

(DEFUN COM-EXIT-KBD-MACRO-MOVER ()
  (*THROW 'EXIT-KBD-MACRO-MOVER T))

(DEFCOM COM-SORT-VIA-KEYBOARD-MACROS "Sort the region alphabetically.
Keyboard macros are read to move to the various part of the region to be sorted." ()
  (REGION (BP1 BP2)
    (WITH-BP (FIRST-BP BP1 ':NORMAL)
      (WITH-BP (LAST-BP BP2 ':MOVES)
	(SETF (WINDOW-MARK-P *WINDOW*) NIL)
	(MOVE-BP (POINT) FIRST-BP)
	(MUST-REDISPLAY *WINDOW* DIS-BPS)
	(LET ((MOVE-TO-KEY-MACRO (MAKE-KBD-MACRO-MOVER "move to the start of the sort key"))
	      (MOVE-OVER-KEY-MACRO (MAKE-KBD-MACRO-MOVER "move over the sort key"))
	      (MOVE-TO-NEXT-MACRO (MAKE-KBD-MACRO-MOVER "move to the end of the record")))
	  (SORT-INTERVAL-FUNCTIONS MOVE-TO-KEY-MACRO MOVE-OVER-KEY-MACRO MOVE-TO-NEXT-MACRO
				   #'INTERVAL-WITH-SORT-INTERVAL-LESSP FIRST-BP LAST-BP T)))))
  DIS-TEXT)

(DEFCOM COM-DESCRIBE-CLASS "Describe the specified class." ()
  (LET ((CLASS (COMPLETING-READ-FROM-MINI-BUFFER
		 "Describe class:"
		 (MAPCAR #'(LAMBDA (X)
			     (SETQ X (<- X ':CLASS-SYMBOL))
			     (CONS (FORMAT NIL "~S" X) X))
			 (CONS OBJECT-CLASS (SI:ALL-SUBCLASSES-OF-CLASS OBJECT-CLASS)))
		 NIL NIL "You are typing the name of a class, to be described.")))
    (AND (ATOM CLASS) (BARF))
    (DESCRIBE-CLASS-INTERNAL (CDR CLASS)))
  DIS-NONE)

(DEFUN DESCRIBE-CLASS-INTERNAL (CLASS)
  (OR (AND (SYMBOLP CLASS) (BOUNDP CLASS)
	   (ENTITYP (SETQ CLASS (SYMEVAL CLASS))))
      (BARF "~S is not a class" CLASS))
  (FORMAT *TYPEOUT-WINDOW* "~&Instance variables of ~A:~%"
	  (SYMEVAL-IN-CLOSURE CLASS ':NAME))
  (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST NIL (SYMEVAL-IN-CLOSURE CLASS 'SI:INSTANCE-PATTERN))
  (DO ((SYM (SYMEVAL-IN-CLOSURE CLASS 'SI:CLASS-METHOD-SYMBOL))
       (METHS NIL)
       (CL)
       (ML))
      ((EQ SYM 'SI:UNCLAIMED-MESSAGE))
    (SETQ CL (SYMEVAL SYM)
	  ML (%MAKE-POINTER DTP-LIST (FSYMEVAL SYM)))
    (FORMAT *TYPEOUT-WINDOW* "~2%Methods~:[ as a subclass~] of ~A:~%" (EQ CL CLASS)
	    (SYMEVAL-IN-CLOSURE CL ':NAME))
    (DO ((L ML (CDR L))
	 (M)
	 (LL NIL))
	((NLISTP L)
	 (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FUNCTION-NAME (NREVERSE LL))
	 (SETQ SYM L
	       METHS (APPEND ML METHS))
	 (RPLACD (LAST METHS) NIL))
      (OR (ASSQ (CAR (SETQ M (CAR L))) METHS)
	  (PUSH M LL))))
  NIL)

(DEFUN READ-FLAVOR-NAME (PROMPT HELP)
  (SORT-COMPLETION-AARRAY SI:*ALL-FLAVOR-NAMES-AARRAY*)
  (LET ((FLAVOR (COMPLETING-READ-FROM-MINI-BUFFER PROMPT SI:*ALL-FLAVOR-NAMES-AARRAY*
						  NIL NIL HELP)))
    (AND (ATOM FLAVOR) (BARF))
    (CDR FLAVOR)))

(DEFCOM COM-DESCRIBE-FLAVOR "Describe the specified flavor." ()
  (DESCRIBE-FLAVOR-INTERNAL
    (READ-FLAVOR-NAME "Describe flavor:"
		      "You are typing the name of a flavor, to be described."))
  DIS-NONE)

(DEFUN DESCRIBE-FLAVOR-INTERNAL (FLAVOR &AUX FL TEM)
  (OR (SETQ FL (GET FLAVOR 'SI:FLAVOR))
      (BARF "~S is not the name of a flavor" FLAVOR))
  (FORMAT *TYPEOUT-WINDOW* "~&Flavor ")
  (FUNCALL *TYPEOUT-WINDOW* ':ITEM 'FLAVOR-NAME FLAVOR)
  (COND ((SETQ TEM (SI:FLAVOR-DEPENDS-ON FL))
	 (FORMAT *TYPEOUT-WINDOW* " directly depends on flavor~P:~%" (LENGTH TEM))
	 (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FLAVOR-NAME TEM))
	(T
	 (FORMAT *TYPEOUT-WINDOW* " does not directly depend on any other flavors~%")))
  (COND ((SETQ TEM (SI:FLAVOR-INCLUDES FL))
	 (FORMAT *TYPEOUT-WINDOW* "~& and directly includes flavor~P:~%"
		 (LENGTH TEM))
	 (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FLAVOR-NAME TEM)))
  (COND ((SETQ TEM (SI:FLAVOR-DEPENDED-ON-BY FL))
	 (FORMAT *TYPEOUT-WINDOW* "~& and is directly depended on by flavor~P:~%"
		 (LENGTH TEM))
	 (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FLAVOR-NAME TEM)))
  (LOCAL-DECLARE ((SPECIAL LIV))		;For the REM-IF below
    (LET ((LIV (SI:FLAVOR-LOCAL-INSTANCE-VARIABLES FL)))
      (IF (NULL LIV)
	  (FORMAT *TYPEOUT-WINDOW* "~&~S has no local instance variables~%" FLAVOR)
	  (FORMAT *TYPEOUT-WINDOW* "~&Instance variable~P of ~S: "
		  (LENGTH LIV) FLAVOR )
	  (FORMAT:PRINT-LIST *TYPEOUT-WINDOW* "~S"
			     (LOOP FOR V IN LIV COLLECT (IF (ATOM V) V (CAR V))))
	  (TERPRI *TYPEOUT-WINDOW*))
      (COND ((SETQ TEM (SI:FLAVOR-INSTANCE-SIZE FL))
	     (FORMAT *TYPEOUT-WINDOW* "Flavor ~S has instance size ~D,
 with inherited instance variables: "
		     FLAVOR TEM)
	     (FORMAT:PRINT-LIST *TYPEOUT-WINDOW* "~S"
				(REM-IF #'(LAMBDA (X) (MEMQ X LIV))
					(SI:FLAVOR-ALL-INSTANCE-VARIABLES FL)))
	     (TERPRI *TYPEOUT-WINDOW*)))))
  (SI:MAP-OVER-COMPONENT-FLAVORS 0 T NIL #'DESCRIBE-FLAVOR-1 FLAVOR (LIST NIL NIL) FLAVOR)
  (DO ((PLIST (SI:FLAVOR-PLIST FL) (CDDR PLIST))
       (FLAG NIL))
      ((NULL PLIST))
    (COND ((NOT (MEMQ (CAR PLIST) '(:DEFAULT-INIT-PLIST
				     :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)))
	   (COND ((NOT FLAG)
		  (FORMAT *TYPEOUT-WINDOW* "Random properties:~%")
		  (SETQ FLAG T)))
	   (FORMAT *TYPEOUT-WINDOW* "~5X~S:	~S~%" (CAR PLIST) (CADR PLIST)))))
  NIL)

(DEFUN DESCRIBE-FLAVOR-1 (FL STATE TOP-FLAVOR-NAME &AUX (FLAVOR-FLAG NIL))
  (COND ((NOT (MEMQ FL (SECOND STATE)))
	 (DO ((MTES (SI:FLAVOR-METHOD-TABLE FL) (CDR MTES))
	      (MTE)
	      (ELEM)
	      (MSG)
	      (MSG-FLAG NIL NIL)
	      (TEM))
	     ((NULL MTES))
	   (SETQ MTE (CAR MTES) MSG (FIRST MTE) MTE (CDDDR MTE))
	   (OR (SETQ ELEM (ASSQ MSG (FIRST STATE)))
	       (PUSH (SETQ ELEM (LIST MSG NIL NIL)) (FIRST STATE)))
	   (COND ((AND (SETQ TEM (SI:METH-LOOKUP ':BEFORE MTE))
		       (NOT (MEMQ TEM (SECOND ELEM))))
		  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
		    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (SI:METH-FUNCTION-SPEC TEM) "before"
					       MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
		  (PUSH TEM (SECOND ELEM))))
	   ;; This assumes the combination type is daemon.  Hard to check at this level.
	   (COND ((AND (SETQ TEM (SI:METH-LOOKUP 'NIL MTE)) (NULL (THIRD ELEM)))
		  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
		    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (SI:METH-FUNCTION-SPEC TEM) "primary"
					       MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
		  (SETF (THIRD ELEM) TEM)))
	   (COND ((AND (SETQ TEM (SI:METH-LOOKUP ':AFTER MTE)) (NOT (MEMQ TEM (SECOND ELEM))))
		  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
		    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (SI:METH-FUNCTION-SPEC TEM) "after"
					       MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
		  (PUSH TEM (SECOND ELEM))))
	   (COND ((AND (SETQ TEM (SI:METH-LOOKUP ':WRAPPER MTE))
		       (NOT (MEMQ TEM (SECOND ELEM))))
		  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
		    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (SI:METH-FUNCTION-SPEC TEM) "wrapper"
					       MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
		  (PUSH TEM (SECOND ELEM))))
	   ;In case there are any other method types
	   (LOOP FOR TEM IN MTE
		 UNLESS (MEMQ (SI:METH-METHOD-TYPE TEM)
			      '(:BEFORE NIL :AFTER :WRAPPER :COMBINED))
		   UNLESS (MEMQ TEM (SECOND ELEM))
		     DO (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
			  (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (SI:METH-FUNCTION-SPEC TEM)
						     (STRING (SI:METH-METHOD-TYPE TEM))
						     MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
			(PUSH TEM (SECOND ELEM)))
	   (AND MSG-FLAG (TERPRI *TYPEOUT-WINDOW*)))
	 (SETQ FLAVOR-FLAG (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
			     FL (SI:FLAVOR-GETTABLE-INSTANCE-VARIABLES FL)
			     "automatically-generated methods to get instance variable" ""
			     FLAVOR-FLAG TOP-FLAVOR-NAME))
	 (SETQ FLAVOR-FLAG (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
			     FL (SI:FLAVOR-SETTABLE-INSTANCE-VARIABLES FL)
			     "automatically-generated methods to set instance variable" ""
			     FLAVOR-FLAG TOP-FLAVOR-NAME))
	 (SETQ FLAVOR-FLAG (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
			     FL (MAPCAR #'CDR (SI:FLAVOR-INITABLE-INSTANCE-VARIABLES FL))
			     "instance variable" " that may be set by initialization"
			     FLAVOR-FLAG TOP-FLAVOR-NAME))
	 (SETQ FLAVOR-FLAG (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
			     FL (SI:FLAVOR-INIT-KEYWORDS FL)
			     "keyword" " in the :INIT message"
			     FLAVOR-FLAG TOP-FLAVOR-NAME))
	 (SETQ FLAVOR-FLAG (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
			     FL (GET (LOCF (SI:FLAVOR-PLIST FL))
				     ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
			     "macros to access variable" ""
			     FLAVOR-FLAG TOP-FLAVOR-NAME))
	 (LET ((DEFAULT-PLIST (GET (LOCF (SI:FLAVOR-PLIST FL)) ':DEFAULT-INIT-PLIST)))
	   (COND (DEFAULT-PLIST
		  (DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME FL FLAVOR-FLAG TOP-FLAVOR-NAME)
		  (FORMAT *TYPEOUT-WINDOW* " Plus default init plist: ")
		  (DO ((L DEFAULT-PLIST (CDDR L))
		       (FLAG T NIL))
		      ((NULL L))
		    (FORMAT *TYPEOUT-WINDOW* "~:[, ~]~S ~S" FLAG (CAR L) (CADR L)))
		  (TERPRI *TYPEOUT-WINDOW*))))
	 (PUSH FL (SECOND STATE))))
  STATE)

(DEFUN DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME (FL FLAG TOP-FLAVOR-NAME &AUX FLAVOR-NAME)
  (COND ((NOT FLAG)				;If not already printed
	 (SETQ FLAVOR-NAME (SI:FLAVOR-NAME FL))
	 (FORMAT *TYPEOUT-WINDOW* "Method(s) ~:[inherited from~;of~] ~S:~%"
		 (EQ FLAVOR-NAME TOP-FLAVOR-NAME) FLAVOR-NAME)))
  T)						;New value of flag

(DEFUN DESCRIBE-FLAVOR-PRINT-MSG (FL MSG FUNCTION TYPE MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME)
  (DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME FL FLAVOR-FLAG TOP-FLAVOR-NAME)
  (OR MSG-FLAG (FORMAT *TYPEOUT-WINDOW* "   :~A " MSG))
  (FUNCALL *TYPEOUT-WINDOW* ':ITEM 'FUNCTION-NAME FUNCTION TYPE)
  (FUNCALL *TYPEOUT-WINDOW* ':TYO #\SP)
  (VALUES T T))				;New values for the flags

(DEFUN DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST (FL LIST STR1 STR2 FLAG TOP-FLAVOR-NAME)
  (COND (LIST					;If there is something there
	 (DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME FL FLAG TOP-FLAVOR-NAME)
	 (FORMAT *TYPEOUT-WINDOW* " Plus ~A~P~A: ~{~<~%  ~2:;~:S~>~^, ~}~%"
				  STR1 (LENGTH LIST) STR2 LIST)
	 T)))					;New value of the flag

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FLAVOR-NAME "Edit" EDIT-DEFINITION T
			  "Edit the definition of this flavor.")


(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FLAVOR-NAME "Describe"
			  DESCRIBE-FLAVOR-INTERNAL NIL
			  "Describe this flavor.")
  