;;; Zwei commands, see ZWEI;COMA for comments -*-Mode:LISP; Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFCOM COM-FROB-LISP-CONDITIONAL "Change CONDs to ANDs or ORs and vice versa.
When changing to COND, point is left in such a place that LF will add another
clause to this condition, and M-) will add another condition.  Also in this case
an argument specifies the number of clauses that are left in the
consequent, the default is 1, i.e. all clauses but the last are assumed to
be for value, and to belong in the antecedent." ()
  (ATOM-WORD-SYNTAX-BIND
    (LET ((POINT (POINT))
	  FIXBP1 FIXBP2)
     (UNWIND-PROTECT
      (LET (COND-BP COND-TYPE UPCASE-P BP)
        (MULTIPLE-VALUE (COND-BP COND-TYPE)
	  (FIND-CONTAINING-ATOM POINT '(COND AND OR IF)))	;Locate the COND or AND or OR
        (OR COND-BP (BARF))
        (SETQ UPCASE-P (CHAR-UPPERCASE-P (BP-CHAR COND-BP)))    ;Remember if have to lowercase
        (LET ((START-DEFUN-BP (FORWARD-DEFUN POINT -1 T))
              (END-DEFUN-BP (FORWARD-DEFUN POINT 1 T))
              DEPTH)
	  ;; Parse it all once, then don't even bother checking.
	  (LISP-PARSE-FROM-DEFUN (BP-LINE END-DEFUN-BP) START-DEFUN-BP)
	  ;; Count how many levels down the next defun is from the start of this one.
	  (LET ((*LISP-PARSE-PREPARSED-FLAG* T))
	    (DO ((I -1 (1+ I))
		 (BP3 END-DEFUN-BP (FORWARD-SEXP BP3 -1 NIL 1 START-DEFUN-BP)))
		((NULL BP3) (SETQ DEPTH I))))
	  ;; Insert that many ")"'s just before point, so everything is balanced.
	  ;; These ")"'s lie between FIXBP1 and FIXBP2.  We use that to delete them later.
          (COND ((> DEPTH 0)
                 (LET ((BP (LIKELY-UNBALANCED-POINT (FORWARD-LIST COND-BP -1 NIL 1)
						    END-DEFUN-BP)))
		   (SETQ FIXBP1 (COPY-BP BP ':NORMAL)
			 FIXBP2 (COPY-BP BP ':MOVES)))
		 (INSERT FIXBP2 #\CR)
                 (DOTIMES (I DEPTH) (INSERT FIXBP2 #/)))
                 (INSERT FIXBP2 #\CR))))
        (COND ((EQ COND-TYPE 'COND)             ;Changing COND to AND or OR
               (LET ((N (COUNT-LIST-ELEMENTS (FORWARD-LIST COND-BP -1 NIL 1))))
		 (AND (> N 3) (BARF "Too many clauses"))
		 (AND (= N 3)
		      (LET ((BP1 (FORWARD-SEXP COND-BP 2)) BP2 BP3)
			(SETQ BP2 (FORWARD-LIST BP1 1 NIL -1 T)
			      BP3 (FORWARD-WORD BP2))
			(OR (AND (EQ (BP-LINE BP2) (BP-LINE BP3))
				 (STRING-EQUAL (BP-LINE BP2) "T" (BP-INDEX BP2) 0
					       (BP-INDEX BP3)))
			    (BARF "Too many clauses"))
			(SETQ BP1 (BACKWARD-OVER '(#\CR #\TAB #\SP) BP1))
			(SETQ BP1 (FORWARD-CHAR BP1 -1))
			(SETQ N (COUNT-LIST-ELEMENTS (FORWARD-SEXP COND-BP)))
			(DELETE-INTERVAL BP1 BP3 T)
			(SETQ COND-TYPE (IF (= N 1) "OR" "IF")))))
	       (DELETE-INTERVAL COND-BP (FORWARD-WORD COND-BP) T)
	       (AND (EQ COND-TYPE 'COND)	;Still not determined
		    ;; Check for (COND ((NOT ...)))
		    (LET ((BP1 (FORWARD-LIST COND-BP 1 NIL -2 T)))
		      (LET ((BP2 (FORWARD-WORD COND-BP 1 T)))
			(LET ((WORD (STRING-INTERVAL BP1 BP2)))
			  (COND ((OR (STRING-EQUAL WORD "NULL") (STRING-EQUAL WORD "NOT"))
				 (SETQ BP1 (FORWARD-LIST BP1 -1 NIL 1))
				 (LET ((BP3 (FORWARD-LIST BP1)))
				   (DELETE-INTERVAL (FORWARD-CHAR BP3 -1) BP3 T))
				 (DELETE-INTERVAL BP1 (FORWARD-OVER *BLANKS* BP2) T)
				 (SETQ COND-TYPE "OR"))
				(T
				 (SETQ COND-TYPE "AND")))))))
	       (SETQ BP (FORWARD-OVER *BLANKS* (INSERT COND-BP COND-TYPE)))
	       (LET ((BP1 (FORWARD-LIST BP)))	;Remove a level of parens
		 (DELETE-INTERVAL (FORWARD-CHAR BP1 -1) BP1 T))
	       (DELETE-INTERVAL BP (FORWARD-CHAR BP) T))
	      (T
	       (LET ((BP1 (FORWARD-LIST (FORWARD-LIST (FORWARD-CHAR COND-BP -1))
					-1 NIL -1 T)))
		 (INSERT BP1 #/))
		 (DO ((N -1 (1+ N))
		      (BP2 BP1 (FORWARD-SEXP BP2 -1))
		      (ARG (COND (*NUMERIC-ARG-P* (- 1 *NUMERIC-ARG*))
				 ((EQ COND-TYPE 'IF) -1)
				 (T 0))))
		     ((BP-= BP2 COND-BP)
		      (COND ((MINUSP (+ ARG N -3))
			     (DELETE-INTERVAL COND-BP (FORWARD-WORD COND-BP) T)
			     (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS*
						    (INSERT COND-BP "COND")))
			     (INSERT-MOVING BP #/()
			     (COND ((EQ COND-TYPE 'IF)
				    (SETQ BP (FORWARD-SEXP BP 2))
				    (INSERT-MOVING BP ")
 (T"))))
			    (T
			     (SETQ BP (INSERT COND-BP "COND (("))
			     (LET ((BP1 (IF (PLUSP ARG) (FORWARD-LIST BP 1 NIL 1)
					    (FORWARD-SEXP BP (+ ARG N)))))
			       (INSERT BP1 #/)))
			     (SETQ BP (FORWARD-CHAR BP -1))))
		      (COND ((EQ COND-TYPE 'OR)
			     (INSERT (FORWARD-SEXP BP) #/))
			     (INSERT-MOVING BP "(NOT "))))))))
        (OR UPCASE-P (DOWNCASE-INTERVAL COND-BP BP T))
        (MOVE-BP POINT (FORWARD-LIST BP -1 NIL (IF (MEMQ COND-TYPE '(IF OR)) 2 1)))
        (COM-INDENT-SEXP)                       ;Regrind changed stuff
        (MOVE-BP POINT (FORWARD-LIST (FORWARD-SEXP POINT) -1 NIL -1 T)))
      (COND (FIXBP1
	     (DELETE-INTERVAL FIXBP1 FIXBP2 T)
	     (FLUSH-BP FIXBP1)
	     (FLUSH-BP FIXBP2))))))
    DIS-TEXT)

;;; Find the containing member of set
(DEFUN FIND-CONTAINING-ATOM (BP SET)
  (DO ((BP BP)
       (BP1) (BP2) (TEM))
      (NIL)
    (OR (SETQ BP (FORWARD-LIST BP -1 NIL 1))
	(RETURN NIL))
    (SETQ BP1 (FORWARD-LIST BP 1 NIL -1 T)
	  BP2 (FORWARD-ATOM BP1))
    (AND (SETQ TEM (MEM #'STRING-EQUAL (STRING-INTERVAL BP1 BP2 T) SET))
	 (RETURN BP1 (CAR TEM)))))

(DEFUN COUNT-LIST-ELEMENTS (BP &AUX END-BP)
  (SETQ END-BP (FORWARD-SEXP BP))
  (DO ((BP (FORWARD-LIST BP 1 NIL -1 T) (FORWARD-SEXP BP))
       (I -1 (1+ I)))
      (NIL)
    (AND (NULL BP) (RETURN NIL))
    (AND (BP-= BP END-BP) (RETURN I))))

;;; This tries to find someplace that looks like it probably doesn't have enough parens
;;; It takes the first place that has a lesser indentation level than the given BP.
(DEFUN LIKELY-UNBALANCED-POINT (BP LIMIT-BP)
  (DO ((IND (BP-INDENTATION BP))
       (LINE (LINE-NEXT (BP-LINE BP)) (LINE-NEXT LINE))
       (LIMIT-LINE (BP-LINE LIMIT-BP))
       (OLINE (BP-LINE BP)))
      ((EQ LINE LIMIT-LINE) LIMIT-BP)
    (COND ((NOT (MEMQ (LINE-TYPE LINE) '(:COMMENT :BLANK)))
	   (AND ( (LINE-INDENTATION LINE) IND)
		(RETURN (END-OF-LINE OLINE)))
	   (SETQ OLINE LINE)))))

(DEFCOM COM-FROB-DO "Interchange old and new style DO's" ()
  (ATOM-WORD-SYNTAX-BIND
   (LET (DO-BP DO-TYPE
	BP BP1 BP2 BP3)
    (MULTIPLE-VALUE (DO-BP DO-TYPE)
      (FIND-CONTAINING-ATOM (POINT) '(DO DOTIMES DOLIST)))
    (OR DO-BP (BARF))
    (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS* (FORWARD-WORD DO-BP)))
    (COND ((AND (EQ DO-TYPE 'DO)
		(= (LIST-SYNTAX (BP-CH-CHAR BP)) LIST-OPEN))	;New style
	   (OR (= (COUNT-LIST-ELEMENTS BP) 1)
	       (BARF "Too many DO variables"))
	   (OR (SETQ BP1 (FORWARD-SEXP BP)) (BARF))
	   (OR (= (COUNT-LIST-ELEMENTS BP1) 1)
	       (BARF "Cannot have ending form"))
	   (OR (SETQ BP2 (FORWARD-SEXP BP1)) (BARF))
	   (SETQ BP3 (FORWARD-SEXP BP2 -1))
	   (DELETE-INTERVAL (FORWARD-LIST BP2 -1 NIL -1 T) BP2 T)
	   (MOVE-BP (POINT) (DELETE-INTERVAL (FORWARD-LIST BP1 -1 NIL -2 T)
					     (FORWARD-LIST BP3 1 NIL -1 T) T))
	   (INSERT-MOVING (POINT) #\SP)
	   (DELETE-INTERVAL BP (FORWARD-LIST BP 1 NIL -2 T) T))
	  (T					;Old style or special
	   (COND ((NEQ DO-TYPE 'DO)
		  (OR (SETQ BP1 (FORWARD-LIST BP 1 NIL -1 T)) (BARF))
		  (SETQ BP2 (FORWARD-SEXP BP1))
		  (LET ((VARNAME (STRING-INTERVAL BP1 BP2 T)))
		    (DELETE-INTERVAL BP BP1 T)
		    (COND ((EQ DO-TYPE 'DOTIMES)
			   (SETQ BP2 (FORWARD-SEXP BP))
			   (INSERT-MOVING BP2 (IN-CURRENT-FONT " 0 (1+ "))
			   (INSERT-MOVING BP2 VARNAME)
			   (INSERT-MOVING BP2 (IN-CURRENT-FONT ") ( ")))
			  ((EQ DO-TYPE 'DOLIST)
			   (SETQ BP2 (FORWARD-SEXP BP 2))
			   (INSERT-MOVING BP2 (IN-CURRENT-FONT " (CDR "))
			   (INSERT-MOVING BP2 VARNAME)
			   (INSERT-MOVING BP2 (IN-CURRENT-FONT ") (NULL "))))
		    (INSERT-MOVING BP2 VARNAME))
		  (DELETE-INTERVAL DO-BP (FORWARD-WORD DO-BP) T)
		  (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS*
					 (INSERT DO-BP (IN-CURRENT-FONT "DO"))))))
	   (OR (SETQ BP1 (FORWARD-SEXP BP 3)) (BARF))
	   (DELETE-AROUND *WHITESPACE-CHARS* BP1)
	   (MOVE-BP (POINT) (INSERT-MOVING BP1 (IN-CURRENT-FONT #/))))
	   (INSERT-MOVING BP1 (IN-CURRENT-FONT ")
 ("))
	   (INSERT BP (IN-CURRENT-FONT "(("))
	   (INSERT (FORWARD-SEXP BP1) (IN-CURRENT-FONT #/)))
	   (INDENT-INTERVAL-FOR-LISP BP BP1 T)))))
  DIS-TEXT)

(DEFCOM COM-QUERY-REPLACE-LET-BINDING "Replace variable of LET with its value.
Point must be after or within the binding to be modified." ()
  (ATOM-WORD-SYNTAX-BIND
   (LET ((POINT (POINT))
	LET-BP BINDING-BP BP1 BP2 FROM TO)
    (OR (SETQ LET-BP (FIND-CONTAINING-ATOM POINT '(LET))) (BARF))
    (DO ((BP (FORWARD-LIST LET-BP 1 NIL -1 T) NBP)
	 (NBP))
	(NIL)
      (OR (SETQ NBP (FORWARD-SEXP BP 1 NIL 0 NIL NIL T)) (BARF))
      (OR (BP-< NBP POINT) (RETURN (SETQ BINDING-BP BP))))
    (SETQ BP1 (FORWARD-LIST BINDING-BP 1 NIL -1 T)
	  BP2 (FORWARD-SEXP BP1)
	  FROM (STRING-INTERVAL BP1 BP2 T))
    (SETQ BP1 (FORWARD-OVER *WHITESPACE-CHARS* BP2)
	  BP2 (FORWARD-SEXP BP1)
	  TO (STRING-INTERVAL BP1 BP2 T))
    (SETQ BP1 (FORWARD-SEXP LET-BP 2)
	  BP2 (FORWARD-SEXP BP1 1 NIL 1))
    (OR *NUMERIC-ARG-P* (PSETQ FROM TO TO FROM))
    (MOVE-BP POINT BP1)
    (LET ((*INTERVAL* (CREATE-INTERVAL BP1 BP2 T)))
      (QUERY-REPLACE POINT FROM TO T))))
  DIS-TEXT)

(DEFCOM COM-QUERY-REPLACE-LAST-KILL "Replace top of kill ring with region." ()
  (LET ((POINT (POINT)) (MARK (MARK)))
    (QUERY-REPLACE POINT (STRING-INTERVAL (CAR *KILL-RING*)) (STRING-INTERVAL MARK POINT)))
  DIS-TEXT)

(DEFCOM COM-JUST-ONE-SPACE "Replace all whitespace around point with arg spaces" ()
  (DELETE-AROUND *BLANKS* (POINT))
  (DOTIMES (I *NUMERIC-ARG*)
    (INSERT-MOVING (POINT) #\SP))
  DIS-TEXT)

(DEFCOM COM-CANONICALIZE-WHITESPACE "Try to fixup wrong spacing heuristically.
If given an argument, or called just after a yank type command, operates
at the mark, else at point." ()
  (LET ((BP (IF (OR *NUMERIC-ARG-P* (EQ *LAST-COMMAND-TYPE* 'YANK)) (MARK) (POINT)))
	BP1 CH1 CH2 SYN1 SYN2)
    (SETQ BP (BACKWARD-OVER *BLANKS* BP)
	  BP1 (FORWARD-OVER *BLANKS* BP)
	  CH1 (BP-CH-CHAR (OR (FORWARD-CHAR BP -1) (BARF)))
	  CH2 (BP-CH-CHAR BP1)
	  SYN1 (LIST-SYNTAX CH1)
	  SYN2 (LIST-SYNTAX CH2))
    (COND ((OR (= CH2 #\CR)			;If at the end of the line,
	       (MULTIPLE-VALUE-BIND (STRING SLASH COMMENT)
		   (LISP-BP-SYNTACTIC-CONTEXT BP)
		 (OR STRING SLASH COMMENT))))	;or any funny syntax, leave it alone
	  ((NOT (= CH1 #\CR))			;If not at beginning of line,
	   (DELETE-INTERVAL BP BP1 T)		;flush whitespace, and
	   (AND ( SYN1 LIST-OPEN) ( SYN1 LIST-SINGLE-QUOTE)
		( SYN2 LIST-CLOSE)
		(INSERT BP (IN-CURRENT-FONT #\SP))))	;leave zero or one space in its place
	  (( CH2 #/()				;If not start of defun
	   (INDENT-INTERVAL-FOR-LISP BP (BEG-LINE BP 1 T) T NIL T))	;run tab
	  ((DO ((LINE (LINE-PREVIOUS (BP-LINE BP)) (LINE-PREVIOUS LINE))
		(OLINE (BP-LINE BP) LINE)	;Flush blank lines, and
		(TYPE))				;unless previous non-blank is a comment
	       (NIL)
	     (SETQ TYPE (AND LINE (LINE-TYPE LINE)))
	     (COND ((NEQ TYPE ':BLANK)
		    (DELETE-INTERVAL (CREATE-BP OLINE 0) BP T)
		    (RETURN (NEQ TYPE ':COMMENT)))))
	   (INSERT BP #\CR))))			;leave just one in their place
  DIS-TEXT)

(DEFCOM COM-FIND-UNBALANCED-PARENTHESES "Find parenthesis error in buffer" ()
  (LET ((BEG-BP (INTERVAL-FIRST-BP *INTERVAL*))
	(END-BP (INTERVAL-LAST-BP *INTERVAL*))
	(POINT (POINT))
	(OLD-TICK (NODE-TICK *INTERVAL*))
	BEG-BP-1 END-BP-1 BP)
    (UNWIND-PROTECT
      (PROGN
	(SETQ BEG-BP-1 (COPY-BP BEG-BP ':MOVES)
	      END-BP-1 (COPY-BP END-BP ':NORMAL))
	(INSERT BEG-BP-1 "(
")
	(INSERT END-BP-1 "
)")
	(IF (SETQ BP (FORWARD-SEXP BEG-BP))
	    (IF (BP-= BP END-BP)		;All ok
		(TYPEIN-LINE "All parens appear balanced.")
		(MOVE-BP POINT BP)
		(TYPEIN-LINE "Probably extra right-paren here."))
	    (OR (SETQ BP (FORWARD-SEXP END-BP -1))
		(BARF "Cannot find unbalanced parenthesis"))
	    (MOVE-BP POINT BP)
	    (TYPEIN-LINE "Probably no right-paren for this left-paren.")))
      (COND (BEG-BP-1
	     (DELETE-INTERVAL BEG-BP BEG-BP-1 T)
	     (FLUSH-BP BEG-BP-1)))
      (COND (END-BP-1
	     (DELETE-INTERVAL END-BP-1 END-BP T)
	     (FLUSH-BP END-BP-1)))
      (SETF (NODE-TICK *INTERVAL*) OLD-TICK)))
  DIS-BPS)

(DEFCOM COM-DECLARE-SPECIAL "Add the nth previous word to the last special declaration" ()
  (ATOM-WORD-SYNTAX-BIND
    (LET (WORD)
      (LET ((BP1 (FORWARD-WORD (POINT) (- *NUMERIC-ARG*)))
	    BP2)
	(OR BP1 (BARF))
	(SETQ BP2 (FORWARD-WORD BP1 1))
	(OR BP2 (BARF))
	(SETQ WORD (STRING-INTERVAL BP1 BP2 T)))
      (LET ((BP (DO-NAMED DECLARES
			  ((LINE (BP-LINE (POINT)) (LINE-PREVIOUS LINE))
			   (LIMIT-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
			  (NIL)
		  (AND (STRING-EQUAL "(DECLARE " LINE 0 0 9 9)
		       ;;Found a (DECLARE ...), look for SPECIAL in the CARs of the elements
		       (DO ((BP1 (CREATE-BP LINE 9) (FORWARD-SEXP BP1))
			    (BP2)
			    (BP3))
			   ((NULL BP1))
			 (OR (SETQ BP2 (FORWARD-LIST BP1 1 NIL 1 T))
			     (RETURN NIL))
			 (OR (SETQ BP3 (FORWARD-WORD BP2)) (RETURN NIL))
			 (AND (EQ (BP-LINE BP2) (BP-LINE BP3))
			      (STRING-EQUAL "SPECIAL" (BP-LINE BP2) 0 (BP-INDEX BP2) 7
					    (BP-INDEX BP3))
			      (SETQ BP2 (FORWARD-LIST BP1))	;Found one
			      (RETURN-FROM DECLARES (FORWARD-CHAR BP2 -1)))))
		  ;;If there isnt a special declaration, make one at the start of the file
		  (AND (EQ LINE LIMIT-LINE)
		       (RETURN (FORWARD-CHAR (INSERT
					       (SKIP-OVER-BLANK-LINES-AND-COMMENTS
						 (INTERVAL-FIRST-BP *INTERVAL*) T)
					       "(DECLARE (SPECIAL))

")
					     -3))))))
	;;Now put it in and try not to overflow the line
	(WITH-BP (PT (POINT) ':MOVES)		;Preserve point
	  (MOVE-BP (POINT) BP)
	  (INSERT-MOVING (POINT) (STRING-APPEND #\SP WORD))
	  (AUTO-FILL-HOOK #\SP)
	  (COND ((END-LINE-P (POINT))
		 (MOVE-BP (POINT) (END-LINE (POINT) 1))
		 (INSERT (DELETE-BACKWARD-OVER *BLANKS* (POINT)) #\CR)
		 (COM-INDENT-FOR-LISP)))
	  (MOVE-BP (POINT) PT)))))
  DIS-TEXT)

;;; Pattern finding command
(DEFVAR *LAST-PATTERN* NIL)
(DEFVAR *LAST-PATTERN-BP* NIL)
(DEFVAR *LAST-PATTERN-RESTART-LIST*)

(DEFCOM COM-FIND-PATTERN "Move to next occurrence of the given pattern.
The pattern must be a list, ** matches any one thing, ... any number of things.
A numeric argument repeats the last search." ()
  (LET (FORM RESTART BP)
    (COND (*NUMERIC-ARG-P*
	   (SETQ FORM (OR *LAST-PATTERN* (BARF "No previous pattern")))
	   (TYPEIN-LINE "Finding ~S" FORM)
	   (AND (BP-= (POINT) *LAST-PATTERN-BP*) (SETQ RESTART *LAST-PATTERN-RESTART-LIST*)))
	  (T
	   (LET ((FORM-STRING (TYPEIN-LINE-READLINE "Pattern to search for:"))
		 (EOF '(())))
	     (SETQ FORM (READ-FROM-STRING FORM-STRING EOF))
	     (AND (EQ FORM EOF) (BARF "Unbalanced parens"))
	     (OR (LISTP FORM) (BARF "I only know how to search for lists"))
	     ;; This is sort of a kludge
	     (OR (EQ PACKAGE SI:PKG-USER-PACKAGE)
		 (SETQ FORM (SUBLIS (LIST (CONS (INTERN "...") ':...)
					  (CONS (INTERN "**") ':**))
				    FORM))))))
    (MULTIPLE-VALUE (BP RESTART) (FIND-PATTERN (POINT) FORM RESTART))
    (OR BP (BARF))
    (MAYBE-PUSH-POINT BP)
    (MOVE-BP (POINT) BP)
    (SETQ *LAST-PATTERN* FORM *LAST-PATTERN-BP* BP *LAST-PATTERN-RESTART-LIST* RESTART))
  DIS-BPS)

;;; Attempt to find an instance of THING after BP, return a new BP if successful
(DEFUN FIND-PATTERN (BP PATTERN &OPTIONAL RESTART-LIST)
  (DO-NAMED FIND-PATTERN
      ((BP1 (FORWARD-DEFUN BP -1 T) (FORWARD-DEFUN BP2))
       (BP2)
       (STREAM (INTERVAL-STREAM *INTERVAL*))
       (FORM)
       (SI:XR-CORRESPONDENCE-FLAG T)
       (SI:XR-CORRESPONDENCE NIL NIL)
       (RESTART-LIST RESTART-LIST NIL)
       (PLIST)
       (TEM))
      ((NULL BP1) NIL)
    (SETQ PLIST (LOCF (LINE-CONTENTS-PLIST (BP-LINE BP1))))
    (SETQ BP2 (FORWARD-SEXP BP1))		;Find the end of this defun
    ;; Now get the form and correspondence for this defun, using previous if there
    (COND (BP2
	   (COND ((AND (SETQ TEM (GET PLIST 'CORRESPONDENCE))
		       (COND ((> (CADR TEM) (INTERVAL-REAL-TICK BP1 BP2 T))
			      (SETQ FORM (CAR TEM) SI:XR-CORRESPONDENCE (CADDR TEM))
			      T)
			     (T
			      (REMPROP PLIST 'CORRESPONDENCE)
			      NIL))))
		 (T
		  (FUNCALL STREAM ':SET-BP BP1)
		  (SETQ FORM (READ STREAM))
		  (PUTPROP PLIST (LIST FORM (TICK) SI:XR-CORRESPONDENCE) 'CORRESPONDENCE)))
	   (AND RESTART-LIST (SETQ FORM (CAR RESTART-LIST) RESTART-LIST (CDR RESTART-LIST)))
	   (DO ((FORM FORM (CAR RESTART-LIST))
		(RESTART-LIST RESTART-LIST (CDR RESTART-LIST))
		(FOUND)
		(BP3))
	       (NIL)
	     (MULTIPLE-VALUE (FOUND RESTART-LIST)
	       (FIND PATTERN FORM RESTART-LIST))
	     (OR FOUND (RETURN NIL))
	     (AND (SETQ BP3 (CADR (MEMQ FOUND SI:XR-CORRESPONDENCE)))
		  (BP-< BP BP3)
		  (RETURN-FROM FIND-PATTERN BP3 RESTART-LIST))))
	  (T					;Look forward for next defun
	   (SETQ BP2 BP1)))))

;;; Attempt to find an instance of THING in LIST
(DEFUN FIND (THING LIST &OPTIONAL RESTART-LIST &AUX VAL)
  (COND	((AND RESTART-LIST (MULTIPLE-VALUE (VAL RESTART-LIST)
			     (FIND THING (CAR RESTART-LIST) (CDR RESTART-LIST))))
	 (PUSH LIST RESTART-LIST))
	((MATCH THING LIST) (SETQ VAL LIST RESTART-LIST NIL))
	((ATOM LIST) (SETQ VAL NIL RESTART-LIST NIL))
	(T
	 (DO ((LIST LIST (CDR LIST)))
	     ((NULL LIST) (SETQ VAL NIL RESTART-LIST NIL))
	   (MULTIPLE-VALUE (VAL RESTART-LIST) (FIND THING (CAR LIST)))
	   (COND (VAL
		  (PUSH (CDR LIST) RESTART-LIST)
		  (RETURN NIL))))))
  (VALUES VAL RESTART-LIST))

;;; Simple minded pattern matcher
;;; ** matches an arbitrary frob, ... an arbitrary number (possibly 0) of frobs
(DEFUN MATCH (A B)
  (DO ((A A (CDR A))
       (B B (CDR B))
       (VAL))
      (NIL)
    (COND ((EQ A B) (RETURN T))
	  ((EQ A ':...) (RETURN 'CDR))
	  ((EQ A ':**) (RETURN T))
	  ((NOT (= (%DATA-TYPE A) (%DATA-TYPE B))) (RETURN NIL))
	  ((NUMBERP A) (RETURN (= A B)))
	  ((ARRAYP A) (RETURN (AND (STRINGP A) (STRINGP B) (STRING-EQUAL A B))))
	  ((NLISTP A) (RETURN NIL))
	  ((NOT (SETQ VAL (MATCH (CAR A) (CAR B)))) (RETURN NIL))
	  ((NEQ VAL T) (RETURN (OR (NULL (SETQ A (CDR A)))
				   (DO B B (CDR B) (NULL B)
				       (AND (MATCH A B) (RETURN T)))))))))

(DEFCOM COM-UNDO "Undo the last undoable command" ()
  (OR (BOUNDP '*UNDO-START-BP*) (BARF "Nothing to undo"))
  (OR (EQ (BP-TOP-LEVEL-NODE *UNDO-START-BP*)
	  (BP-TOP-LEVEL-NODE (INTERVAL-FIRST-BP *INTERVAL*)))
      (BARF "No longer in the same buffer"))
  (TYPEIN-LINE "")
  (LET ((POINT (POINT)) (MARK (MARK))
	(OLD *UNDO-OLD-INTERVAL*)
	(NAME *UNDO-TYPE*))
    (COND ((FQUERY '(:SELECT T) "Undo ~A? " NAME)
	   (MOVE-BP MARK *UNDO-START-BP*)
	   (MOVE-BP POINT *UNDO-END-BP*)
	   (UNDO-SAVE MARK POINT T "Undo")
	   (DELETE-INTERVAL MARK POINT T)
	   (INSERT-INTERVAL-MOVING POINT OLD)
	   (TYPEIN-LINE "~A undone." NAME))))
  DIS-TEXT)

(DEFCOM COM-EXECUTE-COMMAND-INTO-BUFFER "Direct typeout from a command into the buffer" ()
  (LET* ((*TYPEOUT-WINDOW* (MAKE-INTERVAL-TYPEOUT-STREAM))
	 (STANDARD-OUTPUT *TYPEOUT-WINDOW*))
    (PROMPT-LINE "Key: ")
    (PROCESS-COMMAND-CHAR (PROMPT-LINE-ACTIVATE (FUNCALL STANDARD-INPUT ':TYI)))
    (MOVE-BP (MARK) (POINT))
    (MOVE-BP (POINT) (FUNCALL *TYPEOUT-WINDOW* ':READ-BP))
    (SETQ *CURRENT-COMMAND-TYPE* 'YANK))
  DIS-TEXT)

(DEFCOM COM-INSERT-DATE "Print the curent date into the buffer.
Calls TIME:PRINT-CURRENT-TIME, or if given an argument TIME:PRINT-CURRENT-DATE" ()
  (LET ((STREAM (INTERVAL-STREAM (POINT) (POINT) T)))
    (FUNCALL (IF *NUMERIC-ARG-P* #'TIME:PRINT-CURRENT-DATE #'TIME:PRINT-CURRENT-TIME)
	     STREAM)
    (MOVE-BP (MARK) (POINT))
    (MOVE-BP (POINT) (FUNCALL STREAM ':READ-BP)))
  DIS-TEXT)

(DEFCOM COM-COUNT-LINES-REGION "Print the number of lines in the region in the echo area." ()
  (REGION (BP1 BP2)
    (TYPEIN-LINE "~D line~:P.  " (1- (COUNT-LINES BP1 BP2 T))))
  DIS-NONE)

(DEFCOM COM-WHERE-AM-I "Print various things about where the point is.
Print the X and Y positions, the octal code for the following character,
the current line number and its percentage of the total file size.
If there is a region, the number of lines in it is printed.
Fast Where Am I prints a subset of this information faster." (KM)
  (REDISPLAY *WINDOW* ':POINT NIL NIL T)
  (LET ((POINT (POINT))
	(FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*))
	(LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
    (LET ((POINT-LINES (1- (COUNT-LINES FIRST-BP POINT)))
	  (INTERVAL-LINES (1- (COUNT-LINES FIRST-BP LAST-BP)))
	  (AT-END-P (BP-= (INTERVAL-LAST-BP *INTERVAL*) POINT))
	  (BP-IND (BP-INDENTATION POINT))
	  (SW (FONT-SPACE-WIDTH)))
      (TYPEIN-LINE "X=[~D. chars|~D. pixels|~:[~S~;~D.~] columns] ~
			Y=~D.~@[ Char=~O~] Line=~D.(~D%)"
		   (BP-INDEX POINT)
		   BP-IND
		   (ZEROP (\ BP-IND SW))
		   (IF (ZEROP (\ BP-IND SW))
		       (// BP-IND SW)
		       (// (FLOAT BP-IND) SW))
		   (FIND-BP-IN-WINDOW *WINDOW* POINT)
		   (AND (NOT AT-END-P) (BP-CHAR POINT))
		   POINT-LINES
		   (IF (ZEROP INTERVAL-LINES)
		       0
		       (// (* 100. POINT-LINES) INTERVAL-LINES)))))
  (AND (WINDOW-MARK-P *WINDOW*)
       (REGION (BP1 BP2)
	 (TYPEIN-LINE-MORE ", Region has ~D line~:P.  " (1- (COUNT-LINES BP1 BP2 T)))))
  DIS-NONE)

(DEFCOM COM-FAST-WHERE-AM-I "Quickly print various things about where the point is.
Print the X and Y positions, and the octal code for the following character.
If there is a region, the number of lines in it is printed.
Where Am I prints the same things and more." (KM)
  (REDISPLAY *WINDOW* ':POINT NIL NIL T)
  (LET ((POINT (POINT)))
    (LET ((AT-END-P (BP-= (INTERVAL-LAST-BP *INTERVAL*) POINT))
	  (BP-IND (BP-INDENTATION POINT))
	  (SW (FONT-SPACE-WIDTH)))
      (TYPEIN-LINE "X=[~D. chars|~D. pixels|~:[~S~;~D.~] columns] Y=~D.~@[ Char=~O~]"
		   (BP-INDEX POINT)
		   BP-IND
		   (ZEROP (\ BP-IND SW))
		   (IF (ZEROP (\ BP-IND SW))
		       (// BP-IND SW)
		       (// (FLOAT BP-IND) SW))
		   (FIND-BP-IN-WINDOW *WINDOW* POINT)
		   (AND (NOT AT-END-P) (BP-CHAR POINT)))))
  (AND (WINDOW-MARK-P *WINDOW*)
       (REGION (BP1 BP2)
	 (TYPEIN-LINE-MORE ", Region has ~D line~:P.  " (1- (COUNT-LINES BP1 BP2 T)))))
  DIS-NONE)

(DEFCOM COM-ARGLIST "Print the argument list of the specified function.
Reads the name of the function from the mini-buffer (the top of the kill
ring has the /"current/" function from the buffer) and prints the arglist
in the echo area." ()
  (LET ((NAME (READ-FUNCTION-NAME "Arglist" (RELEVANT-FUNCTION-NAME (POINT)) T)))
    (PRINT-ARGLIST NAME))
  DIS-NONE)

(DEFCOM COM-QUICK-ARGLIST "Print the argument list of the function to left of cursor." ()
  (QUICK-ARGLIST)
  DIS-NONE)

(DEFUN QUICK-ARGLIST (&OPTIONAL (STREAM *TYPEIN-WINDOW*))
  (IF *NUMERIC-ARG-P*
      (LET ((NAME (READ-FUNCTION-NAME "Arglist" (RELEVANT-FUNCTION-NAME (POINT)) T)))
	(PRINT-ARGLIST NAME STREAM))
      (LET ((SYMBOL (RELEVANT-FUNCTION-NAME (POINT))))
	(COND ((COND ((MEMQ SYMBOL '(FUNCALL FUNCALL-SELF <-))
		      (SETQ SYMBOL (RELEVANT-METHOD-NAME (POINT)
							 (IF (EQ SYMBOL 'FUNCALL-SELF) 1 2))))
		     ((EQ SYMBOL 'DEFMETHOD)
		      (LET ((METHOD-SYMBOL (RELEVANT-DEFMETHOD-METHOD-NAME (POINT))))
			(COND (METHOD-SYMBOL
			       (SETQ SYMBOL METHOD-SYMBOL)
			       T)))))
	       (MULTIPLE-VALUE-BIND (ARGLIST NAME RETLIST)
		   (METHOD-ARGLIST SYMBOL)
		 (COND ((EQ STREAM *TYPEIN-WINDOW*)
			(TYPEIN-LINE "")
			(FUNCALL STREAM ':TYPEOUT-STAYS)))
		 (FORMAT STREAM "~S: ~:A~@[  ~:A~]"
			 (OR NAME SYMBOL) ARGLIST RETLIST)))
	      ((AND SYMBOL (FDEFINEDP SYMBOL))
	       (PRINT-ARGLIST SYMBOL STREAM))
	      ((BARF))))))	;Looked hard but couldn't find a defined function

(DEFUN PRINT-ARGLIST (SYMBOL &OPTIONAL (STREAM *TYPEIN-WINDOW*))
  (COND ((EQ STREAM *TYPEIN-WINDOW*)
	 (TYPEIN-LINE "")
	 (FUNCALL STREAM ':TYPEOUT-STAYS)))
  (MULTIPLE-VALUE-BIND (ARGLIST RETURNS TYPE)
      (ARGLIST SYMBOL)
    (FORMAT STREAM "~S~@[ (~A)~]: " SYMBOL TYPE)
    (IF (OR (LISTP ARGLIST) (NULL ARGLIST))
	(PRINT-ARGLIST-INTERNAL ARGLIST STREAM)
	(PRINC "??" STREAM))
    (AND RETURNS (FORMAT STREAM "  ~:A" RETURNS))))

;; This prints an arglist in a convenient form, ie:
;; (si:first &special si:second &local &optional (si:third (quote si:default)))
;; prints: (first &special si:second &local &optional (third 'si:default))
(DEFUN PRINT-ARGLIST-INTERNAL (LIST STREAM &AUX SPECIAL)
  (FUNCALL STREAM ':TYO #/()
  (DO ((L LIST (CDR L)))
      ((NULL L)
       (FUNCALL STREAM ':TYO #/)))
    (COND ((SYMBOLP L)
	   (FUNCALL STREAM ':STRING-OUT ". ")
	   (FUNCALL (IF SPECIAL #'PRIN1 #'PRINC) L STREAM)
	   (FUNCALL STREAM ':TYO #/))
	   (RETURN NIL)))
    (SELECTQ (CAR L)
      (&SPECIAL (SETQ SPECIAL T))
      (&LOCAL (SETQ SPECIAL NIL)))
    (COND ((OR (NLISTP (CAR L))			;If the element is a symbol
	       (NLISTP (CDAR L))		;Or if it's not a list with exactly two elmts.
	       (NOT (NULL (CDDAR L))))
	   (FUNCALL (IF SPECIAL #'PRIN1 #'PRINC) (CAR L) STREAM))	;Just print it.
	  (T ;; This is the special case of an element with a default.
	     (FUNCALL STREAM ':TYO #/()
	     (FUNCALL (IF SPECIAL #'PRIN1 #'PRINC) (CAAR L) STREAM)
	     (FUNCALL STREAM ':TYO #\SP)
	     ;; If the default is quoted, print it nicely.
	     (COND ((AND (LISTP (CADAR L))
			 (EQ (CAADAR L) 'QUOTE))
		    (FUNCALL STREAM ':TYO #/')
		    (PRIN1 (CADR (CADAR L)) STREAM))
		   (T (PRIN1 (CADAR L) STREAM)))
	     (FUNCALL STREAM ':TYO #/))))
    (AND (CDR L) (FUNCALL STREAM ':TYO #\SP))))

(DEFCOM COM-BRIEF-DOCUMENTATION "Print brief documentation for the specified function.
Reads the name of the function from the mini-buffer (the default is
the /"current/" function from the buffer) and prints the first
line of its documentation in the echo area." ()
    (LET ((NAME (READ-FUNCTION-NAME "Brief Document" (RELEVANT-FUNCTION-NAME (POINT)) T)))
      (LET ((DOC (FUNCTION-DOCUMENTATION NAME)))
	(COND ((NULL DOC) (TYPEIN-LINE "~S is not documented" NAME))
	      (T (TYPEIN-LINE "~S: ~A" NAME
			      (NSUBSTRING DOC 0 (STRING-SEARCH-CHAR #\CR DOC)))))))
    DIS-NONE)

(DEFCOM COM-LONG-DOCUMENTATION "Print long documentation for the specified function.
Reads the name of the function from the mini-buffer (the default is
the /"current/" function from the buffer) and displays the
function's arguments and documentation" ()
    (LET ((NAME (READ-FUNCTION-NAME "Document" (RELEVANT-FUNCTION-NAME (POINT)) T)))
      (LET ((DOC (FUNCTION-DOCUMENTATION NAME)))
	(COND ((NULL DOC) (TYPEIN-LINE "~S is not documented" NAME))
	      (T (PRINT-ARGLIST NAME)
		 (FORMAT T "~%~A" DOC)))))
    DIS-NONE)

(DEFCOM COM-DESCRIBE-VARIABLE-AT-POINT "Print information about variable at or before cursor.
The information printed is whether it is declared special, whether it has a value,
and whether it has documentation put on by DEFVAR.  If none of these are present,
looks for lookalike symbols in other packages." ()
  (LET* ((BP1 (FORWARD-ATOM (FORWARD-CHAR (POINT) 1 T) -1 T))
	 (BP2 (FORWARD-ATOM BP1)))
    (IF (NULL BP2) (BARF))
    (MULTIPLE-VALUE-BIND (VAR ERROR)
	(CATCH-ERROR (WITH-INPUT-FROM-STRING (S (BP-LINE BP1) (BP-INDEX BP1) (BP-INDEX BP2))
		       (READ S)))
      (IF (OR ERROR (NOT (SYMBOLP VAR))) (BARF))
      (TYPEIN-LINE "")				;Clear the echo area
      (COND ((NOT (DESCRIBE-VARIABLE-INTERNAL VAR))
	     (TYPEIN-LINE "~S is not a declared variable." VAR)
	     (MAPC #'DESCRIBE-VARIABLE-INTERNAL
		   (PACKAGE-LOOKALIKE-SYMBOLS VAR SI:PKG-GLOBAL-PACKAGE
					      '(SPECIAL COMPILER:SYSTEM-CONSTANT
						:VALUE-DOCUMENTATION)))))
      DIS-NONE)))

(DEFUN DESCRIBE-VARIABLE-INTERNAL (VAR)
  (LET ((DECL (GETL VAR '(SPECIAL COMPILER:SYSTEM-CONSTANT)))
	(BOUND (BOUNDP VAR))
	(DOC (GET VAR ':VALUE-DOCUMENTATION)))
    (COND ((OR DECL BOUND DOC)
	   (TYPEIN-LINE-MORE "~&~S has ~:[no~;a~] value" VAR BOUND)
	   (IF (EQ (CAR DECL) 'SPECIAL)
	       (TYPEIN-LINE-MORE " and is declared special ~:[by file ~A~]"
				 (EQ (CADR DECL) T) (CADR DECL)))
	   (IF (EQ (CAR DECL) 'COMPILER:SYSTEM-CONSTANT)
	       (TYPEIN-LINE-MORE " and is a system-constant"))
	   (IF DOC
	       (TYPEIN-LINE-MORE "~%~A" DOC))
	   T))))

(DEFCOM COM-TRACE "Trace or untrace a function.
Reads the name of the function from the mini-buffer (the top of the kill
ring has the /"current/" function from the buffer) then pops up a menu
of trace options.  With an argument, omits menu step" ()
  (LET ((FCN (READ-FUNCTION-NAME "Trace" (RELEVANT-FUNCTION-NAME (POINT)) T)))
    (IF (NOT *NUMERIC-ARG-P*)
	(TV:TRACE-VIA-MENUS FCN)
	(EVAL (IF (ATOM FCN) `(TRACE (,FCN)) `(TRACE (:FUNCTION ,FCN))))))
  DIS-NONE)

(DEFCOM COM-WHERE-IS-SYMBOL "Show which packages contain the specified symbol." ()
  (MULTIPLE-VALUE-BIND (SYMBOL NAME)
      (READ-FUNCTION-NAME "Where is symbol" NIL NIL T)
    (WHERE-IS (OR NAME SYMBOL)))
  DIS-NONE)

(DEFCOM COM-COUNT-LINES-PAGE "Type number of lines on this page.
Also add, in parentheses, the number of lines on the page
before point, and the number of lines after point." ()
   (LET ((POINT (POINT)))
     (LET ((N1 (1- (COUNT-LINES (FORWARD-PAGE POINT -1 T) POINT)))
	   (N2 (1- (COUNT-LINES POINT (FORWARD-PAGE POINT 1 T)))))
       (TYPEIN-LINE "Page has ~D (~D + ~D) lines" (+ N1 N2) N1 N2)))
   DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FUNCTION-NAME "Disassemble" DO-DISASSEMBLE
			  NIL "Disassemble this function.")

(DEFCOM COM-DISASSEMBLE "Disassemble the specified function." ()
  (DO-DISASSEMBLE (READ-FUNCTION-NAME "Disassemble" (RELEVANT-FUNCTION-NAME (POINT)) T))
  DIS-NONE)

(DEFCOM COM-QUICK-DISASSEMBLE "Disassemble the function to the left of the cursor." ()
  (IF *NUMERIC-ARG-P*
      (DO-DISASSEMBLE (READ-FUNCTION-NAME "Disassemble" (RELEVANT-FUNCTION-NAME (POINT)) T))
      (DO-DISASSEMBLE (RELEVANT-FUNCTION-NAME (POINT))))
  DIS-NONE)

(DEFUN DO-DISASSEMBLE (SYMBOL &AUX FSPEC)
  (COND ((FDEFINEDP SYMBOL)
	 (SETQ FSPEC (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC SYMBOL)))
	 (COND ((OR (= (%DATA-TYPE FSPEC) DTP-FEF-POINTER)
		    (AND (LISTP FSPEC)
			 (EQ (CAR FSPEC) 'MACRO)
			 (= (%DATA-TYPE (CDR FSPEC)) DTP-FEF-POINTER)))
		(FORMAT T "~&~S:" SYMBOL)
		(DISASSEMBLE SYMBOL))
	       ((BARF "Can't find FEF for ~S" SYMBOL))))
	((BARF)))
  NIL)
