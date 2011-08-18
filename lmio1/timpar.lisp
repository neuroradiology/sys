;;;     -*- Mode: Lisp; Package: Time; Base: 10. -*-
;;; Convert Date To Binary for the Lisp Machine    DLW 9/14/80
;;; Take a description of a time (including a date) in any of many commonly
;;; understood formats, parse it and return it in a machine-usable form.

;;;    Hack for friday the 13th??

(DEFVAR *TIME-PACKAGE* (PKG-FIND-PACKAGE "TIME"))

;;; Character manipulation primitives that ought to be installed
;;; for general use, possibly not under these names.

;;; Given a character, return a string of that character.
(DEFUN STRING-CHAR (CHAR)
  (LET ((STRING (MAKE-ARRAY NIL 'ART-STRING 1)))
    (ASET CHAR STRING 0)
    STRING))

;;; Is this character a digit?
(DEFUN DIGIT-CHARACTER-P (CHAR)
  (AND ( CHAR #/0) ( CHAR #/9)))

;;; Is this character a letter?
(DEFUN LETTER-CHARACTER-P (CHAR)
  (OR (AND ( CHAR #/A) ( CHAR #/Z))
      (AND ( CHAR #/a) ( CHAR #/z))))


;;; Random list function.
(DEFUN DELQ-ALL (LIST SYMBOLS)
  (DOLIST (SYMBOL SYMBOLS)
    (SETQ LIST (DELQ SYMBOL LIST)))
  LIST)


;;; Lexical analyzer.

;;; This function is the lexical analyzer of the parser; it splits a string
;;; up into tokens.  It takes a string, and optionally the starting and finishing
;;; indexes of a substring within the string to use.  It returns a list.
;;; Each element of the list corresponds to a token.  Numbers, interpreted
;;; in decimal, appear in the list as two-lists; the first element is a fixnum
;;; giving the value of the number, and the second is the number of digits
;;; (including leading zeroes!).  Any other token appears as
;;; a symbol (interned in the Time package).
(DEFUN LEXICALLY-ANALYZE (STRING &OPTIONAL (START 0) (END (STRING-LENGTH STRING)))
  ;; Each time around this loop we either skip over some uninteresting text,
  ;; or we cons another token onto the list of tokens.
  (DO ((INDEX START)
       (RESULT NIL))
      (( INDEX END)
       (NREVERSE RESULT))
    (LET ((CHAR (AREF STRING INDEX)))
      (COND ((MEMQ CHAR '(#\SP #\TAB #/' #\CR))
	     ;; This is a whitespace character, ignore it.
	     (SETQ INDEX (1+ INDEX)))
	    ((MEMQ CHAR '(#/- #/+ #// #/: #/, #/. #/;))
	     ;; This is a special character.  Make a new token which is is
	     ;; symbol whose name is that character.
	     (PUSH (INTERN (STRING-CHAR CHAR) *TIME-PACKAGE*) RESULT)
	     (SETQ INDEX (1+ INDEX)))
	    ((DIGIT-CHARACTER-P CHAR)
	     ;; This is the beginning of a number in decimal.  Make a new token
	     ;; which is a fixnum of this number's value.
	     (DO ((I (1+ INDEX) (1+ I))
		  (DIGITS 1 (1+ DIGITS))
		  (N 0))
		 (NIL)
	       (SETQ N (+ (* N 10.) (- CHAR #/0)))
	       (COND ((OR ( I END)
			  (NOT (DIGIT-CHARACTER-P (SETQ CHAR (AREF STRING I)))))
		      (PUSH (LIST N DIGITS) RESULT)
		      (SETQ INDEX I)
		      (RETURN NIL)))))
	    ((LETTER-CHARACTER-P CHAR)
	     ;; This is the beginning of an alphabetic token.  Scan over all contiguous
	     ;; letters, upcasing them, and make a new token which is a symbol.
	     (DO ((I INDEX (1+ I)))
		 ((OR ( I END)
		      (LET ((CHAR (AREF STRING I)))
			(AND (NOT (LETTER-CHARACTER-P CHAR))
			     (NOT (AND (CHAR-EQUAL CHAR #/-)
				       (< (1+ I) END)
				       (LETTER-CHARACTER-P (AREF STRING (1+ I))))))))
		  (PUSH (INTERN (STRING-UPCASE (NSUBSTRING STRING INDEX I))
				*TIME-PACKAGE*)
			RESULT)
		  (SETQ INDEX I))))
	    ((= CHAR #/()
	     ;; This is the beginning of a parenthesized string.  RFC733 defines such
	     ;; to be equivalent to whitespace, so we ignore this string.  The "Laurel"
	     ;; program puts days of the week in parens, but these are redundant so it
	     ;; is OK to ignore them.
	     (DO ((I INDEX (1+ I)))
		 ((OR ( I END)
		      (= (AREF STRING I) #/)))
		  (SETQ INDEX (1+ I)))))
	    (T
	     (BARF "Unknown character ~C" CHAR))))))

;;; Defining patterns.

;;; A pattern is a list of N pattern elements, which are compared one-for-one
;;; with the first N elements of the token list we are analyzing.  Each
;;; pattern element may be:
;;; (a) A symbol or fixnum, which matches exactly that symbol or fixnum.
;;; (b) A list whose CAR is a "special pattern" symbol, in which case a special
;;;        function associated with that symbol is invoked.
;;; (c) A list of one symbol, which is interpreted as an arbitrary predicate
;;;        of one argument, which is applied to the token and should return
;;;        true if the token "matches".
;;;
;;; Note: symbols with FIXNUM-STRING properties are treated as if they
;;; were fixnums, rather than symbols.  This is for English cardinal and
;;; ordinal numbers.
;;;
;;; The following special pattern symbols exist, with the following special "forms":
;;;  (FIXP), which matches any fixnum.
;;;  (FIXP <n>), which matches any fixnum with exactly n digits.
;;;  (FIXP <m> <n>), which matches any fixnum with between m and n digits inclusive.
;;;  (ANY-OF <pattern-element> <pattern-element> ...), which matches if any of the
;;;    pattern elements match.
;;;  (GET <property-indicator>), which matches if the token is a symbol with a
;;;    non-NIL <property-indicator> property.
;;;  (ANY), which matches any single token.

;;; Examples:
;;;   ((FIXP 1 2) /: (FIXP 2) /: (FIXP 2))    Matches 11:30:15
;;;   ((GET MONTH) (FIXP 1 2) /, (FIXP 4))    Matches Jan 23, 1980
;;;   (12. (GET MERIDIAN))                    Matches 12 pm or 12 am
;;;   ()                                      Matches anything
;;;   ((ANY))                                 Matches anything except no tokens.

;;; The special form DEFPATTERN defines a pattern, information about when to try to
;;; match it, and what to do if it is matched.  The form looks like this:
;;; (DEFPATTERN <from-state> <pattern> <to-state> <lambda-list> . <body>)
;;; The parser has a state, represented by a symbol.
;;; It finds all the DEFPATTERNs for its current
;;; state by finding all those with <from-state> EQ to the current state.
;;; It applies each pattern in succession.  When it finds a pattern that
;;; matches, it invokes the associated function (defined by the <lambda-list>
;;; and <body> of the DEFPATTERN) and sets the state to <to-state>; it also
;;; CDRs off the matched portion of the token list, proceeding with the rest
;;; of the list.  The argument to the function defined by DEFPATTERN are
;;; all those tokens in the token list that matched the pattern, except
;;; those that were expressed in the pattern as symbols or fixnums (since
;;; these are just constants, the function is not interested in them).
;;; (Those that were expressed as ANY-OF pattern elements ARE passed
;;; to the function, even if the token is a constant, just in case the
;;; function cares which of the choices was taken.
;;;
;;; The parse proceeds until there are no tokens left and the state
;;; has a FINAL-STATE property of T.
;;;
;;; There is another version of DEFPATTERN called DEFPATTERN-PEEK, which
;;; is the same except that it "looks ahead" at the tokens, without
;;; passing over them.  Also, the tokens are not passed to the function;
;;; the function must take zero arguments.
;;;
;;; NOTE that the order of DEFPATTERNs in this file is significant, and
;;; defines the order in which patterns are checked.
;;;
;;; A data structure that allows the parser to find all the patterns is
;;; constructed at LOAD-time.  A list of known states is maintained as
;;; the value of *STATES*.  Each state is given a PATTERNS property,
;;; consisting of a list of elements of the form:
;;;  (<pattern> <to-state> <function-to-call> <skip-tokens>)
;;; These lists are CONSed up in reverse order, and NREVERSEd at the end.
;;; <skip-tokens> is true for DEFPATTERN and false for DEFPATTERN-PEEK.

(DEFMACRO DEFPATTERN (FROM-STATE PATTERN TO-STATE LAMBDA-LIST . BODY)
  (LET ((FUNCTION-NAME (GENSYM)))
    `(PROGN 'COMPILE
	    (DEFUN ,FUNCTION-NAME ,LAMBDA-LIST . ,BODY)
	    (DEFINE-PATTERN ',FROM-STATE ',PATTERN ',TO-STATE ',FUNCTION-NAME T))))

(DEFMACRO DEFPATTERN-PEEK (FROM-STATE PATTERN TO-STATE LAMBDA-LIST . BODY)
  (LET ((FUNCTION-NAME (GENSYM)))
    `(PROGN 'COMPILE
	    (DEFUN ,FUNCTION-NAME ,LAMBDA-LIST . ,BODY)
	    (DEFINE-PATTERN ',FROM-STATE ',PATTERN ',TO-STATE ',FUNCTION-NAME NIL))))

;(DEFVAR *STATES* NIL)  Need to check if unbound.
(DECLARE (SPECIAL *STATES*))

;;; This function gets invoked once at load-time before any of the patters
;;; are defined.  There must be exactly one top-level call to this
;;; function in the file, and it must be before all the DEFPATTERNs.
(DEFUN START-PATTERNS ()
  (IF (BOUNDP '*STATES*)
      ;; We are reloading.
      (DOLIST (STATE *STATES*)
	(REMPROP STATE 'PATTERNS)))
  (SETQ *STATES* NIL))

;;; This function runs once at load-time for each DEFPATTERN.  This DEFUN must
;;; appear before any calls to DEFPATTERN in this file.
(DEFUN DEFINE-PATTERN (FROM-STATE PATTERN TO-STATE FUNCTION-NAME SKIP-PATTERNS)
  (OR (MEMQ FROM-STATE *STATES*)
      (PUSH FROM-STATE *STATES*))
  (PUSH (LIST PATTERN TO-STATE FUNCTION-NAME SKIP-PATTERNS)
	(GET FROM-STATE 'PATTERNS)))

;;; This function gets invoked once at load-time after all the patterns
;;; are defined.  There must be exactly one top-level call to this
;;; function in this file, and it must be after all the DEFPATTERNs.
(DEFUN FINISH-PATTERNS ()
  (DOLIST (STATE *STATES*)
    (SETF (GET STATE 'PATTERNS)
	  (NREVERSE (GET STATE 'PATTERNS)))))

;;; Parser.

;;; This is the function that interprets patterns according to the algorithm
;;; described above.  It returns the final value of STATE.

(DEFUN PARSE-1 (TOKEN-LIST INITIAL-STATE)
  (DO ((STATE INITIAL-STATE)
       (TOKENS TOKEN-LIST))
      ((AND (NULL TOKENS) (GET STATE 'FINAL-STATE))
       STATE)
    ;; Try matching the first tokens of TOKENS against all the patterns
    ;; associated with STATE.
    (DO ((TRIES (GET STATE 'PATTERNS) (CDR TRIES)))
	((NULL TRIES)
	 (BARF "No pattern matches the tokens ~S in state ~S" TOKENS STATE))
     (LET ((TRY (CAR TRIES)))
       ;; TRY represents one of the patterns associated with STATE; it looks
       ;; like (<pattern> <to-state> <function-name> <skip-tokens>).
       (COND ((PATTERN-MATCH (FIRST TRY) TOKENS)
	      ;; Found it!  Run the function, advance over the matched tokens,
	      ;; go to the new state and continue.
	      (LET ((RESULT (PATTERN-INVOKE (FIRST TRY) TOKENS (THIRD TRY) (FOURTH TRY))))
		(IF (FOURTH TRY)
		    (SETQ TOKENS RESULT)))
	      (SETQ STATE (SECOND TRY))
	      (RETURN NIL)))))))

;;; Try to match PATTERN against the beginning of TOKEN-LIST.  Return T if
;;; they match, else NIL.
(DEFUN PATTERN-MATCH (PATTERN TOKEN-LIST)
  (DO ((PATTERN PATTERN (CDR PATTERN))
       (TOKEN-LIST TOKEN-LIST (CDR TOKEN-LIST)))
      (NIL)
    (COND ((NULL PATTERN)
	   ;; We've matched each element.  Matches!
	   (RETURN T))
	  ((NULL TOKEN-LIST)
	   ;; There is more pattern, but no more tokens.  No match.
	   (RETURN NIL))
	  ((NOT (MATCH-ELEMENT (CAR PATTERN) (CAR TOKEN-LIST)))
	   ;; This element does not match, lose.
	   (RETURN NIL)))))

;;; Predicate: Does PATTERN-ELEMENT match TOKEN?
(DEFUN MATCH-ELEMENT (PATTERN-ELEMENT TOKEN)
  (COND ((SYMBOLP PATTERN-ELEMENT)
	 ;; The pattern element is a symbol; matching is judged by EQness.
	 (EQ PATTERN-ELEMENT TOKEN))
	((FIXP PATTERN-ELEMENT)
	 ;; Match any fixnum of this value, no matter what its length.
	 (OR
	   ;; First possibility: a number made of digits.
	   (AND (LISTP TOKEN)
		(FIXP (FIRST TOKEN))
		(= (FIRST TOKEN) PATTERN-ELEMENT))
	   ;; Other possibility: a string number.
	   (AND (SYMBOLP TOKEN)
		(GET TOKEN 'FIXNUM-STRING)
		(= (GET TOKEN 'VALUE) PATTERN-ELEMENT))))
	((EQ (FIRST PATTERN-ELEMENT) 'FIXP)
	 ;; Match certain fixnums.
	 (OR
	   ;; First possibility: a number made of digits.
	   (AND (LISTP TOKEN)
		(FIXP (FIRST TOKEN))
		(MATCH-NUMBER PATTERN-ELEMENT (SECOND TOKEN)))
	   ;; Other possibility: a string number.
	   (AND (SYMBOLP TOKEN)
		(GET TOKEN 'FIXNUM-STRING)
		(MATCH-NUMBER PATTERN-ELEMENT (IF (> (GET TOKEN 'VALUE) 9.) 2 1)))))
	((EQ (FIRST PATTERN-ELEMENT) 'ANY)
	 ;; Match any token.
	 T)
	((EQ (FIRST PATTERN-ELEMENT) 'ANY-OF)
	 ;; If the TOKEN is any of these things, match.
	 (MEMQ TOKEN (REST1 PATTERN-ELEMENT)))
	((EQ (FIRST PATTERN-ELEMENT) 'GET)
	 ;; If TOKEN is a symbol with this property, match.
	 (AND (SYMBOLP TOKEN)
	      (GET TOKEN (SECOND PATTERN-ELEMENT))))
	(T
	 ;; Not a "special" form.  This is a predicate to apply.
	 (FUNCALL (FIRST PATTERN-ELEMENT) TOKEN))))

;;; Internal function of MATCH-ELEMENT for matching numbers.
(DEFUN MATCH-NUMBER (PATTERN-ELEMENT LENGTH)
  (SELECTQ (LENGTH PATTERN-ELEMENT)
    (1 T)
    (2 (= (SECOND PATTERN-ELEMENT) LENGTH))
    (3 (AND ( (SECOND PATTERN-ELEMENT) LENGTH)
	    ( (THIRD PATTERN-ELEMENT) LENGTH)))))

;;; Call FUNCTION, passing it all the tokens of TOKEN-LIST that were
;;; matched by PATTERN, except the constants.
(DEFUN PATTERN-INVOKE (PATTERN TOKEN-LIST FUNCTION PASS-ARGUMENTS)
  (PROG ()
	(%OPEN-CALL-BLOCK FUNCTION 0 0)		; No ADI, destination IGNORE.
	(IF (NOT PASS-ARGUMENTS) (GO END-LOOP))        ; Don't give it arguments.
	(%ASSURE-PDL-ROOM (+ 4 (LENGTH PATTERN)))	; (Conservative.)
     LOOP (COND ((NULL PATTERN)
		 (GO END-LOOP)))
	(COND ((NOT (ATOM (CAR PATTERN)))
	       (%PUSH (CAR TOKEN-LIST))))
	(SETQ PATTERN (CDR PATTERN))
	(SETQ TOKEN-LIST (CDR TOKEN-LIST))
	(GO LOOP)
     END-LOOP
	(%ACTIVATE-OPEN-CALL-BLOCK)
	(RETURN TOKEN-LIST)))

;;; Given a token that represents a number, return the number's value.
(DEFUN NUMBER-VALUE (TOKEN)
  (COND ((AND (LISTP TOKEN)
	      (FIXP (FIRST TOKEN)))
	 ;; This is a number token made of digits.
	 (FIRST TOKEN))
	((AND (SYMBOLP TOKEN)
	      (GET TOKEN 'FIXNUM-STRING))
	 ;; This is an English ordinal or cardinal.
	 (GET TOKEN 'VALUE))
	(T (FERROR NIL "The token ~S is not a number at all." TOKEN))))


;;; Keywords.

;;; This stuff runs at LOAD time.  It sets up properties on various interesting
;;; keyword symbols, so that patterns can check for these properties.

;;; The argument is a list of lists.  Each list is a bunch of spellings
;;; of a value, each of which is a string; successive lists have successive values,
;;; starting at FIRST-VALUE.  Each spelling is turned into a symbol, which gets
;;; a VALUE property of the fixnum value, and a <TYPE> property of T.
(DEFUN ASSIGN-TYPE-AND-VALUES (TYPE LIST-OF-LISTS FIRST-VALUE)
  (DO ((REST LIST-OF-LISTS (CDR REST))
       (I FIRST-VALUE (1+ I)))
      ((NULL REST))
    (DOLIST (STRING (CAR REST))
      (IF (STRINGP STRING)			; Don't bash plist of NIL.
	  (LET ((SYMBOL (INTERN (STRING-UPCASE STRING) *TIME-PACKAGE*)))
	    (PUTPROP SYMBOL I 'VALUE)
	    (PUTPROP SYMBOL T TYPE))))))

;;; NOTE: This file must be loaded after the TIME file.
(ASSIGN-TYPE-AND-VALUES 'DAY-OF-THE-WEEK *DAYS-OF-THE-WEEK* 0)
(ASSIGN-TYPE-AND-VALUES 'MONTH *MONTHS* 1)

;;; Take a list of lists of symbols.  Every symbol gets a <type> property
;;; of T and a VALUE property of the first symbol of the list.
(DEFUN ASSIGN-TYPE-AND-VALUES-SYMBOLS (TYPE VALUE-PROP-NAME LIST-OF-LISTS)
  (DOLIST (LIST-OF-SYMBOLS LIST-OF-LISTS)
    (LET ((FIRST-SYMBOL (FIRST LIST-OF-SYMBOLS)))
      (DOLIST (SYMBOL LIST-OF-SYMBOLS)
	(PUTPROP SYMBOL FIRST-SYMBOL VALUE-PROP-NAME)
	(PUTPROP SYMBOL T TYPE)))))

(ASSIGN-TYPE-AND-VALUES-SYMBOLS 'HALF-DAY 'VALUE
 '((NOON N)
   (MIDNIGHT M)))

(ASSIGN-TYPE-AND-VALUES-SYMBOLS 'OFFSET 'OFFSET-VALUE
 '((YEARS YEAR YR)
   (MONTHS MONTH MO)
   (WEEKS WEEK WK)
   (DAYS DAY DA DY)
   (HOURS HOUR HR)
   (MINUTES MINUTE MINS MIN MN)
   (SECONDS SECOND SECS SEC SC)))

(ASSIGN-TYPE-AND-VALUES-SYMBOLS 'MERIDIAN 'VALUE
 '((AM)
   (PM)))

(DEFUN ASSIGN-TIME-ZONES ()
  (DOLIST (ZONE-SPEC *TIMEZONES*)
    (LET ((VALUE (FIRST ZONE-SPEC)))
      (IF (NOT (NULL (SECOND ZONE-SPEC)))
	  (ASSIGN-ZONE (SECOND ZONE-SPEC) VALUE))
      (IF (NOT (NULL (THIRD ZONE-SPEC)))
	  (ASSIGN-ZONE (THIRD ZONE-SPEC) VALUE))
      (IF (PLUSP (FOURTH ZONE-SPEC))
	  (ASSIGN-ZONE (STRING-CHAR (FOURTH ZONE-SPEC)) VALUE)))))

(DEFUN ASSIGN-ZONE (STRING VALUE)
  (IF (STRINGP STRING)				; Don't bash plist of NIL.
      (LET ((SYMBOL (INTERN STRING *TIME-PACKAGE*)))
	(PUTPROP SYMBOL VALUE 'ZONE-VALUE)	; Can't use VALUE: N and M are half-days too!
	(PUTPROP SYMBOL T 'TIME-ZONE))))

(ASSIGN-TIME-ZONES)

(PUTPROP '- 'SIGN T)
(PUTPROP '+ 'SIGN T)

;;; Cardinal and ordinal numbers.
(DEFUN ASSIGN-NUMBERS ()
  (DOTIMES (I 31.)
    (ASSIGN-NUMBER I (INTERN (STRING-UPCASE (FORMAT NIL "~:R" I)) *TIME-PACKAGE*))
    (ASSIGN-NUMBER I (INTERN (STRING-UPCASE (FORMAT NIL "~R" I)) *TIME-PACKAGE*))))

(DEFUN ASSIGN-NUMBER (NUMBER SYMBOL)
  (PUTPROP SYMBOL T 'FIXNUM-STRING)
  (PUTPROP SYMBOL NUMBER 'VALUE))

(ASSIGN-NUMBERS)

;;; Make indefinite articles work, so that "a minute" and "an hour" will be accepted.
(ASSIGN-NUMBER 1 'A)
(ASSIGN-NUMBER 1 'AN)

;;; German numbers.
(IF (GET ':GERMAN 'SI:PRINC-FUNCTION)
    (ASSIGN-GERMAN-NUMBERS))

(DEFUN ASSIGN-GERMAN-NUMBERS ()
  (LET ((BASE ':GERMAN))
    (DOTIMES (I 31.)
      (ASSIGN-NUMBER I (INTERN (STRING-UPCASE (FORMAT NIL "~S" I)) *TIME-PACKAGE*)))))

;;; The patterns.

;;; Buzz words.  The, And, Of, 1st, 2nd, 3rd, 4th.
(DEFCONST *NOISE-WORDS* '(THE AND OF AT ST ND RD TH /;))

(START-PATTERNS)

;;; March 15 means March 15; go look for a year preceeded by a comma.
(DEFPATTERN MAIN ((GET MONTH) (FIXP 1 2)) YEAR-COMMA
	    (MONTH DATE)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-DATE DATE))

;;; 15 March means March 15; go look for a year.
(DEFPATTERN MAIN ((FIXP 1 2) (GET MONTH)) YEAR-COMMA
	    (DATE MONTH)
  (SET-DATE DATE)
  (SET-MONTH-FROM-NAME MONTH))

;;; 3/15/80 means March 15, 1980.  15/3/80 means March 15, 1980 to a European.
;;; If both the numbers are small, an ambuguity must be dealt with.
(DEFPATTERN MAIN ((FIXP 1 2) // (FIXP 1 2) // (FIXP 2)) MAIN
	    (MONTH DATE YEAR-OF-CENTURY)
  (SET-MONTH-AND-DATE MONTH DATE)
  (SET-YEAR-OF-CENTURY YEAR-OF-CENTURY))

;;; 3/15/1980 means March 15, 1980.  Same European problem.
(DEFPATTERN MAIN ((FIXP 1 2) // (FIXP 1 2) // (FIXP 4)) MAIN
	    (MONTH DATE YEAR)
  (SET-MONTH-AND-DATE MONTH DATE)
  (SET-YEAR YEAR))

;;; 3/15 means March 15, year defaults.  Same European problem.
(DEFPATTERN MAIN ((FIXP 1 2) // (FIXP 1 2)) MAIN
	    (MONTH DATE)
  (SET-MONTH-AND-DATE MONTH DATE))

;;; Note: GDixon's convert_date_to_binary_.rd believes in YY-MM-DD; the code
;;; below believes in MM-DD-YY.  RFC733 does not allow numeric months at all.

;;; 3-15-80 means March 15, 1980.  Same European problem.
(DEFPATTERN MAIN ((FIXP 1 2) - (FIXP 1 2) - (FIXP 2)) MAIN
	    (MONTH DATE YEAR-OF-CENTURY)
  (SET-MONTH-AND-DATE MONTH DATE)
  (SET-YEAR-OF-CENTURY YEAR-OF-CENTURY))

;;; 3-15-1980 means March 15, 1980.  Same European problem.
(DEFPATTERN MAIN ((FIXP 1 2) - (FIXP 1 2) - (FIXP 4)) MAIN
	    (MONTH DATE YEAR)
  (SET-MONTH-AND-DATE MONTH DATE)
  (SET-YEAR YEAR))

;;; 3-15 means March 15, year defaults.  Same European problem.
(DEFPATTERN MAIN ((FIXP 1 2) - (FIXP 1 2)) MAIN
	    (MONTH DATE)
  (SET-MONTH-AND-DATE MONTH DATE))

;;; 3-Jan-80 means Jan 3, 1980.
(DEFPATTERN MAIN ((FIXP 1 2) - (GET MONTH) - (FIXP 2)) MAIN
	    (DATE MONTH YEAR)
  (SET-DATE DATE)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-YEAR-OF-CENTURY YEAR))

;;; 3-Jan-1980 means Jan 3, 1980.
(DEFPATTERN MAIN ((FIXP 1 2) - (GET MONTH) - (FIXP 4)) MAIN
	    (DATE MONTH YEAR)
  (SET-DATE DATE)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-YEAR YEAR))

;;; Jan-3-80 means Jan 3, 1980.
(DEFPATTERN MAIN ((GET MONTH) - (FIXP 1 2) - (FIXP 2)) MAIN
	    (MONTH DATE YEAR)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-DATE DATE)
  (SET-YEAR-OF-CENTURY YEAR))

;;; Jan-3-1980 means Jan 3, 1980.
(DEFPATTERN MAIN ((GET MONTH) - (FIXP 1 2) - (FIXP 4)) MAIN
	    (MONTH DATE YEAR)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-DATE DATE)
  (SET-YEAR YEAR))

;;; 1130.4 means 11 hours and 30.4 minutes, in Multics internal headers,
;;; which Zmail sometimes actually sees.  (I think this happens when
;;; a QSEND from Multics turns into mail.)
(DEFPATTERN MAIN ((FIXP 4) /. (FIXP 1)) MAIN
	    (HHMM TENTHS-OF-MINUTES)
  (SET-HHMM HHMM)
  (SET-TENTHS-OF-MINUTE TENTHS-OF-MINUTES))

;;; 1130. means 11 hours and 30 minutes and zero seconds.
(DEFPATTERN MAIN ((FIXP 4) /.) MAIN
	    (HHMM)
  (SET-HHMM HHMM))

;;; 1130 means 11 hours and 30 minutes and zero seconds.
(DEFPATTERN MAIN ((FIXP 4)) MAIN
	    (HHMM)
  (SET-HHMM HHMM))

;;; 113015 means 11 hours, 30 minutes and 15 seconds.
(DEFPATTERN MAIN ((FIXP 6)) MAIN
	    (HHMMSS)
  (SET-HHMMSS HHMMSS))

;;; 11:30 means 11 hours and 30 minutes, go look for seconds.
;;; 11.30 works too; this is a European form.
(DEFPATTERN MAIN ((FIXP 1 2) (ANY-OF /: /.) (FIXP 2)) SECOND
	    (HOUR IGNORE MINUTE)
  (SET-HOUR HOUR)
  (SET-MINUTE MINUTE))

;;; Ed says that Agatha Christie books use 11.3 to mean 11:30:00, also.
(DEFPATTERN MAIN ((FIXP 1 2) /. (FIXP 1)) SECOND
	    (HOUR TENS-OF-MINUTES)
  (SET-HOUR HOUR)
  (SET-TENS-OF-MINUTES TENS-OF-MINUTES))

;;; Looking for seconds, :23 means 23 seconds, look for AM/PM.
;;; .23 works too; this is a European form.
(DEFPATTERN SECOND ((ANY-OF /: /.) (FIXP 1 2)) MERIDIAN
	    (IGNORE SECOND)
  (SET-SECOND SECOND))

;;; Looking for seconds, not finding them, look for AM/PM.
(DEFPATTERN SECOND () MERIDIAN
	    ()
  (SET-SECOND '(0 2)))

;;; Looking for meridian, AM means AM and PM means PM, go back to main state.
(DEFPATTERN MERIDIAN ((GET MERIDIAN)) MAIN
	    (MERIDIAN)
  (SET-MERIDIAN MERIDIAN))

;;; Looking for meridian, not finding it, go back to main state.
(DEFPATTERN MERIDIAN () MAIN
	    ()
   )

;;; 4 PM means what you would think.
(DEFPATTERN MAIN ((FIXP 1 2) (GET MERIDIAN)) MAIN
	    (HOUR MERIDIAN)
  (SET-HOUR HOUR)
  (SET-MERIDIAN MERIDIAN)
  (SET-MINUTE '(0 2))
  (SET-SECOND '(0 2)))

;;; 12 Noon and friends.
(DEFPATTERN MAIN (12. (GET HALF-DAY)) MAIN
	    (HALF-DAY)
  (SET-HALF-DAY HALF-DAY))

;;; Noon and friends.
(DEFPATTERN MAIN ((GET HALF-DAY)) MAIN
	    (HALF-DAY)
  (SET-HALF-DAY HALF-DAY))

;;; Day of the week, as in "Friday, Jan 5"
(DEFPATTERN MAIN ((GET DAY-OF-THE-WEEK) /,) MAIN
	    (DAY-OF-THE-WEEK)
  (SET-DAY-OF-THE-WEEK DAY-OF-THE-WEEK))

;;; Day of the week.
(DEFPATTERN MAIN ((GET DAY-OF-THE-WEEK)) MAIN
	    (DAY-OF-THE-WEEK)
  (SET-DAY-OF-THE-WEEK DAY-OF-THE-WEEK))

;;; These patterns inserted by CAL 10/24/80

;;; "today"
(DEFPATTERN MAIN (TODAY) MAIN
	    ()
	    (SET-TODAY))

;;; "yesterday"
(DEFPATTERN MAIN (YESTERDAY) MAIN
	    ()
	    (SET-YESTERDAY))

;;; "tomorrow"
(DEFPATTERN MAIN (TOMORROW) MAIN
	    ()
	    (SET-TOMORROW))

;;; "now"
(DEFPATTERN MAIN (NOW) MAIN
	    ()
	    (SET-NOW))

;;; "2 days before jan 30"
(DEFPATTERN MAIN ((FIXP) (GET OFFSET) BEFORE) MAIN
	    (OFFSET-VALUE OFFSET-UNITS)
	    (SET-OFFSET '- OFFSET-VALUE OFFSET-UNITS))

;;; "The day before yesterday" or "day before yesterday"
(DEFPATTERN MAIN ((GET OFFSET) BEFORE) MAIN
	    (OFFSET-UNITS)
	    (SET-OFFSET '- '(1 1) OFFSET-UNITS))

;;; "2 days after jan 15"
(DEFPATTERN MAIN ((FIXP) (GET OFFSET) AFTER) MAIN
	    (OFFSET-VALUE OFFSET-UNITS)
	    (SET-OFFSET '+ OFFSET-VALUE OFFSET-UNITS))

;;; "The day after jan 15", "day after tomorrow"
(DEFPATTERN MAIN ((GET OFFSET) AFTER) MAIN
	    (OFFSET-UNITS)
	    (SET-OFFSET '+ '(1 1) OFFSET-UNITS))

;;; "5 minutes from now"
(DEFPATTERN MAIN ((FIXP) (GET OFFSET) FROM) MAIN
	    (OFFSET-VALUE OFFSET-UNITS)
	    (SET-OFFSET '+ OFFSET-VALUE OFFSET-UNITS))

;;; "3 days ago"
(DEFPATTERN MAIN ((FIXP) (GET OFFSET) AGO) MAIN
	    (OFFSET-VALUE OFFSET-UNITS)
	    (SET-NOW)
	    (SET-OFFSET '- OFFSET-VALUE OFFSET-UNITS))

;;; "dlw's birthday"
(DEFPATTERN MAIN ((ANY) S BIRTHDAY) MAIN
	    (NAME)
	    (SET-BIRTHDAY NAME))

;;; "my birthday"
(DEFPATTERN MAIN (MY BIRTHDAY) MAIN
	    ()
	    (SET-BIRTHDAY USER-ID))

;;; - 3 minutes
(DEFPATTERN MAIN ((GET SIGN) (FIXP) (GET OFFSET)) MAIN
	    (SIGN OFFSET-VALUE OFFSET-UNITS)
  (SET-OFFSET SIGN OFFSET-VALUE OFFSET-UNITS))

;;; 3 minutes
(DEFPATTERN MAIN ((FIXP) (GET OFFSET)) MAIN
	    (OFFSET-VALUE OFFSET-UNITS)
  (SET-OFFSET '+ OFFSET-VALUE OFFSET-UNITS))

;;; Time zones
(DEFPATTERN MAIN ((GET TIME-ZONE)) MAIN
	    (TIME-ZONE)
  (SET-TIME-ZONE TIME-ZONE))

;;; Time zones preceeded by a hyphen
(DEFPATTERN MAIN (- (GET TIME-ZONE)) MAIN
	    (TIME-ZONE)
  (SET-TIME-ZONE TIME-ZONE))

;;; If we encounter random commas in MAIN state, we have to just ignore them
;;; in order to win in such cases as "Thursday, 21 May 1981, 00:27-EDT"
(DEFPATTERN MAIN (/,) MAIN
	    ()
  )

;;; Anything else
(DEFPATTERN MAIN ((ANY)) MAIN
	    (TOKEN)
  (BARF "Unrecognized date//time format, starting with token ~A." TOKEN))

;;; If nothing is left and we are in MAIN state, that is the end.
(PUTPROP 'MAIN 'T 'FINAL-STATE)

;;; We just saw "Jan 23", look for a comma followed by a year, e.g. "Jan 23, 80"
(DEFPATTERN YEAR-COMMA (/, (FIXP 2)) MAIN
	    (YEAR-OF-CENTURY)
  (SET-YEAR-OF-CENTURY YEAR-OF-CENTURY))

;;; We just saw "Jan 23", look for a comma followed by a year, e.g. "Jan 23, 1980"
(DEFPATTERN YEAR-COMMA (/, (FIXP 4)) MAIN
	    (YEAR)
  (SET-YEAR YEAR))

;;; If there isn't a comma, go look for the regular kinds of years.
(DEFPATTERN YEAR-COMMA () YEAR
	    ()
  )

;;; We are now in the state of looking for a year.  If we see a number,
;;; that may be a year or it may be the start of something else.  For
;;; example, "6 Jan 59" versus "6 Jan 59 minutes" or "6 Jan 3:23:12".
;;; So we have to look ahead for various possibilities and return to
;;; the main state if any of them are happening.  Otherwise, a number
;;; gets interpreted as a year in this context.
(DEFPATTERN-PEEK YEAR ((FIXP) /.) MAIN
		 ()
  )

(DEFPATTERN-PEEK YEAR ((FIXP) //) MAIN
		 ()
  )

(DEFPATTERN-PEEK YEAR ((FIXP) /:) MAIN
		 ()
  )

(DEFPATTERN-PEEK YEAR ((FIXP) (GET MERIDIAN)) MAIN
		 ()
  )

(DEFPATTERN-PEEK YEAR (12. (GET HALF-DAY)) MAIN
		 ()
  )

(DEFPATTERN-PEEK YEAR ((GET SIGN) (FIXP) (GET OFFSET)) MAIN
		 ()
  )

(DEFPATTERN-PEEK YEAR ((FIXP) (GET OFFSET)) MAIN
		 ()
  )

(DEFPATTERN-PEEK YEAR ((FIXP) (GET MONTH)) MAIN
		 ()
  (BARF "Date and month seen where year expected."))

;;; Finally, there is no other way to interpret the number.  If there
;;; is a number it must be a year.
(DEFPATTERN YEAR ((FIXP)) MAIN
	    (YEAR)
  (SET-YEAR YEAR))

;;; Not a number at all.
(DEFPATTERN YEAR () MAIN
	    ()
  )

;;; This is the end of the patterns.  Don't add new ones after this!
(FINISH-PATTERNS)

;;; Special variables.

;;; These variables hold the time values found in the string.  NIL means
;;; that no such value has been seen yet.

;;; Absolute values.
(DEFVAR *ABS-YEAR*)
(DEFVAR *ABS-MONTH*)
(DEFVAR *ABS-DATE*)
(DEFVAR *ABS-HOUR*)
(DEFVAR *ABS-MINUTE*)
(DEFVAR *ABS-SECOND*)
(DEFVAR *ABS-DAY-OF-THE-WEEK*)
(DEFVAR *ABS-TIME-ZONE*)

;;; Relative values, from offsets.
(DEFVAR *REL-YEAR*)
(DEFVAR *REL-MONTH*)
(DEFVAR *REL-DATE*)
(DEFVAR *REL-HOUR*)
(DEFVAR *REL-MINUTE*)
(DEFVAR *REL-SECOND*)
(DEFVAR *REL-DAY-OF-THE-WEEK*)
;(DEFVAR *REL-TIME-ZONE*)

;;; Values of the "base" time.
(DEFVAR *BASE-YEAR*)
(DEFVAR *BASE-MONTH*)
(DEFVAR *BASE-DATE*)
(DEFVAR *BASE-HOUR*)
(DEFVAR *BASE-MINUTE*)
(DEFVAR *BASE-SECOND*)

(DEFVAR *RELATIVE-P*)

;;; Action functions.

;;; These are the functions invoked by the bodies of the DEFPATTERNs.

(DEFUN SET-MONTH-FROM-NAME (MONTH)
  (IF (NOT (NULL *ABS-MONTH*))
      (BARF "Month specified twice."))
  (SETQ *ABS-MONTH* (GET MONTH 'VALUE)))

(DEFUN SET-MONTH (MONTH)
  (IF (NOT (NULL *ABS-MONTH*))
      (BARF "Month specified twice."))
  (SETQ *ABS-MONTH* (NUMBER-VALUE MONTH)))

(DEFUN SET-DATE (DATE)
  (IF (NOT (NULL *ABS-DATE*))
      (BARF "Date specified twice."))
  (SETQ *ABS-DATE* (NUMBER-VALUE DATE)))

;;; Here we have to deal with the incompatibility betweeen U.S. and European
;;; date format.  If either number is greater than 12., then that number
;;; cannot be the month and so must be the date.  Otherwise, default based
;;; on the location of the machine.
(DEFUN SET-MONTH-AND-DATE (FIRST SECOND)
  (SETQ FIRST (NUMBER-VALUE FIRST) SECOND (NUMBER-VALUE SECOND))
  (COND ((> FIRST 12.)
	 (SETQ *ABS-MONTH* SECOND *ABS-DATE* FIRST))
	((> SECOND 12.)
	 (SETQ *ABS-MONTH* FIRST *ABS-DATE* SECOND))
	((MEMQ *TIMEZONE* '(1 0 -1 -2 -3 -4 -5))
	 ;; Europe, kind of.  (Soneone should check a map, and find out
	 ;; how the Soviet write dates, when we enter that market...)
	 (SETQ *ABS-MONTH* SECOND *ABS-DATE* FIRST))
	(T
	 ;; Patriotic American date format.
	 (SETQ *ABS-MONTH* FIRST *ABS-DATE* SECOND))))

;;; This version takes a fixnum, rather than a two-list.
(DEFUN SET-YEAR-INTERNAL (YEAR)
  (IF (NOT (NULL *ABS-YEAR*))
      (BARF "Year specified twice."))
  (SETQ *ABS-YEAR* YEAR))

(DEFUN SET-YEAR (YEAR)
  (SET-YEAR-INTERNAL (NUMBER-VALUE YEAR)))

(DEFUN SET-YEAR-OF-CENTURY (YEAR-OF-CENTURY)
  (SET-YEAR-INTERNAL
    (+ (NUMBER-VALUE YEAR-OF-CENTURY)
       (* 100. (// *BASE-YEAR* 100.)))))	; Multics crockishly assumes 1900.

(DEFUN SET-HHMM (TIME)
  (IF (NOT (NULL *ABS-HOUR*))
      (BARF "Hour specified twice."))
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice."))
  (SETQ TIME (NUMBER-VALUE TIME)
	*ABS-HOUR* (// TIME 100.)
	*ABS-MINUTE* (\ TIME 100.)))

(DEFUN SET-HHMMSS (TIME)
  (IF (NOT (NULL *ABS-HOUR*))
      (BARF "Hour specified twice."))
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice."))
  (IF (NOT (NULL *ABS-SECOND*))
      (BARF "Second specified twice."))
  (SETQ TIME (NUMBER-VALUE TIME)
	*ABS-HOUR* (// TIME 10000.)
	TIME (- TIME (* *ABS-HOUR* 10000.))
	*ABS-MINUTE* (// TIME 100.)
	*ABS-SECOND* (\ TIME 100.)))

(DEFUN SET-HOUR (HOUR)
  (IF (NOT (NULL *ABS-HOUR*))
      (BARF "Hour specified twice."))
  (SETQ *ABS-HOUR* (NUMBER-VALUE HOUR)))

(DEFUN SET-MINUTE (MINUTE)
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice."))
  (SETQ *ABS-MINUTE* (NUMBER-VALUE MINUTE)))

(DEFUN SET-TENS-OF-MINUTES (TENS-OF-MINUTES)
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice."))
  (SETQ *ABS-MINUTE* (* 10. (NUMBER-VALUE TENS-OF-MINUTES))))

(DEFUN SET-SECOND (SECOND)
  (IF (NOT (NULL *ABS-SECOND*))
      (BARF "Second specified twice."))
  (SETQ *ABS-SECOND* (NUMBER-VALUE SECOND)))

(DEFUN SET-TENTHS-OF-MINUTE (TENTHS)
  (IF (NOT (NULL *ABS-SECOND*))
      (BARF "Second specified twice."))
  (SETQ *ABS-SECOND* (* 6 (NUMBER-VALUE TENTHS))))

(DEFUN SET-MERIDIAN (MERIDIAN)
  (IF (OR (NOT (FIXP *ABS-HOUR*))
	  (< *ABS-HOUR* 0.)
	  (> *ABS-HOUR* 12.))
      (BARF "Meridian value ~A seen in bad context." MERIDIAN))
  (SETQ *ABS-HOUR*
	(IF (EQ (GET MERIDIAN 'VALUE) 'PM)
	    (IF (= *ABS-HOUR* 12.) 12. (+ *ABS-HOUR* 12.))
	    (IF (= *ABS-HOUR* 12.) 0 *ABS-HOUR*))))

(DEFUN SET-HALF-DAY (HALF-DAY)
  (IF (NOT (NULL *ABS-SECOND*))
      (BARF "Second specified twice, by the half-day value /"~A/"." HALF-DAY))
  (IF (NOT (NULL *ABS-HOUR*))
      (BARF "Hour specified twice, by the half-day value /"~A/"." HALF-DAY))
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice, by the half-day value /"~A/"." HALF-DAY))
  (SETQ *ABS-HOUR* (IF (EQ (GET HALF-DAY 'VALUE) 'NOON)
		       12.
		       0.)
	*ABS-MINUTE* 0
	*ABS-SECOND* 0))

(DEFUN SET-DAY-OF-THE-WEEK (DAY-OF-THE-WEEK)
  (IF (NOT (NULL *ABS-DAY-OF-THE-WEEK*))
      (BARF "Day of the week specified twice."))
  (SETQ *ABS-DAY-OF-THE-WEEK* (GET DAY-OF-THE-WEEK 'VALUE)))

(DEFUN SET-TIME-ZONE (TIME-ZONE)
  (IF (NOT (NULL *ABS-TIME-ZONE*))
      (BARF "Time zone specified twice."))
  (SETQ *ABS-TIME-ZONE* (GET TIME-ZONE 'ZONE-VALUE)))

(DEFUN SET-OFFSET (SIGN VALUE UNITS)
  (LET ((VALUE (* (NUMBER-VALUE VALUE) (IF (EQ SIGN '+) 1. -1.))))
    (SELECTQ (GET UNITS 'OFFSET-VALUE)
      (YEARS (SETQ *REL-YEAR* (+ *REL-YEAR* VALUE)))
      (MONTHS (SETQ *REL-MONTH* (+ *REL-MONTH* VALUE)))
      (WEEKS (SETQ *REL-DATE* (+ *REL-DATE* (* 7 VALUE))))
      (DAYS (SETQ *REL-DATE* (+ *REL-DATE* VALUE)))
      (HOURS (SETQ *REL-HOUR* (+ *REL-HOUR* VALUE)))
      (MINUTES (SETQ *REL-MINUTE* (+ *REL-MINUTE* VALUE)))
      (SECONDS (SETQ *REL-SECOND* (+ *REL-SECOND* VALUE)))
      (OTHERWISE (BARF "Bad units" UNITS)))))

(DEFUN SET-TODAY ()
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* ':RELATIVE))

(DEFUN SET-YESTERDAY ()
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *REL-DATE* (1- *REL-DATE*))
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* ':RELATIVE))

(DEFUN SET-TOMORROW ()
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *REL-DATE* (1+ *REL-DATE*))
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* ':RELATIVE))

(DEFUN SET-NOW ()
  (SETQ *ABS-SECOND* *BASE-SECOND*)
  (SETQ *ABS-MINUTE* *BASE-MINUTE*)
  (SETQ *ABS-HOUR* *BASE-HOUR*)
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* ':RELATIVE))

(DEFUN SET-BIRTHDAY (USER-ID)
  (PARSE-1 (LEXICALLY-ANALYZE
	     (FIND-BIRTHDAY (WITH-OUTPUT-TO-STRING (STANDARD-OUTPUT)
			       (CHAOS:FINGER (STRING-APPEND USER-ID "//w")))))
	   'MAIN))
  

;;; Top level.

;;; These are the top level functions and external entrypoints that call
;;; the lexical analyzer and parser; the parser calls the action routines.
;;; Any of these callees may call BARF to report an error; BARF is guaranteed
;;; to THROW out, and therefore not return to its caller.

(DEFMACRO CHECK-RANGE (VARIABLE LOWER UPPER STRING)
  `(IF (OR (< ,VARIABLE ,LOWER)
	   (> ,VARIABLE ,UPPER))
       (BARF "~D is ~:[more~;less~] than the number of ~A."
	     ,VARIABLE (< ,VARIABLE ,LOWER) ,STRING)))

(DEFUN PARSE (STRING &OPTIONAL (START 0) END (FUTUREP T)
	      BASE-TIME MUST-HAVE-TIME DATE-MUST-HAVE-YEAR
	      TIME-MUST-HAVE-SECOND (DAY-MUST-BE-VALID T))
  (DECLARE (RETURN-LIST SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P
			RELATIVE-P))
  (MULTIPLE-VALUE-BIND (ANSWER RELATIVE-P)
      (PARSE-UNIVERSAL-TIME STRING START END FUTUREP BASE-TIME MUST-HAVE-TIME
			    DATE-MUST-HAVE-YEAR TIME-MUST-HAVE-SECOND DAY-MUST-BE-VALID)
    (COND ((STRINGP ANSWER)
	   ANSWER)
	  (T
	   (MULTIPLE-VALUE-BIND (SECS MINUTES HOURS DAY MONTH
				 YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P)
	       (DECODE-UNIVERSAL-TIME ANSWER)
	     (VALUES SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P
		     RELATIVE-P))))))

(DEFUN PARSE-UNIVERSAL-TIME (STRING &OPTIONAL (START 0) END (FUTUREP T) BASE-TIME
			                MUST-HAVE-TIME DATE-MUST-HAVE-YEAR
					TIME-MUST-HAVE-SECOND (DAY-MUST-BE-VALID T))
  (DECLARE (RETURN-LIST UNIVERSAL-TIME RELATIVE-P))
  (IF (AND MUST-HAVE-TIME (EQ STRING ""))
      (BARF "no time supplied"))
  (IF (NULL END)
      (SETQ END (STRING-LENGTH STRING)))
  (PROG KLUDGE ()				;This is needed because multiple values
   (RETURN					;are not propagated through a *CATCH.
     (*CATCH 'TIME-ERROR
       (LET (*ABS-YEAR* *ABS-MONTH* *ABS-DATE* *ABS-HOUR* *ABS-MINUTE* *ABS-SECOND*
	     *ABS-DAY-OF-THE-WEEK* *ABS-TIME-ZONE*
	     (*REL-YEAR* 0) (*REL-MONTH* 0) (*REL-DATE* 0) (*REL-HOUR* 0) (*REL-MINUTE* 0)
	     (*REL-SECOND* 0) *REL-DAY-OF-THE-WEEK*
;	     *REL-TIME-ZONE*
	     *BASE-YEAR* *BASE-MONTH* *BASE-DATE* *BASE-HOUR* *BASE-MINUTE* *BASE-SECOND*
	     *RELATIVE-P*)
  
	 ;; Compute the "base" time: the time to which the string is relative.
	 (COND ((NULL BASE-TIME)
		;; Time is relative to right now.
		(MULTIPLE-VALUE (*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR* *BASE-DATE*
				 *BASE-MONTH* *BASE-YEAR*)
		  (GET-TIME))
		;; If the time is not known, assume a default base time so that we
		;; can still parse fully-specified date/times (e.g. in the file system)
		(IF (NULL *BASE-SECOND*)
		    (SETQ *BASE-SECOND* 0 *BASE-MINUTE* 0 *BASE-HOUR* 0
			  *BASE-DATE* 1 *BASE-MONTH* 1 *BASE-YEAR* 0)))
	       (T
		;; Time is relative to a specified time.
		(MULTIPLE-VALUE (*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR*
				 *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)
		  (DECODE-UNIVERSAL-TIME  BASE-TIME))))
	 (SETQ *BASE-YEAR* (+ 1900. *BASE-YEAR*))
	 
	 ;; Do the parse, calling the action routines, which work by setting the
	 ;; ABS and REL special variables bound above.
	 (PARSE-1 (DELQ-ALL (LEXICALLY-ANALYZE STRING START END) *NOISE-WORDS*) 'MAIN)
	 (IF (AND DATE-MUST-HAVE-YEAR (NULL *ABS-YEAR*))
	     (BARF "no year supplied"))
	 (IF (AND TIME-MUST-HAVE-SECOND (NULL *ABS-SECOND*))
	     (BARF "no seconds supplied"))
	 
	 ;; Now apply lots of defaults.
	 
	 ;; There are many terms, from the lowest order (seconds) to the highest
	 ;; order (years).  A legal date must specify some contiguous subsequence
	 ;; of these.  The low unspecified ones get zeroed; the high unspecified
	 ;; ones are either the next in the future or the previous in the past.
	 ;; Time zones and days of the week are handled specially.
	 
	 ;; First, the following code allows a day of the week to be used to
	 ;; specify a year, month, and date, when it is supposed to.
	 (IF (AND (NULL *ABS-YEAR*)
		  (NULL *ABS-MONTH*)
		  (NULL *ABS-DATE*)
		  (NOT (NULL *ABS-DAY-OF-THE-WEEK*)))
	     ;; Day of week specified the year, month, and date.
	     (LET ((UT (ENCODE-UNIVERSAL-TIME 0 0 0 *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)))
	       (MULTIPLE-VALUE-BIND (NIL NIL NIL NIL NIL NIL BASE-DAY-OF-THE-WEEK)
		   (DECODE-UNIVERSAL-TIME UT)
		 (LET ((DELTA-DAYS (- *ABS-DAY-OF-THE-WEEK* BASE-DAY-OF-THE-WEEK)))
		   (IF FUTUREP
		       (DO () ((> DELTA-DAYS 0))
			 (SETQ DELTA-DAYS (+ DELTA-DAYS 7)))
		       (DO () ((< DELTA-DAYS 0))
			 (SETQ DELTA-DAYS (- DELTA-DAYS 7))))
		   (MULTIPLE-VALUE (NIL NIL NIL *ABS-DATE* *ABS-MONTH* *ABS-YEAR*)
		     (COMPUTE-RELATIVE 0 0 0 (+ *BASE-DATE* DELTA-DAYS)
				       *BASE-MONTH* *BASE-YEAR*))))))
	 
	 ;; Non-specified low-order terms get set to zero (or the moral equivalent
	 ;; of zero), up to the first speicified term.
	 (DO ((TERMS '(*ABS-SECOND* *ABS-MINUTE* *ABS-HOUR*
				    *ABS-DATE* *ABS-MONTH* *ABS-YEAR*) (CDR TERMS))
	      (BASE-TERMS '(*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR*
					  *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)
			  (CDR BASE-TERMS))
	      (LOWEST '(0 0 0 1 1 -100000000) (CDR LOWEST))
	      (HIGHEST '(59. 59. 23. NIL 12. 100000000) (CDR HIGHEST))
	      (STATE 'DEFAULT-LOW-TERMS)
	      (COMPARISON 'EQUAL)
	      (OPERATION NIL))
	     ((NULL TERMS)
	      (IF (EQ STATE 'DEFAULT-LOW-TERMS)
		  (BARF "No time was specified.")))
	   RESTART
	   (LET ((TERM-VALUE (SYMEVAL (CAR TERMS)))
		 (BASE-TERM-VALUE (SYMEVAL (CAR BASE-TERMS))))
	     (SELECTQ STATE
	       (DEFAULT-LOW-TERMS
		;; Non-specified low-order terms get set to default values, which
		;; are zero or one depending on whether the quantity is zero-based
		;; or one-based.
		(COND ((NULL TERM-VALUE)
		       ;; Term is non-specified, default it.
		       (SET (CAR TERMS) (CAR LOWEST)))
		      (T
		       ;; Term is specified: go to the next state and try again.
		       (SETQ STATE 'SKIP-OVER-SPECIFIED)
		       (GO RESTART))))
	       (SKIP-OVER-SPECIFIED
		;; Now we are moving over the contiguous subsequence of values
		;; specified by the user.
		(COND ((NOT (NULL TERM-VALUE))
		       ;; This value was specified by the user.
		       (COND ((> TERM-VALUE BASE-TERM-VALUE)
			      ;; Specified time is later than the base time.
			      (SETQ COMPARISON 'LATER))
			     ((< TERM-VALUE BASE-TERM-VALUE)
			      ;; Specified time is earlier than the base time.
			      (SETQ COMPARISON 'EARLIER))
			     ;; If these terms are equal, use the old value of
			     ;;   COMPARISON based on the lower order terms.
			     ))
		      (T
		       ;; Term is not specified; go to the next state and try again.
		       ;; This SETQ is documented at the next state.
		       (SETQ OPERATION
			     (SELECTQ COMPARISON
			       (EQUAL
				;; The specified and base times are equal, do nothing.
				'EQUAL)
			       (LATER
				;; Specified time is later than base time.
				(IF FUTUREP 'EQUAL 'SUB1))
			       (EARLIER
				;; Specified time is earlier than base time.
				(IF FUTUREP 'ADD1 'EQUAL))))
		       (SETQ STATE 'DEFAULT-HIGH-TERMS)
		       (GO RESTART))))
	       (DEFAULT-HIGH-TERMS
		;; Non-specified high-order terms come from the base time.  The
		;; tricky thing is that we may have to add or subtract one, depending
		;; on FUTUREP and COMPARISON, which requires propagating carry or
		;; borrow.  This information is encoded in OPERATION, which is SETQed
		;; above (so that we don't do it each time around the loop!).
		(IF (NOT (NULL TERM-VALUE))
		    ;; Foo, the rest of the high-order terms have to be unspecified.
		    (BARF "Unrecognized pattern of defaulting."))
		(SELECTQ OPERATION
		  (EQUAL
		   ;; We are just copying base time into abs time.  Keep doing it.
		   (SET (CAR TERMS) BASE-TERM-VALUE))
		  (ADD1
		   ;; Set this term one higher than it is in the base time.
		   (LET ((HIGHEST-VALUE
			   ;; Compute the highest legal value for this term.
			   (IF (EQ (CAR TERMS) '*ABS-DATE*)
			       ;; Highest possible value for dates depends on
			       ;; which month this is.
			       (MONTH-LENGTH *BASE-MONTH* *BASE-YEAR*)
			       ;; Other highest values are just constants.
			       (CAR HIGHEST))))
		     (COND ((< BASE-TERM-VALUE HIGHEST-VALUE)
			    ;; No carry.  Just add one, and copy the rest.
			    (SET (CAR TERMS) (1+ BASE-TERM-VALUE))
			    (SETQ OPERATION 'EQUAL))
			   (T
			    ;; Carry into next term.
			    (SET (CAR TERMS) (CAR LOWEST))))))
		  (SUB1
		   ;; Set this term one lower than it is in the base time.
		   (COND ((> BASE-TERM-VALUE (CAR LOWEST))
			  ;; No borrow.  Just subtract one, and copy the rest.
			  (SET (CAR TERMS) (1- BASE-TERM-VALUE))
			  (SETQ OPERATION 'EQUAL))
			 (T
			  ;; Borrow from the next term.
			  (SET (CAR TERMS)
			       (IF (EQ (CAR TERMS) '*ABS-DATE*)
				   ;; Highest possible value for dates depends on
				   ;; which month this is.
				   (MONTH-LENGTH *BASE-MONTH* *BASE-YEAR*)
				   ;; Other highest values are just constants.
				   (CAR HIGHEST))))))
		  (OTHERWISE
		   (FERROR NIL "Bad value of OPERATION ~S" OPERATION))))
	       (OTHERWISE
		(FERROR NIL "Bad value of STATE ~S" STATE)))))
	 
	 ;; Now hack other random defaults.
;	 (IF (NULL *ABS-TIME-ZONE*)
;	     (SETQ *ABS-TIME-ZONE* *TIMEZONE*))
;	 (SETQ *REL-TIME-ZONE* *ABS-TIME-ZONE*)
	 
	 ;; Check ranges.
	 (CHECK-RANGE *ABS-SECOND* 0 59. "seconds in a minute")
	 (CHECK-RANGE *ABS-MINUTE* 0 59. "minutes in an hour")
	 (CHECK-RANGE *ABS-HOUR* 0 23. "hours in a day")
						;Check this before MONTH-STRING call!
	 (CHECK-RANGE *ABS-MONTH* 1 12. "months in a year")
	 (CHECK-RANGE *ABS-DATE*
		      1
		      (MONTH-LENGTH *ABS-MONTH* *ABS-YEAR*)
		      (FORMAT NIL "days in ~A" (MONTH-STRING *ABS-MONTH*)))
	 (IF (AND DAY-MUST-BE-VALID (NOT (NULL *ABS-DAY-OF-THE-WEEK*)))
	     (VERIFY-DATE *ABS-DATE* *ABS-MONTH* *ABS-YEAR* *ABS-DAY-OF-THE-WEEK*))
	 
	 ;; Now put it together.
	 (MULTIPLE-VALUE (*ABS-SECOND* *ABS-MINUTE* *ABS-HOUR* *ABS-DATE* *ABS-MONTH* *ABS-YEAR*)
	   (COMPUTE-RELATIVE (+ *ABS-SECOND* *REL-SECOND*)
			     (+ *ABS-MINUTE* *REL-MINUTE*)
			     (+ *ABS-HOUR* *REL-HOUR*)
			     (+ *ABS-DATE* *REL-DATE*)
			     (+ *ABS-MONTH* *REL-MONTH*)
			     (+ *ABS-YEAR* *REL-YEAR*)))
	 (RETURN (ENCODE-UNIVERSAL-TIME *ABS-SECOND* *ABS-MINUTE* *ABS-HOUR*
					*ABS-DATE* *ABS-MONTH* *ABS-YEAR* *ABS-TIME-ZONE*)
		 *RELATIVE-P*))))))

;;; This function will accept dates such as -1,March 1980 and return 28,Febuary 1980
;;; CAL 10/24/80

(DEFUN COMPUTE-RELATIVE (SECOND MINUTE HOUR DATE MONTH YEAR)
  (PROG (M)
    (SETQ SECOND (+ SECOND (* 60 (+ MINUTE (* 60 (+ HOUR (* 24 DATE)))))))
    (SETQ DATE (FIX (// (FLOAT SECOND) 86400)))
    (SETQ SECOND (- SECOND (* DATE 86400)))
    (SETQ HOUR (FIX (// SECOND 3600)))
    (SETQ SECOND (\ SECOND 3600))
    (SETQ MINUTE (FIX (// SECOND 60)))
    (SETQ SECOND (\ SECOND 60))
    (SETQ YEAR (+ YEAR (FIX (// (FLOAT (1- MONTH)) 12))))
    (SETQ MONTH (1+ (\ (+ 12 (\ (1- MONTH) 12)) 12)))
 L1 (SETQ M (MONTH-LENGTH MONTH YEAR))
    (COND ((> DATE M)
	   (SETQ DATE (- DATE M))
	   (SETQ MONTH (1+ MONTH))
	   (COND ((> MONTH 12) (SETQ MONTH 1) (SETQ YEAR (1+ YEAR))))
	   (GO L1))
	  ((< DATE 1)				
	   (SETQ MONTH (1- MONTH))
	   (COND ((= MONTH 0) (SETQ MONTH 12) (SETQ YEAR (1- YEAR))))
	   (SETQ DATE (+ (MONTH-LENGTH MONTH YEAR) DATE))
	   (GO L1)))       
    (RETURN SECOND MINUTE HOUR DATE MONTH YEAR)))

(DEFUN FIND-BIRTHDAY (STRING &AUX X)
  (SETQ X (STRING-SEARCH "birthday" STRING))
  (IF (NULL X) (BARF "CANNOT FIND BIRTHDAY"))
  (SUBSTRING STRING (+ 9 X) (STRING-SEARCH ";" STRING (+ 9 X))))

(DEFUN BARF (STRING &REST ARGS)
  (*THROW 'TIME-ERROR (LEXPR-FUNCALL #'FORMAT NIL STRING ARGS)))

(DEFUN TEST ()
  (DO ((S (READLINE) (READLINE))
       (NOW (GET-UNIVERSAL-TIME)))
      ((EQUAL S ""))
    (LET ((ANS (PARSE-UNIVERSAL-TIME S 0 NIL T NOW)))
      (IF (STRINGP ANS)
	  (PRINC ANS)
	  (PRINT-UNIVERSAL-TIME ANS)))
    (TERPRI) (TERPRI)))

;;; This function should be run whenever you make a major change to the
;;; parser.  It has an exhaustive set of test cases, all of which should
;;; be verified.
(DEFCONST *TEST-CASES*
	'("March 15, 1960" "15 March 1960" "3//15//60" "15//3//60" "3//15//1960"
	  "3-15-60" "15-3-1960" "3-15" "3-March-60" "3-Mar-60" "March-3-60"
	  "1130." "11:30" "11:30:17" "11:30 pm" "11:30 AM" "1130" "113000"
	  "11.30" "11.30.00" "11.3" "11 pm" "12 noon"
	  "midnight" "m" "Friday, March 15, 1980" "6:00 gmt" "3:00 pdt"
	  "15 March 60" "15 march 60 seconds"
	  "Fifteen March 60" "The Fifteenth of March, 1960;"
	  "Thursday, 21 May 1981, 00:27-EDT"
	  "One minute after March 3, 1960"
	  "Three days ago" "5 hours ago"
	  "Two days after March 3, 1960"
	  "Three minutes after 23:59:59 Dec 31, 1959"
	  "Now" "Today" "Yesterday" "two days after tomorrow"
	  "one day before yesterday" "the day after tomorrow"
	  "my birthday" "the day before my birthday"
	  "1 hour before dlw's birthday"
	  ))

(DEFUN TEST-PARSER ()
  (TERPRI)
  (DOLIST (CASE *TEST-CASES*)
    (FORMAT T "~40A   " CASE)
    (MULTIPLE-VALUE-BIND (ANS RELATIVE-P)
	(PARSE-UNIVERSAL-TIME CASE)
      (IF (STRINGP ANS)
	  (PRINC ANS)
	  (FORMAT T "~15A" (OR RELATIVE-P "Absolute"))
	  (PRINT-UNIVERSAL-TIME ANS)))
    (TERPRI)))

;;; Time interval stuff.
(defvar time-interval-array (make-array '(50. 2.) ':type ':art-q))

(defvar time-interval-unit-types 0)

(defun time-interval-to-seconds (string &aux (total 0))
  (do ((ix 0)
       (l (string-length string)))
      ((or (null ix) ( ix l)) total)

    (let ((token-start (string-search-not-char #\SP string ix)))
      (if (null token-start) (return total))
      (let* ((token-end (string-search-char #\SP string token-start))
	;;works even if end nil!
	     (units (zwei:parse-number string token-start token-end)))
	(if (null units)
	    (return nil
		    (format
		      nil "Invalid number: ~A" (substring string token-start token-end))))
	(let ((token-start (string-search-not-char #\SP string token-end)))
	  (if (null token-start) (return nil "Units specification missing from time string"))
	  (setq ix (string-search-char #\SP string token-start))
	  (let ((uval (loop for i from 0 below time-interval-unit-types
			    finally (return nil)
			    do
			    (if (string-equal
				  (aref time-interval-array i 0) string 0 token-start nil ix)
				(return (aref time-interval-array i 1))))))
	    (if uval
		(progn
		  (if (char-equal #/y (aref string token-start))	;years?
		      (if (> units 3)		;good till 1999.
			  (incf total (* (// units 4)
					 (time-interval-to-seconds "1 day")))))
		  (incf total (* uval units)))
		(return nil (format nil "Unknown time spec: ~A"
				    (substring string token-start ix))))))))))

(defun init-time-interval-array ()
  (aset "second" time-interval-array 0 0)
  (aset 1 time-interval-array 0 1)
  (setq time-interval-unit-types 1)
  (dolist (l '(("1 second" "seconds" "s" "sec" "secs")
	       ("60 seconds" "minute" "minutes" "min" "mins" "m")
	       ("60 minutes" "hour" "hours" "hr" "hrs" "h")
	       ("24 hours" "day" "days")
	       ("7 days" "week" "weeks" "wk" "wks")
	       ("365 days" "year" "years" "yr" "yrs")))
    (let ((value (time-interval-to-seconds (car l))))
      (dolist (newname (cdr l))
	(aset newname time-interval-array time-interval-unit-types 0)
	(aset value time-interval-array time-interval-unit-types 1)
	(incf time-interval-unit-types)))))

(init-time-interval-array)

(defun seconds-to-interval-string (secs)
  (if (zerop secs)
      "0 seconds"
      (do ((i 0 (1+ i))
	   (last nil))
	  (( i time-interval-unit-types)
	   (seconds-to-interval-string-1 last secs))
	(if (> (aref time-interval-array i 1) secs)
	    (return (seconds-to-interval-string-1 last secs))
	    (if (or (null last)
		    (not (= (aref time-interval-array i 1)
			    (aref time-interval-array last 1))))
		(setq last i))))))

(defvar *four-year-cycle* (time-interval-to-seconds "4 Years"))
(defvar *seconds-in-day* (time-interval-to-seconds "1 day"))

(defun seconds-to-interval-string-1 (index secs)
  (if (not (zerop (// secs *four-year-cycle*)))
      (decf secs (* (// secs *four-year-cycle*) *seconds-in-day*)))
  (let ((quo (// secs (aref time-interval-array index 1)))
	(rem (\ secs (aref time-interval-array index 1))))
    (if (zerop rem)
	(format nil "~D ~A~P" quo (aref time-interval-array index 0) quo)
	(format nil "~D ~A~P ~A" quo (aref time-interval-array index 0) quo
		(seconds-to-interval-string rem)))))

(defprop :time-interval-or-never (print-interval-or-never read-interval-or-never
				  nil nil nil
				  "Click left to input a time interval or /"never/".")
	 tv:choose-variable-values-keyword)

(defun parse-interval-or-never (string &optional from to)
  (setq string (string-trim '(#\SP #\TAB)
			    (if (null (or from to))
				string
				(substring string from to))))
  (if (member string '("none" "no" "" "never" "not ever" "nil" "()"))
      nil
      (multiple-value-bind (val err)
	  (time-interval-to-seconds string)
	(if err
	    (ferror nil "~A: ~A" string err)
	    val))))

(defun read-interval-or-never (stream)
  (parse-interval-or-never (readline stream)))

(defun print-interval-or-never (val &optional (stream t))
  (if (null val)
      (format stream "Never")
      (format stream (seconds-to-interval-string val))))

;;; Now that the time parser is loaded, we can fix up times remembered as strings by
;;; the system generator.
(ADD-INITIALIZATION "TIME-PARSER-LOADED" '(FS:CANONICALIZE-COLD-LOADED-TIMES) '(ONCE))
