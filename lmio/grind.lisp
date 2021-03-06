;;; Simple-minded, tasteful Grind -*-Package: System-Internals; Mode: LISP-*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Specials

(DEFVAR GRIND-IO)		;Stream for output.
(DEFVAR GRIND-REAL-IO)		;Stream which GRIND-PRINT-IO calls.
(DEFVAR GRIND-UNTYO-P)		;T if that stream can do :UNTYO.
(DEFVAR GRIND-WIDTH)		;Width to attempt to print within.
(DEFVAR GRIND-INDENT)		;Current indentation for lines.
(DEFVAR GRIND-HPOS)		;Current hpos (or next char to be printed).
(DEFVAR GRIND-VPOS)		;Current vpos.

;When DISPLACED should be checked for, the value of this variable
;is DISPLACED.  Otherwise, the value is taken from GRIND-DUMMY-DISPLACED
(DEFVAR GRIND-DISPLACED 'DISPLACED)
(DEFVAR GRIND-DUMMY-DISPLACED (NCONS NIL))

(DEFVAR GRIND-NOTIFY-FUN)	;Function to tell about interesting characters

(DEFVAR GRIND-RENAMING-ALIST NIL);Alist of renamings that were performed
				;on the function being ground.
				;We should undo the renamings when we grind
				;so that the code comes out as originally written.
				;Each element of this alist is (original-name new-name).

(DEFPROP QUOTE GRIND-QUOTE GRIND-MACRO)
(DEFPROP DEFUN GRIND-DEFUN GRIND-MACRO)
(DEFPROP MACRO GRIND-DEFUN GRIND-MACRO)
(DEFPROP DEFMACRO GRIND-DEFUN GRIND-MACRO)
(DEFPROP DEFMETHOD GRIND-DEFUN GRIND-MACRO)
(DEFPROP LAMBDA GRIND-LAMBDA GRIND-MACRO)
(DEFPROP NAMED-LAMBDA GRIND-NAMED-LAMBDA GRIND-MACRO)
(DEFPROP PROG GRIND-PROG GRIND-MACRO)
(DEFPROP PROG* GRIND-PROG GRIND-MACRO)
(DEFPROP DO GRIND-DO GRIND-MACRO)
(DEFPROP DO-NAMED GRIND-DO-NAMED GRIND-MACRO)
(DEFPROP COND GRIND-COND GRIND-MACRO)
(DEFPROP SETQ GRIND-SETQ GRIND-MACRO)
(DEFPROP PSETQ GRIND-SETQ GRIND-MACRO)
(DEFPROP AND GRIND-AND GRIND-MACRO)
(DEFPROP OR GRIND-AND GRIND-MACRO)
(DEFPROP LAMBDA GRIND-LAMBDA-COMPOSITION GRIND-L-MACRO)
(DEFPROP LET GRIND-LET GRIND-MACRO)
(DEFPROP LET* GRIND-LET GRIND-MACRO)
(DEFPROP TRACE GRIND-TRACE GRIND-MACRO)

;;; Macro to typeout a constant character

(DEFMACRO GTYO (CH &OPTIONAL NOTIFY)
  (SETQ CH (COND ((NUMBERP CH) CH)
		 (T (INHIBIT-STYLE-WARNINGS (GETCHARN CH 1)))))
  `(PROGN
     ,(AND NOTIFY `(AND GRIND-NOTIFY-FUN
			(NEQ GRIND-IO #'GRIND-COUNT-IO)
			(FUNCALL GRIND-NOTIFY-FUN ,CH ,NOTIFY NIL)))
     (FUNCALL GRIND-IO ':TYO ,CH)))


;;; Macro to do something with indentation bound to current HPOS.

(DEFMACRO GIND BODY
  `((LAMBDA (GRIND-INDENT) ,@BODY)
    GRIND-HPOS))

;;; Macro to do something and not check for DISPLACED in it
;;; (because it is quoted list structure, etc., not evaluated).

(DEFMACRO GRIND-QUOTED BODY
  `((LAMBDA (GRIND-DISPLACED) ,@BODY)
    GRIND-DUMMY-DISPLACED))

;;; CRLF then indent to GRIND-INDENT

(DEFUN GRIND-TERPRI ()
  (COND ((EQ GRIND-IO (FUNCTION GRIND-COUNT-IO))
	 (SETQ GRIND-VPOS (1+ GRIND-VPOS)
	       GRIND-HPOS GRIND-INDENT))
	(T
	 (GTYO #\CR)
	 (DO I GRIND-INDENT (1- I) (ZEROP I)
	   (GTYO #\SP)))))
	
;;; I/O stream which counts VPOS and HPOS, and THROWs to GRIND-DOESNT-FIT-CATCH
;;; if the width overflows.
;;; For now at least, doesn't hack tabs and backspaces and font changes and cetera.

;; Bind this to non-NIL tells streams it is OK to throw to GRIND-DOESNT-FIT-CATCH
;; Doing it this way avoids the incredible slowness of CATCH-ERROR and THROW's interaction.
(DEFVAR GRIND-DOESNT-FIT-CATCH NIL)

(DEFMACRO CATCH-IF-DOESNT-FIT (&BODY BODY)
  `(LET ((GRIND-DOESNT-FIT-CATCH T))
     (*CATCH 'GRIND-DOESNT-FIT-CATCH . ,BODY)))

(DEFPROP GRIND-COUNT-IO T IO-STREAM-P)

(DEFUN GRIND-COUNT-IO (OPERATION &OPTIONAL ARG1 &REST REST)
  (COND ((EQ OPERATION ':WHICH-OPERATIONS) '(:TYO))
	((NEQ OPERATION ':TYO)
	 (STREAM-DEFAULT-HANDLER #'GRIND-COUNT-IO OPERATION ARG1 REST))
	((= ARG1 #\CR)
	 (SETQ GRIND-VPOS (1+ GRIND-VPOS) GRIND-HPOS 0))
	((AND GRIND-DOESNT-FIT-CATCH
	      (>= GRIND-HPOS GRIND-WIDTH))			;Line overflow
	 (*THROW 'GRIND-DOESNT-FIT-CATCH NIL))
	(T (SETQ GRIND-HPOS (1+ GRIND-HPOS)))))

;;; I/O stream which counts VPOS and HPOS and prints (to GRIND-REAL-IO).
;;; This has to do the throw if width overflows also, for untyoable GRIND-REAL-IOs.

(DEFPROP GRIND-PRINT-IO T IO-STREAM-P)

(DEFUN GRIND-PRINT-IO (OPERATION &OPTIONAL &REST REST)
  (COND ((EQ OPERATION ':WHICH-OPERATIONS) '(:TYO))
	((NEQ OPERATION ':TYO)
	 (STREAM-DEFAULT-HANDLER #'GRIND-PRINT-IO OPERATION
				 (CAR REST) (CDR REST)))
	(T (COND ((= (CAR REST) #\CR)
		  (SETQ GRIND-VPOS (1+ GRIND-VPOS) GRIND-HPOS 0))
		 ((AND GRIND-DOESNT-FIT-CATCH
		       (>= GRIND-HPOS GRIND-WIDTH))	;Line overflow
		  (*THROW 'GRIND-DOESNT-FIT-CATCH NIL))
		 (T (SETQ GRIND-HPOS (1+ GRIND-HPOS))))
	   (LEXPR-FUNCALL GRIND-REAL-IO OPERATION REST))))

(DEFUN GRIND-THROW-ERROR (&REST IGNORE)
  (*THROW 'THROW-ERROR-CATCH NIL))

(DEFUN GRIND-ATOM (ATOM STREAM LOC)
  (AND GRIND-RENAMING-ALIST
       (DOLIST (ELT GRIND-RENAMING-ALIST)
	 (AND (EQ (CADR ELT) ATOM)
	      (RETURN (SETQ ATOM (CAR ELT))))))
  (AND GRIND-NOTIFY-FUN
       (NEQ STREAM #'GRIND-COUNT-IO)
       (FUNCALL GRIND-NOTIFY-FUN ATOM LOC T))
  (PRIN1 ATOM STREAM))

;;; Basic Grinding Forms

;Grind an expression all on one line
;************ THE RIGHT WAY TO DO THIS IS IF A CRLF TRIES TO ************
;************ COME OUT IN LINEAR MODE THROW BACK TO THE TOPMOST *********
;************ INSTANCE OF LINEAR MODE.  THEN CALL MACROS SAME ***********
;************ AS IN GRIND-FORM.     -- dam                   ************

;;; Note that LOC is a locative to the thing being ground, and is used 
;;; so that it is possible to replace the thing being printed under
;;; program control.  This is currently used by the Inspector in the
;;; New Window System.
(DEFUN GRIND-LINEAR-FORM (EXP LOC &OPTIONAL (CHECK-FOR-MACROS T) &AUX TEM)
  (COND ((NLISTP EXP)					;Atoms print very simply
	 (GRIND-ATOM EXP GRIND-IO LOC))
	((MEMQ (CAR EXP) '(GRIND-COMMA GRIND-COMMA-ATSIGN GRIND-COMMA-DOT GRIND-DOT-COMMA))
	 (SELECTQ (CAR EXP)
	   (GRIND-COMMA (GTYO #/,))
	   (GRIND-COMMA-ATSIGN (GTYO #/,) (GTYO #/@))
	   (GRIND-COMMA-DOT (GTYO #/,) (GTYO #/.))
	   (GRIND-DOT-COMMA (GTYO #/.) (GTYO #\SP) (GTYO #/,)))
	 (GRIND-LINEAR-FORM (CADR EXP) (LOCF (CADR EXP))))
	((AND CHECK-FOR-MACROS
	      (OR (AND (SYMBOLP (CAR EXP))			;Check for GRIND-MACRO
		       (NOT (EQ (CAR EXP) 'QUOTE))		;(KLUDGE)
		       (SETQ TEM (GET (CAR EXP) 'GRIND-MACRO)))
		  (AND (LISTP (CAR EXP))			;Check for LAMBDA
		       (SYMBOLP (CAAR EXP))
		       (SETQ TEM (GET (CAAR EXP) 'GRIND-L-MACRO)))))
	 (*THROW 'GRIND-DOESNT-FIT-CATCH NIL))		;Macro, don't use linear form
	((EQ (CAR EXP) 'QUOTE)				;(KLUDGE)
	 (GRIND-QUOTE EXP LOC))
	((EQ (CAR EXP) GRIND-DISPLACED)
	 (GRIND-LINEAR-FORM (CADR EXP) (LOCF (CADR EXP))))
	(T (GRIND-LINEAR-TAIL EXP LOC))))

(DEFUN GRIND-LINEAR-TAIL (EXP LOC)
   (GTYO #/( LOC)				;Do linear list
   (DO ((X EXP (CDR X))
	(LOC1 LOC (LOCF (CDR X))))
       ((NLISTP X)
        (COND ((NOT (NULL X))
               (GTYO #\SP)
               (GTYO #/.)
               (GTYO #\SP)
               (GIND (GRIND-LINEAR-FORM X LOC1))))
        (GTYO #/) T))
      (GIND (GRIND-LINEAR-FORM (CAR X) (LOCF (CAR X))))
      (OR (NLISTP (CDR X))
          (GTYO #\SP))))

;First item on the left and the rest stacked vertically to its right,
;except if the first item won't fit one line, stack the rest below it.
;Items are processed through the full hair of GRIND-FORM
(DEFUN GRIND-STANDARD-FORM (EXP LOC)
 (COND ((NLISTP EXP) (GRIND-ATOM EXP GRIND-IO LOC))
       ((EQ (CAR EXP) GRIND-DISPLACED)
	(GRIND-STANDARD-FORM (CADR EXP) (LOCF (CADR EXP))))
       (T 
	(GTYO #/( LOC)
	(GIND (COND ((GRIND-FORM-VER (CAR EXP) (LOCF (CAR EXP)))
	             (GRIND-TERPRI))
	            (T
	             (GTYO #\SP)))
	      (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) (FUNCTION GRIND-FORM))))))

;Similar to above except without the left parenthesis
(DEFUN GRIND-STANDARD-FORM-1 (EXP LOC)
  LOC
  (GIND (COND ((GRIND-FORM-VER (CAR EXP) (LOCF (CAR EXP)))
	       (GRIND-TERPRI))
	      (T
	       (GTYO #\SP)))
	(GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) (FUNCTION GRIND-FORM))))

;Minimal width form.
;This is applied from the outside in if the expression is too wide
;when printed in normal form.
(DEFUN GRIND-MISER-FORM (EXP LOC)
 (GTYO #/( LOC)
 (GRIND-REST-OF-LIST EXP LOC (FUNCTION GRIND-OPTI-MISER)))

;;; {Recursive} top level for miser mode from the outside in.
(DEFUN GRIND-OPTI-MISER (EXP LOC)
  (COND ((NLISTP EXP)					;Atoms -- no optimization anyway
	 (GRIND-ATOM EXP GRIND-IO LOC))
	((EQ (CAR EXP) GRIND-DISPLACED)			;Undisplace displaced forms.
	 (GRIND-OPTI-MISER (CADR EXP) (LOCF (CADR EXP))))
	((GRIND-TRY #'GRIND-FORM EXP LOC))		;Use normal mode if it wins
	(T (GRIND-MISER-FORM EXP LOC))))		;Loses, use miser form

;Vertical form looks the same as miser form, but if
;it doesn't fit anyway we throw out and miser at a higher level rather
;than misering the forms inside of this form.
(DEFUN GRIND-VERTICAL-FORM (EXP LOC &OPTIONAL (FN (FUNCTION GRIND-FORM)))
  (COND ((ATOM EXP) (GRIND-ATOM EXP GRIND-IO LOC))
	(T (GTYO #/( LOC)
	   (GRIND-REST-OF-LIST EXP LOC FN))))

;;; Grind rest of a list vertically using indicated form for the members
(DEFUN GRIND-REST-OF-LIST (TAIL LOC FORM)
  (GIND (DO ((X TAIL (CDR X))
	     (LOC LOC (LOCF (CDR X))))
	    ((COND ((NLISTP X)
		    (GRIND-DOTTED-CDR X LOC))
		   ((NLISTP (CDR X))
		    ((LAMBDA (GRIND-WIDTH)	;last form needs room for right paren
			(FUNCALL FORM (CAR X) (LOCF (CAR X))))
		     (1- GRIND-WIDTH))
		    (GRIND-DOTTED-CDR (CDR X) (LOCF (CDR X))))))
	  (FUNCALL FORM (CAR X) (LOCF (CAR X)))
	  (GRIND-TERPRI))))			;not last form, terpri before next

(DEFUN GRIND-DOTTED-CDR (X LOC)
 (COND ((NOT (NULL X))
        (COND ((GRIND-TRY (FUNCTION (LAMBDA (X LOC)
				       (GTYO #\SP)
				       (GTYO #/.)
				       (GTYO #\SP)
				       (GRIND-ATOM X GRIND-IO LOC)))
			  X LOC))
	      (T (GRIND-TERPRI)
		 (GTYO #\SP)
		 (GTYO #/.)
		 (GTYO #\SP)
		 (GRIND-ATOM X GRIND-IO LOC)))))
 (GTYO #/) T)
 T) ;Must return T for the sake of COND in GRIND-REST-OF-LIST

; Handle backquotes.  They are recognizable as calls to one of these four functions.

(DEFPROP XR-BQ-CONS GRIND-BQ GRIND-MACRO)
(DEFPROP XR-BQ-LIST GRIND-BQ GRIND-MACRO)
(DEFPROP XR-BQ-LIST* GRIND-BQ GRIND-MACRO)
(DEFPROP XR-BQ-APPEND GRIND-BQ GRIND-MACRO)
(DEFPROP XR-BQ-NCONC GRIND-BQ GRIND-MACRO)

;The first thing to do is convert the backquote form
;into a list containing sublists like (grind-comma x) or (grind-comma-atsign x).
;Then we grind that list with a backquote in front.
;The symbols grind-comma, grind-comma-atsign and grind-comma-dot
;at the front of a list are recognized and print out as "," or ",@" or ",.".
;they are recognized in two ways: as grind-macros, by functions which look for such;
;and specially, by grind-as-block and grind-linear-form, which lose with grind-macros.
(DEFUN GRIND-BQ (EXP LOC)
  (GTYO #/`)
  (GRIND-AS-BLOCK (GRIND-UNBACKQUOTIFY EXP) LOC))

(DEFPROP GRIND-COMMA GRIND-COMMA GRIND-MACRO)
(DEFUN GRIND-COMMA (EXP LOC)
  LOC
  (GTYO #/,)
  (GRIND-FORM (CADR EXP) (LOCF (CADR EXP))))

(DEFPROP GRIND-COMMA-ATSIGN GRIND-COMMA-ATSIGN GRIND-MACRO)
(DEFUN GRIND-COMMA-ATSIGN (EXP LOC)
  LOC
  (GTYO #/,) (GTYO #/@)
  (GRIND-FORM (CADR EXP) (LOCF (CADR EXP))))

(DEFPROP GRIND-COMMA-DOT GRIND-COMMA-DOT GRIND-MACRO)
(DEFUN GRIND-COMMA-DOT (EXP LOC)
  LOC
  (GTYO #/,) (GTYO #/.)
  (GRIND-FORM (CADR EXP) (LOCF (CADR EXP))))

(DEFPROP GRIND-DOT-COMMA GRIND-DOT-COMMA GRIND-MACRO)
(DEFUN GRIND-DOT-COMMA (EXP LOC)
  LOC
  (GTYO #/.) (GTYO #\SP) (GTYO #/,)
  (GRIND-FORM (CADR EXP) (LOCF (CADR EXP))))

;Convert the backquote form to a list resembling what the user typed in,
;with "calls" to grind-comma, etc., representing the commas.
(DEFUN GRIND-UNBACKQUOTIFY (EXP)
  (COND ((OR (NUMBERP EXP) (EQ EXP T) (NULL EXP) (STRINGP EXP)) EXP)
	((SYMBOLP EXP) `(GRIND-COMMA ,EXP))
	((ATOM EXP) EXP)
	((EQ (CAR EXP) 'QUOTE) (CADR EXP))
	((EQ (CAR EXP) 'XR-BQ-CONS)
	 (CONS (GRIND-UNBACKQUOTIFY (CADR EXP))
	       (GRIND-UNBACKQUOTIFY-SEGMENT (CDDR EXP) NIL T)))
	((EQ (CAR EXP) 'XR-BQ-LIST)
	 (MAPCAR 'GRIND-UNBACKQUOTIFY (CDR EXP)))
	((EQ (CAR EXP) 'XR-BQ-LIST*)
	 (NCONC (MAPCAR 'GRIND-UNBACKQUOTIFY (BUTLAST (CDR EXP)))
		(GRIND-UNBACKQUOTIFY-SEGMENT (LAST EXP) NIL T)))
	((EQ (CAR EXP) 'XR-BQ-APPEND)
	 (MAPCON 'GRIND-UNBACKQUOTIFY-SEGMENT (CDR EXP)
		 (CIRCULAR-LIST T) (CIRCULAR-LIST NIL)))
	((EQ (CAR EXP) 'XR-BQ-NCONC)
	 (MAPCON 'GRIND-UNBACKQUOTIFY-SEGMENT (CDR EXP)
		 (CIRCULAR-LIST NIL) (CIRCULAR-LIST NIL)))
	(T `(GRIND-COMMA ,EXP))))

;Convert a thing in a backquote-form which should appear as a segment, not an element.
;The argument is the list whose car is the segment-form,
;and the value is the segment to be appended into the resulting list.
(DEFUN GRIND-UNBACKQUOTIFY-SEGMENT (LOC COPY-P TAIL-P)
  (COND ((AND TAIL-P (ATOM (CDR LOC)))
	 (LET ((TEM (GRIND-UNBACKQUOTIFY (CAR LOC))))
	   (COND ((AND (LISTP TEM) (EQ (CAR TEM) 'GRIND-COMMA))
		  (LIST `(GRIND-DOT-COMMA ,(CAR LOC))))
		 (T TEM))))
	((AND (LISTP (CAR LOC))
	      (EQ (CAAR LOC) 'QUOTE)
	      (LISTP (CADAR LOC)))
	 (CADAR LOC))
	(T (LIST (LIST (IF COPY-P 'GRIND-COMMA-ATSIGN 'GRIND-COMMA-DOT)
		       (CAR LOC))))))

;;; Grind a form, choosing appropriate method
;;; The catch for miser mode is at a higher level than this, but
;;; the catch for linear mode is here.  Thus miser mode gets applied
;;; from the outside in, while linear mode gets applied from the
;;; inside out.

(DEFUN GRIND-FORM (EXP LOC &AUX TEM GMF)
  (COND ((ATOM EXP)					;Atoms print very simply
	 (GRIND-ATOM EXP GRIND-IO LOC))
	((EQ (CAR EXP) GRIND-DISPLACED)
	 (GRIND-FORM (CADR EXP) (LOCF (CADR EXP))))
	((AND (SYMBOLP (CAR EXP))			;Check for GRIND-MACRO
	      (OR (NULL (CDR EXP)) (NOT (ATOM (CDR EXP)))) ; but try not to get faked out
	      (SETQ TEM (GET (CAR EXP) 'GRIND-MACRO))
	      (NOT (SETQ GMF (*CATCH 'GRIND-MACRO-FAILED
			       (PROGN (FUNCALL TEM EXP LOC) NIL))))))
	((AND (LISTP (CAR EXP))				;Check for LAMBDA
	      (SYMBOLP (CAAR EXP))
	      (SETQ TEM (GET (CAAR EXP) 'GRIND-L-MACRO))
	      (NOT (SETQ GMF (*CATCH 'GRIND-MACRO-FAILED
			       (PROGN (FUNCALL TEM EXP LOC) NIL))))))
	((AND (MEMQ GMF '(NIL NOT-A-FORM))
	      ;; If linear form works, use it
	      (GRIND-TRY #'GRIND-LINEAR-FORM EXP LOC (NULL GMF))))
	(T (GRIND-STANDARD-FORM EXP LOC))))		;Loses, go for standard form

;;; GRIND-FORM and return T if it takes more than one line.
(DEFUN GRIND-FORM-VER (EXP LOC &AUX TEM)
  (SETQ TEM GRIND-VPOS)
  (GIND (GRIND-FORM EXP LOC))
  (NOT (= GRIND-VPOS TEM)))

;;; Grind with a certain form if it wins and return T,
;;; or generate no output and return NIL if that form won't fit.
(DEFUN GRIND-TRY (FORM EXP LOC &REST ARGS &AUX MARK VP HP)
  (COND (GRIND-UNTYO-P					;UNTYO able, so
	 (SETQ VP GRIND-VPOS				; save current place
	       HP GRIND-HPOS
	       MARK (FUNCALL GRIND-REAL-IO ':UNTYO-MARK))
	 (OR (CATCH-IF-DOESNT-FIT			;Then try doing it
	       (LEXPR-FUNCALL FORM EXP LOC ARGS)
	       T)
	     (PROGN					;Lost, back up to saved place
	       (SETQ GRIND-VPOS VP
		     GRIND-HPOS HP)
	       (FUNCALL GRIND-REAL-IO ':UNTYO MARK)
	       NIL)))					;Return NIL to indicate lossage

	((EQ GRIND-IO (FUNCTION GRIND-COUNT-IO))	;Only counting, so
	 (SETQ VP GRIND-VPOS				; save current place
	       HP GRIND-HPOS)
	 (OR (CATCH-IF-DOESNT-FIT
	       (LEXPR-FUNCALL FORM EXP LOC ARGS)
	       T)					;Then try doing it
	     (PROGN					;Lost, back up to saved place
	       (SETQ GRIND-VPOS VP
		     GRIND-HPOS HP)
	       NIL)))					;Return NIL to indicate lossage

	((CATCH-IF-DOESNT-FIT				;Have to use do-it-twice mode
	     (LET ((GRIND-IO (FUNCTION GRIND-COUNT-IO))
		   (GRIND-VPOS GRIND-VPOS)
		   (GRIND-HPOS GRIND-HPOS))
	       (LEXPR-FUNCALL FORM EXP LOC ARGS)	;So first try it tentatively
	       T))
	 (LEXPR-FUNCALL FORM EXP LOC ARGS)		;Won, do it for real
	 T)))						;Lost, return NIL

;;; Grind Top Level

;;; Top level grinding function.
;;; GRIND-WIDTH used to default to 95.  Now, it defaults to NIL, meaning
;;; try to figure it out and use 95. if you can't.
(DEFUN GRIND-TOP-LEVEL (EXP &OPTIONAL (GRIND-WIDTH NIL)
			    	      (GRIND-REAL-IO STANDARD-OUTPUT)
				      (GRIND-UNTYO-P NIL)
				      (GRIND-DISPLACED 'DISPLACED)
				      (TERPRI-P T)
				      (GRIND-NOTIFY-FUN NIL)
				      (LOC (NCONS EXP)))
  (IF (NULL GRIND-WIDTH)
      (SETQ GRIND-WIDTH (GRIND-WIDTH-OF-STREAM GRIND-REAL-IO)))
  (AND TERPRI-P (FUNCALL GRIND-REAL-IO ':FRESH-LINE))
  (LET ((GRIND-IO (FUNCTION GRIND-PRINT-IO))
	(GRIND-INDENT 0)
	(GRIND-HPOS 0)
	(GRIND-VPOS 0))
     (COND ((LISTP EXP)
	    (GRIND-OPTI-MISER EXP LOC))
	   (T (GRIND-ATOM EXP GRIND-IO LOC)))))

;;; Given a stream, try to figure out a good grind-width for it.
(DEFUN GRIND-WIDTH-OF-STREAM (STREAM)
    (COND ((MEMQ ':SIZE-IN-CHARACTERS (FUNCALL STREAM ':WHICH-OPERATIONS))
	   ;; Aha, this stream handles enough messages that we can figure
	   ;; out a good size.  I suppose there ought to be a new message
	   ;; just for this purpose, but...  And yes, I know it only works
	   ;; with fixed-width fonts, but that is inherent in GRIND-WIDTH.
	   (FUNCALL STREAM ':SIZE-IN-CHARACTERS))
	  (T
	   ;; No idea, do the old default thing.  Better than nothing.
	   95.)))

;;; Grind Definitions

(DECLARE (SPECIAL GRINDEF))			;Remembers the last argument to GRINDEF

;Grind the definitions of one or more functions.  With no arguments,
;repeat the last operation.
(DEFUN GRINDEF (&QUOTE &REST FCNS)
  (AND FCNS (SETQ GRINDEF (COPYLIST FCNS)))
  (MAPC #'GRIND-1 GRINDEF)			;Grind each function
  '*)						;Return silly result

;;; Grind the definition of a function.
;;; (See comments at GRIND-TOP-LEVEL re the WIDTH argument.)
(DEFUN GRIND-1 (FCN &OPTIONAL (WIDTH NIL)
			      (REAL-IO STANDARD-OUTPUT)
			      (UNTYO-P NIL)
	        &AUX EXP EXP1 TEM GRIND-RENAMING-ALIST)
  (IF (NULL WIDTH)
      (SETQ WIDTH (GRIND-WIDTH-OF-STREAM REAL-IO)))
  (PROG GRIND-1 ()
	(COND ((AND (SYMBOLP FCN) (BOUNDP FCN))
	       (GRIND-TOP-LEVEL `(SETQ ,FCN ',(SYMEVAL FCN)) WIDTH REAL-IO UNTYO-P)
	       (TERPRI REAL-IO)))
	(OR (FDEFINEDP FCN) (RETURN NIL))
	(SETQ EXP (FDEFINITION FCN))
	(DO () (())
	  (SETQ EXP1 EXP)
	  (AND (LISTP EXP) (EQ (CAR EXP) 'MACRO)
	       (SETQ EXP1 (CDR EXP)))
	  (OR (AND (NOT (SYMBOLP EXP1))
		   (SETQ TEM (ASSQ 'SI:ENCAPSULATED-DEFINITION
				   (FUNCTION-DEBUGGING-INFO EXP1))))
	      (RETURN NIL))
	  (FUNCALL (GET (CADDR TEM) 'ENCAPSULATION-GRIND-FUNCTION)
		   FCN EXP1 WIDTH REAL-IO UNTYO-P)
	  (AND (EQ (CADDR TEM) 'RENAME-WITHIN)
	       (SETQ GRIND-RENAMING-ALIST
		     (CADR (ASSQ 'RENAMINGS (FUNCTION-DEBUGGING-INFO EXP1)))))
	  (OR (FDEFINEDP (CADR TEM)) (RETURN-FROM GRIND-1 NIL))
	  (SETQ EXP (FDEFINITION (CADR TEM))))
	(AND (OR (TYPEP EXP ':COMPILED-FUNCTION)
		 (AND (LISTP EXP)
		      (EQ (CAR EXP) 'MACRO)
		      (TYPEP (CDR EXP) ':COMPILED-FUNCTION)))
	     (SETQ TEM (AND (SYMBOLP FCN) (GET FCN ':PREVIOUS-EXPR-DEFINITION)))
	     (PROGN (SETQ EXP TEM)
		    (PRINC ";Compiled" REAL-IO)	;COMMENT
		    (TERPRI REAL-IO)))
        (AND (LISTP FCN) (EQ (CAR FCN) ':WITHIN)
	     (EQ EXP (CADDR FCN))
	     (RETURN NIL))
	(GRIND-TOP-LEVEL (COND ((NLISTP EXP)
				`(DEFF ,FCN ',EXP))
			       ((EQ (CAR EXP) 'MACRO)
				(COND ((TYPEP (CDR EXP) ':COMPILED-FUNCTION)
				       `(DEFF ,FCN ',EXP))
				      (T `(MACRO ,FCN
						 . ,(GRIND-FLUSH-LAMBDA-HEAD (CDR EXP))))))
			       ((OR (EQ (CAR EXP) 'SUBST) (EQ (CAR EXP) 'NAMED-SUBST))
				`(DEFSUBST ,FCN . ,(GRIND-FLUSH-LAMBDA-HEAD EXP)))
			       ((NOT (MEMQ (CAR EXP) '(LAMBDA NAMED-LAMBDA)))
				`(FDEFINE ',FCN ',EXP))
			       ((AND (LISTP FCN) (EQ (CAR FCN) ':METHOD))
				(SETQ TEM (GRIND-FLUSH-LAMBDA-HEAD EXP))
				(SETQ TEM (CONS (CDAR TEM) (CDR TEM)))	;Remove OPERATION arg
				`(DEFMETHOD ,(CDR FCN) . ,TEM))
			       (T
				`(DEFUN ,FCN . ,(GRIND-FLUSH-LAMBDA-HEAD EXP))))
			 WIDTH
			 REAL-IO
			 UNTYO-P)
	 (TERPRI REAL-IO)
	 ))

(DEFUN GRIND-FLUSH-LAMBDA-HEAD (LAMBDA-EXP)
    (COND ((ATOM LAMBDA-EXP) LAMBDA-EXP)
	  ((OR (EQ (CAR LAMBDA-EXP) 'NAMED-LAMBDA)
	       (EQ (CAR LAMBDA-EXP) 'NAMED-SUBST))
	   (CDDR LAMBDA-EXP))
	  (T (CDR LAMBDA-EXP))))

;;; Grind Macros

(DEFUN GRIND-QUOTE (EXP LOC)
  (COND ((AND (CDR EXP) (NULL (CDDR EXP)))
	 (GTYO #/' LOC)
	 (GIND (GRIND-AS-BLOCK (CADR EXP) (LOCF (CADR EXP)))))
	(T (GRIND-AS-BLOCK EXP LOC))))

;NOTE- DEFUN looks bad in miser mode, so we have a slight kludge
; to bypass it.  (Would only gain two spaces anyway).  Probably
; this should be generalized to some property on the atom?
(DEFUN GRIND-DEFUN (EXP LOC)
  (GRIND-DEF-FORM EXP LOC 3 (FUNCTION GRIND-OPTI-MISER)))

(DEFUN GRIND-LAMBDA (EXP LOC)
  (GRIND-DEF-FORM EXP LOC 2))

(DEFUN GRIND-NAMED-LAMBDA (EXP LOC)
  (GRIND-DEF-FORM EXP LOC 3))

;DEFUN either craps out and uses miser form, or puts fcn and lambda list
;on the first line and the rest aligned under the U
;Second arg to GRIND-DEF-FORM is number of items on the first line.
;The last one is ground as a block.
(DEFUN GRIND-DEF-FORM (EXP LOC N &OPTIONAL (FORM (FUNCTION GRIND-FORM)))
  ;; Make a prepass over the list and make sure it looks like a form (i.e. not dotted,
  ;; and long enough)
  (LOOP FOR LS = EXP THEN (CDR LS)
	AND I FROM 0
	WHEN (AND (ATOM LS) (OR (< I N) LS))
	DO (*THROW 'GRIND-MACRO-FAILED 'NOT-A-FORM)
	WHEN (NULL LS)
	DO (RETURN))
  (GTYO #/( LOC)
  (DOTIMES (I (1- N))
    (GRIND-QUOTED (GRIND-LINEAR-FORM (CAR EXP) (LOCF (CAR EXP))))
    (GTYO #\SP)
    (SETQ LOC (LOCF (CDR EXP)))
    (SETQ EXP (CDR EXP)))
  (IF (CAR EXP)
      (GRIND-QUOTED (GRIND-AS-BLOCK (CAR EXP) (LOCF (CAR EXP))))
      (GTYO #/( (LOCF (CAR EXP)))
      (GTYO #/) T))
  (COND ((CDR EXP)
	 (GRIND-TERPRI)
	 (DOTIMES (I 4) (GTYO #\SP))
	 (GIND (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) FORM)))
	(T (GTYO #/) T))))

;;; BLOCK FORM: As many frobs per line as will fit;
;;;  and don't undisplace DISPLACED in them, since they aren't forms, just lists.
;;; Don't check for grind macros.  Do recognize GRIND-COMMA, etc.,
;;; because this function is used for printing the body of a backquote.
(DEFUN GRIND-AS-BLOCK (EXP LOC &AUX (GRIND-DISPLACED GRIND-DUMMY-DISPLACED))
   (COND ((NLISTP EXP)
	  (GRIND-ATOM EXP GRIND-IO LOC))
	 (T (GTYO #/( LOC)
	    (GIND (DO ((X EXP (CDR X))
		       (LOC LOC (LOCF (CDR X)))
		       (ELT) (ELTLOC)
		       (FRESHLINEP T)
		       (VP GRIND-VPOS GRIND-VPOS))
		      ((NLISTP X)
		       (GRIND-DOTTED-CDR X LOC))
		    (SETQ ELT (CAR X) ELTLOC (LOCF (CAR X)))
		    (COND ((AND (LISTP ELT)
				(MEMQ (CAR ELT) '(GRIND-COMMA
						  GRIND-COMMA-DOT
						  GRIND-DOT-COMMA
						  GRIND-COMMA-ATSIGN)))
			   (SELECTQ (CAR ELT)
			     (GRIND-COMMA (GTYO #/,))
			     (GRIND-COMMA-ATSIGN (GTYO #/,) (GTYO #/@))
			     (GRIND-COMMA-DOT (GTYO #/,) (GTYO #/.))
			     (GRIND-DOT-COMMA (GTYO #/.) (GTYO #\SP) (GTYO #/,)))
			   (SETQ ELTLOC (LOCF (CADR ELT)) ELT (CADR ELT))))
		    (COND ((GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) ELT ELTLOC))
			  ((GRIND-TRY (FUNCTION GRIND-STANDARD-FORM) ELT ELTLOC))
			  (T				;Won't fit, start another line
			   (OR FRESHLINEP (GRIND-TERPRI))
			   (SETQ VP GRIND-VPOS)
			   (OR (GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) ELT ELTLOC)
			       (GRIND-STANDARD-FORM ELT ELTLOC))))
		    (AND (LISTP (CDR X))		;If not done,
			 (COND ((AND (= VP GRIND-VPOS)	;if still on same line, need a space
				     (< GRIND-HPOS GRIND-WIDTH))	;unless at end of line
				(GTYO #\SP)
				(SETQ FRESHLINEP NIL))
			       (T			;If this was moby, don't put
				(GRIND-TERPRI)		; anything else on the same line
				(SETQ FRESHLINEP T)))) )))))

;;; PROG form is similar, but with exdented tags
(DEFUN GRIND-PROG (EXP LOC)
  (GTYO #/( LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GTYO #\SP)
  (GRIND-AS-BLOCK (CADR EXP) (LOCF (CADR EXP)))
  (GRIND-TERPRI)
  (GTYO #\SP)					;Tags fall under the P of PROG
  (GRIND-REST-OF-PROG (CDDR EXP) (LOCF (CDDR EXP)) (+ GRIND-INDENT 6)))

(DEFUN GRIND-REST-OF-PROG (EXP LOC INDENT)
  (GIND (DO ((X EXP (CDR X))
	     (LOC LOC (LOCF (CDR X))))
	    ((NLISTP X)
	     (GRIND-DOTTED-CDR X LOC))
	  (COND ((ATOM (CAR X))			;Tag
		 (GRIND-ATOM (CAR X) GRIND-IO (LOCF (CAR X)))
		 (OR (< GRIND-HPOS INDENT)	;Put next thing on same line if it fits
		     (NLISTP (CDR X))
		     (GRIND-TERPRI)))
		(T				;Statement
		 (DO I (- INDENT GRIND-HPOS) (1- I) (<= I 0)
		   (GTYO #\SP))
		 (GRIND-FORM (CAR X) (LOCF (CAR X)))
		 (OR (NLISTP (CDR X))
		     (GRIND-TERPRI)))))))

;;; DO: determine whether old or new format, grind out the header,
;;; then do the body like PROG
(DEFUN GRIND-DO (EXP LOC)
  (GRIND-CHECK-DO EXP)
  (GTYO #/( LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GRIND-REST-OF-DO EXP LOC))

(DEFUN GRIND-DO-NAMED (EXP LOC)
  (GRIND-CHECK-DO (CDR EXP))
  (GTYO #/( LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GTYO #\SP)
  (GRIND-ATOM (CADR EXP) GRIND-IO LOC)
  (GRIND-REST-OF-DO (CDR EXP) (LOCF (CDR EXP))))

(DEFUN GRIND-CHECK-DO (EXP)
  (AND (< (LENGTH EXP) 3)
       (*THROW 'GRIND-MACRO-FAILED 'NOT-A-FORM)))

(DEFUN GRIND-REST-OF-DO (EXP LOC)
  (GTYO #\SP)
  (COND ((OR (LISTP (CADR EXP)) (NULL (CADR EXP)))	;New format
	 (GIND (PROGN (GRIND-VERTICAL-FORM (CADR EXP)	;Var list vertically
					   (LOCF (CADR EXP))
                                           (FUNCTION GRIND-DO-VAR))
		      (GRIND-TERPRI)
		      ;; End test / results as COND clause
		      (GRIND-COND-CLAUSE (CADDR EXP) (LOCF (CADDR EXP)))))
	 (SETQ LOC (LOCF (CDDDR EXP)))
	 (SETQ EXP (CDDDR EXP)))
	(T					;Old format
	 (GRIND-LINEAR-FORM (CADR EXP) (LOCF (CADR EXP)))	;Var
	 (GTYO #\SP)
	 (GRIND-LINEAR-FORM (CADDR EXP) (LOCF (CADDR EXP)))	;Initial
	 (GTYO #\SP)
	 (GRIND-LINEAR-FORM (CADDDR EXP) (LOCF (CADDDR EXP)))	;Step
	 (GTYO #\SP)
	 (GRIND-LINEAR-FORM (CAR (SETQ EXP (CDDDDR EXP))) (LOCF (CAR EXP)))	;Endtest
	 (SETQ LOC (LOCF (CDR EXP)))
	 (SETQ EXP (CDR EXP))))
  (GRIND-TERPRI)
  (GTYO #\SP)
  (GRIND-REST-OF-PROG EXP LOC (+ GRIND-INDENT 2)))

(DEFUN GRIND-DO-VAR (EXP LOC)
    (COND ((ATOM EXP) (GRIND-ATOM EXP GRIND-IO LOC))
	  ((GRIND-TRY (FUNCTION GRIND-LINEAR-TAIL) EXP LOC))	;If linear form works, use it
          (T (GTYO #/( LOC)
	     (GRIND-STANDARD-FORM-1 EXP LOC))))

(DEFUN GRIND-LET (EXP LOC)
  (GIND (GTYO #/( LOC)
	(GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
	(GTYO #\SP)
	(GIND (GRIND-VERTICAL-FORM (CADR EXP) (LOCF (CADR EXP)) (FUNCTION GRIND-DO-VAR)))
	(GRIND-TERPRI)
	(GTYO #\SP)
	(GTYO #\SP)
	(GRIND-REST-OF-PROG (CDDR EXP) (LOCF (CDDR EXP)) GRIND-HPOS)))

;;; COND: Print clauses in standard form.  Expressions within
;;; clauses are normally stacked vertically, but if there is one
;;; consequent and it is an atom or a GO, put it to the side if it fits.
;;; Also, if the antecedent is T and there is one consequent, put it to the
;;; side in order to save lines.

(DEFUN GRIND-COND (EXP LOC)
  (GTYO #/( LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GTYO #\SP)
  (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) (FUNCTION GRIND-COND-CLAUSE)))

(DEFUN GRIND-COND-CLAUSE (EXP LOC)
  (COND ((NLISTP EXP)
	 (GRIND-ATOM EXP GRIND-IO LOC))
	((AND (LISTP (CDR EXP))
	      (NULL (CDDR EXP))
	      (OR (EQ (CAR EXP) 'T)
		  (GRIND-SIMPLE-P (CADR EXP)))
	      (GRIND-TRY (FUNCTION GRIND-STANDARD-FORM) EXP LOC)))
	(T (GRIND-VERTICAL-FORM EXP LOC))))

;;; AND and OR: Stack vertically unless only two long and
;;; one or the other is an atom or the second is a GO.  This
;;; is analagous to the rule for COND clauses.
(DEFUN GRIND-AND (EXP LOC)
  (GTYO #/( LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GTYO #\SP)
  (GIND (COND ((AND (CDR EXP)
		    (CDDR EXP)
		    (NULL (CDDDR EXP))
		    (OR (GRIND-SIMPLE-P (CADR EXP))
			(GRIND-SIMPLE-P (CADDR EXP)))
		    (GRIND-TRY (FUNCTION GRIND-STANDARD-FORM-1) (CDR EXP) (LOCF (CDR EXP)))))
	      (T (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) (FUNCTION GRIND-FORM))))))

;;; Predicate for whether something is simple enough to go
;;; "on the same line" in COND, AND, and OR.
(DEFUN GRIND-SIMPLE-P (EXP)
  (OR (NLISTP EXP)
      (EQ (CAR EXP) 'GO)
      (EQ (CAR EXP) 'QUOTE)))

;;; Trace.  If it won't fit on one line, put each trace option and argument on a line.
(DEFUN GRIND-TRACE (EXP LOC)
  (COND ((GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) EXP LOC))	;Fits on one line, OK
	(T (GTYO #/( LOC)					;Doesn't fit
	   (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
	   (GTYO #\SP)
	   (GIND (DO ((L (CDR EXP) (CDR L))
		      (CLAUSE)
		      (LOC))
		     ((NULL L)
		      (GTYO #/) T))
		   (SETQ CLAUSE (CAR L)
			 LOC (LOCF (CAR L)))
		   (COND ((ATOM CLAUSE) (GRIND-ATOM CLAUSE GRIND-IO LOC))
			 ((GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) CLAUSE LOC)) ;Try to use 1 line
			 (T (GTYO #/( LOC)
			    (COND ((NEQ (CAR CLAUSE) ':FUNCTION)
				   ;; Name of function
				   (GRIND-FORM (CAR CLAUSE) (LOCF (CAR CLAUSE)))
				   (SETQ LOC (LOCF (CDR CLAUSE)))
				   (SETQ CLAUSE (CDR CLAUSE))
				   (GTYO #\SP)))
			    (GIND (DO X CLAUSE (CDR X) (NULL X)	;Print each grind option
				    (GRIND-ATOM (CAR X) GRIND-IO LOC)	;First name of option
				    (COND ((EQ (CAR X) ':STEP))	;STEP takes no arg
					  ((MEMQ (CAR X) '(:BOTH :ARG :VALUE NIL))
					   (GTYO #\SP)
					   (GRIND-FORM (CDR X) (LOCF (CDR X)))	;These take N args
					   (SETQ X NIL))
					  ((NULL (CDR X)))	;Don't print what isn't there
					  (T (GTYO #\SP)
					     (GRIND-FORM (CADR X) (LOCF (CADR X)))	;Most take 1 arg
					     (SETQ X (CDR X))))
				    (AND (CDR X) (GRIND-TERPRI))
				    (SETQ LOC (LOCF (CDR X)))))
			    (GTYO #/) T)))
		   (AND (CDR L) (GRIND-TERPRI)))))))

;;; SETQ: print the args two per line.  If a pair don't fit on the line,
;;; for now just craps out through normal miser mode mechanism.
(DEFUN GRIND-SETQ (EXP LOC)
  (GTYO #/( LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GTYO #\SP)
  (GIND (DO X (CDR EXP) (CDDR X) (NULL X)
	  (COND ((GRIND-FORM-VER (CAR X) (LOCF (CAR X)))
		 (GRIND-TERPRI))
		(T (GTYO #\SP)))
	  (IF (NOT (NULL (CDR X))) ;don't bust totally on odd-length SETQ bodies.
	      (GRIND-FORM (CADR X) (LOCF (CADR X))))
	  (AND (CDDR X) (GRIND-TERPRI))))
  (GTYO #/) T))

;EXP is ((LAMBDA (...) ...) ...)
;Grind the Lambda as a form and the arguments underneath it.
(DEFUN GRIND-LAMBDA-COMPOSITION (EXP LOC)
  (GTYO #/( LOC)
  (GIND (PROGN
	 (GRIND-FORM (CAR EXP) (LOCF (CAR EXP)))
	 (GRIND-TERPRI)
	 (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) (FUNCTION GRIND-FORM)))))
