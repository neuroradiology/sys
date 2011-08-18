;This is pass 2 of the Lisp machine Lisp compiler   -*-Mode:LISP; Package:Compiler-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;     "This is insane.  What we clearly want to do is not completely
;;      clear, and is rooted in NCOMPLR."   -- BSG/Dissociated Press.

(DECLARE (COND ((STATUS FEATURE LISPM))
	       ((NULL (MEMQ 'NEWIO (STATUS FEATURES)))
		(BREAK 'YOU-HAVE-TO-COMPILE-THIS-WITH-QCOMPL T))
	       ((NULL (GET 'IF-FOR-MACLISP 'MACRO))
		(LOAD '(MACROS > DSK LISPM))
		(LOAD '(DEFMAC FASL DSK LISPM2))
		(LOAD '(LMMAC > DSK LISPM2))
		(INCLUDE |LISPM; QCDEFS >|))))

;(DECLARE (SETQ RUN-IN-MACLISP-SWITCH T))   ;not a chance!

(DECLARE (*EXPR QCOMPILE0 MEMQL LOGAND LOGLDB BARF))

;PDLLVL, on pass 2, is the current level of the PDL above the last local
;(number of temporary slots).  It isn't always updated by things which
;push and pop on a very local basis, but function calls, etc. update it.
;MAXPDLLVL is the largest value ever attained by PDLLVL.
;It goes into the FEF to say how large a stack frame is needed.
;The function MKPDLLVL sets PDLLVL and updates MXPDLLVL if necessary.
;INCPDLLVL increments PDLLVL by one, updating MXPDLLVL.
(DECLARE (SPECIAL PDLLVL MAXPDLLVL))
;NEEDPDL just says we need <n> more words of room on the pdl beyond what is there now.
(DEFMACRO NEEDPDL (N) `(SETQ MAXPDLLVL (MAX MAXPDLLVL (+ PDLLVL ,N))))

;CALL-BLOCK-PDL-LEVELS is a list of the PDLLVL's corresponding to the open
;call blocks.  PDLLVL is pushed on this stack before a call block is pushed
;and popped when one is popped (ie, the D-LAST is compiled).
;This is used so that we can see how many call blocks lie above
;a given old PDLLVL, so that we can compile instructions to pop call blocks
;rather than just pdl words (though this isn't implemented now).
;The reason for that is that if CALL is open-compiled someday then %SPREAD
;will push an unknown number of args on the pdl.  Each %SPREAD will just increment
;the stack by one.  Popping a fixed number of words loses when popping these,
;but it turns out that you never want to pop one of them without also popping
;the call block that contains it.
;So if we compile using popping call blocks, it will work!
(DEFVAR CALL-BLOCK-PDL-LEVELS)

;DROPTHRU on pass 2 is T if the code now being output can be reached.
;Code which cannot be reached is discarded at a low level.
(DEFVAR DROPTHRU)

;PROG-RETURN-DROPTHRU says that code dropping through the end of a PROG
;is really part of a RETURN, not anything dropping through in the user's code,
;so a NIL should NOT be output.
(DEFVAR PROG-RETURN-DROPTHRU)

;TAGOUT (on pass 2) is NIL until a tag has been output.
;While TAGOUT is NIL, setting a local variable to NIL can be flushed.
(DEFVAR TAGOUT)

;P2FN on pass 2 is the function we are compiling a call to.
;Pass 2 handler functions are normally passed the arglist and destination
;as arguments, since that makes most of them simpler.
;Those that handle more than one function find the function name in P2FN.
(DEFVAR P2FN)

;BDEST on pass 2 is the branch destination of the current form, or a tag destination.
;See P2BRANCH.
(DEFVAR BDEST)

;M-V-TARGET on pass 2 says whether and how the function call now being compiled
;is supposed to return multiple values.  It is NIL for an ordinary call
;from which only one value is expected.  Other things it can be are
;MULTIPLE-VALUE-LIST, or a number of values to just leave on the stack on return.
;See P2MV for more information.
(DEFVAR M-V-TARGET)

(DEFUN BUTLAST (X)
	(PROG (RTN)
	(COND ((NULL X)(RETURN NIL)))
L 	(COND ((NULL (CDR X)) (RETURN RTN)))
	(SETQ RTN (NCONC RTN (LIST (CAR X))))
	(SETQ X (CDR X))
	(GO L)))

;LDIFF as in Interlisp:  applied to (A B C D E) and (D E), it returns (A B C).
(DEFUN LDIFF (LIST SUBLIST)
    (COND ((NULL LIST) NIL)
	  ((EQ LIST SUBLIST) NIL)
	  (T (CONS (CAR LIST)
		   (LDIFF (CDR LIST) SUBLIST)))))

;FIRSTN of a number and a list returns the first that many elements of the list.
;If the list isn't that long, it is extended with NILs.  Like Take in APL.
(DEFUN FIRSTN (N LIST)
  (LET ((NEW-LIST (MAKE-LIST N)))
    (DO ((LIST LIST (CDR LIST))
	 (NEW-LIST NEW-LIST (CDR NEW-LIST)))
	((OR (NULL LIST) (NULL NEW-LIST)))
      (RPLACA NEW-LIST (CAR LIST)))
    NEW-LIST))

;Compile code to compute FORM and leave the result on the PDL.
(DEFUN P2PUSH (FORM) (P2 FORM 'D-PDL))

;; Compile a form for multiple values (maybe).
;; If our value is non-nil, it means that the code compiled
;; failed to produce the multiple values as it was asked to.
;; Normally, the destination should be D-PDL.
;; If you use another destination, then, if the value returned is non-NIL
;; then the single value has been compiled to the given destination,
;; but if the value is NIL, then the destination has been ignored.
;; This happens because forms that know how to generate the multiple
;; values setq M-V-TARGET to NIL.

;; Note: It is assumed that D-RETURN never has an M-V-TARGET,
;; and that an M-V-TARGET of MULTIPLE-VALUE-LIST implies D-PDL.

(DEFUN P2MV (FORM DEST M-V-TARGET)
  (COND ((NULL M-V-TARGET)
	 (P2 FORM DEST))
	(T
	 ;; In macrocode, d-next is the same as d-pdl, but d-pdl causes lots of optimizations.
	 (OR GENERATING-MICRO-COMPILER-INPUT-P
	     (AND (EQ DEST 'D-NEXT)
		  (SETQ DEST 'D-PDL)))
	 (COND ((ADRREFP FORM)
		(P2 FORM DEST))
	       ((EQ (CAR FORM) 'LEXICAL-REF)
		(P2 FORM DEST))
	       ((EQ (CAR FORM) '%POP)
		(P2 FORM DEST))
	       (T (P2F FORM DEST)))
	 (AND M-V-TARGET
	      (BARF FORM '|can't produce multiple values| 'WARN))))
  M-V-TARGET)

;Compile code to compute FORM and put the result in destination DEST.
;If DEST is D-IGNORE, we may not actually bother to compute the value
;if we can tell that there would be no side-effects.
(DEFUN P2 (FORM DEST)
    (AND (MEMQ DEST '(D-PDL D-NEXT))
	 (NEEDPDL 1))
    ;; In macrocode, d-next is the same as d-pdl, but d-pdl causes lots of optimizations.
    (OR GENERATING-MICRO-COMPILER-INPUT-P
	(AND (EQ DEST 'D-NEXT)
	     (SETQ DEST 'D-PDL)))
    (COND ((ADRREFP FORM)
	   (OR (EQ DEST 'D-IGNORE)
	       (OUTI `(MOVE ,DEST ,(P2-SOURCE FORM DEST)))))
	  ((EQ (CAR FORM) 'LEXICAL-REF)
	   (OR (EQ DEST 'D-IGNORE)
	       (PROGN (P2PUSH-CONSTANT (CADR FORM))
		      (OUTI `(MISC ,DEST %LOAD-FROM-HIGHER-CONTEXT)))))
	  ((EQ (CAR FORM) '%POP)      ;Must check for this before calling P2F
	   (SETQ PDLLVL (1- PDLLVL))  ;so that we can decrement PDLLVL.
	   (MOVE-RESULT-FROM-PDL DEST))
	  (T (LET (BDEST M-V-TARGET) (P2F FORM DEST)))))

(DEFUN P2F (FORM DEST)
   (LET ((PDLLVL PDLLVL) P2FN ARGL TM)
	(SETQ P2FN (CAR FORM) ARGL (CDR FORM))
	(COND ((EQ P2FN 'SPROG)
	       (P2PROG DEST
		       (P2SBIND (CAR ARGL) (CADR ARGL) VARS)
							; (RETURNS # BINDS USED)
		       (CADR ARGL)			;VARS LIST
		       (CADDR ARGL)			;NEW GOTAGS
		       (CADDDR ARGL)			;PROG NAME
		       (CAR (CDDDDR ARGL))		;BINDP
		       (CDR (CDDDDR ARGL))		;BODY
		       GOTAGS))
	      ((EQ P2FN 'PPROG)
	       (P2PROG DEST
		       (P2PBIND (CAR ARGL) (CADR ARGL))	;VAR BINDING LIST
							; (RETURNS # BINDS USED)
		       (CADR ARGL)			;VARS LIST
		       (CADDR ARGL)			;NEW GOTAGS
		       (CADDDR ARGL)			;PROG NAME
		       (CAR (CDDDDR ARGL))		;BINDP
		       (CDR (CDDDDR ARGL))		;BODY
		       GOTAGS))				;OLD GOTAGS
	      ((SETQ TM (GET P2FN 'P2))
	       (FUNCALL TM (CDR FORM) DEST))
	      ((SETQ TM (GET P2FN 'QINTCMP))
	       (P2MISC P2FN ARGL DEST TM))
	      (T (P2ARGC P2FN ARGL (GETARGDESC P2FN) DEST P2FN)))))

;;; Compile functions which have their own special instructions.

;Here for a "miscellaneous" instruction (no source address field; args always on PDL).
;Such functions have no P2 properties.  We recognize them by their QINTCMP
;properties, which hold the number of args which the function takes.
;The value of that property is passed as NARGS.  Since P1 already took care of
;any error message, we just ignore any extra args or nullify omitted ones.
;Except that P1 didn't give the user a hint that extra args were going to be ignored
;since it didn't know that this is a "misc" function.
(DEFUN P2MISC (INSN ARGL DEST NARGS)
    (COND ((< NARGS (LENGTH ARGL))	;Too many args
	   (BARF (NTHCDR NARGS ARGL) '|Extra arguments to misc-function ignored| 'WARN)
	   (SETQ ARGL (FIRSTN NARGS ARGL)))
	  ((> NARGS (LENGTH ARGL))	;Too few args
	   (SETQ ARGL (APPEND ARGL (DO ((N (- NARGS (LENGTH ARGL)) (1- N))
					(L NIL (CONS ''NIL L)))
				       ((ZEROP N) L))))))
    (COND (M-V-TARGET
	   (P2ARGC INSN ARGL (LIST (CONS NARGS '((FEF-ARG-REQ FEF-QT-EVAL))))
		   DEST P2FN))
	  (T (ARGLOAD ARGL 'D-PDL)
	     (OUTI (LIST 'MISC DEST INSN)))))

; Compile functions which have special non-destination instructions
; which expect one operand on the pdl and one specified as a source.

(MAPCAR (FUNCTION (LAMBDA (FN MISC)
		    (PUTPROP FN 'P2NODEST 'P2)
		    (PUTPROP FN MISC 'MISC-INSN)))
	'(*PLUS *DIF *TIMES *QUO *LOGAND *LOGXOR *LOGIOR)
	'(M-+ M-- M-* M-// M-LOGAND M-LOGXOR M-LOGIOR))

;Note that DEST should be 0 if we're compiling an instruction
;that doesn't produce a result on the PDL (just sets indicators).
;If microcompiling, the special instruction complicates the microcompiler,
;so use corresponding miscellaneous instruction.
(DEFUN P2NODEST (ARGL DEST)
  (COND ((NULL GENERATING-MICRO-COMPILER-INPUT-P)
    (COND ((EQ DEST 'D-IGNORE)	;Get first argument onto the pdl unless not being used anyway
	   (P2 (CAR ARGL) 'D-IGNORE))
	  (T (P2 (CAR ARGL) 'D-PDL)))
    (LET ((SOURCE (P2-SOURCE (CADR ARGL) DEST)))  ;Make second argument addressable
	 (COND ((EQ DEST 'D-IGNORE))	;Generate no code if not being used
	       (T (OUTI `(,P2FN 0 ,SOURCE))
		  (OR (MEMBER DEST '(0 D-PDL))  ;If the instruction generated a result on the
		      (MOVE-RESULT-FROM-PDL DEST))))))  ;pdl, put it where it belongs.
	(T (P2MISC (GET P2FN 'MISC-INSN) ARGL DEST 2))))

; Compile functions which have special instructions with destination fields.
; These take only one argument.
; The result can go directly to any destination, not just to the PDL.

(MAPC (FUNCTION (LAMBDA (FN) (PUTPROP FN 'P2DEST 'P2)))
      '(CAR CDR CAAR CADR CDAR CDDR))

(DEFUN P2DEST (ARGL DEST)
    (LET ((SOURCE (P2-SOURCE (CAR ARGL) DEST)))
	 (OR (EQ DEST 'D-IGNORE)
	     (OUTI `(,P2FN ,DEST ,SOURCE)))))

; Compile comparison functions.  They have special non-destination instructions
; which just set the indicators, but do not push T or NIL on the pdl.
; These instructions can be used only when calling the function "for predicate"
; (destination D-INDS).  Otherwise, the functions have corresponding
; miscellaneous instructions which are used instead.

(MAPC (FUNCTION (LAMBDA (FN MISC)
	  (PUTPROP FN 'P2COMPAR 'P2)
	  (PUTPROP FN MISC 'MISC-INSN)))
      '(= EQ INTERNAL-> INTERNAL-<)
      '(M-= M-EQ M-> M-<))

(DEFUN P2COMPAR (ARGL DEST)
    (COND ((EQ DEST 'D-IGNORE)
	   (ARGLOAD ARGL DEST))
	  ((EQ DEST 'D-INDS)
	   (P2NODEST ARGL 0))  ;The 0 here means the instruction produces no result
	  (T (P2MISC (GET P2FN 'MISC-INSN) ARGL DEST 2))))

(DEFPROP FUNCTION P2FUNCTION P2)
(DEFUN P2FUNCTION (ARGL DEST)
    (OUTI `(MOVE ,DEST (QUOTE-VECTOR (FUNCTION ,(CAR ARGL))))))

(DEFPROP BREAKOFF-FUNCTION P2BREAKOFF-FUNCTION P2)
(DEFUN P2BREAKOFF-FUNCTION (ARGL DEST)
    (OUTI `(MOVE ,DEST (QUOTE-VECTOR (BREAKOFF-FUNCTION ,(CAR ARGL))))))

(DEFUN (LEXICAL-CLOSURE P2) (ARGL DEST)
  (P2PUSH-CONSTANT (CADR (VAR-LAP-ADDRESS (CAR ARGL))))
  (OUTI `(MISC ,DEST %MAKE-LEXICAL-CLOSURE)))

(DEFPROP FUNCALL P2FUNCALL P2)
(DEFUN P2FUNCALL (ARGL DEST)
    (LET ((SOURCE (P2-SOURCE (CAR ARGL) 'D-PDL)))
	 ;;DONT HACK PDLLVL HERE SINCE GOING TO POP 1 AND PUSH 4
	 (P2ARGC SOURCE	;;;NON-SYMBOLIC FIRST ARG INDICATES IT IS ADDRESS FOR CALL INSTRUCTION
		 (CDR ARGL)
		 '((1005 (FEF-ARG-OPT FEF-QT-EVAL)))
		 DEST
		 NIL)))

(DEFPROP LEXPR-FUNCALL P2-LEXPR-FUNCALL P2)
(DEFUN P2-LEXPR-FUNCALL (ARGL DEST)
    (LET ((SOURCE (P2-SOURCE (CAR ARGL) 'D-PDL)))
	 ;;DONT HACK PDLLVL HERE SINCE GOING TO POP 1 AND PUSH 4
	 (P2ARGC SOURCE	;;;NON-SYMBOLIC FIRST ARG INDICATES IT IS ADDRESS FOR CALL INSTRUCTION
		 (CDR ARGL)
		 '((1005 (FEF-ARG-OPT FEF-QT-EVAL LEXPR-FUNCALL)))
		 DEST
		 NIL)))

;The D-NEXT-LIST stuff is no longer used.  LIST and variants are now plain
;functions, implemented in microcode.  Some day recycle the instructions
;to start making a list to instead make a list out of what is on the stack,
;and reinstate an appropriately-revised version of this code.
;(DEFPROP LIST P2LIST P2)
;(DEFPROP LIST-IN-AREA P2LIST P2)
;(DEFUN P2LIST (ARGL DEST)
;    (AND (>= (LENGTH ARGL) 100)
;	 (BARF `(LIST . ,ARGL) '|Too many arguments - implementation limit| 'DATA))
;    (COND ((EQ P2FN 'LIST-IN-AREA)
;	   (P2PUSH (CAR ARGL))
;	   (SETQ ARGL (CDR ARGL))
;	   (OUTI1 (LIST 'MISC
;			DEST
;			'LIST-IN-AREA
;			(LENGTH ARGL))))
;	  (T (OUTI1 `(MISC ,DEST LIST ,(LENGTH ARGL)))))
;    (MKPDLLVL (+ 3 PDLLVL))
;    (DO ((L ARGL (CDR L))) ((NULL L))
;	(P2 (CAR L) 'D-NEXT-LIST))
;    (AND (EQ DEST 'D-RETURN)
;	 (TAKE-DELAYED-TRANSFER)))

(DEFPROP VALUE-CELL-LOCATION P2VALUE-CELL-LOCATION P2)
(DEFUN P2VALUE-CELL-LOCATION (ARGL DEST)
    (COND ((AND (LISTP (CAR ARGL))
		(EQ (CAAR ARGL) 'QUOTE)
		(LISTP (CADAR ARGL)))
	   (SELECTQ (CAADAR ARGL)
	     (LOCAL-REF (OUTI `(PUSH-E 0 ,(VAR-LAP-ADDRESS (CADR (CADAR ARGL)))))
			(NEEDPDL 1)
			(MOVE-RESULT-FROM-PDL DEST))
	     (LEXICAL-REF (P2PUSH-CONSTANT (CADR (CADAR ARGL)))
			  (NEEDPDL 1)
			  (OUTI `(MISC ,DEST %LOCATE-IN-HIGHER-CONTEXT)))))
	  (T (P2MISC 'VALUE-CELL-LOCATION ARGL DEST 1))))

(DEFPROP COMPILER-LET-INTERNAL P2COMPILER-LET-INTERNAL P2)
(DEFUN P2COMPILER-LET-INTERNAL (ARGL DEST)
    (COND ((NULL (CAR ARGL))
	   (P2F (CONS 'PROGN (CDR ARGL)) DEST))
	  (T
	   (PROGV (LIST (CAAAR ARGL)) (LIST (EVAL (CADAAR ARGL)))
		  (P2F `(COMPILER-LET-INTERNAL ,(CDAR ARGL) . ,(CDR ARGL))
		       DEST)))))

;;; %ACTIVATE-OPEN-CALL-BLOCK must ignore its apparent destination and
;;; instead compile to D-IGNORE (microcode depends on this).
;;; This fails to let the compiler know that the pdl is popped and a delayed
;;; transfer may be taken, but then it didn't know the pdl was pushed either.
(DEFPROP %ACTIVATE-OPEN-CALL-BLOCK P2%ACTIVATE-OPEN-CALL-BLOCK P2)
(DEFUN P2%ACTIVATE-OPEN-CALL-BLOCK (IGNORE IGNORE)
  (OUTI '(MISC D-IGNORE %ACTIVATE-OPEN-CALL-BLOCK)))

;;; Don't actually call %PUSH, just push its argument
(DEFPROP %PUSH P2%PUSH P2)
(DEFUN P2%PUSH (ARGL IGNORE)
  (P2 (CAR ARGL) 'D-PDL))

(DEFPROP SETQ P2SETQ P2)
(DEFUN P2SETQ (ARGL DEST)
    (PROG NIL
     LOOP
	  (P2SETQ-1 (CAR ARGL) (CADR ARGL)
		    (COND ((NULL (CDDR ARGL)) DEST)
			  (T 'D-IGNORE)))
	  (SETQ ARGL (CDDR ARGL))
	  (AND ARGL (GO LOOP))))

;Compile code to set VAR to the result of computing VALUE,
;and also move that value to DEST.
(DEFUN P2SETQ-1 (VAR VALUE DEST)
       (COND ((MEMQ VAR '(NIL T))
	      NIL)
	     ((AND (LISTP VAR) (EQ (CAR VAR) 'LEXICAL-REF))
	      (P2PUSH VALUE)
	      (MOVEM-AND-MOVE-TO-DEST VAR DEST))
	     ((MEMBER VALUE '('0 'NIL))
              (OUTI `(,(CDR (ASSQ (CADR VALUE)
                                  '((0 . SETZERO) (NIL . SETNIL))))
                      0
		      ,(P2-SOURCE VAR 'D-PDL)))
              (OR (MEMQ DEST '(D-IGNORE D-INDS))
                  (P2 VALUE DEST)))
             ((AND (NOT (ATOM VALUE))
                   (CDR VALUE)
                   (EQUAL (CADR VALUE) VAR)
                   (MEMQ (CAR VALUE) '(CDR CDDR 1+ 1-))
                   (MEMQ DEST '(D-IGNORE D-INDS)))
              (OUTI `(SETE ,(CAR VALUE) ,(P2-SOURCE VAR 'D-PDL))))
             (T
              (P2PUSH VALUE)
              (MOVEM-AND-MOVE-TO-DEST VAR DEST)))
       NIL)

;Move the quantity on the top of the stack to the value of a variable
;and also move it to the specified destination.
(DEFUN MOVEM-AND-MOVE-TO-DEST (VAR DEST)
  (COND ((ATOM VAR)
	 (IF (MEMQ DEST '(D-IGNORE D-INDS))
	     (OUTI `(POP 0 (SPECIAL ,VAR)))
	     (OUTI `(MOVEM 0 (SPECIAL ,VAR)))
	     (MOVE-RESULT-FROM-PDL DEST)))
	((EQ (CAR VAR) 'LOCAL-REF)
	 (IF (MEMQ DEST '(D-IGNORE D-INDS))
	     (OUTI `(POP 0 ,(VAR-LAP-ADDRESS (CADR VAR))))
	     (OUTI `(MOVEM 0 ,(VAR-LAP-ADDRESS (CADR VAR))))
	     (MOVE-RESULT-FROM-PDL DEST)))
	((EQ (CAR VAR) 'LEXICAL-REF)
	 (P2PUSH-CONSTANT (CADR VAR))
	 (NEEDPDL 1)
	 (OUTI `(MISC ,DEST %STORE-IN-HIGHER-CONTEXT)))))

(DEFUN MOVE-RESULT-FROM-PDL (DEST)
  (COND ((NOT (EQ DEST 'D-PDL))
	 (OUTI `(MOVE ,DEST (LPDL 77))))))

(DEFPROP PROGN P2PROGN P2)
(DEFUN P2PROGN (ARGL DEST)
    (P2PROG12N (LENGTH ARGL) DEST ARGL))

(DEFPROP PROG2 P2PROG2 P2)
(DEFUN P2PROG2 (ARGL DEST)
    (P2PROG12N 2 DEST ARGL))

;Compile a PROGN or PROG2, etc.  ARGL is the list of argument expressions.
;N says which arg is to be returned as the value of the PROGN or PROG2
;(equals the length of ARGL for PROGN, or 2 for PROG2, etc.).
(DEFUN P2PROG12N (N DEST ARGL)
  (PROG ((IDEST DEST))
	(COND ((AND (NOT (EQ DEST 'D-IGNORE))
		    (< N (LENGTH ARGL)))
	       (SETQ IDEST 'D-PDL)))		;MIGHT COMPILE TEST ON RESULT INDICATORS
	(SETQ N (1- N))				;Convert to origin 0.
	;; Compile the args before the one whose value we want.
	(DOTIMES (I N)
	  (P2 (OR (CAR ARGL) ''NIL) 'D-IGNORE)
	  (POP ARGL))
	;; Compile the arg whose value we want.
	;; If it's the last arg (this is PROGN),
	;; make sure to pass along any multiple value target that the PROGN has,
	;; and to report back how many args were actually pushed.
	(COND ((AND (NULL (CDR ARGL)) M-V-TARGET)
	       (COND ((P2MV (OR (CAR ARGL) ''NIL) IDEST M-V-TARGET)
		      (INCPDLLVL))
		     ((NUMBERP M-V-TARGET)
		      (MKPDLLVL (+ PDLLVL M-V-TARGET))
		      (SETQ M-V-TARGET NIL))
		     (T (INCPDLLVL)		;MULTIPLE-VALUE-LIST was the target.
			(SETQ M-V-TARGET NIL))))
	      ((AND (NULL (CDR ARGL)) BDEST)
	       (P2BRANCH (OR (CAR ARGL) ''NIL) IDEST BDEST)
	       (SETQ BDEST NIL)
	       (COND ((EQ IDEST 'D-PDL) (INCPDLLVL))))
	      (T (P2 (OR (CAR ARGL) ''NIL) IDEST)
		 (COND ((EQ IDEST 'D-PDL) (INCPDLLVL)))))
	(OR (CDR ARGL) (RETURN NIL))
	;; Compile the remaining args.
	(DOLIST (ARG (CDR ARGL))
	  (P2 ARG 'D-IGNORE))
	(COND ((NOT (EQ IDEST DEST))
	       (MOVE-RESULT-FROM-PDL DEST))
	      ((NOT (EQ DEST 'D-IGNORE))
	       (OUTF '(MOVE D-PDL (LPDL 77)))))))	;Make sure it's really in indicators
         		     ; if IDEST and DEST both D-PDL

;Functions to gobble multiple values.

(DEFPROP MULTIPLE-VALUE-BIND P2MULTIPLE-VALUE-BIND P2)
(DEFUN P2MULTIPLE-VALUE-BIND (TAIL DEST)
  (LET ((VLIST (CAR TAIL)))
    (LET ((MVTARGET (LENGTH VLIST))
	  (MVFORM (CADDR TAIL)))
      ;; Compile the form to leave N things on the stack.
      ;; If it fails to do so, then it left only one, so push the other N-1.
      (AND (P2MV MVFORM 'D-PDL MVTARGET)
	   (DO ((I 1 (1+ I))) ((= I MVTARGET))
	     (OUTF '(MISC D-PDL FALSE)))))
    ;; Now pop them off, binding the variables to them.
    ;; Note that the vlist contains the variables
    ;; in the original order,
    ;; each with an initialization of (%POP).
    (P2PBIND VLIST (CADR TAIL))
    (LET ((VARS (CADR TAIL))
	  (BODY (CDDDR TAIL)))
      (P2PROG12N (LENGTH BODY) DEST BODY))))

(DEFPROP MULTIPLE-VALUE P2MULTIPLE-VALUE P2)
(DEFUN P2MULTIPLE-VALUE (TAIL DEST)
  (LET* ((VARIABLES (CAR TAIL))
	 (DEST1 (COND ((AND (EQ DEST 'D-IGNORE) (NULL (CAR VARIABLES))) 'D-IGNORE)
		      (T 'D-PDL))))
    (PROG ()
      (COND ((P2MV (CADR TAIL) DEST1 (LENGTH VARIABLES)) ; NIL if it actually pushes N values.
	     ;; It didn't push them.  Set the other variables to NIL.
	     (DOLIST (VAR (CDR VARIABLES))
		 (AND VAR (P2SETQ-1 VAR 'NIL 'D-IGNORE)))
	     ;; If the single value was discarded, nothing remains to be done.
	     (AND (EQ DEST1 'D-IGNORE) (RETURN NIL)))
	    (T ;; It really did push N values on the stack.  Pop all but the first off.
	     (DOLIST (VAR (REVERSE (CDR VARIABLES)))
	       (COND (VAR (MOVEM-AND-MOVE-TO-DEST VAR 'D-IGNORE))
		     (T (OUTF '(MOVE D-IGNORE (LPDL 77))))))))
      ;; Now there is only one thing on the stack, which is the value
      ;; of the first variable, and the value to be returned by
      ;; the call to MULTIPLE-VALUE.
      (COND ((CAR VARIABLES) (MOVEM-AND-MOVE-TO-DEST (CAR VARIABLES) DEST))
	    (T (MOVE-RESULT-FROM-PDL DEST))))))
  
;; Note that we make no provision for the possibility
;; than anything might want to optimize being compiled
;; for multiple-value-list by storing the list directly
;; to a destination other than D-PDL.
(DEFPROP MULTIPLE-VALUE-LIST P2MULTIPLE-VALUE-LIST P2)
(DEFUN P2MULTIPLE-VALUE-LIST (TAIL DEST)
  (COND ((P2MV (CAR TAIL) 'D-PDL 'MULTIPLE-VALUE-LIST)
	 (OUTF `(MISC ,DEST NCONS)))
	(T (MOVE-RESULT-FROM-PDL DEST))))

;Functions to generate multiple values.

(DEFPROP VALUES P2VALUES P2)
(DEFUN P2VALUES (ARGL DEST)
 (PROG ()
     ;; Handle returning from the top level of a function.
     (COND ((EQ DEST 'D-RETURN)
	    (LET ((NARGS (LENGTH ARGL)))
	      (COND ((= NARGS 1)
		     (P2 (CAR ARGL) 'D-RETURN)
		     (RETURN)))
	      (ARGLOAD ARGL 'D-PDL)
	      (COND ((= NARGS 2) (OUTI '(MISC D-IGNORE %RETURN-2)))
		    ((= NARGS 3) (OUTI '(MISC D-IGNORE %RETURN-3)))
		    ((ZEROP NARGS)
		     (P2VALUES-LIST '('NIL) DEST))
		    (T (P2PUSH-CONSTANT NARGS)
		       (OUTI '(MISC D-IGNORE %RETURN-N))))
	      (SETQ DROPTHRU NIL)	;Above MISC RETURN instructions return
	      (RETURN NIL))))
     (COND ((NUMBERP M-V-TARGET)
	    ;; If we want N values on the stack,
	    ;; then eval all the args to return
	    ;; and save exactly N things on the stack.
	    (DO ((VALS ARGL (CDR VALS)) (I 0 (1+ I)))
		((AND (NULL VALS) ( I M-V-TARGET)))
	      (P2 (OR (CAR VALS) ''NIL)
		  (COND (( I M-V-TARGET) 'D-IGNORE) (T 'D-PDL)))))
	   ((EQ M-V-TARGET 'MULTIPLE-VALUE-LIST)
	    (P2 `(LIST . ,ARGL) DEST))
	   ((NULL M-V-TARGET)
	    (LET ((PDLLVL PDLLVL)) (P2PROG12N 1 DEST ARGL))))
     (SETQ M-V-TARGET NIL)))

(DEFPROP VALUES-LIST P2VALUES-LIST P2)
(DEFUN P2VALUES-LIST (ARGL DEST)
  (PROG (ARG)
     (SETQ ARG (CAR ARGL))
     (COND ((AND (LISTP ARG) (EQ (CAR ARG) 'MULTIPLE-VALUE-LIST))
	    (RETURN (SETQ M-V-TARGET (P2MV (CADR ARG) DEST M-V-TARGET))))
	   ((AND (LISTP ARG) (EQ (CAR ARG) 'LIST))
	    (RETURN (P2VALUES (CDR ARG) DEST)))
	   ((EQ DEST 'D-RETURN)
	    (P2MISC 'RETURN-LIST ARGL 'D-RETURN 1))
	   ((NULL M-V-TARGET)
	    (P2 `(CAR ,ARG) DEST))
	   ((EQ M-V-TARGET 'MULTIPLE-VALUE-LIST)
	    (P2 ARG DEST))
	   ((NUMBERP M-V-TARGET)
	    (NEEDPDL 2)
	    (P2PUSH ARG)
	    (OUTF `(MOVE D-PDL (QUOTE-VECTOR ',M-V-TARGET)))
	    (OUTF '(MISC D-PDL %SPREAD-N))))
     (SETQ M-V-TARGET NIL)))

;;; This version doesn't try to do internal multiple values.  Doing so is
;;; difficult because it would require some sort of new instruction to
;;; shuffle the stack in the appropriate fashion.  This does do external
;;; multiple-values, i.e. an UNWIND-PROTECT to D-RETURN will pass back
;;; multiple values from the body.
(DEFUN (UNWIND-PROTECT P2) (FORMS DEST)
  (AND M-V-TARGET
       (BARF (CONS 'UNWIND-PROTECT FORMS) "Internal multiple values not implemented" 'BARF))
  (LET ((RESTART-TAG (GENSYM))
	(PDLLVL0 PDLLVL))
    (LET ((CALL-BLOCK-PDL-LEVELS CALL-BLOCK-PDL-LEVELS))
      ;; Open a multiple-value call to *CATCH with four values expected.
      (OUTI1 `(ADI-CALL CALL D-IGNORE (QUOTE-VECTOR (FUNCTION *CATCH))
			(RESTART-PC (QUOTE-VECTOR (TAG ,RESTART-TAG))
			 BIND-STACK-LEVEL NIL
			 MULTIPLE-VALUE (QUOTE-VECTOR (QUOTE 4)))))
      (MKPDLLVL (+ PDLLVL 10.))			;4 multiple value words, 6 ADI words
      (PUSH PDLLVL CALL-BLOCK-PDL-LEVELS)
      (MKPDLLVL (+ 4 PDLLVL))			;4 words of call block
      (OUTI `(MOVE D-PDL (QUOTE-VECTOR (QUOTE T))))	;Catch tag is T
      (INCPDLLVL)
      (P2 (CAR FORMS) (IF (EQ DEST 'D-RETURN) DEST 'D-LAST))
      (SETQ PDLLVL (+ PDLLVL0 4))		;Now have just 4 multiple values on stack
      (SETQ DROPTHRU T)
      (OUTF `(RESTART-TAG ,RESTART-TAG)))
    (DOLIST (FORM (CDR FORMS))		;Cleanup forms
      (P2 FORM 'D-IGNORE))
    (SETQ PDLLVL PDLLVL0)
    (OUTI `(MISC ,DEST %UNWIND-PROTECT-CONTINUE))))	;Continue according to stuff on stack

;Compile the body of a PROG.  The variable binding has already been done
;by P1PBIND or P1SBIND, which returned the number of special bindings made
;which is our argument NBINDS.
(DEFUN P2PROG (PROGDEST NBINDS VARS GOTAGS PROGNAME IBINDP BDY OLDGOTAGS)
    (LET ((PROGDESCS PROGDESCS) (RETPROGDESC RETPROGDESC))
       (PROG (IPROGDEST RETTAG PROG-RETURN-DROPTHRU NVALUES)
	     ;; Determine the immediate destination of returns in this prog.
	     (SETQ IPROGDEST 'D-PDL)
	     (AND (MEMQ PROGDEST '(D-IGNORE D-INDS D-RETURN))
		  (NULL M-V-TARGET)
		  (SETQ IPROGDEST PROGDEST))
	     ;; If BIND is used within this PROG, and it's an internal PROG,
	     ;; we must push the specpdl index at entry so we can unbind to it later.
	     (AND IBINDP (NOT (EQ PROGDEST 'D-RETURN))
		  (PROGN (OUTI '(MISC D-PDL SPECIAL-PDL-INDEX))
			 (INCPDLLVL)))
	     ;; How many words are we supposed to leave on the stack?
	     (SETQ NVALUES
		   (COND ((NUMBERP M-V-TARGET) M-V-TARGET)
			 ((EQ IPROGDEST 'D-PDL) 1)
			 (T 0)))
	     ;; The first GOTAG is the prog's return tag.
	     (SETQ RETTAG (GOTAG-PROG-TAG (CAR GOTAGS)))
	     ;; Remember this prog's general environment.
	     (PUSH (MAKE-PROGDESC PROGNAME RETTAG IPROGDEST M-V-TARGET PDLLVL
				  (COND ((AND IBINDP (NEQ PROGDEST 'D-RETURN))
					 (LIST NBINDS))
					(T NBINDS)))
		   PROGDESCS)
	     (OR (EQ PROGNAME T) (SETQ RETPROGDESC (CAR PROGDESCS)))
	     ;; Set the GOTAG-PDL-LEVEL of each of this prog's tags.
	     ;; The return tag is different from the rest
	     ;; because its pdllvl should include the values we want to leave stacked.
	     (SETF (GOTAG-PROGDESC (CAR GOTAGS)) (CAR PROGDESCS))
	     (SETF (GOTAG-PDL-LEVEL (CAR GOTAGS)) (+ PDLLVL NVALUES))
	     (DOLIST (GOTAG (CDR GOTAGS))
	       (SETF (GOTAG-PROGDESC GOTAG) (CAR PROGDESCS))
	       (SETF (GOTAG-PDL-LEVEL GOTAG) PDLLVL))
             (SETQ GOTAGS (APPEND GOTAGS OLDGOTAGS))
	L    (COND ((NULL BDY) (GO X1))
		   ((ATOM (CAR BDY))
                    (OR DROPTHRU (OUTF '(NO-DROP-THROUGH)))
		    (SETQ TAGOUT (SETQ DROPTHRU T))
		    (OUTF (GTAG (CAR BDY))))
		   ((NULL (CDR BDY))
		    (P2BRANCH (CAR BDY) 'D-IGNORE `(BRANCH ALWAYS RETURN NIL ,RETTAG)))
		   (T (P2 (CAR BDY) 'D-IGNORE)))
	     (SETQ BDY (CDR BDY))
	     (GO L)

	X1   ;; If DROPTHRU is NIL, code does not drop through at all.
	     ;; If PROG-RETURN-DROPTHRU is T,
	     ;; a RETURN at the end of the PROG is dropping thru,
	     ;; which means the values desired are already provided.
	     (COND ((AND (NOT (EQ PROGDEST 'D-IGNORE)) DROPTHRU
			 (NOT PROG-RETURN-DROPTHRU))
		    (COND ((NUMBERP M-V-TARGET)
			   (DOTIMES (I M-V-TARGET)
			     (OUTI '(MISC D-PDL FALSE))))
			  (M-V-TARGET (P2RETURN '('NIL) NIL))
			  (T (OUTI (LIST 'MISC IPROGDEST 'FALSE))))))
	     ;; If this is a top-level PROG, we just went to D-RETURN,
	     ;; and nobody will use the RETTAG, so we are done.
	     (AND (EQ PROGDEST 'D-RETURN)
		  (RETURN NIL))
	     ;; Otherwise, this is where RETURNs jump to.
	     (MKPDLLVL (GOTAG-PDL-LEVEL (CAR GOTAGS)))
	     (OUTTAG RETTAG)
	     ;; Unbind any specials and store away the value if
	     ;; it is not supposed to be left on the stack.
	     (AND IBINDP
		  (OUTPUT-UNBIND-TO-INDEX NVALUES))
	     (UNBIND IPROGDEST NBINDS)
	     (AND (NEQ PROGDEST IPROGDEST)
		  (NULL M-V-TARGET)
		  (MOVE-RESULT-FROM-PDL PROGDEST))
	     ;; If we were supposed to produce multiple values, we did.
	     (SETQ M-V-TARGET NIL))))

;; GO and various types of RETURN.

(DEFPROP GO P2GO P2)
(DEFUN P2GO (ARGL IGNORE)
       (COND ((NULL PROGDESCS)
	      (BARF `(GO ,ARGL) '|GO not inside a PROG| 'DATA))
	     ((ATOM (CAR ARGL))
	      (OUTB1 (CAR ARGL)))
	     (T (BARF `(GO ,ARGL) '|computed GO not implemented| 'DATA)) ))

(DEFPROP RETURN P2RETURN P2)
(DEFPROP MULTIPLE-VALUE-RETURN P2RETURN P2)
(DEFUN P2RETURN (ARGL IGNORE)
  (P2RETURN1 ARGL NIL))

(DEFUN P2RETURN1 (ARGL PROGNAME)
  (PROG ((RPDESC RETPROGDESC) IPROGDEST MVTARGET ARG LOSE NVALUES)
	(IF PROGNAME (SETQ RPDESC (ASSQ PROGNAME PROGDESCS)))
	(OR RPDESC
	    (RETURN
	      (IF PROGNAME (BARF `(RETURN-FROM ,PROGNAME . ,ARGL) '|unseen PROG name| 'WARN)
		  (BARF `(RETURN . ,ARGL) "not inside a PROG" 'WARN))))
	(COND ((= (LENGTH ARGL) 1)
	       (SETQ ARG (CAR ARGL)))
	      (T (SETQ ARG `(VALUES . ,ARGL))))
	(SETQ IPROGDEST (PROGDESC-IDEST RPDESC))
	(SETQ MVTARGET (PROGDESC-M-V-TARGET RPDESC))
	;; Compile the arg with same destination and m-v-target
	;; that the PROG we are returning from had.
	(SETQ LOSE (P2MV ARG IPROGDEST MVTARGET))
	;; But, since a PROG has multiple returns, we can't simply
	;; pass on to the PROG's caller whether this function did or did not
	;; generate those multiple values if desired.
	;; If the function failed to, we just have to compensate here.
	(AND LOSE
	     (COND ((NUMBERP MVTARGET)
		    ;; If we wanted N things on the stack, we have only 1, so push N-1 NILs.
		    (DO ((I 1 (1+ I))) ((= I MVTARGET))
		      (OUTF '(MISC D-PDL FALSE))))
		   ((EQ MVTARGET 'MULTIPLE-VALUE-LIST)
		    (OUTF '(MISC D-PDL NCONS)))))
	(SETQ NVALUES (COND ((NUMBERP MVTARGET) MVTARGET)
			    ((EQ IPROGDEST 'D-PDL) 1)
			    (T 0)))
	;; Note how many things we have pushed.
	(AND (EQ IPROGDEST 'D-PDL)
	     (MKPDLLVL (+ PDLLVL NVALUES)))
	(OUTBRET (PROGDESC-RETTAG RPDESC) RPDESC NVALUES)))

(DEFPROP RETURN-LIST P2RETURN-LIST P2)
(DEFUN P2RETURN-LIST (ARGL DEST)
  (P2RETURN `((VALUES-LIST ,(CAR ARGL))) DEST))

;Handle (RETURN-FROM <progname> <value>...) by binding RETPROGDESC to the spec'd prog.
(DEFPROP RETURN-FROM P2RETURN-FROM P2)
(DEFUN P2RETURN-FROM (ARGL IGNORE)
  (P2RETURN1 (CDR ARGL) (CAR ARGL)))

;(RETURN-FROM-T <value>) is like (RETURN-FROM T <value>).
(DEFPROP RETURN-FROM-T P2RETURN-FROM-T P2)
(DEFUN P2RETURN-FROM-T (ARGL IGNORE)
  (P2RETURN1 ARGL T))

;Unbind NBINDS special variables, unless IDEST is D-RETURN.
(DEFUN UNBIND (IDEST NBINDS)
       (OR (EQ IDEST 'D-RETURN)
	   (DO ((N 20 (+ N 20)))
	       ;; N is number of unbinds we would have done if we now
	       ;; unbind another 20.  N-20 is number unbound so far.
	       ;; Note that an UNBIND X instruction unbinds X+1 vars.
	       ((> N NBINDS)
		(OR (= NBINDS (- N 20))
		    (OUTI `(MISC D-IGNORE UNBIND ,(- NBINDS (- N 20) 1)))))
	       (OUTI '(MISC D-IGNORE UNBIND 17)))))

;Compile something to be addressed by an instruction.
;Return the address which the instruction can address it by.
;Can push the value on the stack and return (LPDL 77),
;or for a variable or constant can just return its address.
;DEST is significant only if it is D-IGNORE, in which case
;we compile code to compute and ignore the value.  What we return then is irrelevant.
(DEFUN P2-SOURCE (FORM DEST)
  (COND ((ATOM FORM)
	 `(SPECIAL ,FORM))
	((EQ (CAR FORM) 'LOCAL-REF)
	 (VAR-LAP-ADDRESS (CADR FORM)))
	((EQ (CAR FORM) 'LEXICAL-REF)
	 (COND ((NEQ DEST 'D-IGNORE)
		(P2PUSH-CONSTANT (CADR FORM))
		(OUTI '(MISC D-PDL %LOAD-FROM-HIGHER-CONTEXT))))
	 '(LPDL 77))
	((OR (EQ (CAR FORM) 'QUOTE) (EQ (CAR FORM) 'FUNCTION)
	     (EQ (CAR FORM) 'BREAKOFF-FUNCTION))
	 `(QUOTE-VECTOR ,FORM))
	(T (LET (BDEST M-V-TARGET)
	     (P2F FORM (COND ((EQ DEST 'D-IGNORE) 'D-IGNORE) (T 'D-PDL)))
	     '(LPDL 77)))))

(DEFUN P2PUSH-CONSTANT (NUMBER)
  (OUTI `(MOVE D-PDL (QUOTE-VECTOR (QUOTE ,NUMBER)))))

(DEFUN ADRREFP (EXP) 			       ;PREDICATE T IF CAN BE REF BY ADR ONLY
  (OR (ATOM EXP)
      (MEMQ (CAR EXP) '(QUOTE FUNCTION BREAKOFF-FUNCTION LOCAL-REF))))

(DEFUN MKPDLLVL (X)
       (COND ((> (SETQ PDLLVL X) MAXPDLLVL) (SETQ MAXPDLLVL PDLLVL))))

;Equivalent to (MKPDLLVL (1+ PDLLVL)) but call is just one word.
(DEFUN INCPDLLVL ()
    (SETQ MAXPDLLVL (MAX MAXPDLLVL (SETQ PDLLVL (1+ PDLLVL)))))

(DEFUN ARGLOAD (ARGL DEST)
       (PROG (IDEST)
	     (SETQ IDEST 'D-PDL)
	     (AND (EQ DEST 'D-IGNORE) (SETQ IDEST 'D-IGNORE))
	L    (COND ((NULL ARGL) (RETURN NIL)))
	     (P2 (CAR ARGL) IDEST)
	     (OR (EQ IDEST 'D-IGNORE) (INCPDLLVL))
	     (SETQ ARGL (CDR ARGL))
	     (GO L)))

(DEFUN P2ARGC (FCTN ARGL DESC DEST TARGET)	;NON-SYMBOLIC FCTN MEANS IT IS
							;AN ADDRESS FOR CALL INSTRUCTION
   (LET (COUNT TOKEN-LIST AG1 DSC1 IDEST CALLI TM TDEST LDEST
	       RESTART-PC ADI-LIST
	       (MVTARGET M-V-TARGET)
	       (CALL-BLOCK-PDL-LEVELS CALL-BLOCK-PDL-LEVELS))
	(PROG ()
;TDEST IS DESTINATION ACTUALLY TO BE COMPILED INTO CALL INSTRUCTION.
;LDEST IS "LOGICAL" DESTINATION.  THIS IS USUALLY THE SAME EXCEPT IN CASE OF MULTIPLE-VALUES.
; THEN TDEST IS ASSEMBLED D-IGNORE (IT IS ACTUALLY IGNORED BY THE MICRO-CODE, BUT DOING
; THIS CONFUSES THE MICRO-COMPILER LEAST), WHILE LDEST IS D-PDL, REFLECTING THE FACT THE
; VALUES ACTUALLY SHOW UP ON THE PDL.
	     (SETQ IDEST 'D-NEXT)
	     (SETQ CALLI (COND ((NULL ARGL) 'CALL0)
				(T 'CALL)))
	     (SETQ LDEST (SETQ TDEST DEST))		;MAY GET CHANGED TO D-PDL BELOW
	     ;; Whatever our caller wants in the way of multiple values,
	     ;; we will do it for him.  Say so.
	     (SETQ MVTARGET M-V-TARGET
		   M-V-TARGET NIL)
	     (COND ((NULL MVTARGET))
		   ((EQ MVTARGET 'MULTIPLE-VALUE-LIST)
		    (SETQ ADI-LIST
			  (CONS MVTARGET (CONS NIL ADI-LIST)))
		    (SETQ TDEST 'D-IGNORE LDEST 'D-PDL))
		   ((NUMBERP MVTARGET)
		    ;; MVTARGET is a number => it is number of values,
		    ;; just leave them on the stack.
		    (SETQ ADI-LIST `(MULTIPLE-VALUE (QUOTE-VECTOR ',MVTARGET) . ,ADI-LIST)
			  TDEST 'D-IGNORE LDEST 'D-PDL)))
	     (SETQ AG1 ARGL)
	     (SETQ DSC1 DESC)
	     (COND ((EQ FCTN '*CATCH)
			(SETQ ADI-LIST
			  (CONS 'RESTART-PC
			    (CONS `(QUOTE-VECTOR (TAG ,(SETQ RESTART-PC (GENSYM))))
				   (CONS 'BIND-STACK-LEVEL
					(CONS NIL ADI-LIST)))))
			(COND ((NULL MVTARGET)
				(SETQ LDEST (SETQ TDEST 'D-PDL)))))
					      ;CAUSES PROBLEMS IN MICRO-COMPILER
					      ;IF THIS FLUSHED.  IE RESTART PC TAG
					      ;WOULD HAVE TO GO IN MIDDLE OF RETURN
					      ;OPERATION
		   ((AND (MEMQ 'FEF-ARG-REST (SETQ TM (CADAR (LAST DSC1))))
			 (MEMQ 'FEF-QT-QT TM))
		    (SETQ CALLI 'CALL)
		    (SETQ ADI-LIST
			  (CONS 'FEXPR-CALL
				(CONS NIL ADI-LIST)))))
	     (COND ((NOT (SYMBOLP FCTN))
		    (SETQ TM FCTN))	 ;NON-SYMBOLIC FCTN, ITS ADDRESS FOR CALL
	           (T (SETQ TM `(QUOTE-VECTOR (FUNCTION ,TARGET)))))
	     (COND ((NULL ADI-LIST)
		      (OUTI (LIST CALLI TDEST TM)))
		   (T (OUTI1 (LIST 'ADI-CALL CALLI TDEST TM ADI-LIST))
		      (MKPDLLVL (+ PDLLVL (LENGTH ADI-LIST)))))
	     (COND ((NULL MVTARGET))
		   ((EQ MVTARGET 'MULTIPLE-VALUE-LIST)
		    (INCPDLLVL))
		   ((NUMBERP MVTARGET)
		    (MKPDLLVL (+ PDLLVL MVTARGET))))
	     (PUSH PDLLVL CALL-BLOCK-PDL-LEVELS)
	     (MKPDLLVL (+ 4 PDLLVL))
	L4   (COND ((NULL DSC1) (GO X2)))
	     (SETQ COUNT (CAAR DSC1))
	     (SETQ TOKEN-LIST (CADAR DSC1))
	     (COND ((MEMQ 'FEF-ARG-REST TOKEN-LIST)
		    (SETQ COUNT 1005)))
	L3   (COND ((NULL (CDR AG1))
		    (SETQ IDEST 'D-LAST)))
	     (COND ((= 0 COUNT) (SETQ DSC1 (CDR DSC1)) (GO L4))
		   ((AND (MEMQ 'FEF-ARG-REST TOKEN-LIST)
		         (MEMQ 'FEF-QT-QT TOKEN-LIST))
		      (GO OFEXPR))	  ;DO THIS EVEN IF ARG LIST IS NULL
		   ((NULL AG1) (GO RET))  ;OUT OF ARG LIST
		   ((MEMQ 'FEF-QT-QT TOKEN-LIST)
		    (OUTI `(MOVE ,IDEST (QUOTE-VECTOR (QUOTE ,(CAR AG1))))))
		   ((MEMQL '(FEF-QT-EVAL FEF-QT-DONTCARE) TOKEN-LIST)
		    (COND ((AND (EQ IDEST 'D-LAST)
				(MEMQ 'LEXPR-FUNCALL TOKEN-LIST))
			   (P2 (CAR AG1) 'D-PDL)	;Arg to %SPREAD
			   (OUTI (LIST 'MISC IDEST '%SPREAD)))
			  (T (P2 (CAR AG1) IDEST))))
		   (T (BARF TOKEN-LIST
			    'TOKEN-LIST-LOSES-P2
			    'BARF)))
	     (INCPDLLVL)
	     (SETQ AG1 (CDR AG1))
	     (SETQ COUNT (1- COUNT))
	     (GO L3)
	X2   (COND (AG1 (SETQ DSC1  '((1005 (FEF-ARG-OPT FEF-QT-EVAL))))  ;COMPILE THE REST
			(GO L4)))		;OF THEM, HE MAY POSSIBLY KNOW WHAT HES DOING
	RET  (COND (RESTART-PC
		    (SETQ DROPTHRU T)
		    (OUTF (LIST 'RESTART-TAG RESTART-PC))))
	     (COND ((NULL MVTARGET))
		   ((EQ MVTARGET 'MULTIPLE-VALUE-LIST))
		   ((NUMBERP MVTARGET) (RETURN NIL)))
	     (COND ((NOT (EQ LDEST DEST))	;INTERESTED IN WHERE VALUE IS, NOT WHAT WAS
		    (MOVE-RESULT-FROM-PDL DEST)))  ;ASSEMBLED INTO CALL
	     (COND ((AND (EQ DEST 'D-RETURN)
			 (NULL RESTART-PC))
	            (TAKE-DELAYED-TRANSFER)))
	     (RETURN NIL)

	OFEXPR
	     (OUTI `(MOVE D-LAST (QUOTE-VECTOR (QUOTE ,AG1))))
	     (GO RET)
)))

;Bind a list of variables, computing initializations and binding sequentially.
;VARS are the VARS outside of this binding environment.
;NEWVARS are the VARS inside of it, starting with the variables in X in reverse order,
;except there may be additional entries for optional-specified-flags; each one
;will be on NEWVARS just before its corresponding main variable.
;We have to install these variables one at a time as we go, using successive tails.
(DEFUN P2SBIND (X NEWVARS VARS)
  (LET ((NBINDS 0)			;Number of (internal-aux) special bindings
	(NNEWVARS (LOOP FOR L ON NEWVARS UNTIL (EQ L VARS) COUNT T)))
    (DO ((X X (CDR X)) (HOME))
        ((NULL X))
      (SETQ HOME (NTH (1- NNEWVARS) NEWVARS))
      (AND (P2LMB (CAR X) HOME) (SETQ NBINDS (1+ NBINDS)))
      ;; Set VARS to the tail of NEWVARS starting at the variable we just handled
      ;; or its optional-specified-flag.
      (SETQ NNEWVARS (1- NNEWVARS))
      (AND (CDDR (VAR-INIT HOME)) (SETQ NNEWVARS (1- NNEWVARS)))
      (SETQ VARS (NTHCDR NNEWVARS NEWVARS)))
    (OR (ZEROP NNEWVARS) (BARF X "VARS screwed up by this binding" 'BARF))
    NBINDS))

;Output code for binding the var VARNAME as specified in its HOME.
;Return T if a BINDPOP or BINDNIL instruction was output.
(DEFUN P2LMB (VARNAME HOME)
       (PROG (INTCODE INITFORM)
	     (COND ((NOT (ATOM VARNAME))
		    (SETQ INITFORM (CADR VARNAME))
		    (SETQ VARNAME (CAR VARNAME))))
	     (COND ((NOT (EQ (VAR-NAME HOME) VARNAME))
		    (BARF VARNAME "wrong home in P2LMB" 'BARF)))
	     (SETQ INTCODE (VAR-INIT HOME))
	     ;; If this variable's binding is fully taken care of by function entry,
	     ;; we have nothing to do here.
	     (COND ((NOT (MEMQ (CAR INTCODE) '(FEF-INI-OPT-SA FEF-INI-COMP-C)))
		    (RETURN NIL)))
	     ;; Detect and handle internal special bound variables.
	     (COND ((AND (EQ (VAR-KIND HOME) 'FEF-ARG-INTERNAL-AUX)
			 (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE)))
		    ;; Output BINDNIL, or push value and BINDPOP.
		    (COND ((MEMBER INITFORM '(NIL 'NIL))
			   (OUTIV 'BINDNIL HOME))
			  (T (P2PUSH INITFORM)
			     (OUTIV 'BINDPOP HOME)))
		    (RETURN T)))
             ;; Otherwise, it's an internal local variable,
             ;; or else a special variable already bound by entering the function.
             ;; Don't bind, just init.
	     (COND ((MEMBER INITFORM '(NIL 'NIL))
                    ;; if initting to NIL, then if no tags output so far (TAGOUT is NIL)
                    ;; we can assume it is still NIL from function entry time.
		    (COND ((OR TAGOUT (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE))
			       (VAR-OVERLAP-VAR HOME))
			   (OUTIV 'SETNIL HOME))))
		   ;; Initting var to itself;  do nothing.
		   ((AND (EQ (VAR-TYPE HOME) 'FEF-REMOTE)
			 (EQ INITFORM VARNAME)))
		   (T (P2PUSH INITFORM)
		      ;;IF &OPTIONAL AND FOR MICRO-COMPILER, JUST LEAVE VARIABLE ON STACK.
		      (COND ((AND GENERATING-MICRO-COMPILER-INPUT-P
				  (EQ (CAR INTCODE) 'FEF-INI-OPT-SA)))
			    (T (OUTIV 'POP HOME)))))
	     ;; If there is a specified-flag variable, it was bound to T at entry.
	     ;; Set it to NIL here (ie, if the arg was NOT specified).
	     (COND ((CDDR INTCODE)
		    (OUTIV 'SETNIL (CDDR INTCODE))))
	     (COND ((EQ (CAR INTCODE) 'FEF-INI-OPT-SA) (OUTF (CADR INTCODE))))
	     (RETURN NIL)))

(DEFUN OUTIV (INST VARAB) (OUTI (LIST INST 0 (VAR-LAP-ADDRESS VARAB))))

;Bind a list of variables "in parallel":  compute all values, then bind them all.
;Return the number of special bindings made (BINDPOP and BINDNIL instructions).
;Note: an attempt to bind NIL is ignored at this level.
;Note: if several variables have init forms of (%pop),
;they are popped off the pdl LAST ONE FIRST!
;The "correct" thing would be to pop the first one first,
;but this would require another stack to keep them on to reverse them.
(DEFUN P2PBIND (VARNAMES NEWVARS)
    (LET ((PDLLVL PDLLVL))
       (PROG (VARNAME HOME INTCODE INITFORM NBINDS)
	     (OR VARNAMES (RETURN 0))
	     (SETQ VARNAME (CAR VARNAMES)
		   VARNAMES (CDR VARNAMES))
	     (COND ((NOT (ATOM VARNAME))
		    (SETQ INITFORM (CADR VARNAME))
		    (SETQ VARNAME (CAR VARNAME))))
	     ;; If trying to bind NIL, just discard the value to bind it to.
	     (COND ((NULL VARNAME)
		    (P2 INITFORM 'D-PDL)
		    (RETURN (PROG1 (P2PBIND VARNAMES NEWVARS)
				   (OUTF '(MOVE D-IGNORE (LPDL 77)))))))
	     (COND ((NULL (SETQ HOME (ASSQ VARNAME NEWVARS)))
		    (BARF VARNAME 'NOT-ON-VARS 'BARF)))
	     (SETQ INTCODE (VAR-INIT HOME))
	     ;; If this variable's binding is fully taken care of by function entry,
	     ;; we have nothing to do here.
	     (COND ((AND (NOT (EQ (VAR-KIND HOME) 'FEF-ARG-INTERNAL-AUX))
			 (NOT (MEMQ (CAR INTCODE) '(FEF-INI-OPT-SA FEF-INI-COMP-C))))
		    (RETURN (P2PBIND VARNAMES NEWVARS))))
	     ;; Detect and handle internal special bound variables.
	     (COND ((AND (EQ (VAR-KIND HOME) 'FEF-ARG-INTERNAL-AUX)
			 (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE)))
		    ;; Output a BIND, or BINDNIL, or push value and BINDPOP.
		    (COND ((MEMBER INITFORM '(NIL 'NIL))
			   (SETQ NBINDS (P2PBIND VARNAMES NEWVARS))
			   (OUTIV 'BINDNIL HOME))
			  (T (P2PUSH INITFORM)
			     (INCPDLLVL)
			     (SETQ NBINDS (P2PBIND VARNAMES NEWVARS))
			     (OUTIV 'BINDPOP HOME)))
		    (RETURN (1+ NBINDS))))
	     (COND ((MEMBER INITFORM '(NIL 'NIL))
		    (SETQ NBINDS (P2PBIND VARNAMES NEWVARS))
		    (COND ((OR TAGOUT (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE))
			       (VAR-OVERLAP-VAR HOME))
			   (OUTIV 'SETNIL HOME))))
		   ;; Special vars bound at function entry and wanting to be
		   ;; initted to themselves, need not be set at all.
		   ((AND (EQ (VAR-TYPE HOME) 'FEF-SPECIAL)
			 (EQ INITFORM VARNAME))
		    (SETQ NBINDS (P2PBIND VARNAMES NEWVARS)))
		   (T (P2PUSH INITFORM)
		      (INCPDLLVL)
		      (SETQ NBINDS (P2PBIND VARNAMES NEWVARS))
;IF FOR MICRO-COMPILER AND IS OPTIONAL ARG, JUST LEAVE VARIABLE ON STACK.
		      (COND ((AND GENERATING-MICRO-COMPILER-INPUT-P
				  (MEMQ 'FEF-INI-OPT-SA INTCODE)))
			    (T (OUTIV 'POP HOME)))))
	     (COND ((EQ (CAR INTCODE) 'FEF-INI-OPT-SA) (OUTF (CADR INTCODE))))
	     (RETURN NBINDS))))

;Compile code to test CONDITION and jump to tag if it is NIL
;(for SENSE = TRUE) or if it is non-NIL (for SENSE = FALSE).
(DEFUN BOOL1 (CONDITION SENSE TAG)
    (P2BRANCH CONDITION 'D-INDS
	      `(BRANCH NILIND ,SENSE NIL ,TAG)))

;Like P2, but also supply a "branch destination".
;The branch destination (BDEST) is just a branch instruction which
;could simple-mindedly be compiled right after (P2 FORM DEST),
;but some forms can optimize the code produced by incorporating
;the branch destination into their code.  Such forms can say that
;outputting the branch at the end is superfluous by setting BDEST to NIL.
;Forms which perform unconditional transfers need not worry about BDEST
;since it will be output and then discarded as unreachable.

;An unconditional branch destination can accompany any value of DEST.
;A conditional branch should only be used with DEST = D-INDS.
;This is taken to imply that the indicators are used by the branch,
;not that the indicators will be correctly set up after the optimized
;code is finished branching or not.  If you wish to compile something
;and want the indicators correctly set up according to its value,
;you should use D-INDS with no BDEST, and do your branching yourself.

;Branches which pop the pdl may not be used as branch destinations.
;Most people who look at BDEST don't check for them,
;and the optimizations that BDEST is used for wouldn't work for them anyway.

;A funny kind of branch that can be used as a destination is
;(BRANCH ALWAYS NO-OP NIL tag).  It is a sort of unconditional branch,
;used when the tag to be branched to is known to be right after
;this expression, so that one might think that no branch is needed at all.
;When OUTB is called on such a branch, it does nothing.
;But some functions (such as AND and OR) can optimize these no-op branches
;like any other unconditional branches.

;An even funnier kind of branch destination is the return branch:
;(BRANCH ALWAYS RETURN NIL tag).  This is given as the branch destination
;to the last statement in a PROG, so that if the statement is a RETURN
;then the implicit (RETURN NIL) at the end of the PROG can be omitted
;and the RETURN at the end can just drop through to the PROG's rettag.
;Return branch destinations may not be passed along to subexpressions
;by AND, OR and COND.

(DEFUN P2BRANCH (FORM DEST BDEST)
    (AND (MEMQ DEST '(D-PDL D-NEXT))
	 (NEEDPDL 1))
    (COND ((AND BDEST (NEQ (CADR BDEST) 'ALWAYS)
		(NEQ DEST 'D-INDS))
	   (BARF `(,DEST . ,BDEST) '|BDEST is conditional and DEST is not D-INDS| 'BARF))
	  ;; We can optimize things like (AND 'T (GO FOO)) and (AND 'NIL (GO FOO))
	  ;; into an unconditional jump or into nothing at all.
	  ((AND (EQ (CADR BDEST) 'NILIND)
		(NULL (CADDDR BDEST))
		(NOT (ATOM FORM))
		(EQ (CAR FORM) 'QUOTE))
	   (AND (EQ (NULL (CADR FORM))
		    (EQ (CADDR BDEST) 'TRUE))
		(OUTB `(BRANCH ALWAYS NIL . ,(CDDDR BDEST))))
	   (SETQ BDEST NIL))
          ((ADRREFP FORM)
	   (OR (EQ DEST 'D-IGNORE)
	       (OUTI `(MOVE ,DEST ,(P2-SOURCE FORM DEST)))))
	  ((EQ (CAR FORM) 'LEXICAL-REF)
	   (P2 FORM DEST))
	  ((EQ (CAR FORM) '%POP)
	   (P2 FORM DEST))
	  (T (LET (M-V-TARGET) (P2F FORM DEST))))
    (AND BDEST (OUTB BDEST)))

(DEFPROP ATOM P2ATOM P2)
(DEFPROP LISTP P2ATOM P2)
(DEFPROP NLISPT P2ATOM P2)

;A call to ATOM which is then tested by a branch-if-non-nil, etc.,
;can be turned into just a branch-if-atom, etc.
(DEFUN P2ATOM (ARGL DEST)
    (COND ((EQ (CADR BDEST) 'NILIND)
	   (LET ((SENSE (CADDR BDEST)))
		(OR (EQ P2FN 'LISTP)
		    (SETQ SENSE (OTHER SENSE)))
		(P2BRANCH (CAR ARGL) DEST `(BRANCH ATOMIND ,SENSE . ,(CDDDR BDEST))))
	   (SETQ BDEST NIL))
	  (T (P2MISC P2FN ARGL DEST 1))))

;NOT compiles into a misc insn normally,
;but with a branch destination, it optimizes away by inverting the condition.
(DEFPROP NOT P2NOT P2)
(DEFUN P2NOT (ARGL DEST)
    (COND ((OR (EQ (CADR BDEST) 'NILIND) (EQ (CADR BDEST) 'ATOMIND))
	   (LET ((SENSE (OTHER (CADDR BDEST))))
		(P2BRANCH (CAR ARGL) DEST `(BRANCH ,(CADR BDEST) ,SENSE . ,(CDDDR BDEST))))
	   (SETQ BDEST NIL))
	  (T (P2MISC P2FN ARGL DEST 1))))

(DEFUN OTHER (SENSE)
       (COND ((EQ SENSE 'TRUE) 'FALSE)
	     ((EQ SENSE 'FALSE) 'TRUE)
	     (T (BARF SENSE 'BAD-ARG-OTHER 'BARF))))

(DEFPROP AND P2ANDOR P2)
(DEFPROP OR P2ANDOR P2)

(DEFUN P2ANDOR (ARGL DEST)
       (PROG (TAG UNCONDITIONAL IDEST SENSE TAG1)
	     (SETQ SENSE (COND ((EQ P2FN 'AND) 'TRUE)
			       (T 'FALSE)))
	     (SETQ IDEST 'D-PDL)
	     ;; RETURN branches can't be passed in to the last thing in an AND.
	     (AND (EQ (CADR BDEST) 'ALWAYS)
		  (EQ (CADDR BDEST) 'RETURN)
		  (SETQ BDEST NIL))
	     ;; Any non-null constant as arg in an AND is ignorable unless it is last.
	     ;; NIL as arg in an OR is always ignorable.
	     (SETQ ARGL (COND ((EQ SENSE 'FALSE) (DELETE '(QUOTE NIL) ARGL))
			      ((NULL ARGL) ARGL)
			      (T (NREVERSE (CONS (CAR (LAST ARGL))
						 (DEL #'(LAMBDA (IGNORE X)
							  (AND (NOT (ATOM X))
							       (EQ (CAR X) 'QUOTE)
							       (CADR X)))
						      NIL
						      (CDR (NREVERSE ARGL))))))))
	     (OR ARGL (RETURN (PROG1 (P2BRANCH `',(EQ SENSE 'TRUE) DEST BDEST)
				     (SETQ BDEST NIL))))
	     ;; If we are going to jump somewhere unconditionally after the AND,
	     ;; things which are NIL might as well jump conditionally straight there.
	     ;; But this only works if the value of the AND will be in the right place then.
	     (COND ((AND (EQ (CADR BDEST) 'ALWAYS)
			 (NULL M-V-TARGET)
			 (MEMBER DEST '(D-PDL D-INDS D-IGNORE 0)))
		    (SETQ UNCONDITIONAL T)
		    (SETQ TAG (CAR (CDDDDR BDEST))))
		   (T (SETQ TAG (GENSYM))))
	     (COND ((AND (NULL M-V-TARGET) (EQ DEST 'D-IGNORE))
		    ;; Compilation strategy for AND for effect:
		    ;; compute each arg, using it only to jump to end if it's NIL.
		    ;; The last one we just ignore, but we feed it our BDEST for
		    ;; branch tensioning.  However, (AND form (GO tag)) can be optimized
		    ;; by making it a conditional jump to tag rather than a jump around a jump.
		    (DO ((ARGL ARGL (CDR ARGL)))
			((NULL (CDR ARGL))
			 (P2BRANCH (CAR ARGL) DEST BDEST))
		       (AND (SIMPLEGOP (CADR ARGL))
			    (RETURN (BOOL1 (CAR ARGL) (OTHER SENSE) (GTAG (CADADR ARGL)))))
		       ;; If the next arg of this AND is NIL, this arg is effectively last.
		       ;; However, if AND has a branch destination, it must compute
		       ;; whether to branch based on the NIL, not on this arg.
		       (AND (NOT (ATOM (CADR ARGL)))
			    (EQ (CAADR ARGL) 'QUOTE)
			    (EQ (NULL (CADADR ARGL))
				(EQ SENSE 'TRUE))
			    (RETURN (P2BRANCH (CAR ARGL) DEST BDEST)))
		       (BOOL1 (CAR ARGL) SENSE TAG)))
		   ((AND (NULL M-V-TARGET) (EQ (CADR BDEST) 'NILIND))
		    ;; Compilation strategy for AND followed by jump if NIL:
		    ;; jump compute each value and jump THERE rather than to end if NIL.
		    ;; Compilation strategy for AND followed by jump if not NIL:
		    ;; put that jump if not NIL after the last thing in the AND
		    ;; and go to after that if anything else fails to be non-NIL.
		    (COND ((EQ SENSE (CADDR BDEST))
			   (DO ((ARGL ARGL (CDR ARGL))) ((NULL ARGL))
			       (P2BRANCH (CAR ARGL) DEST BDEST)))
			  (T (DO ((ARGL ARGL (CDR ARGL)))
				 ((NULL (CDR ARGL))
				  (P2BRANCH (CAR ARGL) DEST BDEST))
			       ;; If the next arg of this AND is NIL, this arg is effectively last.
			       ;; Also, BDEST can be flushed since it says branch if
			       ;; not NIL and we now know the value of the AND is always NIL.
			       (AND (NOT (ATOM (CADR ARGL)))
				    (EQ (CAADR ARGL) 'QUOTE)
				    (EQ (NULL (CADADR ARGL))
					(EQ SENSE 'TRUE))
				    (RETURN (P2 (CAR ARGL) DEST)))
			       (BOOL1 (CAR ARGL) SENSE TAG))))
		    (SETQ BDEST NIL))
		   (T
		    ;; Compilation strategy for AND for value
		    ;; (correct indicators required counts as for value):
		    ;; compile each arg, jumping to end if NIL.
		    ;; Compile them to indicators, or to pdl and pop if NIL.
		    ;; If compiling to indicators (no pushing), we can optimize
		    ;; (AND form (GO tag)) just as when we are ignoring the value.
                    (AND (EQ DEST 'D-INDS) (SETQ IDEST 'D-INDS))

		    ;; AND for multiple values is like AND for value on the stack,
		    ;; except that we can pass the M-V-TARGET along to the last form.
		    ;; Also, after the "end" where the failure branches branch to
		    ;; we put code to push N-1 extra NILs, or whatever.
		    ;; The code for the last form jumps around that, to the tag TAG1.
		    (AND M-V-TARGET (SETQ IDEST 'D-PDL))
		    (DO ((ARGL ARGL (CDR ARGL))
			 (BRANCH `(BRANCH NILIND ,SENSE ,(NEQ DEST 'D-INDS) ,TAG)))
			((NULL (CDR ARGL))
			 ;; Compile the last form.  If we want multiple values
			 ;; and it handles them, then say the AND is handling them.
			 (COND (M-V-TARGET
				(COND ((NULL (P2MV (CAR ARGL) IDEST M-V-TARGET))
				       (SETQ TAG1 (GENSYM)))))
			       (UNCONDITIONAL (P2BRANCH (CAR ARGL) DEST BDEST)
					      (SETQ BDEST NIL))
			       (T (P2 (CAR ARGL) 
				      (COND ((AND (EQ DEST 'D-RETURN)
						  (NOT GENERATING-MICRO-COMPILER-INPUT-P))
					     DEST)  ;OK TO DISTRIBUTE DOWN A D-RETURN, SINCE
						    ; IT IS AN IMPLICT TRANSFER
					    (T IDEST)))))) ;COMPILE TO IDEST, SINCE GOING TO
				    ;FALL INTO COMMON POINT WHICH EXPECTS RESULT THERE
		       (P2 (CAR ARGL) IDEST)
		       (AND (EQ IDEST 'D-INDS)
			    (SIMPLEGOP (CADR ARGL))
			    (RETURN (OUTB `(BRANCH NILIND ,(OTHER SENSE) NIL ,(GTAG (CADADR ARGL))))))
		       (OUTB BRANCH))))
	     (COND (TAG1
		    ;; If we want multiple values, and the last form provides them,
		    ;; say that the AND provides them,
		    ;; and arrange to produce some in every other path.
		    (OUTB `(BRANCH ALWAYS NIL NIL ,TAG1))  ;Last form jumps around.
		    (OUTTAG TAG)			;Other paths come here.
		    (COND ((NUMBERP M-V-TARGET)		;Turn single value into N values,
			   (DO I 1 (1+ I) (= I M-V-TARGET)
			     (OUTF '(MISC D-PDL FALSE))))
			  ((EQ M-V-TARGET 'MULTIPLE-VALUE-LIST) ;or into a list of values.
			   (OUTF '(MISC D-PDL NCONS))))
		    (SETQ M-V-TARGET NIL)
		    (OUTTAG TAG1))			;Last form jumps here.
		   ((NOT UNCONDITIONAL)
                    (OUTTAG TAG)
                    (OR (EQ DEST 'D-IGNORE)
                        (EQ DEST 'D-INDS)
                        (MOVE-RESULT-FROM-PDL DEST))))))

;Return T if given a (GO tag) which could be done with just a branch
;(doesn't require popping anything off the pdl).
(DEFUN SIMPLEGOP (FORM)
    (AND (NOT (ATOM FORM))
	 (EQ (CAR FORM) 'GO)
	 (= PDLLVL (GPDLLVL (CADR FORM)))))

(DEFPROP COND P2COND P2)
(DEFUN P2COND (ARGL DEST)
       (PROG (CLAUSE TAG TAG1 TAG2 VALF CLAUSE-LENGTH TM IDEST PRED NOFALLTHRU
	      LAST-CLAUSE-FLAG IDEST-USED)
	     (SETQ TAG2 (GENSYM))	;TAG TO GO TO WITH VALUE OF COND IN DEST
	     (SETQ TAG (GENSYM))	;TAG TO GO TO WITH VALUE OF COND IN IDEST
	     ;; Choose an intermediate destination, depending on ultimate destination.
	     ;; The intermediate destination can match the ultimate one
	     ;; if they are D-IGNORE, D-INDS or D-PDL.
	     ;; Each COND clause can compile its value to IDEST and go to TAG
	     ;; or compile its value to DEST and go to TAG2.

	     ;; Use of TAG and IDEST assumes that multiple values were NOT generated
	     ;; whereas TAG2 and DEST assumes that they were if they are supposed to be.

	     ;; For microcompiler input, we always use TAG and IDEST unless IDEST=DEST.
	     ;; Otherwise, we usually use DEST except for clauses that are just predicates.

	     ;; IDEST-USED is T if a clause has compiled its result to IDEST.
	     ;; The code to move the value is only generated if IDEST/TAG has been used.
	     (AND M-V-TARGET (SETQ DEST 'D-PDL))
	     (SETQ IDEST 'D-IGNORE)
	     (COND ((NOT (EQ DEST 'D-IGNORE))
		    (SETQ VALF T)
		    (SETQ IDEST 'D-PDL)))
	     (COND ((EQ DEST 'D-INDS) (SETQ IDEST 'D-INDS)))

	     ;; Compile next clause.
	L1   (COND ((NULL (CDR ARGL)) (SETQ LAST-CLAUSE-FLAG T)))
	     (SETQ CLAUSE (CAR ARGL))
	     (AND LAST-CLAUSE-FLAG (NULL (CDR CLAUSE))
		  (SETQ CLAUSE (CONS ''T CLAUSE)))
	     (SETQ TAG1 (GENSYM))
	     (SETQ PRED (CAR CLAUSE))
	     (COND ((AND (NOT (ATOM PRED))
			 (EQ (CAR PRED) 'QUOTE))
		    (COND ((NULL (CADR PRED))	;IS THE NULL CONDITION?
			   (AND (NOT LAST-CLAUSE-FLAG)
				(GO L5)))	;YEP.  CAN HAPPEN AS RESULT OF DO EXPANSION.
		          ((CDR ARGL)		;condition always true?
			   (SETQ LAST-CLAUSE-FLAG T)	;If so, discard any remaining clauses
			   (SETQ NOFALLTHRU T)	;after a warning about them.
			   (BARF (CDR ARGL)
				 '|unreachable COND clauses|
				 'WARN)
			   (SETQ ARGL (LIST CLAUSE)))
			  (T (SETQ NOFALLTHRU T)))))
	     (SETQ CLAUSE-LENGTH (LENGTH CLAUSE))
	     ;; Handle certain special cases of clauses.
	     (COND ((AND VALF (= 1 CLAUSE-LENGTH))
		    ;; Clause containing only one element, compiled for value.
		    ;; value of condition is also value of clause.
		    (P2 PRED IDEST)
		    (SETQ IDEST-USED T)
		    (OUTB (LIST 'BRANCH
				'NILIND
				'FALSE
				(EQ IDEST 'D-PDL)	;IF SOMETHING PUSHED, POP IF
				TAG))		; THE BRANCH IS NOT TAKEN
		    (GO L5))
		   ;; Clause of one element, if value is not wanted.
		   ((= 1 CLAUSE-LENGTH) (BOOL1 PRED 'FALSE TAG) (GO L5))
		   ;; Clause is just condition followed by a GO.
		   ((AND (= 2 CLAUSE-LENGTH)
			 (SIMPLEGOP (CADR CLAUSE)))
		    (BOOL1 PRED 'FALSE (GTAG (CADADR CLAUSE)))
		    (GO L5))
		   ;; Clause after this one is (T (GO ...)).
		   ;; Can get special handling only if the GO
		   ;; requires no pdl adjustment.
		   ((AND (NOT NOFALLTHRU)		;ISOLATE CASE OF
			 (NOT LAST-CLAUSE-FLAG)			;((P1 A1) (T (GO X)))
			 (NOT (ATOM (CAR (SETQ TM (CADR ARGL)))))
			 (EQ (CAAR TM) 'QUOTE)
			 (CADAR TM)
			 (= 2 (LENGTH TM))
			 (SIMPLEGOP (CADR TM)))
		    ;; In effect, we turn this into (COND ((NOT P1) (GO X)) (T A1))
		    (BOOL1 PRED 'TRUE (GTAG (CADADR TM)))  ;GO X DIRECTLY IF P1 FALSE
		    (SETQ ARGL (CONS (CONS ''T (CDR CLAUSE)) (CDDR ARGL)))
		    (GO L1))
		   ((NOT NOFALLTHRU)		;Normal COND clause.
		    (BOOL1 PRED 'TRUE TAG1)))	;Jump around clause if predicate fails.

	     ;; If the COND will have to return NIL if this clause's
	     ;; condition is false, then generate a clause to return the nil.
	     (COND ((AND VALF LAST-CLAUSE-FLAG (NOT NOFALLTHRU))
		    (SETQ ARGL (LIST CLAUSE '('T 'NIL)))
		    (SETQ LAST-CLAUSE-FLAG NIL)))

	     ;; Compile the actions of the cond clause, except for the last.
	     (DO ((ACTIONS (CDR CLAUSE) (CDR ACTIONS)))
		 ((NULL (CDR ACTIONS))
		  (SETQ CLAUSE ACTIONS))
	       (P2 (CAR ACTIONS) 'D-IGNORE))

	     ;; Compile last action of cond clause (the value).
	     (LET ((TO-IDEST-P
		     ;; Send value of last clause to IDEST rather than DEST
		     ;; if that means we can avoid a branch to TAG2
		     ;; that would otherwise be necessary.
		     ;; Send values of all clauses to IDEST for microcompiler input.
		     (OR (AND LAST-CLAUSE-FLAG
			      IDEST-USED
			      (NEQ DEST IDEST)
			      ;; Don't do this optimization if mult values wanted
			      ;; because only compilation to DEST can accept them.
			      (NULL M-V-TARGET)
			      ;; If D-RETURN, don't optimize, so it can propagate
			      ;; multiple values if there are any.
			      (NEQ DEST 'D-RETURN))
			 (AND GENERATING-MICRO-COMPILER-INPUT-P
			      (NOT (EQ DEST IDEST))))))
	       (COND (TO-IDEST-P (P2 (CAR CLAUSE) IDEST))
		     ((P2MV (CAR CLAUSE) DEST M-V-TARGET)
		      ;; If value fails to generate mult vals,
		      ;; we must make TAG generate them and go there.
		      (SETQ TO-IDEST-P T)))
	       (COND ((NULL TO-IDEST-P)
		      (COND ((OR (NULL LAST-CLAUSE-FLAG)
				 ;; If last clause, and TAG isn't the same as TAG2,
				 ;; we must still branch to TAG2.
				 (AND IDEST-USED (OR M-V-TARGET (NEQ DEST IDEST))))
			     (OUTB (LIST 'BRANCH 'ALWAYS NIL NIL TAG2)))))
		     (T
		      (SETQ IDEST-USED T)
		      (COND ((NULL LAST-CLAUSE-FLAG)
			     (OUTB (LIST 'BRANCH 'ALWAYS NIL NIL TAG)))) )))

	     ;; Here at end of cond-clause.
	L5   (OUTTAG TAG1)			;Output tag for jumps from failing predicate.
	     (COND ((SETQ ARGL (CDR ARGL))	;If there are more clauses, process them.
		    (GO L1)))

	     ;; There are no more cond clauses!
	     (OUTTAG TAG)
	     (AND IDEST-USED 
		  (COND ((NUMBERP M-V-TARGET)
			 (DOTIMES (I (1- M-V-TARGET))
			   (OUTI '(MISC D-PDL FALSE))))
			((EQ M-V-TARGET 'MULTIPLE-VALUE-LIST)
			 (OUTI '(MISC D-PDL NCONS)))
			((NEQ DEST IDEST)
			 (MOVE-RESULT-FROM-PDL DEST))))
	     ;; We have generated multiple values if necessary.
	     (SETQ M-V-TARGET NIL)
	     (OUTTAG TAG2)
	     (RETURN NIL)

))

(DEFUN GOTAGS-SEARCH (TAG) (OR (ASSQ TAG GOTAGS) (BARF TAG '|Unknown GO-tag| 'DATA)))

(DEFUN GPDLLVL (X) (GOTAG-PDL-LEVEL (GOTAGS-SEARCH X)))

(DEFUN GTAG (X) (GOTAG-LAP-TAG (GOTAGS-SEARCH X)))

;Output an unconditional jump to a specified tag, popping the pdl if necessary.
;Barf if the tag is not known on GOTAGS.
(DEFUN OUTB1 (TAG)
    (OUTBRET TAG NIL 0))

;Output an unconditional transfer to the specified prog tag,
;popping the pdl the appropriate number of times to adjust the
;pdl from its current level to the level required at that tag.

;For handling GO, PROGDESC should be NIL and NVALUES should be 0.
;When jumping to the return tag of a prog, PROGDESC should be
;the desc for the prog we are returning from, and NVALUES should be
;the number of things on the top of the stack which are being left
;there as values to return from the prog.
(DEFUN OUTBRET (TAG PROGDESC NVALUES)
    (PROG (TM (EXITPROGDESC PROGDESC))
	  (SETQ TM (GOTAGS-SEARCH TAG))
	  ;; If this is GO, set EXITPROGDESC to the progdesc of its containing PROG
	  (OR PROGDESC (SETQ EXITPROGDESC (GOTAG-PROGDESC TM)))
	  ;; If we are exiting any PROGs, unwind stacks to their levels.
	  ;; Does not include the prog whose desc is EXITPROGDESC.
	  (LET ((N-UNBINDS 0) LAST-VARIABLE-UNBIND-PDL-LEVEL)
	    (DO ((L PROGDESCS (CDR L)))
		((EQ (CAR L) EXITPROGDESC))
	      (COND ((LISTP (PROGDESC-NBINDS (CAR L)))
		     (SETQ N-UNBINDS (CAR (PROGDESC-NBINDS (CAR L))))
		     (SETQ LAST-VARIABLE-UNBIND-PDL-LEVEL (PROGDESC-PDL-LEVEL (CAR L))))
		    (T (SETQ N-UNBINDS (+ N-UNBINDS (PROGDESC-NBINDS (CAR L)))))))
	    ;; LAST-VARIABLE-UNBIND-PDL-LEVEL is the level at start of PROG body,
	    ;; and does not include the values we want to return.
	    ;; PDLLVL at all times includes those values
	    ;; since they are already on the stack.
	    (COND (LAST-VARIABLE-UNBIND-PDL-LEVEL
		   (POPPDL NVALUES (- PDLLVL NVALUES LAST-VARIABLE-UNBIND-PDL-LEVEL))
		   (OUTPUT-UNBIND-TO-INDEX NVALUES)
		   (MKPDLLVL (+ LAST-VARIABLE-UNBIND-PDL-LEVEL NVALUES -1))))
	    (UNBIND 'D-IGNORE N-UNBINDS))
	  ;; For a prog rettag, the pdl level should include
	  ;; the number of values desired on the stack.
	  (POPPDL NVALUES (- PDLLVL (GOTAG-PDL-LEVEL TM)))
	  ;; If we are a RETURN at the end of a PROG,
	  ;; (ie, where we would jump to is going to be .+1),
	  ;; then don't jump, but do let the prog know
	  ;; that it should NOT output a (MOVE dest 'NIL) instruction.
	  (AND PROGDESC
	       (EQ (CADR BDEST) 'ALWAYS)
	       (EQ (CADDR BDEST) 'RETURN)
	       (EQ (CAR (CDDDDR BDEST)) TAG)
	       (RETURN (SETQ PROG-RETURN-DROPTHRU T)))
	  (OUTB `(BRANCH ALWAYS NIL NIL ,(GOTAG-LAP-TAG TM)))))

;Pop NPOPS words off the pdl, from underneath the top NVALUES words.
;We do not change PDLLVL.
(DEFUN POPPDL (NVALUES NPOPS)
  (COND ((MINUSP NPOPS)
	 (BARF NPOPS `|negative number of pops| 'BARF))
	((NOT (ZEROP NPOPS))
	 (COND ((> NVALUES 1)
		(OUTI `(MOVE D-PDL (QUOTE-VECTOR ',NPOPS)))
		(OUTI `(MOVE D-PDL (QUOTE-VECTOR ',NVALUES)))
		(OUTI '(MISC D-IGNORE POP-M-FROM-UNDER-N)))
	       ((= NVALUES 1)
		(OUTI `(MOVE D-PDL (QUOTE-VECTOR ',NPOPS)))
		(OUTI '(MISC D-PDL SHRINK-PDL-SAVE-TOP)))
	       (T
		 (DO ((N 17 (+ N 17)))
		     ;; N is number of pops we would have done if we now do
		     ;; another POPPDL 17.  N-17 is number of pops so far.
		     ((> N NPOPS)
		      (OR (= NPOPS (- N 17))
			  (OUTI `(MISC D-IGNORE POPPDL ,(- NPOPS (- N 17))))))
		   (OUTI '(MISC D-IGNORE POPPDL 17))))))))

;Output code to unbind to a specpdl index saved on the stack
;underneath N values.  The code pops that one word out of the stack
;but we do not change PDLLVL.
(DEFUN OUTPUT-UNBIND-TO-INDEX (NVALUES)
  (COND ((= NVALUES 0)
	 (OUTI '(MISC D-IGNORE UNBIND-TO-INDEX)))
	((= NVALUES 1)
	 (OUTI '(MISC D-PDL UNBIND-TO-INDEX-MOVE)))
	(T (OUTI `(MOVE D-PDL (QUOTE-VECTOR ',NVALUES)))
	   (OUTI '(UNBIND-TO-INDEX-UNDER-N)))))

(DEFUN OUTI (X)
       (PROG NIL
	     (COND ((NULL DROPTHRU)
		    (RETURN (OUTF (LIST 'COMMENT X))))
		   ((AND (EQ (CADR X) 'D-RETURN)
			(NOT (EQ (CAR X) 'CALL)))
		    (SETQ DROPTHRU NIL)))
	     (OUTF X)))

(DEFUN OUTI1 (X)			;USE THIS FOR OUTPUTING INSTRUCTIONS
  (PROG NIL				;KNOWN TO TAKE DELAYED TRANSFERRS
	(COND ((NULL DROPTHRU)
		(RETURN (OUTF (LIST 'COMMENT X)))))
	(OUTF X)))

(DEFUN TAKE-DELAYED-TRANSFER NIL	;CALL THIS WHEN ARGS TO LIST OR CALL COMPLETED
	(SETQ DROPTHRU NIL))

(DEFUN OUTB (X)
       (COND ((EQ (CADDR X) 'NO-OP) (OUTF (LIST 'COMMENT X)))
	     ((EQ (CADDR X) 'RETURN) (OUTF (LIST 'COMMENT X)))
	     ((NULL DROPTHRU) (OUTF (LIST 'COMMENT X)))
	     (T (COND ((EQ (CADR X) 'ALWAYS)
		       (SETQ DROPTHRU NIL)))
		(PUTPROP (CAR (LAST X)) T 'USED)
		(OUTF X)))) 	

;BRANCH INDICATOR SENSE POPONNOJUMP TAG BRANCH
;OCCURS IN C(IND) = SENSE

(DEFUN OUTTAG (X)
       (COND ((GET X 'USED)
              (OR DROPTHRU (OUTF '(NO-DROP-THROUGH)))
              (SETQ DROPTHRU T)
              (OUTF X))))

;;; Lowest level output routine:  Output one object to the lap code.
;;; If HOLDPROG is NIL, we just print it on the terminal.
;;; Otherwise, it is stuck into QCMP-OUTPUT.

(IF-FOR-MACLISP
 (DEFUN OUTF (X)
       (COND ((NULL HOLDPROG)
	      (PRINT X)
	      (AND (NOT (ATOM X)) (CDR X) (CDDR X) (UNMADR (CADDR X))))
	     (T (SETQ QCMP-OUTPUT (NCONC QCMP-OUTPUT (LIST X)))))) )


(IF-FOR-LISPM
(DEFUN OUTF (X)
       (COND ((NULL HOLDPROG)
	      (PRINT X)
	      (AND (NOT (ATOM X)) (CDR X) (CDDR X) (UNMADR (CADDR X))))
	     ((ARRAY-PUSH (FUNCTION QCMP-OUTPUT) X))
	     (T (ADJUST-ARRAY-SIZE (FUNCTION QCMP-OUTPUT)
				   (* 2 (ARRAY-DIMENSION-N 1 (FUNCTION QCMP-OUTPUT))))
		(OUTF X))	;TRY AGAIN
)) )

;;; ARG DESC LIST -- A LIST OF LISTS
;;; EA LIST (<REPEAT-COUNT> <TOKEN-LIST>)
;;; TOKEN LIST HAS THINGS LIKE FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST,
;;; AND FEF-QT-EVAL FEF-QT-QT.

(IF-FOR-MACLISP
 (DEFUN GETARGDESC (X)
  (PROG (TEM)
	(COND ((SETQ TEM (GET X 'ARGDESC)) (RETURN TEM))
       	      ((SETQ TEM (GET X 'QINTCMP))
		(RETURN (LIST (CONS TEM '((FEF-ARG-REQ FEF-QT-EVAL))))))
	      ((SETQ TEM (GET (COND ((GET X 'Q-HEAD-POINTER)) (T X)) 'EXPR))
		(RETURN (COND ((ATOM TEM) (GETARGDESC TEM))
			      (T (GET-ARGDESC-PROP-FROM-LAMBDA-LIST (CADR TEM))))))
	      ((SETQ TEM (GET X 'Q-ARGS-PROP))
		(RETURN (GET-ARGDESC-PROP-FROM-Q-ARGS-PROP TEM X)))
	      ((GETL X '(FEXPR *FEXPR))
		(RETURN '((1 (FEF-ARG-REST FEF-QT-QT)))))
	      ((GET X 'FSUBR)
	        (BARF X '|reference to random FSUBR; this probably isn't going to win| 'WARN)
		(RETURN '((1 (FEF-ARG-REST FEF-QT-QT)))))
	      ((SETQ TEM (ARGS X))
	       (RETURN (COND ((NULL (CAR TEM))
			      (LIST (CONS (CDR TEM) '((FEF-ARG-REQ FEF-QT-EVAL)))))
			     (T (LIST (CONS (CAR TEM) '((FEF-ARG-REQ FEF-QT-EVAL)))
				      (CONS (CDR TEM) '((FEF-ARG-OPT FEF-QT-EVAL))))))))
	      (T (RETURN '((1005 (FEF-ARG-OPT FEF-QT-EVAL))))) ))))

(IF-FOR-LISPM
(DEFUN GETARGDESC (X)		;second value on LISPM T if this is a guess.
  (PROG (TEM)
    (COND ((SETQ TEM (GET X 'ARGDESC))
	   (RETURN TEM))
	  ((SETQ TEM (GET X 'QINTCMP))
	   (RETURN (LIST (CONS TEM '((FEF-ARG-REQ FEF-QT-EVAL))))))
	  ((SETQ TEM (GET X 'Q-ARGS-PROP))
	   (RETURN (GET-ARGDESC-PROP-FROM-Q-ARGS-PROP TEM X)))
	  ((FBOUNDP X)
	   (SETQ TEM (FSYMEVAL X))
	   (RETURN (COND ((SYMBOLP TEM)
			  (GETARGDESC TEM))
			 ((LISTP TEM)
			  (COND ((EQ (CAR TEM) 'LAMBDA)
				 (GET-ARGDESC-PROP-FROM-LAMBDA-LIST (CADR TEM)))
				((EQ (CAR TEM) 'NAMED-LAMBDA)
				 (GET-ARGDESC-PROP-FROM-LAMBDA-LIST (CADDR TEM)))
				(T '((1005 (FEF-ARG-OPT FEF-QT-EVAL))))))
			 ((AND (TYPEP TEM ':COMPILED-FUNCTION)
			       (SETQ TEM (GET-MACRO-ARG-DESC-POINTER TEM)))
			  ;; Use ADL in preference to %ARGS-INFO so that we
			  ;; find things like FEF-ARG-FUNCTIONAL.
			  ;; The only reason we would have an ADL if the
			  ;; %ARGS-INFO would otherwise be correct
			  ;; is if things like FEF-ARG-FUNCTIONAL are present.
			  (GET-ARGDESC-PROP-FROM-ADL TEM))
			 (T
			  (SETQ TEM (%ARGS-INFO X))
			  (COND ((BIT-TEST %ARG-DESC-INTERPRETED TEM)
				 '((1005 (FEF-ARG-OPT FEF-QT-EVAL))))
				(T (GET-ARGDESC-PROP-FROM-Q-ARGS-PROP TEM X))))))))
    (RETURN '((1005 (FEF-ARG-OPT FEF-QT-EVAL))) T))))

(DECLARE (SPECIAL %%ARG-DESC-MIN-ARGS %%ARG-DESC-MAX-ARGS %ARG-DESC-FEF-QUOTE-HAIR
		%ARG-DESC-QUOTED-REST %ARG-DESC-EVALED-REST))

(DEFUN GET-ARGDESC-PROP-FROM-Q-ARGS-PROP (ARG-PROP FN-NAME)
  (PROG (ANS MIN-ARGS OPT-ARGS)
	(COND ((NOT (= 0 (LOGAND %ARG-DESC-FEF-QUOTE-HAIR ARG-PROP)))
	       (GET-ARGDESC-PROP-FROM-ADL (GET-MACRO-ARG-DESC-POINTER (FSYMEVAL FN-NAME)))))
	(COND ((NOT (= 0 (SETQ MIN-ARGS (#M LOGLDB #Q LDB %%ARG-DESC-MIN-ARGS ARG-PROP))))
		(SETQ ANS (NCONC ANS (LIST (CONS MIN-ARGS '((FEF-ARG-REQ FEF-QT-EVAL))))))))
	(COND ((NOT (= 0 (SETQ OPT-ARGS (- (#M LOGLDB #Q LDB %%ARG-DESC-MAX-ARGS ARG-PROP) MIN-ARGS))))
		(SETQ ANS (NCONC ANS (LIST (CONS OPT-ARGS '((FEF-ARG-OPT FEF-QT-EVAL))))))))
	(COND ((NOT (= 0 (LOGAND %ARG-DESC-QUOTED-REST ARG-PROP)))
		(SETQ ANS (NCONC ANS (LIST '(1 (FEF-ARG-REST FEF-QT-QT)))))))
	(COND ((NOT (= 0 (LOGAND %ARG-DESC-EVALED-REST ARG-PROP)))
		(SETQ ANS (NCONC ANS (LIST '(1 (FEF-ARG-REST FEF-QT-EVAL)))))))
	(RETURN ANS)))

(DEFUN GET-ARGDESC-PROP-FROM-LAMBDA-LIST (LL)
   (PROG (ANS QUOTE-STATUS REST-FLAG OPT-FLAG TOKEN-LIST NEXT-ELEMENT)
	(SETQ QUOTE-STATUS '&EVAL)
    L0	(SETQ TOKEN-LIST NIL)
    L1  (OR LL (RETURN ANS))
	(SETQ NEXT-ELEMENT (CAR LL) LL (CDR LL))
    	(COND ((EQ NEXT-ELEMENT '&AUX)
	       (RETURN ANS))
	      ((EQ NEXT-ELEMENT '&OPTIONAL) (SETQ OPT-FLAG T) (GO L1))
	      ((EQ NEXT-ELEMENT '&FUNCTIONAL)
	       (SETQ TOKEN-LIST (CONS 'FEF-FUNCTIONAL-ARG TOKEN-LIST))
	       (GO L1))
	      ((MEMQ NEXT-ELEMENT '(&EVAL &QUOTE &QUOTE-DONTCARE))
		(SETQ QUOTE-STATUS NEXT-ELEMENT)
		(GO L1))
	      ((OR (EQ NEXT-ELEMENT '&REST) (EQ NEXT-ELEMENT '&KEY))
		(SETQ REST-FLAG T)
		(GO L1))
	      ((MEMQ NEXT-ELEMENT LAMBDA-LIST-KEYWORDS)
	       (GO L1)))
    	(PUSH (CDR (ASSQ QUOTE-STATUS '(
		(&EVAL . FEF-QT-EVAL) (&QUOTE . FEF-QT-QT)
		(&QUOTE-DONTCARE . FEF-QT-DONTCARE)  )))
	      TOKEN-LIST)
	(PUSH (COND (REST-FLAG 'FEF-ARG-REST)
		    ((NULL OPT-FLAG) 'FEF-ARG-REQ)
		    (T 'FEF-ARG-OPT))
	      TOKEN-LIST)
	(SETQ ANS (NCONC ANS (LIST
			      (LIST 1 TOKEN-LIST))))
	(COND (REST-FLAG (RETURN ANS)))
	(GO L0)))

(DEFUN GET-ARGDESC-PROP-FROM-ADL (ADL)
  (LET (ARGDESC)
    (DO ((L ADL (CDR L))
	 ITEM
	 SYNTAX
	 QUOTE)
	((NULL L) (NREVERSE ARGDESC))
      (SETQ ITEM (CAR L))
      (AND (BIT-TEST %FEF-NAME-PRESENT ITEM) (SETQ L (CDR L)))
      (SETQ SYNTAX (MASK-FIELD %%FEF-INIT-OPTION ITEM))	;SKIP EXTRA INIT Q
      (OR (= SYNTAX FEF-INI-NONE) (= SYNTAX FEF-INI-NIL) (= SYNTAX FEF-INI-SELF)
	  (SETQ L (CDR L)))
      (SETQ SYNTAX (MASK-FIELD %%FEF-ARG-SYNTAX ITEM))
      (SETQ QUOTE
	    (COND ((> (MASK-FIELD %%FEF-QUOTE-STATUS ITEM) FEF-QT-EVAL)
		   '(FEF-QT-QT))
		  (T '(FEF-QT-EVAL))))
      (AND (BIT-TEST FEF-FUNCTIONAL-ARG ITEM) (PUSH 'FEF-FUNCTIONAL-ARG QUOTE))
      (COND ((> SYNTAX FEF-ARG-REST)
	     (RETURN (NREVERSE ARGDESC)))
	    ((= SYNTAX FEF-ARG-REST)
	     (RETURN (NRECONC ARGDESC `((1 (FEF-ARG-REST . ,QUOTE))))))
	    ((= SYNTAX FEF-ARG-OPT)
	     (PUSH `(1 (FEF-ARG-OPT . ,QUOTE)) ARGDESC))
	    (T (PUSH `(1 (FEF-ARG-REQ . ,QUOTE)) ARGDESC))))))

;;;Testing functions

;Given the lap address of a variable, print out the name of the variable in a comment.
;Used when compiling a function and printing the lap code on the terminal.
(DEFUN UNMADR (X)
  (COND ((AND (NOT (ATOM X)) (MEMQ (CAR X) '(ARG LOCBLOCK)))
	 (DO ((VS ALLVARS (CDR VS))) ((NULL VS) NIL)
	    (AND (EQUAL X (VAR-LAP-ADDRESS (CAR VS)))
		 (PROGN (PRINC '|  ;|)
			(PRIN1 (VAR-NAME (CAR VS)))
			(RETURN (VAR-NAME (CAR VS)))))))))
