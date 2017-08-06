;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.25
;;; Reason: Uncomment code in SYMBOL-FROM-STRING to go with 78.23
;;; Written 12/22/81 13:59:15 by MMcM,
;;; while running on Retriever from band 6
;;; with System 78.24, ZMail 38.4, Symbolics 8.7, Tape 6.2, LMFS 21.15, Canon 9.3, microcode 841.



; From file SECTIO.LISP >ZWEI POINTER:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

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
		 (:ALWAYS-METHOD
		  (SETQ SYM (CONS ':METHOD SPEC)))
		 ((:METHOD :HANDLER :MAYBE-METHOD)
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
				(SYMBOLP (CAR SPEC)) (SYMBOLP (CADR SPEC))
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
		       (SYMBOLP (CAR SPEC)) (SYMBOLP (CADR SPEC))
		       (SETQ SYM (CONS TYPE SPEC)))))
	       ;; Something we don't understand, make a bogus symbol to use as a property
	       ;; list to remember the location of this definition
	       (SETQ SYM (INTERN STR *UTILITY-PACKAGE*))))
	 (IF (NOT (SYS:VALIDATE-FUNCTION-SPEC SYM))
	     (VALUES NIL NIL T)
	     (VALUES SYM STR)))))

)
