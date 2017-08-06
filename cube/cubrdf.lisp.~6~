;;; -*- Mode: LISP;  Package: CUBE -*-
(defun multicsp macro (ignore) nil)

(defun itsp macro (ignore) nil)

(defmacro array* (&rest ignore) nil)

(DEFUN ARRAY (&QUOTE X TYPE &EVAL &REST DIMLIST)
    (APPLY (FUNCTION *ARRAY) (CONS X (CONS TYPE DIMLIST))))

(DEFUN *ARRAY (X TYPE &REST DIMLIST &AUX ARRAY)
    (AND (MEMQ TYPE '(READTABLE OBARRAY))
	 (FERROR NIL "The array type ~S is not yet in" TYPE))
    (SETQ ARRAY
	  (MAKE-ARRAY NIL 'ART-Q DIMLIST))
    (COND ((EQ TYPE 'FIXNUM) (FILLARRAY ARRAY '(0)))
	  ((EQ TYPE 'FLONUM) (FILLARRAY ARRAY '(0.0))))
    (COND ((NULL X)
	   ARRAY)
	  ((SYMBOLP X)
	   (RPLACA (FUNCTION-CELL-LOCATION X) ARRAY)
	   X)
	  (T (FERROR NIL "~S is not a legal first arg for *ARRAY" X))))

