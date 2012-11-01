;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.24
;;; Reason: Problem with EQUAL-HASH-TABLEs.
;;; Written 12/20/81 19:37:55 by DLA,
;;; while running on Lisp Machine Filecomputer from band 5
;;; with System 78.23, ZMail 38.4, Experimental Local-File 31.0, microcode 841.



; From file HASH > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFUN REMHASH-EQUAL (KEY HASH-TABLE &AUX HASH SIZE)
  (SETQ HASH (ABS (SXHASH KEY))
	SIZE (EQUAL-HASH-TABLE-SIZE HASH-TABLE))
  (DO ((IDX (\ HASH SIZE) (\ (1+ IDX) SIZE))
       (COUNT SIZE (1- COUNT))
       (TEM))
      ((ZEROP COUNT))   ;Avoid infinite loop if whole hash table filled with -1.
    (OR (SETQ TEM (EQUAL-HASH-TABLE-HASH HASH-TABLE IDX))
	(RETURN NIL))				;Not found
    (COND ((AND (= TEM HASH) (EQUAL KEY (EQUAL-HASH-TABLE-KEY HASH-TABLE IDX)))
	   (SETF (EQUAL-HASH-TABLE-HASH HASH-TABLE IDX) -1)
	   (SETF (EQUAL-HASH-TABLE-KEY HASH-TABLE IDX) NIL)
	   (SETF (EQUAL-HASH-TABLE-VALUE HASH-TABLE IDX) NIL)
	   (SETF (EQUAL-HASH-TABLE-FULLNESS HASH-TABLE)
		 (1- (EQUAL-HASH-TABLE-FULLNESS HASH-TABLE)))
	    ;If the guy following this one is NIL, can make this NIL too and 
	    ;continue clobbering all consecutive previous -1s to NIL. 
	   (OR (EQUAL-HASH-TABLE-HASH HASH-TABLE (\ (1+ IDX) SIZE))
	       (DO ((IDX IDX (IF (ZEROP IDX) (1- SIZE) (1- IDX))))
		   ((NOT (EQ (EQUAL-HASH-TABLE-HASH HASH-TABLE IDX) -1)))
		 (SETF (EQUAL-HASH-TABLE-HASH HASH-TABLE IDX) NIL)))
	   (RETURN T)))))

(DEFUN GETHASH-EQUAL (KEY HASH-TABLE &AUX HASH SIZE TEM)
  (SETQ HASH (ABS (SXHASH KEY))
	SIZE (EQUAL-HASH-TABLE-SIZE HASH-TABLE))
  (DO ((IDX (\ HASH SIZE) (\ (1+ IDX) SIZE))
       (COUNT SIZE (1- COUNT)))
      ((ZEROP COUNT)	  ;Under perverse conditions, entire hash table could fill with
       (VALUES NIL NIL))  ; -1.  This avoids an infinite loop in that case.
    (OR (SETQ TEM (EQUAL-HASH-TABLE-HASH HASH-TABLE IDX))
	(RETURN NIL NIL))			;Not found
    (AND (= TEM HASH) (EQUAL KEY (EQUAL-HASH-TABLE-KEY HASH-TABLE IDX))
	 (RETURN (EQUAL-HASH-TABLE-VALUE HASH-TABLE IDX) T))))

)

