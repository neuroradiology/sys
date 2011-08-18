;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.41
;;; Reason: Scroll window mouse items to be able to be :BUTTONS.
;;; Written 12/29/81 10:33:17 by BSG,
;;; while running on Terrier from band 2
;;; with System 78.40, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.24, Canon 9.8, microcode 841.



; From file SCROLL.LISP >LMWIN POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFMETHOD (ESSENTIAL-SCROLL-MOUSE-MIXIN :MOUSE-CLICK) (BUTTON X Y &AUX ITEM TYPE OP)
  (COND ((NOT (LDB-TEST %%KBD-MOUSE-N-CLICKS BUTTON))
	 (MULTIPLE-VALUE (ITEM TYPE)
	   (FUNCALL-SELF ':MOUSE-SENSITIVE-ITEM X Y))
	 (COND ((NULL ITEM)
		(PROCESS-RUN-FUNCTION "Mouse select" #'MOUSE-SELECT SELF))
	       ((OR (NULL TYPE)
		    (SETQ OP (FIRST (CDR (ASSQ TYPE TYPE-ALIST)))))

		;;psych out :BUTTONS --- Copy of code in (TV:BASIC-MENU :MOUSE-BUTTONS)
		(COND ((AND (LISTP ITEM)
			    ( (LENGTH ITEM) 3)
			    (EQ (SECOND ITEM) ':BUTTONS))
		       (SETQ ITEM (NTH (LDB %%KBD-MOUSE-BUTTON BUTTON) (THIRD ITEM)))))

		(BLINKER-SET-VISIBILITY ITEM-BLINKER NIL)
		(FUNCALL-SELF ':EXECUTE (IF OP
					    (LIST* NIL OP ITEM)
					  ITEM)))
	       (T
		(FUNCALL-SELF ':FORCE-KBD-INPUT (LIST TYPE ITEM SELF BUTTON))))
	 T)))

)
