;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.45
;;; Reason: Don't get parity error booting freshly powered-on machine
;;; Written 1/07/82 19:37:34 by Moon,
;;; while running on Beagle from band 3
;;; with System 78.44, ZMail 38.5, Symbolics 8.9, Tape 6.5, LMFS 21.31, Canon 9.11, microcode 841.



; From file DISK.LISP >LMIO POINTER:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;Must be defined before initialization below
(DEFUN WIRE-PAGE (ADDRESS &OPTIONAL (WIRE-P T) SET-MODIFIED DONT-BOTHER-PAGING-IN)
  (IF WIRE-P
      (DO ()
	  ((%CHANGE-PAGE-STATUS ADDRESS %PHT-SWAP-STATUS-WIRED NIL)
	   (IF SET-MODIFIED			;Set modified bit without changing anything
	       (IF DONT-BOTHER-PAGING-IN	;and without touching uninitialized memory
		   (%P-STORE-TAG-AND-POINTER ADDRESS DTP-TRAP ADDRESS)
		   (%P-STORE-DATA-TYPE ADDRESS (%P-DATA-TYPE ADDRESS)))))
	(COND ((NOT DONT-BOTHER-PAGING-IN)
	       (%P-LDB 1 (%POINTER ADDRESS)))	;Haul it in
	      ((NULL (%PAGE-STATUS ADDRESS))
	       (WITHOUT-INTERRUPTS		;Try not to get aborted
		 (LET ((PFN (%FINDCORE)))
		   (OR (%PAGE-IN PFN (LSH ADDRESS -8))
		       ;Page already got in somehow, free up the PFN
		       (%CREATE-PHYSICAL-PAGE (LSH PFN 8))))))))
      (UNWIRE-PAGE ADDRESS)))

)
