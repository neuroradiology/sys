;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.2
;;; Reason: Another halfword clobbered (in ZWEI:FIND-COMBINED-METHODS)
;;; Written 12/10/81 19:16:45 by MMcM,
;;; while running on Basset from band 1
;;; with System 78.5, ZMail 38.0, Experimental Tape 6.0, Experimental LMFS 20.5, Experimental Symbolics 8.1, Experimental Canon 9.0, microcode 836.



; From file SECTIO.LISP DSK:<ZWEI> SCRC:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFUN FIND-COMBINED-METHODS (FLAVOR MESSAGE &AUX FL SM METHOD)
  "Return a list of the non-combined methods involved in handling MESSAGE to FLAVOR"
  (OR (SETQ FL (GET FLAVOR 'SI:FLAVOR))
      (BARF "~S not DEFFLAVOR'ed" FLAVOR))
  (OR (SETQ SM (SI:FLAVOR-SELECT-METHOD FL))
      (BARF "~S not a composed, instantiated flavor" FLAVOR))
  (OR (SETQ METHOD (CDR (SI:ASSQ-CAREFUL MESSAGE (%MAKE-POINTER DTP-LIST SM))))
      (BARF "Flavor ~S does not handle message ~S" FLAVOR MESSAGE))
  (SETQ METHOD (EH:FUNCTION-NAME METHOD))
  (COND ((SETQ SM (CDDDR (SI:FUNCTION-SPEC-GET METHOD 'SI:COMBINED-METHOD-DERIVATION)))
	 (NCONC (REVERSE (CDR (ASSQ ':WRAPPER SM)))	;Try to approximate the order
		(REVERSE (CDR (ASSQ ':BEFORE SM)))	;in which they're called
		(REVERSE (CDR (ASSQ NIL SM)))
		(COPYLIST (CDR (ASSQ ':AFTER SM)))
		(MAPCAN #'(LAMBDA (X)
			    (AND (NOT (MEMQ (CAR X) '(:WRAPPER :BEFORE NIL :AFTER)))
				 (REVERSE (CDR X))))
			SM)))
	(T (LIST METHOD))))

)

