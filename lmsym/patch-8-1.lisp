;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.1
;;; Reason: Patch #'ZWEI:COPY-MSG
;;; Written 12/09/81 15:07:50 by BEE,
;;; while running on Beagle from band 4
;;; with Experimental System 78.0, Experimental ZMail 38.0, Experimental Tape 6.0, Experimental LMFS 20.2, Experimental Symbolics 8.0, Experimental Canon 9.0, microcode 836.



; From file temp
#8R USER:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "USER")))


(%P-STORE-TAG-AND-POINTER (%MAKE-POINTER-OFFSET DTP-LOCATIVE #'ZWEI:COPY-MSG 8)
			  DTP-EXTERNAL-VALUE-CELL-POINTER
			  (LOCF (FSYMEVAL 'ZWEI:SOME-PLIST-NOT)))

)

