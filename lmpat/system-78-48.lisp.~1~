;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.48
;;; Reason: (CHAOS:HOST-TRUSTED-P <host-address>) & *TRUSTED-SUBNETS* site variable.
;;; Written 1/11/82 13:51:04 by BSG,
;;; while running on Beagle from band 4
;;; with System 78.45, ZMail 38.5, Tape 6.5, LMFS 21.33, Symbolics 8.12, microcode 841, Distribution copy.



; From file chsaux.lisp >lmio POINTER:
#8R CHAOS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CHAOS")))


;;; File system access ---

(SI:DEFINE-SITE-VARIABLE *TRUSTED-SUBNETS* :TRUSTED-CHAOS-SUBNETS)

(DEFUN HOST-TRUSTED-P (HOST) 
  (LET ((SUBNET (LDB #O0808 HOST)))
    (OR (= SUBNET MY-SUBNET)
	(MEMQ SUBNET *TRUSTED-SUBNETS*))))

)
