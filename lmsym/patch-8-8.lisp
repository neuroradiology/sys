;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.8
;;; Reason: New distribution-tape system.
;;; Written 1/07/82 14:18:21 by BSG,
;;; while running on Basset from band 2
;;; with System 78.44, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.29, Canon 9.11, microcode 841.



; From file dummy
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(let ((fs:load-pathname-defaults
	(copytree fs:load-pathname-defaults)))
  (load "sys:distribution;distribution lisp >")
  (make-system 'distribution ':nowarn))

)


