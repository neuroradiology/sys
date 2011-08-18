;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.42
;;; Reason: (fs:logical-pathname directory-translatable-p).
;;; Written 1/04/82 14:06:45 by BSG,
;;; while running on Basset from band 2
;;; with System 78.41, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.28, Canon 9.11, microcode 841.



; From file PATHNM.LISP >LMIO POINTER:
#8R FILE-SYSTEM:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))

(DEFMETHOD (LOGICAL-PATHNAME :DIRECTORY-TRANSLATABLE-P) (&AUX TRANS)
  (SETQ TRANS (ASSOC DIRECTORY (FUNCALL HOST ':TRANSLATIONS)))
  (IF TRANS
      (VALUES
	(TRANSLATION-PHYSICAL-DIRECTORY TRANS)
	(TRANSLATION-PHYSICAL-DEVICE TRANS))))

)
