;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.27
;;; Reason: NIL length for output openings (fixes garbage in wholine).
;;; Written 1/01/82 21:37:23 by BSG,
;;; while running on Beagle from band 2
;;; with System 78.41, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.26, Canon 9.11, microcode 841.



; From file fsstr.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (lmfs-opening-mixin :length) ()
  (if (memq mode '(:write :append))
      nil
    bytes-in-file))

)
