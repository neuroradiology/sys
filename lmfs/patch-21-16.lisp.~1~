;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.16
;;; Reason: NEWEST out of string-for-printing, hosts put in string-for-wholine.
;;; Written 12/22/81 16:27:15 by BSG,
;;; while running on Spaniel from band 1
;;; with System 78.26, ZMail 38.4, Symbolics 8.7, Tape 6.2, LMFS 21.15, Canon 9.3, microcode 841.



; From file fsstr.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (lmfs-pathname :string-for-printing) ()
  (string-append
    (cond ((eq fs:host si:local-host) "")
	  (t (string-append (funcall fs:host ':name-as-file-computer) ":")))
    (lmfs-pathname-pname-string t)))		;sure, suppress newest.

)

; From file fsstr.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (lmfs-pathname :string-for-wholine) ()
  (funcall-self ':string-for-printing))

)
