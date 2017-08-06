;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.15
;;; Reason: Suppress NEWEST in wholine.
;;; Written 12/18/81 14:01:20 by BSG,
;;; while running on Terrier from band 4
;;; with System 78.20, ZMail 38.3, Symbolics 8.5, Tape 6.1, LMFS 21.14, Canon 9.1, microcode 840.



; From file FSSTR.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))


(defmethod (lmfs-pathname :string-for-host) ()
  (lmfs-pathname-pname-string nil))		;don't suppress newest

(defmethod (lmfs-pathname :string-for-wholine) ()
  (lmfs-pathname-pname-string t))

(declare-flavor-instance-variables (lmfs-pathname)
(defun lmfs-pathname-pname-string (suppress-newest)
  (let ((ename (if (and (null fs:name) (null fs:type) (null fs:version))
		   ""
		 (lmfs-pathname-fname-string suppress-newest))))
    (cond ((eq fs:directory ':wild)
	   (string-append "*" *path-delimiter* ename))
	  ((null fs:directory) ename)
	  ((string-equal fs:directory *path-delimiter*)
	   (string-append *path-delimiter* ename))
	  (t (string-append fs:directory *path-delimiter* ename))))))
(compile-flavor-methods fs:remote-lmfs-pathname local-lmfs-pathname)
)
