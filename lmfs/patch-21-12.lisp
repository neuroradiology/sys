;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.12
;;; Reason: 21.11 blowage
;;; Written 12/18/81 09:53:20 by BSG,
;;; while running on Terrier from band 1
;;; with System 78.16, ZMail 38.2, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.6, Canon 9.0, microcode 840.



; From file FSSTR.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (lmfs-pathname :string-for-dired) ()
  (string-append
    (if (eq fs:name ':wild)
	"*"
	fs:name)
    (cond ((and (memq fs:type '(:unspecific nil))
		(memq fs:version '(:unspecific nil :newest)))	;implied semant.
	   "")
	  ((eq fs:type ':directory) "")		;this is problematic -- running debate.
	  (t (string-append
	       *TYPE-DELIMITER*
	       (cond ((memq fs:type '(:unspecific nil)) "")
		     ((eq fs:type ':wild) "*")
		     (t fs:type))
	       (cond ((memq fs:version '(:unspecific nil)) "")
		     (t *VERSION-DELIMITER*))
	       (cond ((eq fs:version ':newest) "NEWEST")
		     ((eq fs:version ':oldest) "OLDEST")
		     ((memq fs:version '(:unspecific nil)) "")
		     ((eq fs:version ':wild) '*)
		     ((numberp fs:version) (format nil "~D" fs:version))
		     (t fs:version)))))))

)
