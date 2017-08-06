;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.13
;;; Reason: :NEWEST/OLDEST suppressed in :STRING-FOR-EDITOR.
;;; Written 12/18/81 10:31:37 by BSG,
;;; while running on Terrier from band 1
;;; with System 78.16, ZMail 38.2, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.6, Canon 9.0, microcode 840.



; From file FSSTR.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (lmfs-pathname :string-for-editor) ()
  (string-append
    (lmfs-pathname-fname-string t)		;t is suppress newest
    " " fs:directory " " (funcall fs:host ':name-as-file-computer) ":"))

(defmethod (lmfs-pathname :string-for-host) ()
  (let ((ename (if (and (null fs:name) (null fs:type) (null fs:version))
		   ""
		 (lmfs-pathname-fname-string nil))))	;don't suppress newest
    (cond ((eq fs:directory ':wild)
	   (string-append "*" *path-delimiter* ename))
	  ((null fs:directory) ename)
	  ((string-equal fs:directory *path-delimiter*)
	   (string-append *path-delimiter* ename))
	  (t (string-append fs:directory *path-delimiter* ename)))))

(defmethod (lmfs-pathname :string-for-dired) ()
  (lmfs-pathname-fname-string nil))

(declare-flavor-instance-variables (lmfs-pathname)
(defun lmfs-pathname-fname-string (suppress-newest
				   &aux (vers fs:version))
  (if (and suppress-newest (eq vers ':newest))
      (setq vers nil))
  (string-append
    (if (eq fs:name ':wild)
	"*"
	fs:name)
    (cond ((and (memq fs:type '(:unspecific nil))
		(memq vers '(:unspecific nil)))	;implied semant.
	   "")
	  ((eq fs:type ':directory) "")		;this is problematic -- running debate.
	  (t (string-append
	       *TYPE-DELIMITER*
	       (cond ((memq fs:type '(:unspecific nil)) "")
		     ((eq fs:type ':wild) "*")
		     (t fs:type))
	       (cond ((memq vers '(:unspecific nil)) "")
		     (t *VERSION-DELIMITER*))
	       (cond ((eq vers ':newest) "NEWEST")
		     ((eq vers ':oldest) "OLDEST")
		     ((memq vers '(:unspecific nil)) "")
		     ((eq vers ':wild) '*)
		     ((numberp vers) (format nil "~D" vers))
		     (t vers))))))))
)
