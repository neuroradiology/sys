;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.26
;;; Reason: Fix interaction of completion with file types
;;; Written 12/31/81 00:16:13 by Moon,
;;; while running on Pointer from band 2
;;; with System 78.41, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.25, Canon 9.10, microcode 840.



; From file fsstr.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (local-lmfs-pathname :complete-string) (string options)
  (let ((newparse (lmfs-parse-for-completer string)))
    (if (null newparse)
	(values string nil)			;bad syntax
	(multiple-value-bind (string flag)
	    (funcall (fs:merge-pathname-defaults newparse self)
		     ':complete-myself string options fs:type fs:version)
	  (let ((host-name (funcall fs:host ':name-as-file-computer)))
	    (if (string-equal host-name "UNKNOWN")
		(values string flag)
		(values (string-append host-name ":" string) flag)))))))

)

; From file fsstr.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (local-lmfs-pathname :complete-myself) 
				(string options default-type default-version)
  default-version ;not used
  (multiple-value-bind (dirfd err)
      (with-fs-locked-mv (get-filedesc-from-path fs:directory ':directory 1))
      (if err
	  (values string nil)
	  (let ((namels (with-fs-locked
			  (scan-dir-for-completion
			    dirfd
			    (if (symbolp fs:name) nil fs:name)
			    (if (symbolp fs:type) nil fs:type)
			    options))))
	    (loop for namel in namels do
		  (cond ((eq (second namel) ':directory)
			 (setf (first namel)
			       (string-append (first namel) *path-delimiter*)))))
	    (selectq (length namels)			    
	      (0 (values string nil))
	      (1 (values (namel-to-string-for-completer self (car namels) options)
			 (if (memq ':read options) ':old ':new)))
	      (otherwise
	        ;; If no type specified, and exactly one file with the default type
	        ;; exists, complete to that file ignoring any other files with
	        ;; different types.
	        (if (and (symbolp fs:type)
			 (= (loop for (name type) in namels
				  count (equal type default-type))
			    1))
		    (values (namel-to-string-for-completer self
			      (loop for x in namels
				    when (equal (cadr x) default-type)
				      return x)
			      options)
			    (if (memq ':read options) ':old ':new))
		  ;; Multiple possible completions, complete the unambiguous part
		  ;; but return null second value to indicate unsuccessful completion
		  (values (namel-maximize namels self options) nil))))))))

)
