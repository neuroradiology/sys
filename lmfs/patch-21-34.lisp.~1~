;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.34
;;; Reason: (lmfs:local-lmfs-pathname :get-extended-property/plist), optional efficiency bums.
;;; Written 1/11/82 13:27:53 by BSG,
;;; while running on Beagle from band 4
;;; with System 78.45, ZMail 38.5, Tape 6.5, LMFS 21.33, Symbolics 8.12, microcode 841, Distribution copy.



; From file fsstr.LISP >lmfs POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

      
;;;Access stuff 11 Jan 1982  -- An efficiency bum.  Users of these ought to
;;;get-handler-for or memq which operations, etc., and use fs:file-properties if not.
;;;Intended to minimize crunching and consing for file server access checks.

(defmethod (local-lmfs-pathname :get-extended-property) (propname &optional (error-p t))
  (multiple-value-bind
    (result err)
      (with-fs-locked-mv
	(file-extended-getprop fs:directory fs:name fs:type fs:version propname))
    (if err
	(if error-p
	    (ferror nil "Cannot ascertain ~S property of ~A: ~A" propname self result)
	  err)
      result)))

(defmethod (local-lmfs-pathname :get-extended-plist) (&optional (error-p t))
  (let ((result (with-fs-locked
		  (file-extended-properties fs:directory fs:name fs:type fs:version))))
    (cond ((stringp result)
	   (if error-p
	       (ferror nil "Cannot ascertain properties of ~S: ~A" self result)
	     result))
	  (t (rplaca result self)))))

)

; From file paths.lisp >lmfs POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun file-properties (dirpn name type version &aux uprops usps)
  (multiple-value-bind (fd err)
      (get-fd-from-dn-en dirpn name type version ':deleted)
    (Or err
	(let ((*DONT-DEACTIVATES* (cons fd *DONT-DEACTIVATES*)))
	  (multiple-value (uprops usps) (keywordized-plist fd))
	  (values 
	    (nconc
	      (file-properties-from-fd fd)	;extra work for :pathname, ok.
	      uprops
	      (get-backup-info fd))
	    (nconc usps
		   (if (eq type ':directory)
		       *dir-changeable-props*
		       *nondir-changeable-props*)))))))

(defun keywordized-plist (fd &aux usps)
  (loop with p = (file-plist fd)
	for pp on p by 'cddr
	do (push (car (rplaca pp (intern (car pp) "")))	;keywordize
		 usps)
	finally (return p usps)))


(defun file-extended-properties (dirpn name type version)
  (multiple-value-bind (fd err)
      (get-fd-from-dn-en dirpn name type version ':deleted)
    (if err
	(values nil err)
      (values 
	(prog1 (cons nil (keywordized-plist fd))
	       (check-deactivate-file fd))
	nil))))					;Remember, friend, properties are strings, too

(defun file-extended-getprop (dirpn name type version prop)
  (multiple-value-bind (fd err)
      (get-fd-from-dn-en dirpn name type version ':deleted)
    (or err
	(prog1 (fs-plist-get fd prop)
	       (check-deactivate-file fd)))))


)
