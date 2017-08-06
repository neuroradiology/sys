;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.8
;;; Reason: (lmfs:find-backup-copies ..<paths>..)
;;; Written 12/17/81 14:47:18 by BSG,
;;; while running on Terrier from band 1
;;; with System 78.16, ZMail 38.2, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.6, Canon 9.0, microcode 840.



; From file BACKUP.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defresource r-dump-directory (total-length)	;don't source-file conflict name
  :constructor (make-dump-directory)
  :initial-copies 0
  :matcher ( (array-dimension-n 1 object) total-length))

(defun create-dump-directory (path &optional (end nil))
  (let ((i-opened nil)
	(file))
    (unwind-protect
      (progn
	(cond ((or (stringp path) (typep path 'fs:pathname))
	       (setq file (open path '(:fixnum :byte-size 8.)) i-opened t))
	      (t (setq file path)))
	(if (null end)
	    (setq end (funcall file ':length)))
	(funcall file ':set-pointer (- end 3))
	(funcall file ':set-pointer (cdd-read-fixnum file 3))
	(if (not (= 1 (cdd-read-fixnum file 2)))
	    (ferror nil "Does not look like a backup directory file - ~a" file))
	(let* ((adrbas (cdd-read-fixnum file 3))
	       (dumpid-a (cdd-read-fixnum file 3))
	       (map-start (- (cdd-read-fixnum file 3) adrbas))
	       (map-end (- (cdd-read-fixnum file 3) adrbas)))
	  (let ((total-length map-end))
	    (let ((dmpd (allocate-resource 'r-dump-directory total-length)))
	      (alter-dump-directory dmpd
			  string-start  adrbas
			  map-start     map-start
			  map-len       (- map-end map-start)
			  version       1
			  dumpid        nil
			  previous      (if (zerop adrbas) nil adrbas))
	      (funcall file ':set-pointer adrbas)
	      (funcall file ':string-in "Unexpected eof in dump directory" dmpd
		       0 total-length)
	      (setf (dmpd-dumpid dmpd) (get-dmpd-string dmpd dumpid-a))
	      dmpd))))
      (if i-opened (close file)))))

(defun cdd-read-fixnum (file n &aux (val 0))
  (dotimes (i n)
    (setq val (dpb (funcall file ':tyi)
		   (logior #O08 (lsh (* i 8) 6))
		   val)))
  val)


(defun get-dmpd-string (dmpd adr)
  (let ((len (dmpd-read-fixnum dmpd adr 2)))
    (substring dmpd (+ adr 2) (+ adr 2 len))))

(defun compare-dmpd-string (dmpd adr string)
  (setq string (string string))
  (let ((len (dmpd-read-fixnum dmpd adr 2)))
    (and (= len (string-length string))
	 (string-equal string dmpd 0 (+ adr 2) nil (+ adr 2 len)))))

(defun dmpd-read-fixnum (dmpd adr n &aux (val 0))
  (dotimes (i n)
    (setq val (dpb (aref dmpd (+ adr i))
		   (logior #O08 (lsh (* i 8) 6))
		   val)))
  val)


(defun display-tape-directory (name &aux dumps)
  (unwind-protect
    (progn
      (let ((path (funcall (dump-directory-template) ':new-name (string name))))
	(with-open-file (file path '(:fixnum))
	  (let ((dmpd (create-dump-directory file)))
	    (do () (())
	      (push dmpd dumps)
	      (if (dmpd-previous dmpd)
		  (setq dmpd (create-dump-directory file (dmpd-previous dmpd)))
		(return nil))))))
      (dolist (dmpd dumps)
	(format t "~&~A" (dmpd-dumpid dmpd))
	(do ((x (dmpd-map-start dmpd))) (())
	  (if (zerop (dmpd-read-fixnum dmpd x 3)) (return nil))
	  (let ((name (get-dmpd-string dmpd (dmpd-read-fixnum dmpd x 3))))
	    (if (zerop (dmpd-read-fixnum dmpd (+ x 3) 3))	;directory
		(format t "~&Directory ~A" name)
	      (format t
		      "~&File ~A.~A.~D, created ~A"
		      name
		      (get-dmpd-string dmpd (dmpd-read-fixnum dmpd (+ x 3) 3))
		      (dmpd-read-fixnum dmpd (+ x 6) 3)
		      (time:print-universal-time
			(dmpd-read-fixnum dmpd (+ x 9) 4)
			nil))))
	  (incf x (+ 3 3 3 4)))))
    (dolist (dump dumps) (deallocate-resource 'r-dump-directory dump))))


(defmacro using-allocated-resource ((var form name) &body body)
  `(let ((,var nil))
     (unwind-protect
       (progn
	 (setq ,var ,form)
	 ,@body)
       (if ,var (deallocate-resource ',name ,var)))))


(defun find-backup-copies (&rest paths)
  (let ((paths (loop for path in paths collect (fs:parse-pathname path si:local-host)))
	(dirlist
	  (mapcar #'car
		  (cdr (fs:directory-list (funcall
					    *dump-directory-template*
					    ':new-pathname ':name ':wild ':version ':newest)
					  ':fast)))))
    (loop for dir in dirlist
	  do (find-backup-copies-in-file paths dir t))))

(defun find-backup-copies-in-file (paths dir &optional (print nil) &aux founds prev-dump)
  (with-open-file (file dir ':characters nil)
    (do ((first t nil))
	((and (not first) (null prev-dump))
	 founds)
      (using-allocated-resource (dmpd (create-dump-directory file prev-dump) r-dump-directory)
	(setq prev-dump (dmpd-previous dmpd))
	(do ((x (dmpd-map-start dmpd))
	     (dumpid (dmpd-dumpid dmpd))
	     (tape (funcall (funcall file ':truename) ':name))	;yuk shd be fixed
	     (cur-dir nil)
	     (dir-good nil))
	    ((zerop (dmpd-read-fixnum dmpd x 3)))
	  (cond ((zerop (dmpd-read-fixnum dmpd (+ x 3) 3))	;directory?
		 ;;See if this directory worthy of further consideration
		 (setq dir-good
		       (loop for path in paths
			     ;;gonna cons like hell, but I see no other way.
			     with tdir = (funcall (fs:parse-pathname
						    (get-dmpd-string
						      dmpd (dmpd-read-fixnum dmpd x 3))
						    si:local-host)
						  ':string-for-directory)
			     as dir = (funcall path ':string-for-directory)
			     if (or (eq dir ':wild)
				    (string-equal tdir dir))
			     do
			     (setq cur-dir tdir)
			     (return t)
			     finally (return nil))))
		(dir-good			;This is a file.  If this dir is
						;interesting, then....
		 (loop for path in paths
		       with nameptr = (dmpd-read-fixnum dmpd x 3)
		       and typeptr = (dmpd-read-fixnum dmpd (+ x 3) 3)
		       and cversion = (dmpd-read-fixnum dmpd (+ x 6) 3)
		       as name = (funcall path ':name)
		       and type = (funcall path ':type)
		       and version = (funcall path ':version)
		       and dir = (funcall path ':string-for-directory)
		       if (and (or (symbolp dir) (string-equal dir cur-dir))
			       (or (symbolp name) (compare-dmpd-string dmpd nameptr name))
			       (or (symbolp type) (compare-dmpd-string dmpd typeptr type))
			       (or (symbolp version) (= version cversion)))
		       do
		       (let ((fpath
			       (fs:make-pathname-internal
				    ;;WARNING- :DIRECTORY FORMAT KNOWN HERE!!!!
				    si:local-host ':unspecific cur-dir
				    (get-dmpd-string dmpd nameptr)
				    (get-dmpd-string dmpd typeptr)
				    cversion)))
			 (if print
			     (format t "~&~A, created ~A, on tape ~A~%  (~A)"
				     fpath
				     (time:print-universal-time
				       (dmpd-read-fixnum dmpd (+ x 9) 4) nil)
				     tape
				     dumpid)
			   (push (list
				   fpath
				   (time:print-universal-time
				     (dmpd-read-fixnum dmpd (+ x 9) 4) nil)
				   tape
				   dumpid)
				 founds)))))
		(t))				;dir not interesting, page thru
	  (incf x (+ 3 3 3 4)))))))
)
