;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.10
;;; Reason: Distribution dumper second-guess hair not hairy enough.
;;; Written 1/07/82 18:13:41 by BSG,
;;; while running on Basset from band 4
;;; with System 78.44, ZMail 38.5, Symbolics 8.9, Tape 6.5, LMFS 21.31, Canon 9.11, microcode 841.



; From file distribution-dump.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

;;;There are two cases here, programs that are loaded and programs that aren't loaded.
;;;If the program isn't loaded, we use the latest source.  If the program is loaded,
;;;we are quite picky about what source version is dumped.

(defun write-distribution-source-file (stream system path &aux case fetch-path report-path
				       extra version latest-version newprobe)
  (let* ((generic-path (funcall path ':generic-pathname))
	 (trans-path (funcall path ':translated-pathname))
	 (qf-source-path (funcall generic-path ':get ':qfasl-source-file-unique-id)))
    (do () (())
      top      
      ;;The actual file whence this thing came claims to exist.
      (setq extra nil)
      (setq case
	    (cond ((null qf-source-path)	;non-loaded file
		   (cond ((setq fetch-path (probef (funcall path ':new-version ':newest)))
			  (setq extra (format nil "UNSPECIFIC-VERSION T~%"))
			  (setq report-path (funcall path ':new-version
						     (funcall fetch-path ':version)))
			  (go dump))
			 (t (setq fetch-path path report-path path)	;have something
						;intelligent to say
			    ':unspecific-not-found)))
		  ;;We are here because we know that this is loaded file.  Does host exist?
		  (t
		   (setq version (funcall qf-source-path ':version))
		   (setq report-path (funcall path ':new-version version))
		   (cond ((and (fs:get-pathname-host (funcall qf-source-path ':host))
			       ;;The host of the real place it comes from exists.
			       (cond ((setq fetch-path (probef qf-source-path))
				      ;;Not only that, but the file exists.
				      (go dump))
				     (t nil))))
			  ;;Host or file doesn't exist.
			  ;;Try just plain-old SYS host, same version?
			 ((setq fetch-path
				(probef (setq report-path
					      (funcall path ':new-version version))))
			  (go dump))
			 ;;Too bad. Can't find comparable thing.  See if any exists.
			 ((setq fetch-path
				(probef (funcall path ':new-version ':newest)))
			  ':specific-not-found-but-there-are-some)
			 (t ':specific-not-found)))))
      query
	    
      (selectq 
	(fquery `(:choices
		   (((:skip "Nothing; skip dumping this file.") #/N #/n)
		    ((:try-again "Try again.") #/T #/t)
		    ((:dirlist "Directory list.") #/D #/d)
		    ,@(if (eq case ':specific-not-found-but-there-are-some)
			  (list '((:latest "Latest to be used.") #/L #/l)))
		    ((:else "Dump something else.") #/E #/e))
		   :help-function
		   (lambda (&rest ignore) (wdsf-help ',path ',report-path ',fetch-path
						     ',case)))
		(format
		  nil "~A~%What shall I do? "
		  (selectq case
		    (:specific-not-found-but-there-are-some
		     (format
		       nil
		       "Cannot find a copy of ~A with the correct source version."
		       report-path))		       
		    (t
		     (format nil "Cannot find any source for ~A." path)))))
	(:skip (return nil))
	(:try-again (go top))
	(:dirlist  (distribution-display-directory path)
		   (go query))
	(:latest (setq latest-version (funcall fetch-path ':version))
		 (setq extra (format
			       nil "UNSPECIFIC-VERSION T~%NEEDED-VERSION ~D~%" version))
		 (go vmatch))
	(:else   (format t "~&Type new pathname to be passed off as ~A~% (default ~A) : "
			 report-path trans-path)
		 (let ((new-path (fs:parse-pathname
				   (string-trim '(#\sp #\tab) (readline)) nil trans-path)))
		   (setq new-path (fs:merge-pathname-defaults new-path trans-path
							      (funcall trans-path ':type)))
		   (cond ((null (fs:get-pathname-host (funcall new-path ':host)))
			  (format t "~&The host ~A isn't a known file server."
				  (funcall new-path ':host)))
			 ((null (setq newprobe (probef new-path)))
			  (format t "~&~A doesn't appear to exist." new-path))
			 ((and (equal (funcall newprobe ':type)
				      (funcall path ':type))
			       (equal (funcall newprobe ':name)
				      (funcall path ':name)))
			  (setq latest-version (funcall newprobe ':version)
				fetch-path newprobe)
			  (go vmatch))
			 ;;A completely wild thing is being substituted.
			 (t (setq extra (format
					  nil"UNSPECIFIC-VERSION T~%NEEDED-VERSION ~D~%"
					  version))
			    (setq fetch-path new-path
				  report-path (funcall path ':new-version ':unspecific))
			    (go dump)))
		   (go query))))
      vmatch
      (cond ((not (numberp latest-version))(go cv-dump))
	    ((= latest-version version)
	     (setq extra nil)
	     (go cv-dump))
	    ((> latest-version version) (go cv-dump))
	    ((fquery format:yes-or-no-quietly-p-options
		     "~A isn't even as recent in version as the needed ~
				   version ~D.~%Do you really want to use ~A? "
		     fetch-path version fetch-path)
	     (go cv-dump))
	    (t (go query)))
	
      cv-dump
	
      (setq report-path (funcall report-path ':new-version latest-version))
	
      dump
      
      (or (typep report-path 'fs:logical-pathname)
	  (setq report-path (coerce-report-path-to-logical report-path))
	  (return nil))
	
      (distribution-file-writer
	stream "SOURCE-FILE" fetch-path report-path t system extra)
      (return t))))

)

