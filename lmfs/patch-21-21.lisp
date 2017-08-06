;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.21
;;; Reason: Backup: CHVV to assume "append to tape" if can, less useless info in maps.
;;; Written 12/24/81 13:22:13 by BSG,
;;; while running on Beagle from band 4
;;; with System 78.33, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.18, Canon 9.5, microcode 840.



; From file nb.lisp >bsg POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

;;; All this stuff is really from LMFS backup.lisp

(defvar *backup-try-append* t)

(defun backup-dumper (&rest args)
  (let ((*backup-dump-tape-name* "")
	(*backup-set-dates* t)
	(*backup-dump-bot-p* nil)
	(*backup-dump-history* nil)
	(*backup-tape-name-8* (format nil "~8a" ""))
	(*backup-dump-sequence-number* 1)
	(*backup-dump-type* ':incremental)
	(*backup-dump-start-node* lmfs:*PATH-DELIMITER*)
	(*backup-dump-start-path* nil)
	(*backup-dump-tape-drive* 0)
	(*backup-dump-comment* "")
	(*backup-dump-dump-name* "")
	(*backup-dump-deleteds* nil)
	(*backup-try-append* *backup-try-append*)
	(*backup-dump-start-time* (time:get-universal-time)))
    (setq *backup-dump-dump-name*
	  (format nil "Backup dump of ~A"
		  (time:print-universal-time *backup-dump-start-time* nil)))
    (*catch
      'dumper-abort
      (if args
	  (progn
	    (tv:doplist (args prop ind)
	      (selectq ind
		(:set-dates   (setq *backup-set-dates* prop))
		(:tape-name   (setq *backup-dump-tape-name* prop))
		(:dump-type   (setq *backup-dump-type* prop))
		(:start-node  (setq *backup-dump-start-node* prop))
		(:start-path  (setq *backup-dump-start-path* prop)
			      (setq *backup-dump-start-node*
				    (funcall prop ':string-for-host)))
		(:deleted     (setq *backup-dump-deleteds* prop))
		(:tape-drive  (setq *backup-dump-tape-drive* prop))
		(:try-append  (setq *backup-try-append* prop))
		(:comment	    (setq *backup-dump-comment* prop))
		(t            (ferror nil "Unknown keyword to backup dumper: ~S" ind)))))
	(backup-dump-choose-variable-values))
    (do () (())
      (if (backup-dump-validate-args)
	  (return)
	  (backup-dump-choose-variable-values)))
    (backup-dump-i))))

(defun backup-dump-choose-variable-values ()
  (tv:choose-variable-values
    `(""
      (*backup-dump-type* "Dump Type"
			  :assoc ,*backup-dump-type-names*)
      (*backup-dump-tape-name* "Tape Reel ID" :string)
      (*backup-dump-start-node* "Starting directory" :string)
      (*backup-dump-tape-drive* "Tape drive number" :number)
      (*backup-dump-deleteds* "Dump deleted files" :boolean)
      (*backup-try-append* "Append to tape" :boolean)
      (*backup-set-dates* "Set date dumped" :boolean)
      (*backup-dump-comment* "Comment" :string))
    ':margin-choices
      '("Do It" ("Abort" . ((*throw 'dumper-abort 'dumper-abort))))
    ':label *backup-dump-dump-name*))

(defun backup-deal-with-bot ()
  (do () (())
    (cond ((funcall *backup-stream* ':bot-p)	;at bot -- analyze the tape
	   (backup-make-statements-about-the-mounted-tape)
	   (return nil))
	  ((and					;not at bot -- wants to rewind
	     (not *backup-try-append*)
	     (yes-or-no-p
	       (format nil
		       "~&Drive ~D is not at beginning of tape.  Shall I rewind it?~@
			      (an answer of /"no/" means append to tape ~A): "
		       (funcall *backup-stream* ':unit) *backup-dump-tape-name*)))
	   (setq *backup-dump-bot-p* t)
	   (funcall *backup-stream* ':rewind)
	   (backup-make-statements-about-the-mounted-tape)
	   (return nil))
	  
	  (t					;not BOT case - wants apnd - check file marks
	   (with-open-stream
	     (*backup-stream* (tape:open-tape *backup-dump-tape-drive* ':mode ':read
					      ':no-bot-prompt t ':no-read-ahead))
	     (funcall *backup-stream* ':backspace)	;BS over last EOF?
	     (cond ((null (funcall *backup-stream* ':tyi)) 	;at EOF, great
		    (funcall *backup-stream* ':backspace)
		    (funcall *backup-stream* ':close)
		    (setq *backup-dump-bot-p* nil)
		    (return nil))
		   (				;not at EOF - very suspicious
		    (progn
		      (funcall *backup-stream* ':skip-file)
		      (yes-or-no-p
			(format nil "~&Tape ~A is not at a likely point for appending a dump.  Shall I continue at the~@
					next tape mark? (/"no/" means rewind and unload	and wait for a new tape? ")))
		    (setq *backup-dump-bot-p* nil)
		    (return nil))
		   (t (rewind-unload-new-tape))))))))

(defun backup-make-statements-about-the-mounted-tape (&aux dont-rewind reel
						      (unit (funcall *backup-stream* ':unit)))
  (setq *backup-dump-bot-p* t)
  (with-open-stream
    (*backup-stream* (tape:open-tape unit ':mode ':read ':no-bot-prompt t
				     ':no-read-ahead t))
    (*catch 'reloader-no-prelude
      (do () (())
	(cond ((string-equal "PRELUDE"
			     (condition-bind
			       ((:tape-error
				  #'(lambda (&rest ignore)
				      (format t "~&Tape error checking tape - We ~
				     conclude that this is a fresh tape.")
				      (*throw 'reloader-no-prelude nil))))
			       (funcall *backup-stream* ':line-in)) 0 0 nil 7)
	       (funcall *backup-stream* ':rewind)
	       (let ((prelude (reloader-read-prelude)))
		 (cond ((null (setq reel (get prelude ':reel)))
			(return nil)) ;Not even a backup tape
		       ((string-equal (string-trim " " reel) *backup-dump-tape-name*)
			;; Equal case, this reel matches
			(cond ((and
				 (not *backup-try-append*)
				 (yes-or-no-p
				   (format nil "~&Do you wish to overwrite tape ~A?~@
					 The usual answer here is /"no/", meaning append to~@
					 the end of tape ~A.  If you answer /"yes/", tape ~A~@
					 will be irretrievably overwritten: "
					 reel reel reel)))
			       (setq *backup-dump-bot-p* t)
			       (funcall *backup-stream* ':rewind))
			      (t  (backup-forward-to-end)
				  (setq *backup-dump-bot-p* nil dont-rewind t)))
			(return))
		       ;;Some other backup tape
		       ((yes-or-no-p		;Wants to use mounted one instead
			  (format nil "~&Tape ~A, containing a ~A~@
			taken by ~A at ~A appears to be on drive ~D.~@
			Did you really want to use reel ~A instead? "
				  reel
				  (or (get prelude ':dump-name)
				      (get prelude ':type))
				  (get prelude ':user-id)
				  (if (get prelude ':dump-time)
				      (time:print-universal-time
					(get prelude ':dump-time) nil)
				    "???")
				  (funcall *backup-stream* ':unit)
				  reel))
			       (funcall *backup-stream* ':rewind)
			       (setq *backup-dump-tape-name* reel
				     *backup-dump-bot-p* t
				     *backup-tape-name-8* (format
							    nil "~8a"
							    *backup-dump-tape-name*))
			       ;;Loop thru once more...
			       )
		       ((yes-or-no-p		;Overwrite this reel
			  (format
			    nil "~&In that case, do you want to OVERWRITE~@
				the reel labelled ~A, and make it be reel ~A?~@
				(an answer of /"no/" means you want to mount another one): "
			    reel *backup-dump-tape-name*))
			(funcall *backup-stream* ':rewind)
			(setq *backup-dump-bot-p* t)
			(return))
		       (t (rewind-unload-new-tape)))))
	      (t (return))))))
  (or dont-rewind (funcall *backup-stream* ':rewind)))

(defun dump-recurse (starpath &rest options)

  ;; Separate the sheep from the goats.

  (error-restart
    (let ((dirlist (fs:directory-list starpath ':additional-info (if *backup-dump-deleteds*
								     ':deleted)))
	  (dirs nil)
	  (files nil)
	  (dumpable-files nil))
      (dolist (elt dirlist)
	(let ((path (car elt)))
	  (if (not (null path))
	      (if (or *backup-dump-deleteds*
		      (not (get (cdr elt) ':deleted)))
		  (if (eq (funcall path ':type) ':directory)
		      (push path dirs)
		      (push elt files))))))
      
      ;; Find out which files are gonna get dumped.
      
      (dolist (file files)
	(let* ((props file)			;a cons is a plist ..
	       (comp-date (get props ':complete-dump-date))
	       (inc-date (get props ':incremental-dump-date)))
	  (let ((latest-backup-date (max (or inc-date 0) (or comp-date 0)))
		(latest-mod-date (max (or (get props ':creation-date) 1)
				      (or (get props ':modification-date) 1))))
	    (if (or (eq *backup-dump-type* ':complete)
		    (and (eq *backup-dump-type* ':incremental)
			 (> latest-mod-date latest-backup-date)))
		(push (list (car file) comp-date inc-date props) dumpable-files)))))
      
      ;; Alphabetical listing/dumping desired if anybody ever intends to look at it.
      
      (setq dumpable-files (sortcar dumpable-files #'zwei:dired-pathname-lessp))
      (setq dirs (sort dirs #'fs:pathname-lessp))
      
      ;; Write tape and map header.
      
      (if (or dumpable-files dirs)		;slightly one-level-system optimization.
						;should hair it up not to map until
						;something starts dumping.
	  (do ((dir-finished))
	      (dir-finished)
	    (setq dir-finished nil)
	    (*catch 'backup-dir-retry
	      (condition-bind ((:end-of-tape
				 #'(lambda (&rest ignore)
				     (backup-eot-handler)
				     (*throw 'backup-dir-retry nil))))
		(dump-directory-list starpath dumpable-files dirs)
		
		;; Dump the files herein before recursing.
		
		(dolist (file dumpable-files)
		  (do (dumped-file)
		      (dumped-file)
		    (setq dumped-file nil)
		    (error-restart
		      (*catch
			'backup-file-retry
			(condition-bind
			  ((:tape-error #'backup-dump-tape-error-handler))
			  (apply #'dump-file file)
			  (setq dumped-file t)))))
		  (setq dumpable-files (delq file dumpable-files 1))	;in case restart...
		  (funcall *backup-stream* ':write-eof))
		
		;; With that having been done, now dump the dirs.
		
		(dolist (dir dirs)
		  (lexpr-funcall #'dump-recurse
				 (funcall (funcall dir ':condense-directory)
					  ':new-pathname
					  ':name ':wild ':type ':wild ':version ':wild)
				 options)
		  (setq dirs (delq dir dirs 1)))
		(setq dir-finished t))))))))

)
