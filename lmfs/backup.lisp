;-*- Mode:LISP; Package:LMFS-*-
;; (C) Copyright Symbolics, Inc., 1981

(defvar *backup-debug-mode* nil)

;;; Need to do: dump directory plists.  BOT switch in history maker.  Better directory  
;;; reloading. Reloader error handling. problems with mount-new-tape.
;;; Retriever interface.
;;; + the obvious large things.

;;;  WATCH OUT FOR THIS -- EVAL IT AND BACKUP TIMES DONT GET SET
(eval-when (eval) (setq *backup-debug-mode* t))
;;;

(defconst *CURRENT-BACKUP-TAPE-VERSION* 3)
(defvar *backup-set-dates* t)
(defvar *backup-stream* nil)
(defvar *files-dumped* 0)
(defvar *backup-map-stream* nil)
(defvar *backup-dump-bot-p* nil)
(defvar *backup-dump-history* nil)
(defvar *backup-dump-tape-name* nil)
(defvar *backup-tape-name-8* nil)
(defvar *backup-dump-type* nil)
(defvar *backup-dump-start-node* nil)
(defvar *backup-dump-start-path* nil)
(defvar *backup-dump-tape-drive* 0)
(defvar *backup-dump-comment* "")
(defvar *backup-dump-start-time* "")
(defvar *backup-dump-dump-name* "")
(defvar *backup-dump-dump-on-tape* 1)
(defvar *backup-dump-sequence-number* nil)
(defvar *backup-dump-deleteds* nil)

(defconst *backup-dump-type-names*
	  '(("Incremental" . :INCREMENTAL)
	    ("Consolidated" . :CONSOLIDATED)
	    ("Complete" . :COMPLETE)
	    ("Archive" . :ARCHIVE)))

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
		(:comment	    (setq *backup-dump-comment* prop))
		(t            (ferror nil "Unknown keyword to backup dumper: ~S" ind)))))
	(backup-dump-choose-variable-values))
    (do () (())
      (if (backup-dump-validate-args)
	  (return)
	  (backup-dump-choose-variable-values)))
    (backup-dump-i))))

(defmacro bdva-error body
  `(progn
     (format t . ,body)
     (return nil)))

(defun backup-dump-validate-args ()		;everything special, thx to choose-v-v
  (prog ()
	(if (not (and (numberp *backup-dump-tape-drive*)
		      ( *backup-dump-tape-drive* 0)
		      ( *backup-dump-tape-drive* 7)))
	    (bdva-error "~&Invalid tape drive number: ~S" *backup-dump-tape-drive*))
	(if (not (memq *backup-dump-deleteds* '(t nil)))
	    (bdva-error "~&Invalid deleted-dumping spec: ~S" *backup-dump-deleteds*))
	(if (not (memq *backup-dump-type* '(:complete :incremental :consolidated)))
	    (bdva-error "~&Invalid dump type: ~S" *backup-dump-type*))
	(if (= (string-length *backup-dump-tape-name*) 0)
	    (bdva-error "~&A tape name must be specified."))
	(if (> (string-length *backup-dump-tape-name*) 8.)
	    (bdva-error "~&Tape name ~A does not fit in 8 characters, try one that does."
			*backup-dump-tape-name*)
	    ;; idea here is string-copy in defstorage-generated code wants = length field
	    (setq *backup-tape-name-8* (format nil "~8a" *backup-dump-tape-name*)))
	(let ((parsed
		(car (errset (fs:parse-pathname *backup-dump-start-node* *LOCAL-FS-HOST*)))))
	  (if (null parsed)
	      (bdva-error "~&Invalid starting node pathname: ~A" *backup-dump-start-node*))
	  (let ((dir (funcall parsed ':directory))
		(name (funcall parsed ':name))
		(type (funcall parsed ':type))
		(version (funcall parsed ':version)))
	    (cond ((null dir)
		   (bdva-error "~&Directory must be specified: ~A" parsed))
		  ((and (null name) (null type) (null version))	;>a> ! Great!
		   (setq *backup-dump-start-path*
			 (funcall parsed ':new-pathname
				  ':name ':wild ':type ':wild ':version ':wild)))
		  ((and (null type) (null version))	;>a>b, add a trailing >
		   (setq *backup-dump-start-path*
			 (funcall (funcall parsed ':condense-directory)
				  ':new-pathname
				  ':name ':wild ':type ':wild ':version ':wild)))
		  (t (setq *backup-dump-start-path* parsed
			   *backup-dump-start-node* (funcall parsed ':string-for-host))))))
	(return t)))

(defun backup-dump-choose-variable-values ()
  (tv:choose-variable-values
    `(""
      (*backup-dump-type* "Dump Type"
			  :assoc ,*backup-dump-type-names*)
      (*backup-dump-tape-name* "Tape Reel ID" :string)
      (*backup-dump-start-node* "Starting directory" :string)
      (*backup-dump-tape-drive* "Tape drive number" :number)
      (*backup-dump-deleteds* "Dump deleted files" :boolean)
      (*backup-set-dates* "Set date dumped" :boolean)
      (*backup-dump-comment* "Comment" :string))
    ':margin-choices
      '("Do It" ("Abort" . ((*throw 'dumper-abort 'dumper-abort))))
    ':label *backup-dump-dump-name*))

(defun complete-dump ()
  (backup-dumper ':dump-type ':complete))

(defun backup-dump-i (&aux (map-path (backup-dump-map-path)))
  (setq *files-dumped* 0)
  (with-open-stream (*backup-stream* (tape:open-tape
				       *backup-dump-tape-drive* ':mode ':write
				       ':no-bot-prompt t))
    (funcall *backup-stream* ':check-ready)
    (backup-deal-with-bot) 
;      (condition-bind       ;;error system losing badly
;	((nil #'backup-general-lossage-handler))
    (progn
      (error-restart
	(with-open-file (*backup-map-stream* map-path
					     ':direction ':output ':incremental-update t)
	  (backup-dump-prelude-and-map-start-incantations)
	  (do () (())
	    (format t "~&Beginning pass over ~A" *backup-dump-start-path*)
	    (dump-recurse *backup-dump-start-path*)
	    (format t "~&Ended pass over ~A" *backup-dump-start-path*)
	    (if (yes-or-no-p
		  (format nil "Shall I dump more stuff on tape ~A?~@
			(an answer of no means exit, leaving tape positioned at EOF): "
			  *backup-dump-tape-name*))
		(progn
		  (setq *backup-dump-start-node* nil)
		  (do () (())
		    (backup-dump-choose-variable-values)
		    (if (backup-dump-validate-args) (return))))
	      (return)))
	  (setq *backup-stream* (prog1 nil (funcall *backup-stream* ':close)))
	  (backup-dump-end-incantations)
	  (record-backup-times *backup-dump-dump-name*)
	  (create-backup-dump-history *backup-dump-dump-name*))))))

(defun backup-deal-with-bot ()
  (do () (())
    (if (not (funcall *backup-stream* ':bot-p))
	(if (yes-or-no-p
	      (format nil "~&Drive ~D is not at beginning of tape.  Shall I rewind it?~@
		(an answer of /"no/" means append to tape ~A): "
		      (funcall *backup-stream* ':unit) *backup-dump-tape-name*))
	    (progn
	      (setq *backup-dump-bot-p* t)
	      (funcall *backup-stream* ':rewind)
	      (backup-make-statements-about-the-mounted-tape)
	      (return nil))
						;not BOT case - check file marks
	    (with-open-stream
	      (*backup-stream* (tape:open-tape *backup-dump-tape-drive* ':mode ':read
					       ':no-bot-prompt t ':no-read-ahead))
	      (funcall *backup-stream* ':backspace)	;BS over last EOF?
	      (if (null (funcall *backup-stream* ':tyi))	;at EOF, great
		  (progn
		    (funcall *backup-stream* ':backspace)
		    (funcall *backup-stream* ':close)
		    (setq *backup-dump-bot-p* nil)
		    (return nil))
		  (progn			;not at EOF - very suspicious
		    (funcall *backup-stream* ':skip-file)
		    (if (yes-or-no-p
			  (format nil "~&Tape ~A is not at a likely point for appending a dump.  Shall I continue at the~@
					next tape mark? (/"no/" means rewind and unload	and wait for a new tape? "))
			(progn
			  (setq *backup-dump-bot-p* nil)
			  (return nil))
			(rewind-unload-new-tape))))))
	(progn (backup-make-statements-about-the-mounted-tape)
	       (return nil)))))

(defun backup-make-statements-about-the-mounted-tape (&aux
						      (unit (funcall *backup-stream* ':unit)))
  (with-open-stream
    (*backup-stream* (tape:open-tape unit ':mode ':read ':no-bot-prompt t))
    (*catch 'reloader-no-prelude
      (do () (())
	(if (string-equal "PRELUDE"
			  (condition-bind
			    ((:tape-error
			       #'(lambda (&rest ignore)
				   (format t "~&Tape error checking tape - We ~
				     conclude that this is a fresh tape.")
				   (*throw 'reloader-no-prelude nil))))
			    (funcall *backup-stream* ':line-in) 0 0 nil 7))
	    (progn
	      (funcall *backup-stream* ':rewind)
	      (let ((prelude (reloader-read-prelude)))
		(if (and (get prelude ':reel)
			 (not (string-equal (string-trim " " (get prelude ':reel))
					    *backup-dump-tape-name*)))
		    (if (yes-or-no-p
			  (format nil "~&Tape ~A, containing a ~A~@
			taken by ~A at ~A appears to be on drive ~D.~@
			Did you really want to use reel ~A instead? "
				  (get prelude ':reel)
				  (or (get prelude ':dump-name)
				      (get prelude ':type))
				  (get prelude ':user-id)
				  (if (get prelude ':dump-time)
				      (time:print-universal-time
					(get prelude ':dump-time) nil)
				      "???")
				  (funcall *backup-stream* ':unit)
				  (get prelude ':reel)))
			(progn (funcall *backup-stream* ':rewind)
			       (setq *backup-dump-tape-name* (get prelude ':reel)
				     *backup-dump-bot-p* t
				     *backup-tape-name-8* (format nil "~8a"
								  *backup-dump-tape-name*))
			       (return))
			(if (yes-or-no-p
			      (format nil "~&In that case, do you want to OVERWRITE~@
				the reel labelled ~A, and make it be reel ~A?~@
				(an answer of /"no/" means you want to mount another one): "
				      (get prelude ':reel) *backup-dump-tape-name*))
			    (progn
			      (funcall *backup-stream* ':rewind)
			      (setq *backup-dump-bot-p* t)
			      (return))
			    (rewind-unload-new-tape)))
		    (return))))
	    (return)))))
  (funcall *backup-stream* ':rewind))		;hahaha old one

(defun rewind-unload-new-tape ()
  (funcall *backup-stream* ':rewind)
  (funcall *backup-stream* ':set-offline)
  (format t "~&Type any character when ready: ")
  (tyi)
  (funcall *backup-stream* ':clear-error ':read)
  (funcall *backup-stream* ':check-ready))

(defun backup-dump-map-path ()
  (let ((map-path (fs:parse-pathname ">dump-maps>foo.map" *LOCAL-FS-HOST*)))
    (funcall map-path ':create-directory nil)
    (funcall map-path ':new-name (multiple-value-bind (nil min hr day mon yr)
				     (time:decode-universal-time *backup-dump-start-time*)
				   (format nil "~A-~D//~D//~D-~D:~2,48D"
					   (string-downcase *backup-dump-type*)
					   mon day yr hr min)))))

(defun backup-dump-prelude-and-map-start-incantations ()
  (format t "~&Beginning ~A dump.~@
             The name of the dump is /"~A/".~@
             The Tape Reel ID is ~A~@
             The dump map is being written to ~A.~%"
	  (cdr (rassq *backup-dump-type* *backup-dump-type-names*))
	  *backup-dump-dump-name*
	  *backup-dump-tape-name*
	  (funcall *backup-map-stream* ':truename))
  (format *backup-map-stream* "Dumped by ~A on machine ~A~%~%" user-id si:local-host)
  (dump-prelude)
  (format t "~&First tape of ~A dump.~@
             The name of the dump is /"~A/".~@
             The Tape Reel ID is ~A~%"
	  (cdr (rassq *backup-dump-type* *backup-dump-type-names*))
	  *backup-dump-dump-name*
	  *backup-dump-tape-name*))

(defun backup-dump-end-incantations ()
  (format *backup-map-stream*
	  "~%~%Dump completed at ~A.~%" (time:print-universal-time
					      (time:get-universal-time) nil))
  (format t "~&~D files dumped.~%" *files-dumped*))

(defun backup-general-lossage-handler (&rest args)
  (let ((msg (errset (format nil "~S" args))))
    (cerror nil t nil
	    "An error has occured during dumping:~:
	  ~A~:
	  Type Control-Meta-C to retry." msg)))

(defun dump-prelude (&aux (curtime (time:get-universal-time)))
  (format *backup-map-stream*
	  "~&Tape #~D, Reel ~A of ~A at ~A.~%~:[Not dumping~;Dumping~] deleted files.~2%"
	  *backup-dump-sequence-number* *backup-dump-tape-name*
	  *backup-dump-dump-name* (time:print-universal-time curtime nil)
	  *backup-dump-deleteds*)
  (format *backup-stream*
	  "PRELUDE~@
	   VERSION ~D~@
	   TAPE-SYSTEM-VERSION ~D~@
	   DEFAULT-DIRECTORY-VERSION ~D~@
	   REEL ~A~@
	   TYPE ~A~@
	   DUMP-NAME ~A~@
	   MAP-FILE-PATH ~A~@
	   DUMP-TIME ~A~@
	   TIME ~A~@
	   SEQUENCE ~D~@
	   START-PATH ~A~@
	   DELETED-FILES ~S~@
	   COMMENT ~A~@
	   USER-ID ~A~@
	   MACHINE ~A~@
	   END~%"
	   *CURRENT-BACKUP-TAPE-VERSION*
	   (si:get-system-version 'tape)					
	   *DIR-VERSION*
	   *backup-dump-tape-name* *backup-dump-type*
	   *backup-dump-dump-name*
	   (funcall *backup-map-stream* ':truename)
	   (time:print-universal-time *backup-dump-start-time* nil)
	   (time:print-universal-time curtime nil) *backup-dump-sequence-number*
	   *backup-dump-start-node*
	   *backup-dump-deleteds*
	   *backup-dump-comment* user-id (chaos:host-data))
  (funcall *backup-stream* ':write-eof))

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
      (setq dirs (sort dirs #'zwei:dired-pathname-lessp))
      
      ;; Write tape and map header.
      
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
		    (condition-bind ((:tape-error #'backup-dump-tape-error-handler))
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
	    (setq dir-finished t)))))))

(defun backup-dump-tape-error-handler (&rest args)
  (format *backup-map-stream*
	  "~&~2%Tape Error: ~A~%" (with-output-to-string (standard-output)
				    (tape:tape-errprint (third args))))
  (funcall *backup-stream* ':clear-error)
  (funcall *backup-stream* ':write-eof)
  (*throw 'backup-file-retry nil))
				  
(defun dump-directory-list
       (starpath dumpable-files dirs &aux (time (time:get-universal-time)))
  (push `(:directory ,starpath) *backup-dump-history*)
  (format *backup-stream*
	  "DIRECTORY ~A~@
	   TIME ~A~@
	   FILE-COUNT ~D~@
	   DIRECTORY-COUNT ~D~@
	   END~%"
	   starpath (time:print-universal-time time nil)
	   (length dumpable-files) (length dirs))
  (format *backup-map-stream*
	  "~%~%Directory ~A on tape ~A at ~A~%~D file~:P, ~D inferior director~:@P.~%~%"
	  starpath *backup-dump-tape-name* (time:print-universal-time time nil)
	  (length dumpable-files) (length dirs))
  (funcall *backup-stream* ':write-eof))
	  

(defun dump-file (pathname comp-date inc-date props)
  (let ((dumptime (time:get-universal-time))
	(*DONT-GRATUITOUSLY-DEACTIVATE-DIRECTORIES* T)
	(creation-date (get props ':creation-date)))
    (with-open-file (file pathname ':direction ':dumper ':inhibit-links t
				   ':preserve-dates t ':deleted t)
      (let (uid partid
	    (comp-tape (get props ':complete-dump-tape))
	    (inc-tape (get props ':incremental-dump-tape)))
	(incf *files-dumped*)
	(let ((fd (funcall file ':get-lmfs-fd)))
	  ;;open fd- dont need to lock -- just pointer chasing
	  (setq uid (fd-uid fd) partid (partt-partition-id (fd-partition-desc fd))))
	
	(funcall *backup-stream* ':clear-error)	;for wiseniks who return nonlocally--
	(format *backup-stream*
		"FILE ~A~@
                 PARTITION ~O~@
		 TIME ~A~@
		 CREATION-DATE ~A~@
                 UNIQUE-ID ~O~%"
		(funcall file ':truename)
		partid
		(time:print-universal-time dumptime nil)
		(time:print-universal-time creation-date nil)
		uid)
	
	(format *backup-map-stream*
		"~&  ~A~60T Tape ~A at ~A"
		(funcall (funcall file ':truename) ':string-for-dired)
		*backup-dump-tape-name* (time:print-universal-time dumptime nil))
	
	(if (or inc-date comp-date)
	    (format *backup-map-stream* "~&~15xPrevious dump tapes: "))
	(if comp-date
	    (progn (format *backup-stream* "COMPLETE-DUMP-TAPE ~A~%" comp-tape)
		   (format *backup-stream* "COMPLETE-DUMP-DATE ~A~%"
			   (time:print-universal-time comp-date nil))
		   (format *backup-map-stream* "Complete: ~A at ~A "
			   comp-tape (time:print-universal-time comp-date nil))))
	(if inc-date
	    (progn (format *backup-stream* "INCREMENTAL-DUMP-TAPE ~A~%" inc-tape)
		   (format *backup-stream* "INCREMENTAL-DUMP-DATE ~A~%"
			   (time:print-universal-time inc-date nil))
		   (format *backup-map-stream* "Incremental: ~A at ~A "
			   comp-tape (time:print-universal-time inc-date nil)))))
    
      (format *backup-stream* "END~%")
      (funcall *backup-stream* ':force-output)	;sys 75 standard for end physical record
      (dump-file-i file)
      (push `(:file ,pathname ,creation-date) *backup-dump-history*)
      (or *backup-debug-mode* (not *backup-set-dates*)
	  (funcall pathname ':putprop
		   (list
		     (selectq *backup-dump-type*
		       (:complete  ':completely-backed-up)
		       ((:incremental :consolidated) ':incrementally-backed-up)
		       (t (ferror nil "Odd kind of dump here")))
		     *backup-tape-name-8* dumptime)
		   *backup-dump-dump-name*)))))

(defvar *waiting-file-buf* nil)

(defun dump-file-i (stream)
  (let* ((fd (funcall stream ':get-lmfs-fd))
	 (reclen (partt-dataw-per-record (fd-partition-desc fd)))
	 (totlen (+ (fd-logical-header-length fd) (fd-addressible-length fd)))
	 (osbuf nil))
    (loop for total-addr from 0 below totlen by reclen
	  do
	  (let ((buf nil)
		(err nil)
		(headerp nil))
	    (setq headerp (< total-addr (fd-logical-header-length fd)))
	    (funcall stream ':set-abs-ptr total-addr)
	    (multiple-value (buf err)
	      (with-fs-locked-mv
		(get-buffer-given-fd-and-wordaddr headerp fd
						  (if headerp
						      total-addr
						      (- total-addr
							 (fd-logical-header-length fd))))))
	    (if err (ferror nil "Addressibility error in backup ~A" err))
	    (await-backup-buf osbuf)
	    (funcall *backup-stream* ':file-buffer-out buf (fb-array buf))
	    (setq osbuf buf)))
    (await-backup-buf osbuf)))

(defun await-backup-buf (buf)
  (if buf
      (progn
	(setq *waiting-file-buf* buf)
	(process-wait "Tapebuf" #'(lambda ()
				    (funcall *backup-stream*
					     ':check-file-buffer
				       *waiting-file-buf*)))
	(with-fs-locked (downreference-file-buffer buf)))))




(defun backup-eot-handler ()
  (funcall *backup-stream* ':rewind)
  (funcall *backup-stream* ':set-offline)
  (funcall *backup-stream* ':close)
  (format t "~&End of tape ~A encountered, rewinding it and setting it offline."
	   *backup-dump-tape-name*)
  (format *backup-map-stream*
	  "~&~%End of tape ~A encountered at ~A." *backup-dump-tape-name*
	  (time:print-universal-time (time:get-universal-time) nil))
  (record-backup-times *backup-dump-dump-name*)
  (create-backup-dump-history *backup-dump-dump-name*)
  (do () (())
    (do () (())
      (format t "~&Please enter name of the ~:R tape for ~A: "
	      (1+ *backup-dump-sequence-number*) *backup-dump-dump-name*)
      (let ((read (string-trim " " (readline))))
	(cond ((zerop (string-length read)))
	      ((> (string-length read) 8.)
	       (format t "~&~A is greater than 8 characters long.  That is too long." read))
	      (t (setq *backup-dump-tape-name* read
		       *backup-tape-name-8* (format nil "~8a" read))
		 (incf *backup-dump-sequence-number*)
		 (return t)))))
    (do () (())
      (format t "~&Type single-digit tape drive, when ready, for next tape, SPACE is ~D: "
	      *backup-dump-tape-drive*)
      (let ((c (tyi)))
	(cond ((not (fixp c)) (tv:beep))	;mice, god knows what
	      ((and ( c #/0) ( c #/9))
	       (setq *backup-dump-tape-drive* (- c #/0))
	       (if (tape:drive-ready-p *backup-dump-tape-drive*)
		   (return)
		   (format t
			   "~&Drive ~D doesn't appear to be ready." *backup-dump-tape-drive*)))
	      ((= c #\SP)
	       (format t "~D" *backup-dump-tape-drive*)
	       (if (tape:drive-ready-p *backup-dump-tape-drive*)
		   (return)
		   (format t
			   "~&Drive ~D doesn't appear to be ready." *backup-dump-tape-drive*)))
	      (t (tv:beep)))))
    (if (not (tape:drive-bot-p *backup-dump-tape-drive*))
	(if (yes-or-no-p
	      (format nil "~&Drive ~D is not at BOT.  Are you~@
	      sure drive ~D is the right drive? "
		      *backup-dump-tape-drive* *backup-dump-tape-drive*))
	    (return))
	(return)))
  (format t "~&Continuing ~A on drive ~A, reel #~D."
	  *backup-dump-tape-name* *backup-dump-tape-drive* *backup-dump-sequence-number*)
  (setq  *backup-stream*
	 (tape:open-tape *backup-dump-tape-drive* ':mode ':write ':no-bot-prompt t))
  (backup-deal-with-bot)
  (dump-prelude))

(defun record-backup-times (&special *backup-dump-dump-name*)
  (cond ((and (not *backup-debug-mode*) *backup-set-dates*)
	 (format t "~&Setting backup dates for files just dumped.")
	 (let ((*DONT-GRATUITOUSLY-DEACTIVATE-DIRECTORIES* t))	;performance bum
	   (maphash-equal #'(lambda (ignore path)
			      (let ((p (funcall path ':get *backup-dump-dump-name*)))
				(if p (funcall path ':change-properties t
					       ':internal-backup-info p))))
			  fs:*pathname-hash-table*))))
  (deactivate-cleanup))				;does for whole dumper


;;;
;;; This stuff reads tape and worries about what the tape says and whether it
;;; wants to reload anything in particular or not.

(defvar *reloader-tape-format* nil)
(defvar *reloader-dir-version* nil)

(defun scan-tape (&rest  args)
  (lexpr-funcall #'reloader ':list args))

;;; As of 9/1/81, there are no more version 1 tapes.  

(defvar *backup-prelude-info* nil)
(defvar *backup-cur-directory-info* nil)
(defvar *backup-reload-list* nil)

(defun reloader (&rest args &aux listp)
  (setq args (copylist args))			;delq on stack-consed list may lose?
  (setq listp (memq ':list args))
  (if listp (setq args (delq ':list args)))
  (let ((*backup-tape-drive-number* 0)
	(*reloader-dir-version* nil)
	(*backup-reload-list* (reloader-parse-reload-specs args)))
    (with-open-stream
      (*backup-stream* (tape:open-tape *backup-tape-drive-number* ':no-read-ahead t))

      (funcall *backup-stream* ':check-ready)
      (if (not (funcall *backup-stream* ':bot-p))
	  (if (yes-or-no-p
		(format nil "~&Drive ~D is not at beginning of tape.  Shall I rewind it?~@
		(an answer of /"no/" means read from current point on tape): "
			(funcall *backup-stream* ':unit)))
	      (funcall *backup-stream* ':rewind)))

      (let ((*backup-prelude-info* (reloader-read-prelude))
	    (*backup-cur-directory-info*))
	(reloader-talk-about-prelude)
	(do ((story (reloader-read-tape-story)
		    (reloader-read-tape-story)))
	    (())
	  retry
	  (cond ((get story ':TAPE-ERROR)
		 (let ((maybetype (getl story '(:FILE :DIRECTORY :PRELUDE))))
		   (selectq (car maybetype)
		     (:FILE   (format t "~&Won't reload ~A due to tape error"
				      (cadr maybetype)))
		     (:DIRECTORY (format t "~&Problem reloading directory ~A, ~
				 check for lost information." (cadr maybetype))
				 (remprop story ':TAPE-ERROR)
				 (go retry))
		     (:PRELUDE   (ferror nil "~&Problems reading tape prelude, tape bad."))
		     (t          (format
				   t
				   "~&Cannot reconstruct ASCII tape record due to error.")))
		   (reloader-resync)))
		((get story ':directory)
		 (setq *backup-cur-directory-info* story)
		 (if listp
		     (format t "~&Directory: ~A, ~D file~:P, ~D director~:@P."
			     (get story ':directory)
			     (get story ':file-count)
			     (get story ':directory-count)))
		 (funcall *backup-stream* ':skip-file))
		((get story ':file)
		 (if listp
		     (progn
		       (format t "~&File ~A" (get story ':file))
		       (funcall *backup-stream* ':skip-file))
		     (if (reloader-want-to-reload-p story)
			 (file-reloader story)
			 (funcall *backup-stream* ':skip-file))))
		((getl story '(:prelude))
		 (setq *backup-prelude-info* story)
		 (reloader-talk-about-prelude)
		 (if (and (not listp)
			  (not (yes-or-no-p
				 "Do you want to reload from this dump, as well? ")))
		     (return nil))
		 (funcall *backup-stream* ':skip-file))
		((get story ':end-of-tape)
		 (return nil))))))))

(defun reloader-resync ()			;called when sync is all lost and screwed
  (format t "~&Attempting to resynchronize input tape.")
  (do () (())
    ;;Take advantage of knowing that records begin at buffer address 0
    ;;This guy is supposed to return on an ASCII sequence,having eaten nothing of it.
    (if (not (eq (*catch ':tape-error
		   (condition-bind ((:tape-error #'thrower-of-tape-errors))
		     (funcall *backup-stream* ':skip-file)
		     (multiple-value-bind (buf index how-many)
			 (funcall *backup-stream* ':get-input-buffer)
		       ;;Heuristicate it out
		       (if (and (> how-many 20.)
				;;Should be only printable ascies and CR
				(loop for i from 1 to 20. finally (return t) do
				      (let ((c (aref buf i)))
					(if (and (not (= c #\CR))
						 (or (< c #\SP)
						     (> c #O177)))
					    (return nil))))
				;;Should be a space or CR in the first 20.
				(loop for i from 1 to 20. finally (return nil) do
				      (let ((c (aref buf i)))
					(if (or (= c #\SP) (= c #\CR))
					    (return t))))
				;;First 4 are PREL, DIRE, FILE, etc., must be upcase
				(loop for i from 1 below 4 finally (return t)
				      (let ((c (aref buf i)))
					(if (not (and ( c #/A) ( c #/Z))) (return nil)))))
			   ;;OK, this is surely an ASCII record.
			   (progn
			     (funcall *backup-stream* ':advance-input-buffer index) ;used none
			     (return t))))))
		 'tape-error))
	(return t))))

(defun reloader-talk-about-prelude ()
  (and (get *backup-prelude-info* ':reel)
       (format t "~&Reading tape ~A, dumped ~A by ~A"
	       (get *backup-prelude-info* ':reel)
	       (if (get *backup-prelude-info* ':time)
		   (time:print-universal-time (get *backup-prelude-info* ':time) nil)
		   "???")
	       (get *backup-prelude-info* ':user-id)))
  (and (get *backup-prelude-info* ':sequence)
       (format t "~&Sequence #~D." (get *backup-prelude-info* ':sequence)))
  (and (get *backup-prelude-info* ':comment)
       (format t "~&Comment: ~A" (get *backup-prelude-info* ':comment))))

;;; Some moby design is needed here. At least allow starnames matching subtrees
;;; and files containing same.

(defun reloader-parse-reload-specs (args &aux (specs nil))
  (cond ((null args)
	 (list (ncons t)))
	(t (dolist (spec args)
	     (cond ((stringp spec)
		    (push (ncons (fs:parse-pathname spec *LOCAL-FS-HOST*))
			  specs))
		   ((typep spec 'fs:pathname)
		    (push (ncons spec) specs))))
	   specs)))

;;; Use the cdrs to indicate "got it" and stop at some way/some point

(defun reloader-want-to-reload-p (plist)
  (let ((pathname  (fs:parse-pathname (get plist ':FILE) *LOCAL-FS-HOST*)))
    (dolist (wanted *backup-reload-list*)
      (cond ((eq (car wanted) t)
	     (return t))
	    ((and (string-equal (funcall pathname ':directory)
				(funcall (car wanted) ':directory))
		  (string-equal (funcall pathname ':name)
				(funcall (car wanted) ':name))
		  (string-equal (funcall pathname ':type)
				(funcall (car wanted) ':type))
		  (or (null (funcall (car wanted) ':version))
		      (= (funcall (car wanted) ':version)
			 (funcall pathname ':version))))
	     (return t))
	    (t nil)))))

(defun reloader-read-prelude ()
  (if (not (eq (car (reloader-read-prop-line *backup-stream*)) ':PRELUDE))
      (ferror 'reloader-no-prelude "Prelude apparently missing from this tape."))
  (prog1 (reloader-read-tape-story)	;a plist
	 (funcall *backup-stream* ':skip-file)))

(defun reloader-read-tape-story (&optional (stream *backup-stream*) (nci nil))
  (do ((props nil)) (())
    (let ((parsed (reloader-read-prop-line stream)))
      (selectq (car parsed)
	(:TAPE-ERROR  (return `(nil :TAPE-ERROR . ,props)))
	(:END-OF-TAPE (return '(nil :END-OF-TAPE t)))
	(:END         (or nci (funcall stream ':clear-input))
		      (return (cons nil props)))
	(t (setq props (nconc parsed props)))))))

(defun thrower-of-tape-errors (&rest ignore)
  (funcall *backup-stream* ':clear-error)
  (*throw 'tape-error ':tape-error))

(defun reloader-read-prop-line (stream)
  (let ((o (*catch 'tape-error
	     (condition-bind ((:tape-error #'thrower-of-tape-errors))
	       (let ((line (readline stream nil)))	;EOF option
		 (if (null line)
		     '(:END-OF-TAPE)
		     (reloader-parse-prop-line line)))))))
    (if (eq o ':tape-error)
	'(:TAPE-ERROR)
	o)))

(defun reloader-parse-prop-line (line)
  (let ((startx (string-search-not-char #\SP line)))	;find start of key
    (and startx					;make sure there is one
	 (let* ((endx (string-search-char #\SP line startx))	;now find end of key
		(key (reloader-keyword-inconvert line startx endx)))
	   (if (null endx)
	       (list key nil)
	       (let ((val-startx (string-search-not-char #\SP line endx)))
		 (if (null val-startx)
		     (list key nil)
		     (list key (reloader-keyvalue-inconvert key line val-startx nil)))))))))

(defconst *backup-keywords*
	  ;;  inconvert      outconvert   props
	  '(((zwei:parse-number reloader-dec-print)
	     :VERSION :SEQUENCE :FILE-COUNT :DIRECTORY-COUNT :DEFAULT-DIRECTORY-VERSION
	     :PART-SIZE :TAPE-SYSTEM-VERSION)
	    ((fs:parse-directory-date-property reloader-time-print)
	     :TIME :DUMP-TIME)
	    ((reloader-parse-octal reloader-oct-print)
	     :UNIQUE-ID :PARTITION)
	    ((substring princ)
	     :PRELUDE :FILE :DIRECTORY :REEL :MAP-FILE-PATH :START-PATH :COMMENT
	     :USER-ID :MACHINE :END :DUMP-NAME)
	    ((reloader-upkg-intern prin1)
	     :DELETED-FILES :TYPE)))

(defun reloader-upkg-intern (x start end)
  ;;This winner return-arrays what WE cons!!!
  (si:intern1 (substring x start end) ""))

(defun reloader-parse-octal (x start end)
  (zwei:parse-number x start end 8.))

(defun reloader-keyword-inconvert (keystring start end)
  (loop for type in *backup-keywords*
	finally (return (reloader-upkg-intern keystring start end))
	do
	(let ((keyword (dolist (x (cdr type))
			 (if (string-equal keystring x start 0 end)
			     (return x)))))
	  (if keyword
	      (return keyword)))))

(defun reloader-keyvalue-inconvert (key line start end)
  (loop for type in *backup-keywords*
	finally (return (substring line start end))
	do
	(if (memq key (cdr type))
	    (return (funcall (caar type) line start end)))))

;;; This stuff really reloads stuff using file system guts and filogenesis.

(defun file-reloader (plist &aux what-happened)
  (let ((path (fs:parse-pathname (get plist ':file) *LOCAL-FS-HOST*)))
    (if (let ((probism (open path '(:probe :deleted))))
	  (not (stringp probism)))
	(format t "~&Can't reload ~A, there already" path)
	(setq what-happened
	      (*catch
		':tape-error
		(condition-bind ((:tape-error #'thrower-of-tape-errors)
				 (:reloader-data-format-error
				   #'(lambda (&rest ignore)
				       (*throw ':tape-error 'format-garbage))))
		  (funcall *backup-stream* ':clear-input)
		  (with-fs-locked
		    (let ((fd (reload-file *the-partition*)))
		      (multiple-value-bind (par err)
			  (reloader-get-dir path)
			(if err
			    (format t "~&Can't get dir for ~A:~%~A" path err)
			    (reloader-add-dire fd par path)))
		      (if (fd-parent fd)
			  (check-deactivate-file fd)
			  (mapc 'disconnect-buffer-from-file (fd-buffer-list fd)))))))))
    (selectq what-happened
      (tape-error     (format t "~&Did not reload ~A due to tape error." path)
		      (reloader-resync))
      (format-garbage (format t "~&Did not reload ~A due to apparent garbage on tape." path))
      (t nil)))
  (or (eq what-happened 'tape-error) (funcall *backup-stream* ':skip-file)))

(defun reloader-get-dir (filepath)		;returns fd, err
  (let* ((dname (funcall filepath ':directory))
	 (mypath (funcall (fs:parse-pathname dname *LOCAL-FS-HOST*)
			  ':new-pathname ':type ':directory ':version 1))
	 (myename (funcall mypath ':name)))

    ;; Try to get this directory outright

    (multiple-value-bind (fd err)
	(get-filedesc-from-path dname ':directory 1)
      (cond ((null err))			;fall right thru, fine. found it.
	    ((or (string-search " DNF " err)	;didnt find it, get the parent and create
		 (string-search " NSD " err))
	     (multiple-value-bind (parfd parerror)
		 (reloader-get-dir mypath)	;GETS **PARENT**
	       (if parerror
		   (setq err parerror)
		   (multiple-value (fd err)
		       (create-directory
			 (fd-partition-desc parfd) parfd myename))
		   (if (null err)
		       (format t "~&Created ~A." mypath)
		       (format t "~&Couldn't create ~A:~%~A" mypath err))))))	;my answers are final
      (values fd err))))

(defun reloader-add-dire (fd par path)
  (with-fileheader-addressibility (fd 0 h)
    (format t "~&Adding ~A ~A ~D to ~A"
	    (fd-file-name fd) (fd-file-type fd) (fd-file-version fd)
	    (funcall path ':directory))
    
    (with-fileheader-addressibility (fd (file-header-dire-location h) hdire)
      
      ;; Could search the dir linearly here for the uid, in case it is only
      ;; ex which is wrong.
      
      (multiple-value-bind (dire ex xerr ignore ignore ignore)
	  (get-new-directory-entry par (fd-file-name fd) (fd-file-type fd)
				   (fd-file-version fd))
	(if xerr
	    (format t "~&Cannot add ~A ~A ~D to ~S~%~A"
		    (fd-file-name fd) (fd-file-type fd)
		    (fd-file-version fd) par xerr)
	    (progn
	      
	      ;; Dont attach as son of parent, fd isnt really kosher enough to serve.
	      (setf (fd-parent fd) par)
	      (setf (fd-entry-index fd) ex)
	      (with-fileheader-addressibility-modifying
		(fd (file-header-info-location h) fhi)
		(setf (fh-info-parent-dir-uid fhi) (fd-uid par))
		(setf (fh-info-parent-dir-part-id fhi)
		      (partt-partition-id (fd-partition-desc par)))
		(setf (fh-info-parent-dir-address fhi) (fd-r0addr par))
		(setf (fh-info-dir-entry-index-in-parent fhi) ex)
		(write-out-buffer-from-addressor fhi))
	      
	      (set-bufmod-from-addressor dire)
	      (copy-directory-entry hdire dire)
	      (write-out-buffer-from-addressor dire)
	      (downreference-addressor dire)))))))

(defun barfing-get-free-addr (partition)
  (let ((addr (allocate-disk-record partition)))
    (if (fixp addr)
	addr
	(ferror nil "Out of room in partition ~S" partition))))
    

(defun reload-file (partition)
  (let ((dwpr (partt-dataw-per-record partition))
	(headerlen 99999.)
	(h1buf nil)
	(dbuf nil)
	(hbufs nil)
	(hrecs 1)
	(records-expected 0)
	(recs-alloced nil)
	(r0addr nil)
	(fd (make-file-desc
	      partition-desc partition
	      parent	     nil
	      uid	     (generate-file-uid partition)
	      logical-header-length 99999.)))
    (unwind-protect
      (progn
	(setq h1buf (allocate-random-buffer))
	(funcall *backup-stream* ':fill-input-file-buffer h1buf (fb-array h1buf))
	(funcall *backup-stream* ':await-input-file-buffer h1buf)
	(push h1buf (fd-buffer-list fd))
	(set-up-buf-as-header-containing h1buf fd 0)
	(set-buffer-modified h1buf)		;prevent chkwordsmen from freeing...
	(with-fileheader-addressibility (fd 0 fh)
	  (setq headerlen  (file-header-logical-size fh))
	  (if (or (> headerlen 5000.)
		  (< headerlen (directory-entry-size-in-words))
		  ( (file-header-version fh) 1)
		  (< (file-header-number-of-elements fh) 5)
		  (> (file-header-number-of-elements fh) 20.)
		  ( (file-header-header-fm-location fh)
		     (partt-dataw-per-record partition)))
	      (ferror ':reloader-data-format-error
		      "File header on tape appears to be garbage"))
	  (with-fileheader-addressibility (fd (file-header-header-fm-location fh) hfm)
	    (if (or (> (file-map-valid-length hfm) (file-map-allocated-length hfm))
		    (> (file-map-allocated-length hfm) 16.)
		    (zerop (file-map-valid-length hfm))
		    (zerop (file-map-element hfm 0)))
		(ferror ':reloader-data-format-error
			"Header file map on tape appears to be garbage")))
	  (setf (fd-logical-header-length fd) headerlen)
	  (setq r0addr (barfing-get-free-addr partition))
	  (push r0addr recs-alloced)
	  (setf (fd-r0addr fd) r0addr)
	  (set-up-buf-as-header-containing h1buf fd 0)	;reestablish
	  (place-buffer-block-check-words h1buf nil)
	  (setf (fb-address h1buf) r0addr)
	  (with-fileheader-addressibility-modifying (fd (file-header-dire-location fh) dire)
	    (format t "~&Reloading ~A.~A.~D"
		    (directory-entry-file-name dire)
		    (directory-entry-file-type dire)
		    (directory-entry-file-version dire))
	    (setf (fd-file-name fd) (directory-entry-file-name dire))
	    (setf (fd-file-type fd) (directory-entry-file-type dire))
	    (setf (fd-file-version fd) (directory-entry-file-version dire))

	    (setf (directory-entry-record-0-address dire) r0addr)
	    (setf (directory-entry-date-time-created dire) (time:get-universal-time))
	    (setf (directory-entry-unique-ID dire) (fd-uid fd)))
	  (with-fileheader-addressibility-modifying (fd (file-header-header-fm-location fh)
							hfm)
	    (setf (file-map-element hfm 0) r0addr)
	    (setq hrecs (file-map-valid-length hfm))
	    (loop for i from 1 below hrecs
		  do
		  (push (barfing-get-free-addr partition) recs-alloced)
		  (push (allocate-random-buffer) hbufs)
		  (setf (file-map-element hfm i) (car recs-alloced))
		  (let ((hbuf (car hbufs)))	;just put it there
		    (push hbuf (fd-buffer-list fd))
		    (funcall *backup-stream* ':fill-input-file-buffer hbuf (fb-array hbuf))
		    (funcall *backup-stream* ':await-input-file-buffer hbuf)
		    (set-up-buf-as-header-containing hbuf fd (* i dwpr))
		    (set-buffer-modified hbuf)	;issue with the checkwordsmen?
		    (place-buffer-block-check-words hbuf nil)
		    (setf (fb-address hbuf) (car recs-alloced))
		    (set-buffer-modified hbuf))))
	  ;; Now we have enough addressibility to hack the regular file map.
	  (with-fileheader-addressibility-modifying (fd (file-header-info-location fh) fhi)
	    (setf (fh-info-duplicate-uid fhi) (fd-uid fd)))

	  (do ((fma (file-header-file-map-location fh))) ((zerop fma))
	    (with-fileheader-addressibility (fd fma fm)
	      ;; would like to return nil out of do, but unwind-protect is broken.
	      (if (zerop (file-map-valid-length fm))
		  (setq fma 0)
		  (progn
		    (set-bufmod-from-addressor fm)
		    (loop for i from 0 below (file-map-valid-length fm)
			  do
			  (incf records-expected)
			  (push (barfing-get-free-addr partition) recs-alloced)
			  (setf (file-map-element fm i) (car recs-alloced)))
		    (setq fma (file-map-link fm))))))

	  ;;Ok, all file maps fine. Now read in file.

	  (loop for i from 1 to records-expected
		do
		(setq dbuf (allocate-random-buffer))
		(funcall *backup-stream* ':fill-input-file-buffer dbuf (fb-array dbuf))
		(funcall *backup-stream* ':await-input-file-buffer dbuf)
		(setf (fb-address dbuf)
		      (get-record-address-for-io fd (* dwpr (+ i (1- hrecs)))))
		(set-buffer-modified dbuf)
		(set-up-buf-as-data-containing
		  dbuf fd
		  (if (zerop (\ headerlen dwpr))
		      (* (1- i) dwpr)
		      (+ (* (1- i) dwpr) (- dwpr (\ headerlen dwpr)))))
		(place-buffer-block-check-words dbuf nil)
		(without-interrupts
		      (push dbuf (fd-buffer-list fd))
		      (setq dbuf nil))
		(if (or (null *WRITE-BEHIND*)
			(zerop (\ i (1+ *WRITE-BEHIND-COUNT*))))
		    (flush-buffers-for-file fd t)))	;don't do headers
	  
	  (set-bufmod-from-addressor fh))
	(flush-buffers-for-file fd)
	(setq hbufs nil dbuf nil recs-alloced nil r0addr nil h1buf nil)
	fd)					;let some outer guy hack the dire

      ;; Unwind protect cleanups

      (if dbuf (push dbuf hbufs))
      (if h1buf
	  (progn
	    (zero-file-buffer h1buf)
	    (if (and (fb-address h1buf)
		     (not (zerop (fb-address h1buf)))
		     (equal r0addr (fb-address h1buf)))
		(write-out-file-buffer h1buf))
	    (push h1buf hbufs)))
      (dolist (buf hbufs)
	(blast-clear-buffer buf)
	(deallocate-resource 'fs-buffer buf))
      (and h1buf
	   fd (dolist (buf (fd-buffer-list fd))
		(if (fb-file-desc buf)
		    (progn
		      (blast-clear-buffer buf)
		      (deallocate-resource 'fs-buffer buf)))))
      (if r0addr (deposit-disk-record r0addr partition))
      (dolist (r recs-alloced) (deposit-disk-record r partition)))))

(defvar *cbdh-ht-addr* 0)
(defvar *cbdh-strings* nil)			;don't know ht order


(defvar *dump-directory-template* nil)

(defun dump-directory-template ()
  (or *dump-directory-template*
      (setq *dump-directory-template*
	    (fs:parse-pathname (string-append *PATH-DELIMITER* "dump-maps"
					      *PATH-DELIMITER* "x.directory")
			       *LOCAL-FS-HOST*))))

(defun create-backup-dump-history (dumpid)
  (let ((ht (make-equal-hash-table))
	(*cbdh-ht-addr* 6)			;never gets to be zero, allows zero sentinels
	(*cbdh-strings*  (list "DUMY")))		;so that ht works, reader will skip.
    (setq *backup-dump-history* (reverse *backup-dump-history*))	;let fcn restart,
						;nreverse is non-obvious screw to user
    (setq dumpid (cbdh-hash ht dumpid))
    (let ((file-image
	    (loop for (type object creation-date) in *backup-dump-history*
		  collect
		  (selectq type
		    (:file       (list (cbdh-hash ht (funcall object ':name))
				       (cbdh-hash ht (funcall object ':type))
				       (funcall object ':version)
				       creation-date))
		    (:directory  (list (cbdh-hash ht (funcall object ':string-for-host))
				       0 0 0))))))
      (let ((path (funcall (dump-directory-template)
			   ':new-name *backup-dump-tape-name*))
	    (newp t)
	    (adrbas 0))
	(if (probef path)			;exists already
	    (setq newp nil))
	(with-open-file (file path
			      ':direction (if newp ':output ':append)
			      ':characters nil ':byte-size 8.)
	  (if (not newp)
	      (setq adrbas (funcall file ':read-pointer)))
	  (format t "~&~:[Appending to~;Creating~] ~A" newp  (funcall file ':truename))
	  (dolist (s (nreverse *cbdh-strings*))
	    (cbdh-put-fix file (string-length s) 2)
	    (funcall file ':string-out s))
	  (cbdh-put-fix file 0 3)
	  (let ((map-start (funcall file ':read-pointer)))
	    (dolist (entry file-image)
	      (cbdh-put-fix file (first entry) 3)
	      (cbdh-put-fix file (second entry) 3)
	      (cbdh-put-fix file (third entry) 3)
	      (cbdh-put-fix file (fourth entry) 4))
	    (cbdh-put-fix file 0 3)
	    (let ((map-end (funcall file ':read-pointer)))
	      (cbdh-put-fix file 1 2)		;version
	      (cbdh-put-fix file adrbas 3)	;this start, previous end
	      (cbdh-put-fix file dumpid 3)
	      (cbdh-put-fix file map-start 3)	;where this map starts
	      (cbdh-put-fix file map-end 3)))))))	;LAST THING = where this trailer start
  (setq *backup-dump-history* nil))

(defun cbdh-put-fix (file num bytes)
  (dotimes (i bytes)
    (funcall file ':tyo (ldb (logior #O08 (lsh (* i 8) 6)) num))))

(defun cbdh-hash (ht string)
  (or (gethash-equal string ht)
      (prog1
	(puthash-equal string *cbdh-ht-addr* ht)
	(push string *cbdh-strings*)
	(incf *cbdh-ht-addr* (+ 2 (string-length string))))))

(defstruct (dump-directory :named
			   (:type :array-leader)
			   (:conc-name dmpd-)
			   (:make-array (:dimensions total-length :type 'art-string)))
  (phys-len total-length)
  (string-start 0)
  string-len
  map-start
  map-len
  version
  dumpid
  (previous nil))


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
	    (let ((dmpd (make-dump-directory
			  string-start  adrbas
			  map-start     map-start
			  map-len       (- map-end map-start)
			  version       1
			  dumpid        nil
			  previous      (if (zerop adrbas) nil adrbas))))
	      (funcall file ':set-pointer adrbas)
	      (if (funcall file ':get-handler-for ':get-input-buffer)
		  (do ((ix 0) (left total-length)) (())
		    (multiple-value-bind (buf bufx count)
			(funcall file ':get-input-buffer)
		      (if (null buf) (return))
		      (setq count (min count left))
		      (copy-array-portion buf bufx (+ bufx count) dmpd ix (+ ix count))
		      (incf ix count)
		      (decf left count)
		      (funcall file ':advance-input-buffer)))
		  (dotimes (i total-length)
		    (aset (funcall file ':tyi) dmpd i)))
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

(defun dmpd-read-fixnum (dmpd adr n &aux (val 0))
  (dotimes (i n)
    (setq val (dpb (aref dmpd (+ adr i))
		   (logior #O08 (lsh (* i 8) 6))
		   val)))
  val)


(defun display-tape-directory (name &aux dumps)
  (let ((path (funcall (dump-directory-template) ':new-name (string name))))
    (with-open-file (file path '(:fixnum :byte-size 8))
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
