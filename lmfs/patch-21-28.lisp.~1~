;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.28
;;; Reason: Dump map format and NIL-reporting problems.
;;; Written 1/01/82 22:51:44 by BSG,
;;; while running on Beagle from band 2
;;; with System 78.41, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.26, Canon 9.11, microcode 841.



; From file backup.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

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
		"~&  ~32A (~A by ~A) on ~A, ~A"
		(funcall (funcall file ':truename) ':string-for-dired)
		(time:print-universal-time creation-date nil)
		(get props ':author)
		*backup-dump-tape-name*
		(time:print-brief-universal-time dumptime nil *backup-dump-start-time*))
	
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
			   inc-tape (time:print-universal-time inc-date nil)))))
    
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

)
