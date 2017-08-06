;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.23
;;; Reason: Backup operation improvements. "^@^@^@^@^@" bug "therein" trounced.
;;; Written 12/28/81 10:35:20 by BSG,
;;; while running on Terrier from band 1
;;; with System 78.40, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.22, Canon 9.8, microcode 841.



; From file fsstr.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (lmfs-opening-mixin :send-output-buffer) (buf endindex)
  (or (eq buf cur-data-addressor)
      (ferror nil "You gave me some other buffer than I gave you."))
  ;;Incremental-update files can get their buffers gratuitously written out by others,
  ;;and thus lose the bufmod flag.  So the next operation, which is cheap, is a
  ;;reasonable one to perform here.
  (set-bufmod-from-addressor cur-addressor)
  (let* ((eaten (- endindex cur-byte-offset))
	 (newptr (+ cur-byte-address eaten)))
    (setq bytes-in-file (max bytes-in-file newptr))
    (setq cur-byte-address newptr)
    (decf bytes-left-in-block eaten)
    (incf cur-byte-offset eaten)))

)

; From file backup.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defvar *backup-operator*)
)

; From file backup.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun backup-dumper (&rest args)
  (let ((*backup-dump-tape-name* "")
	(*backup-operator* user-id)
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
		(:operator    (setq *backup-operator* prop))
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

)

; From file backup.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

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
      (*backup-operator* "Person operating" :string)
      (*backup-set-dates* "Set date dumped" :boolean)
      (*backup-dump-comment* "Comment" :string))
    ':margin-choices
      '("Do It" ("Abort" . ((*throw 'dumper-abort 'dumper-abort))))
    ':label *backup-dump-dump-name*))

)

; From file backup.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun backup-dump-prelude-and-map-start-incantations ()
  (format t "~&Beginning ~A dump.~@
             The name of the dump is /"~A/".~@
             The Tape Reel ID is ~A~@
             The dump map is being written to ~A.~%"
	  (cdr (rassq *backup-dump-type* *backup-dump-type-names*))
	  *backup-dump-dump-name*
	  *backup-dump-tape-name*
	  (funcall *backup-map-stream* ':truename))
  (format *backup-map-stream* "Dumped by ~A on machine ~A~%~%" *backup-operator*
	  si:local-host)
  (dump-prelude)
  (format *backup-map-stream*
	  "~&First tape of ~A dump.~@
             The name of the dump is /"~A/".~@
             The Tape Reel ID is ~A~%"
	  (cdr (rassq *backup-dump-type* *backup-dump-type-names*))
	  *backup-dump-dump-name*
	  *backup-dump-tape-name*))

)

; From file backup.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

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
	   *backup-dump-comment* *backup-operator* (chaos:host-data))
  (funcall *backup-stream* ':write-eof))

)

; From file backup.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

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
	  (create-backup-dump-history *backup-dump-dump-name*)))))
  (format t "~& ~A completed." *backup-dump-dump-name*))

)
