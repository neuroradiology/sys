;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.31
;;; Reason: Backup: don't "RE"-set dates, close map earlier, dumper openings in peek work.
;;; Written 1/07/82 12:51:03 by BSG,
;;; while running on Basset from band 2
;;; with System 78.44, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.29, Canon 9.11, microcode 841.



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
	  (format t "~&You may demount reel ~A now if you wish." *backup-dump-tape-name*)
	  (backup-dump-end-incantations))
	(record-backup-times *backup-dump-dump-name*)
	(create-backup-dump-history *backup-dump-dump-name*))))
  (format t "~& ~A completed." *backup-dump-dump-name*))

)

; From file backup.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun record-backup-times (&special *backup-dump-dump-name*)
  (cond ((and (not *backup-debug-mode*) *backup-set-dates*)
	 (format t "~&Setting backup dates for files just dumped.")
	 (let ((*DONT-GRATUITOUSLY-DEACTIVATE-DIRECTORIES* t))	;performance bum
	   (maphash-equal #'(lambda (ignore path)
			      (let ((p (funcall path ':get *backup-dump-dump-name*)))
				(cond (p (funcall path ':change-properties t
					       ':internal-backup-info p)
					 (funcall path ':remprop *backup-dump-dump-name*)))))
			  fs:*pathname-hash-table*))))
  (deactivate-cleanup))				;does for whole dumper

)

; From file fsstr.lisp >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (lmfs-dumper-opening-mixin :peek-file-system) (&optional (indent 0))
  "Returns a scroll item describing a stream"
  (tv:scroll-parse-item
    ':mouse `(nil :eval (tv:peek-file-system-stream-menu ',self)
		  :documentation "menu of useful things to do to this open file.")
    (and ( indent 0) (format nil "~vx" indent))
    "Dumper opening, "
    (funcall (funcall-self ':pathname) ':string-for-printing)
    ", "
    `(:function ,#'(lambda (stream)
		     (setf (tv:value 0)  (multiple-value-list
					   (funcall stream ':who-line-information)))
		     (third (tv:value 0)))	;byte address
		(,self) nil ("~D"))
    " words "
    `(:function ,#'(lambda ()
		     (fourth (tv:value 0)))
		() nil ("~@[ (~D%)~]"))))

)
