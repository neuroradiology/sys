;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.33
;;; Reason: Backup: Write EOFs after EOT (interim kludge).
;;; Written 1/08/82 10:09:18 by BSG,
;;; while running on Basset from band 5
;;; with System 78.45, ZMail 38.5, Symbolics 8.9, Tape 6.5, LMFS 21.32, Canon 9.11, microcode 841.



; From file backup.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun backup-eot-handler ()
  ;;; This is a kludge to be fixed in next version of TAPESTR
  (condition-bind ((:end-of-tape #'(lambda (&rest ignore)
				     (*throw 'backup-eot-done nil))))
    (*catch 'backup-eot-done (funcall *backup-stream* ':write-eof))
    (*catch 'backup-eot-done (funcall *backup-stream* ':write-eof)))
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

)
