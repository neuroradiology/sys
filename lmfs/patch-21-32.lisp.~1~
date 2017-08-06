;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.32
;;; Reason: Gross sync lossage ("FileFinish" multifile problem) in server CLOSE.
;;; Written 1/07/82 19:36:08 by BSG,
;;; while running on Basset from band 4
;;; with System 78.44, ZMail 38.5, Symbolics 8.9, Tape 6.5, LMFS 21.31, Canon 9.11, microcode 841.



; From file server.lisp >lmfs POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun file-server-close-connection (fh)
  (let ((data (get fh server-instance)))
    (if (null data)
	(format stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh)
	(let ((direction (server-dataproc-comm-iotype data))
	      (opening (server-dataproc-comm-opening data))
	      (cell (server-dataproc-comm-cell data)))
	  (cond ((null opening)
		 (format stream "~A ~A ERROR UFH F No opening on handle ~A" tid fh fh))
		(t 
		 (if (eq direction 'input)
		     (rplaca cell 'wsync))
		 (cond ((eq opening 'directory)
			(format stream "~A ~A CLOSE" tid fh))
		       (t
			(selectq server-protocol-version
			  (0
			   (format stream "~A ~A CLOSE ~D ~A ~D~%~A~%"
				   tid fh
				   (funcall (funcall opening ':truename) ':version)
				   (time:print-universal-time
				     (funcall opening ':creation-date) nil)
				   (funcall opening ':length)
				   (funcall opening ':truename)))
			  (1
			   (format stream "~A ~A CLOSE ~A ~D~%~A~%"
				   tid fh
				   (cv-time (funcall opening ':creation-date))
				   (funcall opening ':length)
				   (funcall opening ':truename))))))
		 (funcall stream ':force-output)
		 (if (eq direction 'input)
		     (process-wait "ReadFinish" #'null-car cell)
		   (process-wait "WriteFinish" #'null-car cell))	;!!
		 (setf (server-dataproc-comm-opening data) nil)
		 (cond ((not (eq opening 'directory))
			(funcall opening ':close)
			(setq server-openings (delq opening server-openings))))))))))

)
