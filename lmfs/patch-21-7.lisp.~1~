;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.7
;;; Reason: Server FILEPOS didn't work.
;;; Written 12/17/81 12:24:56 by BSG,
;;; while running on Beagle from band 1
;;; with System 78.17, ZMail 38.2, Experimental Symbolics 8.4, Experimental Tape 6.1, Experimental LMFS 21.6, Canon 9.0, microcode 840.



; From file SERVER.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

;;; Random commands.

(defun file-server-filepos (fh rest)
  (let ((data (get fh server-instance)))
    (if (null data)
	(format stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh)
	(let ((direction (server-dataproc-comm-iotype data))
	      (opening (server-dataproc-comm-opening data))
	      (cell (server-dataproc-comm-cell data)))
	  (format stream "~A ~A FILEPOS" tid fh)
	  (funcall stream ':force-output)
	  (await-data-process cell 'fpsync)
 	  (funcall opening ':set-pointer (car rest))
	  (rplaca cell (if (eq direction 'input) 'read 'write))))))

)

; From file SERVER.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun file-server-data-top-level (server-instance cell handle
				   &aux &special (fs:*local-server-via-net* nil))
  (trap-lossage (nil "File Server Data Connection")
   (do () (())
     (process-wait "ServDCWait" #'car cell)
     (let* ((data (get handle server-instance))
	    (opening (server-dataproc-comm-opening data))
	    (dconn (server-dataproc-comm-conn data))
	    (binp (server-dataproc-comm-binp data)))
      (selectq (car cell)
 	 (undata				;Gute Nacht, O Wesen.
	  (rplaca cell nil)
	  (return nil))
	 
	 ((fpsync wsync)
	  (send-sync-mark dconn)
	  (rplaca cell nil))
	 
	 (directory
	  (server-dataproc-hack-directory data handle)
	  (rplaca cell nil))
	 (write
	  (if (null opening) (ferror nil "file-server-data-top-level - no opening"))
	  (do () (())
	    (if (not (eq (car cell) 'write)) (return nil))
	    (let* ((pkt (chaos:get-next-pkt dconn)))
	      (select (chaos:pkt-opcode pkt)
		(chaos:eof-op
		    (chaos:return-pkt pkt)
		    (setq pkt (chaos:get-next-pkt dconn))
		    (or (= (chaos:pkt-opcode pkt) fs:%file-synchronous-mark-opcode)
			(break this-break-will-be-removed))
		    (chaos:return-pkt pkt)
		    (rplaca cell nil)
		    (return nil))
		(fs:%file-binary-opcode
		    (funcall opening ':string-out pkt chaos:first-data-word-in-pkt
			     (+ (// (chaos:pkt-nbytes pkt) 2) chaos:first-data-word-in-pkt))
		    (chaos:return-pkt pkt))
		(fs:%file-character-opcode
		    (funcall opening ':string-out (chaos:pkt-string pkt)
			     0 (chaos:pkt-nbytes pkt))
		    (chaos:return-pkt pkt))
		(otherwise (ferror nil "Unknown pkt opcode: ~O" (chaos:pkt-opcode pkt)))))))

	 (read
	  (if (null opening) (ferror nil "file-server-data-top-level - no opening"))
	  (do (last eofp) (())
            (if (server-window-read-check cell dconn) (return nil))
	    (let ((pkt (chaos:get-pkt)))
	      (cond (binp
		       (multiple-value (last eofp)
			 (funcall opening ':string-in nil pkt
				  chaos:first-data-word-in-pkt chaos:max-data-words-per-pkt))
		       (setf (chaos:pkt-opcode pkt) fs:%file-binary-opcode)
		       (setf (chaos:pkt-nbytes pkt)
			     (* 2 (- last chaos:first-data-word-in-pkt))))
		    (t (multiple-value (last eofp)
			 (funcall opening ':string-in nil (chaos:pkt-string pkt)
				  0 chaos:max-data-bytes-per-pkt))
		       (setf (chaos:pkt-opcode pkt) fs:%file-character-opcode)
		       (setf (chaos:pkt-nbytes pkt) last)))
	      (if (plusp (chaos:pkt-nbytes pkt))
		  (chaos:send-pkt dconn pkt (chaos:pkt-opcode pkt))	;don't let SEND dft it
		  (chaos:return-pkt pkt))
	      (cond (eofp
		     (if (server-window-read-check cell dconn) (return nil))
		     (chaos:send-pkt dconn (chaos:get-pkt) chaos:eof-op)
		     (rplaca cell nil)
		     (return nil))))))
	 (t (ferror nil "Bogus com-cell value: ~S" (car cell))))))
    (trap-lossage (nil "Data Conn error closeout")
       (let ((sib (server-dataproc-comm-sibling (get handle server-instance))))
	 (and sib (funcall (server-dataproc-comm-data-proc sib) ':kill))))))

)

; From file SERVER.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun server-window-read-check (cell dconn)
  (do () (())
    (if (neq (car cell) 'read) (return t))
    (if (chaos:may-transmit dconn) (return nil))
    (process-wait "SyncNETO" #'(lambda (cell dconn)
				 (or (neq (car cell) 'read)
				     (chaos:may-transmit dconn)))
		  cell dconn)))

)
