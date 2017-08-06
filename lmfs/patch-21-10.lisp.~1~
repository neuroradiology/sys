;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.10
;;; Reason: (lmfs:print-server-lossages) to get at error log nicely.
;;; Written 12/17/81 16:07:47 by BSG,
;;; while running on Terrier from band 1
;;; with System 78.16, ZMail 38.2, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.6, Canon 9.0, microcode 840.



; From file SERVER.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmacro trap-lossage ((lossagename id) code &body errors)
  (let ((magic (gensym))
	(value (gensym)))
    `(let ((,value
	    (*catch ',magic
	      (condition-bind ((,lossagename
				#'(lambda (&rest msg)
				    (if lmfs-debug-server
					nil
				      (progn (tv:notify nil "File server got an error.")
					     (push (server-mung-error msg ',id)
						   lmfs-server-lossages)
					     (*throw ',magic ',magic))))))
		,code))))
       (if (eq ,value ',magic)
	   (progn . ,errors)
	   ,value))))

)

; From file SERVER.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun server-mung-error (msg id)		;MUST be called in error environment
  (setq msg (copylist msg))			;stack consed list won't do
  (if (eq (car msg) 'chaos:los-received-state)
      (rplaca (last msg) (string-append (car (last msg)))))	;DONT point at pkt!
  (list
    (cv-time (time:get-universal-time))
    id msg))

)

; From file SERVER.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun rfile-server ()
  (let (tid stream conn alldatas server-openings
	(server-instance (gensym))		;bind em all local....
	&special (user-id "File Server")
	(server-protocol-version server-protocol-version)
	(fs:*local-server-via-net* nil))
    (if (or (null user-id)
	    (zerop (string-length user-id)))
	(progn
	  (setq user-id "File Server")
	  (print-server-login-exegesis)))
    (trap-lossage (nil "Server Top Level")
	(progn
	    (setq conn (chaos:listen "FILE"))
	    (if *lmfs-server-dont-answer-logins*
		(progn
		  (chaos:reject (prog1 conn (setq conn nil))
				*lmfs-server-dont-answer-logins*)
		  (ferror nil *lmfs-server-dont-answer-logins*)))
	    (let* ((pkt (chaos:read-pkts conn))	;s/b rfc
		   (result (server-parse-rfc pkt)))
	      (cond ((fixp result)
		     (setq server-protocol-version result))
		    (t (chaos:reject (prog1 conn (setq conn nil)) result)
		       (ferror nil result))))
	    (chaos:accept conn)
	    (funcall tv:who-line-file-state-sheet ':add-server conn "File")
	    (setq stream (chaos:stream conn))
	    (do ((pkt) (op)) (())
	      (setq pkt (trap-lossage (nil "Server Reading packets")
				      (chaos:get-next-pkt conn)
			  (ferror ':server-control-conn-network-lossage
				  "Control connection lost")))
	      (setq op (chaos:pkt-opcode pkt))
	      (cond ((or (= op chaos:eof-op)
			 (= op chaos:cls-op))
		     (funcall stream ':force-output)
		     (chaos:return-pkt pkt)
		     (return nil))
		    ((not (= op chaos:dat-op))
		     (ferror nil "Unrecognized packet opcode: ~S" op)))
	      (let* ((string (chaos:pkt-string pkt))
		     (strings (get-strings-from-pktstring string)))	;nl-delimited strings
	    
		(destructuring-bind (tid fh cmd . rest) (parse-cmd-string string)
		  (if *lmfs-server-dont-answer-logins*
		      (format stream "~A ~A ERROR HNA F Host not available - ~A "
			      tid (or fh "")
			      *lmfs-server-dont-answer-logins*)
		      (selectq cmd
			(:login (setq user-id (file-server-login rest)))
			(:open   (file-server-open fh rest (car strings)))
			(:data-connection (file-server-data-connection fh rest))
			(:undata-connection (file-server-undata-connection fh))
			(:close (file-server-close-connection fh))
			(:filepos (file-server-filepos fh rest))
			(:delete (file-server-delete fh strings))
			(:rename (file-server-rename fh strings))
			(:expunge (file-server-expunge fh strings))
			(:complete (file-server-complete fh rest strings))
			;; I have been informed that s-b-s is an obsl. crock.
			;;(:set-byte-size (file-server-set-byte-size fh rest))
			(:directory (file-server-directory fh rest strings))
			(:change-properties (file-server-change-props fh strings))
			(otherwise (format stream "~A ~A ERROR UKC F Unknown command: ~A"
					   tid (or fh "") cmd)))))
		(funcall stream ':force-output)
		(chaos:return-pkt pkt)))))
    (if conn (trap-lossage (nil "Server Top Level close")
			   (chaos:close conn
					(or *lmfs-server-dont-answer-logins*
					    "Server error"))))
    (if conn (chaos:remove-conn conn))
    (if server-openings
	(trap-lossage (nil "Server finish closing remaining openings")
		      (dolist (opening server-openings)
			(funcall opening ':close ':abort))))
    (trap-lossage (nil "Closeout undata") (dolist (cell alldatas) (rplaca cell 'undata)))))

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
	 (and sib
	      (setq sib (get sib server-instance))
	      (funcall (server-dataproc-comm-data-proc sib) ':kill))))))

)

; From file SERVER.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun print-server-lossages ()
  (if (null lmfs-server-lossages)
      (format t "~&No lossages.")
    (dolist (l lmfs-server-lossages)
      (destructuring-bind (time key (err . args)) l
	(format t "~&~A  ~A~45T ~S~%~10T~A"
		time key err
		(if (stringp (car args))
		    (lexpr-funcall #'format nil args)
		  (format nil "~S" args))))
      (if (funcall standard-input ':listen)
	  (return nil)))))

)
