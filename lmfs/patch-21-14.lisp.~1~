;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.14
;;; Reason: Server: Chaos closings in control connection not to be considered errors.
;;; Written 12/18/81 10:45:56 by BSG,
;;; while running on Terrier from band 1
;;; with System 78.16, ZMail 38.2, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.6, Canon 9.0, microcode 840.



; From file SERVER.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun rfile-server ()
  (if (or (null user-id)
	  (zerop (string-length user-id)))
      (trap-lossage (nil "Server Login")
	  (progn
	    (login "File Server" si:local-host)
	    (print-server-login-exegesis))))
  (let (tid stream conn alldatas server-openings
	(server-instance (gensym))		;bind em all local....
	&special (user-id "File Server")
	(server-protocol-version server-protocol-version)
	(fs:*local-server-via-net* nil))
    (trap-lossage (nil "Server Top Level")
        (*catch 'server-chaos-disappear
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
				    (condition-bind
				      ((chaos:host-down
					 #'(lambda (&rest ignore)
					     (*throw 'server-chaos-disappear nil)))
				       (chaos:los-received-state
					 #'(lambda (&rest ignore)
					     (*throw 'server-chaos-disappear nil))))
				      (chaos:get-next-pkt conn))
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
		    ;;(:set-byte-size (file-server-set-byte-size fh rest)) ;obsolete?
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
      (cond ((funcall standard-input ':listen)
	     (let ((c (funcall standard-input ':tyi)))
	       (or (= c #\SP) (funcall standard-input ':untyi c)))
	     (return nil))))))

)
