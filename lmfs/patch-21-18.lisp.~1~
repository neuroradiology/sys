;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.18
;;; Reason: Better server unwind protect; server PEEK features.
;;; Written 12/23/81 11:52:43 by BSG,
;;; while running on Spaniel from band 1
;;; with System 78.27, ZMail 38.4, Symbolics 8.7, Tape 6.2, LMFS 21.17, Canon 9.3, microcode 841.



; From file server.LISP >LMFS POINTER:
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
		     (process-wait "ReadFinish" #'null-car cell))
		 (setf (server-dataproc-comm-opening data) nil)
		 (cond ((not (eq opening 'directory))
			(funcall opening ':close)
			(setq server-openings (delq opening server-openings))))))))))

)

; From file server.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun file-server-data-connection (fh rest)
  (let ((ifh (car rest))
	(ofh (cadr rest)))
    (if (not (and ifh ofh (symbolp ifh) (symbolp ofh)))
	(format stream "~A ~A ERROR IRF F Ill-formed data-connection request")
	(let ((dconn (chaos:connect (chaos:foreign-address conn) (string ofh))))
	  (if (stringp dconn)
	      (format stream "~A ~A ERROR NWL F Reverse data connection (~A) lost:~%~A"
		      tid (or fh "") ofh dconn)
	      (progn
		(putprop ifh
			 (make-server-dataproc-comm iotype 'input conn dconn)
			 server-instance)
		(putprop ofh
			 (make-server-dataproc-comm iotype 'output conn dconn)
			 server-instance)
		(let ((ocell (cons nil ofh))
		      (icell (cons nil ifh))
		      (idata (get ifh server-instance))
		      (odata (get ofh server-instance)))
		  (setf (server-dataproc-comm-data-proc idata)
			(process-run-function
			  (string-append "File Server Data " ifh)
			  'file-server-data-top-level 
			  server-instance icell ifh))
		  (setf (server-dataproc-comm-data-proc odata)
			(process-run-function
			  (string-append "File Server Data " ofh)
			  'file-server-data-top-level 
			  server-instance ocell ofh))
		  (setf (server-dataproc-comm-sibling idata) ofh)
		  (setf (server-dataproc-comm-sibling odata) ifh)
		  (push ocell alldatas)		;allow cleanup
		  (push icell alldatas)
		  (setf (server-dataproc-comm-cell idata) icell)
		  (setf (server-dataproc-comm-cell odata) ocell)
		  (format stream "~A ~A DATA-CONNECTION" tid fh))))))))

)

; From file server.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun file-server-login (rest)
  (let ((uname (car rest)))
    (if (null uname)
	(format stream "~A  ERROR ILI F Invalid Login syntax" tid)
	(progn
	  (format stream "~A  ~A ~A ~%~A~%" tid "LOGIN" uname
		  (fs:make-pathname ':host si:local-host ':device ':unspecific
				    ':directory (string-append *PATH-DELIMITER* uname)
				    ':name nil ':type nil ':version nil))
	  (string uname)))))

)

; From file server.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun file-server-undata-connection (fh)
  (let ((data (get fh server-instance)))
    (if (null data)
	(format stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh)
	(dolist (fh (list fh (server-dataproc-comm-sibling data)))
	  (let* ((data (get fh server-instance))
		 (cell (server-dataproc-comm-cell data)))
	    (await-data-process cell 'undata)
	    (chaos:remove-conn (server-dataproc-comm-conn data))
	    (setq alldatas (delq cell alldatas)))
	  (remprop fh server-instance))))
  (format stream "~A ~A UNDATA-CONNECTION" tid fh))

)

; From file server.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun lmfs-peek-data-process (cell itag)
  (let* ((handle (cdr cell))
	 (data (get handle itag))
	 (process (server-dataproc-comm-data-proc data)))
    (list (list ':pre-process-function 'lmfs-peek-server-preprocess
		'lmfs-cdata data 'cur-display (ncons nil))
	  (tv:scroll-parse-item
	    "      "
	    `(:mouse
	       (nil :eval (tv:peek-process-menu ',process)
		    :documentation
		    "Menu of useful things to do to this process.")
	       :string
	       ,(format nil "~A" (process-name process)))
	    "    "
	    `(:function ,#'tv:peek-whostate ,(ncons process))
	    ", sibling "
	    (string (server-dataproc-comm-sibling data))
	    (format nil ", ~A" (server-dataproc-comm-iotype data))
	    ", cmd "
	    `(:function ,#'car (,cell)))
	  nil)))

)

; From file server.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun lmfs-peek-server (sg)
  (let ((itag (symeval-in-stack-group 'server-instance sg)))
    (list '()
	  (tv:scroll-parse-item
	    "    User: "
	    `(:function ,#'symeval-in-stack-group (user-id ,sg) 15.)
	    "    Server Tag: "
	    (string itag))
	  (tv:scroll-maintain-list
	    `(lambda () (symeval-in-stack-group 'alldatas ',sg))
	    `(lambda (x) (lmfs-peek-data-process x ',itag))))))

)

; From file server.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun lmfs-peek-server-preprocess (list-item)
  (let* ((plist (locf (first list-item)))
	 (data (get plist 'lmfs-cdata))
	 (cell (get plist 'cur-display))
	 (curdisp (car cell))
	 (opening (server-dataproc-comm-opening data)))
    (cond ((eq curdisp opening))
	  ((null opening) (setf (third list-item) nil))
	  ((eq opening 'directory)
	   (rplaca cell opening)
	   (setf (third list-item) (tv:scroll-parse-item "         Directory state.")))
	  ((setf (third list-item) (funcall opening ':peek-file-system 9))
	   (rplaca cell opening)))))

)

; From file server.LISP >LMFS POINTER:
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
    (unwind-protect
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
	    (funcall tv:who-line-file-state-sheet
		     ':add-server conn "FILE" si:current-process
		     'lmfs-peek-server (process-stack-group si:current-process))
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
      (trap-lossage (nil "Closeout undata") (dolist (cell alldatas) (rplaca cell 'undata))))))

)
