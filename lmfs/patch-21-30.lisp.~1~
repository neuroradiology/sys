;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.30
;;; Reason: Server trace facility; Byte size  8 on chars opening now "usage error".
;;; Written 1/07/82 10:50:18 by BSG,
;;; while running on Retriever from band 5
;;; with System 78.43, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.28, Canon 9.11, microcode 841.



; From file server.lisp >lmfs POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))


(defvar trace-server-enabled nil)
(defvar server-traces nil)

)

; From file server.lisp >lmfs POINTER:
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
		
		(if trace-server-enabled
		    (without-interrupts (push (string-append string) server-traces)))
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

; From file fsstr.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun open-local-lmfs (pathname logpath options
			&aux (mode ':read) (type nil) (noerror-p nil)
			(deleted-p nil) 
			opening (byte-size nil) (fflavor nil) (link-to)
			(newfile-opt 'didnt-say) (oldfile-opt 'didnt-say)
			(random-options nil))

  (*catch 'open-local-lmfs
    (*catch 'retry-open-local-lmfs
      (tv:doplist (options prop ind)
	(selectq ind
	  ((:ignore nil))
	  (:direction  (setq mode (selectq prop
				    (nil      (if (null type) (setq type ':fixnum))
					      ':probe)
				    (:in      ':read)
				    (:out     ':write)
				    (:input   ':read)
				    (:output  ':write)
				    ((:append :dumper) prop)
				    (t (ferror nil "Unknown direction - ~S" prop)))))
	  (:inhibit-links     (if prop (push ':dont-chase-links random-options)))
	  (:characters        (setq type
				    (selectq prop
				      ((t)  ':ascii)
				      (nil  ':fixnum)
				      (t    ':default))))
	  ((:single :block :raw :super-image :temporary))
	  (:byte-size         (or (eq prop ':default) (setq byte-size prop)))
	  (:deleted           (setq deleted-p prop))
	  (:error             (setq noerror-p (not prop)))
	  (:inhibit-links     (if prop (push ':dont-chase-links random-options)))
	  (:incremental-update  (if prop (push ':incremental-update random-options)))
	  (:preserve-dates    (if prop (push ':no-update-reference-time random-options)))
	  (:flavor            (setq fflavor prop))
	  (:link-to           (setq link-to prop))
	  (:old-file	      (setq oldfile-opt prop))
	  (:new-file          (setq newfile-opt prop))
	  ((:new-version :estimated-size))
	  (otherwise (ferror nil "FHN ERROR UOO F ~S is not a known OPEN option" ind))))
      
      ;; Handle other than files.
      (selectq fflavor
	(nil         )				;normal trip
	(:directory
	        (setq link-to (funcall pathname ':pathname-as-directory))
	        (*throw 'open-local-lmfs
			(funcall link-to ':create-directory (not noerror-p))))
	(:link  (*throw 'open-local-lmfs
			(funcall pathname ':create-link link-to)))
	(t      (ferror nil "Unknown file flavor - ~S" fflavor)))

      ;;Handle binariness.
      (selectq type
	(:fixnum (push ':binary random-options))
	((nil :ascii :default)
                 (or (eq type ':default) (setq type ':ascii))
		 (if (not (or (memq byte-size '(nil :default))
			      (equal byte-size *DEFAULT-CHAR-FILE-BYTE-SIZE*)))
		     (*throw 'open-local-lmfs
			     (format nil
				     "FHN ERROR IBS F Byte size ~D not supported for character opening."
				     byte-size)))))

      ;;Default the oldfile/newfile stuff
      (if (eq newfile-opt 'didnt-say)
	  (setq newfile-opt (not (null (memq mode '(:write :append))))))
      (if (eq oldfile-opt 'didnt-say)
	  (setq oldfile-opt (not newfile-opt)))
      (if (and (eq oldfile-opt ':append) (memq mode '(:append :write)))
	  (setq oldfile-opt nil mode ':append))	;Newest `kosher' way of saying `append'

      ;; Do mode-specific processing.
      (selectq mode
	((:read :write :append)
	             (if (memq ':dont-chase-links random-options)
			 ;;Dumper ok too, but don't advertise it.
			 (ferror nil ":inhibit-links only valid for probe opening."))))
      (selectq mode
	((:read :dumper)
	             (if newfile-opt (ferror nil ":NEW-FILE ~S meaningless for input opening."
					     newfile-opt))
		     (or (eq oldfile-opt 't)
			 (ferror nil ":OLD-FILE ~S meaningless for input opening.")))
	((:write :append)
	             (selectq oldfile-opt
		       ;; :append should have been removed by now.
		       (:error         )	;cool, this is the default
		       ((t :rewrite)   )        ;We really don't support reusing the old file
						;except in append. Let it FAX error later.
		       (:new-version         (push ':namedup-map-newest random-options))
		       (:rename              (push ':namedup-rename-dup random-options))
		       (:rename-and-delete   (push ':namedup-rename-dup-delete
						   random-options))
		       ((nil :replace)  (push ':namedup-rename-dup-delete random-options))
						;; This good way to implement replace, no?
		       (t (ferror nil "Unrecognized value for :OLD-FILE - ~S" oldfile-opt))))

        (:probe
	            ;; See if can get away with probe-pseudoopening, much faster if so.
	             (if (setq opening (fast-probe pathname logpath deleted-p))
			 (*throw 'open-local-lmfs opening))))
      
      (setq opening				;Obtain an opening or error
	    (with-fs-locked
	      (lmfs-open-file
		pathname logpath mode deleted-p type byte-size random-options)))

      ;; Did we get an opening, or an error?
      (if (stringp opening)
	  (if noerror-p				;'Twas an error, take approp. action
	      (*throw 'open-local-lmfs opening)
	      (*throw 'retry-open-local-lmfs
		      (setq pathname (fs:file-process-error
				       opening(format nil "(mode = ~A)" mode) t nil
				       pathname (send pathname ':type)))))
	  (*throw 'open-local-lmfs opening)))	;We won. Return the opening.
    (lexpr-funcall #'open pathname options)))

)
