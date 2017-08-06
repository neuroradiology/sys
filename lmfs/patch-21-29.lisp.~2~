;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.29
;;; Reason: UNDATA bugs, sync errors in server, better :DELETE-to-opening handling.
;;; Written 1/06/82 12:27:38 by BSG,
;;; while running on Basset from band 2
;;; with System 78.41, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.26, Canon 9.11, microcode 841.



; From file server.lisp >lmfs POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun server-window-write-check (cell dconn val)
  (do () (())
    (if (neq (car cell) val) (return nil))
    (if (chaos:read-pkts dconn) (return t))
    (process-wait "SyncNETI" #'(lambda (cell dconn val)
				 (or (neq (car cell) val)
				     (chaos:read-pkts dconn)))
		  cell dconn val)))

)

; From file server.lisp >lmfs POINTER:
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
	    (let* ((pkt (if (server-window-write-check cell dconn 'write)
			    (chaos:get-next-pkt dconn)
			  (return nil))))
	      (select (chaos:pkt-opcode pkt)
		(chaos:eof-op
		    (chaos:return-pkt pkt)
		    (setq pkt (if (server-window-write-check cell dconn 'write)
				  (chaos:get-next-pkt dconn)
				(return nil)))
		    (or (= (chaos:pkt-opcode pkt) fs:%file-synchronous-mark-opcode)
			(ferror "Unrecognized Opcode in data server: ~O"
				(chaos:pkt-opcode pkt)))
		    (chaos:return-pkt pkt)
		    (%store-conditional (locf (car cell)) 'write nil)
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
		     (%store-conditional (locf (car cell)) 'read nil)
		     (return nil))))))
	 (t (ferror nil "Bogus com-cell value: ~S" (car cell))))))))

)

; From file server.lisp >lmfs POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun lmfs-peek-data-process (cell itag)
  (let* ((handle (cdr cell))
	 (data (get handle itag)))
    (cond ((null data)
	   (tv:scroll-parse-item
			(format nil "      Vanished process ~A, instance ~A." handle itag)))
	  (t (let ((process (server-dataproc-comm-data-proc data)))
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
		     nil))))))

)

; From file server.lisp >lmfs POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun file-server-undata-connection (fh)
  (let ((data (get fh server-instance)))
    (cond ((null data)
	   (format stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh))
	  (t (dolist (fh (list fh (server-dataproc-comm-sibling data)))
	       (let* ((data (get fh server-instance))
		      (cell (server-dataproc-comm-cell data)))
		 (await-data-process cell 'undata)
		 (setq alldatas (delq cell alldatas)))
	       (remprop fh server-instance))
	     (chaos:remove-conn (server-dataproc-comm-conn data))
	     (format stream "~A ~A UNDATA-CONNECTION" tid fh)))))

)

; From file fsstr.lisp >lmfs POINTER:.lisp >lmfs POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (lmfs-opening-mixin :after :close) (&optional abort-p &aux was-mode)
  (setq was-mode mode
	abort-p (and (or (memq ':delete options)
			 (eq abort-p ':abort))
		     (memq mode '(:write :append))))
						;only time we give a damn
  (cond ((memq mode '(:write :append))
	 (send-self ':force-output)		;clear out stream stuff in any case
	 (if (not abort-p) (setf (fd-byte-length file) bytes-in-file))))

  (with-fs-locked
    (if cur-data-addressor (deallocate-resource 'fs-data-addressor cur-data-addressor))
    (if cur-addressor (progn (downreference-addressor cur-addressor)
			     (setq cur-addressor nil)))
    (if cur-buffer (progn (downreference-file-buffer cur-buffer)
			  (setq cur-buffer nil)))

    (setf (fd-openings file) (delq self (fd-openings file)))
    (if (and (memq mode '(:write :append)) (not abort-p))
	(update-file-attributes file ':byte-length ':number-of-records
				(if (not (memq ':no-update-reference-time options))
				    ':date-time-used)
				':date-time-modified)

	(update-file-attributes file
				(if (and (eq mode ':read)
					 (not (memq ':no-update-reference-time options)))
				    ':date-time-used)))
    (setq mode ':closed)
    (flush-buffers-for-file file)
    (let ((hbuf (fd-header-buffer file)))
      (if (and hbuf (neq was-mode ':probe))
	  (let ((hbrc (fb-reference-count hbuf)))
	    (if (= hbrc 1) (setf (fd-header-buffer file) nil))
	    (downreference-file-buffer hbuf))))
    (if (and abort-p (eq was-mode ':write))	;yes, I know this is redundant, BUT
						;better safe then sorry when the next
						;guy modifies this method.
	(progn
	  (update-file-attributes file ':delete)	;won't exp if not deleted, so delete
	  (if (fixp (expunge-directory-entry
		      (fd-parent file) (fd-entry-index file)))	;destroy the thing.
	      (setq file nil))))			;dont try to deactivate

    ;;Handle rename at close time options - note we do this only if there was a problem
    ;;at open time, i.e., an actual name duplication.
    ;;It is not considered bad that we have already pretty much finished and flushed the file.
    (cond ((and file
		(not abort-p)
		(neq was-mode ':closed)
		(memq ':close-rename-pending options))
	   (let ((dirpath (funcall real-pathname ':directory))
		 ;; ()deserves better primitive in PATHS for this...
		 (name (funcall real-pathname ':name))
		 (type (funcall real-pathname ':type))
		 (version (funcall real-pathname ':version)))
	     (multiple-value-bind (bad-guy-fd err)
		 (get-fd-from-dn-en dirpath name type version ':deleted)
	       (cond ((null err)		;Yup, guy still exists.
		      (multiple-value-bind (ignore brerr)
			  (rename-file-dfd bad-guy-fd (fd-parent file)
					   (format nil "~A~A~D" name type version)
					   "renamed" ':newest)
			(selectq brerr
			  (:create-through-link-magic)
			  (nil  (if (memq ':namedup-rename-dup-delete options)
				    (update-file-attributes bad-guy-fd ':delete))
				(check-deactivate-file bad-guy-fd))
			  (t (ferror nil "Can't rename old ~A at close time - ~A"
				     real-pathname brerr))))))
	       ;;OK, bad guy either disappeared of his own, or we renamed him.
	       (multiple-value-bind (ignore rerr)
		   (rename-file-dfd file (fd-parent file) name type version)
		 (selectq rerr
		   (nil)			;FABULOUS
		   (:create-through-link-magic)	;go to hell
		   (t (ferror nil "Failed to rename ~S to ~A at close time - ~A"
			      file real-pathname rerr))))))))
    (if (and (not abort-p) (eq was-mode ':write) (fd-grc-info file))
	(delete-old-generations file))		;lost for some reason, just delete
    (and file (or (eq was-mode ':closed) (check-deactivate-file file))))
  t)

)

; From file fsstr.lisp >lmfs POINTER:.lisp >lmfs POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(compile-flavor-methods
  lmfs-binary-input-opening
  lmfs-binary-output-opening
  lmfs-char-input-opening
  lmfs-char-output-opening
  lmfs-full-probe-opening
  lmfs-dumper-opening)

)
