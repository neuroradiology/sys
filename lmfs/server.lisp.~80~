;-*- Mode:LISP; Package:LMFS -*-
;;;
;;; Copyright (c) Symbolics, Inc., 1981
;;;

(defstruct (server-dataproc-comm (:type :list) :conc-name)
  iotype control-proc data-proc conn sibling cell opening arg binp)

;;; (push '(trap-lossage 0 7 1 3 2 1) zwei:*lisp-indent-offset-alist*)
;;; (push '(destructuring-bind 0 4 2 3) zwei:*lisp-indent-offset-alist*)

;;; Toplevel and parsers

;;; This is separate to allow debugging by recompiling rfile-server.

(defun file-server (&rest ignore)
  (rfile-server))

(declare (special fs:*local-server-via-net*))
(defvar *lmfs-server-dont-answer-logins* nil)
(defvar server-protocol-version 0)
(defvar server-openings)
(defvar alldatas)				;all dataconn cells
(defvar tid)					;running transaction id
(defvar stream)					;connection stream
(defvar conn)					;connection
(defvar server-instance)			;tag for props
(defvar uname)
(defvar lmfs-server-lossages nil)
(defvar lmfs-debug-server nil)

(defun cv-time (x) (time:print-universal-time x nil))

(defmacro trap-lossage ((lossagename id) code &body errors)
  (let ((magic (gensym))
	(value (gensym)))
    `(let ((,value
	    (*catch ',magic
	      (condition-bind ((,lossagename #'(lambda (&rest msg)
						 (if lmfs-debug-server
						     nil
						     (progn
						       (tv:notify
							 nil
							 "File server got an error.")
						       (push
							 (list
							   (cv-time (time:get-universal-time))
							   ',id
							   (copylist msg))
							 lmfs-server-lossages)
						       (*throw ',magic ',magic))))))
		,code))))
       (if (eq ,value ',magic)
	   (progn . ,errors)
	   ,value))))


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

(defun server-parse-rfc (pkt &aux s fx version)
  (setq s (chaos:pkt-string pkt))
  (setq fx (string-search "FILE" s))
  (cond ((null fx) "Unparseable RFC")
	(t (setq fx (string-search-not-char #\SP s (+ fx 4)))
	   (cond ((null fx) 0)
		 ((null (setq version (parse-number s fx)))
		  "Unparseable version number in RFC")
		 ((or (= version 0) (= version 1)) version)
		 (t (format nil "Unsupported FILE protocol version: ~D" version))))))

(defun parse-cmd-string (string &aux answers)
  (let ((nlx (string-search-char #\CR string)))
    (do ((start 0) (lim (or nlx (string-length string))))
	(( start lim) (nreverse answers))
      (if (char-equal (aref string start) #\SP)
	  (progn
	    (push nil answers)
	    (incf start))
	  (let ((endx (or (string-search-char #\SP string start lim) lim)))
	    (push (or (parse-number string start endx)
		      (si:intern1 (substring string start endx) ""))
		  answers)
	    (setq start (1+ endx)))))))

(defun get-strings-from-pktstring (string &aux answer)
  (do ((start (string-search-char #\CR string)))
      ((null start) nil)
    (let ((ix (string-search-char #\CR string (1+ start))))
      (if (null ix)
	  (progn
	    (if (not (= (1+ start) (string-length string)))
		(push (substring string (1+ start)) answer))
	    (return (nreverse answer))))
      (push (substring string (1+ start) ix) answer)
      (setq start ix))))

(defun file-server-login (rest)
  (let ((uname (car rest)))
    (if (null uname)
	(format stream "~A  ERROR ILI F Invalid Login syntax" tid)
	(progn
	  (format stream "~A  ~A ~A ~%~A~%" tid "LOGIN" uname
		  (fs:make-pathname ':host si:local-host ':device ':unspecific
				    ':directory (string-append *PATH-DELIMITER* uname)
				    ':name nil ':type nil ':version nil))
	  uname))))

;;; Opening files. 

(defun file-server-open (fh rest filename &aux modes answer fmode binp)
  (let ((losep
	  (*catch 'open-opt-lost
	    (progn
	      (loop for olist on rest
		    do
		    (let ((opt (car olist)))
		      (selectq opt
			((:binary :character :default)
			 (setq fmode
			       (check-symm-modes '(:binary :default :character) opt modes)))
			((:read :write :probe)
			 (push (check-symm-modes '(:read :write :probe) opt modes) modes))
			((:temporary :raw :super-image))
			((:deleted :preserve-dates)
			 (push opt modes))
			(:byte-size
			 (push (cadr olist) modes)
			 (push ':byte-size modes)
			 (pop olist))
			(t (open-err "UOO F Unknown option: " opt)))))
	      (if (null fh)
		  (if (or (memq ':read modes) (memq ':write modes))
		      (open-err "ICO F Inconsistent open options for probe opening")
		      (or (memq ':probe modes) (push ':probe modes)))

		  ;; FHN given. must be real read or write.

		(let* ((comdata (get fh server-instance))
		       (type (server-dataproc-comm-iotype comdata)))
		    (if (null comdata)
			(open-err "UFH F No open data channel for this file handle: " fh))
		    (if (or (and (eq type 'input)
				 (memq ':write modes))
			    (and (eq type 'output)
				 (memq ':read modes)))
			(open-err "ICO F File handle type inconsistent with open mode."))

		    (if (not
			  (or (memq ':read modes) (memq ':write modes) (memq ':probe modes)))
			(push (selectq type (input ':read) (output ':write)) modes))))
		  
	      (let ((pathname (lmfs-parse-for-server filename)))
		(if (stringp pathname) (open-err "STX F Bad filename syntax: " pathname))
		(let ((opening
			(open pathname
			      ':direction
			      (cond ((memq ':read modes) ':input)
				    ((memq ':write modes) ':output)
				    (t nil))
			      ':characters
			      (selectq fmode
				(:default ':default)
				(:binary nil)
				(:character t)
				(nil t)
				(t fmode))
			      ':error nil
			      ':deleted (memq ':deleted modes)
			      ':preserve-dates (memq ':preserve-dates modes)
			      ':byte-size (cadr (memq ':byte-size modes)))))
		  (if (stringp opening) (*throw 'open-opt-lost (lmfs-error-string opening)))
		  (setq binp
			(selectq fmode
			  (:default (not (funcall opening ':characters)))
			  (:character nil)
			  (t t)))				       
		  (setq answer
			(selectq server-protocol-version
			  (0  
			   (format nil
				   "~D ~A ~D ~S~%~A~%"
				   (funcall (funcall opening ':truename) ':version)
				   (cv-time (funcall opening ':creation-date))
				   (funcall opening ':length)
				   (funcall opening ':qfaslp)
				   (funcall opening ':truename)))
			  (1
			   (format nil
				   "~A ~D ~S ~S~%~A~%"
				   (time:print-universal-time
				     (funcall opening ':creation-date) nil)
				   (funcall opening ':length)
				   binp		;qfaslp, needed for compatibility
				   (not binp)
				   (funcall opening ':truename)))))
		  (if (memq ':probe modes)
		      (funcall opening ':close)
		      (let ((servi (get fh server-instance)))
			(push opening server-openings)
			(setf (server-dataproc-comm-binp servi) binp)
			(setf (server-dataproc-comm-opening servi) opening)
			(rplaca (server-dataproc-comm-cell servi)
				(if (memq ':read modes) 'read 'write))))
		  nil))))))
    (if (null losep)
	(format stream  "~A ~A OPEN ~A" tid fh answer)
	(format stream  "~A ~A ERROR ~A" tid fh losep))))

(defun lmfs-error-string (msg)
  (if (or (string-equal msg "FHN ERROR " 0 0 10. 10.)
	  (string-equal msg "FNH ERROR " 0 0 10. 10.))
      (substring msg 10.)
      (string-append "LOS F " msg)))

(defun open-err (&rest args)
  (*throw 'open-opt-lost (apply 'string-append args)))

(defun check-symm-modes (posses x sofar)
  (dolist (y posses)
    (dolist (z sofar)
      (if (and (eq y z)
	       (not (eq y x)))
	  (open-err "ICO F Mode " x " inconsistent with " z))))
  x)

(defun lmfs-parse-for-server (string)
  (*catch 'parse-lose
    (condition-bind ((:lmfs-path-syntax #'(lambda (&rest msg)
					    (*throw 'parse-lose
						    (lexpr-funcall #'format
								   nil (cdr msg))))))
      (fs:parse-pathname string si:local-host))))


;;; Data connection stuff.


(defun null-car (x) (null (car x)))

(defun send-sync-mark (dconn)
  (chaos:send-pkt dconn (chaos:get-pkt) fs:%file-synchronous-mark-opcode))

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
		(let ((ocell (ncons nil))
		      (icell (ncons nil))
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

(defun file-server-undata-connection (fh)
  (let ((data (get fh server-instance)))
    (if (null data)
	(format stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh)
	(dolist (fh (list fh (server-dataproc-comm-sibling data)))
	  (let* ((data (get fh server-instance))
		 (cell (server-dataproc-comm-cell data)))
	    (await-data-process cell 'undata)
	    (chaos:remove-conn (server-dataproc-comm-conn data)))
	  (remprop fh server-instance))))
  (format stream "~A ~A UNDATA-CONNECTION" tid fh))

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
	    (if (not (eq (car cell) 'read)) (return nil))
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
		     (chaos:send-pkt dconn (chaos:get-pkt) chaos:eof-op)
		     (rplaca cell nil)
		     (return nil))))))
	 (t (ferror nil "Bogus com-cell value: ~S" (car cell))))))
    (trap-lossage (nil "Data Conn error closeout")
       (let ((sib (server-dataproc-comm-sibling (get handle server-instance))))
	 (and sib (funcall (server-dataproc-comm-data-proc sib) ':kill))))))

(defun await-data-process (cell flag)
  (rplaca cell flag)
  (process-wait "ServerSync" #'null-car cell))

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
		 (cond ((not (eq opening 'directory))
			(funcall opening ':close)
			(setq server-openings (delq opening server-openings))))))))))

;;; Random commands.

(defun file-server-filepos (fh rest)
  (let ((data (get fh server-instance)))
    (if (null data)
	(format stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh)
	(let ((direction (server-dataproc-comm-iotype data))
	      (opening (server-dataproc-comm-opening data))
	      (cell (server-dataproc-comm-cell data)))
	  (funcall opening ':set-pointer (car rest))
	  (format stream "~A ~A FILEPOS" tid fh)
	  (await-data-process cell 'fpsync)
	  (rplaca cell (if (eq direction 'input) 'read 'write))))))

(defun file-server-delete (fh strings)
  (cond ((null fh)				;must be string, delete random file
	 (if (not (= (length strings) 1))
	     (format stream "~A  ERROR IRF F Inconsistent command options")
	     (let ((path (lmfs-parse-for-server (first strings))))
	       (if (stringp path)
		   (format stream "~A  ERROR STX F Syntax error in pathname" tid)
		   (let ((result (funcall path ':delete nil)))
		     (if (stringp result)
			 (format stream "~A  ERROR ~A" tid (lmfs-error-string result))
			 (format stream "~A  DELETE" tid)))))))
	(t					;delete while open
	 (if strings
	     (format stream "~A ~A ERROR IRF F Inconsistent delete command options"
		     tid fh)
	     (let ((data (get fh server-instance)))
	       (if (or (null data) (null (server-dataproc-comm-opening data)))
		   (format stream "~A ~A ERROR UFH F No opening for handle ~A"
			   tid fh fh)
		   (progn
		     (funcall (server-dataproc-comm-opening data) ':delete)
		     (format stream "~A ~A DELETE" tid fh))))))))


(defun file-server-directory (fh rest strings &aux data)
  (cond ((or (null fh) (not (= (length strings) 1)))
	 (format stream "~A ~A ERROR IRF F Inconsistent DIRECTORY command" tid (or fh "")))
	((or (null (setq data (get fh server-instance)))
	     (not (eq (server-dataproc-comm-iotype data) 'input)))
	 (format stream "~A ~A ERROR UFH F Bad file handle for DIRECTORY command" tid fh fh))
;	((car (server-dataproc-comm-cell data)) ;; your problem
;	 (format stream "~A ~A ERROR BSY F File handle ~A busy" tid fh fh))
	(t
	 (setf (server-dataproc-comm-arg data) (cons (car strings) rest))
	 (setf (server-dataproc-comm-opening data) 'directory)	;make close work
	 (setf (car (server-dataproc-comm-cell data)) 'directory)
	 (format stream "~A ~A DIRECTORY" tid fh))))


(defun server-dataproc-hack-directory (data handle &aux ok)
  (trap-lossage (nil "Directory lister toplevel")
     (let* ((conn (server-dataproc-comm-conn data))
	    (stream (chaos:stream conn))
	    (arg (server-dataproc-comm-arg data))
	    (string (car arg))
	    (opts (cdr arg))
	    (path (lmfs-parse-for-server string)))
       (if (stringp path)
	   (send-data-async-lossage
	     conn (format nil "STX F Error parsing path: ~A" string) handle)
	   (let ((dirlist (funcall path ':directory-list
				   (cons ':noerror opts))))
	     (if (stringp dirlist)
		 (send-data-async-lossage conn dirlist handle)
		 (progn
		   (server-dirlist-single (cdar dirlist) nil stream)
		   (dolist (file (cdr dirlist))
		     (server-dirlist-single (cdr file) (car file) stream))
		   (funcall stream ':tyo #\CR)
		   (setq ok t)))))
       (funcall stream ':force-output)
       (if ok (chaos:send-pkt conn (chaos:get-pkt) chaos:eof-op)))
    (send-data-async-lossage conn "LOS F System error during dir list processing" handle)))

(defun send-data-async-lossage (conn msg handle)
  (let ((pkt (chaos:get-pkt)))
    (chaos:set-pkt-string pkt " " handle " ERROR LOS F " msg)
    (chaos:send-pkt conn pkt fs:%file-asynchronous-mark-opcode)))

(defun server-dirlist-single (props pn stream)
  (format stream "~%")
  (if pn (format stream "~A~%" pn))
  (tv:doplist (props prop ind)
    (format stream "~A " ind)
    (if (eq ind ':settable-properties)
	(loop for x on prop do (princ (car x) stream) (if (cdr x) (tyo #\SP stream)))
	(or (dolist (spec fs:*known-directory-properties*)
	      (if (memq ind (cdr spec))
		  (progn
		    (funcall (or (cadar spec) #'princ) prop stream)
		    (return t))))
	    (princ prop stream)))
    (format stream "~%")))

  
(defun file-server-change-props (fh strings &aux (prfh (or fh "")))
  (trap-lossage (nil "Change properties toplevel")
     (cond ((not (> (length strings) 1))
	    (format stream "~A ~A ERROR IRF F No properties//pathname given." tid prfh))
	   (t
	    (let ((path (lmfs-parse-for-server (car strings))))
	      (if (stringp path)
		  (format stream "~A ~A ERROR STX F Syntax error in supplied path: ~A"
			  tid prfh)
		  (let ((plist (ncons nil)))
		    (dolist (spec (cdr strings))
		      (let ((spacex (string-search-char #\SP spec)))
			(if (null spacex)
			    (ferror nil "Ill formatted property spec: ~A" spec))
			(let ((sym (si:intern1 (substring spec 0 spacex) "")))
			  (putprop plist
				   (server-convert-known-file-property spec (1+ spacex) sym)
				   sym))))
		    (trap-lossage (nil "Change properties")
                             ;; i like "m"
			(let ((m (lexpr-funcall path ':change-properties nil (cdr plist))))
			  (if (eq m 't)
			      (format stream "~A ~A CHANGE-PROPERTIES" tid prfh)
			      (format stream "~A ~A ERROR LOS F ~A" tid prfh m)))
		      (format stream "~A ~A ERROR SYS F Internal error" tid prfh)))))))
    (format stream "~A ~A ERROR SYS F Internal error" tid prfh)))

(defun server-convert-known-file-property (string index ind)	;not really general
  (or (dolist (spec fs:*known-directory-properties*)
	(if (memq ind (cdr spec))
	    (return (funcall (caar spec) string index))))
      (substring string index nil)))	;default is substr

(defun file-server-rename (fh strings)
  (cond ((null fh)				;must be string, delete random file
	 (if (not (= (length strings) 2))
	     (format stream "~A  ERROR IRF F Inconsistent RENAME command options" tid)
	     (let ((path1 (lmfs-parse-for-server (first strings)))
		   (path2 (lmfs-parse-for-server (second strings))))
	       (if (stringp path1)
		   (format stream "~A  ERROR STX F Syntax error in pathname" tid)
		   (if (stringp path2)
		       (format stream "~A  ERROR STX F Syntax error in rename pathname" tid)
		       (trap-lossage (nil "Rename 2 args")
			 (progn
			   (if (null (funcall path1 ':version))
			       (setq path1 (funcall path1 ':new-version ':newest)))
			   (if (null (funcall path2 ':version))
			       (setq path2 (funcall path2 ':new-version ':newest)))
			   (let ((result (funcall path1 ':rename path2 nil)))
			     (if (stringp result)
				 (format stream "~A  ERROR ~A" tid (lmfs-error-string result))
			       (format stream "~A  RENAME" tid))))
			 (format stream "~A  ERROR SYS F System error renaming" tid)))))))
	(t					;rename while open
	 (if (not (= (length strings) 1))
	     (format stream "~A ~A ERROR IRF F Inconsistent rename command options"
		     tid fh)
	     (let ((path (lmfs-parse-for-server (first strings))))
	       (if (stringp path)
		   (format stream "~A ~A ERROR STX F Syntax error in pathname" tid fh)
		   (let* ((data (get fh server-instance))
			  (opening (server-dataproc-comm-opening data)))
		     (if (or (null data)
			     (null opening)
			     (symbolp opening))	;yes, i know thx
			 (format stream "~A ~A ERROR UFH F No opening for handle ~A"
				 tid fh fh)
			 (trap-lossage (nil "Rename while open")
			   (progn
			     (if (null (funcall path ':version))
				 (setq path (funcall path ':new-version ':newest)))
			     (let ((result
				     (funcall opening ':rename path nil)))
			       (if (stringp result)
				   (format stream "~A ~A ERROR ~A" tid fh
					   (lmfs-error-string result))
				 (format stream "~A ~A RENAME" tid fh))))
			   (format stream "~A ~A ERROR SYS F System error while renaming"
				   tid fh))))))))))


(defun file-server-expunge (fh strings &aux path result)
  (cond (fh
	 (format stream "~A ~A ERROR IRF File handle given in EXPUNGE command." tid fh))
	((null strings)
	 (format stream "~A  ERROR IRF F No pathname given to EXPUNGE command." tid))
	((cdr strings)
	 (format stream "~A  ERROR IRF F Extra junk given to EXPUNGE command." tid))
	((stringp (setq path (lmfs-parse-for-server (first strings))))
	 (format stream "~A  ERROR STX F Syntax error in pathname" tid))
	((stringp (setq result (funcall path ':expunge)))
	 (format stream "~A  ERROR LOS F Expunge error: ~A" tid result))
	(t (format stream "~A  EXPUNGE ~D" tid result))))

(defun file-server-complete (fh args strings &aux path result success)
  (cond (fh
	 (format stream "~A ~A ERROR IRF File handle given to COMPLETE command." tid fh))
	((not (= (length strings) 2))
	 (format stream "~A  ERROR IRF Wrong number of strings given in COMPLETE command."
		 tid))
;	((stringp (setq path (lmfs-parse-for-server (first strings))))
;	 (format stream "~A  ERROR STX F Syntax error in pathname." tid))
	(t (setq path (lmfs-parse-for-server (first strings)))
	   ;;string result means an error
	   (if (stringp path)			;ZMACS will supply semibogus paths....!!
	       (setq path (fs:user-homedir si:local-host)))
	   (multiple-value (result success)
	     (funcall path ':complete-string (second strings)
		      (list*
			(if (memq ':write args) ':write ':read)
			(if (memq ':new-ok args) ':new-ok ':old)
			(if (memq ':deleted args) '(:deleted)))))
	   (let ((x (and result (string-search-char #/: result))))	;strip out host
	     (if x (setq result (substring result (1+ x)))))
	   (format stream "~A  COMPLETE ~A~%~A~%" tid success result))))

(defun print-server-login-exegesis ()
  (tv:notify
    nil
    "This machine has been invoked as a remote file server, but is otherwise free."))

;;; This oughtta go at THE BOTTOM.....

(add-initialization "FILE" '(PROCESS-RUN-FUNCTION "File Server" 'file-server)
		    nil 'chaos:server-alist)
