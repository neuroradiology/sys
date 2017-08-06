;-*- Mode:LISP; Package:Distribution; Base:8. -*-
;;; Scan mode to find all errors?

(defvar *loaded-distribution-list* nil)
(declare (special *distribution-standard-systems*))
		  
(defconst *distribution-tape-version* 2)
(defvar *debug-distribution-tape* nil)
(defvar *log-file*)

(defmacro wdt-error-restart (tag &body body)
  (let ((ctag (gensym)))
    `(do ((wdt-error-restart-success nil))
	 (wdt-error-restart-success nil)
       (*catch ',ctag
	 (condition-bind ((,tag #'(lambda (&rest ignore) (*throw ',ctag ',ctag))))
	   (progn .,body)
	   (setq wdt-error-restart-success t))))))

(defun distribution-dump-map-path (reel time)
  (let ((map-path (fs:parse-pathname "sys:distribution;foo map")))
    (funcall map-path ':new-name (multiple-value-bind (nil nil nil day mon yr)
				     (time:decode-universal-time time)
				   (format nil "~A-~D//~D//~D-distribution"
					   (string-trim '(#\SP) reel)
					   mon day yr)))))

(defun write-distribution-tape (&aux (sequence 1) (time (time:get-universal-time)))
  (or *loaded-distribution-list* (load "sys:distribution;distribution-list"))
  (multiple-value-bind (systems bands comment dstream reel ucode fonts)
      (get-distribution-options)
    (if dstream
	(with-open-stream (stream dstream)
	  (format stream "DISTRIBUTION-TAPE~%VERSION ~D~%" *distribution-tape-version*)
	  (format stream "DUMP-TIME ~A ~A~%"
		  (time:print-universal-time time nil)
		  (time:timezone-string))
	  (format stream "REEL ~A~%" reel)
	  (format stream "SEQUENCE ~D~%" sequence)
	  (format stream "TAPE-SYSTEM-VERSION ~D~%" (si:get-system-version 'tape))
	  (format stream "USER-ID ~A~%" user-id)
	  (format stream "SITE ~A~%" (si:get-site-option ':site-pretty-name))
	  (format stream "MACHINE ~A~%" (funcall si:local-host ':name))
	  (if fonts (format stream "TV-FONTS-DUMPED T~%"))
	  (if ucode (format stream "MICROCODE-VERSION-DUMPED ~D~%" ucode))
	  (format stream "COMMENT ~A~%" comment)
	  (format stream "END~%")
	  (funcall stream ':eof)
	  (if bands (write-bands-tape-file stream bands))
	  (if systems (write-systems-tape-file stream systems))
	  (format stream "END-OF-TAPE-PROLOGUE-FILES~%END~%")
	  (funcall stream ':eof)
	  (with-open-file (*log-file* (distribution-dump-map-path reel time)
				      ':direction ':output)
	    (setq si:.file-aborted-flag. nil)	;DONT delete it.
	    (format t "~&Log file going to ~A." (funcall *log-file* ':truename))
	    ;;the above may not work on ITS, I guess.
	    (format *log-file*
		    "Distribution dump by ~A on ~A (~A) at ~A.~%"
		    user-id (funcall si:local-host ':name)
		    (si:get-site-option ':site-pretty-name)
		    (time:print-universal-time time nil))
	    (format *log-file* "Distribution tape version ~D.~%" *distribution-tape-version*)
	    (format *log-file* "The given tape reel name is ~A.~%" reel)
	    (if (and comment
		     (not (zerop (string-length comment))))
		(format *log-file* "Comment: ~A~%" comment))
	    (format *log-file* "~%~%~%")
	    (if fonts (write-distribution-fonts stream))
	    (if ucode (write-distribution-ucode stream ucode))
	    (if bands
		(format
		  *log-file*
		  "~3%-------------------------- B A N D S --------------------------~3%"))
	    (dolist (band bands)
	      (write-distribution-band band stream))
	    (if systems
		(format
		  *log-file*
		  "~3%------------------------ S Y S T E M S ------------------------~3%"))
				
	    (dolist (system systems)
	      (write-distribution-system system stream))
	    (format *log-file* "~2%End of dump.")
	    (dotimes (i 2) (funcall stream ':eof)))))))	;cause simtape to indicate end.

(defun write-distribution-system (system stream &aux tem)
  (let ((name (si:system-name system)))
    (format *log-file* "~2%SYSTEM: ~A~%" name)
    (format t "~&Dumping ~A." name)
    (loop for file in (si:system-source-files system ':all)	;all types
	  do
	  (wdt-error-restart (:end-of-tape)
	    (cond ((equal (funcall file ':type) "QFASL")
		   ;;Assume this is a font or some other ludicrosity and punt.
		   ;;Some other day ...
		   )
		  (t
		   (write-distribution-source-file stream system file)
		   (cond (;if no qfasl
			  (and (setq tem (funcall file ':generic-pathname))
			       (null (funcall tem ':get ':qfasl-source-file-unique-id)))
			  (let* ((object-path  (funcall file ':new-type "QFASL"))
				 (probe-result (probef object-path)))
			    (if probe-result 
				(wdt-error-restart (:end-of-tape)
				  (distribution-file-writer
				    stream "OBJECT-FILE"
				    probe-result
				    (funcall object-path ':new-version
					     (funcall probe-result ':version))
				    nil system nil))))))))))
    (distribution-dump-patches system stream)))
    
;;;There are two cases here, programs that are loaded and programs that aren't loaded.
;;;If the program isn't loaded, we use the latest source.  If the program is loaded,
;;;we are quite picky about what source version is dumped.

(defun write-distribution-source-file (stream system path &aux case fetch-path report-path
				       extra version latest-version newprobe)
  (let* ((generic-path (funcall path ':generic-pathname))
	 (trans-path (funcall path ':translated-pathname))
	 (qf-source-path (funcall generic-path ':get ':qfasl-source-file-unique-id)))
    (do () (())
      top      
      ;;The actual file whence this thing came claims to exist.
      (setq extra nil)
      (setq case
	    (cond ((null qf-source-path)	;non-loaded file
		   (cond ((setq fetch-path (probef (funcall path ':new-version ':newest)))
			  (setq extra (format nil "UNSPECIFIC-VERSION T~%"))
			  (setq report-path (funcall path ':new-version
						     (funcall fetch-path ':version)))
			  (go dump))
			 (t ':unspecific-not-found)))
		  ;;We are here because we know that this is loaded file.  Does host exist?
		  (t
		   (setq version (funcall qf-source-path ':version))
		   (setq report-path (funcall path ':new-version version))
		   (cond ((and (fs:get-pathname-host (funcall qf-source-path ':host))
			       ;;The host of the real place it comes from exists.
			       (cond ((setq fetch-path (probef qf-source-path))
				      ;;Not only that, but the file exists.
				      (go dump))
				     (t nil))))
			  ;;Host or file doesn't exist.
			  ;;Try just plain-old SYS host, same version?
			 ((setq fetch-path
				(probef (setq report-path
					      (funcall path ':new-version version))))
			  (go dump))
			 ;;Too bad. Can't find comparable thing.  See if any exists.
			 ((setq fetch-path
				(probef (funcall path ':new-version ':newest)))
			  ':specific-not-found-but-there-are-some)
			 (t ':specific-not-found)))))
      query
	    
      (selectq 
	(fquery `(:choices
		   (((:skip "Nothing; skip dumping this file.") #/N #/n)
		    ((:try-again "Try again.") #/T #/t)
		    ((:dirlist "Directory list.") #/D #/d)
		    ,@(if (eq case ':specific-not-found-but-there-are-some)
			  (list '((:latest "Latest to be used.") #/L #/l)))
		    ((:else "Dump something else.") #/E #/e))
		   :help-function
		   (lambda (&rest ignore) (wdsf-help ',path ',report-path ',fetch-path
						     ',case)))
		(format
		  nil "~A~%What shall I do? "
		  (selectq case
		    (:specific-not-found-but-there-are-some
		     (format
		       nil
		       "Cannot find a copy of ~A with the correct source version."
		       report-path))		       
		    (t
		     (format nil "Cannot find any source for ~A." path)))))
	(:skip (return nil))
	(:try-again (go top))
	(:dirlist  (distribution-display-directory path)
		   (go query))
	(:latest (setq latest-version (funcall fetch-path ':version))
		 (setq extra (format
			       nil "UNSPECIFIC-VERSION T~%NEEDED-VERSION ~D~%" version))
		 (go vmatch))
	(:else   (format t "~&Type new pathname to be passed off as ~A~% (default ~A) : "
			 report-path trans-path)
		 (let ((new-path (fs:parse-pathname
				   (string-trim '(#\sp #\tab) (readline)) nil trans-path)))
		   (setq new-path (fs:merge-pathname-defaults new-path trans-path
							      (funcall trans-path ':type)))
		   (cond ((null (fs:get-pathname-host (funcall new-path ':host)))
			  (format t "~&The host ~A isn't a known file server."
				  (funcall new-path ':host)))
			 ((null (setq newprobe (probef new-path)))
			  (format t "~&~A doesn't appear to exist." new-path))
			 ((and (equal (funcall newprobe ':type)
				      (funcall path ':type))
			       (equal (funcall newprobe ':name)
				      (funcall path ':name)))
			  (setq latest-version (funcall newprobe ':version)
				fetch-path newprobe)
			  (go vmatch))
			 ;;A completely wild thing is being substituted.
			 (t (setq extra (format
					  nil"UNSPECIFIC-VERSION T~%NEEDED-VERSION ~D~%"
					  version))
			    (setq fetch-path new-path
				  report-path (funcall path ':new-version ':unspecific))
			    (go dump)))
		   (go query))))
      vmatch
      (cond ((not (numberp latest-version))(go cv-dump))
	    ((= latest-version version)
	     (setq extra nil)
	     (go cv-dump))
	    ((> latest-version version) (go cv-dump))
	    ((fquery format:yes-or-no-quietly-p-options
		     "~A isn't even as recent in version as the needed ~
				   version ~D.~%Do you really want to use ~A? "
		     fetch-path version fetch-path)
	     (go cv-dump))
	    (t (go query)))
	
      cv-dump
	
      (setq report-path (funcall report-path ':new-version latest-version))
	
      dump
      
      (or (typep report-path 'fs:logical-pathname)
	  (setq report-path (coerce-report-path-to-logical report-path))
	  (return nil))
	
      (distribution-file-writer
	stream "SOURCE-FILE" fetch-path report-path t system extra)
      (return t))))

(defun coerce-report-path-to-logical (path)
  (tv:beep)
  (format t "~&~A is not a logical (device-independent) pathname.  There is no way~@
	  that this file can be loaded at the other end unless a logical pathname ~
	  is supplied." path)

  (loop as choice = (fquery '(:choices (((nil   "Nothing; skip dumping this file.") #/N #/n)
					((:prompt "Prompt for a logical pathname")
					 #/P #/p)))
			    "What do you wish to do? ")
	if (null choice) return nil
	do
	(format t "~&Type a logical pathname, which is where ~A~%is to claim to belong: "
		path)
	(let ((line (string-trim '(#\SP #\TAB) (readline)))
	      new-path dir host dirfound version)
	  (cond ((zerop (string-length line)))
		((null (setq new-path (car (errset (fs:parse-pathname line)))))
		 (format t "~&~A is an invalid pathname." line))
		((not (typep new-path 'fs:logical-pathname))
		 (format t "~&~A isn't a logical pathname either." new-path))
		((and (or (not (equal (funcall path ':name) (funcall new-path ':name)))
			  (not (equal (funcall path ':type) (funcall new-path ':type))))
		      (not (y-or-n-p (format
				       nil
				       "~&~A and ~A don't have the same name and type.~@
				      Are you sure this is right? " path new-path)))))
		((and (null (setq dirfound
				  (assoc (setq dir (funcall new-path ':directory))
					 (funcall
					   (setq host
						 (funcall new-path ':host)) ':translations))))
		      (not (y-or-n-p
			     (format nil
				     "~&~A isn't a defined, translated directory on ~A.~@
				     Are you sure ~A is right? " dir host new-path)))))
		((null (funcall new-path ':version))
		 (if 
		   (y-or-n-p
		     (format nil "~&~A has no version.  Do you want to pass it off as~@
			   version ~D, which is what will go on the tape?~@
			   (/"no/" means load as /"latest/"): "
			     new-path (setq version (funcall path ':version))))
		   (setq new-path (funcall new-path ':new-version version)))
		 (return new-path))
		(t (return new-path))))))

(defun wdsf-help (path report fetch case)
  (format t "~&S = Skip this, make no attempt to dump any version of ~A.~@
	       T = Try again to get ~A (after you've attempted to remedy the problem).~@
               D = List all files matching ~A.~@
	       ~:[~*~*~;L = Use ~A, the latest version of ~A.~@
               ~]E = Prompt for a new pathname to be written as ~A.~2%"
	  path report (funcall path ':new-pathname ':type ':wild ':version ':wild)
	  (eq case ':specific-not-found-but-there-are-some) fetch path path)) 

(defun distribution-display-directory (path &aux directory)
  (setq path (funcall path ':new-pathname ':version ':wild ':type ':wild))
  (setq directory (fs:directory-list path ':noerror))
  (if (stringp directory)
      (format t "~&~A" directory)
    (setq directory (cdr directory))
    (format t "~&~A on ~A:~%" (funcall path ':string-for-host)
	    (funcall
	      (let ((host (funcall path ':host)))
		(or (funcall host ':send-if-handles  ':host) host))
	      ':name-as-file-computer))
    (dolist (file directory)
      (format t "~&~A~15T~D ~D(~D)~30t"
	      (funcall (car file) ':string-for-dired) (get file ':length-in-blocks)
	      (get file ':length-in-bytes) (get file ':byte-size))
      (time:print-universal-time (get file ':creation-date))
      (format t "~@[ ~A~]~%" (get file ':author)))))


(defun distribution-file-writer
       (stream key realpath reportpath characters system additional)
  (with-open-file (file realpath ':characters characters)	;just in case
    (let* ((truename (funcall file ':truename))
	   (props (fs:file-properties truename t))
	   (sysname (if (stringp system) system (si:system-name system)))
	   (truepath (funcall truename ':string-for-printing))
	   (gmtcrd (time:print-universal-time (get props ':creation-date) nil 0))
	   (author (get props ':author)))
      (format stream
	      "~A ~A~@
		SYSTEM ~A~@
		SOURCE-PATH-FLAVOR ~A~@
		SOURCE-PATH ~A~@
		AUTHOR ~A~@
		CREATION-DATE ~A GMT~@
		BYTE-SIZE ~D~@
		CHARACTERS ~S~@
		~@[~A~]END~%"
	      key
	      reportpath
	      sysname
	      (typep truename)
	      truepath
	      author
	      gmtcrd
	      (get props ':byte-size)
	      characters
	      additional)
      (format *log-file*
	    "   ~26A  ~30A ~8A ~A~%" reportpath truepath author gmtcrd))
    (format t "~&Writing ~A." reportpath)
    (funcall stream ':eof)
    (if characters
	(stream-copy-until-eof file stream)
      (distribution-16-bit-copy file stream))
    (funcall stream ':eof)))

(defresource distribution-8bit ()
  :initial-copies 0
  :constructor (make-array 2 ':type 'art-8b ':displaced-to 0 ':displaced-index-offset 0))

(defun distribution-16-bit-copy (from to)
  (using-resource (indirect distribution-8bit)
    (do () (())
      (multiple-value-bind (buf start lastx)
	  (funcall from ':read-input-buffer)
	(if (null buf) (return nil))
	(let ((bytelen (* 2 (- lastx start))))
	  (si:change-indirect-array indirect 'art-8b (list bytelen) buf (* 2 start))
	  (funcall to ':string-out indirect 0 bytelen))
	(funcall from ':discard-current-input-buffer)))))

(defun distribution-dump-patches (system stream)
  (let ((pdir (si:system-patch-directory system))
	(name (si:system-name system)))
    (cond ((not (null pdir))
	   (let* ((pn (si:patch-directory-pathname pdir))
		  (loaded-version (si:get-system-version system))
		  (version (distribution-dump-system-directory pdir pn name stream
							       loaded-version)))
	     (loop for (minor reason)
		   in (second
			(distribution-dump-version-directory pdir pn name version stream))
		   if (and reason ( minor 0))
		   do (distribution-dump-patch-file pdir pn name version minor stream)))))))

(defun distribution-dump-system-directory (pdir pn name stream loadedv)
  (let ((pd (funcall pn ':patch-file-pathname name (si:patch-directory-same-directory-p pdir)
		     (si:patch-directory-patch-atom pdir)
		     ':system-directory)))
    (cond ((probef pd)
	   (let ((file-v (let ((ibase 10.))
			   (with-open-file (str pd) (second (read str))))))
	     (or (= file-v loadedv)
		 (ferror nil "Trying to write out ~A ~D, but ~A has version ~D."
			 name loadedv pd file-v))
	     (distribution-file-writer stream "PATCH-SYSTEM-DIRECTORY" pd pd t name nil)
	     file-v))
	  (t 0))))

(defun distribution-dump-version-directory (pdir pn name version stream)
  (let ((pd (funcall pn ':patch-file-pathname name (si:patch-directory-same-directory-p pdir)
		     (si:patch-directory-patch-atom pdir)
		     ':version-directory version)))
    (cond ((probef pd)
	   (distribution-file-writer stream "PATCH-VERSION-DIRECTORY" pd pd t name
				     (format nil "PATCH-VERSION ~D~%" version))
	   (with-open-file (str pd) (let ((ibase 10.)) (read str))))
	  (t 0))))

(defun distribution-dump-patch-file (pdir pn name version minor stream)
  (let ((pd (funcall pn ':patch-file-pathname name (si:patch-directory-same-directory-p pdir)
		     (si:patch-directory-patch-atom pdir)
		     ':patch-file version minor "lisp"))
	(addnl (format nil "PATCH-VERSION ~D~%PATCH-MINOR-VERSION ~D~%" version minor)))
    (distribution-file-writer stream "PATCH-SOURCE-FILE" pd pd t name addnl)
    (setq pd (funcall pd ':new-type "qfasl"))
    (distribution-file-writer stream "PATCH-OBJECT-FILE" pd pd nil name addnl)))


(defun distribution-define-system (sys &optional (no-error-p nil) &aux home)
  (if (listp sys)
      (setq home (cadr sys) sys (car sys)))
  (setq sys (string sys))
  (cond ((si:find-system-named sys t))
	(home
	 (load home)
	 (si:find-system-named sys))
	(no-error-p nil)
	(t (ferror nil "No idea where system ~A declaration can be found." sys))))




(defun distribution-tape-stream (&key &optional
				 (tape-p nil)
				 (reel nil)
				 (direction ':input)
				 (host nil)
				 (unit 0) &aux mode)
  (setq mode (selectq direction (:output ':write) (t ':read)))
  (make-instance
    (selectq mode
      (:read  'si:embedded-length-character-input-stream)
      (:write 'si:embedded-length-character-output-stream))
    ':target-stream
    (cond ((eq tape-p ':connect)
	   (if (null host) (ferror nil "Connect specified, but no host."))
	   (if (null reel) (ferror nil "Connect specified, but no reel name."))
	   (let ((conn (chaos:connect host (format nil "S-TAPE ~A ~A ~D" mode reel unit))))
	     (if (stringp conn)
		 (ferror nil "Can't connect to S-TAPE at ~A, ~A" host conn)
	       (chaos:make-stream conn ':direction direction))))
	  (tape-p
	   (tape:open-tape unit ':mode mode))
	  (t (let ((conn (chaos:listen "TAPE")))
	       (if (stringp conn)
		   (ferror nil "Can't connect to TAPE: ~A" conn))
	       (chaos:accept conn)
	       (chaos:make-stream conn ':direction direction))))))

(defun distribution-collect-tokens (remark)
  (if remark (format t "~&~A, separate by spaces: " remark))
  (distribution-tokenize (readline)))

(defun distribution-tokenize (s)
  (nreverse
    (do  ((start 0)
	  (x)
	  (collected nil))
	 (())
    (or
      (setq start (string-search-not-char #\SP s start))
      (return collected))
    (setq x (string-search-char #\SP s start))
    (push (substring s start x) collected)
    (if x
	(setq start x)
      (return collected)))))

(defun get-distribution-system-specs (&optional
				      (remark "Systems to dump"))
  (do () (())
    (*catch 'systems-retry
      (return
	(loop for sys in (distribution-collect-tokens remark)
	      collect (or (distribution-define-system sys t)
			  (*throw 'systems-retry
				  (format t "~&~A isn't a known system." sys))))))))



(defun get-distribution-ucode-number (&aux val)
  (do () (())
    (format t "~&Type microcode version number, default (CR) is ~D: "
	    %microcode-version-number)
    (let ((s (string-trim '(#\sp #\tab) (readline))))
      (cond ((zerop (string-length s)) (return %microcode-version-number))
	    ((or (null (setq val (parse-number s)))
		 (not (fixp val))
		 (not (plusp val)))
	     (format t "~&Invalid number: ~A" s))
	    
	    (t (return val))))))

(defun distribution-merge (current new what-to-do)
  (selectq what-to-do
    (:add   (dolist (x new)
	      (or (member x current)
		  (push x current))))
    (:remove (dolist (x new)
	       (setq current (delete x current)))))
  current)

(defun write-distribution-fonts (stream)
  (format *log-file* "~3%------------------------ F O N T S ---------------------------~3%")
  (let ((gpath (fs:parse-pathname  "sys:fonts;* qfasl >")))
    (dolist (font-file (mapcar #'car (cdr (fs:directory-list gpath ':fast))))
      (distribution-file-writer
	stream "TV-FONT-FILE" font-file (funcall gpath ':back-translated-pathname font-file)
	nil "TV-FONTS"
	(format nil "FONT ~A~%" (funcall font-file ':name))))))

(defun write-distribution-ucode (stream uversion)
  (format *log-file* "~3%-------------------- M I C R O C O D E  ~D --------------------~3%"
	  uversion)
  (format t "~&Dumping microcode source, object, table and symbols for microcode ~D."
	  uversion)
  (let ((versaddnl (format nil "MICROCODE-VERSION ~D~%" uversion))
	(ucadr (funcall (fs:parse-pathname "sys:ucadr;ucadr lisp") ':new-version uversion))
	(tbl (funcall (fs:parse-pathname "sys:ubin;ucadr tbl") ':new-version uversion))
	(sym (funcall (fs:parse-pathname "sys:ubin;ucadr sym") ':new-version uversion))
	(mcr (funcall (fs:parse-pathname "sys:ubin;ucadr mcr") ':new-version uversion)))
    (distribution-file-writer stream "MICROCODE-SOURCE" ucadr ucadr t "UCODE" versaddnl)
    (distribution-file-writer stream "MICROCODE-OBJECT" mcr mcr nil "UCODE" versaddnl)
    (distribution-file-writer stream  "MICROCODE-SYMBOLS" sym sym t "UCODE" versaddnl)
    (distribution-file-writer stream "MICROCODE-TABLE" tbl tbl t "UCODE" versaddnl)))



;;; Band stuff

(defflavor disk-band-mixin
	((rqb nil) unit block part-base part-size wiredp quantum top report iolen)
	()
  (:initable-instance-variables unit part-base part-size quantum)
  (:default-init-plist :quantum 17.)
  (:required-methods :io :close))

(defmethod (disk-band-mixin :after :init) (&rest ignore)
  (setq rqb (si:get-disk-rqb quantum))
  (setq top (+ part-base part-size))
  (setq block part-base)
  (setq report 0)
  (format t "~&~D blocks.~%" part-size)
  (cond ((fixp unit)
	 (setq wiredp t)
	 (si:wire-disk-rqb rqb)
	 (setq si:disk-error-retry-count 20.))
	(t (setq wiredp nil))))

(defmethod (disk-band-mixin :set-iolen) ()
  (setq iolen (min (- top block) quantum)))

(defmethod (disk-band-mixin :before :io) ()
  (if (and wiredp (< iolen quantum))
      (si:wire-disk-rqb rqb iolen)))

(defmethod (disk-band-mixin :after :io) ()
  (incf block iolen)
  (do ((n-hundred (// (- block part-base) 100.))) (( report n-hundred))
    (incf report)
    (format t "~d " report)))


(defmethod (disk-band-mixin :after :close) (&rest ignore)
  (cond (rqb
	 (if wiredp (si:unwire-disk-rqb rqb))
	 (si:return-disk-rqb rqb))))

(defflavor disk-band-input-mixin ()
	   (disk-band-mixin)
  (:required-flavors si:buffered-input-stream))

(defmethod (disk-band-input-mixin :io) ()
  (if wiredp
      (si:disk-read-wired rqb unit block)
    (si:disk-read rqb unit block)))

(defmethod (disk-band-input-mixin :discard-input-buffer) (ignore))

(defflavor disk-band-output-mixin ()
	   (disk-band-mixin)
  (:required-flavors si:buffered-output-stream))

(defmethod (disk-band-output-mixin :io) ()
  (if wiredp
      (si:disk-write-wired rqb unit block)
    (si:disk-write rqb unit block)))

(defmethod (disk-band-output-mixin :before :new-output-buffer) ()
  (if ( block top)
      (ferror nil "Attempt to write outside of partition.")
    (funcall-self ':set-iolen)))

(defmethod (disk-band-output-mixin :discard-output-buffer) (&rest ignore))

(defmethod (disk-band-output-mixin :send-output-buffer) (ignore ignore)
  (funcall-self ':io))

(defflavor disk-band-8-bit-input-stream ()
	   (disk-band-input-mixin si:buffered-input-stream))

(defmethod (disk-band-8-bit-input-stream :next-input-buffer) (&rest ignore)	;nohang
  (if ( block top)
      nil
    (funcall-self ':set-iolen)
    (funcall-self ':io)
    (values (si:rqb-8-bit-buffer rqb) 0 (* 4 sys:page-size iolen))))

(defflavor disk-band-8-bit-output-stream ()
	   (disk-band-output-mixin si:buffered-output-stream))

(defmethod (disk-band-8-bit-output-stream :new-output-buffer) ()
  (values (si:rqb-8-bit-buffer rqb) 0 (* 4 sys:page-size iolen)))



(defmacro with-unit-argument ((var . args) &body body)
  `(let ((,var 0))				;sloppy - var can't have same name as an arg
     (unwind-protect
       (progn
	 (setq ,var (si:decode-unit-argument . ,args))
	 . ,body)
       (si:dispose-of-unit ,var))))

(defun write-distribution-band (band-spec stream)
  (let ((unit-spec (first band-spec))
	(part-name (second band-spec)))
    (with-unit-argument (unit  unit-spec "Reading for distribution tape")
      (let ((start (si:find-disk-partition-for-read part-name nil unit)))
	(or start (ferror nil "Can't find ~S, even though we checked before."))
	(format t "~&")
	(let ((comment (si:partition-comment part-name unit))
	      (length (si:measured-size-of-partition part-name unit))
	      (machine (if (stringp unit-spec) unit-spec (funcall si:local-host ':name)))
	      (dunit (if (stringp unit-spec) 0 unit-spec)))
	  (format stream
		  "PARTITION ~A~@
		     PART-SIZE ~D~@
		     MACHINE ~A~@
		     DISK-UNIT ~D~@
		     PARTITION-COMMENT ~A~@
		     END~%"
		  part-name length machine dunit comment)
	  (format *log-file* "BAND:   ~A (~D blocks) from ~A Drive ~D: ~A~%"
		  part-name length machine dunit comment)
	  (format t "~&Writing band ~A ~:[from ~A~;drive ~D~]. (~A) ~D blocks."
		  part-name (numberp unit-spec) unit-spec comment length)
	  (funcall stream ':eof)
	  (with-open-stream
	    (part-stream (make-instance 'disk-band-8-bit-input-stream
					':unit unit ':part-base start ':part-size length))
	    (stream-copy-until-eof part-stream stream))
	  (funcall stream ':eof))))))

(defun get-distribution-band-spec (&optional remark)
  (if remark (format t "~&~A" remark))
  (do () (())
    (format t "~&Type a band's name, or name and unit: ")
    (let ((spec (distribution-collect-tokens nil)))
      (if (null spec)
	  nil
	(map #'(lambda (x) (let ((y (parse-number (car x))))
			     (if y (rplaca x y))))
	     spec)
	  (let ((part (first spec))
		(unit (second spec)))
	    (if (null unit) (setq unit 0))
	    (if (numberp part) (setq part (format nil "LOD~D" part)))
	    (cond ((numberp unit)
		   (if (si:find-disk-partition part nil unit)
		       (return (list unit part))
		     (format t "~&Part ~A not found on drive ~D." part unit)))
		  ((null (chaos:address-parse unit))
		   (format t "~&Unknown host: ~A" unit))
		  (t (if (with-unit-argument (punit unit "Distribution tape")
			   (si:find-disk-partition part nil punit))
			 (return (list unit part))
		       (format t "~&Part ~A not found on drive ~D." part unit)))))))))

(defun distribution-display-band-list (bands)
  (format t "~&Bands to be dumped: ")
  (format:print-list t "~A"
     (mapcar #'display-one-band bands)))

(defun display-one-band (x)
  (let ((unit (first x))
	(name (second x)))
    (cond ((stringp unit)
	   (format nil "Band ~A on ~A" name unit))
	  ((= unit 0) name)
	  ((format nil "Band ~A drive ~D" name unit)))))



;;;
;;; Window System interface to above froboz.
;;;

(defflavor distribution-option-frame (chvv-pane interact-pane scroll-pane menu-pane)
	   (tv:any-tyi-mixin tv:stream-mixin
	    tv:bordered-constraint-frame-with-shared-io-buffer)
  (:default-init-plist :width #O600 :height #O1000))

(defflavor distribution-chvv-pane ()
	   (tv:dont-select-with-mouse-mixin tv:choose-variable-values-pane))

(defflavor distribution-interact-pane ()
	   (tv:dont-select-with-mouse-mixin
	    tv:pane-mixin
	    tv:autoexposing-more-mixin tv:window))

(defflavor distribution-scroll-pane ()
	   ;; See INSPCT.lisp
	   (tv:pane-mixin 
	     tv:scroll-mouse-mixin
	    tv:margin-scrolling-with-flashy-scrolling-mixin
	    tv:flashy-scrolling-mixin tv:dont-select-with-mouse-mixin tv:borders-mixin
	    tv:margin-scroll-mixin tv:margin-region-mixin
	    tv:scroll-window)
  )

;;; Choose variable values should share Trotsky's fate.

(defvar *dlcv-Global-var-dump-device* "Net Listen")
(defvar *dlcv-Global-dump-fonts* t)
(defvar *dlcv-Global-comment* "")
(defvar *dlcv-Global-ucode* nil)
(defvar *dlcv-Global-host* "")

(defconst *distribution-dump-chvv-options*
	  '((*dlcv-Global-var-dump-device* "Access tape via" :choose
					   ("Local" "Net Listen" "Connect"))
	    (*dlcv-Global-host* "Tape host, if connect" :string)
	    (*dlcv-Global-dump-fonts* "Dump Fonts" :boolean)
	    (*dlcv-Global-ucode* "Dump Microcode" :boolean)
	    (*dlcv-Global-comment* "Tape Comment" :string)))


(defconst *distribution-dump-command-menu*
	  '(("Add System" :value :add-system
	     :documentation "Prompt for the name of a system to add")
	    ("Standard Systems" :value :standard-systems
	     :documentation "Dump all the standardly dumped systems")
	    ("Delete System"
	     :buttons
	     ((NIL :value :delete-one-system)
	      (NIL :value :reset-system-list)
	      (NIL :value :beep))
	     :documentation
	     "L: Prompt for system not to dump, or mouse it.  M: Reset system list to empty.")
	    ("Add Band" :value :add-band
	     :documentation "Prompt for a band to dump.")
	    ("Delete Band"
	     :buttons
	     ((NIL :value :delete-one-band)
	      (NIL :value :reset-band-list)
	      (NIL :value :beep))
	     :documentation
	     "L: Prompt for band not to dump, or mouse it.  M: Reset band list to empty.")
	    ("Ucode Version" :value :ucode-version
	     :documentation "Specify version of microcode to be dumped")
	    ("Abort" :value :abort :font fonts:medfnb
	     :documentation "Click to get out of here without doing anything")
	    ("Do it" :value :do-it :documentation "Start the dump" :font fonts:medfnb)))
	    
(defmethod (distribution-option-frame :before :init) (&rest ignore)
  (setq tv:selected-pane 'interact-pane
	tv:panes `((chvv-pane      distribution-chvv-pane
				   :label (:string "Distribution dump options"
						   :font fonts:tr12i)
				   :margin-choices nil
				   :variables
				   ,*distribution-dump-chvv-options*
				   :stack-group ,sys:%current-stack-group)	;!@#$%#$&
		   (scroll-pane    distribution-scroll-pane
				   :label nil
				   :margin-scroll-regions
				   ((:top "Top of option list")
				    (:bottom "Bottom of option list")))
		   (interact-pane  distribution-interact-pane :label nil)
		   (menu-pane      tv:command-menu-pane
				   :item-list
				   ,*distribution-dump-command-menu*))
	tv:constraints
	        '((only . ((chvv-pane scroll-pane interact-pane menu-pane)
			   ((chvv-pane :ask :pane-size)
			    (menu-pane :ask :pane-size))
			   ((interact-pane .20))
			   ((scroll-pane :even)))))))

(defmethod (distribution-option-frame :after :init) (&rest ignore)
  (setq chvv-pane (funcall-self ':get-pane 'chvv-pane))
  (setq scroll-pane (funcall-self ':get-pane 'scroll-pane))
  (setq interact-pane (funcall-self ':get-pane 'interact-pane))
  (setq menu-pane (funcall-self ':get-pane 'menu-pane)))



(defwindow-resource distribution-frame ()
  :make-window (distribution-option-frame))

(defun get-distribution-options (&aux (ucode-version sys:%microcode-version-number)
				 interact-pane
				 systems bands reel (drive 0) do-it
				   scroll-pane
				   &special	;love ya, chvv
				   (*dlcv-Global-dump-fonts* t)
				   (*dlcv-Global-comment* "")
				   (*dlcv-Global-ucode* nil))

  (using-resource (frame distribution-frame)
    (setq scroll-pane (funcall frame ':get-pane 'scroll-pane))
    (unwind-protect				;make sure these stack pointers go away..
      (progn
	(funcall
	  (funcall frame ':get-pane 'chvv-pane) ':set-stack-group sys:%current-stack-group)
	;;should setup menus for dumping too, but,.. leave for a few hours..
	(funcall scroll-pane
		 ':set-display-item
		 (distribution-dump-display-item (locf bands) (locf systems)
						 (locf reel) (locf drive)
						 (locf ucode-version)))
	(setq interact-pane (funcall frame ':get-pane 'interact-pane))
	(tv:window-call (frame :deactivate)
	 (tv:window-call (interact-pane :deactivate)
	  (let ((terminal-io interact-pane)
		(chvv (funcall frame ':get-pane 'chvv-pane)))
	    (*catch
	      'outer
	      (do () (())
		(*catch
		  'sys:command-level
		  (do ((noclear nil)) (())
		    (if noclear
			(setq noclear nil)
		      (funcall interact-pane ':clear-screen))	;should be small
		    (funcall scroll-pane ':redisplay)
		    ;;One day the activity system will fix all this, right?
		    (if (neq tv:selected-window interact-pane)
			(funcall interact-pane ':select))
		    (let ((object (funcall frame ':any-tyi)))
		      (cond
			((numberp object)	;let it go for now
			 (selectq (char-upcase object)
			   (#\END  (if (null reel)
				       (setq reel (get-reel-spec))
				     (cond ((and (null bands) (null systems)
						 (null *dlcv-Global-ucode*)
						 (null *dlcv-Global-dump-fonts*))
					    (format t "~&What do you want to dump?")
					    (setq noclear t))
					   ((and (equal *dlcv-Global-var-dump-device*
							"Connect")
						 (or (and
						       (zerop
							 (string-length *dlcv-Global-host*))
						       (progn
							 (format t
								 "~&You must specify a host.")
							 (setq noclear t)))
						     (and
						       (null (chaos:address-parse
							       *dlcv-Global-host*))
						       (progn
							 (format t "Bad host: ~A"
								 *dlcv-Global-host*)
							 (setq noclear t))))))
					   (t (setq do-it t)
					      (*throw 'outer 'do-it)))))
			   (#/S			;add system
			    
			    (setq systems (distribution-merge
					    systems (get-distribution-system-specs) ':add)))
			   
			   (#/B	    
			    (setq bands
				  (distribution-merge
				    bands (list (get-distribution-band-spec)) ':add)))
			   (#/R  (setq reel (get-reel-spec)))
			   (#/F  (setq *dlcv-Global-dump-fonts*
				       (not *dlcv-Global-dump-fonts*))
			         (funcall chvv
					  ':redisplay-variable '*dlcv-Global-dump-fonts*))
			   (#/U (funcall frame ':force-kbd-input ':ucode-mouse))
			   (#\SP)
			   (t (tv:beep))))
			((atom object)
			 (selectq object
			   (:reel-mouse  (setq reel (get-reel-spec)))
			   (:drive-mouse (setq drive (get-drive-spec)))
			   (:ucode-mouse (setq ucode-version (get-distribution-ucode-number))
					 (setq *dlcv-Global-ucode* t)
					 (funcall chvv ':redisplay-variable
						  '*dlcv-Global-ucode*))))
			(t (selectq (car object)
			     (system-delete (setq systems (delete (cadr object) systems)))
			     (band-delete   (setq bands (delete (cadr object) bands)))
			     (:menu
			      (let ((val (funcall (fourth object) ':execute (second object))))
				(selectq val
				  (:do-it    (funcall frame ':force-kbd-input '#\end))
				  (:abort    (setq do-it nil)
					     (*throw 'outer ':abort))
				  (:ucode-version
				            (funcall frame ':force-kbd-input ':ucode-mouse))
				  (:reset-band-list (setq bands nil))
				  (:add-band (funcall frame ':force-kbd-input #/B))
				  (:delete-one-band
				   (format t "~&Mouse or type band to remove: ")
				   (let ((x (funcall terminal-io ':any-tyi)))
				     (if (and (listp x)
					      (memq (car x) '(band-select band-delete)))
					 (setq bands (delete (cadr x) bands))
				       (funcall terminal-io ':untyi x)
				       (setq bands (delete (get-distribution-band-spec nil)
							   bands)))))
				  (:standard-systems
				   (setq systems (mapcar #'distribution-define-system
							 *distribution-standard-systems*)))
				  (:add-system (funcall frame ':force-kbd-input #/S))
				  (:reset-system-list  (setq systems nil))
				  (:delete-one-system
				   (format t "~&Mouse or type system to remove: ")
				   (let ((x (funcall terminal-io ':any-tyi)))
				     (if (and (listp x)
					      (memq (car x) '(system-select system-delete)))
					 (setq systems (delete (cadr x) systems))
				       (funcall terminal-io ':untyi x)
				       (setq
					 systems
					 (delete (get-distribution-system-specs nil) 
						 systems)))))
				  (t (print val)))))
			     
			     (:variable-choice
			      (tv:choose-variable-values-process-message
				(second object) object))
			     (t (print object)))))))))))))

	
	)
      (funcall scroll-pane ':set-display-item nil))
    (and do-it
	 (values systems bands *dlcv-Global-comment*
		 (distribution-tape-stream
		   ':tape-p (cond ((equal *dlcv-Global-var-dump-device* "local") t)
				  ((equal *dlcv-Global-var-dump-device* "connect") ':connect)
				  (t nil))
		   ':host *dlcv-Global-host*
		   ':direction  ':output
		   ':unit drive
		   ':reel reel)
		 reel (and *dlcv-Global-ucode* ucode-version)
		 *dlcv-Global-dump-fonts*))))

(defun distribution-dump-display-item (bands systems reel drive ucode-version)
  (list (list
	  ':pre-process-function 'distribution-list-preprocess
	  'systems systems 'bands bands
	  'bands-maintain
	  (list ()
		(tv:scroll-parse-item "Bands being dumped:")
		(tv:scroll-maintain-list
		  `(lambda () (car ',bands))
		  `(lambda (x)
		     (tv:scroll-parse-item
		       `(:mouse 
			  (nil :buttons
			       ((NIL :kbd (:band-select ,x))
				(NIL :kbd (:band-delete ,x))
				(NIL :kbd :beep))
			       :documentation
			       "L: Specify a band for choice.  M: Delete band from dump list")
			  :function ,',#'display-one-band (,x) nil ("    ~A"))))))
	  'systems-maintain
	  (list ()
		(tv:scroll-parse-item "Systems being dumped:")
		(tv:scroll-maintain-list
		  `(lambda () (car ',systems))
		  `(lambda (x)
		     (tv:scroll-parse-item
		       `(:mouse
			  (nil :buttons
			       ((NIL :kbd (:system-select ,x))
				(NIL :kbd (:system-delete ,x))
				(NIL :kbd :beep))
			        :documentation
				"L: Specify a system for choice. M: Delete system from dump list")
			  :function ,',#'si:system-name (,x) nil ("    ~A")))))))
	
	(tv:scroll-parse-item			;
	  `(:mouse
	     (nil :kbd :reel-mouse :documentation "Change tape reel")
	     :function
	     ;;Can't name these vars reel and drive or this function
	     ;;won't work interpreted.
	     ,#'(lambda (a-reel)
		  (setq a-reel (car a-reel))
		  (string-append
		    (if (null a-reel)
			"Tape reel unspecified"
		      (string-append "Reel " a-reel))))
	     (,reel))
	  `(:mouse
	     (nil :kbd :drive-mouse :documentation "Change tape drive")
	     :function
	     ,#'(lambda (a-drive)
		  (setq a-drive (car a-drive))
		  (if (string-equal *dlcv-Global-var-dump-device* "Local")
		      (format nil ", drive ~D." a-drive)
		    "."))
	     (,drive)))
	(tv:scroll-parse-item
	  `(:function ,#'(lambda ()
			   (if *dlcv-Global-ucode*
			       "Source and other files for UCADR "
			     "No microcode to be dumped.")) ())
	  `(:mouse
	     (nil :kbd :ucode-mouse :documentation "Change ucode version to be dumped")
	     :function
	     ,#'(lambda (*ucv)
		  (if *dlcv-Global-ucode*
		      (format nil "~D" (car *ucv))
		    ""))
	     (,ucode-version)))
	nil					;bands                   (FOURTH)
	nil					;systems                 (FIFTH)
	))

(defun get-reel-spec ()
  (do () (())
    (format t "~&Type reel name: ")
    (let ((s (string-trim '(#\sp #\tab) (readline))))
      (if (not (zerop (string-length s)))
	  (return s)))))

(defun get-drive-spec ()
  (do () (())
    (format t "~&Type tape drive # 1 digit~%SPACE is 0: ")
    (let ((c (tyi)))
      (cond ((not (fixp c)) (tv:beep))	;mice, god knows what
	    ((and ( c #/0) ( c #/9))
	     (return (- c #/0)))
	    ((= c #\SP)
	     (format t "0")
	     (return 0))
	      (t (tv:beep))))))

(defvar *no-band-scrpr* (tv:scroll-parse-item "No bands to be dumped."))
(defvar *no-sys-scrpr* (tv:scroll-parse-item "No systems to be dumped."))

(defun distribution-list-preprocess (list-item &aux (plist (locf (first list-item))))
  (let ((bands (get plist 'bands))
	(systems (get plist 'systems)))
    (setf (fourth list-item)
	  (if (null (car bands))			;no bands
	      *no-band-scrpr*
	    (get plist 'bands-maintain)))
    (setf (fifth list-item)
	  (if (null (car systems))
	      *no-sys-scrpr*
	    (get plist 'systems-maintain)))))

;;;
;;;  Reloader-oriented writers...
;;;



(defun write-bands-tape-file (stream bands)
  (format stream "BANDS-DUMPED~%END~%")
  (loop for (unit name) in bands
	do
	(with-unit-argument (dunit unit "Recording comment for distribution tape")
	  (if (numberp unit)
	      (format stream "~A ~D ~A ~A~%" (funcall si:local-host ':name) unit name
		      (si:partition-comment name dunit))
	    (format stream "~A 0 ~A ~A~%" unit name (si:partition-comment name dunit)))))
  (funcall stream ':eof))

(defun write-systems-tape-file (stream systems)
  (format stream "SYSTEMS-DUMPED~%END~%")
  (loop for system in systems
	as version-info = (multiple-value-list (si:get-system-version system))
	do (format stream "~A " (si:system-name system))
	   (cond ((null (car version-info))
		  ;;Probably should go through whole patch directory system and figure out
		  ;;what the version is -- to be considered.
		  (format stream "LATEST"))
		 (t (loop for item in version-info do (format stream "~D " item))))
	   (format stream "~%"))
  (funcall stream ':eof))


