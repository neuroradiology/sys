;-*- Mode:LISP; Package:Distribution; Base:8. -*-

;;;
;;; Distribution reloader
;;;

(defun distribution-inconvert-sexp (string &optional (start 0) ignore)
  (read-from-string string nil start))

(defconst *distribution-reloader-keywords*
	  '(((parse-number lmfs:reloader-dec-print)
	     :PART-SIZE :MICROCODE-VERSION :SEQUENCE :NEEDED-VERSION :BYTE-SIZE
	     :TAPE-SYSTEM-VERSION :DISK-UNIT
	     :PATCH-VERSION :PATCH-MINOR-VERSION :MICROCODE-VERSION-DUMPED)
	    ((distribution-inconvert-sexp princ)
	     :UNSPECIFIC-VERSION :TV-FONTS-DUMPED :CHARACTERS)
	    ((fs:parse-directory-date-property lmfs:reloader-time-print)
	     :CREATION-DATE DUMP-TIME)
	    ((substring princ)
	     :PARTITION :PARTITION-COMMENT :REEL :USER-ID SITE :MACHINE :SYSTEM
	     :END-OF-TAPE-PROLOGUE-FILES
	     :COMMENT :AUTHOR
	     :DISTRIBUTION-TAPE :OBJECT-FILE :SOURCE-FILE :SOURCE-PATH :SOURCE-PATH-FLAVOR
	     :PATCH-SYSTEM-DIRECTORY :PATCH-OBJECT-DIRECTORY :PATCH-SOURCE-FILE
	     :PATCH-OBJECT-FILE :TV-FONT-FILE
	     :MICROCODE-SOURCE :MICROCODE-OBJECT :MICROCODE-SYMBOLS :MICROCODE-TABLE
	     :BANDS-DUMPED :SYSTEMS-DUMPED)))


;;; Ok, CHVV, you can have Czechoslovakia.

(defvar *drldr-Global-device* "Local")
(defvar *drldr-Global-reel* "")
(defvar *drldr-Global-drive* 0)
(defvar *drldr-Global-host* "")

(defun distribution-reloader-get-stream ()	;use 'em special..
  (*catch
    'drldr-chvv-abort
    (do () (())
      (tv:choose-variable-values
	'((*drldr-Global-device* "Access tape via" :choose
				 ("Local" "Net Listen" "Connect"))
	  (*drldr-Global-reel* "Tape reel name" :string)
	  (*drldr-Global-host* "Host, if Connect" :string)
	  (*drldr-Global-drive* "Drive, if Connect or Local" :number))
	':label '(:string "Where to get distribution tape" :font fonts:tr12i)
	':margin-choices '("Do It" ("Abort" (*throw 'drldr-chvv-abort nil))))
      (cond ((and (member *drldr-Global-device* '("Connect" "Local"))
		  (zerop (string-length (string-trim '(#\SP #\TAB) *drldr-Global-reel*))))
	     (format t "~&For ~A mode, you must specify a reel, and you haven't yet."
		     *drldr-Global-device*))
	    ((and (string-equal *drldr-Global-device* "Connect")
		  (zerop (string-length (string-trim '(#\SP #\TAB) *drldr-Global-host*))))
	     (format t "~&You said to connect out, but you haven't said what host."))
	    ((and (string-equal *drldr-Global-device* "Connect")
		  (null (chaos:address-parse *drldr-Global-host*)))
	     (format t "~&~A isn't a known Chaos host." *drldr-Global-host*))
	    (t (return
		 (distribution-tape-stream
		   ':reel *drldr-Global-reel*
		   ':unit *drldr-Global-drive*
		   ':host *drldr-Global-host*
		   ':tape-p (cond ((equal *drldr-Global-device* "local") t)
				  ((equal *drldr-Global-device* "connect") ':connect)
				  (t nil))
		   ':direction ':input))))
      (if (not (y-or-n-p "Do you want to try again? "))
	  (return nil)))))


(defvar *all-distribution-reloader-keywords*)

(declare (special lmfs:*backup-keywords*))

(defun distribution-story-reader (stream &optional (eof t))
  (let ((lmfs:*backup-keywords* *all-distribution-reloader-keywords*))
    (prog1
      (lmfs:reloader-read-tape-story stream t)
      (if eof (distribution-clear-to-eof stream)))))

(defselect (fast-ignore)
  (:which-operations nil '(:string-out :tyo))
  (:tyo (&rest ignore))
  (:string-out (&rest ignore)))

(defun distribution-clear-to-eof (stream)
  (stream-copy-until-eof stream 'fast-ignore)
  (funcall stream ':clear-eof))

(defvar *logical-load-host* "SYS")

(defun distribution-load-parse-tape-band-spec (string)
  ;;typically  "pointer 3 mcr2 UCADR 841 exp"
  (let ((ibase 10.))				;10 drives, right?
    (multiple-value-bind (host host-end)
	(read-from-string string)
      (multiple-value-bind (unit unit-end)
	  (read-from-string string nil host-end)
	(multiple-value-bind (name name-end)
	    (read-from-string string nil unit-end)
	  (list (string host)
		unit
		(string name)
		(if (< name-end (string-length string))
		    (substring string name-end)
		  "")))))))

(defun distribution-load-parse-tape-system-spec (string &aux version)
  ;;typically "CUBE NEWEST" or "System 23 4 RELEASED"
  (let* ((tokenized (distribution-tokenize string))
	 (name (car tokenized))
	 (len (length tokenized)))
    (cond ((= len 1)				;Shouldn't happen, but
	   (list name name nil nil nil))
	  ((= len 2)
	   (cond ((setq version (parse-number (second tokenized)))
		  (list name (format nil "~A, version ~D" name version) version nil nil))
		 (t (list name (format nil "~A, ~A" name (second tokenized))
			  nil nil nil))))
	  (t (let ((maj (parse-number (second tokenized)))
		   (min (parse-number (third tokenized)))
		   (status (fourth tokenized)))
	       (cond ((not (and maj min))
		      (format t "~&Don't understand system spec ~&, ignoring." string)
		      nil)
		     (t (list name (format nil "~A ~D.~D~@[ (~A)~]" name maj min status)
			      maj min status))))))))

(defvar *punted-hosts* nil)

(defun load-distribution-tape (&aux s reel prologue tape-band-list tape-system-list
			       &special (*punted-hosts*))
  (multiple-value (s reel) (distribution-reloader-get-stream))
  (if s
      (with-open-stream (stream s)
	(setq *all-distribution-reloader-keywords*
	      (append *distribution-reloader-keywords* lmfs:*backup-keywords*))
	(setq prologue (distribution-story-reader stream))
	(or (getl prologue '(:distribution-tape))
	    (ferror nil "~&This does not appear to be a distribution tape"))
	(or (equal (get prologue ':version) 2)
	    (ferror nil "~&This is not a version 2 distribution tape, but appears to be~
			version ~D." (get prologue ':version)))
	(do () (())
	  (let* ((story (distribution-story-reader stream nil))	;don't eof
		 (type (car (getl story '(:bands-dumped :systems-dumped
							:end-of-tape-prologue-files)))))
	    (selectq type
	      (:bands-dumped
	         (loop as line = (funcall stream ':line-in)
		       when (equal line "") return nil
		       as parsed = (distribution-load-parse-tape-band-spec line)
		       if parsed do (push parsed tape-band-list)))
	      (:systems-dumped
	         (loop as line = (funcall stream ':line-in)
		       when (equal line "") return nil
		       as parsed = (distribution-load-parse-tape-system-spec line)
		       if parsed do (push parsed tape-system-list)))
	      (:end-of-tape-prologue-files
	       (distribution-clear-to-eof stream)
	       (return nil))
	      (t   (ferror nil "Unrecognized tape file: ~S" story))))
	  (funcall stream ':clear-eof))
	(let* ((opts '((:load t) :skip :selective))
	       (ucv (get prologue ':microcode-version-dumped))
	       (choices
		 (tv:multiple-choose
		   "Items to be loaded"
		   (nconc
		     (if (get prologue ':tv-fonts-dumped)
			 (list `((:fonts) "TV Font library" ,opts)))
		     (if ucv
			 (list `((:ucode ,ucv) ,(format nil "Microcode files, version ~D" ucv)
				 ,opts)))				
		     (loop for band in (reverse tape-band-list)
			   collect (list (cons ':band band)
					 (string-append "Band, " (fourth band))
					 opts))
		     (loop for system in (reverse tape-system-list)
			  collect (list (cons ':system system)
					(string-append "System, " (second system))
					opts)))
		  (copytree			;rplacation going on here...
		    '((:load "Load") (:skip "Skip") (:selective "Selective"))))))
	  (if
	    choices				;punt if nil, clicked 'bort.
	    (distribution-tape-scan stream choices))))))

(defun distribution-tape-scan (stream choices)
  (loop as story = (distribution-story-reader stream)
	as didit = nil
	as rtype = (car (getl story
			      '(:end-of-tape
				 :tv-font-file
				 :partition :source-file :object-file
				 :patch-system-directory :patch-version-directory
				 :patch-source-file :patch-object-file
				 :microcode-source :microcode-object
				 :microcode-symbols :microcode-table)))
	do
	(selectq rtype
	  (:end-of-tape (format t "~&End of tape. Reload done.")
			(return nil))
	  (:partition
	     (loop with host = (get story ':machine)
		   and  unit = (get story ':disk-unit)
		   and  name = (get story ':partition)
		   for ((type . item) action) in choices
		   if (and (eq type ':band)
			   (destructuring-bind (thost tunit tname tcomment) item
			     (cond ((and (= unit tunit) (string-equal tname name)
					 (string-equal thost host)
					 (or (eq action ':load)
					     (and (eq action ':selective)
						  (y-or-n-p
						    (format nil "Load ~A? " tcomment)))))
				    (distribution-load-band stream story tcomment)
				    t))))
		   return (setq didit t)))
	  (:tv-font-file
	   (loop for ((type) action) in choices
		 if  (and (eq type ':fonts)
			  (or (eq action ':load)
			      (and (eq action ':selective)
				   (y-or-n-p (format nil "Load font: ~A? "
						     (get story ':font))))))
		 return (distribution-load-font-file stream (setq didit story))))
	  ((:source-file :object-file :patch-system-directory :patch-version-directory
			 :patch-source-file :patch-object-file)
	   (loop with sysname = (get story ':system)
		 for ((type . item) action) in choices
		 if (and (eq type ':system)
			 (string-equal sysname (car item))
			 (or (eq action ':load)
			     (and (eq action ':selective)
				  (load-file-ynp rtype story))))
		 return (distribution-load-file stream (setq didit story))))
	  ((:microcode-source :microcode-object :microcode-symbols :microcode-table)
	   (loop with version = (get story ':microcode-version)
		 for ((type . item) action) in choices
		 if (and (eq type ':ucode)
			 (= (car item) version)
			 (or (eq action ':load)
			     (and (eq action ':selective)
				  (y-or-n-p (format nil "(Microcode ~D) Load ~A? "
						    version (get story rtype))))))
		 return (distribution-load-ucode-file stream (setq didit story)))))
	(if (not didit)
	    (distribution-clear-to-eof stream))))


(defun load-file-ynp (rtype story &aux (majv (get story ':patch-version)))
  (y-or-n-p
    (format nil "(System ~A) Load ~A? "
	    (get story ':system)
	    (selectq rtype
	      ((:source-file :object-file) (get story rtype))
	      ((:patch-source-file :patch-object-file)
	       (format nil "Patch ~D.~D (~:[source~;object~])"
		       majv (get story ':patch-minor-version)
		       (eq rtype ':patch-object-file)))
	      (:patch-system-directory
	       (format nil "Patch System directory"))
	      (:patch-version-directory
	       (format nil "Patch directory, v. ~D" majv))
	      (t (get story rtype))))))

(defun distribution-load-band (stream story comment)
  (loop with psize = (get story ':part-size)
	as gdbs = (get-distribution-band-spec	;dest-bind has once-only problem
		    (format nil "Where do you want to put ~A (<part> or <part unit>)~@
		    (Confirmation will be requested)? " comment))
	do
	(destructuring-bind (unit-spec part) gdbs		 
	  (with-unit-argument (unit unit-spec "Distribution tape load")
            (multiple-value-bind (first size)
		(si:find-disk-partition-for-write part nil unit)
	      (cond ((null first) nil)
		    ((> psize size)
		     (format t "~&~A is ~D blocks, and won't fit in the ~D here." psize size)
		     nil)
		    (t (si:update-partition-comment part "Incomplete load" unit)
		       (with-open-stream
			 (part-stream (make-instance
					'disk-band-8-bit-output-stream ':unit unit
					':part-base first ':part-size psize))
			 (stream-copy-until-eof stream part-stream))
		       (si:update-partition-comment part comment unit)
		       (funcall stream ':clear-eof)
		       (return t))))))
	(selectq
	  (fquery `(:choices (((:skip "Nothing; skip the band") #/N)
			      ((:load "Load it") #/L)
			      ((:print-disk-label "Print disk label") #/P))
			     :help-function
			     ,#'(lambda (&rest ignore)
				  (format
				    t "~&N = Nothing. Skip this band, don't so much ~
					      as try to load it.~@
					      L = Ask again for where to load it, and try~
					      again.~@
					      P = Print the disk label on drive zero.~%")))
		  "What should I do? ")
	  (:skip (return (distribution-clear-to-eof stream)))
	  (:load)
	  (:print-disk-label  (print-disk-label)))))


(defvar *distribution-test-xlate-hosts* nil)

(defun distribution-extract-host (string)
  (let* ((start (string-search-not-char #\SP string))
	 (colx (string-search-char #/: string start)))
    (or colx (ferror nil "Unhosted pathname: ~A" string))
    (values (substring string start colx) colx)))

(defun distribution-load-file-define-host (log-host)
  (loop do
	(format t "~&Type physical host name for logical host ~A: " log-host)
	(let ((new-host (string-trim '(#\SP #\TAB) (readline)))
	      host)
	  (cond ((zerop (string-length new-host)))
		((null (setq host (fs:get-pathname-host new-host)))
		 (format t "~&~A is not a known host." new-host))
		((typep host 'fs:logical-host)
		 (format t "~&~A is a logical host.  I want a physical one." new-host))
		(t (return (fs:add-logical-pathname-host log-host new-host nil)))))))

(defun distribution-load-file-define-dir-translation (host dir &aux path trans-dir cpath
						      (phys-host (funcall host ':host)))
  (loop as uhd = (fs:user-homedir phys-host)
	do
	(format t "~&Type a pathname for host ~A, whose directory component~@
		  will be the translation of directory ~A on host ~A, ~@
		  for example, ~A: "
		phys-host dir host (funcall uhd ':string-for-host))
	as new-path = (string-trim '(#\SP #\TAB) (readline))
	do
	(cond ((zerop (string-length new-path)))
	      ((neq phys-host
		    (funcall (setq path (fs:parse-pathname new-path nil uhd)) ':host))
	       (format t "~&~A isn't on host ~A. It has to be." path phys-host))
	      ((memq (setq trans-dir (funcall path ':directory)) '(nil :unspecific))
	       (format t "~&~A isn't quite the right thing - its directory component is ~S."
		       path trans-dir))
	       ;;Now we need a way to probe for directories, the old Ibblegribble is unpopular
	      ((not (memq (funcall path ':name) '(nil :unspecific)))
	       (cond ((funcall path ':get-handler-for ':pathname-as-directory)
		      (setq cpath (funcall (funcall path ':pathname-as-directory)
					   ':new-pathname ':name nil ':type ':nil
					   ':version nil))
		      (cond ((y-or-n-p
			       
			       (format nil "~&~A doesn't look like a directory pathname as I ~
			       asked for.~%Did you mean ~A? " path cpath))
			     (return
			       (fs:change-logical-pathname-directory
				 host dir (funcall cpath ':string-for-host))))))
		     (t (format t "~&A seems to be a lot more than a directory spec."
				path))))
	      (t
	       (fs:change-logical-pathname-directory host dir
						     (funcall path ':string-for-host))
	       (return t)))))

(defun distribution-test-load-translate (path)
  (multiple-value-bind (host-name restx)
      (distribution-extract-host path)
    (let ((rest (substring path restx))
	  (new-host-name (cadr (assoc host-name *distribution-test-xlate-hosts*))))
      (values (string-append new-host-name rest)
	      new-host-name
	      (fs:get-pathname-host new-host-name)))))
      

(defun distribution-load-file (stream story &aux path dir (didit nil))
  (*catch
    'punt
    (cond ((getl story '(:patch-system-directory
			  :patch-version-directory :patch-source-file :patch-object-file))
	   (distribution-load-patch-files stream story))
	  ((setq path (cadr (getl story '(:source-file :object-file))))
	   ;;A regular old file
	   ;;Try to see what host is here --
	   (let* ((host-name (distribution-extract-host path))
		  (host (fs:get-pathname-host host-name)))
	     (if (assoc host-name *distribution-test-xlate-hosts*)
		 (multiple-value (path host-name host)
		   (distribution-test-load-translate path)))
	     (cond ((null host)
		    (if (member host-name *punted-hosts*)
			(*throw 'punt 'punt))
		    (selectq
		      (fquery
		       '(:choices
			  (((:define "Define Logical Host") #/D)
			   ((:skip "Skip this file") #/S)
			   ((:punt "Punt this host for the rest of the tape.") #/P)))
		       "(For ~A): Host ~A is not defined.~@
			       Do you want to define it temporarily as a logical host? "
		       path host-name)
		      (:skip (*throw 'punt 'punt))
		      (:punt (push host-name *punted-hosts*)
			     (*throw 'punt 'punt)))
		    (distribution-load-file-define-host host-name)))
	     ;;;Ok, now hack directory.
	     (let ((logpath (fs:parse-pathname path)))
	       (setq host (funcall logpath ':host))
	       (cond ((null (funcall logpath ':directory-translatable-p))
		      ;; "The directory is not defined."
		      (setq dir (funcall logpath ':directory))
		      (selectq
			(fquery `(:choices
				   (((:define ,(format nil "Define directory ~A." dir)) #/D)
				    ((:skip "Skip this file.") #/S)))
				"(For ~A): Directory ~A not defined on logical host ~A.~@
				Do you want to define it as a directory on host ~A? "
				logpath dir host (funcall host ':host))
			(:skip (*throw 'punt 'punt)))
		      (distribution-load-file-define-dir-translation host dir)))
	       (setq didit (distribution-load-path stream logpath story)))))))
  (or didit (stream-copy-until-eof stream 'fast-ignore))
  (funcall stream ':clear-eof))


(defun distribution-load-path (stream path story &aux (target-path path) (doit t) props
			       tape-date tape-author file-date file-author)
  (setq tape-date (get story ':creation-date) tape-author (get story ':author))
  (cond ((and (eq (funcall path ':version) ':newest)
	      (getl story '(:source-file :object-file))))	;other files version dont ct
	((null (probef path)))			;not there, seems great too.
	((progn
	   (setq props (fs:file-properties path)
		 file-date (get props ':creation-date)
		 file-author (get props ':author))
	   
	   (and (= file-date tape-date)
		(string-equal file-author tape-author)))
	 (format t "~&~A already there, with correct author & date.  Not loading." path)
	 (setq doit nil))
	(t
	 (loop do
	       (selectq
		 (fquery
		  `(:choices
		     (((nil "Nothing. Don't load it") #/N)
		      ((:latest "Latest. Load as latest.") #/L)
		      ((:rename "Rename old") #/R)
		      ((:elsewhere "Elsewhere") #/E)
		      ((:dirlist "Directory list") #/D))
		     :help-function
		     (lambda (&rest ignore) (loadpath-help ',path )))
		  "~&~A exists already.  It was created ~A by ~A.  What~@
		  I have on tape was created ~A by ~A.  What should I do? "
		  path (time:print-universal-time file-date nil) file-author
		  (time:print-universal-time tape-date nil) tape-author)
		 
		 (nil  (return (setq doit nil)))
		 (:dirlist (distribution-display-directory path))
		 (:latest (setq target-path (funcall target-path ':new-version ':newest))
			  (return (setq doit t)))
		 (:rename (cond ((distribution-load-path-rename path)
				 (return (setq doit t)))))
		 (:elsewhere (let ((tem (distribution-load-path-get-alt-path path)))
			       (cond (tem (setq target-path tem doit t)
					  (return t)))))))))

  (cond (doit
	 (with-open-file (file target-path ':direction ':output
			       ':characters (get story ':characters))
	   (format t "~&Loading ~A into ~A." path (funcall file ':truename))
	   (if (get story ':characters)
	       (stream-copy-until-eof stream file)
	     (distribution-16-bit-reverse-copy stream file))
	   (close file)
	   (setq target-path (funcall file ':truename)))
	 (fs:change-file-properties target-path nil ':author tape-author
				    ':creation-date tape-date)
	 t)
	(t nil)))

(defun loadpath-help (path)
  (format t "~&N = Do nothing, do not load ~A anywhere.~@
	       L = Load the version of ~A on tape as the newest version of ~A.~@
	       R = Prompt for a name to rename ~A to, and then load the version on tape.~@
	       E = Prompt for another place to put the copy of ~A on tape.~@
	       D = List all files matching ~A.~%~%"
	  path
	  path (funcall (funcall path ':new-version ':unspecific) ':translated-pathname)
	  (funcall path ':translated-pathname)
	  path
	  (funcall (funcall path ':translated-pathname) ':new-pathname
		   ':version ':wild ':type ':wild ':name ':wild)))


(defun distribution-load-path-get-alt-path (logpath &aux parsed merged)
  (loop with physpath = (funcall logpath ':translated-pathname)
	do (format t "~&Type new pathname for ~A, (default ~A): " logpath physpath)
	as line = (string-trim '(#\SP #\TAB) (readline))
	do
        (cond ((zerop (string-length line)))
	      (t (setq parsed (fs:parse-pathname line nil physpath))
		 (setq merged (fs:merge-pathname-defaults parsed physpath
							  (funcall physpath ':type)
							  ':newest))
		 (cond ((eq (funcall merged ':version) ':newest)
			(return merged))
		       ((probef merged)
			(format t "~&~A already exists, too." merged)
			(return nil))
		       (t (return merged)))))))


(defun distribution-load-path-rename (logpath &aux parsed merged err)
  (loop with physpath = (funcall logpath ':translated-pathname)
	do (format t "~&Type new pathname for ~A, (default ~A): " physpath physpath)
	as line = (string-trim '(#\SP #\TAB) (readline))
	do
        (cond ((zerop (string-length line)))
	      (t (setq parsed (fs:parse-pathname line nil physpath))
		 (setq merged (fs:merge-pathname-defaults parsed physpath
							  (funcall physpath ':type)
							  ':newest))
		 (cond ((stringp (setq err (renamef physpath merged nil)))
			(format t "~&Cannot rename ~A: ~A" physpath err)
			(return nil))
		       ((probef physpath)
			(format t "~&~A still exists." merged)
			(return nil))
		       (t (return t)))))))

(defresource distribution-16bit ()
  :initial-copies 0
  :constructor (make-array 2 ':type 'art-16b ':displaced-to 0 ':displaced-index-offset 0))

(defun distribution-16-bit-reverse-copy (from to &aux 16len)
  (using-resource (indirect distribution-16bit)
    (do () (())
      (multiple-value-bind (buf start lastx)
	  (funcall from ':read-input-buffer)
	(cond ((null buf) (return nil))
	      ((or (oddp start) (oddp (- lastx start)))	;hard to believe, but just in case,
	       (return (loop as c1 = (funcall from ':tyi)
			     as c2 = (funcall from ':tyi)
			     when (null c1) return nil
			     (funcall to ':tyo (dpb c2 #O1010 c1)))))
	      ;; King of elegance.  Double indirect arrays don't work, so....
	      ((prog1
		 (array-indexed-p buf)		;assume 16..
		 (setq 16len (// (- lastx start) 2)))
	       (let ((disp-to (%p-contents-offset buf 1))
		     (disp-off (%p-contents-offset buf 3)))
		 (or (eq (array-type disp-to) 'art-16b)
		     (ferror nil "Odd array indirected to - ~S" disp-to))
		 (let ((rstart (// start 2))
		       (roff (// disp-off 2)))
		   (funcall to ':string-out disp-to (+ rstart roff) (+ rstart roff 16len)))))
	      (t 
	       (si:change-indirect-array indirect 'art-16b (list 16len) buf (// start 2))
	       (funcall to ':string-out indirect 0 16len)))
	(funcall from ':discard-current-input-buffer)))))


(defun distribution-load-patch-files (stream story &aux pd)
  (let* ((info (getl story '(:patch-system-directory :patch-version-directory
						     :patch-source-file :patch-object-file)))
	 (sysname (get story ':system))
	 (system (si:find-system-named sysname t)))
    (cond ((null system)
	   (cond ((get story ':patch-system-directory)
		  (format t "~&~A isn't defined as a system in this environment.~@
				     Load up the proper definition and try this ~
				     loading again."))
		 (t (format t "~&Skipping ~A, not defined as a system." (cadr info))))
	   nil)					;I didn't load it.
	  ((null (setq pd (si:system-patch-directory system)))
	   (cond ((get story ':patch-system-directory)
		  (format t "~&~A isn't defined as a patchable system in this environment.~@
				     Load up the proper definition and try this ~
				     loading again."))
		 (t (format t "~&Skipping ~A, not defined as patchable." (cadr info))))
	   nil)					;I didn't load it.
	  (t (let ((patch-path (si:patch-directory-pathname pd))
		   (maj (get story ':patch-version))
		   (min (get story ':patch-minor-version)))
	       (distribution-load-path
		 stream
		 (lexpr-funcall
		   patch-path
		   ':patch-file-pathname sysname (si:patch-directory-same-directory-p pd)
		   (si:patch-directory-patch-atom pd)
		   (selectq (car info)
		     (:patch-system-directory '(:system-directory))
		     (:patch-version-directory (list ':version-directory maj))
		     (t (list ':patch-file maj min (selectq (car info)
						     (:patch-object-file "QFASL")
						     (t "LISP"))))))
		 story))))))			;Return loadpath's value

(defun distribution-load-ucode-file (stream story)
  (or
    (distribution-load-path
      stream
      (funcall (if (get story ':microcode-source)
		   (fs:parse-pathname "sys:ucadr;ucadr")
	       (fs:parse-pathname "sys:ubin;ucadr"))
	       ':new-type-and-version
	       (cond ((get story ':microcode-source) "LISP")
		     ((get story ':microcode-object) "MCR")
		     ((get story ':microcode-symbols) "SYM")
		     ((get story ':microcode-table) "TBL"))
	       (get story ':microcode-version))
      story)
    (distribution-clear-to-eof stream)))

(defun distribution-load-font-file (stream story)
  (or (distribution-load-path
	stream
	(funcall (fs:parse-pathname "sys:fonts;foo qfasl >") ':new-name
		 (get story ':font))
	story)
      (distribution-clear-to-eof stream)))
