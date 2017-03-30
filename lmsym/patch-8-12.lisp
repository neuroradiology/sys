;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.12
;;; Reason: (distribution:map-distribution-tape) to construct map from tape.
;;; Written 1/08/82 13:49:01 by BSG,
;;; while running on Basset from band 5
;;; with System 78.45, ZMail 38.5, Symbolics 8.10, Tape 6.5, LMFS 21.33, Canon 9.11, microcode 841.



; From file distribution-load.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

(defconst *distribution-reloader-keywords*
	  '(((parse-number lmfs:reloader-dec-print)
	     :PART-SIZE :MICROCODE-VERSION :SEQUENCE :NEEDED-VERSION :BYTE-SIZE
	     :TAPE-SYSTEM-VERSION :DISK-UNIT
	     :PATCH-VERSION :PATCH-MINOR-VERSION :MICROCODE-VERSION-DUMPED)
	    ((distribution-inconvert-sexp princ)
	     :UNSPECIFIC-VERSION :TV-FONTS-DUMPED :CHARACTERS)
	    ((fs:parse-directory-date-property lmfs:reloader-time-print)
	     :CREATION-DATE :DUMP-TIME)
	    ((substring princ)
	     :PARTITION :PARTITION-COMMENT :REEL :USER-ID :SITE :MACHINE :SYSTEM
	     :END-OF-TAPE-PROLOGUE-FILES
	     :COMMENT :AUTHOR
	     :DISTRIBUTION-TAPE :OBJECT-FILE :SOURCE-FILE :SOURCE-PATH :SOURCE-PATH-FLAVOR
	     :PATCH-SYSTEM-DIRECTORY :PATCH-OBJECT-DIRECTORY :PATCH-SOURCE-FILE
	     :PATCH-OBJECT-FILE :TV-FONT-FILE
	     :MICROCODE-SOURCE :MICROCODE-OBJECT :MICROCODE-SYMBOLS :MICROCODE-TABLE
	     :BANDS-DUMPED :SYSTEMS-DUMPED)))

)

; From file distribution-load.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

(defun distribution-reloader-get-stream ()	;use 'em special..
 (values
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
	   (return nil))))
   *drldr-Global-reel*))

)

; From file distribution-load.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

(defun map-distribution-tape (&aux s reel prologue)
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
	(with-open-file (*log-file* (distribution-dump-map-path (or reel
								    (get prologue ':reel))
								(get prologue ':dump-time))
				    ':direction ':output)
	  (setq si:.file-aborted-flag. nil)	;DONT delete it.
	  (format t "~&Log file going to ~A." (funcall *log-file* ':truename))
	  ;;the above may not work on ITS, I guess.
	  (format *log-file*
		  "Distribution dump by ~A on ~A (~A) at ~A.~%"
		  (get prologue ':user-id)
		  (get prologue ':machine)
		  (get prologue ':site)
		  (time:print-universal-time (get prologue ':dump-time) nil))
	  (format *log-file* "Reconstruction of map by reading tape ~A at ~A.~%"
		  reel (time:print-universal-time (time:get-universal-time) nil))
	  (format *log-file* "Distribution tape version ~D.~%" (get prologue ':version))
	  (format *log-file* "The given tape reel name was ~A.~%" (get prologue ':reel))
	  (if (and (get prologue ':comment)
		   (not (zerop (string-length (get prologue ':comment)))))
	      (format *log-file* "Comment: ~A~%" (get prologue ':comment)))
	  (format *log-file* "All creation dates Greenwich Mean Time (GMT).~%")
	  (format *log-file* "~%~%~%")

	  (format *log-file*
		  "~3%------------------------- S U M M A R Y -------------------------~3%")
	  (if (get prologue ':tv-fonts-dumped)
	      (format *log-file* "TV Fonts dumped.~%"))
	  (if (get prologue ':microcode-version-dumped)
	      (format *log-file* "Microcode ~D source, object, table, and symbols.~%"
		      (get prologue ':microcode-version-dumped)))
	  (do () (())
	    (let* ((story (distribution-story-reader stream nil))	;don't eof
		   (type (car (getl story '(:bands-dumped :systems-dumped
							  :end-of-tape-prologue-files)))))
	      (selectq type
		(:bands-dumped
	         (loop as line = (funcall stream ':line-in)
		       when (equal line "") return nil
		       as parsed = (distribution-load-parse-tape-band-spec line)
		       if parsed do (format *log-file* "Band, ~A~%" (fourth parsed))))
		(:systems-dumped
	         (loop as line = (funcall stream ':line-in)
		       when (equal line "") return nil
		       as parsed = (distribution-load-parse-tape-system-spec line)
		       if parsed do (format *log-file* "System, ~A~%" (second parsed))))
		(:end-of-tape-prologue-files
		 (distribution-clear-to-eof stream)
		 (return nil))
		(t   (ferror nil "Unrecognized tape file: ~S" story))))
	    (funcall stream ':clear-eof))
	  
	  (loop with last-sys = ':none
		as story = (distribution-story-reader stream)
		if (get story ':end-of-tape)
		return (format *log-file* "~%~%End of tape.~%")
		as system-name = (or (get story ':system)
				     (if (get story ':partition) "Bands"))
		if (not (equal system-name last-sys))
		do (cond ((equal system-name "UCODE")
			  (format
			    *log-file*
			    "~3%-------------------- M I C R O C O D E  ~D --------------------~3%"
			    (get story ':microcode-version)))
			 ((equal system-name "TV-FONTS")
			  (format *log-file* "~3%------------------------ F O N T S ---------------------------~3%"))
			 
			 ((equal system-name "BANDS")
			  (format
			    *log-file*
			    "~3%-------------------------- B A N D S --------------------------~3%"))
			 (t (if (member last-sys '("UCODE" "BANDS" "TV-FONTS" :none))
				(format
				  *log-file*
				  "~3%------------------------ S Y S T E M S ------------------------~3%"))
			    (format *log-file* "~%System:  ~A~2%" system-name)))
		     (setq last-sys system-name)
		if (get story ':partition)
		do (format *log-file* "BAND:   ~A (~D blocks) from ~A Drive ~D: ~A~%"
			   (get story ':partition)
			   (get story ':part-size)
			   (get story ':machine)
			   (get story ':disk-unit)
			   (get story ':partition-comment))
		else do
		     (format *log-file*
			     "   ~26A  ~30A ~8A ~A~%"
			     (cadr (getl story
				 '(:tv-font-file
				 :source-file :object-file
				 :patch-system-directory :patch-version-directory
				 :patch-source-file :patch-object-file
				 :microcode-source :microcode-object
				 :microcode-symbols :microcode-table)))
			     (get story ':source-path)
			     (get story ':author)
			     (time:print-universal-time (get story ':creation-date) nil 0))
		     do (distribution-clear-to-eof stream))))))

)

; From file distribution-dump.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

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
	    (format *log-file* "All creation dates Greenwich Mean Time (GMT).~%")
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

)

