;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.11
;;; Reason: The strings "newest" and "oldest" accepted as file versions.
;;; Written 12/18/81 09:34:17 by BSG,
;;; while running on Terrier from band 1
;;; with System 78.16, ZMail 38.2, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.6, Canon 9.0, microcode 840.



; From file FSSTR.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (lmfs-pathname :parse-namestring) (ignore namestring &optional (start 0) end
						     &aux vdindex tdindex pdindex)
  ;; Take off leading and trailing spaces
  (setq start (or (string-search-not-char #\SP namestring start end) start))
  (setq end (1+ (or (string-reverse-search-not-char #\SP namestring end start) (1- end))))
  ;; Figure out where if at all the various delimited fields are
  (setq pdindex (string-reverse-search *PATH-DELIMITER* namestring end start)
	vdindex (string-reverse-search *VERSION-DELIMITER* namestring end start)
	tdindex (string-reverse-search *TYPE-DELIMITER* namestring end start))
  ;; Psych out <, embedded >'s, and other pathnames that may have gotten here by accident.
  (or (= end start)
      (char-equal (character *PATH-DELIMITER*) (aref namestring start))
      (and (char-equal #/< (aref namestring start))
	   (ferror ':lmfs-path-syntax
		   "Pathname intended for other operating system?: ~A" namestring))
      (null pdindex)
      (and (= pdindex (+ start 1))		;*>... ok
	   (char-equal #/* (aref namestring start)))
      (ferror ':lmfs-path-syntax "Embedded ~A, but not an absolute path?: ~A"
	      *PATH-DELIMITER* namestring))
  ;; See if any of these delimiters came in the wrong order, e.g., >a.b>c
  (if (or (and pdindex vdindex (< vdindex pdindex))
	  (and pdindex tdindex (< tdindex pdindex)))
      (ferror ':lmfs-path-syntax
	      "Directory names may not have versions or types: ~A" namestring))
  (if (and tdindex vdindex (= tdindex vdindex))
      (progn
       (setq tdindex (string-reverse-search *TYPE-DELIMITER* namestring vdindex start))
       (if (and tdindex pdindex (< tdindex pdindex))
	   (ferror ':lmfs-path-syntax "Directory names may not have types: ~A" namestring))
       (if (null tdindex) (setq tdindex vdindex vdindex nil))))
  (if (and vdindex tdindex (< vdindex tdindex))
      (ferror ':lmfs-path-syntax "Type and version out of order: ~A" namestring))

  ;Now actually extract the various fields and parse the version
  (let ((t-dir (and pdindex (substring namestring start pdindex)))
	(t-name (substring namestring (1+ (or pdindex (1- start))) (or tdindex vdindex end)))
	(t-type (and tdindex (substring namestring (1+ tdindex) (or vdindex end))))
	(t-version (and vdindex
			(let ((vstart (1+ vdindex)))
			  (cond ((and (> end vstart)
				      (char-equal #/* (aref namestring vstart)))
				 ':wild)
				((string-equal namestring "NEWEST" vstart 0 end nil)
				 ':newest)
				((string-equal namestring "OLDEST" vstart 0 end nil)
				 ':oldest)
				((zwei:parse-number namestring vstart end))
				((ferror
				   ':lmfs-path-syntax "Bad version: must be a number: ~A"
				   namestring)))))))

    ;; Convert wild names
    (cond ((string-equal t-name "") (setq t-name nil))
	  ((string-equal t-name "*") (setq t-name ':wild)))
    (if (string-equal t-dir "*") (setq t-dir ':wild))
    (if (string-equal t-type "*") (setq t-type ':wild))
    ;; Fix a bad crock a poor way.
    (and t-type (stringp t-type) (> (string-length t-type)
				    (directory-entry-file-type-max-length))
	 (setq t-type (substring t-type 0 (directory-entry-file-type-max-length))))
    ;; Special case the ROOT.
    (if (and pdindex (zerop (string-length t-dir))) (setq t-dir *PATH-DELIMITER*))
    ;; May we have the envelope with the answer, please?
    (values ':unspecific t-dir t-name t-type t-version)))

)

; From file FSSTR.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (lmfs-pathname :string-for-dired) ()
  (string-append
    (if (eq fs:name ':wild)
	"*"
	fs:name)
    (cond ((and (memq fs:type '(:unspecific nil))
		(memq fs:version '(:unspecific nil :newest)))	;implied semant.
	   "")
	  ((eq fs:type ':directory) "")		;this is problematic -- running debate.
	  (t (string-append
	       *TYPE-DELIMITER*
	       (cond ((memq fs:type '(:unspecific nil)) "")
		     ((eq fs:type ':wild) "*")
		     (t fs:type))
	       (cond ((memq fs:version '(:unspecific nil :newest)) "")
		     (t *VERSION-DELIMITER*))
	       (cond ((eq fs:version ':newest) "NEWEST")
		     ((eq fs:version ':oldest) "OLDEST")
		     ((memq fs:version '(:unspecific nil)) "")
		     ((eq fs:version ':wild) '*)
		     ((numberp fs:version) (format nil "~D" fs:version))
		     (t fs:version)))))))

)
