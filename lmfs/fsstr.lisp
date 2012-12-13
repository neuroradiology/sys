;;; <LMFS>FSSTR.LSP;1 15-May-81 09:02:58, Ed by BSG -*-Package:LMFS; Mode:LISP; Lowercase:T-*-
;;; Copyright (c) Symbolics, Inc., 1981

;;; External (flavor instance) interface to file system, high
;;; level behaviors.

(defvar *FILE-SYSTEM-LOCK* (ncons nil))
(defvar *LOCKPTR* (locf (car *FILE-SYSTEM-LOCK*)))
(defvar *WRITE-BEHIND* T)
(defconst *WRITE-BEHIND-COUNT* 4)

;;; A word on locking policy ..
;;;  #O01451377777
;;;
;;; No, seriously, folks... There is one lock.  It is declared above. It locks
;;; the whole file system. Most importantly, any hacking around with any datastructure
;;; other than LMFS openings. It is only locked in this program.  Note that :tyi, :tyo,
;;; etc. do not have to lock -- openings and reference-counted buffers and addressors
;;; aren't going to go away.   :establish-addressor, :open, and :close are the important
;;; ones to be into locking.

;;;Change this when, if ever, unwind-protect returns multiple values through.
(defmacro with-fs-locked-mv body
  `(let (v1 v2 v3 v4 v5)
     (unwind-protect
       (multiple-value (v1 v2 v3 v4 v5)
	 (progn (process-lock *LOCKPTR*)
		. ,body))
       (if (eq (car *FILE-SYSTEM-LOCK*) current-process) (process-unlock *LOCKPTR*)))
     (values v1 v2 v3 v4 v5)))

(defmacro with-fs-locked body
  `(unwind-protect
     (progn (process-lock *LOCKPTR*)
	    . ,body)
     (if (eq (car *FILE-SYSTEM-LOCK*) current-process) (process-unlock *LOCKPTR*))))

;;; Highly variable addressing arrays generated here.

(defresource fs-data-addressor ()
  :constructor
  (make-array 2 ':type 'art-8b ':displaced-to 0 ':displaced-index-offset 0))

(declare (special *PATH-DELIMITER* *VERSION-DELIMITER* *TYPE-DELIMITER*))

(defconst *LOCAL-FS-HOST* '"local")
(defconst *DEFAULT-BINARY-FILE-BYTE-SIZE* 16.)
(defconst *DEFAULT-CHAR-FILE-BYTE-SIZE* 8.)



(defflavor lmfs-pathname ()
	   (fs:meaningful-root-mixin)
  (:included-flavors fs:pathname))

(defmethod (lmfs-pathname :before :init) (&rest ignore)
  (if (eq fs:directory ':root)
      (setq fs:directory *path-delimiter*)))

(defmethod (lmfs-pathname :string-for-printing) ()
  (string-append
    (cond ((eq fs:host si:local-host) "")
	  (t (string-append (funcall fs:host ':name-as-file-computer) ":")))
    (funcall-self ':string-for-host)))

(defmethod (lmfs-pathname :init-file) (program-name)
  (funcall-self ':new-pathname ':name program-name
	     ':type "init" ':version ':newest))
	     
(defmethod (lmfs-pathname :homedir) ()
  (funcall-self ':new-pathname ':directory (string-append *PATH-DELIMITER* user-id)
		':name ':nil ':type nil ':version nil))

(defmethod (lmfs-pathname :string-for-editor) ()
  (string-append
    (send-self ':string-for-dired)
    " " fs:directory " " (funcall fs:host ':name-as-file-computer) ":"))

(defmethod (lmfs-pathname :string-for-host) ()
  (let ((ename (if (and (null fs:name) (null fs:type) (null fs:version))
		   ""
		   (funcall-self ':string-for-dired))))
    (cond ((eq fs:directory ':wild)
	   (string-append "*" *path-delimiter* ename))
	  ((null fs:directory) ename)
	  ((string-equal fs:directory *path-delimiter*)
	   (string-append *path-delimiter* ename))
	  (t (string-append fs:directory *path-delimiter* ename)))))

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
	       (cond ((memq fs:version '(:unspecific nil :newest)) "")
		     ((eq fs:version ':wild) '*)
		     ((numberp fs:version) (format nil "~D" fs:version))
		     (t fs:version)))))))


(defmethod (lmfs:lmfs-pathname :condense-directory) (&rest whats)
  	;;>a>b c :directory -> >a>b>c ()
  (funcall-self ':new-pathname
		':directory
		(if (equal fs:directory *PATH-DELIMITER*)
		    (string-append fs:directory fs:name)
		    (string-append fs:directory *PATH-DELIMITER* fs:name))
		':name (get (locf whats) ':name)
		':type (get (locf whats) ':type)
		':version (get (locf whats) ':version)))

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
			(if (and (> end (1+ vdindex))
				 (char-equal #/* (aref namestring (1+ vdindex))))
			    ':wild
			    (or (zwei:parse-number namestring (1+ vdindex) end)
				(ferror
				  ':lmfs-path-syntax "Bad version: must be a number: ~A"
				  namestring))))))

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

(defmethod (lmfs-pathname :patch-file-pathname) (name same-directory-p patom type &rest args)
  (selectq type
    (:system-directory
     (funcall-self ':new-pathname ':name (if same-directory-p patom name)
		   ':type (if same-directory-p "DIRECTORY" (format nil "~a-DIR" patom))
		   ':version ':newest))
    (:version-directory
     (funcall-self ':new-pathname ':name (format nil "~:[~a~*~;~*~a~]-~d" same-directory-p
						 name patom (car args))
		   ':type (if same-directory-p "DIRECTORY" (format nil "~a-DIR" patom))
		   ':version ':newest))
    (:patch-file
     (funcall-self ':new-pathname ':name (format nil "~:[~a~*~;~*~a~]-~d-~d" same-directory-p
						 name patom (car args) (cadr args))
		   ':type (caddr args) ':version ':newest))))

;;;
;;;  Messages to support new protocol that makes FSEdit work on any host
;;;


(defmethod (lmfs-pathname :string-for-directory) ()
  fs:directory)					;should work fine

(defmethod (lmfs-pathname :pathname-as-directory) ()
  (fs:make-pathname
    ':host fs:host
    ':directory
    (if (equal fs:directory *PATH-DELIMITER*)
	(string-append fs:directory fs:name)
	(string-append fs:directory *PATH-DELIMITER* fs:name))
    ':name ':unspecific
    ':type ':unspecific
    ':version ':unspecific))

(defmethod (lmfs-pathname :directory-pathname-as-file) (&aux delimx)
  (cond ((not (stringp fs:directory)) (ferror nil "Can't make pathname as file - ~A" self))
	((string-equal fs:directory "")
	 (ferror nil "Pathname has no real directory - ~A" self))
	((string-equal fs:directory *path-delimiter*)		;the ROOT
	 (fs:make-pathname ':host fs:host ':directory "" ':name "" ':type ':directory
			   ':version 1))
	((null (setq delimx (string-reverse-search-char (character *path-delimiter*)
							fs:directory)))
	 (ferror nil "No ~A in directory name?" *path-delimiter*))
	((zerop delimx)				;off of root
	 (fs:make-pathname ':host fs:host ':directory *path-delimiter*
			   ':name (substring fs:directory 1) ':type ':directory ':version 1))
	(t
	 (fs:make-pathname
	   ':host fs:host
	   ':directory (substring fs:directory 0 delimx)
	   ':name (substring fs:directory (1+ delimx)) ':type ':directory ':version 1))))


;;;
;;; Local LMFS pathnames, thus dealing with file system semantics as opposed
;;; to syntax.
;;;

(defflavor local-lmfs-pathname () (lmfs-pathname))

(defun default-null-file-type (ftype)
  (cond ((memq ftype '(nil :unspecific)) "")
	((symbolp ftype) ftype)
	;; One day we do this right?
	((> (string-length ftype) (directory-entry-file-type-max-length))
	 (substring ftype 0 (directory-entry-file-type-max-length)))
	(t ftype)))

(defmethod (local-lmfs-pathname :delete)  (&optional error-p)
  (multiple-value-bind (fd err)
      ;; First, lay hands on the file.
      (with-fs-locked-mv
	(get-fd-from-dn-en fs:directory fs:name (default-null-file-type fs:type)
			   (or fs:version ':oldest)))
    (cond
      ;; Connection failure? Delete via special kludge if so
      ((and err (string-equal (substring err 4 13.) "ERROR CNF"))
       (with-fs-locked
	 (dcf fs:directory fs:name fs:type fs:version)	;crocque in "COMMANDS"
	 (setq err nil)))
      ;; Other kind of error?
      (err (if error-p
	       (ferror ':lmfs-operation-error ":delete - ~A~%~A" self err)
	       err))
      ;; Delete-through link? Send target the delete msg if so.
      ((and (fd-link-p fd) (memq ':delete (with-fs-locked (get-link-transparencies fd))))
       (funcall (fs:parse-pathname (fd-link-p fd) *LOCAL-FS-HOST*) ':delete))
      (t
       ;;Ok, try to delete it.
       (with-fs-locked
	 ;; Directory with stuff in it?
	 (cond ((and (eq (fd-file-type fd) ':directory)
		     (dir-not-empty-p fd))
		(if error-p
		    (ferror ':lmfs-operation-error ":delete- ~A is not empty" self)
		    (format nil "FHN ERROR DNE F The directory ~A is not empty" self)))
	       (t
		;;OK, "delete" the damned thing
		(update-file-attributes fd ':delete)
		;; If we deleted a directory, deactivate it so PATHS won't find it next time.
		(if (eq (fd-file-type fd) ':directory)
		    (check-deactivate-file fd))
		t)))))))			;This is the expected result for winnage.

(defmethod (local-lmfs-pathname :rename) (new-name &optional error-p)
  (or (typep-2 new-name 'local-lmfs-pathname)
      (ferror nil "Argument to :rename must be a pathname for this host - ~S."
	      new-name))
  (multiple-value-bind (fd err)
    (with-fs-locked-mv
      (get-fd-from-dn-en fs:directory fs:name (default-null-file-type fs:type) fs:version))
    (if (null err)
	(if (and (fd-link-p fd) (memq ':rename (with-fs-locked
						 (get-link-transparencies fd))))
	    (funcall (fs:parse-pathname (fd-link-p fd) *LOCAL-FS-HOST*)
		     ':rename new-name error-p)
	    (multiple-value-bind (dir err)
	      (with-fs-locked-mv
		(get-filedesc-from-path (funcall new-name ':directory) ':directory 1))
	      (if (null err)
		  (setq err (with-fs-locked
			      (rename-file-dfd
				fd
				dir
				(funcall new-name ':name)
				(default-null-file-type (funcall new-name ':type))
				(funcall new-name ':version))))))))
    (cond (err
	   (if (eq err ':create-through-link-magic)
	       ;; The gut level of the Creator found a create-thru-link
	       (progn					;internal procs would be nice here..
		 (with-fs-locked (check-deactivate-file fd))
		 (funcall-self
		   ':rename (fs:parse-pathname (fd-link-p fd) *LOCAL-FS-HOST*) error-p)))
	   (if error-p
	       (ferror ':lmfs-operation-error
		       ":rename - ~A to ~A~%~A" self new-name err)
	       err))
	  (t))))


(defmethod (local-lmfs-pathname :directory-list) (options)
  (if (memq ':no-extra-info options) (push ':no-dir-info options))
  (if (eq fs:directory ':wild)
      (list-all-directories self options)
    (multiple-value-bind (dirlist error)
	(with-fs-locked-mv
	  (directory-list  fs:directory
			   (if (member fs:name '("*" * :wild)) ':unspecific fs:name)
			   (if (member fs:type '("*" * :wild)) ':unspecific
			     (default-null-file-type fs:type))
			   (if (member fs:version '("*" * :wild :newest :oldest nil))
			       ':unspecific fs:version)
			   options))
      
      (if error
	  (if (memq ':noerror options)
	      error
	    (ferror ':lmfs-operation-error "Can't list dir ~A :~A" self error))
	(cond ((memq fs:version '(:newest :oldest))
	       ;; Wants only newest or oldest versions, we got them all, sort what we got.
	       (rplacd dirlist (sortcar (cdr dirlist)
					#'(lambda (x y)
					    (cond ((not (equal (first x) (first y)))
						   (string-lessp (first x) (first y)))
						  ((not (equal (second x) (second y)))
						   (string-lessp (second x) (second y)))
						  (t (< (third x) (third y)))))))
	       (do ((list (cdr dirlist))) ((null (cdr list)))
		 (let ((this (car (first list)))
		       (next (car (second list))))
		   ;; Found multiple versions of the same file
		   (cond ((and (equal (first this) (first next))
			       (equal (second this) (second next)))
			  ;; If want newest, flush this element, next one is
			  ;; newer.
			  (if (eq fs:version ':newest)
			      (setf (first list) (second list)))
			  ;; Flush an element.
			  (rplacd list (cddr list)))
			 (t
			  ;; Otherwise try next
			  (setq list (cdr list))))))
	       ))
	(mapc #'(lambda (x)
		  (and (car x)			;skip nil element
		       (let* ((flist (car x))
			      (fname (first flist))
			      (ftype (second flist))
			      (fversion (third flist)))
			 (rplaca x (fs:make-pathname
				     ':host fs:host
				     ':directory fs:directory
				     ':device fs:device
				     ':name fname ':type ftype ':version fversion)))))
	      dirlist)
	(if (memq ':sorted options)
	    (rplacd dirlist (sortcar (cdr dirlist) #'fs:pathname-lessp)))
	dirlist))))

(defun list-all-directories (path options)	;the path gives other components
  (let ((answer (*catch
		  'list-all-directories
		  (let ((subanswer (list-all-directories-recurse
				     (funcall path ':new-directory ':root)
				     options)))
		    (list* (global-file-props *the-root*) subanswer)))))
    (if (stringp answer)
	(if (memq ':noerror options)
	    answer
	  (ferror nil "Can't list all directories - ~A" answer))
      (if (memq ':sorted options)
	  (rplacd answer (sortcar (cdr answer) #'fs:pathname-lessp)))
      answer)))

(declare-flavor-instance-variables (local-lmfs-pathname)
(defun list-all-directories-recurse (path options)
  (let ((local-list (funcall path ':directory-list options)))
    (if (stringp local-list)
	(*throw 'list-all-directories local-list))
    (let ((subdir-list (funcall (funcall path ':new-pathname
					 ':name ':wild ':type ':directory ':version ':wild)
				':directory-list '(:noerror :no-dir-info))))
      (if (stringp subdir-list)
	  (*throw 'list-all-directories subdir-list))
      (loop for (path) in (cdr subdir-list)
	    nconc (list-all-directories-recurse
		    (funcall (funcall path ':pathname-as-directory)
			     ':new-pathname ':name fs:name ':type fs:type
			     ':version fs:version)
		    options)
	    into result
	    finally (return (nconc (cdr local-list) result)))))))

;;;In want of chaos protocol on this.......
(defmethod (local-lmfs-pathname :list-dir-no-subdir-info) (&rest args)
  (funcall-self ':directory-list (cons ':no-dir-info args)))

(defmethod (local-lmfs-pathname :properties) (&optional (error-p t))
  (multiple-value-bind (result changeables)
      (with-fs-locked-mv (file-properties fs:directory fs:name fs:type fs:version))
    (if (stringp result)
	(if error-p
	    (ferror ':lmfs-operation-error "Cannot get properties for ~A: ~A" self result)
	    result)
	(values (rplaca result self) changeables))))

(defmethod (local-lmfs-pathname :change-properties) (error-p &rest props &aux dir-deleting
							     (err1 t))
  (and (eq fs:type ':directory)
       (tv:doplist ((locf props) prop ind)
	 (if (and (eq prop ':deleted) ind) (setq dir-deleting t))))
  ;;The implementation of dir deleting here is not optimal.  Too bad. Not common op
  ;;use :delete yourself if you want efficiency.  The issue here is deactivating
  ;;the dir after delete so the stuff in PATHS fails to find it.
  (if dir-deleting (setq err1 (funcall-self ':delete error-p)))
  (if (stringp err1)
      err1					;if error-p :delete blew up itself...
      (let ((result fs:(lmfs:with-fs-locked
			 (lmfs:change-file-properties
			   directory name type version lmfs:props))))
	(if (stringp result)
	    (if error-p
		(ferror ':lmfs-operation-error "Cannot change properties for ~A: ~A" self result)
		result))
	(if dir-deleting (funcall self ':delete error-p))	;deactivate it.
	t)))

(defmethod (local-lmfs-pathname :complete-string) (string options)
  (let ((newparse (lmfs-parse-for-completer string)))
    (if (null newparse)
	(values string nil)			;bad syntax
	(multiple-value-bind (string flag)
	    (funcall (fs:merge-pathname-defaults newparse self)
		     ':complete-myself string options)
	  (let ((host-name (funcall fs:host ':name-as-file-computer)))
	    (if (string-equal host-name "UNKNOWN")
		(values string flag)
		(values (string-append host-name ":" string) flag)))))))

(defun lmfs-parse-for-completer (string)
  (*catch 'completer-lose
    (condition-bind ((:lmfs-path-syntax #'(lambda (&rest ignore)
					    (*throw 'completer-lose nil))))
      (fs:parse-pathname string si:local-host))))

(defmethod (local-lmfs-pathname :complete-myself) (string options)
  (multiple-value-bind (dirfd err)
      (with-fs-locked-mv (get-filedesc-from-path fs:directory ':directory 1))
      (if err
	  (values string nil)
	  (let ((namels (with-fs-locked
			  (scan-dir-for-completion
			    dirfd
			    (if (symbolp fs:name) nil fs:name)
			    (if (symbolp fs:type) nil fs:type)
			    options))))
	    (loop for namel in namels do
		  (cond ((eq (second namel) ':directory)
			 (setf (first namel)
			       (string-append (first namel) *path-delimiter*)))))
	    (selectq (length namels)			    
	      (0 (values string nil))
	      (1 (values (namel-to-string-for-completer self (car namels) options)
			 (if (memq ':read options) ':old ':new)))
	      (t (values (namel-maximize namels self options) nil)))))))


(defun namel-to-string-for-completer (instance namel options)
  (let ((newpn (funcall instance ':new-pathname
			':name (car namel)
			':type (if (eq (cadr namel) ':directory) nil (cadr namel))
			':version (if (memq ':new options) ':newest nil))))
    (funcall newpn ':string-for-host)))


(defun namel-maximize (namels instance options)
  (namel-to-string-for-completer instance
				 (list (namel-maximize-1 #'car namels)
				       (namel-maximize-1 #'cadr namels))
				 options))

(defun namel-maximize-1 (fcn namels)
  (let ((maxl 0))
    (dolist (namel namels)
      (setq maxl (max maxl (string-length (funcall fcn namel)))))
    (let ((string (make-array maxl ':type 'art-string ':leader-list '(0))))
      (copy-array-contents (funcall fcn (car namels)) string)
      (dolist (namel namels)
	(let ((test-string (funcall fcn namel)))
	  (if (symbolp test-string) (return nil))
	  (let ((sl (string-length test-string)))
	    (dotimes (i (min maxl sl))
	      ;; hardware redisplay fcn wanted
	      (if (not (char-equal (aref string i) (aref test-string i)))
		  (return (setq maxl i)))))))
      (store-array-leader maxl string 0)
      string)))

(defmethod (local-lmfs-pathname :open) (path &rest args)
  (open-local-lmfs self path args))



;;; Local pathname messages to manage local file system in ways that only
;;; work for local file-system.
;;; BSG 9 Aug 1981

(defmethod (local-lmfs-pathname :create-directory) (&optional
						     (err-p t)
						     &rest
						     options &aux (plist (locf options))
						     (usid (select user-id
							     (NIL "")
							     (""  "")
							     (T (string user-id)))))
  (let ((err
	  (let* ((dname (funcall-self ':directory))	;until namespaces.....
		 (partid (get plist ':partition))
		 (part (if partid (with-fs-locked (find-partition-from-part-id partid)))))	;errs
	    (if (string-equal *PATH-DELIMITER* dname)	;this is the ROOT
		(if *the-root*
		    "FHN ERROR DAX F The root already exists."
		    (if (null part)
			"FHN ERROR NPT F A partition must be specified when creating the root."
			(with-fs-locked
			  (setq *the-root*
				(create-directory part nil "root" ':author usid)))))
		(let* ((split (fs:parse-pathname dname *LOCAL-FS-HOST*))
		       (pardir (funcall split ':directory))
		       (fname (funcall split ':name)))
		  (if part
		      (with-fs-locked
			(multiple-value-bind (fd err)
			    (create-dir-from-path part pardir fname ':author usid)
			  (if (null err)
			      (check-deactivate-file fd))
			  (or err t)))
		      (with-fs-locked
			(multiple-value-bind (pfd perr)
			    (get-filedesc-from-path pardir ':directory 1)
			  (or perr
			      (multiple-value-bind (fd err)
				  (create-directory (fd-partition-desc pfd) pfd fname
						    ':author usid)
				(if (null err)
				    (check-deactivate-file fd))
				(or err t)))))))))))
    (if (stringp err)
	(if err-p
	    (ferror ':lmfs-operation-error "Cannot create ~A - ~%~A"
		    (funcall-self ':directory) err)
	    err)
	t)))

(defmethod (local-lmfs-pathname :create-link) (linkto-path &optional transparencies)
  (cond ((or (null fs:type) (null fs:name))
	 (format nil "FHN ERROR BLK F Incompletely specified link: ~A" self))
	((not (and (typep linkto-path 'local-lmfs-pathname)
		   (funcall linkto-path ':directory)
		   (not (memq ':unspecific (list (funcall linkto-path ':directory)
						 (funcall linkto-path ':name)
						 (funcall linkto-path ':type)
						 (funcall linkto-path ':version))))
		   (not (null (funcall linkto-path ':type)))
		   (not (null (funcall linkto-path ':name)))
		   (or (null (funcall linkto-path ':version))
		       (numberp (funcall linkto-path ':version)))))
	 (format nil "FHN ERROR BLP F Unacceptable pathname for link: ~A" linkto-path))
	((with-fs-locked
	   (multiple-value-bind (fd err)
	       (create-file-from-path fs:directory fs:name fs:type (or fs:version ':newest)
				      ':author (string user-id)
				      ':link (funcall linkto-path ':string-for-host))
	     (if (null err)
		 (progn
		   (if transparencies
		       (set-link-transparencies fd transparencies)
		       (check-deactivate-file fd))))
	     (or err t))))))



;;; Following  guy operates upon his DIRECTORY component.

(defmethod (local-lmfs-pathname :expunge) (&rest options &aux (error-p t))
  (tv:doplist (options prop ind)
    (selectq ind
      (:error  (setq error-p prop))))
  ;; Returns 2 values, records gotten and list of errors.
  (multiple-value-bind (recs errs)
      (with-fs-locked-mv (expunge-dir-path fs:directory))
      (if (fixp recs)
	  (values recs errs)			;appears to have worked....
	  (if error-p
	      (ferror nil "Expunge error - ~A - ~A" self recs)
	      recs))))
      
(compile-flavor-methods lmfs-pathname local-lmfs-pathname)


(defmacro validate-addressor ()				;for speed
  `(or (and cur-addressor
	    (not (zerop bytes-left-in-block)))
       (send-self ':establish-addressor)))

(defflavor lmfs-opening-mixin
  (file							;the filesys fd- object
   options						;random open options
   mode							;:read, :write, :probe
   real-pathname

   cur-buffer						;in canibus veritas
   cur-addressor
   cur-data-addressor
   cur-byte-offset
   cur-byte-address
   cur-byte-addressor-base
   cur-word-addressor-base

   expected-next-address			        ;implicit sys75 variable
   given-byte-size
   using-byte-size					;power of 2
   bytes-left-in-block
   bytes-in-file
   original-byte-length)				;for append mode openings

  ()
  (:initable-instance-variables real-pathname options mode given-byte-size file)
  (:included-flavors si:file-stream-mixin))


(defflavor lmfs-char-input-opening ()
	   (lmfs-opening-mixin
	    si:input-file-stream-mixin si:buffered-input-character-stream))

(defflavor lmfs-char-output-opening ()
	   (lmfs-opening-mixin
	    si:output-file-stream-mixin si:buffered-output-character-stream))

(defflavor lmfs-binary-input-opening ()
	   (lmfs-opening-mixin
	    si:input-file-stream-mixin si:buffered-input-stream))

(defflavor lmfs-binary-output-opening ()
	   (lmfs-opening-mixin
	    si:output-file-stream-mixin si:buffered-output-stream))


(defflavor lmfs-full-probe-opening ()
	   (si:file-stream-mixin lmfs-opening-mixin))	;no base flavor, but
	   ;;instantiateable nevertheless

(defflavor lmfs-dumper-opening-mixin ()		;who-line behavior for dumper
	   ()
  (:required-flavors lmfs-opening-mixin))

(defflavor lmfs-dumper-opening ()
	   (lmfs-dumper-opening-mixin lmfs-opening-mixin))

;;; What probe-openings return...
;;; Currently obsolete "AGAIN" 8/11/81, but I'm not deleting it yet.
;;; "Reusing openings is like looking for a gas leak with a lighted match" -BSG
;;; Resurrected "AGAIN" 10/21/81, but this time, not as a technique for
;;; reusing large openings, but rather, for avoiding them.

(defflavor probe-pseudoopening () (si:file-stream-mixin si:property-list-mixin))

(defmethod (probe-pseudoopening :truename) ()
  (funcall-self ':get ':truename))

(defmethod (probe-pseudoopening :qfaslp) ()
  (funcall-self ':get ':qfaslp))

(defmethod (probe-pseudoopening :length) ()
  (funcall-self ':get ':length))

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
		 (if (not (memq byte-size '(nil :default)))
		     (ferror
		       nil "Byte size specification only valid for binary opening - ~A"
		       pathname))))

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

;;; Fast probe 21 Oct 1981
;;; Just check out the directory
(defunp fast-probe (pathname logpath deleted-p)
  (let ((dir (funcall pathname ':directory))
	(name (funcall pathname ':name))
	(version (funcall pathname ':version))
	(type (funcall pathname ':type)))
    (if (memq type '(:unspecific nil)) (setq type ""))
    (if (and (eq type ':directory) (equal dir "") (equal name ""))
	(return nil))					;go slow for the root
    (if (or (not (stringp dir)) (not (stringp name))
	    (and (not (stringp type)) (neq type ':directory)))
	(ferror nil "Non-strings in pathname at probe time ~A" pathname))
    (multiple-value-bind
      (nversion length creation-date)
	(with-fs-locked-mv
	  (fast-probe-info-getter dir name type version deleted-p))
      (return
	(cond ((null nversion) nil)
	      ((stringp nversion) nversion)
	      (t
	       (make-instance 'probe-pseudoopening
			      ':pathname logpath
			      ':property-list
			        (list ':truename (funcall pathname ':new-version nversion)
				      ':length length
				      ':creation-date creation-date))))))))

(defmethod (lmfs-opening-mixin :check-new-mode) (new-mode)
  (cond ((eq mode ':closed) nil)
	((eq mode ':dumper) nil)
	((eq new-mode ':dumper) nil)
	((eq new-mode ':probe) nil)
	((eq mode ':probe) nil)
	((memq mode '(:write :append))
	 (format nil "FHN ERROR FAO F File is already open for writing - ~A" real-pathname))
	((memq new-mode '(:write :append))
	 (format nil "File is already open, may not open for writing - ~A" real-pathname))
	(t nil)))


(defun lmfs-open-file (pathname logpath mode deleted-p ftype byte-size options)
  (prog (fd err one-out dpath ename type version err1 rename-pending)
    ;; Validate the byte size.

    (cond ((memq byte-size '(:default nil))
           (cond ((eq mode ':write)
		  (if (memq ':binary options)
		      (setq byte-size *DEFAULT-BINARY-FILE-BYTE-SIZE*)
	    (setq byte-size *DEFAULT-CHAR-FILE-BYTE-SIZE*)))
		 ((setq byte-size nil))))	;Canonicalize :default
	  ((or (< byte-size 1)
	       (> byte-size 16.))
	   (return (format nil "FHN ERROR IBS F Invalid byte size: ~D" byte-size))))


    (setq one-out nil)					;to loop is stupid
tryagain
    (setq dpath (send pathname ':directory)
	  ename (send pathname ':name)
	  type (default-null-file-type (send pathname ':type))
	  version (or (send pathname ':version) ':newest))

    (if (not (stringp dpath)) (ferror nil "Directory is not a string - ~S" dpath))
    (if (not (stringp ename)) (ferror nil "File name is not a string - ~S" ename))
    (if (not (stringp type)) (ferror nil "File type is not a string - ~S" type))
    (if (not (or (memq version '(:newest :oldest)) (numberp version)))
	(ferror nil "File version not a number or keyword - ~S" version))

tryagain-with-falsified-names
    (cond
      ((eq mode ':write)			;File creating case.
       (multiple-value (fd err)
	 (create-file-from-path dpath (string ename) (string type) version
				':author (string user-id)
				':characters (eq ftype ':ascii)
				':byte-size byte-size))
       (setq err1 err)			;this error  is probably better..
	  
       ;;Res Ignobilis Creata E Regulis Cannonica Arte Resoluta (=thx to hic...)

       (cond ((eq err ':create-through-link-magic)
	      ;; The gut level of the Creator found a create-thru-link
	      (setq pathname (fs:parse-pathname (fd-link-p fd) *LOCAL-FS-HOST*))
	      (check-deactivate-file fd)
	      (setq err1 nil)
	      (go tryagain)))))

    ;;if  not create case..... OR create failed -- the above if falls thru
    ;; if it successfully creates, or errs other than the magic kludge -

    (if (or err (not (eq mode ':write)))
	(progn

	 ;; Attempt to grab the file to prove it exists for the pleasure
	 ;; or displeasure (:write) of the particular opening.

	 (multiple-value (fd err)
	   (get-fd-from-dn-en dpath ename type version
			      ':check-write-openings
			      (if deleted-p ':deleted)))

	 ;; Link case - File exists already but is a link.  If link has appropriate
	 ;; transparencies, chase it. Otherwise, it's a namedup.

	 (if (and (null err)			;don't even bother...
		  (fd-link-p fd)		;if link, and
		  (or				;appropriate to chase for this mode
		   (memq (cdr (assq mode '((:read . :read)
					   (:write . :create)
					   (:append . :write))))
			 (get-link-transparencies fd))
		   (and (eq mode ':probe) (not (memq ':dont-chase-links options)))))
	     (progn				;chase the link.
	      (setq pathname (fs:parse-pathname (fd-link-p fd) *LOCAL-FS-HOST*))
	      (check-deactivate-file fd)
	      (go tryagain)))

	 ;; File exists already for create (:write), not a create-through link

	 (if (and (eq mode ':write) (not err))
	     (cond ((memq ':namedup-map-newest options)	;take wanted error action
		     (setq pathname (send pathname ':new-pathname ':version ':newest))
		     (check-deactivate-file fd)
		     (go tryagain))
		    ((and (or (memq ':namedup-rename-dup options)
			      (memq ':namedup-rename-dup-delete options))
			  (not one-out))

		     ;;Real excitement here.  Gonna hack around with names at close time.
		     ;;In the meantime, "rename" this file (to be created) magically by
		     ;;mucking over the local name vars -- close method will switch names
		     ;;and do whate'er deletion needs be done.

		     (setq ename (format nil "~A
~A
~D" ename type version) type "temp"
			   version ':newest)
		     (check-deactivate-file fd)
		     (push ':close-rename-pending options)
		     (setq one-out t rename-pending t)
		     (go tryagain-with-falsified-names))
		    (t 
		      (check-deactivate-file fd)
		      (return "FHN ERROR FAX F File already exists"))))))
    
    (if (or err1 err) (return (or err1 err)))
	
    (cond ((and (fd-link-p fd) (not (memq mode '(:probe :dumper))))
	   (check-deactivate-file fd)
	   (return (format nil "~A is a link, which is not transparent for ~A,~%and thus cannot be opened for this operation."
			   pathname mode))))

    ;; OK, file found and really exists.  Go set up a real opening, return it when done.


    ;; See if other openings are enthusiastic about their baby brother.

    (setq err (dolist (opening (fd-openings fd))
		(let ((answer (send opening ':check-new-mode mode)))
		  (if answer (return answer)))))
    (if err (return  err))

    (or rename-pending
	(setq pathname (send pathname ':new-pathname
			     ':name (fd-file-name fd)
			     ':type (setq type (fd-file-type fd))
			     ':version (setq version (fd-file-version fd)))))
    
    ;;; Implement the poor man's qfaslp.  The idea here (system 77 protocol) is to conclude
    ;;; binariness on our own when ftype is ':default.  The only person who should ever
    ;;; care is LOAD.  Now only 8 bit files can be non-binary.  So we will say that
    ;;; ALL non-8 bit files are binary. This, of course, is false, since there could
    ;;; be 8-bit binary files.  In an upcoming version, we will maintain characterness.
    ;;; For now, byte size will do fine, and this should never confuse LOAD.

    (and (eq ftype ':default) ( (fd-byte-size fd) 8.) (push ':binary options))
    
    (return
      (make-instance
	(selectq mode
	  (:read
	   (if (memq ':binary options)
	       'lmfs-binary-input-opening
	       'lmfs-char-input-opening))
	  ((:write :append)
	   (if (memq ':binary options)
	       'lmfs-binary-output-opening
	       'lmfs-char-output-opening))
	  (:dumper 'lmfs-dumper-opening)
	  (t 'lmfs-full-probe-opening))
	':pathname logpath
	':real-pathname pathname
	':file fd
	':mode mode
	':options options
	':given-byte-size byte-size))))

(defmethod (lmfs-opening-mixin :before :init) (&rest ignore)
  (selectq mode
    ((:probe :read)
     (setq bytes-in-file (fd-byte-length file) cur-byte-address 0))
    (:write  (setq bytes-in-file 0 cur-byte-address 0))
    (:append (setq bytes-in-file (fd-byte-length file)
		   cur-byte-address bytes-in-file
		   original-byte-length cur-byte-address))
    (:dumper (setq bytes-in-file (fd-addressible-length file)
		   cur-byte-address 0))))

(defmethod (lmfs-opening-mixin :after :init) (&rest ignore)
  
  (if (eq mode ':append)
      ;;better tell stream stuff!
      (funcall-self ':set-output-pointer-base bytes-in-file))
  (if (null given-byte-size) (setq given-byte-size (fd-byte-size file)))
  ;; Although it better be true that stored bytesize is canonical...
  (setq using-byte-size (canonicalize-byte-size given-byte-size))
  
  (setq cur-buffer nil)
  (setq cur-addressor nil)
  (setq expected-next-address cur-byte-address)
  (setf (fd-file-map-addenda file)		;inc update opt orred in.
	(or (fd-file-map-addenda file)
	    (not (null (memq ':incremental-update options)))))
  (setq cur-data-addressor (allocate-resource 'fs-data-addressor))	;probe needs qfaslp
  
  (push self (fd-openings file))
  (if (not (eq mode ':probe))
      (progn
	(if (fd-header-buffer file)		;15 Oct 81
	    (upreference-file-buffer (fd-header-buffer file))
	    (setf (fd-header-buffer file) (get-buffer-given-fd-and-wordaddr t file 0)))
	(if (not (memq ':dont-update-reference-time options))
	    (update-file-attributes file ':date-time-used)))))

(defun canonicalize-byte-size (size)
  (if (member size '(1 2 4 8. 16.))
      size
      (expt 2 (haulong size))))

(defmethod (lmfs-opening-mixin :truename) () real-pathname)
(defmethod (lmfs-opening-mixin :namestring) () (send real-pathname ':string-for-printing))
(defmethod (lmfs-opening-mixin :get-lmfs-fd) () file)
(defmethod (lmfs-opening-mixin :length) () bytes-in-file)
(defmethod (lmfs-opening-mixin :creation-date) () (fd-date-time-created file))

(defmethod (lmfs-opening-mixin :delete) (&optional error-p)
  error-p						;can't see how this can work...
  (if (eq mode ':closed)
      (ferror nil "File must be open to delete -- ~A" real-pathname))
  (or (memq ':delete options) (push ':delete options)))




(defmethod (lmfs-opening-mixin :establish-addressor) ()

  ;; We were called because either
  ;;   (1) There is no current addressor. Includes just-opened case.
  ;;   (2) There are reputed to be no bytes left (r or w as appropr.)
  ;;      in the current block.
  ;;   (3) Maybe a set ptr was done too -- will flush addressor

  ;; It might also be the end of file, but that's ok if we just return.

  ;; Let's check for EOF case first...

  (prog ()
    (selectq mode
      (:read 
       (if (= bytes-in-file cur-byte-address)
	   (progn					;eof
	    (if cur-addressor
		(progn					;note still have buffer..
		 (with-fs-locked (downreference-addressor cur-addressor))
		 (setq cur-addressor nil)))
	    (return nil))))
      ((:write :append))
      (:closed (ferror ':lmfs-operation-error "Cannot do I//O on closed file - ~A" self))
      (t (ferror ':lmfs-operation-error "Cannot do I//O on ~S opening - ~A" mode self)))

    ;; If there is a current addressor, it can't be worth anything. Flush it.
    ;; Keep a hold on cur-buffer --

    (if cur-addressor
	(with-fs-locked
	  (downreference-addressor cur-addressor)
	  (setq cur-addressor nil)))


    ;; The values of the next let* can be said to be the most important
    ;; statements in the file system.  Compute exactly what bit, byte, and word
    ;; the current pointer means, and exactly how many of each of those things
    ;; it means are left in the current block.

    (let* ((cur-bits (* using-byte-size cur-byte-address))
	   (dwpb (partt-dataw-per-block (fd-partition-desc file)))
	   (cur-word (// cur-bits *BITS-PER-WORD*))
	   (cur-block (// cur-word dwpb))
	   (cur-blockbase (* cur-block dwpb))
	   (blockbase-bits (* cur-blockbase *BITS-PER-WORD*))
	   (blockbase-bytes (// blockbase-bits using-byte-size))
	   (byte-offset (- cur-byte-address blockbase-bytes))
	   (blockend-words (+ dwpb cur-blockbase))
	   (blockend-bits (* blockend-words *BITS-PER-WORD*))
	   (blockend-bytes (// blockend-bits using-byte-size))
	   (true-end-bytes (selectq mode
			     (:read (min blockend-bytes bytes-in-file))
			     (:append blockend-bytes)
			     (:write blockend-bytes)))
	   (block-bytes-left (- true-end-bytes cur-byte-address)))
      
      ;; Figure out if this doesnt correspond to whatever record we were
      ;; keeping track of.  It is important to write out and downreference
      ;; it before attempting to allocate another one, so we may be able
      ;; to reuse this one.


      (if cur-buffer
	  (if (and (>= cur-word (fb-lowest-data-addr cur-buffer))
		   (< cur-word (fb-highest-data-addr+1 cur-buffer)))
	      nil				;that's ok, no problem
	      (progn
		(if (and (fb-modified cur-buffer)
			 (or (not *WRITE-BEHIND*)
			     (not (minusp (fb-lowest-header-addr cur-buffer)))
			     (let ((dwpr (partt-dataw-per-record (fd-partition-desc file))))
			       ( 0
				  (\ (fb-lowest-data-addr cur-buffer)
				     (* (1+ *WRITE-BEHIND-COUNT*) dwpr))
				  dwpr))))
		    (with-fs-locked (flush-buffers-for-file file t)))	;dont write header
		;;idea here is seeking, not consistency.
		(with-fs-locked (downreference-file-buffer cur-buffer))
		(setq cur-buffer nil))))

      ;; do these now in case write-out-file-buffer loses its lunch.

      (setq cur-word-addressor-base cur-blockbase
	    cur-byte-addressor-base blockbase-bytes
	    bytes-left-in-block	    block-bytes-left
	    cur-byte-offset	    byte-offset)

      (with-fs-locked
	(setq cur-addressor (get-file-data-block-regardless file cur-word-addressor-base))
	(if (null cur-buffer)
	    (progn
	      (setq cur-buffer (buffer-from-addressor cur-addressor))
	      (upreference-file-buffer cur-buffer)
	      (if (memq mode '(:write :append))
		  (set-buffer-modified cur-buffer))))
	(setq bytes-left-in-block block-bytes-left))	;in case we were the grow-invalidator

      (si:change-indirect-array cur-data-addressor
				(cdr (assoc
				      using-byte-size
				      '((16. . art-16b)(8. . art-8b)(4 . art-4b)
					(2 . art-2b)(1. . art-1b))))
				(list (- true-end-bytes blockbase-bytes))
				(fb-array cur-buffer)
				(* (8bldr-word-displacement cur-addressor)
				   (// *BITS-PER-WORD* using-byte-size))))))


(defmethod (lmfs-opening-mixin :after :force-output) ()
  (with-fs-locked (flush-buffers-for-file file))) ;window is ok

(defmethod (lmfs-opening-mixin :after :close) (&optional abort-p &aux was-mode)
  (setq was-mode mode
	abort-p (and (eq abort-p ':abort) (memq mode '(:write :append))))
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
				':date-time-modified
				(if (memq ':delete options) ':delete))

	(update-file-attributes file
				(if (and (eq mode ':read)
					 (not (memq ':no-update-reference-time options)))
				    ':date-time-used)
				(if (memq ':delete options) ':delete)))
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
					   (format nil "~A
~A
~D" name type version)
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





(defstorage (qfasl-magic-words)
	    (word1 fixnum-bytes 2)
            (word2 fixnum-bytes 2))

(defmethod (lmfs-opening-mixin :qfaslp) ()
  (let ((ocba cur-byte-address))
    (unwind-protect
      (progn
	(funcall-self ':real-set-pointer 0)
	(let ((mode (if (eq mode ':probe) ':read mode))) (validate-addressor))
	(and (= (qfasl-magic-words-word1 cur-addressor) #O143150)
	     (= (qfasl-magic-words-word2 cur-addressor) #O071660)))
      (and ocba (funcall-self ':real-set-pointer ocba)))))	;


(defmethod (lmfs-opening-mixin :plist) ()
  (list ':creation-date (fd-date-time-created file) ':truename real-pathname))

(defmethod (lmfs-opening-mixin :invalidate-addressors) ()     ;called internally from hdr grow
  (setq bytes-left-in-block 0))

(defmethod (lmfs-opening-mixin :set-backup-info) (dump-type tapename date-time)
  (with-fs-locked
    (update-file-attributes
      file
      (list (if (eq dump-type ':complete)
		':completely-backed-up
		':incrementally-backed-up)
	    tapename date-time))))

;;; The implementation of the system 75 Stream stuff.

;;; Used to be :set-ptr before sys 75
(defmethod (lmfs-opening-mixin :real-set-pointer) (val)		;I think its ok
							;for I & O
  (if (< val 0)
       (ferror ':lmfs-operation-error "set-pointer: Negative address ~D invalid - ~A"
	       val real-pathname))
  (if (> val bytes-in-file)
      (ferror ':lmfs-operation-error "set-pointer: Byte address ~D not in ~D of file ~A"
	      val bytes-in-file real-pathname))
      
  (if (and (eq mode ':append)
	   (< val original-byte-length))
      (ferror ':lmfs-operation-error "set-pointer: Byte address ~D below original ~D for append mode~%~A"
	      val original-byte-length real-pathname))

  (if cur-addressor
      (with-fs-locked (downreference-addressor cur-addressor)
		      (setq cur-addressor nil)))
  (setq cur-byte-address val))

(defmethod (lmfs-opening-mixin :next-input-buffer) (&rest ignore)	;no hang
  (if ( expected-next-address bytes-in-file)
      nil
      (progn
	(funcall-self ':real-set-pointer expected-next-address)
	(validate-addressor)
	(setq expected-next-address (+ cur-byte-address bytes-left-in-block))
	(values cur-data-addressor cur-byte-offset (+ bytes-left-in-block cur-byte-offset)))))

(defmethod (lmfs-opening-mixin :discard-input-buffer) (&rest ignore))	;beats me...

(defmethod (lmfs-opening-mixin :new-output-buffer) ()
  (validate-addressor)
  (values cur-data-addressor cur-byte-offset (+ bytes-left-in-block cur-byte-offset)))


(defmethod (lmfs-opening-mixin :send-output-buffer) (buf endindex)
  (or (eq buf cur-data-addressor)
      (ferror nil "You gave me some other buffer than I gave you."))
  (let* ((eaten (- endindex cur-byte-offset))
	 (newptr (+ cur-byte-address eaten)))
    (setq bytes-in-file (max bytes-in-file newptr))
    (setq cur-byte-address newptr)
    (decf bytes-left-in-block eaten)
    (incf cur-byte-offset eaten)))

(defmethod (lmfs-opening-mixin :discard-output-buffer) (&rest ignore)
  ;;Sorry, Charlie, we can't do that and maintain our lower-level contracts.
  nil)

(defmethod (lmfs-opening-mixin :set-buffer-pointer) (ptr)
  (funcall-self ':real-set-pointer ptr)
  (validate-addressor)
  (setq expected-next-address cur-byte-address))
  

(defmethod (lmfs-opening-mixin :get) (what)
  (selectq what
    (:creation-date  (fd-date-time-created file))
    (:truename       real-pathname)))

;;;Kludges to make dumper appear in wholine
(defmethod (lmfs-dumper-opening-mixin :set-abs-ptr) (ptr)
  (setq cur-byte-address ptr))

(defmethod (lmfs-dumper-opening-mixin :after :init) (ignore)
  (funcall tv:who-line-file-state-sheet ':add-stream self))

(defmethod (lmfs-dumper-opening-mixin :after :close) (&optional ignore)
  (funcall tv:who-line-file-state-sheet ':delete-stream self))

(defmethod (lmfs-dumper-opening-mixin :who-line-information) ()
  (values (funcall-self ':pathname) ':input cur-byte-address
	  (// (* 100. cur-byte-address) (fd-addressible-length file))))



(compile-flavor-methods
  lmfs-binary-input-opening
  lmfs-binary-output-opening
  lmfs-char-input-opening
  lmfs-char-output-opening
  lmfs-full-probe-opening
  lmfs-dumper-opening)

