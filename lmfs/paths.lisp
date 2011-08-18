;;; <LMFS>PATHS.LSP;1  5-May-81 12:53:10, Edit by BSG -*-Package:LMFS; Mode:LISP-*-

;;; Copyright (c) Symbolics, Inc., 1981

;;; Code having to do with hierarchies, activation, and deactivation of files.
;;; Notions of pathname-strings dealt with here, but not pathname objects.

(defconst *PATH-DELIMITER* ">")
(defconst *TYPE-DELIMITER* ".")
(defconst *VERSION-DELIMITER* ".")

(defconst *MAX-LINKS-PER-LEVEL* 10.)
(declare (special *THE-ROOT*))

(defvar *DONT-DEACTIVATES* nil)
(defvar *DONT-GRATUITOUSLY-DEACTIVATE-DIRECTORIES* nil)

(defun split-pathname (path)
  (if (not (string-equal (substring path 0 1) *PATH-DELIMITER*))
      (values
       nil nil (format nil 
	"FHN ERROR IPS F Pathname does not begin with path delimiter: ~A" path))
      (if (string-equal path *PATH-DELIMITER*)
	  (values ':root nil nil)
	  
	  ;; Not the root. Parse and find out indeed which directory.
	  
	  (let* ((delim-x (string-reverse-search *PATH-DELIMITER* path))
		 (pl (string-length path))
		 (dpath (substring path 0 delim-x))
		 (ename (substring path (1+ delim-x) pl)))
	    (if (zerop (string-length dpath)) (setq dpath *PATH-DELIMITER*))
	    (values dpath ename nil)))))

(defun get-filedesc-from-path (path type version)	;fd, err

  ;; link chasing is not in the contract of this guy, at least not at
  ;; the end-of-path level. Directory link chasing is always implicit..

  (multiple-value-bind (dpath ename err)
    (split-pathname path)
    (if err
	(values nil err)
	(if (eq dpath ':root)
	    (if (null *THE-ROOT*)
		(values nil (no-fs-error))
	      (values *THE-ROOT* nil))
	    (multiple-value-bind (fd err)
	      (get-fd-from-dn-en dpath ename type version)
	      (values fd err))))))

(defunp get-fd-from-dn-en (dpath ename type version &rest opts
				 &aux (link-count 0) (orig-dpath dpath) dir entry ex err)

  ;; See if in known active (represented) sons.

  (if (and (eq type ':directory) (equal dpath "") (equal ename ""))
      (if (null *THE-ROOT*)
	  (return-values nil (no-fs-error))
	(return-values *the-root* nil)))	;the way flavor guys come in...

retry-dir
  (incf link-count)
  (if (> link-count *MAX-LINKS-PER-LEVEL*)
      (return-values nil (format nil "Too many links (~D) to resolve directory pathname:~%~A"
				 *MAX-LINKS-PER-LEVEL* orig-dpath)))
  (multiple-value (dir err) (get-filedesc-from-path dpath ':directory 1))
  
  (if err (return-values nil err))
  
  ;; Check for directory link at this point...

  (if (fd-link-p dir)
      (progn
       (setq dpath (fd-link-p dir))			;value is target dir
       (go retry-dir)))

  (if (numberp version)					;not :highest, etc.
      (let ((active? (loop for son in (fd-sons dir)
			   if (and (string-equal ename (fd-file-name son))
				   (= version (fd-file-version son))
				   (let ((ftype (fd-file-type son)))
				     (or (eq type ftype)	;dir?
					 (string-equal type ftype))))
			   do (return son)
			   finally (return nil))))
	(if active? (return-values active? nil))))
  
  ;; Not active. Well, now we have to search the directory.
  ;; Actually, it might be active and this is a :highest, etc, too.

  (multiple-value (entry ex err)
    (lexpr-funcall #'search-directory dir ename type version opts))	;GJ%DEL
  ;; He will refcount buffer/addressor..
  (if err (return-values nil err))
  (if (null entry)
      (return-values
       nil
       (if (eq type ':directory)
	   (format nil "FNH ERROR NSD F: The directory ~A does not exist."
		   (dirpath-catenate dpath ename))
	   (format nil "FNH ERROR FNF F The file ~A was not found."
		   (pathname-catenate dpath ename type version)))))
  
  ;; Ok, found the thing. Activate it.
  
  (multiple-value-bind (fd err)
    (activate-file dir entry ex)
    (downreference-addressor entry)
    (return-values fd err)))

(defun no-fs-error ()
  (format nil "FHN ERROR NFS F There is no local file system on ~A." (chaos:host-data)))
 
(defun dirpath-catenate (dname ename)
   (if (string-equal dname *PATH-DELIMITER*)
       (string-append dname ename)
       (string-append dname *PATH-DELIMITER* ename)))

(defun pathname-catenate (dpath ename type version)
  (string-append
    (dirpath-catenate dpath ename)
	  *TYPE-DELIMITER*
	  type
	  (if (eq version ':newest)
	      ""
	      (string-append
		*VERSION-DELIMITER*
		(format nil "~D" version)))))

(defun create-file-from-path (dirpath name type version &rest options)
  (multiple-value-bind (dir err)
    (get-filedesc-from-path dirpath ':directory 1)
    (if err (values nil err)
	(multiple-value-bind (fd err)
	  (lexpr-funcall 'create-file dir name type version options)
	  (values fd err)))))

(defun create-dir-from-path (part parpath name &rest opts)
  (multiple-value-bind (par err)
      (get-filedesc-from-path parpath ':directory 1)
    (if err (values nil err)
	(multiple-value-bind (dir err)
	    (lexpr-funcall #'create-directory part par name opts)
	  (values dir err)))))



(defunp activate-file (dir entry entry-index)		;=> fd, err

  ;; See if there already, for keyword case.

  (let ((active? (loop for son in (fd-sons dir)
		       if (= entry-index (fd-entry-index son))
		       do (return son)
		       finally (return nil))))

    (if active? (return-values active? nil))

    ;; So really activate it.

    (multiple-value-bind (fd err)
      (activate-file-from-header
       dir
       (fd-partition-desc dir)				;needs work
       (directory-entry-record-0-address entry)
       (directory-entry-file-name entry)		;for errors
       (directory-entry-unique-ID entry))		;for cf check
      (if err (return-values nil err))
      (setf (fd-entry-index fd) entry-index)
      (push fd (fd-sons dir))
      (return-values fd nil))))

(defunp activate-file-from-header (par part r0addr errname uid)
  
  (if (not (check-address-against-partition part r0addr))
      (return-values nil (format nil "FNH ERROR CNF F Record 0 Disk Partition Address out of bounds: ~A ~O"
				 errname r0addr)))

  (let ((fd (make-file-desc
	     partition-desc     part
	     link-p		nil
	     sons		nil
	     buffer-list	nil
	     current-length	0			;more thought needed
	     byte-size		0
	     openings		nil

	     ;; Don't connect in as a son until something meaningful
	     ;; in here, such as full name, etc.
	     
	     parent		par
	     file-name		errname
	     uid		uid
	     r0addr		r0addr
	     addressible-length 0
	     logical-header-length 999999.		;until get it right
             file-map-addenda   nil
	     grc-info		nil
	     )))
    (let ((v (verify-address t fd 0)))		;check check words..
      (if v (return-values nil v)))

    (with-fileheader-addressibility (fd 0 h)
        (if (not (= (file-header-version h) *DEFAULT-HEADER-VERSION*))
	    (return-values nil (format nil "FHN ERROR CNF F Header in bad version ~D"
				       (file-header-version h))))
	(if (not (< (file-header-dire-location h) (partt-dataw-per-record part)))
	    (return-values nil (format nil "FHN ERROR CNF F Clobbered header dir-entry location, header unusable. ~o"
				       (file-header-dire-location h))))
      (with-fileheader-addressibility (fd (file-header-dire-location h) dire)
	(if (and uid (not (= uid (directory-entry-unique-ID dire))))
	    (return-values nil (format nil "FHN ERROR CNF F Connection failure: ~A"
				       errname))
	    (setf (fd-uid fd) (directory-entry-unique-ID dire))) ;for nil case
	
	(setf (fd-file-type fd)
	      (cond ((directory-entry-directory dire) ':directory)
		    (t (directory-entry-file-type dire))))

	(setf (fd-file-version fd) (directory-entry-file-version dire))
	(setf (fd-byte-length fd) (directory-entry-byte-length dire))
	(setf (fd-byte-size fd) (directory-entry-bytesize dire))
	(setf (fd-date-time-created fd) (directory-entry-date-time-created dire))
	(setf (fd-grc-info fd)
	      (and (not (zerop (directory-entry-generation-retention-count dire)))
		   (directory-entry-generation-retention-count dire)))
	(let ((bypw (// *BITS-PER-WORD* (fd-byte-size fd))))
	  (setf (fd-current-length fd)
		(// (+ (fd-byte-length fd) -1 bypw)
		    bypw)))

	;; Fix up buffers if there are now pieces of data in any of them.


	(setf (fd-logical-header-length fd) (file-header-logical-size h))
	(loop for buf in (fd-buffer-list fd)		;fix up data addrs
	      do (set-up-buf-as-header-containing
		  buf fd (fb-lowest-header-addr buf)))

	(setf (fd-file-name fd)
	      (if (= (directory-entry-file-name-true-length dire)
		     (directory-entry-file-name-length dire))
		  (directory-entry-file-name dire)
		  (fs-plist-get fd 'name)))
	
	(with-fileheader-addressibility (fd (file-header-header-fm-location h) hfm)
         (let ((hrecs (file-map-valid-length hfm)))
	   (if (and (< hrecs (file-map-allocated-length hfm))
		    (not (zerop (file-map-element hfm hrecs))))	;old "zeroes"
	       (finish-aborted-header-grow fd))))

	(with-fileheader-addressibility
	 (fd (file-header-file-map-location h) fm)
	 
	  (let* ((dwpr (partt-dataw-per-record (fd-partition-desc fd)))
		 (hrem (\ (fd-logical-header-length fd) dwpr))
		 (len (if (zerop hrem) 0 (- dwpr hrem))))
	    (setf (fd-addressible-length fd)
		  (+ len (* dwpr (fm-len-rep fd fm))))))	     

	

	;; Handle link dharma -- gonna cons this string anyway
	;; each time info wanted, whether we activate it each time or not.
	;; So some heurstic kludge is needed for procrastinating
	;; link deactivation.

	(if (directory-entry-link dire)
	    (setf (fd-link-p fd)
		  (with-filedata-addressibility (fd 0 linktarget)
		    (link-target-pathname linktarget)))

	    ;; Not link - handle dir magics
	    (if (directory-entry-directory dire)
		(with-filedata-addressibility (fd 0 dirh)
		  (setf (fd-dir-entries-index-offset fd)
			(dir-header-entries-index-offset dirh))
		  (setf (fd-dir-entry-size fd) (dir-header-direntry-size dirh)))))

	(return-values fd nil)))))

(defun fm-len-rep (fd fm)
  (+ (file-map-valid-length fm)
     (if (zerop (file-map-link fm))
	 0
	 (with-fileheader-addressibility (fd (file-map-link fm) ffm)
	   (fm-len-rep fd ffm)))))

(defun check-deactivate-file (fd)
  (if (file-theoretically-deactivateable-p fd)
      (deactivate-file fd)))

(defun file-theoretically-deactivateable-p (fd)
  (and (null (fd-sons fd))
       (not (symbolp (fd-parent fd)))
       (null (fd-openings fd))
       (not (memq fd *DONT-DEACTIVATES*))
       (loop for buf in (fd-buffer-list fd)
	     finally (return t)
	     if (not (zerop (fb-reference-count buf)))
	     do (return nil))))

(defun deactivate-file (fd)

  ;; File should be guaranteed to be theoretically deactivateable at this point.

  (dolist (buf (fd-buffer-list fd))
    (if (fb-modified buf)
	(write-out-file-buffer buf)))

  (mapc 'disconnect-buffer-from-file (fd-buffer-list fd))

  ;; Try to deactivate uptree.  Perhaps at this point (Moon suggests,)
  ;; we should keep an lru ring or gratuitously kept-active dirs
  ;; to optimize activations ...

  (let ((par (fd-parent fd)))
    (if (symbolp par)
	(break attempt-to-deactivate-parentless-file)
	(progn
	 (setf (fd-sons par) (delq fd (fd-sons par)))
	 (setf (fd-parent fd) nil)
	 (if (and (not *DONT-GRATUITOUSLY-DEACTIVATE-DIRECTORIES*)
		  (not (symbolp (fd-parent par))))
	     (check-deactivate-file par))))))

(defun deactivate-cleanup (&optional (fd *the-root*) &aux
			   &special (*DONT-GRATUITOUSLY-DEACTIVATE-DIRECTORIES* T))
  (deactivate-cleanup-recurse fd))

(defun deactivate-cleanup-recurse (fd)
  (if (fd-sons fd) (mapc #'deactivate-cleanup-recurse (fd-sons fd)))
  (check-deactivate-file fd))

(defun expunge-dir-path (dir-pn)
  (multiple-value-bind (fd err)
    (get-filedesc-from-path dir-pn ':directory 1)

    (if err
	(values 0 (list (list err dir-pn)))
	(multiple-value-bind (records excuses)
	    (expunge-directory fd)

	  (values records
		  (mapc
		   #'(lambda (x)
		       (let ((flist (second x)))
			 (setf (second x)
			       (pathname-catenate dir-pn (first flist)
						  (second flist)(third flist)))))
		   excuses))))))

  
(defun get-link-transparencies (fd &aux (ans nil))	;a primitive- had better be a link
  (with-filedata-addressibility (fd 0 linktarget)
    (if (link-target-transparent-read-thru linktarget) (push ':read ans))
    (if (link-target-transparent-write-thru linktarget) (push ':write ans))
    (if (link-target-transparent-delete-thru linktarget) (push ':delete ans))
    (if (link-target-transparent-create-thru linktarget) (push ':create ans))
    (if (link-target-transparent-rename-thru linktarget) (push ':rename ans)))
  ans)

(defun set-link-transparencies (fd specs)
  ;;a primitive- had better be a link
  (with-filedata-addressibility-modifying (fd 0 linktarget)
    (loop for (attribute val) on specs by 'cddr
	  do
	  (selectq attribute
	    (:read (setf (link-target-transparent-read-thru linktarget) val))
	    (:write (setf (link-target-transparent-write-thru linktarget) val))
	    (:rename (setf (link-target-transparent-rename-thru linktarget) val))
	    (:create (setf (link-target-transparent-create-thru linktarget) val))
	    (:delete (setf (link-target-transparent-delete-thru linktarget) val))))
    (update-file-attributes fd ':date-time-modified)
    (if (fb-modified (buffer-from-addressor linktarget))	;upd-file-atts might have done it...
	(write-out-buffer-from-addressor linktarget))
    (get-link-transparencies fd)))


(defun get-link-transparencies-dn-en (dname name type version)
  (multiple-value-bind (fd err)
    (get-fd-from-dn-en dname name type version)
    (or err
	(if (fd-link-p fd)
	    (get-link-transparencies fd)
	    (format nil "get-link-transparencies: ~A is not a link."
		    (pathname-catenate dname name type version))))))

(defun set-link-transparencies-dn-en (dname name type version specs)
  (multiple-value-bind (fd err)
    (get-fd-from-dn-en dname name type version)
    (or err
	(if (fd-link-p fd)
	    (set-link-transparencies fd specs)
	    (format nil "set-link-transparencies: ~A is not a link."
		    (pathname-catenate dname name type version))))))
 
;;; Issue remaining to be solved about how//whether dir hacking commands
;;; or their interfaces deal with dir links at the end level...


(defun get-dir-link-transparencies-path (dirpn)
  (multiple-value-bind (fd err)
    (get-filedesc-from-path dirpn ':directory 1)
    (or err
	(if (and (eq (fd-file-type fd) ':directory)
		 (not (fd-link-p fd)))
	    (get-dir-link-transparencies fd)

	    (format nil "get-dir-link-transparencies: ~A is not a directory." dirpn)))))

(defun set-dir-link-transparencies-path (dirpn specs)
  (multiple-value-bind (fd err)
    (get-filedesc-from-path dirpn ':directory 1)
    (or err
	(if (and (eq (fd-file-type fd) ':directory)
		 (not (fd-link-p fd)))
	    (set-dir-link-transparencies fd specs)

	    (format nil "get-dir-link-transparencies: ~A is not a directory." dirpn)))))

(defvar *nondir-changeable-props*
	'(:dont-delete  :generation-retention-count :deleted))

(defvar *dir-changeable-props*
	'(:dont-delete :deleted :auto-expunge-interval :default-generation-retention-count))

(defvar *fs-maintained-props*
	'(:length-in-bytes :byte-size :creation-date :reference-date :deleted
			   :offline :length-in-blocks :directory :link-to
			   :link-transparencies :default-link-transparencies
			   :auto-expunge-interval :default-generation-retention-count
			   :generation-retention-count :modification-date))

(dolist (p *fs-maintained-props*) (putprop p t 'lmfs-maintains))

(defvar *currently-changeable-props*
	'(:dont-delete :default-generation-retention-count
		       :auto-expunge-interval
		       :generation-retention-count :deleted))


(defvar *secretly-changeable-props* '(:author :creation-date
					      :internal-backup-info
					      :link-transparencies
					      :default-link-transparencies))

(defun directory-list (dirpn name type version &optional options)
  (multiple-value-bind (fd err)
      (get-filedesc-from-path dirpn ':directory 1)
    (if err
	(values nil err)
	(let ((env (list nil name type version options fd)))
	  (scan-directory
	    fd
	    #' (lambda (entry ex env)
		 (let ((name (second env))
		       (type (third env))
		       (version (fourth env))
		       (options (fifth env))
		       (dir (sixth env)))
		   (if (and (or (not (directory-entry-deleted entry))
				(memq ':deleted options))
			    (or (eq name ':unspecific)
				(compare-name-to-directory-entry name entry ex dir))
			    (or (eq type ':unspecific)
				(if (eq type ':directory)
				    (directory-entry-directory entry)
				    (directory-entry-file-type-compare entry type)))
			    (or (eq version ':unspecific)
				(and (numberp version)
				     (= version (directory-entry-file-version entry)))))
		       (progn
			 (push
			   (if (memq ':fast options)	;speed up FED etc
			       (ncons (listify-file-name-from-entry dir entry ex))
			       (file-properties-from-entry entry ex dir options))
			   (first env))
			 (if (memq ':additional-info options)	;speed up dumper...
			     (multiple-value-bind (fd err)
				 (activate-file dir entry ex)
			       (or err
				   (setf (cdar (first env))
					 (nconc (get-backup-info fd)
						(cdar (first env))))))))))
		 nil)				;DONT STOP
	    env)
	  ;; woops. uncrockify this when add partition ID's to entry....

	  (push (global-file-props fd) (first env))
	  (let ((answer (first env)))
	    (if (and (cdr answer)
		     (null (cddr answer))	;one thing only
		     (null (get (second answer) ':directory)))
		(putprop (car answer) *nondir-changeable-props* ':settable-properties))
	    (values answer nil))))))


(defun global-file-props (fd)			;global info
  (let ((nilent (ncons nil)))
    (putprop nilent (copylist *currently-changeable-props*) ':settable-properties)
    (putprop nilent (* 32. (partt-record-size-words (fd-partition-desc fd))) ':block-size)
    nilent))

(defun file-properties (dirpn name type version &aux fplist usps)
  (multiple-value-bind (fd err)
      (get-fd-from-dn-en dirpn name type version ':deleted)
    (Or err
	(let ((*DONT-DEACTIVATES* (cons fd *DONT-DEACTIVATES*)))
	  (values 
	    (nconc
	      (file-properties-from-fd fd)	;extra work for :pathname, ok.
	      (let ((p (file-plist fd)))	;get fs-maintained prop list
		(loop for pp on p by 'cddr
		      do
		      (push (car (rplaca pp (intern (car pp) "")))	;keywordize
			    usps))
		(setq fplist p))
	      (get-backup-info fd))
	    (nconc usps
		   (if (eq type ':directory)
		       *dir-changeable-props*
		       *nondir-changeable-props*)))))))

(defun file-properties-from-fd (fd &aux entry)
  (unwind-protect
    (let ((par (fd-parent fd))
	  (ex (fd-entry-index fd)))
      (setq entry (if (eq fd *the-root*)
		      (with-fileheader-addressibility (fd 0 h)
			(get-general-file-addressor T fd (file-header-dire-location h)))
		      (get-general-file-addressor nil par (entry-index-to-offset par ex))))
      (prog1 (file-properties-from-entry entry ex par)
	     (check-deactivate-file fd)))
    (and entry (downreference-addressor entry))))

(defmacro  c-putprop (pl val ind) `(rplacd ,pl (cons ,ind (cons ,val (cdr ,pl)))))

(defun file-properties-from-entry (entry ex dir &optional opts
				   &aux (plist
					  (ncons
					    (listify-file-name-from-entry  dir entry ex))))
  (if (not (or (directory-entry-directory entry) (directory-entry-link entry)))
      (progn
	(c-putprop plist (directory-entry-byte-length entry) ':length-in-bytes)
	(c-putprop plist (directory-entry-bytesize entry) ':byte-size)))
  (c-putprop plist (directory-entry-author entry) ':author)
  (c-putprop plist (directory-entry-date-time-created entry) ':creation-date)
  ;;Unfortunately, the order in which these things are listed is user-visible.
  (if (not (directory-entry-directory entry))
      (c-putprop plist
		 (let ((dtm (directory-entry-date-time-modified entry)))
		   (if (zerop dtm) (directory-entry-date-time-created entry) dtm))
		 ':modification-date))
  (if (not (zerop (directory-entry-date-time-used entry)))
      (c-putprop plist (directory-entry-date-time-used entry) ':reference-date))
  (c-putprop plist (directory-entry-deleted entry) ':deleted)
  (c-putprop plist (not (or (directory-entry-incrementally-backed-up entry)
			    (directory-entry-completely-backed-up entry)))
	     ':not-backed-up)
  (c-putprop plist (directory-entry-migrated entry) ':offline)
  (c-putprop plist (directory-entry-safety-switch entry) ':dont-delete)
  (c-putprop plist (directory-entry-number-of-records entry) ':length-in-blocks)
  (if (not (directory-entry-directory entry))
      (progn
	
	(c-putprop plist (directory-entry-generation-retention-count entry)
		   ':generation-retention-count))
      (progn					;yes, a directory
	(c-putprop plist t ':directory)
	(if (and (not (directory-entry-link entry))
		 (not (memq ':no-dir-info opts)))
	    (multiple-value-bind (fdd err)
		(if (null dir)
		    (values			;cheat for parentless files
		      (fb-file-desc (8bldr-buffer entry)) nil)
		    (activate-file dir entry ex))
	      (if err (c-putprop plist (format nil "Cannot activate directory: ~A" err)
				 ':directory-reading-error)
		  (progn
		    (with-filedata-addressibility (fdd 0 dir)
		      (c-putprop plist
				 (if (dir-header-auto-expunge-p dir)
				     (dir-header-auto-expunge-interval dir)
				     nil)
				 ':auto-expunge-interval)
		      (c-putprop plist		;misnamed
				 (dir-header-auto-expunge-last-time dir)
				 ':date-last-expunged)
		      (c-putprop plist
				 (if (zerop (dir-header-default-generation-retention-count dir))
				     nil
				     (dir-header-default-generation-retention-count dir))
				 ':default-generation-retention-count)
		      (c-putprop plist (get-dir-link-transparencies fdd)
				 ':default-link-transparencies))
		    (check-deactivate-file fdd)))))))
  (if (directory-entry-link entry)
      (multiple-value-bind (fdd err)
	  (activate-file dir entry ex)
	(if err
	    (putprop plist (format nil "Cannot get link target: ~A" err) ':link-to)
	    (progn
	      (c-putprop plist (fd-link-p fdd) ':link-to)
	      (c-putprop plist (get-link-transparencies fdd) ':link-transparencies)
	      (check-deactivate-file fdd)))))
  plist)




(defun change-file-properties (dirpn name type version props &aux aux-props)
  ;; First, attempt to get the file in hand.
  (multiple-value-bind (fd err)
      (get-fd-from-dn-en dirpn name type version ':deleted)
    (or err
	(progn
	  (loop for (ind prop) on props by 'cddr	;thanks gsb
		do
		(cond ((or (memq ind *currently-changeable-props*)
			   (memq ind *secretly-changeable-props*))
		       (selectq ind
			 (:link-transparencies
			  (if (fd-link-p fd)
			      (set-link-transparencies fd prop)))
			 (:default-link-transparencies
			  (if (and (eq (fd-file-type fd) ':directory)
				   (not (fd-link-p fd)))
			      (set-dir-link-transparencies fd prop)))
			 (:default-generation-retention-count
			  (if (eq (fd-file-type fd) ':directory)
			      (with-filedata-addressibility (fd 0 dir)
				(setf (dir-header-default-generation-retention-count dir)
				      (or prop 0))
				(write-out-buffer-from-addressor dir))))
			 (:auto-expunge-interval
			  (if (and (eq (fd-file-type fd) ':directory)
				   (not (fd-link-p fd)))
			      (with-filedata-addressibility (fd 0 dir)
				(if (or (null prop) (zerop prop))
				    (progn
				      (setf (dir-header-auto-expunge-p dir) nil)
				      (setf (dir-header-auto-expunge-interval dir) 0))
				    (progn
				      (setf (dir-header-auto-expunge-interval dir) prop)
				      (setf (dir-header-auto-expunge-p dir) t)))
				(write-out-buffer-from-addressor dir))))
			 (:generation-retention-count
			  (setf (fd-grc-info fd) prop)
			  (push ind aux-props))
			 (:internal-backup-info
			  (push prop aux-props))
			 (t (push (list ind prop) aux-props))))
		      ((get ind 'lmfs-maintains))	;bogus attempt to set
		    (t (fs-plist-put fd ind prop ':dont-write))))
	  (lexpr-funcall #'update-file-attributes fd aux-props)
	  (flush-buffers-for-file fd)
	  (check-deactivate-file fd)		;for god's sake, this is a dname/ename xface
	  t))))



(defun fast-probe-info-getter (dname name type version deleted-p
			       &aux vers length crdate ex)
  ;;=>vers length crdate
  (multiple-value-bind (dir err)
      (get-filedesc-from-path dname ':directory 1)
    (or err
	(progn
	  (protect-buffer-addressor (entry nil)
	    (multiple-value (entry ex err)
	      (search-directory dir name type version deleted-p))
	    ;; He will refcount buffer/addressor..
	    (if (null entry)
		(setq vers "FHN ERROR FNF F File not found")
		;;dont cons pretty msg just so probef can throw it away....
		;; Consider the reloader.
		(and
		  (not (directory-entry-link entry))	;go hard way for links
		  (progn
		    (setq vers (directory-entry-file-version entry))
		    (setq length (directory-entry-byte-length entry))
		    (setq crdate (directory-entry-date-time-created entry))))))
	  ;;DONT deactivate---
	  (values vers length crdate)))))
