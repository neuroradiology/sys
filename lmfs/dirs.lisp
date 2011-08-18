;;; <LMFS>DIRS.LSP;32 30-Apr-81 10:46:21, Edit by BSG -*-Package:LMFS; Mode:LISP-*-
;;; <LMFS>DIRS.LSP;1 26-Mar-81 11:35:22, Edit by BSG -*-Mode:LISP;Package:LMFS-*-

;;; Copyright (c) Symbolics, Inc., 1981

;;; Things having to do with directory entries and their listing and array;
;;; and the searching thereof.  NOT things associated with their semantics.


(declare (*lexpr fs-error))

(defvar *DIR-VERSION* 1)
(defvar *VERSION-FOR-DIRS* 1)
(defvar *MAX-HIERARCHY-DEPTH* 16.)
(defvar *DEFAULT-DEFAULT-LINK-TRANSPARENCIES* '(:read t :write t))

(defun create-directory (partition parent-dir name ;=> file-desc errexp
			 &rest options
			 &aux file err)
  
  ;; Create the file to be the directory; verify hier. depth.
  ;; Create start of header.
  ;; Update parent dir- shd be no problem
  (multiple-value (file err)
    (lexpr-funcall 'create-file parent-dir name ':directory *VERSION-FOR-DIRS*
		   options))
  (if err
      (values nil err)

      ;;Should be no lossage hereonin.
      
      ;; create-file, given dir, made the filedata block-align.
      ;; buffer state is "bad"..
      (progn
	(init-directory-header file)
	(set-dir-link-transparencies file *DEFAULT-DEFAULT-LINK-TRANSPARENCIES*)
	(flush-buffers-for-file file)
	(if (or (null parent-dir)
		(not (eq partition (fd-partition-desc parent-dir))))
	    (progn
	      (push file (partt-root-list partition))
	      (update-label-roots partition)))
	(values file nil))))

(defun init-directory-header (file
			      &aux (total-header (dir-header-size-in-words))
			      (partition (fd-partition-desc file))
			      (parent-dir (fd-parent file)))
  (with-filedata-addressibility-modifying (file 0 dirh)
    (setf (dir-header-version dirh) *DIR-VERSION*)
    (setf (dir-header-name dirh) (fd-file-name file))
    (setf (dir-header-number-of-entries dirh) 0)
    (setf (dir-header-free-entry-list dirh) 0)
    (setf (dir-header-direntry-size dirh) (directory-entry-size-in-words))
    (setf (dir-header-entries-per-block dirh)
	  (// (partt-dataw-per-block partition) (directory-entry-size-in-words)))
    (setf (dir-header-default-generation-retention-count dirh) 0)
    (setf (dir-header-auto-expunge-last-time dirh) (time:get-universal-time))
    (if (null parent-dir)
	(progn (setf (dir-header-uid-path-length dirh) 0)
	       (setf (dir-header-hierarchy-depth dirh) 0))
	(with-filedata-addressibility (parent-dir 0 par)
	  (let ((par-hdepth (dir-header-hierarchy-depth par))
		(where))
	    (setf (dir-header-hierarchy-depth dirh) (1+ par-hdepth))
	    (with-filedata-addressibility
	     (file
	      (setq where (lmfs-allocate-data-area file total-header *MAX-HIERARCHY-DEPTH*))
	      uid-array)
	     
	     ;; fileheaders are block-aligned in end, at least for dirs.
	     (setq total-header(+ *MAX-HIERARCHY-DEPTH* where))
	     
	     (with-filedata-addressibility (parent-dir (dir-header-uid-path-offset par) paruids)
	       (loop for i from 0 to (1- par-hdepth)
		     do (setf (uid-array-element uid-array i)
			      (uid-array-element paruids i))))
	     (setf (uid-array-element uid-array par-hdepth)
		   (fd-uid parent-dir))
	     (loop for i from (1+ par-hdepth) to (1- *MAX-HIERARCHY-DEPTH*)
		   do (setf (uid-array-element uid-array i) 0))
	     
	     (setf (dir-header-uid-path-offset dirh) where)
	     (setf (dir-header-uid-path-length dirh) *MAX-HIERARCHY-DEPTH*)))))

    (setf (fd-dir-entry-size file) (directory-entry-size-in-words))	;support different versions.
    (setf (fd-dir-entries-index-offset file)
	  (let ((round-to-entries (round-up-to-boundary
				   (directory-entry-size-in-words) total-header)))
	    ;; this 1- is to make the first one 1, not zero.
	    (1- (// round-to-entries (directory-entry-size-in-words)))))
    (setf (dir-header-entries-index-offset dirh) (fd-dir-entries-index-offset file))))


(defmacro search-directory-throw ()
  `(progn
     (upreference-addressor entry)
     (setq found-ptr entry found-ex ex)
     (*throw '*found nil)))


(defun search-directory (dir name type version &rest opts)	;=> addressibility, ex
							;or nil
  (if (and opts (atom opts))
      (setq opts (list opts)))			;auld forme kloudge
  (with-filedata-addressibility (dir 0 dirh)
    (or (= (dir-header-version dirh) *DIR-VERSION*)
	(fs-error "Directory in bad version: ~S" (dir-header-version dirh)))
      (if (dir-header-auto-expunge-p dirh) (check-dir-auto-expunge dir dirh))
    (let ((number-of-entries (dir-header-number-of-entries dirh))
	  (entries-index-offset (dir-header-entries-index-offset dirh))
	  (direntry-size (dir-header-direntry-size dirh))
	  (entries-per-block (dir-header-entries-per-block dirh))
	  (bsize (partt-dataw-per-block (fd-partition-desc dir)))
	  found-ptr (found-ex -1)
	  (del-ok (memq ':deleted opts))
	  (faowchk (memq ':check-write-openings opts))
	  (newest -1)
	  (oldest 99999999.))
      (*catch '*found
        (loop for ex from 1 to number-of-entries
	  do
	  (let* ((rex (+ ex entries-index-offset))
		 (block (// rex entries-per-block))
		 (relent (\  rex entries-per-block))
		 (wordaddr (+ (* block bsize) (* relent direntry-size))))
	    (with-filedata-addressibility (dir wordaddr entry)
	      (cond ((and (eq version ':free) (zerop (directory-entry-unique-ID entry)))
		     (search-directory-throw))
		    ((zerop (directory-entry-unique-ID entry)))	;forget that!
		    ((not (and (compare-name-to-directory-entry name entry ex dir)
			       (or (and (eq type ':directory)
					(directory-entry-directory entry))
				   (directory-entry-file-type-compare entry type)))))
		    ((and (numberp version) (= version (directory-entry-file-version entry))
			  (or (not (directory-entry-deleted entry)) del-ok))
		     (search-directory-throw))
		    ((or (and (eq version ':newest)
			      (> (directory-entry-file-version entry) newest)
			      (or (not (directory-entry-deleted entry)) del-ok)
			      (or (not faowchk) (search-directory-faowchk dir entry))
			      (setq newest (directory-entry-file-version entry)))
			 (and (eq version ':oldest)
			      (< (directory-entry-file-version entry) oldest)
			      (or (not (directory-entry-deleted entry)) del-ok)
			      (or (not faowchk) (search-directory-faowchk dir entry))
			      (setq oldest (directory-entry-file-version entry))))
		     (upreference-addressor entry)
		     (and found-ptr (downreference-addressor found-ptr))
		     (setq found-ptr entry found-ex ex)))))))
      (values found-ptr found-ex))))

(defun search-directory-faowchk (dir entry)
  "Check for potential newest//oldest already open for writing,and punt it"
  (let ((uid (directory-entry-unique-ID entry)))
    (loop for fd in (fd-sons dir) finally (return t)
	  do
	  (if (= uid (fd-uid fd))
	      (if (loop for opening in (fd-openings fd)
			finally (return nil)
			do
			(if (funcall opening ':check-new-mode ':read)
			    (return t)))
		  (return nil))))))


;;;Contract of this gentleprogram:
;;;You give it DIR, a function (FCN) to be called on each entry, and "closure" as ENV.
;;;Function will be called on ENTRY-ADDRESSOR, ENTRY-INDEX, and ENV for each entry.
;;;Unless you give ZUIDSW T, FCN will NOT be called on free entries.
;;;WHAT YOUR FCN RETURNS IS IMPORTANT: if it returns nil, the scan continues to
;;;the end. If it returns NON-NIL, the scan STOPS, and the upreferenced addressor
;;;to that entry is returned. If you don't want it, you'd better downreference it
;;;(highly unlikely, cause why'd you return non-nil in that case?)
;;;Values returned are the upreferenced addressor and entry index of the entry
;;;at which FCN returned non-nil, if any, or NIL.
;;;If FCN wants to save some addressors it sees go by, it'd better upreference
;;;them before saving, 'cause SCAN-DIRECTORY keeps its own house clean.

(defun scan-directory (dir fcn env &optional zuidsw);=> addressibility, ex
							;or nil
  (with-filedata-addressibility (dir 0 dirh)
    (or (= (dir-header-version dirh) *DIR-VERSION*)
	(fs-error "Directory in bad version: ~S" (dir-header-version dirh)))
    (let ((number-of-entries (dir-header-number-of-entries dirh))
	  (entries-index-offset (dir-header-entries-index-offset dirh))
	  (direntry-size (dir-header-direntry-size dirh))
	  (entries-per-block (dir-header-entries-per-block dirh))
	  (bsize (partt-dataw-per-block (fd-partition-desc dir)))
	  found-ptr (found-ex -1))
      (if (dir-header-auto-expunge-p dirh) (check-dir-auto-expunge dir dirh))
      (*catch '*found
        (loop for ex from 1 to number-of-entries
	  do
	  (let* ((rex (+ ex entries-index-offset))
		 (block (// rex entries-per-block))
		 (relent (\  rex entries-per-block))
		 (wordaddr (+ (* block bsize) (* relent direntry-size))))
	    (with-filedata-addressibility (dir wordaddr entry)
	      (cond ((and (not zuidsw)
			  (zerop (directory-entry-unique-ID entry))))
		    ((funcall fcn entry ex env)
		     (search-directory-throw)))))))
      (values found-ptr found-ex))))

(defun compare-name-to-directory-entry (name entry entry-index dir)
  (let ((given-length (string-length name)))
    (if (= (directory-entry-file-name-true-length entry) given-length)
	(if (> given-length (directory-entry-file-name-max-length))
	    (if (directory-entry-file-name-compare
		  entry
		  (substring name 0 (directory-entry-file-name-max-length)))
		(multiple-value-bind (fd err)	;eyuck
		    (activate-file dir entry entry-index)
		  (if err nil
		      (prog1 (fs-plist-compare fd 'name name)
			     (check-deactivate-file fd))))
		nil)
	    (directory-entry-file-name-compare entry name))
	nil)))

(defun get-free-directory-entry (dir &aux error)
  (multiple-value-bind (ptr ex)
    (search-directory dir "" "" ':free) 
    (if (null ptr)
	(multiple-value (ptr ex error)
	  (allocate-new-directory-entry dir)))

    (values ptr ex error)))

(defun allocate-new-directory-entry (dir)
  (with-filedata-addressibility-modifying (dir 0 dirh)
    (let* ((newnx (1+ (dir-header-number-of-entries dirh)))
	   (rnewx (+ newnx (dir-header-entries-index-offset dirh)))
	   (bsize (partt-dataw-per-block (fd-partition-desc dir)))
	   (epb (dir-header-entries-per-block dirh))
	   (bno (// rnewx epb))
	   (rbx (\  rnewx epb)))
      
      (incf (dir-header-number-of-entries dirh))
      (write-out-buffer-from-addressor dirh)
      ;; no problem with writing it out now. the entry is guaranteed
      ;; to have zero uid in any case.
      
      (values
        (get-file-data-block-regardless
	 dir
	 (+ (* bno bsize) (* rbx (directory-entry-size-in-words))))
	newnx
	nil))))


(defun simple-list-directory (dir)
  (let ((env (cons dir (ncons nil))))
    (scan-directory dir
		    #'(lambda (entry ex env)
			(push (cons ex (listify-file-name-from-entry (car env) entry ex))
			      (first (cdr env)))
			nil)				;== keep going
		    env
		    nil)				;zero uid NOT wanted
    (nreverse (first (cdr env)))))

(defun dir-not-empty-p (dir)			;used at DELETE time.
  (or (fd-sons dir)
      (let ((env (ncons nil)))
	(scan-directory dir
			#'(lambda (ignore ignore env)
			    (setf (car env) t)
			    t)			;and return 
			env
			nil)			;zero uid not wanted - first hit is fine.
	(car env))))

(defun get-new-directory-entry (dir name type version
				    &aux crthru-fd crthru-err (grc 0))
  ;;This baloney would not be necessary if there were effective funarging.
  (let ((env (list (list  name type (if (numberp version) version -1))
		   ;;Dont use backquote, this gets RPLACA'ed.
		   (list 0 nil nil 0 nil)	;highx highptr freeptr freex highvlinkx
		   ;last-link-highest-entryindex
		   nil				;THIRD, error
		   dir)))			;FOURTH, dir
    (let
      ((found
	 (scan-directory
	   dir
	   #'(lambda (entry ex env)
	       (destructuring-bind ((name type version) sinfo nil dir) env
		 (cond ((zerop (directory-entry-unique-ID entry))
			(if (null (third sinfo))
			    (progn
			      (upreference-addressor entry)
			      (setf (third sinfo) entry)
			      (setf (fourth sinfo) ex)))
			nil)
		       ((not (and (if (eq type ':directory)
				      (directory-entry-directory entry)
				      (directory-entry-file-type-compare entry type))
				  (compare-name-to-directory-entry name entry ex dir)))
			nil)
		       ((= version (directory-entry-file-version entry))
			;; This really should not happen, flavor level guy should psych it out
			;; for himself, so we shouldn't see this msg.
			;; It does happen in :create-directory, though, currently.
			(setf (third env) "FHN ERROR FAX F The file already exists.")
			t)
		       ((> (directory-entry-file-version entry) (first sinfo))
			;; This is the highversion
			(setf (fifth sinfo)
			      (if (directory-entry-link entry) ex nil))
			(upreference-addressor entry)
			(and (second sinfo) (downreference-addressor (second sinfo)))
			(setf (first sinfo) (directory-entry-file-version entry))
			(setf (second sinfo) entry)
			nil))))
	   env
	   ':zero-uid-wanted)))
      
      ;; Only found if FAX error, thanks, scan-dir, but no thanks.
      (if found (downreference-addressor found)))

    (destructuring-bind (nil (highvers highptr new newx highvlinkx) error) env
      
      (setq grc					;get new guy's generation retention count
	    (if highptr
		(directory-entry-generation-retention-count highptr)
		(with-filedata-addressibility (dir 0 dirh)
		  (dir-header-default-generation-retention-count dirh))))

      (if (and (not error)
	       highvlinkx
	       (eq version ':newest)
	       (progn
		(multiple-value (crthru-fd crthru-err)
		  (activate-file dir highptr highvlinkx))
		(and (null crthru-err)
		     (let ((ctp (memq ':create (get-link-transparencies crthru-fd))))
		       (if ctp
			   (progn
			    (if highptr (downreference-addressor highptr))
			    (if new (downreference-addressor new)))
			   (check-deactivate-file crthru-fd))
		       ctp))))

	  (values nil 0 ':create-through-link-magic crthru-fd 0 grc)

	  ;; Not the case of a create-through-link ..
	  
	  (progn
	   (if (and (not error) (null new))
	       (multiple-value (new newx error)
		 (allocate-new-directory-entry dir)))
	   (if error
	       (if new (downreference-addressor new))	;not likely
	       
	       ;; right place to inherit generation attributes?
	       
	       (setq version
		     (if (eq version ':newest) (1+ highvers)
			 version)))
	   (if highptr (downreference-addressor highptr))
	   (values new newx error nil version grc))))))
    

(defun expunge-directory-entry (dir ex)			;=> records or some excuse.
  (let ((entry-offset (entry-index-to-offset  dir ex)))
    (with-filedata-addressibility (dir entry-offset entry)
      (let ((result
	     (if (zerop (directory-entry-unique-ID entry))
		 "Directory entry vanished somehow"
		 (if (not (directory-entry-deleted entry))
		     "The file has not been marked as deleted."
		     (multiple-value-bind (fd err)
		       (activate-file dir entry ex)
		       (if err
			   (if (string-equal (substring err 4 13.) "ERROR CNF")
			       (progn (remove-directory-entry entry) 0)
			       err)
			   (let ((answer (expunge-file fd)))
			     (if (fixp answer) (remove-directory-entry entry))
			     answer)))))))
	(if (fixp result)
	    result
	    (list result (listify-file-name-from-entry dir entry ex)))))))

(defun remove-directory-entry (entry)
  (setf (directory-entry-unique-ID entry) 0)
  (set-bufmod-from-addressor entry)
  (write-out-buffer-from-addressor entry))

(defun expunge-directory (dir)

  ;; Do this in two passes to facilitate doing it in more than 1 network operation

  (let ((env (ncons nil))
	(recs 0)
	(excuses nil))
    (scan-directory dir
		    #'(lambda (entry ex env)
			(if (directory-entry-deleted entry)
			    (push ex (car env)))
			nil)				;dont stop --
		    env
		    nil)				;zero uid NOT wanted
     (dolist (ex (car env))
       (let ((ans (expunge-directory-entry dir ex)))
	 (if (fixp ans)
	     (setq recs (+ recs ans))
	     (push ans excuses))))
     (with-filedata-addressibility-modifying (dir 0 dirh)
       (setf (dir-header-auto-expunge-last-time dirh) (time:get-universal-time))
       (write-out-buffer-from-addressor dirh))
     (values recs excuses)))

(defun listify-file-name-from-entry (dir entry entry-index)
  (list* (if (= (directory-entry-file-name-length entry)
		(directory-entry-file-name-true-length entry))
	     (directory-entry-file-name entry)
	     (multiple-value-bind (fd err)		;eyuck
	       (activate-file dir entry entry-index)
	       (if err (format nil "..Error getting name ~A ~A ~D" err dir entry-index)
		   (prog1 (or (fs-plist-get fd 'name)
			      (format nil "..Error no stored name ~A" (directory-entry-file-name entry)))
			  (check-deactivate-file fd)))))
	 (cond ((directory-entry-directory entry) ':directory)
	       ((directory-entry-file-type entry)))
	 (directory-entry-file-version entry)
	 (directory-entry-link entry)
	 (if (directory-entry-deleted entry)
	     '(:deleted)
	     nil)))

;;; Utilities of various sorts

(defun round-up-to-boundary (boundary quantity)
  (* (// (+ quantity boundary -1) boundary) boundary))

(defun round-down-to-boundary (boundary quantity)
  (* (// quantity boundary) boundary))


(defun lmfs-allocate-data-area (file whatsofar howmuch)
  (let ((bsize (partt-dataw-per-block (fd-partition-desc file))))
    (if (> howmuch bsize)
	(ferror nil "lmfs-allocate-data-area: won't fit in a block: ^D words" howmuch))
    (lmfs-allocate-header-area file whatsofar howmuch)))


;;;----------------------------------------------------------------------

(defun entry-index-to-offset (dir ex)
  (let* ((part (fd-partition-desc dir))
	 (dwpb (partt-dataw-per-block part))
	 (desize (fd-dir-entry-size dir))
	 (ent-per-block (// dwpb desize))

	 (entry-index-offset (fd-dir-entries-index-offset dir)))

    (let* ((rex (+ ex entry-index-offset))
	   (block (// rex ent-per-block))
	   (relent (\ rex ent-per-block)))
      (+ (* block dwpb) (* relent desize)))))


(defun set-dir-link-transparencies (dir specs)
  (with-filedata-addressibility-modifying (dir 0 dirh)
    (loop for (attribute val) on specs by 'cddr
	  do
	  (selectq attribute
	    (:read (setf (dir-header-default-link-transparencies-read-thru dirh) val))
	    (:write (setf (dir-header-default-link-transparencies-write-thru dirh) val))
	    (:rename (setf (dir-header-default-link-transparencies-rename-thru dirh) val))
	    (:create (setf (dir-header-default-link-transparencies-create-thru dirh) val))
	    (:delete (setf (dir-header-default-link-transparencies-delete-thru dirh) val))))
    (write-out-buffer-from-addressor dirh)
    (get-dir-link-transparencies dir)))

(defun get-dir-link-transparencies (dir &aux (ans nil))	;a primitive- had better be a dir
  (with-filedata-addressibility (dir 0 dirh)
    (if (dir-header-default-link-transparencies-read-thru dirh) (push ':read ans))
    (if (dir-header-default-link-transparencies-write-thru dirh) (push ':write ans))
    (if (dir-header-default-link-transparencies-delete-thru dirh) (push ':delete ans))
    (if (dir-header-default-link-transparencies-create-thru dirh) (push ':create ans))
    (if (dir-header-default-link-transparencies-rename-thru dirh) (push ':rename ans)))
  ans)


;;; Rename

;;; 9 June 1981

(defun rename-file-dfd (fd dir name type version
			   &aux (par (fd-parent fd))
			   (ex (fd-entry-index fd)))

  ;; Dir can't be null.  That feature is not of great urgency.
  ;; If dirs not eq, should check legality of move wrt master dirs etc.
  ;; I don't particularly like renaming directories either, but those shouldnt
  ;; get this far.  Cross-dir dir moves are absolutely out.

  (or (and dir par) (break rename-file-dfd--parentless-file))

  (multiple-value-bind (new-dire new-ex err crthru-fd newvers ggrc)
    (get-new-directory-entry dir name type version)
    (if err
	(values crthru-fd err)				;see create-file
	(protect-buffer-addressor (new-dire new-dire)
	  (with-fileheader-addressibility (fd 0 h)
	    (with-fileheader-addressibility-modifying (fd (file-header-dire-location h) hdire)
	      (setf (fd-file-name fd) name)
	      (store-file-name-from-fd fd hdire)
	      (if (not (eq (fd-file-type fd) ':directory))
		  (progn
		    (setf (fd-file-type fd) type)
		    (setf (directory-entry-file-type hdire) (fd-file-type fd))
		    (setf (fd-file-version fd) (if (numberp version) version newvers))
		    (setf (directory-entry-file-version hdire) (fd-file-version fd))))

	      (setf (fd-grc-info fd) (if (zerop ggrc) nil ggrc))

	      (with-fileheader-addressibility-modifying (fd (file-header-info-location h) info)
		(setf (fh-info-parent-dir-uid info) (fd-uid dir))
		(setf (fh-info-parent-dir-part-id info) (partt-partition-id (fd-partition-desc dir)))
		(setf (fh-info-parent-dir-address info) (fd-r0addr dir))
		(setf (fh-info-dir-entry-index-in-parent info) new-ex)

		(if (not (= (directory-entry-file-name-length hdire)
			    (directory-entry-file-name-true-length hdire)))
		    (fs-plist-put fd 'name name))		;may write out
	      
		(if (not (eq dir par))
		    (progn (setf (fd-sons par) (delq fd (fd-sons par)))
			   (setf (fd-parent fd) dir)
			   (push fd (fd-sons dir))))
		(setf (fd-entry-index fd) new-ex)

		(write-out-buffer-from-addressor-if-needed info))

	      (copy-directory-entry hdire new-dire)
	      (protect-buffer-addressor (old-dire (get-general-file-addressor
						   nil par (entry-index-to-offset par ex)))
		(setf (directory-entry-unique-ID old-dire) 0)
		(write-out-buffer-from-addressor old-dire))

	      (write-out-buffer-from-addressor-if-needed new-dire)
	      (write-out-buffer-from-addressor-if-needed hdire)))

	  (values t nil)))))


(defun delete-old-generations (fd)
  (let ((par (fd-parent fd)))
    (or par (fs-error "delete-old-generations: no parent" fd))
    (let* ((grc (or (fd-grc-info fd) (fs-error "delete-old-generations: null grc")))
	   (curtime (time:get-universal-time))
	   (limit (- (fd-file-version fd) grc)))
      (if (plusp limit)
	  (scan-directory
	   par
	   #'(lambda (entry ex env)
	       (let ((name (first env))
		     (type (second env))
		     (time (third env))
		     (limit (fourth env))
		     (dir (fifth env)))
		 (if (and (not (directory-entry-directory entry))
			  ;;decided that delete-thru links dont delete thru
			  (directory-entry-file-type-compare entry type)
			  (not (directory-entry-deleted entry))
			  (<= (directory-entry-file-version entry) limit);avoid name comp if can
			  (compare-name-to-directory-entry name  entry ex dir))
		     (progn
		      (set-bufmod-from-addressor entry)
		      (setf (directory-entry-date-time-deleted entry) time)
		      (setf (directory-entry-deleted entry) t)
		      ;; Can't be THAT bad.... avg one each time, right?
		      (write-out-buffer-from-addressor entry))))
	       nil)					;don't stop
	   `(,(fd-file-name fd) ,(fd-file-type fd) ,curtime ,limit ,par))))))

(defvar *dont-recurse-auto-expunge-this-dir* nil)	;scan-dir will try..

(defun check-dir-auto-expunge (dir dirh)
  (if (and (not (eq dir *dont-recurse-auto-expunge-this-dir*))
	   (> (time:get-universal-time) (+ (dir-header-auto-expunge-last-time dirh)
					   (dir-header-auto-expunge-interval dirh))))
      (let ((*dont-recurse-auto-expunge-this-dir* dir))
	(expunge-directory dir))))		;bitbucket the answers


(defun scan-dir-for-completion (fd name type options)	;no versions says mmcm..
  (let ((env (list name type options fd nil)))

    ;; first, get all the matchers. Then sort em out.
    (scan-directory
      fd
      #'(lambda (entry ex env)
	  (let ((name (first env))
		(type (second env))
		(options (third env))
		(dir (fourth env)))
	    ;; In terms of silly long name, complete only on first x.
	    (let ((namel (if name (string-length name) 0))
		  (typel (if type (string-length type) 0)))
	      (if (and (or (not (directory-entry-deleted entry))
			   (memq ':deleted options))
		       (or (null name)
			   ;; I guess a cleverer macro is needed out of defstorage here...
			   (%string-equal name 0 (directory-entry-file-name entry) 0 namel))
		       (or (null type)
			   (%string-equal type 0 (directory-entry-file-type entry) 0 typel)))
		  (push (listify-file-name-from-entry dir entry ex) (fifth env)))))
	  nil)
      env)
    (let ((unique-files))
      (dolist (namel (fifth env))
	(if (not
	      (dolist (test-namel unique-files)
		;; compare name and type
		(if (and (string-equal (car namel) (car test-namel))
			 (string-equal (cadr namel) (cadr test-namel)))
		    (return t))))
	    (push namel unique-files)))
      unique-files))) 				;return this to outer level
