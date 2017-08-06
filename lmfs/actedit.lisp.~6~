;-*- Mode:LISP; Package:LMFS -*-


;;; Filedesc trees

(defflavor fd-tree ((old-inferiors nil) (old-sons (gensym)) (old-buflist nil)
		    (old-bufdisp nil))
	   (tv:tree)
  (:init-keywords :object))

(defmethod (fd-tree :after :init) (plist)
  ;;??? I dont understand why the init keyword fails
  (if (get plist ':object) (setq tv:object (get plist ':object))))

(defmethod (fd-tree :buffer-list) ()
  (if (equal (with-fs-locked (fd-buffer-list tv:object)) old-buflist)
      old-bufdisp
      (setq old-bufdisp
	    (with-fs-locked (loop for buf in (fd-buffer-list tv:object)
				  collect
				  (or (and (memq buf old-buflist)
					   (dolist (old old-bufdisp)
					     (if (eq (funcall old ':object) buf)
						 (return old))))
				      (make-instance 'fb-tree ':object buf))
				  into obs
				  finally (progn
					    (setq old-buflist
						  (copylist
						    (fd-buffer-list tv:object)))
					    (return obs)))))))

(defmethod (fd-tree :scroll-item) (&optional (indent 0))
  (setq tv:indentation indent)
  (funcall-self ':line-redisplay)
  (list ()
	(tv:scroll-parse-item
	  ':mouse `(tv:tree-mouse ,self)
	  `(:function ,self (:print-string)))
	(and (fd-buffer-list tv:object)
	     (tv:scroll-maintain-list
	       `(lambda () (funcall ',self ':buffer-list))
	       `(lambda (tree) (funcall tree ':scroll-item ,(1+ indent)))))
	(tv:scroll-maintain-list `(lambda () (funcall ',self ':visible-inferiors))
				 `(lambda (tree)
				    (funcall tree ':scroll-item ,(1+ indent))))))

(defmethod (fd-tree :line-redisplay) ()
  (setq tv:print-string
	(format nil "~:[FILE~;DIR ~]~12O   ~VX~A"
		(eq (fd-file-type tv:object) ':directory)
		(%pointer tv:object)
		tv:indentation
		(afse-pathmake tv:object))))

(defun afse-pathmake (fd)
  (cond ((eq fd *the-root*) *PATH-DELIMITER*)
	((null (fd-parent fd)) (afse-pathmake-file fd))
	((eq (fd-parent fd) *the-root*)
	 (string-append *PATH-DELIMITER* (afse-pathmake-file fd)))
	(t (string-append (afse-pathmake (fd-parent fd)) *PATH-DELIMITER*
			  (afse-pathmake-file fd)))))

(defun afse-pathmake-file (fd)
  (cond ((eq (fd-file-type fd) ':directory) (fd-file-name fd))
	(t (format nil "~A~A~A~A~D"
		   (fd-file-name fd)
		   *TYPE-DELIMITER* (fd-file-type fd)
		   *VERSION-DELIMITER* (fd-file-version fd)))))

(defmethod (fd-tree :visible-inferiors) ()
  (if (with-fs-locked
	(equal old-sons (fd-sons tv:object)))
      old-inferiors
      (setq tv:inferiors
	    (setq old-inferiors
		  (loop for fd in
			(with-fs-locked
			  (setq old-sons
				(copylist (fd-sons tv:object))))
			collect
			(or (and (memq fd old-sons)
				 (dolist (old old-inferiors)
				   (if (eq (funcall old ':object) fd) (return old))))
			    (make-instance 'fd-tree
					   ':object fd ':superior self)))))))

;;File buffer trees

(defflavor fb-tree ()
	   (tv:tree)
  (:init-keywords :object))

(defmethod (fb-tree :after :init) (plist)
  ;;??? I dont understand why the init keyword fails
  (if (get plist ':object) (setq tv:object (get plist ':object))))

(defmethod (fb-tree :line-redisplay) ()
  (setq tv:print-string
	(let ((buf tv:object))
	  (format
	    nil
	    "~20X~VX Buf ~8O rc ~2D recadd ~6O HDR ~4O DATA ~5O ~:[Pure~;Mod~]~:[~; Cr~]"
	    tv:indentation
	    (%pointer buf)
	    (fb-reference-count buf) (fb-address buf)
	    (fb-lowest-header-addr buf) (fb-lowest-data-addr buf)
	    (fb-modified buf)
	    (fb-creating buf)))))



;;;The window flavor

(defflavor afse-mixin () ()
  (:included-flavors tv:basic-tree-scroll))

(defmethod (afse-mixin :set-hierarchy) (root partition)
  (funcall-self ':set-display-item
		(list ()
		      ;; The following two make-instance's are NOT simply for the
		      ;; purpose of calling them for ':scroll-item -- what they
		      ;; return in response to this message CONTAINS THEMSELVES!
		      (funcall (make-instance 'fd-tree ':object root) ':scroll-item)
		      (funcall (make-instance
				 'fd-tree ':object
				 (freemap-file (partt-free-record-info partition)))
			       ':scroll-item))))

(defmethod (afse-mixin :edit-object) (tree)
  (let ((object (funcall tree ':object)))
    (selectq (typep object)
      (file-desc  (afse-edit-file-desc object self))
      (file-buffer  (afse-edit-buffer object self))
      (t (tv:beep))))
  (tv:tree-edit-end-typeout))
 
(defmethod (afse-mixin :who-line-documentation-string) ()
  "L: Describe object       R:Menu")

(defmethod (afse-mixin :tree-interpret-blip) (blip)
  (selectq (first blip)
    (tv:tree-mouse
     (let ((tree (second (second blip))))
	 (selectq (fourth blip)
	   (#\mouse-1-1 (describe (funcall tree ':object))
			(tv:tree-edit-end-typeout)	   )
	   (#\mouse-2-1 (tv:beep))
	   (#\mouse-3-1 (funcall-self ':edit-object tree)))))))

(defvar *afse-edit-fd-default-item* nil)

(defun afse-edit-file-desc (fd window)
  (multiple-value-bind (choice item)
      (tv:menu-choose
	'(("Describe" :value :describe :documentation "Describe the file-desc")
	  ("Inspect"  :value :inspect :documentation "Inspect the file-desc")
	  ("Deactivate" :value :deactivate :documentation
	   "Attempt to deactivate the file-desc")
	  ("Subtree Deact" :value :subtree-deactivate
	   :documentation "Deactivate as much as possible of this subtree"))
	(format nil "File Desc Ops ~A" (afse-pathmake fd))
	'(:mouse)
	*afse-edit-fd-default-item*
	window)
    (and choice (setq *afse-edit-fd-default-item* item))
    (selectq choice
      (:deactivate     (if (not (file-theoretically-deactivateable-p fd))
			   (format t "~&File appears not to be deactivateable now.")
			   (check-deactivate-file fd)))
      (:subtree-deactivate
		       (with-fs-locked (deactivate-cleanup fd)))
      (t               (afse-edit-common choice fd window)))))


(defvar *afse-edit-buffer-default-item* nil)

(defun afse-edit-buffer (buf window)
  (multiple-value-bind (choice item)
      (tv:menu-choose
	'(("Describe" :value :describe :documentation "Describe the buffer")
	  ("Inspect"  :value :inspect :documentation "Inspect the buffer")
	  ("Write" :value :write :documentation "If modified, write out this buffer")
	  ("Disconnect" :value :disconnect :documentation
	   "If possible, disconnect buffer from file"))
	"Buffer Ops"
	'(:mouse)
	*afse-edit-buffer-default-item*
	window)
    (and choice (setq *afse-edit-buffer-default-item* item))
    (selectq choice
      (:write        (afse-buffer-write buf))
      (:disconnect   (afse-buffer-disconnect buf))
      (t (afse-edit-common choice buf window)))))

(defun afse-buffer-write (buf)
  (or (with-fs-locked
	(cond ((fb-modified buf)
	       (write-out-file-buffer buf)
	       t)
	      (t nil)))
      (format t "~&Buffer not modified, won't write anything")))

(defun afse-buffer-disconnect (buf)
  (let ((ans
	  (with-fs-locked
	    (cond ((fb-modified buf) "Won't disconnect, buffer needs writing")
		  ((not (zerop (fb-reference-count buf)))
		   (format nil "Won't disconnect, buffer has nonzero reference count of ~D."
			   (fb-reference-count buf)))
		  ((null (fb-file-desc buf))
		   "Buffer very mysteriously has no file pointer.")
		  (t (disconnect-buffer-from-file buf) nil)))))
    (if ans (format t "~&~A" ans))))

(defun afse-edit-common (choice obj ignore)
  (selectq choice
    (nil)
    (:describe       (describe obj))
    (:inspect        (inspect obj))))

(compile-flavor-methods afse-mixin fb-tree fd-tree)
