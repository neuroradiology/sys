;;; <LMFS>PLIST.LSP;1 27-May-81 10:47:51, Edit by BSG -*-Package:LMFS; Mode:LISP-*-


;;; Copyright (c) Symbolics, Inc., 1981

;;; All that hath to do with LMFS file-plists.

(defconst *MINIMUM-MEANINGFUL-FS-PLIST-BLOCK-SIZE*
  (+ (plist-block-size-in-words) 20.))

(defconst *PLIST-BLOCK-VERSION* 1)

(defconst *MAXIMUM-ALLOWABLE-IND-AND-PROP-LEN* 512.)

;;; FS-PLIST-INIT   -- the allocation areas shall be initted, and the first
;;; (empty) plist block made up.

(defun fs-plist-init (fd h &aux (last-plb nil))
  (let* ((dwpb (partt-dataw-per-block (fd-partition-desc fd)))
	 (lowstart (file-header-property-list-location h))
	 (highlim (+ lowstart (file-header-property-list-length h))))
    (do nil (nil)					;loop through these blox
      (let* ((roundedend (round-up-to-boundary dwpb (1+ lowstart)))
	     (curend (min roundedend highlim))
	     (this-piece (- curend lowstart)))
	(cond ((< this-piece *MINIMUM-MEANINGFUL-FS-PLIST-BLOCK-SIZE*)
	       (if last-plb (return nil))	;fine, just punt or normal terminate
	       (break intolerably-small-plist-allocation-area))
	      (t
	       (if last-plb (setf (plist-block-next-fragment last-plb) lowstart))
	       (with-fileheader-addressibility-modifying (fd lowstart plb)
		 (fs-plist-init-plb plb lowstart this-piece)
		 (if (and last-plb
			  (not (eq (buffer-from-addressor plb)
				   (buffer-from-addressor last-plb))))
		     (write-out-buffer-from-addressor last-plb))
		 (setq last-plb
		       (prog1 (upreference-addressor plb)
			      (if last-plb (downreference-addressor last-plb)))))))
	(setq lowstart (+ lowstart this-piece))))
    (if last-plb
	(progn
	  (write-out-buffer-from-addressor last-plb)
	  (downreference-addressor last-plb)))))


(defun fs-plist-init-plb (plb start size)
  (setf (plist-block-version plb) *PLIST-BLOCK-VERSION*)
  (setf (plist-block-total-length plb) size)
  (setf (plist-block-lowest-high-word-allocated plb) (+ start size))
  (setf (plist-block-lowest-free-word plb) (+ start (plist-block-size-in-words)))
  (setf (plist-block-perceived-words-deallocated plb) 0)
  (setf (plist-block-next-fragment plb) 0)
  (setf (plist-block-number-of-properties plb) 0))


(defun fs-plist-block-allocate-high (plb size)		;addr or nil
  (let ((whatsleft (- (plist-block-lowest-high-word-allocated plb)
		      (plist-block-lowest-free-word plb))))
    (if (< whatsleft size)
	nil
	(let ((newbot (- (plist-block-lowest-high-word-allocated plb) size)))
	  (prog1 newbot
		 (setf (plist-block-lowest-high-word-allocated plb) newbot))))))

(defmacro return-if (x)					;i've needed this
  (let ((g (gensym)))					;many times --
    `(let ((,g ,x))
       (if ,g (return ,g)))))

(defun fs-plist-get (fd prop)
  (setq prop (string prop))			;new way
  (with-fileheader-addressibility (fd 0 fh)
    (do ((plbp (file-header-property-list-location fh))) ((zerop plbp) nil)
      (with-fileheader-addressibility (fd plbp plb)
	(return-if
	 (do n 0 (1+ n) (= n (plist-block-number-of-properties plb))
	     (if (let ((iptr (plist-block-props-indicator plb n)))
		   (and (not (zerop iptr))
			(with-fileheader-addressibility (fd iptr istring)
			 (vstring-data-compare istring prop))))

		 (return
		  (with-fileheader-addressibility
		   (fd (plist-block-props-property plb n) pstring)

		   (vstring-data pstring))))))
	(setq plbp (plist-block-next-fragment plb))))))

(defun fs-plist-compare (fd prop val)
  (setq prop (string prop))			;new way
  (with-fileheader-addressibility (fd 0 fh)
    (do ((plbp (file-header-property-list-location fh))) ((zerop plbp) nil)
      (with-fileheader-addressibility (fd plbp plb)
	(return-if
	 (do n 0 (1+ n) (= n (plist-block-number-of-properties plb))
	     (if (let ((iptr (plist-block-props-indicator plb n)))
		   (and (not (zerop iptr))
			(with-fileheader-addressibility (fd iptr istring)
			 (vstring-data-compare istring prop))))

		 (return
		  (with-fileheader-addressibility
		   (fd (plist-block-props-property plb n) pstring)

		   (vstring-data-compare pstring val))))))
	(setq plbp (plist-block-next-fragment plb))))))

(defun fs-plist-put (fd ind val &optional dont-write-flag
			&aux
			(best-block nil) best-block-addr (best-block-amt 999999.)
			(remove-block nil)
			(req-word-size
			 (+ 1 ;; 1 for the new pair word
			    (fs-plist-alloclen ind)
			    (fs-plist-alloclen val))))
  (if (null val)
      (fs-plist-remove fd ind)
      (progn

       (or (stringp val)
	   (and (symbolp val) (setq val (string val)))
	   (setq val (let ((base 10.) (*nopoint nil))(format nil "~S" val))))
       (setq ind (string ind))			;new way is accept symbols

       ;; Latest theory.  Try to find a block where it fits.  If you find the old
       ;; prop, purge it. If you find a zero slot, that's nice, if you don't,
       ;; well, that's nice too.

       (with-fileheader-addressibility (fd 0 fh)
         (with-fileheader-addressibility (fd (file-header-dire-location fh) dire)
           (if (not (directory-entry-properties-were-put dire))
	       (update-file-attributes fd ':there-are-properties)))

	 (do ((plbp (file-header-property-list-location fh))
	       (zslot-found))
	      ((zerop plbp) nil)

	    (with-fileheader-addressibility (fd plbp plb)
	      (do n 0 (1+ n) (= n (plist-block-number-of-properties plb))
		  (let ((iptr (plist-block-props-indicator plb n)))
		    (cond ((and (zerop iptr)(null zslot-found))
			   (setq zslot-found n))
			  ((with-fileheader-addressibility (fd iptr istring)
			     (vstring-data-compare istring ind))
			   (fs-plist-rem-internal fd plb n)
			   (set-bufmod-from-addressor plb)
			   (setq remove-block (upreference-addressor plb))
			   (or zslot-found (setq zslot-found n))
			   (return t)))))

	      ;; Wasn't found in this block.  Remember how much space in this block
	      ;; in case we have to allocate anew.
	      (let ((free-amount (+ (plist-block-perceived-words-deallocated plb)
				    (- (plist-block-lowest-high-word-allocated plb)
				       (plist-block-lowest-free-word plb)))))
		(cond ((< free-amount req-word-size))	;forget it
		      ((> free-amount best-block-amt))	;somebody was smaller..
		      (t				;ok, remember this block
		       (if best-block (downreference-addressor best-block))
		       (setq best-block (upreference-addressor plb))
		       (setq best-block-addr plbp)
		       (setq best-block-amt free-amount))))
	      (setq plbp (plist-block-next-fragment plb))))

	 ;; Take the best block and allocate it there

	 (cond ((null best-block)
		(grow-header-for-plist fd)
		(fs-plist-put fd ind val))	;eat it, kid.
	       (t
		(protect-buffer-addressor (plb best-block)
		  (if (> req-word-size (- (plist-block-lowest-high-word-allocated plb)
					   (plist-block-lowest-free-word plb)))

		      ;; Reconstruct the block to claim space

		      (let ((cur-props
			     (loop for n from 0 below (plist-block-number-of-properties plb)
				   collect (cons (plist-block-props-indicator plb n)
						 (plist-block-props-property plb n)))))
			(fs-plist-init-plb plb
					   best-block-addr
					   (plist-block-total-length plb))
			(loop for (ind . val) in cur-props
			      and n from 0 by 1
			      do (fs-plist-pair-put plb n ind val fd))))


		  (loop for n from 0 below (plist-block-number-of-properties plb)
			if (zerop (plist-block-props-indicator plb n))
			do (return (fs-plist-pair-put plb n ind val fd))
			finally
			(progn (fs-plist-pair-put plb n ind val fd)
			       (setf (plist-block-number-of-properties plb) (1+ n))))

		  (set-bufmod-from-addressor best-block)
		  (if (null dont-write-flag)
		      (progn
		       (write-out-buffer-from-addressor-if-needed best-block)
		       (if remove-block
			   (progn
			    (write-out-buffer-from-addressor-if-needed remove-block)))))
		  (if remove-block (downreference-addressor remove-block)))))))))

(defun grow-header-for-plist (fd)
  (let ((last-plbp
	  ;; If UNWIND-PROTECT were compiled properly, this would be a lot easier.
	  (with-fileheader-addressibility (fd 0 fh)	;find last
	    (do ((plbp (file-header-property-list-location fh))
		 (next-plbp))
		()
	      (if (zerop
		    (with-fileheader-addressibility (fd plbp plb)
		      (setq next-plbp (plist-block-next-fragment plb))))
		  (return plbp)
		  (setq plbp next-plbp))))))
    (multiple-value-bind (new-plbp size)
	(allocate-new-header-block fd)
      (with-fileheader-addressibility-modifying (fd new-plbp plb)
	(fs-plist-init-plb plb new-plbp size)
	(write-out-buffer-from-addressor plb))
      (with-fileheader-addressibility-modifying (fd last-plbp last-plb)
	(setf (plist-block-next-fragment last-plb) new-plbp)
	(write-out-buffer-from-addressor last-plbp)))))

(defun fs-plist-pair-put (plb n ind val fd)
  (if (= n (plist-block-number-of-properties plb))
      (setf (plist-block-lowest-free-word plb)
	    (1+ (plist-block-lowest-free-word plb))))
  (setf (plist-block-props-indicator plb n)
	(fs-plist-str-allocate fd plb ind))
  (setf (plist-block-props-property plb n)
	(fs-plist-str-allocate fd plb val)))

(defun fs-plist-alloclen (string)
  (// (+ 2 (string-length string) (1- *BYTES-PER-WORD*))
      *BYTES-PER-WORD*))

(defun fs-plist-str-allocate (fd plb val)
  (let ((allocptr
	 (fs-plist-block-allocate-high plb (fs-plist-alloclen val))))
    (if (null allocptr) (break fs-plist-str-allocate-lost))
    (with-fileheader-addressibility-modifying (fd allocptr vsp)
      (setf (vstring-data vsp) val))
    allocptr))

(defun fs-plist-str-deallocate (fd plb ptr)
  (setf (plist-block-perceived-words-deallocated plb)
	(+ (plist-block-perceived-words-deallocated plb)
	   (with-fileheader-addressibility (fd ptr vsp)
	     (// (+ 2 (vstring-data-length vsp) (1- *BYTES-PER-WORD*))
		 *BYTES-PER-WORD*)))))

(defun fs-plist-remove (fd ind)
  (setq ind (string ind))
  (with-fileheader-addressibility (fd 0 fh)
    (do ((plbp (file-header-property-list-location fh)))
	((zerop plbp) nil)

      (with-fileheader-addressibility (fd plbp plb)
	(do n 0 (1+ n) (= n (plist-block-number-of-properties plb))
	    (let ((iptr (plist-block-props-indicator plb n)))
	      (cond ((with-fileheader-addressibility (fd iptr istring)
		       (vstring-data-compare istring ind))
		     (fs-plist-rem-internal fd plb n)
		     (return t)))))

	(setq plbp (plist-block-next-fragment plb))))))

(defun fs-plist-rem-internal (fd plb n)
  (fs-plist-str-deallocate fd plb (plist-block-props-property plb n))
  (fs-plist-str-deallocate fd plb (plist-block-props-indicator plb n))
  (setf (plist-block-props-property plb n) 0)
  (setf (plist-block-props-indicator plb n) 0))




(defun file-plist (fd)
  (with-fileheader-addressibility (fd 0 h)
      (do ((plbp (file-header-property-list-location h))
	   (pl))
	  ((zerop plbp) pl)
	(with-fileheader-addressibility (fd plbp plb)
	  (loop for i from 0 below (plist-block-number-of-properties plb)
		do
		(let ((iloc (plist-block-props-indicator plb i))
		      (ploc (plist-block-props-property plb i)))
		  (if (not (zerop iloc))
		      (progn
			(with-fileheader-addressibility (fd ploc p)
			  (push (vstring-data p) pl))
			(with-fileheader-addressibility (fd iloc p)
			  (push (vstring-data p) pl))))))
	  (setq plbp (plist-block-next-fragment plb))))))
