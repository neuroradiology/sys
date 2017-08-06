;-*- Mode:LISP; Package:LMFS -*-

;;; LMFS Salvage/reclaim functions

;;; Copyright (c) 1981, Symbolics Inc.

;;; Currently have not addresssed issues of running this concurrently with
;;; other FS users.

(defvar salvabit-arrays nil)			;he shall SAVE salvager bit arrays...
(defvar *bitsalv-err-trace* nil)
(defvar *STD-DISK-JUNK* #O30500000000)

(defstruct (salv-instance :named (:conc-name svi-))
  partition					;of interest
  known-in-use					;bits ON for known in use
  alloc-by-system				;system allocated while running
  dealloc-by-system				;system deallocated while running
  buflist					;list of salv buffers, refct = 1.
  limit						;how many
  nbufs
  start						;starting addr
  current-address				;into partition
  lockptr					;for sync ... tbd
  in-use-count					;as seen in use---
  files-found-meter
  dirs-found-meter
  disk-error-records
  scavenge-p
  scavenge-ht
  scavenge-rootlist
  thats-all-for-now)

(defresource (scavenge-ht t)
	     (make-equal-hash-table ':size 1000.))


(defstruct (scavenge-fd :named (:conc-name scfd-))
  uid
  (found-in-part nil)				;make 'em for unfounds, too.
  (sons nil)					;r0addr when found
  (par nil)
  ex)						;claimed entry index

(defun salv-msg (severity format-ctl &rest args)
  (format t "~%~D ~A ~A" severity (time:print-universal-time (time:get-universal-time) nil)
	  (lexpr-funcall #'format nil format-ctl args)))

(defun bitsalv-part (partition &optional scavenge &aux svi)
  ;; Reconstruct free-record info of partition, optionally scavenge
  
  (setq svi (make-salv-instance
	      partition          partition
	      buflist            nil
	      limit              (partt-disk-addr-limit partition)
	      start              (freemap-file-bit-zero-recno
				   (partt-free-record-info partition))
	      in-use-count       0
	      files-found-meter  0
	      dirs-found-meter   0
	      disk-error-records 0
	      lockptr            (locf (car (ncons nil)))
	      scavenge-p	 scavenge
	      nbufs              4))
  (setf (svi-current-address svi) (svi-start svi))
  ;; Get and clear the three arrays.
  
  (setf (svi-known-in-use svi) (get-salv-array (svi-limit svi)))
  (setf (svi-alloc-by-system svi) (get-salv-array (svi-limit svi)))
  (setf (svi-dealloc-by-system svi) (get-salv-array (svi-limit svi)))
  
  (salv-msg 0 "Beginning freemap reconstruction~:[ with reverse connection check~]."
	    (null (svi-scavenge-p svi)))

  ;; Now get some buffers.
  
  (unwind-protect
    (progn
      (if (svi-scavenge-p svi)
	  (progn
	    (setf (svi-scavenge-ht svi) (allocate-resource 'scavenge-ht))
	    (setf (svi-scavenge-rootlist svi) nil)
	    (clrhash-equal (svi-scavenge-ht svi))))
      
      (dotimes (i (svi-nbufs svi))
	(let ((buf (allocate-random-buffer)))
	  (push buf (svi-buflist svi))
	  (setf (fb-record-length buf)
		(- (partt-record-size-words partition)
		   (* 2 (partt-record-size-blocks partition)
		      (block-check-words-size-in-words))))
	  (setf (fb-address buf) nil)
	  (setf (fb-file-desc buf)
		(make-file-desc
		  partition-desc   (svi-partition svi)
		  parent           nil
		  file-name        (format nil "Bitsalv bogus file desc#~D" i)
		  buffer-list      (list buf)))
	  ;; rc should be zero, (that all works, thank god), so can use rc as freeing criterion
	  (upreference-file-buffer buf)))	;dont let addressings free the buf..
      
      (loop until ( (svi-current-address svi) (svi-limit svi))
	    do
	    (bitsalv-fill-file-buffers svi)
	    (dolist (buf (svi-buflist svi))
	      (if (fb-address buf) (bitsalv-process-file-buffer svi buf))))
      
      ;; Here's where we gotta start locking against other active frobozzes when
      ;; we truly run parallel.
      
      (wring-freemap partition)			;get map on disk accurate
      (freemap-assume-salvabit partition (svi-known-in-use svi))	;salvabit, assumpsit
      
      ;; Unlock here.
      
      (if (svi-scavenge-p svi)
	  (scavenge-pass-2 svi)))
    
    (salv-msg 0 "End of reconstruction.")
    (progn
      (if (and (svi-scavenge-p svi) (svi-scavenge-ht svi))
	  (deallocate-resource 'scavenge-ht (svi-scavenge-ht svi)))
      (free-salv-arrays (svi-known-in-use svi) (svi-alloc-by-system svi) (svi-dealloc-by-system svi))
      (dolist (buf (svi-buflist svi))
	(setf (fb-file-desc buf) nil)
	(setf (fb-reference-count buf) 0)
	(free-file-buffer buf))
      (setf (svi-buflist svi) nil))))

(defun get-salv-array (size)
  (let ((ary
	  (or
	    (without-interrupts
	      (dolist (ary salvabit-arrays)
		(if ( (array-length ary) size)
		    (return (prog1 ary (setq salvabit-arrays (delq ary salvabit-arrays)))))))
	    (make-array size ':type 'art-1b))))
    (copy-array-contents "" ary)		;Moon hack
    ary))

(defun free-salv-arrays (&rest arrays)
  (dolist (ary arrays)  (without-interrupts (push ary salvabit-arrays))))


(defun bitsalv-fill-file-buffers (svi &aux bufs-to-go (did 0))
  (dolist (buf (svi-buflist svi)) (setf (fb-address buf) nil))
  (loop for recno from (svi-current-address svi) below (svi-limit svi)
	and buf in (svi-buflist svi)
	do
	(setf (fb-address buf) recno)
	(push buf bufs-to-go)
	(incf did))
  (setq bufs-to-go (nreverse bufs-to-go))
  (if (*catch
	'disk-lossage
	(lexpr-funcall #'internal-salv-disk-read svi
		       (svi-current-address svi)
		       (mapcar 'fb-array bufs-to-go))
	nil)
      (loop for buf in bufs-to-go
	    do
	    (if (*catch 'disk-lossage
		  (internal-salv-disk-read svi (fb-address buf) (fb-array buf))
		  nil)
		(progn
		  (incf (svi-disk-error-records svi))
		  (setf (fb-address buf) nil)))))
  (incf (svi-current-address svi) did))

(defun internal-salv-disk-read (svi addr &rest bufs &aux (part (svi-partition svi)))
  (condition-bind ((:disk-error #'(lambda (&rest ignore) (*throw 'disk-lossage t))))
    (dolist (buf bufs)
      (lexpr-funcall #'file-disk-read
		     (partt-device-id part)
		     (+ (* addr (partt-record-size-blocks part))
			(partt-address-base-in-blocks part))
		     bufs))))

(defstorage (words)
	    (words  array (*) fixnum))

(defun bitsalv-process-file-buffer (svi buf &aux (part (svi-partition svi)) file-id
				    (array (fb-array buf)))
  (upreference-file-buffer buf)			;below doesnt do this
  
  ;; Try to psych out whether this guy is really a header.

  (protect-buffer-addressor (adr (obtain-8bit-addrarray buf))
    (set-word-address-in-addressor adr array 0)	;look at check words
    (if (and (not (zerop (setq file-id (block-check-words-file-id adr))))
	     (not (and
		    (= (words-words adr 0) *STD-DISK-JUNK*)
		    (= (words-words adr 2) *STD-DISK-JUNK*)))
	     (zerop (block-check-words-word-0s-rel-addr adr))
	     (block-check-words-headerp adr)
	     (progn (set-word-address-in-addressor
		      adr
		      array
		      (- (partt-record-size-words part)
			 (block-check-words-size-in-words)))
		    (= file-id (block-check-words-file-id adr)))
	     (progn (set-word-address-in-addressor
		      adr
		      array
		      (- (partt-block-size-words part)
			 (block-check-words-size-in-words)))
		    (= file-id (block-check-words-file-id adr)))
	     ;	     (or (and
	     ;	   (= (block-check-words-word-0s-rel-addr adr)
	     ;     (- (partt-block-size-words part) (* 2 (block-check-words-size-in-words)))))
	     )
	;;eyup
	(*catch
	  'file-header-loses
	  (bitsalv-process-file-header svi buf file-id)))))

(defun bitsalv-process-file-header (svi buf uid &aux (fd (fb-file-desc buf)) hrecs
				    (part (svi-partition svi)))

  ;;Well, gang, at this time, we know we have the record 0 of a file header in our
  ;;paws. Link up some cruft to make the file-system work.

  (setf (fd-r0addr fd) (fb-address buf))	;see above
  (setf (fd-logical-header-length fd) 999999.)	;see principal kludgery in PATHS.LISP
  (setf (fd-uid fd) uid)

  ;; O.K... now paw through the header..

  (set-up-buf-as-header-containing buf fd 0)	;pre-fix up addressing messes...hahaha
  (with-fileheader-addressibility (fd 0 h)
     (if (not (= (file-header-version h) 1))
	 (bitsalv-header-error "File Header wrong version (o)~O" (file-header-version h) ))
     (incf (svi-files-found-meter svi))
     (setf (fd-logical-header-length fd) (file-header-logical-size h))
     (set-up-buf-as-header-containing buf fd 0)	;fix up addressing messes...
     (let ((direloc (file-header-dire-location h)))
       (if ( direloc (fd-logical-header-length fd))
	   (bitsalv-header-error "Directory entry claimed to be in suspicious location"))
       (with-fileheader-addressibility (fd direloc dire)
         (if (not (= uid (directory-entry-unique-ID dire)))
	     (bitsalv-header-error "Directory entry uid doesn't match"))
	 (if (directory-entry-directory dire) (incf (svi-dirs-found-meter svi))))
       
       ;; Well, I guess we're willing to believe that at least this record's really
       ;; in use.

       (bitsalv-mark-in-use svi (fb-address buf))

       ;; Let's munge thru the header map..

       (with-fileheader-addressibility (fd (file-header-header-fm-location h) hfm)
         (setq hrecs (file-map-valid-length hfm))
	 (if (or (zerop hrecs)
		 (> hrecs (// (round-up-to-boundary (partt-dataw-per-record part)
						    (fd-logical-header-length fd))
			      (partt-dataw-per-record part))))
	     (bitsalv-header-error "Header file map record count suspicious ~D" hrecs))
	 (loop for rno from 1 to (1- hrecs)
	       do
	       (if (null (bitsalv-mark-in-use svi (file-map-element hfm rno)))
		   (bitsalv-header-error "Reused address in header ~D"
					 (file-map-element hfm rno)))))

       ;; That seemed to work. So at least the whole header is unambiguously addressible.
       ;; Process the "regular" file map. WORK NEEDED HERE when grow file maps...
       
       (with-fileheader-addressibility (fd (file-header-file-map-location h) fm)
	 (bitsalv-process-fm svi fd fm)))
     (if (svi-scavenge-p svi)
	 (scavenge-file-header svi fd h))))

(defun bitsalv-process-fm (svi fd fm)
  (loop for rno from 0 below (file-map-valid-length fm)
	do
	(if (null (bitsalv-mark-in-use svi (file-map-element fm rno)))
	    (bitsalv-header-error "Reused address in file ~D"
					 (file-map-element fm rno))))

  ;; I hope to hell this thing finds the rest of its own header if it a'int in r0 --
  (if (not (zerop (file-map-link fm)))
      (with-fileheader-addressibility (fd (file-map-link fm) ffm)
	(bitsalv-process-fm svi fd ffm))))

(defun bitsalv-header-error (&rest args)
  (let ((message (lexpr-funcall #'format nil args)))
    (if *bitsalv-err-trace* (format t "~%~A" message))
    (*throw 'file-header-loses message)))

(defun bitsalv-mark-in-use (svi addr)
  (and (plusp addr)
       (< addr (svi-limit svi))
       ( addr (svi-start svi))
       (zerop (aref (svi-known-in-use svi) (- addr (svi-start svi))))
       (progn
	 (aset 1 (svi-known-in-use svi) (- addr (svi-start svi)))
	 (incf (svi-in-use-count svi))
	 t)))



;;; Maybe this goes in RECORDS.LISP??
(defun freemap-assume-salvabit (part array
				&aux (map (partt-free-record-info part))
				(base (freemap-file-bit-zero-recno map))
				(mapfile (freemap-file map))
				(nblocks (freemap-file-nblocks map))
				(bsize (partt-dataw-per-block part))
				(nbits-per-word (freemap-file-n-bits-per-word map))
				(bufs nil)
				(mask (1- (ash 1 nbits-per-word))))
  (loop for blocknum from 1 to nblocks
	do
	(with-filedata-addressibility-modifying (mapfile (* bsize blocknum) mb)
	  (let ((buf (buffer-from-addressor mb)))
	    (or (memq buf bufs) (push buf bufs)))
	  
	  (loop for wordno  from 0 below (freemap-block-number-of-words mb)
		and recno from (freemap-block-recno-of-bit-0 mb) by nbits-per-word
		with nrecs-covered = (- (freemap-block-highest-recno+1 mb)
					(freemap-block-recno-of-bit-0 mb))
		with locsum = 0
		finally (progn
			  (setf (freemap-block-checksum mb) (fs-freemap-block-checksum
							      mb nbits-per-word))
			  (setf (freemap-block-number-of-free-recs mb) locsum)
			  (return nil))
		do
		(let ((my-suggestion
			(if (eq array ':REALLY-AND-TRULY-FREE-EVERYTHING)
			    mask
			    (logxor mask (extract-bits-from-bitarray array
								     (- recno base)
								     mask)))))
		  (if (and (= wordno (1- (freemap-block-number-of-words mb)))	;last word
			   (not (zerop (\ nrecs-covered nbits-per-word))))
		      (setf my-suggestion
			    (logand my-suggestion
				    (1- (ash 1 (\ nrecs-covered nbits-per-word))))))
		  (setf (freemap-block-array mb wordno) my-suggestion)
		  (incf locsum  (bits-on-per-word my-suggestion))))))
  (mapc 'write-out-file-buffer bufs))

(DEFVAR *INDIRECT-dummy* (make-array 32. ':type 'art-1b))

(defresource (indirect-16 t)
	     (make-array 400000 ':type 'art-16b ':displaced-to *indirect-dummy*))

(defun extract-bits-from-bitarray (array offset mask)
  (with-resource (indirect-16 indir)
    (si:change-indirect-array indir 'art-16b '(400000) array nil)
    (let ((bign (+ (aref indir (// offset 16.))
		   (ash (aref indir (1+ (// offset 16.))) 16. ))))
      (logand mask (ash bign (- (\ offset 16.)))))))


;;; Scavenger

(defun scavenge-get-scfd (ht uid)

  (let* ((scfd (gethash-equal uid ht)))

    ;; Unconditionally let there be an scfd. There are enough flags to
    ;; know if it's new or not.

    (or scfd
	(puthash-equal uid (make-scavenge-fd uid uid) ht))))

    
;;; Build a replica of this partition's hierarchy in "core".


(defun scavenge-file-header (svi fd h)
  (let ((scfd (scavenge-get-scfd (svi-scavenge-ht svi) (fd-uid fd))))
    (if (not (scfd-found-in-part scfd))		;I should hope not..
	(progn
	  (setf (scfd-found-in-part scfd) (fd-r0addr fd))
	  ;; Must be new at this point.
	  (let ((fh-location  (file-header-info-location h)))
	    (if (< fh-location (fd-logical-header-length fd))
		;; Simple check for gubbish

		(with-fileheader-addressibility (fd fh-location fhi)
		  (if (= (fh-info-duplicate-uid fhi) (fd-uid fd))	;check again
		      (let ((par-uid (fh-info-parent-dir-uid fhi)))
			(if (zerop par-uid)
			    (push scfd (svi-scavenge-rootlist svi))
			    (progn
			      (setf (scfd-ex scfd) (fh-info-dir-entry-index-in-parent fhi))
			      (let ((par-scfd
				      (scavenge-get-scfd (svi-scavenge-ht svi) par-uid)))
				(setf (scfd-par scfd) par-scfd)
				(push scfd (scfd-sons par-scfd))))))))))))))

;;; Called after whole partition is passed through. Note that freemap is guaranteed ok.

(defvar **global-rootl** nil)			;lexoscope wanted here..

(defun scavenge-pass-2 (svi)
  (salv-msg 0 "Beginning scavenge connection check//reconstruct.")
  (salv-msg 0 "Beginning hunt for orphans")
  (let ((**global-rootl** nil))
    (maphash-equal #'(lambda (ignore scfd)
		       (if (null (scfd-found-in-part scfd)) (push scfd **global-rootl**)))
		   (svi-scavenge-ht svi))
    (if **global-rootl** (scav-repatriator svi **global-rootl**))
    (salv-msg 0 "End orphan hunt."))

  (dolist (root (svi-scavenge-rootlist svi))
    (if (> (scfd-uid root) 1000)		;not magic file (e.g., vol map)
	(scavenge-root svi root))))



(defun scavenge-root (svi scfd)
  (if (= (scfd-uid scfd) (fd-uid *the-root*))
      (scavenge-dir svi scfd *the-root*)
      ;; Were concurrency being done right here, we would search down/activate
      ;; along the current active data structure from the UID path, which is basically
      ;; what the UID path was put in there anyway for.  However, until then...

      (multiple-value-bind (fd err)
	  (activate-file-from-header nil (svi-partition svi) (scfd-found-in-part scfd)
				     (format nil "Scavenge Activation ~O" (scfd-uid scfd))
				     (scfd-uid scfd))
	(if err
	    (format t "~%~A Error activating ~O for scavenge, record address ~O:~%~A"
		    (scfd-uid scfd) (scfd-found-in-part scfd) err)
	    (scavenge-dir svi scfd fd)))))

(defun scavenge-dir (svi scfd fd)
;;; Probably a disk error and lmfs-data-error handler here would be a good move

  (if (not (eq (fd-file-type fd) ':directory)) 
      (salv-msg 3 "Attempt to dir-process that which is not a dir ~A ~A ~D"
	      (fd-file-name fd) (fd-file-type fd) (fd-file-version fd))
      (progn
	(dolist (son (scfd-sons scfd))
	  (scavenge-entry svi fd son))
	;; Conserve buffers
	(flush-buffers-for-file fd)
	(dolist (son (scfd-sons scfd))
	  (if (scfd-sons son)
	      (with-filedata-addressibility
		(fd (entry-index-to-offset fd (scfd-ex son)) entry)

		(multiple-value-bind (sfd err)
		    (activate-file fd entry (scfd-ex son))
		  (if err
		      (salv-msg 1 "Cannot activate ~S~%     ~A"
				(listify-file-name-from-entry fd entry (scfd-ex son))
				err)
		      (progn
			(scavenge-dir svi son sfd)
			(check-deactivate-file sfd))))))))))

(defun scavenge-entry (svi fd scfd)
  ;; Here we need some option to always update the dir entry from the header.
  ;; Until we figure out what we want here, checking the UID oughtta do it.

  (let ((ex (scfd-ex scfd))
	(uid (scfd-uid scfd))
	(r0addr (scfd-found-in-part scfd)))
    (let ((entry-data-offset (entry-index-to-offset fd ex)))
      (if ( entry-data-offset (fd-addressible-length fd))
	  (progn
	    (salv-msg 2 "Entry index ~o out of bounds on ~S, UID ~O, will add."
		      ex fd uid)
	    (scav-add-dire svi fd uid r0addr))
	  (with-filedata-addressibility (fd entry-data-offset entry)
	    (cond ((not (= uid (directory-entry-unique-ID entry)))
		   (salv-msg 2 "Entry missing from ~S index ~D., UID ~O, will add." fd ex uid)
		   (scav-add-dire svi fd uid r0addr))
		  ((not (= r0addr (directory-entry-record-0-address entry)))
		   (salv-msg 2 "Record 0 Address wrong at ~S index ~D., UID ~O, will fix"
			     (listify-file-name-from-entry fd entry ex) ex uid))))))))
		  ;; Otherwise, fine, no action needed.

(defun scav-add-dire (svi par uid r0addr)
  (multiple-value-bind (fd err)
      (activate-file-from-header par (svi-partition svi) r0addr "Repatriation Activation" uid)
    (if err
	(salv-msg 2 "Cannot activate orphan entry to ~S UID ~O r0addr ~O~%~A"
		  par uid r0addr err)
	(with-fileheader-addressibility (fd 0 h)
	  (salv-msg 0 "Adding ~A ~A ~D to ~S r0addr ~O"
		    (fd-file-name fd) (fd-file-type fd) (fd-file-version fd) par r0addr)

	  (with-fileheader-addressibility (fd (file-header-dire-location h) hdire)

	    ;; Could search the dir linearly here for the uid, in case it is only
	    ;; ex which is wrong.

	    (multiple-value-bind (dire ex xerr ignore ignore ignore)
		(get-new-directory-entry par (fd-file-name fd)
					 (fd-file-type fd)
					 (fd-file-version fd))
	      (if xerr
		  (salv-msg 2 "Cannot add ~A ~A ~D to ~S~%~A"
			    (fd-file-name fd) (fd-file-type fd)
			    (fd-file-version fd) par xerr)
		  (progn
		    (setf (fd-entry-index fd) ex)
		    (with-fileheader-addressibility-modifying
		      (fd (file-header-info-location h) fhi)
		      (setf (fh-info-dir-entry-index-in-parent fhi) ex)
		      (write-out-buffer-from-addressor fhi))

		    (set-bufmod-from-addressor dire)	;upper level writes out
		    (copy-directory-entry hdire dire)
		    (downreference-addressor dire)))))))))


(defun scav-repatriator (svi orphans)
  (create-dir-from-path (svi-partition svi) ">" "repatriations")	;ignore error
  (let* ((rdir (get-fd-from-dn-en ">" "repatriations" ':directory 1))
	 (rdir-scfd (scavenge-get-scfd (svi-scavenge-ht svi) (fd-uid rdir)))
	 (root-scfd (scavenge-get-scfd (svi-scavenge-ht svi) (fd-uid (fd-parent rdir)))))
    ;; Root shoulda been found.
    (setf (scfd-found-in-part rdir-scfd) (fd-r0addr rdir))
    (setf (scfd-ex rdir-scfd) (fd-entry-index rdir))
    (setf (scfd-par rdir-scfd) root-scfd)

    (push rdir-scfd (scfd-sons root-scfd))

    (dolist (scfd orphans)
      (do ((num 1 (1+ num))) (())
	(multiple-value-bind (fd err)
	    (create-directory (svi-partition svi)
			      rdir
			      (format nil "lost-~D" num)
			      ':uid (scfd-uid scfd))
	  (if (null err)
	      (progn
		(salv-msg 0 "Creating directory ~A in >repatriations." (fd-file-name fd))
		(push scfd (scfd-sons rdir-scfd))
		(setf (scfd-par scfd) rdir-scfd)
		(setf (scfd-ex scfd) (fd-entry-index fd))
		(setf (scfd-found-in-part scfd) (fd-r0addr fd))
		(return))))))))
