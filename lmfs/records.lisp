;;; <LMFS>RECORDS.LSP;1 15-Apr-81 08:58:39, Edit by BSG -*-Package:LMFS; Mode:LISP-*-

;;;
;;; Copyright (c) Symbolics, Inc., 1981
;;;

;;;
;;;  New LM file system record management
;;;

#M (eval-when (compile eval)
     (load 'defstorage)
     (load 'fs-dcls)
     (load 'bufmacs))

#M (declare (muzzled t))

(defvar *bloodlet-trace* nil)
(defvar *FREE-MAP-UID* 3)
(defvar *MAGIC-MAPINIT-PREWITHDRAW* 10.)
(defvar *DEFAULT-FREEMAP-VERSION* 1)
(defvar *DEFAULT-FREEMAP-NPBW* 24.)			;a-model lispm
(defvar *DEFAULT-FREE-POOL-SIZE* 100.)
(defvar *DEFAULT-FREEMAP-WEGTM-FRACTION* 0.33)
(defvar *DEFAULT-FREEMAP-WTMD-FRACTION* 0.75)
(defvar *DEFAULT-FREEMAP-DUMP-FRACTION* 0.66)

(defun allocate-disk-record (part)			;=> recadd or nil.
  (check-arg-type part partition)

  (let ((map (partt-free-record-info part)))
    (if (zerop (freemap-cur-howmany map))
	(progn
	  (replenish-free-record-pool map)
	  (if (zerop (freemap-cur-howmany map))
	      nil					;final answer
	      (allocate-disk-record part)))

	;; claim is there are some records today

	(prog1 (aref (freemap-array map) (freemap-cur-take map))
	       (aset 0 (freemap-array map) (freemap-cur-take map)) ;king of cleverness
	       (without-interrupts
		 (decf (freemap-cur-howmany map))
		 (if (= (freemap-cur-take map) (1- (freemap-pool-size map)))
		     (setf (freemap-cur-take map) 0)
		     (incf (freemap-cur-take map))))))))
  

(defun deposit-disk-record (rec part)
  (check-arg-type part partition)
  (check-arg-type rec fixnum)

  (let ((map (partt-free-record-info part)))
    (if (= (freemap-cur-howmany map) (freemap-pool-size map))
	(break freemap-buffer-overflow))		;not supposed to happen
    (if (zerop rec) (break zero-disk-address-given-to-free))
    (aset rec (freemap-array map) (freemap-cur-put map))
    (without-interrupts
      (if (= (freemap-cur-put map) (1- (freemap-pool-size map)))
	  (setf (freemap-cur-put map) 0)
	  (incf (freemap-cur-put map)))
      (incf (freemap-cur-howmany map)))
    (if (not (< (freemap-cur-howmany map) (freemap-when-this-many-dump map)))
	(bloodlet-freemap map (freemap-dump-that-many map)))))

(defun wring-freemap (part)
  (let ((map (partt-free-record-info part)))
    (bloodlet-freemap map (freemap-cur-howmany map))))

;;;----------------------------------------------------------------------



(defun replenish-free-record-pool (map)
  (check-arg-type map freemap)

  (let* ((part (freemap-partition map))
	 (mapfile (freemap-file map))
	 (nblocks (freemap-file-nblocks map))
	 (bsize (partt-dataw-per-block part))
	 (took 0)
	 (lossagep nil)					;find problem
	 (nbits-per-word (freemap-file-n-bits-per-word map))
	 (goal (freemap-when-empty-get-this-many map)))

    ;; For cleanliness-interruption

    (setf (freemap-cur-howmany map) 0)
    (setf (freemap-cur-put map) 0)
    (setf (freemap-cur-take map) 0)

    (loop for blocknum from 1 to nblocks
	  until (zerop goal)
	  do
	  (with-filedata-addressibility (mapfile (* bsize blocknum) mb)
	    (if (not (= blocknum (freemap-block-blocknum mb)))
		(setq lossagep t)
		(if (plusp (freemap-block-number-of-free-recs mb))
		    (if (not (= (fs-freemap-block-checksum mb nbits-per-word)
				(freemap-block-checksum mb)))
			(setq lossagep t)
			(let ((cs (freemap-block-checksum mb))
			      (tookp nil))
			  (loop for wordno from 0 to (1- (freemap-block-number-of-words mb))
				until (zerop goal)
				do
				(let* ((orword (freemap-block-array mb wordno))
				       (word orword))
				  (if (not (zerop word))
				      (let ((mungs 0)
					    (basadr (+ (* wordno nbits-per-word)
						       (freemap-block-recno-of-bit-0 mb))))
					(loop for i from 0 to (1- nbits-per-word)
					      for rec = basadr then (1+ rec)
					      until (or (zerop word)
							(zerop goal))
					      do
					      (if (oddp word)
						  (progn
						    (aset rec (freemap-array map)
							  (freemap-cur-put map))
						    (incf (freemap-cur-put map))
						    (setq mungs (logior mungs (ash 1 i)))
						    (incf took)
						    (decf goal)
						    (decf (freemap-block-number-of-free-recs mb))

						    (setq tookp t)))
					      (setq word (ash word -1)))
					(if (not (zerop mungs))	;mung cksum and data
					    (progn
					     (setf (freemap-block-array mb wordno)
						   (logand (logxor mungs -1)
							   orword))
					     (setq cs (logxor cs (fs-rot
								  mungs
								  nbits-per-word
								  wordno)))))))))
			  (if tookp
			      (progn
				(setf (freemap-block-checksum mb) cs)
				(set-bufmod-from-addressor mb)	;DONT OPTIMIZE!
				(write-out-buffer-from-addressor mb)))))))))

    (if lossagep (setf (freemap-trouble-encountered map) t))
    (setf (freemap-cur-howmany map) took)))


(defun bloodlet-freemap (map goal)
  (check-arg-type map freemap)
  (check-arg-type goal fixnum)

  (let* ((part (freemap-partition map))
	 (mapfile (freemap-file map))
	 (bsize (partt-dataw-per-block part))
	 (bits-per-word (freemap-file-n-bits-per-word map))
	 (lossagep nil)
	 (bufs nil))

    (loop until (zerop goal)
	  do
	  (let ((r (or (allocate-disk-record part)
		       (break bloodlet-file-map-withdrew-null))))
	    (cond
	     ((or (< r (freemap-file-bit-zero-recno map))
		  (not (< r (partt-disk-addr-limit part))))
	      (setq lossagep t)
	      (decf goal))
	     (t
	      (let* ((recno (- r (freemap-file-bit-zero-recno map)))
		     (rpb (freemap-records-represented-in-block map))
		     (blkno (1+ (// recno rpb)))
		     (blkrem (\ recno rpb))
		     (wordno (// blkrem bits-per-word))
		     (bitno (\ blkrem bits-per-word))
		     (test-bit (ash 1 bitno)))
	       
		
		(with-filedata-addressibility (mapfile (* bsize blkno) mb)
		  (decf goal)				;do this in ANY case
		  (if (not (= blkno (freemap-block-blocknum mb)))
		      (setq lossagep t)


		      (let ((word (freemap-block-array mb wordno)))

			;; Make sure not already free
			(if *bloodlet-trace*
			   (format t "~%BLT rec ~O wno ~O bno ~O tb ~O word ~O"
				   recno wordno bitno test-bit word))
			(if (not (zerop (logand word test-bit)))
			    (progn
			     (setq lossagep t)
			     (if *bloodlet-trace* (format t "*****")))

			    ;; Ok really mark as free

			    (let ((buf (buffer-from-addressor mb)))
			      (set-buffer-modified buf)
			      (if (not (memq buf bufs))
				  (progn
				   (upreference-file-buffer buf)
				   (push buf bufs)))
			      (setf (freemap-block-array mb wordno)
				    (logior test-bit word))
			      (incf (freemap-block-number-of-free-recs mb))
			      (setf (freemap-block-checksum mb)
				    (logxor (freemap-block-checksum mb)
					    (fs-rot test-bit bits-per-word wordno)))))))))))))
    (dolist (buf bufs)
      (write-out-file-buffer buf)
      (downreference-file-buffer buf))

    (if lossagep (setf (freemap-trouble-encountered map) t))))


(defun create-freemap (part)				;for init and load
  (let ((map (make-freemap)))
    (setf (freemap-partition map) part)
    (setf (freemap-cur-howmany map) 0)
    (setf (freemap-cur-put map) 0)
    (setf (freemap-cur-take map) 0)
    (setf (freemap-trouble-encountered map) nil)
    (setf (freemap-array map) (make-array *DEFAULT-FREE-POOL-SIZE*))
    (setf (freemap-pool-size map) *DEFAULT-FREE-POOL-SIZE*)
    (setf (freemap-when-empty-get-this-many map)
	  (fix (times *DEFAULT-FREEMAP-WEGTM-FRACTION*
		      (freemap-pool-size map))))
    (setf (freemap-when-this-many-dump map)
	  (fix (times *DEFAULT-FREEMAP-WTMD-FRACTION*
		      (freemap-pool-size map))))
    (setf (freemap-dump-that-many map)
	  (fix (times *DEFAULT-fREEMAP-DUMP-FRACTION*
		      (freemap-pool-size map))))
    (setf (freemap-file map) nil)
    (setf (partt-free-record-info part) map)
    map))


(defun init-freemap (part frec nrec)
  (if (not (null (partt-free-record-info part)))
      (ferror nil "init-freemap: part already has freemap: ~S" part))
  (let ((map (create-freemap part)))

    ;; Now comes good part.  Put in enough records to cover the
    ;; header creation of the file. 10. OUGHT DO IT.

    (loop for r from frec to (+ *MAGIC-MAPINIT-PREWITHDRAW* frec -1)	;miracle of LOOP
	  do (deposit-disk-record r part))
  
    (setf (freemap-trouble-encountered map) t)		;in case interr.

    (multiple-value-bind (file err)
      (create-file nil "*FREE MAP*" ".SYSTEM." 1 ':uid *FREE-MAP-UID* ':partition part)
      (if err (ferror nil "init-freemap: err ~S from create-file" err))
      (setf (freemap-file-n-bits-per-word map) *DEFAULT-FREEMAP-NPBW*)
      (setf (freemap-file-bit-zero-recno map) frec)
      (setf (freemap-file map) file)
      (let* ((dwpb (partt-dataw-per-block part))
	     (uwpb (- dwpb (freemap-block-size-in-words)))	;usable wds
	     (bpw (freemap-file-n-bits-per-word map))
	     (bpb (* uwpb bpw))
	     (nblocks (// (+ nrec bpb -1) bpb))
	     (error nil))

	(setf (freemap-file-nblocks map) nblocks)
	(setf (freemap-records-represented-in-block map) bpb)


	;; Deposition ought now work. However, let us first
	;; set up the header. Guaranteed can withdraw it.

	(protect-buffer-addressor (fmh)
          (multiple-value (fmh error)
	    (get-file-data-block-regardless file 0))
	  (if error (ferror nil "init-freemap: err ~S from addressing file" error)))

	(with-filedata-addressibility-modifying (file 0 fmh)
	  (setf (freemap-header-version fmh) *DEFAULT-FREEMAP-VERSION*)
	  (setf (freemap-header-first-bit-record-no fmh) frec)
	  (setf (freemap-header-number-of-blocks fmh) nblocks)
	  (setf (freemap-header-records-represented-in-block fmh) bpb)
	  (setf (freemap-header-n-bits-per-word fmh) bpw)
	  (write-out-file-buffer (buffer-from-addressor fmh)))


	(loop for blkno from 1 to nblocks
	      do
	      (let* ((relblkno (1- blkno))
		     (thisbase (+ (* relblkno bpb) frec))
		     (thisend (min (+ frec nrec)
				   (+ thisbase bpb)))
		     (nwords (// (+ (- thisend thisbase) bpw -1) bpw))
		     (raddr (* blkno dwpb)))
		(protect-buffer-addressor (fmh)
		  (multiple-value (fmh error)
		    (get-file-data-block-regardless file raddr))
		  (if error (ferror nil
				    "trouble addressing freemap record ~S ~S"
				    error raddr)))
		(with-filedata-addressibility-modifying (file raddr mb)
		  (setf (freemap-block-blocknum mb) blkno)
		  (setf (freemap-block-recno-of-bit-0 mb) thisbase)
		  (setf (freemap-block-number-of-words mb) nwords)
		  (setf (freemap-block-number-of-free-recs mb) 0)
		  (setf (freemap-block-highest-recno+1 mb) thisend)
		  (dotimes (i nwords) (setf (freemap-block-array mb i) 0))
		  (setf (freemap-block-checksum mb)
			(fs-freemap-block-checksum mb bpw))

		  (write-out-file-buffer (buffer-from-addressor mb)))

		;; Pretty slow, but right thing.

		(loop for r from (max thisbase (+ frec *MAGIC-MAPINIT-PREWITHDRAW*))
		      to (1- thisend)			;LOOP wins again...

		      do (deposit-disk-record r part))))))
    (setf (freemap-trouble-encountered map) nil)))

(defunp load-freemap (part mapaddr)			;dont withdraw/repl.
  (multiple-value-bind (file err)
    (activate-file-from-header nil part mapaddr "Free Record map"
			       *FREE-MAP-UID*)

    (if err (return err))
    (let ((map (create-freemap part)))
      (setf (freemap-file map) file)
      (with-filedata-addressibility (file 0 maph)
	(or (= (freemap-header-version maph) 1)
	    (return (format nil "Bad freemap version: ~S"
			    (freemap-header-version maph))))
	(setf (freemap-file-nblocks map) (freemap-header-number-of-blocks maph))
	(setf (freemap-file-bit-zero-recno map) (freemap-header-first-bit-record-no maph))
	(setf (freemap-file-n-bits-per-word map) (freemap-header-n-bits-per-word maph))
	(setf (freemap-records-represented-in-block map)
	      (freemap-header-records-represented-in-block maph))))
    (return nil)))

;;;----------------------------------------------------------------------

(defun relocate-address-to-partition (part addr)
  (if (or
       (>= addr (partt-disk-addr-limit part))
       (< addr 0))
      (ferror nil "relocate-address-to-partition: bad addr ~S in ~S"
	      addr part)
      (+ (* addr (partt-record-size-blocks part))
	 (partt-address-base-in-blocks part))))

(defun check-address-against-partition (part addr)
  (not (or
	 (>= addr (partt-disk-addr-limit part))
	 (< addr 0))))

;;;----------------------------------------------------------------------


;;; Dept. of Utilities


(defun fs-freemap-block-checksum (mb bpw)
 (logxor
  bpw
  (fs-rot (freemap-block-blocknum mb) bpw 1)
  (fs-rot (freemap-block-recno-of-bit-0 mb) bpw 2)
  (fs-rot (freemap-block-highest-recno+1 mb) bpw 3)
  (fs-rot (freemap-block-number-of-words mb) bpw 4)
  (loop with cs = 0
	for i from 0 to (1- (freemap-block-number-of-words mb))
	finally (return cs)
	do (setq cs (logxor cs (fs-rot (freemap-block-array mb i)
				       bpw i))))))


(defun fs-rot (what len howmany)
  (let* ((rotx (\ howmany len))			;gratuitous
	 (zotx (- len rotx)))
    (if (zerop rotx)
	what
	(logior
	  (ash					;shift up
	    (ldb zotx what)			;low bits
	    rotx)
	  (ash what (- zotx))))))

(defun bits-on-per-word (x)
  ;; Gosper's HAKMEM algorithm
  ;; in hoc magicus
  ;; works for anything up to 63 bits per word, but numbers here good to 36.

  (let* ((sh1 (logand #O333333333333 (ash x -1)))
	 (threes (- x
		    sh1
		    (logand #O111111111111 (ash sh1 -1)))))
    (\ (+ (logand threes #O070707070707)
	  (logand (ash threes -3) #O070707070707))
       63.)))
 
