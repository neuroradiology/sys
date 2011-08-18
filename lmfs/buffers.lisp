;;; <LMFS>BUFFERS.LSP;1  2-Apr-81 13:42:13, Edit by BSG -*-Package:LMFS; Mode:LISP-*-

;;; Copyright (c) Symbolics, Inc., 1981

;;; That which manages file buffers.  The basic idea is the simulation
;;; of a virtual memory via Lisp macros.

(defconst fs-array-needed-here-kludge (make-array 1))
(defconst *checking-check-words* t)
(defconst *trace-buf-al-deal* nil)
(defconst *dbg-check-buffer-affiliation* nil)
(defconst *dbg-check-arg-types* nil)
(defconst *READ-AHEAD-COUNT* 4)
(defconst *READ-AHEAD* T)

(defstruct (8bit-addrarray
	    (:conc-name 8bldr-)
	    (:type :named-array-leader)
	    (:make-array
	      (:length 1024. :type 'art-8b :displaced-to fs-array-needed-here-kludge :displaced-index-offset 0)))
    (fill-pointer-HANDS-OFF nil)
    (ref-count		0)
    (buffer		nil)
    (word-displacement  0)			;kludge until double-indirects of differing
						;sizes work.
    (rel-address        0))			;what address was stored in here


(defmacro fs-ckarg-type x
  `(if *dbg-check-arg-types* (check-arg-type . ,x)))

(defmacro fs-ckarg-addressor (x)
  `(fs-ckarg-type ,x 8bit-addrarray))

(defconst *FILE-BUFFER-WORD-SIZE* 1024.)

(defvar fs-zrc-bufs nil)				;0 ref count, not-modified bufs
(defvar fs-errbreak-p)


(defresource fs-buffer ()
  :constructor
  (make-file-buffer
    array (make-file-buffer-i *FILE-BUFFER-WORD-SIZE* 'art-32b
			      ':named-structure-symbol 'physical-file-buffer
			      ':leader-length 2)))



(defresource (fs-8bit-addressing-array)
  (make-8bit-addrarray))

(defun obtain-8bit-addrarray (buf)
  (let ((addressor (allocate-resource 'fs-8bit-addressing-array)))
    (setf (8bldr-ref-count addressor) 1)
    (setf (8bldr-buffer addressor) buf)
    addressor))

(defun verify-address (headerp file wordaddr)
  (let ((buf nil)
	(err nil))
    (unwind-protect
      (progn
	(multiple-value (buf err) (get-buffer-given-fd-and-wordaddr headerp file wordaddr))
	err)
      (if buf (downreference-file-buffer buf)))))



;; This is the "page table" of the file system.

(defun get-buffer-given-fd-and-wordaddr (headerp file wordaddr &aux auxbufs)
  (fs-ckarg-type file file-desc)
  (fs-ckarg-type wordaddr fixnum)
  (let* ((first-try (search-buffer-list headerp file wordaddr))
	 (headerlen  (fd-logical-header-length file))
	 (total-addr   (if headerp wordaddr (+ wordaddr headerlen))))
    (if first-try (values first-try nil)
	(let ((raddr (get-record-address-for-io file total-addr)))
	  (let ((second-try (search-buffer-list headerp file wordaddr)))

	    ;; For short files, reading the header in the above
	    ;; may have put the buffer on the list already.
	    
	    (if second-try (values second-try nil)
		
		(let ((buf (find-usable-buffer file))	;he ups refcount, t = dont want 0
		      ;;avoid using record 0's buf, cause gonna search more recordaddrs soon
		      (dwpr (partt-dataw-per-record (fd-partition-desc file))))
		  (setf (fb-good-p buf) nil)
		  (setf (fb-creating buf) t)
		  
		  ;; See if we are really looking for a piece catalogued as
		  ;; header, not file, i.e., the boundary fraction.
		  
		  (let* ((lrecl (fb-record-length buf))
			 (headrem (\ headerlen lrecl)))
		    (if (and (not headerp)	;look for data,
			     (not (zerop headrem))	;header has a fraction
			     (< wordaddr (- lrecl headrem)))	;< 1st record's worth
			(setq headerp t wordaddr (round-down-to-boundary dwpr headerlen)
			      total-addr wordaddr)))
		  
		  (if headerp
		      (set-up-buf-as-header-containing
			buf file (round-down-to-boundary dwpr wordaddr))
		      (set-up-buf-as-data-containing
			buf file (- (round-down-to-boundary dwpr total-addr) headerlen)))
		  (if (and *READ-AHEAD* (not headerp))	;simplify calculations
		      ;;dont let r0 reads get in way-- theoretically,
		      ;;header buffer strategy will get this most often, but it is
		      ;;more critical here to functioning. Files with file maps in
		      ;;other than the first record will still perform awfully (> 820kwords)
		      (with-fileheader-addressibility (file 0 nignore)
			(loop for i from 1 to *READ-AHEAD-COUNT*
			      as next-total-addr = (+ total-addr (* i dwpr))
			      with rabufs = nil
			      finally (setq auxbufs (nreverse rabufs))
			      do
			      ;;(print (list total-addr next-total-addr i ))
			      (let ((found
				      (search-buffer-list
					nil file
					(- (round-down-to-boundary dwpr next-total-addr)
					   headerlen))))
				
				(if found
				    (progn
				      (downreference-file-buffer found)
				      (return (setq auxbufs (nreverse rabufs)))))
				(if (and (< next-total-addr (fd-addressible-length file))
					 (= (+  raddr i)
					    (get-record-address-for-io file next-total-addr)))
				    (let ((auxbuf (find-usable-buffer file)))
				      (setf (fb-creating auxbuf) t)
				      (setf (fb-good-p auxbuf) nil)
				      (set-up-buf-as-data-containing
					auxbuf file
					(- (round-down-to-boundary dwpr next-total-addr)
					   headerlen))
				      (push auxbuf rabufs))
				    (return (setq auxbufs (nreverse rabufs))))))))
		  (let ((errp (lexpr-funcall
				#'read-file-record-into-buffer file buf raddr auxbufs)))
		    (or errp
			(and (place-buffer-block-check-words buf t)	;t is good
			     (or (null auxbufs)
				 (loop for ab in auxbufs finally (return t) do
				       (or (place-buffer-block-check-words ab t)
					   (return nil)))))
			(setq errp (format nil
					   "FHN ERROR CNF F Buffer check words do not match, word ~O ~:[data~;header~] file damaged"
					   wordaddr headerp)))
		    (if errp			;read lost
			(progn
			  (dolist (ab auxbufs)
			    (downreference-file-buffer ab)
			    (disconnect-buffer-from-file ab))
			  (downreference-file-buffer buf)
			  (disconnect-buffer-from-file buf)
			  (values nil errp))
			(progn
			  (mapc 'downreference-file-buffer auxbufs)	;zrc is just great.
			  (with-fileheader-addressibility (file 0 h)
			    ;; see grow-file-header
			    (if (= (file-header-version h) *DEFAULT-HEADER-VERSION*)
				(setf (fd-logical-header-length file)
				      (file-header-logical-size h))))
			  (values buf nil)))))))))))


(defun search-buffer-list (headerp file wordaddr)
  (loop for buf in (fd-buffer-list file)
	if (and (not (fb-creating buf))
		(if headerp
		    (and (<= (fb-lowest-header-addr buf) wordaddr)
			 (< wordaddr (fb-highest-header-addr+1 buf)))
		    (and (<= (fb-lowest-data-addr buf) wordaddr)
			 (< wordaddr (fb-highest-data-addr+1 buf)))))
	do
	(if (and (= (fb-reference-count buf) 0)
		 (not (fb-modified buf)))
	    (progn
	      (if *trace-buf-al-deal*
		  (format t "~%search-buffer-list: ~S taken off zrc for ~S" buf file))
	      (setq fs-zrc-bufs (delq buf fs-zrc-bufs))))
	(return (upreference-file-buffer buf))
	finally (return nil)))


(defun get-buffer-addressor (headerp buffer wordaddr)		;=> addressor
  (fs-ckarg-type buffer file-buffer)  (fs-ckarg-type wordaddr fixnum)

  (let ((addressor (obtain-8bit-addrarray buffer)))
    (set-word-address-in-addressor
     addressor
     (fb-array buffer)
     (let* ((partt (fd-partition-desc (fb-file-desc buffer)))
	    (wpb (partt-dataw-per-block partt))
	    (relwordaddr (- wordaddr
			    (if headerp
				(fb-lowest-header-addr buffer)
				(fb-lowest-data-addr buffer))))
	    (relblk (// relwordaddr wpb)))
       (+ (\  relwordaddr wpb)
	  (*  (partt-block-size-words partt) relblk)
	  (block-check-words-size-in-words))))
    (setf (8bldr-rel-address addressor) wordaddr)	;for debugging/validation
    addressor))

;;; This is the general interface called by all the compiled macros.
;;; headerp is  a constant generated by the macros.

(defun get-general-file-addressor (headerp file wordaddr)
  (fs-ckarg-type file file-desc)
  (fs-ckarg-type wordaddr fixnum)

  (if (or (and headerp (> wordaddr (fd-logical-header-length file)))
	  (and (not headerp) (> wordaddr (fd-addressible-length file))))
      (break get-general-file-addressor-bad-arg))
  (multiple-value-bind (buf err)
      (get-buffer-given-fd-and-wordaddr headerp file wordaddr)
    (if err
	(ferror ':lmfs-data-error "Data error ~A accessing ~S ~:[data~;header~] addr ~O"
		err file headerp wordaddr))
    (values (get-buffer-addressor headerp buf wordaddr) buf)))

(defun buffer-from-addressor (adr)
  (fs-ckarg-addressor adr)
  (8bldr-buffer adr))

(defun set-bufmod-from-addressor (adr)
  (fs-ckarg-addressor adr)
  (set-buffer-modified (8bldr-buffer adr)))


(defun set-buffer-modified (buf)
  (fs-ckarg-type buf file-buffer)
  (setf (fb-modified buf) t)
  (setf (fb-good-p buf) nil))

(defun upreference-buffer-from-addressor (adr)
  (fs-ckarg-addressor adr)
  (upreference-file-buffer (8bldr-buffer adr))
  adr)

(defun upreference-addressor (adr)
  (fs-ckarg-addressor adr)
  (upreference-buffer-from-addressor adr)
  (incf (8bldr-ref-count adr))
  adr)

(defun upreference-file-buffer (buf)
  (fs-ckarg-type buf file-buffer)
  (incf (fb-reference-count buf))
  buf)

(defun downreference-addressor (adr)
  (fs-ckarg-addressor adr)
  (let ((rc (8bldr-ref-count adr)))
    (if (< rc 1)
	(fs-error "file addressor refcount zero in downreference-addressor" adr)
	(progn
	  (downreference-file-buffer (8bldr-buffer adr))
	  (setf (8bldr-ref-count adr) (1- rc))
	  (if (= 1 rc)					;was the last one
	      (progn
		(setf (8bldr-buffer adr) nil)
		(deallocate-resource 'fs-8bit-addressing-array adr)))))))

(defun downreference-file-buffer (buf)
  (fs-ckarg-type buf file-buffer)
  (let ((rc (fb-reference-count buf)))
    (if (< rc 1)
	(fs-error "zero buffer refcount in downreference-file-buffer" buf)
	(progn
	  (setf (fb-reference-count buf) (1- rc))
	  (if (and (= 1 rc)					;was the last one
		   (not (fb-modified buf)))
	      (free-file-buffer buf))))))

(defun free-file-buffer (buf)
  (fs-ckarg-type buf file-buffer)
  (if (fb-modified buf)
      (fs-error "free-file-buffer: modified buffer" buf))
  (if (not (= (fb-reference-count buf) 0))
      (fs-error "free-file-buffer: nz ref count" buf))
  (if *dbg-check-buffer-affiliation*
      (progn
	(if (memq buf fs-zrc-bufs)
	    (fs-error "free-file-buffer: Already in zrc bufs" buf))
	(let* ((prop (get 'fs-buffer 'defresource))
	       (item (assq buf (si:resource-object-list prop))))
	  (if (and item (null (second item)))
	      (fs-error "free-file-buffer: Already in free resource list" buf)))))
  (if *trace-buf-al-deal*
      (format t "~%free-file-buffer: ~S to ~:[resource~;zrc-bufs~], file =~S."
	      buf (fb-file-desc buf) (fb-file-desc buf)))
  (if (fb-file-desc buf)				;if not label, or whatever
      (push buf fs-zrc-bufs)
      (deallocate-resource 'fs-buffer buf)))

(defun find-usable-buffer (file)		;this is all nuts s/b redone
  (fs-ckarg-type file file-desc)
  (let* ((old-zrc fs-zrc-bufs)
	 (buf (or (pop fs-zrc-bufs) (allocate-resource 'fs-buffer)))
	 (old-file (fb-file-desc buf)))
    (if *trace-buf-al-deal*
	(format t "~%find-usable-buffer: Got ~S from ~:[resource~;zrc-bufs~] for ~S."
		buf old-zrc file))
    (if (not (eq file old-file))
	(progn
	  (and old-file
	       (setf (fd-buffer-list old-file) (delq buf (fd-buffer-list old-file))))
	  (push buf (fd-buffer-list file))))
    (if (not (zerop (fb-reference-count buf)))
	(fs-error "non-zero buffer refcount in find-usable-buffer" buf))

    ;;Impossibilize matches on this buffer

    (setf (fb-address buf) 0)				;canonical non-addr
    (setf (fb-highest-data-addr+1 buf) -1)			;until consist.
    (setf (fb-lowest-data-addr buf) -1)
    (setf (fb-highest-header-addr+1 buf) -1)
    (setf (fb-lowest-header-addr buf) -1)

    (upreference-file-buffer buf)			;more coherence/tracing
    (setf (fb-file-desc buf) file)
    (setf (fb-record-length buf)
	  (partt-dataw-per-record (fd-partition-desc file)))
    buf))

;;; array displacer needed to be implemented on lispm here

(defun set-word-address-in-addressor (addressor basearray addr)
  (fs-ckarg-addressor addressor)
  (setf (8bldr-word-displacement addressor) addr)
  (lmfs-change-indirect-array addressor basearray (* 4 addr)))

(defun virginize-buffer (buf)
  (zero-file-buffer buf)
  (setf (fb-creating buf) nil)
  (setf (fb-modified buf) nil)
  (setf (fb-good-p buf) t))

(defun establish-buf-as-header-containing (buf file whataddr)
  (fs-ckarg-type buf file-buffer)
  (fs-ckarg-type file file-desc)

  (setf (fb-good-p buf) nil)
  (set-up-buf-as-header-containing buf file whataddr)
  (virginize-buffer buf)
  (place-buffer-block-check-words buf nil))

(defun establish-buf-as-data-containing (buf file whataddr)
  (fs-ckarg-type buf file-buffer)
  (fs-ckarg-type file file-desc)

  (setf (fb-good-p buf) nil)
  (set-up-buf-as-data-containing buf file whataddr)
  (virginize-buffer buf)
  (place-buffer-block-check-words buf nil))

(defun set-up-buf-as-header-containing (buf file whataddr &optional headerlen)
  ;; opt arg is for header-grower to impose it opinion...
  (fs-ckarg-type buf file-buffer)
  (fs-ckarg-type file file-desc)

  ;; Addresses are guaranteed impossibilized..
  (setf (fb-file-desc buf) file)
  (setf (fb-record-length buf)
	(partt-dataw-per-record (fd-partition-desc file)))

  (let* ((lrecl (fb-record-length buf))
	 (headerl  (or headerlen (fd-logical-header-length file)))
	 (headrem (\ headerl lrecl))
	 (hend (+ whataddr lrecl)))
    (if (not (zerop (\ whataddr lrecl)))
	(fs-error "set-up-buf-as-header-containing: call on bad boundary" buf file whataddr))
    (setf (fb-lowest-header-addr buf) whataddr)
    (if (< headerl hend)
	(progn
	  (setf (fb-highest-header-addr+1 buf) headerl)
	  (setf (fb-lowest-data-addr buf) (- headrem))
	  (setf (fb-highest-data-addr+1 buf) (- lrecl headrem)))
	(setf (fb-highest-header-addr+1 buf) hend))))

(defun set-up-buf-as-data-containing (buf file whataddr)
  ;; May only be called for beginning of record, hence, should never be called for fraction
  (fs-ckarg-type buf file-buffer)
  (fs-ckarg-type file file-desc)

  (setf (fb-file-desc buf) file)
  (setf (fb-record-length buf)
	(partt-dataw-per-record (fd-partition-desc file)))

  (let* ((lrecl (fb-record-length buf))
	 (headerl (fd-logical-header-length file)))
    (if (not (zerop (\ (+ headerl whataddr) lrecl)))
	(fs-error "set-up-buf-as-data-containing: call on bad boundary" buf file whataddr))
    (setf (fb-lowest-header-addr buf) -1)
    (setf (fb-highest-header-addr+1 buf) -1)
    (setf (fb-lowest-data-addr buf) whataddr)
    (setf (fb-highest-data-addr+1 buf) (+ whataddr lrecl))))

(defresource (16b-cleararray t)
	     (make-array 2048. ':type ':art-16b ':displaced-to fs-array-needed-here-kludge))
	     
(defun zero-file-buffer (buf)
  (fs-ckarg-type buf file-buffer)
  (let ((real-buf (fb-array buf)))
    (with-resource (16b-cleararray clr)
      (si:change-indirect-array clr 'art-16b (list (* 2 (array-length real-buf)))
				real-buf nil)
      (copy-array-contents "" clr))))

(defun place-buffer-block-check-words (buf check)
  (fs-ckarg-type buf file-buffer)
  (let ((fd (fb-file-desc buf))
	(r0p (= (fb-lowest-header-addr buf) 0)))
    (if (null fd)
	t						;return as if check case anyway
	(protect-buffer-addressor (adr (obtain-8bit-addrarray
					 (upreference-file-buffer buf)))
	  (let* ((part (fd-partition-desc fd))
		 (block-size-words (partt-block-size-words part))
		 (record-size-blocks (partt-record-size-blocks part))
		 (dataw-per-block (partt-dataw-per-block part))
		 (uid (fd-uid fd))
		 (array (fb-array buf)))
	    (loop for bno from 0 below record-size-blocks
		  and buf-offset from 0 by block-size-words
		  and data-addr from (if (> (fb-highest-data-addr+1 buf) (fb-lowest-data-addr buf))
					 (fb-lowest-data-addr buf)
					 -9999999.) by dataw-per-block
		  and hdr-addr from (if (> (fb-highest-header-addr+1 buf) (fb-lowest-header-addr buf))
					(fb-lowest-header-addr buf)
					-9999999.) by dataw-per-block
		  finally (return t)
		  do
		  (let* ((headerp (minusp data-addr))
			 (rel (if headerp hdr-addr data-addr)))
		    (set-word-address-in-addressor adr array buf-offset)
		    (if check
			(if (and *checking-check-words*
				 (check-the-actual-words adr uid headerp rel r0p))
			    (return nil))
			(progn
			 (setf (block-check-words-headerp adr) headerp)
			 (setf (block-check-words-word-0s-rel-addr adr) rel)
			 (setf (block-check-words-file-id adr) uid)))

		    (set-word-address-in-addressor adr array (- (+ block-size-words buf-offset)
								(block-check-words-size-in-words)))
		    (if check
			(if (and *checking-check-words*
				 (check-the-actual-words adr uid headerp rel r0p))
			    (return nil))
			(progn
			 (setf (block-check-words-headerp adr) headerp)
			 (setf (block-check-words-word-0s-rel-addr adr) rel)
			 (setf (block-check-words-file-id adr) uid))))))))))

(defun check-the-actual-words (adr uid headerp rel r0p)	;val of t means lose
  (if (null uid)				;root activate
      nil					;it's ok
      (not (and (= (block-check-words-file-id adr) uid)
		(or r0p
		    (and (eq (block-check-words-headerp adr) headerp)
			 (= (block-check-words-word-0s-rel-addr adr) rel)))))))

(defun write-out-buffer-from-addressor (adr)
  (fs-ckarg-addressor adr)
  (write-out-file-buffer (buffer-from-addressor adr)))

(defun write-out-buffer-from-addressor-if-needed (adr)
  (let ((buf (buffer-from-addressor adr)))		;will argcheck
    (if (fb-modified buf)
	(write-out-file-buffer buf))))


;;; We do not support randomly allocated files. Either the thing
;;; is in the file or its the next off the end.

(defun get-file-data-block-regardless (file addr) ; => addressor, err
  (fs-ckarg-type file file-desc)
  (fs-ckarg-type addr fixnum)

  (let ((part (fd-partition-desc file))
	(curlen (fd-addressible-length file)))
    (if (>= addr curlen)
	(if (> addr (+ (partt-dataw-per-record part) curlen))
	    (break bad-grow-in-get-file-data-block-regardless)
	    (progn
	      (let ((err (grow-file-to-addr file addr)))
		(if err (values nil err)
		    (let ((adr (get-general-file-addressor nil file addr)))
		      (downreference-file-buffer	;undo grow's protect
		       (buffer-from-addressor adr))
		      (values adr nil))))))
	(values (get-general-file-addressor nil file addr) nil))))


;;;----------------------------------------------------------------------

;;; Random utility

(defun flush-buffers-for-file (file &optional (dont-do-header nil))

  (fs-ckarg-type file file-desc)

  (without-interrupts
    (setf (fd-buffer-list file)
	  (sort (fd-buffer-list file)
		#'(lambda (x y)
		    (let ((xa (fb-address x))
			  (ya (fb-address y)))
		      (and (fixp xa) (fixp ya) (< xa ya)))))))

  ;; If doing headers, do headers SECOND.
  (if (not dont-do-header) (flush-buffers-for-file file t))

  (do ((curp (fd-buffer-list file) (cdr curp))
       (start-address nil)
       (start-bufp nil)
       (i 0))
      ((null curp) (if start-bufp (flushbufs-contig start-bufp i)))
    
    (let* ((buf (car curp))
	   (addr (fb-address buf)))
      (if (and (fb-modified buf)		;candidate?
	       (fixp addr)			;no barfy buffers
	       (or (not dont-do-header)
		   (minusp (fb-lowest-header-addr buf))))
	  (if start-bufp
	      (if (= addr (+ i start-address))
		  (incf i)
		  (progn
		    (if start-bufp (flushbufs-contig start-bufp i))
		    (setq start-bufp curp i 1 start-address addr)))
	      ;;first time or after nonqualified buffer
	      (setq start-bufp curp i 1 start-address addr))
	  (progn				;nonqualified buffer
	    (if start-bufp (flushbufs-contig start-bufp i))
	    (setq start-bufp nil))))))

(defun flushbufs-contig (bufsp i)
  (if (= i 1)
      (write-out-file-buffer (car bufsp))
      (let ((l (make-list i)))
	(loop for buf in bufsp and j from 0 below i do
	      (setf (nth j l) buf)
	      finally (apply #'write-out-file-buffer l)))))

(defun disconnect-buffer-from-file (buf)
  (fs-ckarg-type buf file-buffer)
  (if (not (= (fb-reference-count buf) 0))
      (fs-error "disconnect-buffer-from-file: nonzero ref count:" buf))

  (let ((file (fb-file-desc buf)))
    (setf (fd-buffer-list file) (delq buf (fd-buffer-list file)))
    (setf (fb-file-desc buf) nil)
    (setf (fb-address buf) 0)
    (if (and *dbg-check-buffer-affiliation* file (not (memq buf fs-zrc-bufs)))
	(fs-error "disconnect-buffer-from-file: 0rc, but not in zrc already" buf file))
    (if (null file)
	(free-file-buffer buf))))


(defun blast-clear-buffer (buf)			;used by environment-save
  (setf (fb-reference-count buf) 0)
  (setf (fb-file-desc buf) nil)
  (setf (fb-address buf) 0))

(defun allocate-random-buffer ()
  ;; If there is a zrc buf, move it into the really-free buffer state first
  ;; and then allocate-resource it.

  (if fs-zrc-bufs
      (let ((buf (car fs-zrc-bufs)))
	(if *trace-buf-al-deal* (format t "~%allocate-random-buffer: ~S car of zrc" buf))
	(if (fb-file-desc buf)
	    (disconnect-buffer-from-file buf))	;will leave on zrc bufs
	(free-file-buffer (pop fs-zrc-bufs))))
  (let ((buf (allocate-resource 'fs-buffer)))
    (if *trace-buf-al-deal*
	(format t "~%Allocate-random-buffer: ~S from resource" buf))
    buf))

;;; Stuff needed by header grower.

(defun file-buffer-data-copy (from to)
    (with-resource (16b-cleararray clr1)
	(si:change-indirect-array clr1 'art-16b (list (* 2 (array-length (fb-array from))))
				  (fb-array from) nil)
	(with-resource (16b-cleararray clr2)
	  (si:change-indirect-array clr2 'art-16b (list (* 2 (array-length (fb-array to))))
				    (fb-array to) nil)
	  (copy-array-contents clr1 clr2))))

(defun file-buffer-zero-header-therein (buf)
  (let ((low (fb-lowest-header-addr buf))
	(high (fb-highest-header-addr+1 buf))
	(dwpb (partt-dataw-per-block (fd-partition-desc (fb-file-desc buf)))))
    (do haddr low (+ haddr dwpb) ( haddr high)
	(protect-buffer-addressor (adr (get-buffer-addressor t
							     (upreference-file-buffer buf)
							     haddr))
	  (copy-array-portion "" 0 0 adr 0 (* 4 dwpb))))))



(defun file-buffer-clear-from-address (buf headerl)
  ;; Let's compute block offset remainder.

  (let* ((fd (fb-file-desc buf))
	 (partt (fd-partition-desc fd))
	 (dwpb (partt-dataw-per-block partt))
	 (awpb (partt-block-size-words partt))
	 (bpr (partt-record-size-blocks partt))
	 (headerl-blocks (// headerl dwpb))
	 (blkrem (\ headerl-blocks bpr)))	;the remainder of the header in blocks.
    (loop for i from blkrem below bpr
	  do
	  (protect-buffer-addressor (addressor (obtain-8bit-addrarray
						 (upreference-file-buffer buf)))
	    (set-word-address-in-addressor
	      addressor
	      (fb-array buf)
	      (+ (* i awpb)
		 (block-check-words-size-in-words)))
	    (copy-array-portion "" 0 0 addressor 0 (* 4 dwpb))))))


;;; Change what an indirect array points at, or what its offset is.
(defun lmfs-change-indirect-array (array displaced-to index-offset)
  (%p-store-contents-offset displaced-to array 1)
  (%p-dpb-offset 1 %%q-flag-bit array 1) ;flag signals index offset
  (%p-store-contents-offset index-offset array 3)
  array)
