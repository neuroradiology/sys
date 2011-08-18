;;; <LMFS>FILES.LSP;9  8-Apr-81 11:12:39, Edit by BSG -*-Package:LMFS; Mode:LISP-*-
;;; <LMFS>FILES.LSP;1  2-Apr-81 16:46:22, Edit by BSG -*-LISP-*-

;;; That which deals with actual file dharma, as opposed to reading, writing,
;;; opening, closing, or addressing them.  Birth, death, etc. hacked here.


;;;  Copyright (c) Symbolics, Inc., 1981


(declare (special fs-errbreak-p *BITS-PER-WORD*))

(defconst *LINK-TARGET-VERSION* 1)
(defconst *DEFAULT-BYTESIZE* 8.)

(defconst *DEFAULT-HEADER-VERSION* 1)
(defconst *DEFAULT-HEADER-FM-SIZE* 4)
(defconst *DEFAULT-FILE-HEADER-INFO-VERSION* 1)

(defconst *DEFAULT-DIR-FHEADER-SIZE* 250.)
(defconst *DEFAULT-LINK-FHEADER-SIZE* 250.)
(defconst *DEFAULT-FILE-FHEADER-SIZE* 250.)

(defconst *DEFAULT-FILE-FM-SIZE* 64.)
(defvar *READ-METER-VECTOR* (let ((a (make-array 10. ':type ':art-q)))
			      (fillarray a '(0))
			      a))
(defvar *WRITE-METER-VECTOR* (let ((a (make-array 10. ':type ':art-q)))
			      (fillarray a '(0))
			      a))
(defvar *FS-ROOT-VOL* 0)

(defun (file-desc :named-structure-invoke) (op ob &optional stream ignore ignore)
    (if (eq op ':which-operations) '(:print-self)
	(format stream "#<FILE-DESC ~O ~A>" (%pointer ob) (fd-file-name ob))))


;;; Create the file.  Create the dir entry. Update it.


(defunp create-file (parent name type version &rest options	;=> file err
			    &aux readthru writethru createthru renamethru deletethru)
			    
  ;; At this level, version must be guaranteed nondup.
  (let ((dir-p (eq type ':directory))
	(binp nil)
	(part nil)
	(uid nil)
	(vol nil)
	(link-p nil)
	(author "")
	(lvx 0)
	(ex 0) (dire nil)
	(grc 0)
	(bytesize *BITS-PER-CHAR*)
	dataw rsize)
    (loop for (ind prop) on options by 'cddr
	  do (selectq ind
	       (:volume      (setq vol prop))		;pass actual objects
	       (:link	     (setq link-p prop))
	       (:partition   (setq part prop))
	       (:author      (setq author (or prop "")))
	       (:uid	     (setq uid prop))
	       (:byte-size   (setq bytesize prop))
	       (:characters  (setq binp (not prop)))
	       (:generation-retention-count (setq grc prop))))

    (if (null vol)
	(setq vol
	      (if (not (null part))
		  (partt-volume part)			;no vol, but part given
		  (if (null parent)
		      *FS-ROOT-VOL*
		      (get-vol-from-volx		;real dir, inherit
		       (fd-partition-desc parent)
		       (with-filedata-addressibility (parent 0 dirh)
			 (dir-header-default-volid dirh)))))))
    (if (null part)
	(setq part (choose-allocation-partition vol))
	(if (not (eq vol (partt-volume part)))
	    (ferror nil "Attempt to create file in part ~S not part of vol ~S"
		    part vol)))


    (if (null bytesize)
	(setq bytesize (cond (link-p *BITS-PER-WORD*)
			     (dir-p *BITS-PER-WORD*)
			     (t *DEFAULT-BYTESIZE*))))

    (if (null uid) (setq uid (generate-file-uid part)))

    (setq dataw (partt-dataw-per-block part)
	  rsize (* dataw (partt-record-size-blocks part)))


    (let ((maxn (* dataw *CHARS-PER-WORD*)))		;stop at once.
      (if (> (string-length name) maxn)
	  (return-values nil
			 (format nil
				 "The name supplied is too long (~D chars max)"
				 maxn))))

    ;; Start by getting the new directory entry. Now this might lose
    ;; in any number of interesting divers ways, not the least of which
    ;; is it returns ':create-through-link-magic ...

    (if parent
	(multiple-value-bind (xdire xex err crthru-fd newvers ggrc)
	  (get-new-directory-entry parent name type version)
	  (if err (return-values crthru-fd err))		;OH NO, IT LOST
	  (if (not (numberp version)) (setq version newvers))
	  (if (zerop grc) (setq grc ggrc))		;inherit generation ret ct
	  (setq ex xex dire xdire)))			;dire is upref'd..

    ;; Now try making the fd..

    (let* ((file (make-file-desc
		  partition-desc     part
		  sons		     nil
		  buffer-list	     nil
		  current-length     0
		  byte-length	     0
		  byte-size	     bytesize
		  openings	     nil
		  parent	     parent
		  file-name	     name
		  file-type	     type
		  file-version	     version
		  uid		     uid
		  entry-index	     ex
		  link-p	     link-p
		  addressible-length 0
		  date-time-created  (time:get-universal-time)
		  grc-info	     (if (zerop grc) nil grc)
		  file-map-addenda   nil
		  logical-header-length
		  	(round-up-to-boundary (partt-dataw-per-block part)
			   (cond (link-p *DEFAULT-LINK-FHEADER-SIZE*)
				 (dir-p *DEFAULT-DIR-FHEADER-SIZE*)
				 (t *DEFAULT-FILE-FHEADER-SIZE*)))))

	   (buf (find-usable-buffer file)))		;all right magic

      ;; Now make it seem as though this buffer really was into being
      ;; this piece of header for this file.

      (if (> (fd-logical-header-length file) (partt-dataw-per-record part))
	  (fs-error "create-file: invalid header length" (fd-logical-header-length file)))

      ;; Sets limit words

      (establish-buf-as-header-containing buf file 0)	;zeroes data, too

      (let ((h0rec (allocate-disk-record part)))
	(or (numberp h0rec) (return-values nil "Out of room in partition//volume"))
	(setf (fd-r0addr file) h0rec)			;fh-info needs this
	(setf (fb-address buf) h0rec))

      (setf (fd-logical-header-length file)		;in case changed
	    (lay-out-file-header file))
      ;;  although it should not really change

      (with-fileheader-addressibility (file 0 h)
	  
	;; Set up the header file map.
	  
	(with-fileheader-addressibility-modifying (file (file-header-header-fm-location h) fm)
	  (setf (file-map-allocated-length fm)
		(- (file-header-header-fm-length h) (file-map-size-in-words)))
	  (setf (file-map-valid-length fm) 1)
	  (setf (file-map-element fm 0) (fd-r0addr file)))
      
	;; Set up directory entry in file header.

	(with-fileheader-addressibility-modifying (file (file-header-dire-location h) hdire)
	  (fill-in-directory-entry file hdire grc author binp lvx)
	  (set-up-fh-info file h ex)
	  (if parent
	      (progn
		(push file (fd-sons parent))
		(set-bufmod-from-addressor dire)
		(copy-directory-entry hdire dire)
		(write-out-buffer-from-addressor dire)
		(downreference-addressor dire)))
	  
	  ;; Get link attributes while directory has half a chance of being in buffer
	  
	  (if link-p
	      
	      (with-filedata-addressibility (parent 0 dirh)
		(setq readthru (dir-header-default-link-transparencies-read-thru dirh))
		(setq writethru (dir-header-default-link-transparencies-write-thru dirh))
		(setq createthru (dir-header-default-link-transparencies-create-thru dirh))
		(setq deletethru (dir-header-default-link-transparencies-delete-thru dirh))
		(setq renamethru (dir-header-default-link-transparencies-rename-thru dirh)))))
	
	(initialize-real-file-map file h)	;sets addrbl. len in fd, too
	
	(fs-plist-init file h)

	;; putprop name if needed

	(if (> (string-length name) (directory-entry-file-name-max-length))
	    (fs-plist-put file 'name name)))

      ;; end of header addressibility on h
      
      ;; For links, set up link data.  All of which has been pre-checked
      ;; appropriately.


      (if link-p
	  (with-filedata-addressibility-modifying (file 0 linktarget)
	    (setf (link-target-version linktarget) *LINK-TARGET-VERSION*)
	    (setf (link-target-pathname linktarget) link-p)
	    (setf (link-target-transparent-read-thru linktarget) readthru)
	    (setf (link-target-transparent-write-thru linktarget) writethru)
	    (setf (link-target-transparent-rename-thru linktarget) renamethru)
	    (setf (link-target-transparent-create-thru linktarget) createthru)
	    (setf (link-target-transparent-delete-thru linktarget) deletethru)
	    
	    (write-out-buffer-from-addressor-if-needed linktarget)))
	    
      (if (fb-modified buf) (write-out-file-buffer buf))
      (downreference-file-buffer buf)

      (return-values file nil))))

Œ

;;;  "Little routines" to make create-file a shorter function


(defun fill-in-directory-entry (fd hdire grc author binp lvx)
  (store-file-name-from-fd fd hdire)
  (setf (directory-entry-file-type hdire)
	(if (eq (fd-file-type fd) ':directory) "*DIR*" (fd-file-type fd)))
  (setf (directory-entry-file-version hdire)
	(if (numberp (fd-file-version fd)) (fd-file-version fd) 0))
  (setf (directory-entry-bytesize hdire) (fd-byte-size fd))
  (setf (directory-entry-author hdire) author)
  (setf (directory-entry-number-of-records hdire) 1)
  (setf (directory-entry-record-0-address hdire) (fd-r0addr fd))
  (setf (directory-entry-byte-length hdire) 0)
  (setf (directory-entry-logical-volume-index hdire) lvx)
  (setf (directory-entry-binary-p hdire) binp)
  (setf (directory-entry-generation-retention-count hdire) grc)
  (setf (directory-entry-date-time-created hdire)
	(fd-date-time-created fd))
  (setf (directory-entry-unique-ID hdire) (fd-uid fd))
  (setf (directory-entry-link hdire) (not (null (fd-link-p fd))))
  (setf (directory-entry-directory hdire) (eq (fd-file-type fd) ':directory)))

(defun store-file-name-from-fd (fd hdire)
  (let ((namel (string-length (fd-file-name fd))))
    (setf (directory-entry-file-name-true-length hdire) namel)
    (setf (directory-entry-file-name hdire)
	  (if (> namel (directory-entry-file-name-max-length))
	      (substring (fd-file-name fd) 0 (directory-entry-file-name-max-length))
	      (fd-file-name fd)))))

(defun lay-out-file-header (file
			    &aux (part (fd-partition-desc file)))
			    			;actual hdr addressible
  (with-fileheader-addressibility-modifying (file 0 hdr)
    (setf (file-header-version hdr) *DEFAULT-HEADER-VERSION*)
    (setf (file-header-logical-size hdr) (fd-logical-header-length file))
    (setf (file-header-bootload-generation hdr) (partt-monotone-generator-a part))
    (setf (file-header-version-in-bootload hdr)
	  (let ((val (1+ (partt-monotone-generator-b part))))
	    (setf (partt-monotone-generator-b part) val)
	    val))
    (setf (file-header-number-of-elements hdr) 8.) ;defined by code below


    (let ((objl 0))

      (setf (file-header-fh-header-name hdr) "hedr")
      (setf (file-header-fh-header-location hdr) 0)
      (setf (file-header-fh-header-length hdr) (file-header-size-in-words))
	  

      (setf (file-header-header-fm-length hdr)
	    (setq objl
		  (+ (file-map-size-in-words) *DEFAULT-HEADER-FM-SIZE*)))
      (setf (file-header-header-fm-name hdr) "hdfm")
      (setf (file-header-header-fm-location hdr)
	    (lmfs-allocate-header-area
	     file
	     (+ (file-header-fh-header-location hdr)
		(file-header-fh-header-length hdr))
	     objl))


      (setf (file-header-dire-length hdr)
	    (setq objl (directory-entry-size-in-words)))
      (setf (file-header-dire-name hdr) "dire")
      (setf (file-header-dire-location hdr)
	    (lmfs-allocate-header-area
	     file
	     (+ (file-header-header-fm-location hdr)
		(file-header-header-fm-length hdr))
	     objl))


      (setf (file-header-info-length hdr)
	    (setq objl (fh-info-size-in-words)))
      (setf (file-header-info-name hdr) "info")
      (setf (file-header-info-location hdr)
	    (lmfs-allocate-header-area
	     file
	     (+ (file-header-dire-location hdr)
		(file-header-dire-length hdr))
	     objl))


      (setf (file-header-file-map-length hdr)
	    (setq objl (+ (file-map-size-in-words) *DEFAULT-FILE-FM-SIZE*)))
      (setf (file-header-file-map-name hdr) "fmap")
      (setf (file-header-file-map-location hdr)
	    (lmfs-allocate-header-area
	     file
	     (+ (file-header-info-location hdr)
		(file-header-info-length hdr))
	     objl))

      (let ((whatsleft (- (fd-logical-header-length file)
			  (+ (file-header-file-map-location hdr)
			     (file-header-file-map-length hdr)))))
	(if (< whatsleft 0)
	    (break file-header-doesnt-fit-in-initial-allocation))

	(setf (file-header-property-list-length hdr) whatsleft))
      (setf (file-header-property-list-name hdr) "plst")
      (setf (file-header-property-list-location hdr)
	    (+ (file-header-file-map-location hdr)
	       (file-header-file-map-length hdr)))

      (let ((high
	     (round-up-to-boundary (partt-dataw-per-block (fd-partition-desc file))
				   (+ (file-header-property-list-location hdr)
				      (file-header-property-list-length hdr)))))
	(setf (file-header-pad-area-name hdr) "    ")
	(setf (file-header-pad-area-location hdr) high)
	(setf (file-header-pad-area-length hdr) 0)
	(setf (file-header-pad2-area-name hdr) "    ")
	(setf (file-header-pad2-area-location hdr) high)
	(setf (file-header-pad2-area-length hdr) 0)
	high))))

(defun set-up-fh-info (file h ex)
  (with-fileheader-addressibility-modifying (file (file-header-info-location h) inf)
    (let ((par (fd-parent file)))
      (setf (fh-info-version inf) *DEFAULT-FILE-HEADER-INFO-VERSION*)
      (setf (fh-info-parent-dir-uid inf)
	    (if (null par) 0 (fd-uid par)))
      (setf (fh-info-parent-dir-part-id inf)
	    (if (null par) 0 (partt-partition-id (fd-partition-desc par))))
      (setf (fh-info-parent-dir-address inf)
	    (if (null par) 0 (fd-r0addr par)))
      (setf (fh-info-dir-entry-index-in-parent inf) ex)
      (setf (fh-info-duplicate-uid inf) (fd-uid file)))))

(defun initialize-real-file-map (file h)
  (with-fileheader-addressibility-modifying (file (file-header-file-map-location h) fm)
    (setf (file-map-allocated-length fm) (- (file-header-file-map-length h)
					    (file-map-size-in-words)))
    (setf (file-map-valid-length fm) 0)			;no data, just header.
    (let* ((dwpr (partt-dataw-per-record (fd-partition-desc file)))
	   (hrem (\ (fd-logical-header-length file) dwpr)))
      (setf (fd-addressible-length file) (if (zerop hrem) 0 (- dwpr hrem))))))


;;;  Allocate things guaranteed to fit in a block.

(defun lmfs-allocate-header-area (file whatsofar howmuch)
  (let ((bsize (partt-dataw-per-block (fd-partition-desc file))))
    (if (> howmuch bsize)
	(ferror nil "lmfs-allocate-header-area: won' t fit in a block: ^D words" howmuch))
    (let* ((nextb (round-up-to-boundary bsize whatsofar))
	   (left (- nextb whatsofar)))
      (if (> howmuch left) nextb whatsofar))))

Œ

(defun grow-file-to-addr (file addr)

  (check-arg-type file file-desc)
  (check-arg-type addr fixnum)

  (with-fileheader-addressibility (file 0 h)
    (with-fileheader-addressibility-modifying (file (file-header-file-map-location h) fm)
      (let* ((part (fd-partition-desc file))
	     (dpr (partt-dataw-per-record part))
	     (headerl (fd-logical-header-length file))
	     (hrecs (// (round-up-to-boundary dpr headerl) dpr))
	     (fulll (+ headerl addr))
	     (wantl (// 
		     (round-up-to-boundary
		      dpr
		      (+ fulll 1))		;forces next record
		     dpr)))
	(or (zerop (\ fulll dpr)) (break grow-file-illaligned-addr))

	(let ((newrec (grow-file-map file fm hrecs wantl)))
	  (if (numberp newrec)				;won
	      (let ((buf (find-usable-buffer file)))
		(setf (fd-addressible-length file)
		      (- (* wantl dpr) headerl))

		;; The seemingly gratuitous hack of creating this buffer
		;; serves the purpose of preventing buffer finder from
		;; wanting to read in gubbish currently in that record.
		
		(establish-buf-as-data-containing buf file addr)
		;; find-usable returns refcount of 1, use it that way
		(setf (fb-address buf) newrec)
		nil)
	      newrec))))))				;error indic

(defun grow-file-map (file fm hrecs wantl)
  (let ((rec (allocate-disk-record (fd-partition-desc file))))
    (if (fixp rec)
	(grow-file-map-i file fm (- (1- wantl) hrecs) rec))
    rec))

(defun grow-file-map-i (fd fm fmx rec)
  (if (< fmx (file-map-allocated-length fm))
      (progn
	(set-bufmod-from-addressor fm)
	(setf (file-map-element fm fmx) rec)
	(setf (file-map-valid-length fm) (1+ fmx))
	;; Write it out, check words solve all ruad problems.
	;; Always fm-update directories, null-parents, requested files (and hdr grows)
	(if (or (eq (fd-file-type fd) ':directory)
		(null (fd-parent fd))
		(fd-file-map-addenda fd))

	    (write-out-buffer-from-addressor fm)))
      (progn
	(if (zerop (file-map-link fm))
	    (progn					;header grow case
	      (if (not (= fmx (file-map-allocated-length fm)))
		  (fs-error "grow-file-map-i: non-contiguous grow" fd fm fmx rec))
	      (setf (file-map-link fm) (allocate-new-file-map-block fd))
	      (set-bufmod-from-addressor fm)
	      (write-out-buffer-from-addressor fm)))
	(with-fileheader-addressibility (fd (file-map-link fm) ffm)
	  (grow-file-map-i fd ffm (- fmx (file-map-allocated-length fm)) rec)))))
	  

(defun allocate-new-file-map-block (file)
  (multiple-value-bind (baddr blen)
      (allocate-new-header-block file)
    (with-fileheader-addressibility-modifying (file baddr fm)
      (setf (file-map-valid-length fm) 0)
      (setf (file-map-link fm) 0)
      (setf (file-map-allocated-length fm) (- blen (file-map-size-in-words)))
      baddr)))

;; Find an unused header block.  Grow the header if necessary.

(defstorage (fm-test-three)
     (words being fixnum
	    w1 w2 w3))

(defun test-three-zero-words (file addr)
  (with-fileheader-addressibility (file addr p)
    (and (zerop (fm-test-three-w1 p))
	 (zerop (fm-test-three-w2 p))
	 (zerop (fm-test-three-w3 p)))))

(defun allocate-new-header-block (file)
  (let* ((part (fd-partition-desc file))
	 (wpb (partt-dataw-per-block part))
	 (hlen (fd-logical-header-length file)))
    (values
      (or
	(loop for addr from 0 below hlen by wpb
	      if (test-three-zero-words file addr)
	      do (return addr)
	      finally (grow-file-header file))
	(loop for addr from hlen below (fd-logical-header-length file) by wpb	;it changed!
	      if (test-three-zero-words file addr)
	    do (return addr)
	    finally (fs-error "allocate-new-header-block: can't." file)))
      wpb)))
Œ

(defun expunge-file (fd				;=> fixnum recsgotten or errstring
		     &aux (records 0))
  (check-arg-type fd file-desc)
  (let ((part (fd-partition-desc fd)))
    (if (not (file-theoretically-deactivateable-p fd))
	(if (fd-openings fd)
	    "File is in use and may not be expunged."
	    "File appears to be in use by the file system and may not be expunged.")
	(progn

	 ;; Don't set modifyings ...

	 (with-fileheader-addressibility (fd 0 h)
	   (with-fileheader-addressibility (fd (file-header-header-fm-location h) hfm)
	     (with-fileheader-addressibility (fd (file-header-file-map-location h) fm)
	       (setq records
		     (+ (free-addresses-in-file-map fd fm)
			(free-addresses-in-file-map fd hfm 1)))
	       (let ((buf (buffer-from-addressor h)))
		 (zero-file-buffer buf)
		 (write-out-file-buffer buf))	;no header -- all words go incl chex
	       (deposit-disk-record (fd-r0addr fd) part)
	       (incf records))))
	 (deactivate-file fd)
	 records))))

(defun free-addresses-in-file-map (fd fm &optional (start 0) &aux
				   (part (fd-partition-desc fd)))
  (+ (if (zerop (file-map-link fm))
	 0
	 (with-fileheader-addressibility (fd (file-map-link fm) ffm)
	   (free-addresses-in-file-map fd ffm)))
     (do ((rno (1- (file-map-valid-length fm)) (1- rno))
	  (records 0))
	 ((< rno start) records)
       (deposit-disk-record
	 (prog1 (file-map-element fm rno)
		(setf (file-map-element fm rno) 0)	;gratuitous paranoia
		(incf records))
	 part))))

Œ
(defun read-file-record-into-buffer (file buf record &rest more-bufs)
  (check-arg-type file file-desc)
  (check-arg-type buf  file-buffer)
  (dolist (ab more-bufs)
    (check-arg-type ab file-buffer))

  (let* ((part (fd-partition-desc file))
	 (devid (partt-device-id part))
	 (rsize (partt-record-size-words part))
	 (raddr (relocate-address-to-partition part record))
	 (ercod (lexpr-funcall #'fs-read-disk devid rsize raddr (fb-array buf)
			       (mapcar #'fb-array more-bufs))))
    (cond ((null more-bufs) (incf (aref *READ-METER-VECTOR* 1)))
	  (( (1+ (length more-bufs))
	      (array-dimension-n 1 *READ-METER-VECTOR*))
	   (incf (aref *READ-METER-VECTOR* (1- (array-dimension-n 1 *READ-METER-VECTOR*)))))
	  (t (incf (aref *READ-METER-VECTOR* (1+ (length more-bufs))))))
    (if (null ercod)
	(progn
	  (setf (fb-creating buf) nil)
	  (setf (fb-address buf) record)
	  (setf (fb-modified buf) nil)
	  (setf (fb-good-p buf) t)
	  (loop for auxbuf in more-bufs
		as r = (1+ record) then (1+ r) do
		(setf (fb-creating auxbuf) nil)
		(setf (fb-address auxbuf) r)
		(setf (fb-modified auxbuf) nil)
		(setf (fb-good-p auxbuf) t))))
    ercod))

(defun get-record-address-for-io (file absword)
  (check-arg-type file file-desc)
  (check-arg-type absword fixnum)

  ;; he knoweth not from headers or data (visavis absword..)
  ;; absword doesn't know about check-words, though
  ;; i.e., absword is data words into the total file.

  (if (= absword 0)
      
      ;; Buck stops here.  Bottom line of recursion.
      
      (fd-r0addr file)

      ;; Otherwise, get the record address from the appropriate file map.

      (let* ((part (fd-partition-desc file))
	     (rsize (partt-dataw-per-record part))
	     (rno (// absword rsize))
	     (hrecs (// (round-up-to-boundary rsize (fd-logical-header-length file))
			rsize)))
	     
	(setq absword (round-down-to-boundary rsize absword))

	(with-fileheader-addressibility (file 0 h)
	  (if (< absword (fd-logical-header-length file))
	      
	      ;; A header address.

	      (with-fileheader-addressibility (file (file-header-header-fm-location h) hfm)
		(file-map-element hfm rno))

	      ;; A real file address.

	      (with-fileheader-addressibility (file (file-header-file-map-location h) fm)
		(get-file-map-element file fm (- rno hrecs))))))))

;;; Yes, gigantic files are going to be kind of slow when you want to get the
;;; record address.

(defun get-file-map-element (fd fm x)
  (cond ((< x (file-map-valid-length fm)) (file-map-element fm x))	;in the valid portion
	((< x (file-map-allocated-length fm))	;in the allocated portion, but not
						;the valid portion, MUST be an error
						;(were there more links, the 2 would be equal)
	 (fs-error "get-file-map-element: out of bounds" fm x))
	((zerop (file-map-link fm))
	 (fs-error "get-file-map-element: out-of-bounds, no link" fm x))
	(t (with-fileheader-addressibility (fd (file-map-link fm) ffm)
	     (get-file-map-element fd ffm (- x (file-map-allocated-length fm)))))))
	      

(defun write-out-file-buffer (buf &rest more-bufs)
  (check-arg-type buf file-buffer)

  (let* ((file (fb-file-desc buf))
	 (part (fd-partition-desc file))
	 (rsize (partt-record-size-words part))
	 (devid (partt-device-id part))
	 (raddr (relocate-address-to-partition part (fb-address buf))))
    (if (< raddr 1)
	(break write-out-file-buffer-bad-address))

    ;; the stuff must be good, or we never should have gotten here
    ;; to decide to write it. So assert its goodness.

    (cond ((null more-bufs) (incf (aref *WRITE-METER-VECTOR* 1)))
	  (( (1+ (length more-bufs))
	      (array-dimension-n 1 *WRITE-METER-VECTOR*))
	   (incf (aref *WRITE-METER-VECTOR* (1- (array-dimension-n 1 *WRITE-METER-VECTOR*)))))
	  (t (incf (aref *WRITE-METER-VECTOR* (1+ (length more-bufs))))))
    (let ((ercod (lexpr-funcall #'fs-write-disk devid rsize raddr (fb-array buf)
				(mapcar #'fb-array more-bufs))))
      (or ercod
	  (prog1
	   nil
	   (setf (fb-good-p buf) t)
	   (setf (fb-modified buf) nil)
	   (if (zerop (fb-reference-count buf)) (free-file-buffer buf))
	   (dolist (ab more-bufs)
	     (setf (fb-good-p ab) t)
	     (setf (fb-modified ab) nil)
	     (if (zerop (fb-reference-count ab)) (free-file-buffer ab))))))))

;;;----------------------------------------------------------------------

(defun update-file-attributes (file &rest what
				    &aux (curtime (time:get-universal-time)))
  (if (loop for x in what finally (return nil) if x do (return t))
							;some guys make
							;gratuitous calls
      (progn
	(with-fileheader-addressibility (file 0 h)
	  (with-fileheader-addressibility-modifying (file (file-header-dire-location h) hdire)
	    (dolist (key what)
	      (selectq key
		(:byte-size          (setf (directory-entry-bytesize hdire) (fd-byte-size file)))
		(:byte-length	     (setf (directory-entry-byte-length hdire) (fd-byte-length file)))
		(:date-time-modified (setf (directory-entry-date-time-modified hdire) curtime))
		(:date-time-used     (setf (directory-entry-date-time-used hdire) curtime))
		(:there-are-properties (setf (directory-entry-properties-were-put hdire) t))
		(:number-of-records  (setf (directory-entry-number-of-records hdire)
					   (let ((dwpr (partt-dataw-per-record (fd-partition-desc file))))
					     (// (round-up-to-boundary
						  dwpr
						  (+ (fd-logical-header-length file)
						     (fd-addressible-length file)))
						 dwpr))))
		(:delete	     (setf (directory-entry-deleted hdire) t)
				     (setf (directory-entry-date-time-deleted hdire) curtime))
		(:undelete	     (setf (directory-entry-deleted hdire) nil)
				     (setf (directory-entry-date-time-deleted hdire) 0))
		(:generation-retention-count
				     (setf (directory-entry-generation-retention-count hdire)
					   (if (null (fd-grc-info file)) nil (fd-grc-info file))))
		(t (cond ((listp key)
			  (selectq (first key)
			    (:author (setf (directory-entry-author hdire)	;for sys copy
					   (string (second key))))
			    (:creation-date
			             (setf (directory-entry-date-time-created hdire)
					   (second key))
				     (setf (fd-date-time-created file) (second key)))
			    (:incrementally-backed-up
			             (setf (directory-entry-incrementally-backed-up hdire) t)
				     (with-fileheader-addressibility-modifying
				       (file (file-header-info-location h) fhi)
				       (setf (fh-info-inc-dump-tapename fhi) (second key))
				       (setf (fh-info-inc-dump-date fhi) (third key))
				       (write-out-buffer-from-addressor fhi)))
			    (:completely-backed-up
			             (setf (directory-entry-completely-backed-up hdire) t)
				     (with-fileheader-addressibility-modifying
				       (file (file-header-info-location h) fhi)
				       (setf (fh-info-comp-dump-tapename fhi) (second key))
				       (setf (fh-info-comp-dump-date fhi) (third key))
				       (write-out-buffer-from-addressor fhi)))
			    (:deleted  (setf (directory-entry-deleted hdire) (second key))
				       (setf (directory-entry-date-time-deleted hdire)
					     (if (second key) curtime 0)))
			    (:dont-delete
			               (setf (directory-entry-safety-switch hdire)
					     (second key)))))))))

	    (write-out-buffer-from-addressor-if-needed hdire)

	    ;; Thing is good enuf out there if any if this blows out..

	    ;; Copy stuff to real directory entry now.

	    (let ((par (fd-parent file)))
	      (if (not (symbolp par))			;not root or sim.
		  (progn
		   (let ((eoffset (entry-index-to-offset par (fd-entry-index file))))
		     (with-filedata-addressibility-modifying (par eoffset entry)
		       (if (= (fd-uid file) (directory-entry-unique-ID entry))
			   (copy-directory-entry hdire entry)
			   (fs-error "connection failure at attribute update" file entry))
		       (write-out-file-buffer (buffer-from-addressor entry))))))))))))

(defun get-backup-info (fd)
  (with-fileheader-addressibility (fd 0 h)
    (with-fileheader-addressibility (fd (file-header-dire-location h) dire)
      (with-fileheader-addressibility (fd (file-header-info-location h) fhi)
	(let ((plist nil))
	  (if (directory-entry-completely-backed-up dire)
	      (progn
		(push (fh-info-comp-dump-date fhi) plist)
		(push ':complete-dump-date plist)
		(push (string-trim " " (fh-info-comp-dump-tapename fhi)) plist)
		(push ':complete-dump-tape plist)))
	  (if (directory-entry-incrementally-backed-up dire)
	      (progn
		(push (fh-info-inc-dump-date fhi) plist)
		(push ':incremental-dump-date plist)
		(push (string-trim " " (fh-info-inc-dump-tapename fhi)) plist)
		(push ':incremental-dump-tape plist)))
	  plist)))))

(defun copy-directory-entry (from to)
  (set-bufmod-from-addressor to)	;!!!! Should fix "rename" bug...
  (let ((ebytesize (* *BYTES-PER-WORD* (directory-entry-size-in-words))))
    (fs-copy-array-portion from 0 ebytesize to 0 ebytesize)))

Œ
;;; Header growance.

;;; General strategy.
;;;
;;;   1. Get a new record for the new piece. If this fails now, nothing is lost.
;;;   2. Invalidate everybody's opening addressors.
;;;   3. Write out ALL CURRENT BUFFERS. File is now consistent.
;;;   4. Get a hold of buffer for record who's gonna change meaning.
;;;   5. Hold buffer list in hand so that none of this gets out until the world
;;;      is reasonably consistent.  If anything blows, these buffers dont exist.
;;;   6. Get a new "random buffer". Magically establish it as right new thing.
;;;   7. Copy the current data from highest header record into this guy.
;;;   8. Zero out/initialize free blocks in new guy.
;;;   9. Write out new guy.  On-disk header never heard of him, he's bogus as far
;;;      as incremental permanent state can see.
;;;
;;;   Two cases here.  Either the hfm-containing record is the one being changed,
;;;   or not.

;;;    Case 1.  HFM record is one who will lose data to new record.

;;;  10. Diddle bufctl words to indicate new status of this record.
;;;  11. Muck over header file map to indicate new record, incl. headerlen.
;;;  12. Zero/initialize new free blocks.
;;;  13. Write out header.
;;;  14. Modify active header length. If thing aborts between 13 and 14, h0-readr
;;;      knows to update active header-length magically. Same below.
;;;  15. Thread buffers back/in.

;;;;   Case 2.  Intermediate record needs reorganization.

;;;  10. Put the HFM in an intermediate state where there is a nonzero entry
;;;      one beyond the length in the file map.  It points to the new guy.
;;;  11. Write out HFM.  Header length has not been hacked.
;;;      If we activate during this new state, we do 12 and 13 by hand from the activator.
;;;  12. Fix up the old intermed and write it out. The activator can do this all over again
;;;      if it determines it needs to. Now is the interesting window.
;;;  13. Fix the HFM state and write it out.
;;;  14. Hack with active header length as above.
;;;  15. Thread buffers etc.

;;;     Good luck....

(defun grow-file-header (fd)
  (let* ((part (fd-partition-desc fd))
	 (increment (partt-dataw-per-record part))
	 (newheaderl (+ increment (fd-logical-header-length fd)))
	 (new-record (allocate-disk-record part))
	 hfmbuf intermedbuf newbuf intermed-addressor)
    (if (not (fixp new-record))
	new-record
	(unwind-protect
	  (progn
	    (dolist (opening (fd-openings fd))
	      (funcall opening ':invalidate-addressors))
	    (flush-buffers-for-file fd)
	    (with-fileheader-addressibility (fd 0 fh)
              (with-fileheader-addressibility-modifying
		(fd (file-header-header-fm-location fh) hfm)

		(setq hfmbuf (buffer-from-addressor hfm))
		(setq intermed-addressor
		      (get-general-file-addressor t fd (1- (fd-logical-header-length fd))))
		(setq intermedbuf (buffer-from-addressor intermed-addressor))

		;; That was all setup.

		;; Get the buffers outta the lists.

		(setf (fd-buffer-list fd)
		      (delq hfmbuf
			    (delq intermedbuf (fd-buffer-list fd))))

		;; Anything blows, it didn't happen.

		;; Establish the new buffer.

		(without-interrupts
		  (setq newbuf (allocate-random-buffer))	;maybe fix protocol on this
		  (upreference-file-buffer newbuf))
		
		;; Just to initialize it..
		(establish-buf-as-header-containing newbuf
						    fd
						    (fb-lowest-header-addr intermedbuf))

		;; This does the real work..

		(set-up-buf-as-header-containing
		  newbuf fd (+ (fb-lowest-header-addr intermedbuf) increment) newheaderl)

		(file-buffer-data-copy intermedbuf newbuf)
		(place-buffer-block-check-words newbuf nil)
		(file-buffer-zero-header-therein newbuf)

		(setf (fb-address newbuf) new-record)
		(write-out-file-buffer newbuf)

		;; Figure out which case

		(cond ((eq hfmbuf intermedbuf)	;one record, no window

		       (set-up-buf-as-header-containing hfmbuf fd 0 newheaderl)
		       (setf (file-map-element hfm (file-map-valid-length hfm))
			     new-record)
		       ;; Remember, window-lovers, this buf is threaded out.
		       (setf (file-map-valid-length hfm)
			     (1+ (file-map-valid-length hfm)))

		       (setf (file-header-logical-size fh) newheaderl)

		       (file-buffer-clear-from-address intermedbuf
						       (fd-logical-header-length fd)))

		      (t			;There is an intermediate buffer.

		       (setf (file-map-element hfm (file-map-valid-length hfm)) new-record)
		       (write-out-file-buffer hfmbuf)
		       ;; That takes care of the new record, but not the active length.
		       
		       ;; Now (possibly to be redone) fix up the old intermed.

		       (set-up-buf-as-header-containing
			 intermedbuf fd (fb-lowest-header-addr intermedbuf) newheaderl)
		       (place-buffer-block-check-words intermedbuf nil)

		       (file-buffer-clear-from-address intermedbuf
						       (fd-logical-header-length fd))

		       (write-out-file-buffer intermedbuf)
		       (setf (file-map-valid-length hfm)
			     (1+ (file-map-valid-length hfm)))))

		(setq new-record nil)	;for unwind-protect
		(write-out-file-buffer hfmbuf)
		(setf (fd-logical-header-length fd) newheaderl)
	      
		(push hfmbuf (fd-buffer-list fd))
		(push newbuf (fd-buffer-list fd))
		(if (not (eq intermedbuf hfmbuf))
		    (push intermedbuf (fd-buffer-list fd)))
		(update-file-attributes fd ':number-of-records))))

	  (progn				;unwind-protect cleanups
	    ;; hfmbuf taken care of by macros.

	    (if new-record (deposit-disk-record new-record part))
	    (if newbuf (downreference-file-buffer newbuf))
	    (if intermed-addressor (downreference-addressor intermed-addressor))))
	t)))


;;; This gets invoked from the activator (activate-file) when file is in the (crashed)
;;; state of most of the (t) clause above...

;;; We have to get the intermed guy, clear out his top, fix his check words, fix the hfm.

(defun finish-aborted-header-grow (fd)
  (let* ((recl (partt-dataw-per-record (fd-partition-desc fd)))
	 (newheaderl (+ (fd-logical-header-length fd) recl))
	hfmbuf intermedbuf curhrecs)

    (unwind-protect
      (with-fileheader-addressibility (fd 0 h)
        (with-fileheader-addressibility-modifying (fd (file-header-header-fm-location h) hfm)
	  (setq curhrecs (file-map-valid-length hfm))
	  (setq hfmbuf (buffer-from-addressor hfm))
	  
	  (setf (fd-buffer-list fd) (delq hfmbuf (fd-buffer-list fd)))

	  (without-interrupts
	    (setq intermedbuf (allocate-random-buffer))
	    (upreference-file-buffer intermedbuf))

	  (set-up-buf-as-header-containing intermedbuf fd (* recl curhrecs))
	  (setf (fb-address intermedbuf)
		(file-map-element hfm (1- curhrecs)))
	  (let ((errp
		  (read-file-record-into-buffer fd intermedbuf (fb-address intermedbuf))))
	    (if (null errp)
		(setq errp
		      (if (place-buffer-block-check-words intermedbuf nil)
			  nil
			  "FHN ERROR CNF F Check words don't match")))
	    (if errp (fs-error "unacceptable error during header grow recovery" intermedbuf
			       errp)))
	  (set-up-buf-as-header-containing intermedbuf fd (* recl curhrecs) newheaderl)
	  (place-buffer-block-check-words intermedbuf nil)
	  (file-buffer-clear-from-address intermedbuf (fd-logical-header-length fd))
	  (write-out-file-buffer intermedbuf)
	  
	  (setf (file-map-valid-length hfm) (1+ curhrecs))
	  (write-out-file-buffer hfmbuf)
	  (setf (fd-logical-header-length fd) newheaderl)
	  (push hfmbuf (fd-buffer-list fd))
	  (push intermedbuf (fd-buffer-list fd))
	  (update-file-attributes fd ':number-of-records)))
      (if intermedbuf (downreference-file-buffer intermedbuf)))))
