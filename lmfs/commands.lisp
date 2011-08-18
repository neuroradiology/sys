;;; <LMFS>COMMANDS.LSP;1 17-Jun-81 11:14:04, Edit by BSG -*-Package:LMFS; Mode:LISP-*-

;;; Copyright (c) Symbolics, Inc., 1981
;;; Debugging and testing commands for local file system. Supposed to
;;; run in maclisp and lispm, nobody is supposed to call anything in here.

(declare (special *the-partition*))

(defun fmbs (bno)
  (let* ((fmf (freemap-file (partt-free-record-info *the-partition*)))
	 (dwpb (partt-dataw-per-block (fd-partition-desc fmf))))
    (with-filedata-addressibility (fmf 0 fmh)
      (let ((nb (freemap-header-number-of-blocks fmh))
	    (rrib (freemap-header-records-represented-in-block fmh))
	    (bpw (freemap-header-n-bits-per-word fmh)))
	(if (> bno nb) (ferror nil "Bad block number: ~O" bno))
	(with-filedata-addressibility (fmf (* bno dwpb) fmb)
	  (format t "~%Block #~D, header has ~D.~%" bno
		  (freemap-block-blocknum fmb))
	  (format t "~%Covers record (o) ~O to but not incl ~O, ~O rec//blk"
		  (freemap-block-recno-of-bit-0 fmb)
		  (freemap-block-highest-recno+1 fmb)
		  rrib)
	  (format t "~%This is (decimal) ~D records accounted for."
		  (- (freemap-block-highest-recno+1 fmb) (freemap-block-recno-of-bit-0 fmb)))
	  (let ((cfr (loop with s = 0 for i from 0 to (1- (freemap-block-number-of-words fmb))
			   finally (return s)
			   do (setq s (+ s (bits-on-per-word (freemap-block-array fmb i)))))))
	    (format t "~% ~O (o) words, ~O (o) free recs ~:[~; (s//b ~O)~]"
		    (freemap-block-number-of-words fmb)
		    (freemap-block-number-of-free-recs fmb)
		    (not (= cfr (freemap-block-number-of-free-recs fmb)))
		    cfr)
	    (format t "~%Decimally, ~D found free, meaning ~D in use."
		    cfr (- (freemap-block-highest-recno+1 fmb)
			   (freemap-block-recno-of-bit-0 fmb)
			   cfr)))
	  (let ((csthere (freemap-block-checksum fmb))
		(csright (fs-freemap-block-checksum fmb bpw)))
	    (if (= csthere csright)
		(format t "~% Checksum is ~O, which is right." csthere)
		(format t "~% Checksum indicated as ~O, but we think it s//b ~O."
			csthere csright))))))))

(defun free-record-status (&optional (part *the-partition*)
			   &aux
			   (freemap (partt-free-record-info part))
			   (fmf (freemap-file freemap))
			   (dwpb (partt-dataw-per-block part)))
			   
  (format t "~&Partition ~S, size ~D (dec) records, free map is ~D block~:P.~%"
	  part (partt-disk-addr-limit part) (freemap-file-nblocks freemap))
  (loop for bno from 1 to (freemap-file-nblocks freemap)
	with total-rec = 0
	and free-rec = 0
	finally (progn
		  (format t "~&Active record buffer has ~D free records out of ~D maximum."
			  (freemap-cur-howmany freemap) (freemap-pool-size freemap))
		  (incf free-rec (freemap-cur-howmany freemap))
		  (let ((used (- total-rec free-rec)))
		    (format t "~%~%Grand Total ~D (decimal) free, ~D used out of ~D (~D%)."
			    free-rec used total-rec (// (* used 100.) total-rec))))
	do
	
	(with-filedata-addressibility (fmf 0 fmh)
	  (let ((nb (freemap-header-number-of-blocks fmh)))
	    (if (> bno nb) (ferror nil "Bad block number: ~O" bno))
	    (with-filedata-addressibility (fmf (* bno dwpb) fmb)
	      (let ((total-here (- (freemap-block-highest-recno+1 fmb)
				   (freemap-block-recno-of-bit-0 fmb))))
		(format
		  t "~%Block #~D, record (o) ~O up to but not ~O, ~D free (dec) out of ~D"
		  (freemap-block-blocknum fmb)
		  (freemap-block-recno-of-bit-0 fmb)
		  (freemap-block-highest-recno+1 fmb)
		  (freemap-block-number-of-free-recs fmb)
		  total-here)
		(incf total-rec total-here)
		(incf free-rec (freemap-block-number-of-free-recs fmb))))))))

(defun directory-skeleton ()
  (directory-skeleton-recurse (fs:parse-pathname ">" *LOCAL-FS-HOST*) 0))

(defun directory-skeleton-recurse (inferiors-path indent)
  (format t "~&~VX~A" indent (funcall inferiors-path ':directory))
  (dolist (inferior (cdr(fs:directory-list (funcall inferiors-path ':new-pathname
						    ':name ':wild
						    ':type ':directory
						    ':version ':wild))))
    (directory-skeleton-recurse
      (funcall (car inferior) ':condense-directory)
      (1+ indent))))
	    
(deff dskel 'directory-skeleton)
(deff walk-directory-tree 'directory-skeleton)
(deff wdt 'directory-skeleton)

(declare (*lexpr #M => time:print-universal-time fs:parse-pathname))
(declare (special *LOCAL-FS-HOST* *PATH-DELIMITER*))

(defun lmfs-parse-and-default-path (x)
  (fs:merge-and-set-pathname-defaults (fs:parse-pathname x)))

(defun fstat (namestring)
  (db-print-file-status (lmfs-parse-and-default-path namestring)))

(defun db-print-file-status (path)
  (let ((opening (send path ':open '(:probe :deleted))))
    (if (or (stringp opening) (symbolp opening))
	(format t "~%~%Error - ~A" opening)
	(let* ((name (send opening ':namestring))
	       (fd (send opening ':get-lmfs-fd))
	       (par (fd-parent fd))
	       (ex (fd-entry-index fd)))
	  (db-print-file-status-i name 0 par ex)
	  (if (fd-link-p fd)
	      (format t "~%Entry is a LINK to ~A, transparencies ~S."
		      (fd-link-p fd) (get-link-transparencies fd)))
	  (send opening ':close)))))

(defun db-print-file-status-i (name ilev par ex
				    &aux eo)
  (setq eo (entry-index-to-offset par ex))
  (with-filedata-addressibility (par eo entry)
    (format t "~%~%~VXFile ~A, #~D @ ~O" ilev name ex eo)
    (format t "~%~VXCreated ~A Used ~A Mod ~A" ilev
	    (time:print-universal-time (directory-entry-date-time-created entry) nil)
	    (time:print-universal-time (directory-entry-date-time-used entry) nil)
	    (time:print-universal-time (directory-entry-date-time-modified entry) nil))
    (if (directory-entry-deleted entry)
	(format t "~%~VXEntry was deleted at ~A" ilev
		(time:print-universal-time (directory-entry-date-time-deleted entry) nil)))
    (format t "~%~VX~D records, ~D bytes of ~D bits each." ilev
	    (directory-entry-number-of-records entry)
	    (directory-entry-byte-length entry)
	    (directory-entry-bytesize entry))))


(declare (special *the-root* *LOCAL-FS-HOST*)(*lexpr get-fd-from-dn-en))
(defvar ws-merge-pn nil)
(defun ws (&optional (dn ">"))
  (if (null ws-merge-pn) (setq ws-merge-pn (fs:parse-pathname (string-append
								*LOCAL-FS-HOST* ":"
								*PATH-DELIMITER*))))
  
  (multiple-value-bind (fd err)
    (get-filedesc-from-path dn ':directory 1)
    (if err (ferror nil "Err: ~A  ~%Getting ~A." err dn))
    (ws-recurse dn fd 0)))

(defun ws-recurse (dname dir lev)
  (let ((contents (simple-list-directory dir)))
    (dolist (l contents)
      (let ((n (first l))
	    (ilev (* 3 lev))
	    (fname (second l))
	    (type (third l))
	    (version (fourth l))
	    (linkp (fifth l)))
	(if (and (eq type ':directory) (not linkp))
	    (let* ((dirpath (send ws-merge-pn ':new-pathname ':directory dname ':name fname))
		   (dpath (send dirpath ':string-for-printing)))
	      (format t "~%~%~VXDirectory ~A #~D" ilev dpath n)
	      (ws-recurse
	       dpath
	       (get-fd-from-dn-en dname fname ':directory ':newest)
	       (1+ lev)))
	    (progn
	     (db-print-file-status-i
	      (send 
	       (send ws-merge-pn ':new-pathname
		   ':directory dname ':name fname ':type type ':version version)
	       ':string-for-printing)
	      ilev dir n)
	     (if linkp (format t "~%~VXEntry is a LINK." ilev))))))))


(defun was (&optional (dir *the-root*))			;walk active subtree
  (was-recurse dir 0))

(defun was-recurse (file lev)
  (format t "~%~VX ~A ~A ~D   (~O)"
	  (* 3 lev)
	  (fd-file-name file) (fd-file-type file) (fd-file-version file)
	  #M(maknum file)
	  #Q (%pointer file))
  (dolist (son (fd-sons file))
    (was-recurse son (1+ lev))))


(defun wsad (dn fcn env &rest options)
  (if (null dn) (setq dn ">"))
  (if (null ws-merge-pn) (setq ws-merge-pn (fs:parse-pathname
					     (string-append *LOCAL-FS-HOST* ":"
							    *PATH-DELIMITER*))))
  (multiple-value-bind (fd err)
      (get-filedesc-from-path dn ':directory 1)
    (if err (ferror nil "Err: ~A  ~%Getting ~A." err dn))
    (wsa-recurse dn fd 0 fcn env options)))

(defun wsa-recurse (path fd lev fcn env options &aux (ct 0))
  (if (and (eq (fd-file-type fd) ':directory) (not (fd-link-p fd)))
      (progn
	(if (memq ':dirs-wanted options)
	    (funcall fcn fd path lev env))
	(let ((contents (simple-list-directory fd)))
	  (format t "~&~A ~D files .. " path (length contents))
	  (dolist (l contents)
	    (incf ct)
	    (format t "~D " ct)
	    (let ((fname (second l))
		  (type (third l))
		  (version (fourth l)))
	      (let ((newpn
		      (send 
			(send ws-merge-pn ':new-pathname
			      ':directory path ':name fname
			      ':type  (if (eq type ':directory) nil type)
			      ':version (if (eq type ':directory) nil version))
			':string-for-printing)))
		(multiple-value-bind (fd err)
		    (get-fd-from-dn-en path fname type version ':deleted)
		  (if (null fd)
		      (format t "~%Error ~A on ~A" err newpn)
		      (wsa-recurse newpn fd (1+ lev) fcn env options))))))))
      (funcall fcn fd path lev env)))

(defun wsa-test ()
  (wsad ">" #'(lambda (fd p lev env) (format t "~%~VX~A ~S" (* 3 lev) p fd)) nil
	':dirs-wanted))


(defun kludge-fix-links ()
  (wsad ">"
	#'(lambda (fd p lev env)
	    (if (fd-link-p fd)
		(let ((realpath (with-filedata-addressibility (fd 3 l)
				  (link-target-pathname l))))
		  (with-filedata-addressibility-modifying (fd 0 rfb)
		    (format t "~%~A -- ~A" p realpath)
		    (setf (link-target-pathname rfb) realpath)
		    (write-out-buffer-from-addressor rfb)))))
	nil))

(defun kludge-fix-plists-1 ()
    (wsad ">"
	#'(lambda (fd p lev env)
	    (with-fileheader-addressibility (fd 0 h)
	      (if (and (zerop (file-header-pad-area-length h))
		       (zerop (file-header-pad2-area-length h)))
		  (format t "~%skipping ~A" p)
		  (let ((real-left (- (fd-logical-header-length fd)
				      (file-header-property-list-location h))))
		    (setf (file-header-property-list-length h) real-left)
		    (setf (file-header-pad-area-length h) 0)
		    (setf (file-header-pad2-area-length h) 0)
		    (setf (file-header-pad-area-location h) (fd-logical-header-length fd))
		    (setf (file-header-pad2-area-location h) (fd-logical-header-length fd))
		    (set-bufmod-from-addressor h)
		    (write-out-buffer-from-addressor h)
		    (format t "~%Did ~A ~O" p real-left)))))
	nil
	':dirs-wanted))


(defmacro with-active-file ((f v) . body)
  (let ((opening (gensym))
	(param (gensym))
	(fd (or v (and (symbolp f) f) (ferror nil "Bad syntax in with-active-file, no var."))))
    `(let ((,param ,f)
	   (,opening nil)
	   (,fd nil))
       (unwind-protect
	(progn
	 (cond ((typep-2 ,param 'file-desc) (setq ,fd ,param))
	       #M((sfap ,param) (setq ,fd (send (sfa-call ,param 'opening nil) ':get-lmfs-fd)))
	       ((or (typep-2 ,param 'local-lmfs-pathname)
		    #M(and (typep-2 ,param 'flavor-instance)
			   (eq (send ,param ':typep) 'local-lmfs-pathname))
		    )
		(setq ,opening (open ,param))
		(setq ,fd (send ,opening ':get-lmfs-fd)))
	       ((or (stringp ,param) (symbolp ,param))
		(setq ,opening (open (lmfs-parse-and-default-path ,param)))
		(setq ,fd (send ,opening ':get-lmfs-fd)))
	       ((or (typep-2 ,param 'lmfs-opening)
		    #M(and (typep-2 ,param 'flavor-instance) (eq (send ,param ':typep) 'lmfs-opening))
		    )
		(setq ,fd (send ,param ':get-lmfs-fd)))
	       (t (ferror nil "Can't coerce ~S into a file." ,param)))
	 . ,body)
	(and ,opening (send ,opening ':close))))))


(defun plst (f)						;any kind of file
  (with-active-file (f)
    (with-fileheader-addressibility (f 0 h)
      (do ((plbp (file-header-property-list-location h)))
	  ((zerop plbp) (terpri)(princ "done"))
	(with-fileheader-addressibility (f plbp plb)
	  (format t "~2%Property List v. ~D, block at ~o, ~O (o) props."
		  (plist-block-version plb) plbp (plist-block-number-of-properties plb))
	  (format t "~%Total length ~o, perceived deallocated ~o."
		  (plist-block-total-length plb) (plist-block-perceived-words-deallocated plb))
	  (format t "~%Highest alloc. ~o, lowest free ~o."
		  (plist-block-lowest-high-word-allocated plb) (plist-block-lowest-free-word plb))
	  (format t "~%Next fragment at ~o" (plist-block-next-fragment plb))
	  (loop for i from 0 below (plist-block-number-of-properties plb)
		do
		(let ((iloc (plist-block-props-indicator plb i))
		      (ploc (plist-block-props-property plb i)))
		  (with-fileheader-addressibility (f iloc p)
		    (format t "~2%   ind @ ~o, len ~o ~30T~A"
			    iloc (vstring-data-length p) (vstring-data p)))
		  (with-fileheader-addressibility (f ploc p)
		    (format t "~%  prop @ ~o, len ~o ~30T~A"
			    ploc (vstring-data-length p) (vstring-data p)))))
	  (setq plbp (plist-block-next-fragment plb)))))))

(defun kludge-fix-plists-2 ()
    (wsad ">"
	#'(lambda (fd p lev env)
	    lev env
	    (with-fileheader-addressibility (fd 0 h)
	      (if (= (with-fileheader-addressibility (fd (file-header-property-list-location h) pl)
		       (plist-block-version pl))
		     1)
		  (format t "~%Skipping ~A" p)
		  (progn
		   (fs-plist-init fd h)
		    (format t "~%Did ~A" p)))))
	nil
	':dirs-wanted))

(defun pl-get (f ind)
  (with-active-file (f)
    (fs-plist-get f ind)))

(defun pl-put (f val ind)				;yay maclisp
  (with-active-file (f)
    (fs-plist-put f ind val)))

(declare (*lexpr fs:parse-pathname))

(defun df (file word &optional (count 1))
 (with-active-file (file fd)
   (with-filedata-addressibility (fd word a)
     (simvector-dump a count))))

(defun dfh (file word &optional (count 1))
  (with-active-file (file)
    (with-fileheader-addressibility (file word a)
      (simvector-dump a count))))

(defun rnff (f1 f2)
  (send (lmfs-parse-and-default-path f1) ':rename (lmfs-parse-and-default-path f2)))

(declare (*lexpr mopen))

(defun dfap (path)					;dump ascii file path
  (let ((f #M(mopen path)
	   #Q(send (lmfs-parse-and-default-path path) ':open)))
    (terpri)
    (do nil  (nil)
    	(let ((line (readline f)))
	  (if (null line) (return (close f)))
	  (progn
	   (princ  line)
	   (terpri))))))

(defun dlfp (path)
  (send (lmfs-parse-and-default-path path) ':delete))

(defun lsdp (dpath)
  (multiple-value-bind (fd err)
    (get-filedesc-from-path dpath ':directory 1)
    (if err (ferror nil "Error dirpath ~A ~A" err dpath))
    (mapc 'print (simple-list-directory fd)))
  nil)

(defun gpde (dpath name type version)
  (multiple-value-bind (fd err)
    (get-filedesc-from-path dpath ':directory 1)
    (if err (ferror nil "Error dirpath ~A ~A" err dpath))
    (search-directory fd name type version ':deleted)))

(defun dcf (dpath name type version) (delete-confail-entry dpath name type version))

(defun delete-confail-entry (dpath name type version)	;cloudge
  (let ((de (gpde dpath name type version)))
    (setf (directory-entry-deleted de) t)
    (set-bufmod-from-addressor de)
    (write-out-buffer-from-addressor de)
    (downreference-addressor de)
    t))

(defun pdwp (path)				;works for dirs, etc. if given fd
  (with-active-file (path fd)
    (terpri)
    (princ "word no? ")
    (let ((w (read))
	  (m nil))
      (df fd w 1)
      (with-filedata-addressibility (fd w adr)
	(princ "New word? ")
	(setq m (fb-modified (buffer-from-addressor adr)))
	(let ((data (read)))
	  (#M simvector-aset #Q aset (ldb 0010 data) adr 0)
	  (#M simvector-aset #Q aset (ldb 1010 data) adr 1)
	  (#M simvector-aset #Q aset (ldb 2010 data) adr 2)
	  (#M simvector-aset #Q aset (ldb 3010 data) adr 3))
	(df fd w 1)
	(set-bufmod-from-addressor adr)
	(write-out-buffer-from-addressor adr)
	(setf (fb-modified (buffer-from-addressor adr)) m))
      (df fd w 1))))


(defun phwp (fd)
  (with-active-file (fd)
    (terpri)
    (princ "word no? ")
    (let ((w (read))
	  (m nil))
      (dfh fd w 1)
      (with-fileheader-addressibility (fd w adr)
	(princ "New word? ")
	(setq m (fb-modified (buffer-from-addressor adr)))
	(let ((data (read)))
	  (#M simvector-aset #Q aset (ldb 0010 data) adr 0)
	  (#M simvector-aset #Q aset (ldb 1010 data) adr 1)
	  (#M simvector-aset #Q aset (ldb 2010 data) adr 2)
	  (#M simvector-aset #Q aset (ldb 3010 data) adr 3))
	(dfh fd w 1)
	(set-bufmod-from-addressor adr)
	(write-out-buffer-from-addressor adr)
	(setf (fb-modified (buffer-from-addressor adr)) m))
      (dfh fd w 1))))

(defun ddr (recno &optional (word 0)(count 5))
  (or *the-partition* (ferror nil "Partition not defined yet."))
  (let* ((buf (allocate-random-buffer))
	 (part *the-partition*)
	 (ercod (fs-read-disk (partt-device-id part)
			      2000
			      (relocate-address-to-partition part recno)
			      (fb-array buf))))

       (if ercod
	   (progn
	    (deallocate-resource 'fs-buffer buf)
	    (ferror nil "Error reading rec ~D for ~A: " recno
		    (partt-partition-name part))))
       (let ((adr (obtain-8bit-addrarray buf)))
	 (unwind-protect
	  (progn
	   (upreference-file-buffer buf)
	   (setf (fb-address buf) recno)
	   (set-word-address-in-addressor adr (fb-array buf) word)
	   (simvector-dump adr count))
	  (downreference-addressor adr)))))

(defun check-check-words-globally ()
  (wsad nil #'check-words-checker nil  ':dirs-wanted))

(defun check-words-checker (fd path ignore ignore)
  (let ((tot-length (+ (fd-addressible-length fd) (fd-logical-header-length fd))))
    (loop for x from 0 below tot-length by (partt-dataw-per-record (fd-partition-desc fd))
	  do
	  (if (< x (fd-logical-header-length fd))
	      (cwc-bufdo fd path t x)
	      (cwc-bufdo fd path nil (- x (fd-logical-header-length fd)))))))



(defun cwc-bufdo (fd path headerp x)
  (multiple-value-bind (buf err)
      (let ((*checking-check-words* NIL))
	(get-buffer-given-fd-and-wordaddr headerp fd x))
    (or err (if (not (place-buffer-block-check-words buf t))
		(progn (format t "~% path ~A headerp ~S addr ~S Checkwords lose."
			       path headerp x)
		       (if (yes-or-no-p "Fix? ")
			   (progn
			     (place-buffer-block-check-words buf nil)
				   (write-out-file-buffer buf))))))
    (if buf (downreference-file-buffer buf))))


(defun compute-records-used-globally ()
  (let ((env (list 0)))
    (wsad nil #'cru-walk env ':dirs-wanted)
    (format t "~%~D records used, PLUS 2 or three for freemap itself." (car env))))

(defun cru-walk (fd path ignore env)
  (let ((tot-length (+ (fd-addressible-length fd) (fd-logical-header-length fd))))
    (incf (car env) (// tot-length (partt-dataw-per-record (fd-partition-desc fd))))))
