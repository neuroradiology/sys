;;; <LMFS>VOLS.LSP;1  8-May-81 11:19:15, Edit by BSG -*-Package:LMFS; Mode:LISP-*-


(defvar *TRACK-SIZE-RECS* 4.)			;real answer needed someday
(defvar *LABEL-VERSION* 1)
(defvar *THE-ROOT* nil)
(defvar *PART-START-RESERVE* 10.)			;skip for labels

#M (eval-when (compile eval)
     (load 'defstorage)
     (load 'bufmacs)
     (load 'fs-dcls))


(defmacro with-label-addressible ((part adr err) . body)
  (let ((buf (gensym)) (labaddr (gensym)) (ercod (gensym)))
    `(let* ((,err nil)
	    (,buf (allocate-random-buffer))
	    (,labaddr 0)
	    ;	   (2labaddr (+ *TRACK-SIZE-RECS* labaddr))
	    (,ercod (fs-read-disk (partt-device-id ,part)
				  (partt-record-size-words ,part)
				  (relocate-address-to-partition part ,labaddr)
				  (fb-array ,buf))))

       (if ,ercod
	   (progn
	    (deallocate-resource 'fs-buffer ,buf)
	    (setq ,err
		  (format nil "Error reading partition label for ~A: "
			  (partt-partition-name ,part))))
	   (let ((,adr (obtain-8bit-addrarray ,buf)))
	     (unwind-protect
	      (progn
	       (upreference-file-buffer ,buf)
	       (setf (fb-address ,buf) ,labaddr)
	       (set-word-address-in-addressor ,adr (fb-array ,buf) 0)
	       (if (not (= (partition-label-version ,adr) *LABEL-VERSION*))
		   (setq ,err (format nil "Bad label version on ~A, ~S"
				      (partt-partition-name ,part)
				      (partition-label-version ,adr)))
		   (progn . ,body)))
	      (downreference-addressor ,adr)))))))


	
(defun write-out-vol-label (adr part)
  (let* ((buf (buffer-from-addressor adr))
	 (ercod (fs-write-disk (partt-device-id part)
			       (partt-record-size-words part)
			       (relocate-address-to-partition part (fb-address buf))
			       (fb-array buf))))
    (if ercod
	(format nil "Cannot write back label ~A: ~S"
		(partition-label-name adr) ercod)
	nil)))



(defunp open-partition (device address len name		;device-rel addr
			       block-size-words record-size-blocks)

  (let ((part (make-partition
	        device-id          device
		address-base-in-blocks address
		disk-addr-limit    (// len record-size-blocks)
		partition-name     name
		block-size-words   block-size-words
		record-size-blocks record-size-blocks)))

    (compute-partition-desc-params part)
    (with-label-addressible (part lab err)
      (if err (return-values nil err))
      (or (string-equal (partition-label-name lab) name)
	  (return-values
	   nil (format nil "Wrong partition name in label- s//b ~A, is ~A."
		       name (partition-label-name lab))))
      (setf (partt-partition-id part) (partition-label-partition-id lab))
      (setf (partt-uid-generator part) (partition-label-uid-generator lab))
      (setf (partt-monotone-generator-a part) (partition-label-monotone-generator lab))
      (setf (partt-monotone-generator-b part) 0)
      
      ;;  -volume-table  TBDone
      
      (let ((err
	     (load-freemap part (partition-label-free-store-info lab))))
	(if err (return-values nil err)))
      (setf (partition-label-label-accepted lab)
	    (time:get-universal-time))
      (let ((errs (activate-roots part)))

	(let ((ercod (write-out-vol-label lab part)))
	  (if ercod (if errs
			(progn (deactivate-roots part)
			       (return-values nil ercod)))
	      (return-values part errs)))))))


(defun compute-partition-desc-params (part)
  (setf (partt-record-size-words part)
	(* (partt-record-size-blocks part) (partt-block-size-words part)))
  (setf (partt-dataw-per-block part)
	(- (partt-block-size-words part)
	   (* 2 (block-check-words-size-in-words))))
  (setf (partt-dataw-per-record part)
	(* (partt-record-size-blocks part)
	   (partt-dataw-per-block part)))
  
  (setf (partt-active-directory-list part)nil)
  (setf (partt-buffer-list part) nil)			;needs work
  (setf (partt-root-list part) nil))



(defunp init-partition  (vol device address len name	;device-rel addr
			     block-size-words record-size-blocks)
  
  (let ((part (make-partition
	       volume             vol
	       partition-name	  name
	       device-id	  device
	       address-base-in-blocks address
	       disk-addr-limit    (// len record-size-blocks)
	       block-size-words   block-size-words
	       record-size-blocks record-size-blocks
	       partition-id	  (generate-partition-id)
	       monotone-generator-b 0
	       uid-generator      (ash (fix (random)) -1))))

    (setf (partt-monotone-generator-a part)
	  (ash (partt-partition-id part) -1))

    (compute-partition-desc-params part)

    (let* ((buf (allocate-random-buffer))
	   (labaddr 0)
	   ;;(2labaddr (+ *TRACK-SIZE-RECS* labaddr))
	   )
      (zero-file-buffer buf)
      (upreference-file-buffer buf)

      (let ((lab (obtain-8bit-addrarray buf)))
	(unwind-protect
	 (progn
	  (set-word-address-in-addressor lab (fb-array buf) 0)
	  (setf (partition-label-version lab) *LABEL-VERSION*)
	  (setf (partition-label-name lab) name)
	  (setf (partition-label-label-size lab)
		(partition-label-size-in-words))
	  (setf (partition-label-partition-id lab) (partt-partition-id part))
	  (setf (partition-label-uid-generator lab) (partt-uid-generator part))
	  (setf (partition-label-monotone-generator lab) (partt-monotone-generator-a part))
	  
	  ;; root table -volume-table  TBDone

	  ;; let roots stay zero for now....
	  (init-freemap part
			*PART-START-RESERVE*
			(- (partt-disk-addr-limit part) *PART-START-RESERVE*))

	  (setf (partition-label-free-store-info lab)
		(fd-r0addr (freemap-file (partt-free-record-info part))))
	  (setf (partition-label-label-accepted lab)
		(time:get-universal-time))

	  (let ((ercod (fs-write-disk device
				      (partt-record-size-words part)
				      (relocate-address-to-partition part labaddr)
				      (fb-array buf))))
	    (if ercod
		(return-values nil (format nil "Cannot write back label ~A: ~S"
					   name ercod))
		(return-values part nil))))
	 (downreference-addressor lab))))))




(defun activate-roots (part &aux errs)
  (with-label-addressible (part lab ec)
    (if ec (list ec)
	(let ((praddr (partition-label-primary-root lab)))
	  (or (zerop praddr)
	      (let ((err (activate-a-root part praddr "primary")))
		(if err (push err errs)))))
	
	;; do rootlist here when ready
	
	errs)))


(defun activate-a-root (part r0addr id)
  (multiple-value-bind (fd err)
    (activate-file-from-header nil part r0addr				 
			       (format nil "root ^S for ~A"
				       id (partt-partition-name part))
			       nil)
    (if err err
	(if (= (with-filedata-addressibility (fd 0 dirh)
		 (dir-header-hierarchy-depth dirh))
	       0)
	    (progn
	     (setf (fd-parent fd) nil)
	     (if (not (null *THE-ROOT*))
		 (fs-error "Heute ist zur Wurzel ein Bruder geboren"))

	     ;;Hodie natus est etc.
	     (setq *THE-ROOT* fd)
	     (push fd (partt-root-list part))
	     nil)
	    (connect-fd-to-hierarchy fd)))))		;returns err


(defun update-label-roots (part)			;called from create-dir
  (with-label-addressible (part lab ec)
    (if ec ec
	(progn
	 (dolist (r (partt-root-list part))
	   (let ((r0addr (fd-r0addr r)))		;get rec 0 address
	     (if (and (null (fd-parent r))		;real live root!
		      (eq part (fd-partition-desc r)))
		 (setf (partition-label-primary-root lab) r0addr)
		 ;; do secondary roots here
		 )))
	 (write-out-vol-label lab part)))))	;returns errcode

