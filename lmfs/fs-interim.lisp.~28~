;-*- Mode:LISP; Package:LMFS -*-

;; "This is just random scaffolding.."  -DLW

(defvar *fs-cleanly-shut-down* t)
(defvar *added-lmfs-to-initialization-lists* nil)
(defvar *the-partition* nil)

(declare (special *THE-ROOT*))

(defvar *the-partition-base* nil)
(defvar *the-partition-len* nil)
(defvar *the-partition-limit* nil)

(defvar *last-uid* 0)
(defvar *the-partition* nil)

(defun generate-partition-id (&rest ignore)
  (generate-file-uid))

(defun generate-file-uid (&rest ignore)		;interim versions, see the name of this file.
  (do () (())
    (let ((time (time:get-universal-time)))
      (if (> time *last-uid*)
	  (return (setq *last-uid* time))))))

(defun shutwarm ()
  (and *the-root* *the-partition* (wring-freemap *the-partition*)))

(defun shut () (if *the-partition* (shutwarm)))

(defun get-vol-from-volx (&rest ignore) nil)

(defun find-partition-from-part-id (part-id)
  (if (= part-id (partt-partition-id *the-partition*))
      *the-partition*
      (ferror nil "Partition not found - ID ~O" part-id)))

(defun fs-error (&rest args) (ferror ':lmfs-internal-error "~S" args))

(defun choose-allocation-partition (&rest ignore)
  *the-partition*)

(defun fs-file-part-init (part-name &optional (unit 0))
  (or (fs-get-partition part-name unit)
      (ferror nil "Can't find partition ~A on ~D" part-name unit))
  (multiple-value-bind (part errcode)
      (init-partition nil unit *the-partition-base* *the-partition-len* "file" 256. 4.)
    (if errcode (fs-error "Can't init *the-partition*" errcode))
    (setq *the-partition* part)))

(defun init-any-file-part (name &optional (unit 0))
  (multiple-value-bind (start len)
      (si:find-disk-partition name nil unit)
    (if (null start)
	(ferror nil "Can't find partition ~A on unit ~D." name unit))
    (multiple-value-bind (part errcode)
	(init-partition nil unit start len "file" 256. 4)
      (or errcode part))))

(defun fstart (part-name)
  (setq *the-root* nil)
  (setq *the-partition* nil)
  (if (not *fs-cleanly-shut-down*) (clear-fs-resources))
  (loop for unit from 0 to 7
	finally (return nil)
	do
	(and (fs-get-partition part-name unit)
	     (multiple-value-bind (part err)
		 (open-partition
		   unit *the-partition-base* *the-partition-len* part-name 256. 4)
	       (cond ((and (null err) (not (stringp part)))
		      (setq *fs-cleanly-shut-down* nil)
		      (setq *the-partition* part)
		      (return t)))))))
  
(defun fs-get-partition (part-name unit)
  (multiple-value-bind (start len)
      (si:find-disk-partition part-name nil unit)
    (cond (start
	   (setq *the-partition-base* start)
	   (setq *the-partition-len* len)
	   (setq *the-partition-limit* (+ *the-partition-base* *the-partition-len*))
	   t))))

(defun fs-read-disk (devid rsize raddr &rest bufs)
  rsize
  (fs-check-raddr-range raddr)
  (lexpr-funcall #'file-disk-read devid raddr bufs)
  nil)

(defun fs-write-disk (devid rsize raddr &rest bufs)
  rsize
  (fs-check-raddr-range raddr)
  (lexpr-funcall #'file-disk-write devid raddr bufs)
  nil)


(defun fs-check-raddr-range (raddr)
  raddr)

(comment
  (if (or (< raddr *the-partition-base*)
	  (>= raddr *the-partition-limit*))
      (ferror nil "fs-check-raddr-range: bogus raddr: ~D s//b in [~D,~D)"
	      raddr *the-partition-base* *the-partition-limit*)))

(declare (special *the-root*))

(defun create-root ()
  (setq *the-root* (create-directory *the-partition* nil "root")))


(defun 8bit-dump (sv n)
  (dotimes (i n)
    (let ((a (* 4 i)))
      (let ((a1 (aref sv (+ a 3)))
	    (a2 (aref sv (+ a 2)))
	    (a3 (aref sv (+ a 1)))
	    (a4 (aref sv (+ a 0))))
	(format t "~%~4O F: ~12O H: ~6O ~6O B: ~3O ~3O ~3O ~3O C: ~C~C~C~C"
	      i
	      (+ a4 (lsh (+ a3 (lsh (+ a2 (lsh a1 8)) 8)) 8))
	      (+ a2 (lsh a1 8))
	      (+ a4 (lsh a3 8))
	      a1 a2 a3 a4
	      a4 a3 a2 a1)))))

(deff simvector-dump #'8bit-dump)

(defun lmfs-clear-resource (x)
  (let ((r (or (get x 'defresource)
	       (ferror nil "lmfs-clear-resource: not a resource: ~S" x))))
    (setf (si:resource-object-list r) nil)))

(defun clear-fs-resources ()
  (mapc #'lmfs-clear-resource '(fs-8bit-addressing-array fs-buffer))
  (setq fs-zrc-bufs nil))


(defun setup-for-disk-save ()
  (if (or *the-root* *the-partition*)
      (progn
	(shutwarm)
	(setq *the-root* nil *the-partition* nil fs-zrc-bufs nil)
	(mapc #'force-free-resources '(fs-8bit-addressing-array fs-buffer))
	(mapc #'(lambda (x) (blast-clear-buffer (car x)))
	      (si:resource-object-list (get 'fs-buffer 'defresource)))
	(setq *fs-cleanly-shut-down* t))))

(defun force-free-resources (r)
  (mapc #'(lambda (x) (setf (second x) nil)) (si:resource-object-list (get r 'defresource))))

(if (null *added-lmfs-to-initialization-lists*)
    (progn
      (add-initialization "LMFS" '(setup-for-disk-save) '(:before-cold))
      (add-initialization "LMFS logout" '(shut) '(:logout))
      (add-initialization "LMFS" '(fstart "FILE"))
      (setq *added-lmfs-to-initialization-lists* t)))

