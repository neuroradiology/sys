;-*- Mode:LISP; Package:TAPE; Lowercase:T -*-
;;; (C) Copyright Symbolics, Inc., 1981

;;; Sys 75 version

(defvar *TRACE-TAPE-STATUS* nil)
(defvar *trace-tape-error-retries* t)  ;--t for now, change to nil later
(defconst *tape-read-retry-count* 5)
(defconst *tape-write-retry-count* 5)

(defstruct (tape-buffer :named (:conc-name tpb-))
  (rqb nil)
  (8ary nil)
  (wiredp nil)
  (count 0)
  (bufx 0)
  (file-buffer nil)
  (error-state nil)
  (state ':idle))

(defstruct (tape-file-buffer :named (:conc-name tpfb-))
  buf
  phybuf
  done
  byte-length)

(defflavor tape-stream-mixin
	((state ':closed)  dma-buf stream-buf (io-in-progress nil) io-retry-count
	 (unit 0) (density 1600.) (swap-bytes nil) (mode ':read) (no-bot-prompt)
	 (options nil) (last-wrote nil) (buffer-size nil) (pad-char nil)
	       (record-length nil))
	()
  (:init-keywords :no-read-ahead :mode)
  (:gettable-instance-variables mode state record-length unit)
  (:initable-instance-variables unit mode record-length
				buffer-size pad-char density no-bot-prompt))

(defflavor tape-input-stream-mixin () ()
	   (:included-flavors tape-stream-mixin))

(defflavor tape-output-stream-mixin () ()
	   (:included-flavors tape-stream-mixin))

(defflavor tape-input-stream ()
	   (tape-input-stream-mixin si:buffered-input-character-stream))

(defflavor tape-output-stream ()
	   (tape-output-stream-mixin si:buffered-output-character-stream))


(defun open-tape (&optional (unit 0) &rest rest)
  (lexpr-funcall #'make-instance
		 (if (tv:doplist (rest prop ind)
		       (if (and (eq ind ':mode) (eq prop ':write))
			   (return t)))
		     'tape-output-stream
		     'tape-input-stream)
		 ':unit unit
		 rest))


(defmethod (tape-stream-mixin :init) (plist)
  (if (get plist ':no-read-ahead) (push ':no-read-ahead options))
  (setq record-length (or record-length buffer-size 4096.)
	buffer-size (or buffer-size record-length))
  (if (> record-length buffer-size)
      (ferror nil "Record length ~D greater than buffer size ~D." record-length buffer-size))
  (or (zerop (\ record-length 4))
      (ferror nil "Record length must be a multiple of 4 for some reason"))
  (multiple-value (dma-buf stream-buf)
		 (funcall-self ':create-buffers))
  (if (and (not no-bot-prompt)
	   (prog1 t (funcall-self ':check-ready))
	   (not (funcall-self ':bot-p))
	   (fquery format:yes-or-no-quietly-p-options
		   "Tape ~D is not at beginning-of-tape.  Shall I rewind it? "
		   unit))
      (progn
	(funcall-self ':offline-op ':rewind)
	(tape-wait-rewind-complete)))
  (funcall-self ':clear-error))

(defmethod (tape-stream-mixin :create-buffers) ()
  (let ((bufpages (// (lmfs:round-up-to-boundary (* 4 sys:page-size) buffer-size)
		      (* 4 sys:page-size))))
    (values (make-tape-buffer-i bufpages)
	    (make-tape-buffer-i bufpages))))
    
(defun make-tape-buffer-i (pages) 
  (let* ((rqb (si:get-disk-rqb pages))
	 (8ary (array-leader rqb sys:%disk-rq-leader-8-bit-buffer)))
    (make-tape-buffer 8ary 8ary rqb rqb)))


(defmethod (tape-stream-mixin :start-io) (cmd)
  (if io-in-progress
      (ferror nil "start-io: IO already in progress on dma buf ~S" dma-buf))
  (si:wire-disk-rqb (tpb-rqb dma-buf))
  (setf (tpb-wiredp dma-buf) t)
  (setf (tpb-error-state dma-buf) nil)
  (setf (tpb-state dma-buf) ':ioing)
  (setq io-in-progress cmd)
  (setq io-retry-count 0)
  (if (tpb-file-buffer dma-buf)
      (let ((tpfb (tpb-file-buffer dma-buf)))
	(setf (tpfb-done tpfb) nil)
	(lmfs:wire-file-buffers-and-rqb (eq cmd ':read) (list (tpfb-phybuf tpfb)))
	(tape-command unit cmd
		      (%p-contents-offset (tpfb-phybuf tpfb) 1)	;1 for header
		      (tpfb-byte-length tpfb)))
      (tape-command unit cmd (rqb-to-data-virtual-address (tpb-rqb dma-buf))
		    (if (eq cmd ':read)
			record-length
			(lmfs:round-up-to-boundary 4 (tpb-count dma-buf))))))	;write 4s

(defmethod (tape-stream-mixin :clear-error) (&rest ignore)
  (setf (tpb-file-buffer stream-buf) nil)
  (setf (tpb-error-state stream-buf) nil)
  (setf (tpb-file-buffer dma-buf) nil)
  (setf (tpb-error-state dma-buf) nil))
  
  
;;;Input side

(defmethod (tape-input-stream-mixin :next-input-buffer)	(&rest ignore)
  (funcall-self ':validate-input-buffer)
  (cond ((eq state ':input-eof)
	 (funcall-self ':discard-current-input-buffer-i)
	 nil)
	(t (values (tpb-8ary stream-buf) 0 (tpb-count stream-buf)))))

(defmethod (tape-input-stream-mixin :discard-input-buffer) (array)
  (or (eq array (tpb-8ary stream-buf))
      (ferror nil "You gave me another buffer than I gave you"))
  (funcall-self ':discard-current-input-buffer-i))

(defmethod (tape-input-stream-mixin :discard-current-input-buffer-i) ()
  (setq state ':input)
  (setf (tpb-file-buffer stream-buf) nil)
  (setf (tpb-state stream-buf) ':used-up-stream)
  (setf (tpb-bufx stream-buf) (tpb-count stream-buf))
  (setf (tpb-error-state stream-buf) nil))

(defmethod (tape-input-stream-mixin :clear-eof) (&rest ignore))

(defmethod (tape-input-stream-mixin :after :clear-error) (&rest ignore)
  (setq state ':input)
  (if (memq ':no-read-ahead options)
      (setf (tpb-state dma-buf) ':holding)
      (funcall-self ':start-io ':read)))

(defmethod (tape-input-stream-mixin :validate-input-buffer) (&optional no-read-ahead)
  (cond ((tpb-error-state stream-buf)
	 (if (tape-eofstatp (tpb-error-state stream-buf))
	     (setq state ':input-eof)
	     (process-tape-error (tpb-error-state stream-buf))))
	;; Stream buf used up. Get the dma buf. Start it if its not going or compltd already.
	((= (tpb-bufx stream-buf) (tpb-count stream-buf))
	 (setf (tpb-state stream-buf) ':used-up-stream)
	 (selectq (tpb-state dma-buf)
	   ((:holding :used-up-stream)     (funcall-self ':start-io ':read)
					   (funcall-self ':await-dma-buf))
	   (:completed)
	   (:ioing                         (funcall-self ':await-dma-buf))	   
	   (t (ferror nil "Mysterious dma-buf state in :validate-input-buffer: ~A"
		      (tpb-state dma-buf))))
	 (funcall-self ':await-dma-buf)
	 (funcall-self ':switch-buffers)
	 (if (null (tpb-error-state stream-buf))
	     (if (or (memq ':no-read-ahead options) no-read-ahead)
		 (setf (tpb-state dma-buf) ':holding)
		 (funcall-self ':start-io ':read))
	     (if (tape-eofstatp (tpb-error-state stream-buf))
		 (setq state ':input-eof)))
	 (if (and (zerop (tpb-count stream-buf))
		  (not (tpb-file-buffer stream-buf))
		  (not (tpb-error-state stream-buf)))
	     (ferror nil "tape stream :validate-input-buffer - zero count, no error")))
	;; Stream buf has stuff in it -- make this check gratuitously.
	(t
	 (if (eq (tpb-state dma-buf) ':ioing)
	     (funcall-self ':check-tape-completion)))))
	 
(defmethod (tape-input-stream-mixin :fill-input-file-buffer) (buf phybuf)
  (if (not (memq ':no-read-ahead options))
      (ferror nil "Can't fill file buffers with read ahead. Sorry."))
  (funcall-self ':await-dma-buf)		;make sure not running
  (let ((tpfb (make-tape-file-buffer
		done        nil
		buf	    buf
		phybuf	    phybuf
		byte-length (* 4 (array-dimension-n 1 phybuf)))))
    (let ((bytes-left (- record-length (tpb-count stream-buf))))
      (if (or (tpb-file-buffer stream-buf) (zerop bytes-left))
	  (funcall-self ':clear-input))
      (setf (tpb-file-buffer stream-buf) tpfb))
    (funcall-self ':switch-buffers)
    (funcall-self ':start-io ':read)))

(defmethod (tape-input-stream-mixin :await-input-file-buffer) (buf)
  (if (not (and (tpb-file-buffer dma-buf)
		(eq (tpfb-buf (tpb-file-buffer dma-buf)) buf)))
      (ferror nil "Awaited file buffer ~S is not known to tape opening ~S." buf self))
  (funcall-self ':validate-input-buffer))


(defmethod (tape-input-stream-mixin :skip-file) (&optional (n 1))
  (funcall-self ':discard-current-input-buffer)	;Maybe he didn't use up the input buffer,
						;and the validate will fail to eat dma..
  (funcall-self ':await-dma-buf)
  (if (and (tpb-error-state dma-buf)
	   (tape-eofstatp (tpb-error-state dma-buf)))
      (progn
	(funcall-self ':validate-input-buffer ':no-read-ahead)
	(or (eq state ':input-eof)
	    (ferror nil "Skip file - not in input eof"))
	(funcall-self ':discard-current-input-buffer-i)
	(decf n)
	(setf (tpb-state dma-buf) ':holding)))
  (dotimes (i n)
    (funcall-self ':offline-op ':skip-file)
    (setq io-retry-count 0)
    (setf (tpb-error-state dma-buf)
	  (if (tape-error-p)
	      (tape-errstat)
	      nil))
    (setf (tpb-state dma-buf) ':completed)
    (if (and (tpb-error-state dma-buf)
	     (tape-eofstatp (tpb-error-state dma-buf)))
	(progn
	  (funcall-self ':validate-input-buffer)
	  (setf (tpb-state dma-buf) ':holding)
	  (funcall-self ':discard-current-input-buffer-i))))
  (funcall-self ':discard-current-input-buffer-i))

;;; Output side

(defmethod (tape-output-stream-mixin :after :clear-error) (&rest ignore)
  (setq state ':output)
  (setf (tpb-state stream-buf) ':filling)
  (setf (tpb-count stream-buf) 0))

(defmethod (tape-output-stream-mixin :new-output-buffer) ()
  (or (zerop (tpb-count stream-buf))
      (ferror nil
	      "No right to ask for new output buffer, neither sent nor discarded old one"))
  (values (tpb-8ary stream-buf) 0 record-length))

(defmethod (tape-output-stream-mixin :discard-output-buffer) (array)
  (or (eq array (tpb-8ary stream-buf))
      (ferror nil "You're trying to discard a different output buffer than I gave you.")))

(defmethod (tape-output-stream-mixin :send-output-buffer) (buf endindex)
  (or (eq buf (tpb-8ary stream-buf))
      (ferror nil "You're trying to send a different output buffer than I gave you."))
  (setf (tpb-count stream-buf) endindex)
  (funcall-self ':end-record))

(defmethod (tape-output-stream-mixin :end-record) ()
  (funcall-self ':await-dma-buf)
  (if (tpb-error-state dma-buf)
      (process-tape-error (tpb-error-state dma-buf)))
  (if (or (plusp (tpb-count stream-buf))
	  (tpb-file-buffer stream-buf))
      (progn
	(if pad-char (loop for i from (tpb-count stream-buf) below record-length
			   do (aset pad-char (tpb-8ary stream-buf) i)
			   finally (setf (tpb-count stream-buf) record-length)))
	(funcall-self ':switch-buffers)
	(setf (tpb-count stream-buf) 0)
	(setf (tpb-state stream-buf) ':filling)
	(funcall-self ':start-io ':write))))

(defmethod (tape-output-stream-mixin :finish) ()
  (cond ((tpb-file-buffer stream-buf)
	 (funcall-self ':end-record))		;avoid stream stuff, which will punt..
	(t (funcall-self ':force-output)))
  (funcall-self ':await-dma-buf)
  (if (tpb-error-state dma-buf) (process-tape-error (tpb-error-state dma-buf))))

(defmethod (tape-output-stream-mixin :file-buffer-out) (buf phybuf) 
  (let ((tpfb (make-tape-file-buffer
		done        nil
		buf	    buf
		phybuf	    phybuf
		byte-length (* 4 (array-dimension-n 1 phybuf)))))
    (let ((bytes-left (- record-length (tpb-count stream-buf))))
      (if (or (tpb-file-buffer stream-buf) (zerop bytes-left))
	  (funcall-self ':force-output))
      (setf (tpb-file-buffer stream-buf) tpfb)))
  (funcall-self ':finish))

(defmethod (tape-output-stream-mixin :check-file-buffer) (buf)
  (if (and (tpb-file-buffer stream-buf)
	   (eq (tpfb-buf (tpb-file-buffer stream-buf)) buf))
      nil				;not done
      (if (and (tpb-file-buffer dma-buf)
	       (eq (tpfb-buf (tpb-file-buffer dma-buf)) buf))
	  (let ((tpfb (tpb-file-buffer dma-buf)))
	    (if (tpfb-done tpfb)
		(progn
		  (setf (tpb-file-buffer dma-buf) nil)
		  t)			;handle errors later
		nil))			;not done
	  t)))	    		;not found = done.

(defmethod (tape-output-stream-mixin :eof) tape-write-eof-common)
(defmethod (tape-output-stream-mixin :write-eof) tape-write-eof-common)
(declare-flavor-instance-variables (tape-stream-mixin)
(defun tape-write-eof-common (ignore &optional (n 1))
  (funcall-self ':finish)
  (dotimes (i n)
    (tape-wait-op-complete)
    (tape-command unit ':write-eof))
  (tape-wait-op-complete)
  (setq last-wrote ':eof)))


;;;Common stuff

(defmethod (tape-stream-mixin :await-dma-buf) ()
  (loop while io-in-progress
	do (tape-wait-op-complete)		;perhaps more sophist needed here?
	   (funcall-self ':check-tape-completion)))

(defmethod (tape-stream-mixin :check-tape-completion) (&aux stat)
  (cond ((neq (tpb-state dma-buf) ':ioing))	;I/O not started
	((not (tape-complete-p)));I/O still in progress
	((or (not (tape-error-p))		;Successful completion
	     (tape-eotstatp (setq stat (tape-errstat)))
	     (tape-eofstatp stat)	;or EOF or EOT
	     ( io-retry-count			;or unsuccessful completion
		(selectq io-in-progress
		  (:read *tape-read-retry-count*)
		  (:write *tape-write-retry-count*)
		  (otherwise 0))))
	 (cond (*TRACE-TAPE-STATUS*
		(tape-errprint (tape-errstat))
		(format t "!!!")))
	 (if (minusp (tape-byte-residue))
	     (ferror nil ":check-tape-completion - negative residue"))
	 (if (eq io-in-progress ':write)
	     (setq last-wrote ':data))
	 (setq io-in-progress nil)
	 (setf (tpb-state dma-buf) ':completed)
	 (setf (tpb-wiredp dma-buf) nil)
	 (setf (tpb-error-state dma-buf)
	       (if (tape-error-p)
		   (tape-errstat)
		   nil))
	 (let ((tpfb (tpb-file-buffer dma-buf)))
	   (cond (tpfb
		  (lmfs:unwire-file-buffers-and-rqb (list (tpfb-phybuf tpfb)))
		  (setf (tpfb-done tpfb) t))
		 (t
		  (si:unwire-disk-rqb (tpb-rqb dma-buf))
		  (setf (tpb-bufx dma-buf) 0)
		  (setf (tpb-count dma-buf)
			(- record-length (tape-byte-residue)))
		  (or ( 0 (tpb-count dma-buf) record-length)
		      (ferror nil "Tape control transferred too many bytes?"))))))
	(t					;Recoverable error -- initiate retry
	 (incf io-retry-count)
	 (cond (*TRACE-TAPE-STATUS*
		(tape-errprint (tape-errstat))
		(format t "!!!"))
	       (*trace-tape-error-retries*
		(format t "~&Retry #~D of tape ~A operation.  Status:"
			  io-retry-count io-in-progress)
		(tape-errprint (tape-errstat))
		(terpri)))
	 (tape-command unit ':space-reverse nil 1)
	 (tape-wait-op-complete)
	 (let ((tpfb (tpb-file-buffer dma-buf)))
	   (tape-command unit
			 (if (eq io-in-progress ':write) ':write-erg io-in-progress)
			 (if tpfb (%p-contents-offset (tpfb-phybuf tpfb) 1)
			     (rqb-to-data-virtual-address (tpb-rqb dma-buf)))
			 (cond (tpfb (tpfb-byte-length tpfb))
			       ((eq io-in-progress ':read) record-length)
			       (t (lmfs:round-up-to-boundary 4 (tpb-count dma-buf)))))))))

(defmethod (tape-stream-mixin :switch-buffers) ()
  (let ((cur-stream stream-buf)
	(cur-dma dma-buf))
    (setf (tpb-state cur-dma) ':stream)
    (setf (tpb-error-state cur-stream) nil)
    (setf (tpb-state cur-stream) ':used-up-stream)
    (setq dma-buf cur-stream stream-buf cur-dma)))


(defmethod (tape-stream-mixin :rewind) ()
  (funcall-self ':offline-op ':rewind)
  (setq last-wrote nil))

(defmethod (tape-stream-mixin :backspace) ()
  (funcall-self ':offline-op ':space-reverse))

(defmethod (tape-stream-mixin :offline-op) (op)
  (funcall-self ':await-dma-buf)
  (if (eq mode ':read) (funcall-self ':discard-current-input-buffer-i))
  (setf (tpb-count stream-buf) 0)
  (setf (tpb-bufx stream-buf) 0)
  (setf (tpb-state dma-buf) ':holding)
  (setf (tpb-error-state stream-buf) nil)
  (setf (tpb-error-state dma-buf) nil)
  (if (eq op ':skip-file)
      (tape-command unit ':space-forward nil 0)
      (tape-command unit op))
  (tape-wait-op-complete)			;frees controller
  ;;don't wait for rewind to complete, do this offline.
  )




(declare-flavor-instance-variables (tape-stream-mixin)
(defun process-tape-error (err)
  (cond ((tape-eotstatp err)
	 (setq last-wrote nil)
	 (ferror ':end-of-tape "The end of the tape was unexpectedly encountered."))
	(t
	 (tape-errprint err)
	 (if (not (drive-ready-p unit))
	     (do ((first nil t)) ((drive-ready-p unit))
	       (if first (tape-errprint (tape-status unit)))
	       (*catch 'error-restart
		 (cerror nil t nil
			 "Tape drive ~D ~:[has become~;is still~] unready.~@
			Type Control-Meta-C to restart." unit first))))
	 (ferror ':tape-error "Tape error - Hardware status ~O" err)))))

(defmethod (tape-stream-mixin :set-offline) ()		;probbly at eot
  (funcall-self ':await-dma-buf)
  (tape-command unit ':off-line)
  (tape-wait-op-complete)
  (setq mode ':offlined))

(defmethod (tape-stream-mixin :check-ready) (&optional (stream terminal-io))
  (do () (())
    (if (drive-ready-p unit)
	(return t)
	(progn
	  (format stream "~&Tape drive ~A does not appear to be ready.~@
                 Type any character when it is ready: " unit)
	  (funcall stream ':tyi)))))

(defmethod (tape-stream-mixin :close) (&optional ignore)
  (selectq mode
    (:closed)
    (:offlined     (tape-wait-op-complete)	;Stop touching my DMA buffer
		   (funcall-self ':truly-close-out))
    (:read         (tape-wait-op-complete)	;Stop touching my DMA buffer
		   (funcall-self ':truly-close-out))
    (:write	   (and last-wrote (funcall-self ':finish))
		   (selectq last-wrote
		     (nil)
		     (:data   (funcall-self ':write-eof 2)
			      (funcall-self ':backspace))
		     (:eof    (funcall-self ':write-eof 1)
			      (funcall-self ':backspace)))
		   (funcall-self ':truly-close-out)))
  (setq mode ':closed))

(defmethod (tape-stream-mixin :truly-close-out) ()
  (si:return-disk-rqb (tpb-rqb dma-buf))
  (si:return-disk-rqb (tpb-rqb stream-buf)))

(defmethod (tape-stream-mixin :bot-p) ()
  (tape-bot-p (tape-status unit)))

