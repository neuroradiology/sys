;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8. -*-



(defresource embedded-length-buf (type length)
  :constructor (make-array length ':type type)
  :initial-copies 0
  :matcher (and ( (array-dimension-n 1 object) length)
		(eq (array-type object) type)))

(defflavor embedded-length-output-mixin
	((el-buf nil)				;the buffer
	 (target-stream nil)			;the target stream
	 (el-index)				;index of next byte to go into buf
	 array-type
	 array-size)
	()
  (:required-flavors buffered-output-stream)
  (:initable-instance-variables target-stream array-type array-size)
  (:default-init-plist :array-size 4096. :array-type 'art-8b))

(defmethod (embedded-length-output-mixin :before :init) (&rest ignore)
  (or target-stream
      (ferror nil "No target stream given."))
  (or (funcall target-stream ':get-handler-for ':string-out)
      (ferror "~S doesn't support :STRING-OUT" target-stream)))

(defmethod (embedded-length-output-mixin :after :init) (&rest ignore)
  (format target-stream "EMBEDDED-LENGTH-STREAM VERSION 1~%END~%")
  (setq el-index 0)
  (setq el-buf (allocate-resource 'embedded-length-buf array-type array-size)))

(defmethod (embedded-length-output-mixin :new-output-buffer) ()
  (if ( el-index array-size)
      (funcall-self ':send-real-output-buffer))
  (values el-buf el-index array-size))

(defmethod (embedded-length-output-mixin :discard-output-buffer) (arg-buf)
  (or (eq arg-buf el-buf)
      (ferror nil "I didn't give you this buffer ~S" arg-buf)))

(defmethod (embedded-length-output-mixin :send-output-buffer) (arg-buf arg-end)
  (or (eq arg-buf el-buf)
      (ferror nil "I didn't give you this buffer ~S" arg-buf))
  (setq el-index arg-end)) 

(defmethod (embedded-length-output-mixin :send-real-output-buffer) ()
  (cond ((plusp el-index)
	 (format target-stream "BLOK~12,48D" el-index)
	 (funcall target-stream ':string-out el-buf 0 el-index)))
  (setq el-index 0))

(defmethod (embedded-length-output-mixin :clear-error) (&rest ignore))

(defmethod (embedded-length-output-mixin :eof) ()
  (funcall-self ':force-output)
  (format target-stream "EOF "))

(defmethod (embedded-length-output-mixin :after :close) (&rest ignore)	;do it abort or not
  ;;buffered-output-stream has already forced output.
  (cond (el-buf
	 (funcall-self ':send-real-output-buffer)
	 (deallocate-resource 'embedded-length-buf el-buf)
	 (format target-stream "EOV ")
	 (funcall target-stream ':force-output)
	 (funcall target-stream ':close nil))))

(defmethod (embedded-length-output-mixin :after :force-output) ()
  (funcall-self ':send-real-output-buffer)
  (funcall target-stream ':force-output))



(defflavor embedded-length-input-mixin
	((target-stream nil)
	 (el-state ':closed)
	 (el-got-buffer)			;target's input buffer
	 (keybuf (make-array 12. ':type 'art-string))
	 el-limit				;where target buffer ends
	 el-count)				;how many left in block
	()
  (:initable-instance-variables target-stream)
  (:required-flavors buffered-input-stream))

(defmethod (embedded-length-input-mixin :before :init) (&rest ignore)
  (or target-stream
      (ferror nil "No target stream given."))
  (or (and (funcall target-stream ':get-handler-for ':string-in)
	   (funcall target-stream ':get-handler-for ':read-input-buffer)
	   (funcall target-stream ':get-handler-for ':advance-input-buffer))
      (ferror nil "~S doesn't support required input methods." target-stream))
  (setq el-state ':no-block-yet))

(defmethod (embedded-length-input-mixin :after :init) (&rest ignore)
  (return-array
    (let ((ary (make-array 100. ':type ':art-string ':leader-list '(0))))
      (embedded-length-input-mixin-funny-string-collect ary target-stream)
      (if (not (and (string-search "EMBEDDED-LENGTH-STREAM" ary)
		    (string-search "VERSION 1" ary)
		    (progn
		      (embedded-length-input-mixin-funny-string-collect ary target-stream)
		      (string-equal ary "END"))))
	  (ferror nil "This stream ~S does not contain embedded-length data."
		  target-stream))
      ary)))
	       
(defun embedded-length-input-mixin-funny-string-collect (s stream)
  (store-array-leader 0 s 0)
  (loop for i from 1 to 100.
	as c = (funcall stream ':tyi)
	if (or (null c) (= c #\CR))
	return nil
	finally (return nil)
	do (array-push s c)))

(defmethod (embedded-length-input-mixin :next-input-buffer) (&rest ignore)	;no-hang
  (loop with xmuch = 0
	and start = 0
	do
	(selectq el-state
	  (:eov    (ferror ':end-of-volume "End of volume encountered on ~S" self))
	  (:eof    (return nil))
	  (:no-block-yet
	   (funcall target-stream ':string-in "Premature EOF" keybuf 0 4)
	   (setq el-got-buffer nil)
	   (cond ((string-equal keybuf "EOF " 0 0 4 4)
		  (setq el-state ':eof))
		 ((string-equal keybuf "EOV " 0 0 4 4)
		  (setq el-state ':eov))
		 ((string-equal keybuf "BLOK" 0 0 4 4)
		  (funcall target-stream ':string-in "Premature EOF" keybuf 0 12.)
		  (or (setq el-count (parse-number keybuf))
		      (ferror nil "Invalid block length in stream: ~A" keybuf))
		  (setq el-state ':reading))
		 (t (ferror nil "Unknown keyword or tape out of sync: ~A"
			    (substring keybuf 0 4)))))
	  (:reading
	     (cond ((plusp el-count)
		    (or el-got-buffer
			(multiple-value (el-got-buffer start el-limit)
			  (funcall target-stream ':read-input-buffer "Premature EOF")))
		    
		    (setq xmuch (min el-count (- el-limit start)))
		    (decf el-count xmuch)
		    (setq el-limit (+ start xmuch))
		    (return el-got-buffer start el-limit))	;state stays reading
		   (t (if el-got-buffer (funcall-self ':discard-input-buffer el-got-buffer))
		      (setq el-state ':no-block-yet))))
	  (:closed (ferror nil "Stream closed - ~S" self))
	  (t (ferror nil "Invalid state: ~S" el-state)))))

(defmethod (embedded-length-input-mixin :discard-input-buffer) (arg-buf)
  (or (eq arg-buf el-got-buffer)
      (ferror nil "This isn't the buffer I gave you: ~S" arg-buf))
  (funcall target-stream ':advance-input-buffer el-limit)
  (setq el-got-buffer nil))

(defmethod (embedded-length-input-mixin :after :close) (&optional abort)
  (if el-got-buffer (funcall-self ':discard-input-buffer el-got-buffer))
  (or (eq el-state ':close) (funcall target-stream ':close abort))
  (setq el-state ':closed))

(defmethod (embedded-length-input-mixin :clear-eof) ()
  (if (eq el-state ':eof)
      (setq el-state ':no-block-yet)))



(defflavor embedded-length-8-bit-output-stream ()
	   (embedded-length-output-mixin buffered-output-stream)
  (:default-init-plist :array-type 'art-8b))

(defflavor embedded-length-8-bit-input-stream ()
	   (embedded-length-input-mixin buffered-input-stream))


(defflavor embedded-length-character-output-stream ()
	   (embedded-length-output-mixin buffered-output-character-stream)
  (:default-init-plist :array-type 'art-8b))

(defflavor embedded-length-character-input-stream ()
	   (embedded-length-input-mixin buffered-input-character-stream))
