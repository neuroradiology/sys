;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.11
;;; Reason: Bogus output buffer forcing in embedded-length-stream.
;;; Written 1/08/82 12:06:28 by BSG,
;;; while running on Basset from band 5
;;; with System 78.45, ZMail 38.5, Symbolics 8.10, Tape 6.5, LMFS 21.33, Canon 9.11, microcode 841.



; From file embedded-length-stream.lisp >distribution POINTER:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(defmethod (embedded-length-output-mixin :after :force-output) ()
  (funcall-self ':send-real-output-buffer))	;Don't force output- could end a tape record

)

; From file embedded-length-stream.lisp >distribution POINTER:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(compile-flavor-methods
  embedded-length-8-bit-output-stream
  embedded-length-8-bit-input-stream
  embedded-length-character-output-stream
  embedded-length-character-input-stream)
)

; From file distribution-dump.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

(defun distribution-tape-stream (&key &optional
				 (tape-p nil)
				 (reel nil)
				 (direction ':input)
				 (host nil)
				 (unit 0) &aux mode)
  (setq mode (selectq direction (:output ':write) (t ':read)))
  (make-instance
    (selectq mode
      (:read  'si:embedded-length-character-input-stream)
      (:write 'si:embedded-length-character-output-stream))
    ':target-stream
    (cond ((eq tape-p ':connect)
	   (if (null host) (ferror nil "Connect specified, but no host."))
	   (if (null reel) (ferror nil "Connect specified, but no reel name."))
	   (let ((conn (chaos:connect host (format nil "S-TAPE ~A ~A ~D" mode reel unit))))
	     (if (stringp conn)
		 (ferror nil "Can't connect to S-TAPE at ~A, ~A" host conn)
	       (chaos:make-stream conn ':direction direction))))
	  (tape-p
	   (tape:open-tape unit ':mode mode ':record-length 4096.))
	  (t (let ((conn (chaos:listen "TAPE")))
	       (if (stringp conn)
		   (ferror nil "Can't connect to TAPE: ~A" conn))
	       (chaos:accept conn)
	       (chaos:make-stream conn ':direction direction))))))

)

