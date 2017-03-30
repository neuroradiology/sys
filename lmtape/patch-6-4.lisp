;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Tape version 6.4
;;; Reason: More 6.3
;;; Written 12/23/81 15:19:53 by BSG,
;;; while running on Spaniel from band 1
;;; with System 78.28, ZMail 38.5, Symbolics 8.7, Tape 6.3, LMFS 21.18, Canon 9.5, microcode 841.



; From file tapestr.lisp >lmtape POINTER:
#8R TAPE:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TAPE")))

(defflavor tape-embedded-length-output-mixin () ()
  (:required-flavors tape-output-stream-mixin tape-stream-mixin))

(defflavor tape-embedded-length-input-mixin () ()
  (:required-flavors tape-input-stream-mixin tape-stream-mixin))


(defflavor tape-embedded-length-output-stream ()
	   (tape-embedded-length-output-mixin tape-output-stream))

(defflavor tape-embedded-length-input-stream ()
	   (tape-embedded-length-input-mixin tape-input-stream))


(defmethod (tape-embedded-length-output-mixin :before :new-output-buffer) ()
  (if (zerop (tpb-bufx stream-buf))
      (setf (tpb-bufx stream-buf) 4)))

(defmethod (tape-embedded-length-output-mixin :before :send-output-buffer) (buf endindex)
  (or ( endindex 4) (ferror nil "Not even four characters to be written."))
  (loop for i from 3 downto 0
	do
	(aset (+ #/0 (\ endindex 10.)) buf i)
	(setq endindex (// endindex 10.))))


(defmethod (tape-embedded-length-input-mixin :after :validate-input-buffer) ()
  (cond ((and (eq state ':input)
	      (zerop (tpb-bufx stream-buf)))
	 (setf (tpb-bufx stream-buf) 4)		;that's where guy starts
	 (setf (tpb-count stream-buf)
	       (loop for i from 0 to 3
		     with sum = 0
		     as digit = (aref (tpb-8ary stream-buf) i)
		     finally (return sum)
		     do
		     (or (and ( digit #/0) ( digit #/9))
			 (ferror nil "Bad digit in length field: ~C" digit))
		     (setq sum (+ (* 10. sum) (- digit #/0))))))))


(compile-flavor-methods

  tape-input-stream tape-output-stream
  tape-embedded-length-input-stream tape-embedded-length-output-stream)

)

