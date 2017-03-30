;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Tape version 6.1
;;; Reason: Offline ops weren't clearing out stream mixins' buffers.
;;; Written 12/15/81 12:17:34 by BSG,
;;; while running on Pointer from band 1
;;; with System 78.14, ZMail 38.1, Experimental Symbolics 8.3, Experimental Tape 6.0, Experimental LMFS 21.1, Experimental Canon 9.0, microcode 840.



; From file TAPESTR.LISP DSK:<LMTAPE> SCRC:
#8R TAPE:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TAPE")))

(defmethod (tape-stream-mixin :offline-op) (op)
  (funcall-self ':await-dma-buf)
  (cond ((eq mode ':read)
	 (funcall-self ':discard-current-input-buffer)
	 (funcall-self ':discard-current-input-buffer-i)))
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

)

