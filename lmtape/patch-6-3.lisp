;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Tape version 6.3
;;; Reason: Embedded-length streams for Distribution stuff.
;;; Written 12/23/81 14:14:59 by BSG,
;;; while running on Spaniel from band 1
;;; with System 78.27, ZMail 38.4, Symbolics 8.7, Tape 6.2, LMFS 21.17, Canon 9.3, microcode 841.



; From file tapestr.lisp >lmtape POINTER:
#8R TAPE:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TAPE")))

;;;Input side

(defmethod (tape-input-stream-mixin :next-input-buffer)	(&rest ignore)
  (funcall-self ':validate-input-buffer)
  (cond ((eq state ':input-eof)
	 (funcall-self ':discard-current-input-buffer-i)
	 nil)
	(t (values (tpb-8ary stream-buf) (tpb-bufx stream-buf) (tpb-count stream-buf)))))

)

; From file tapestr.lisp >lmtape POINTER:
#8R TAPE:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TAPE")))

(defmethod (tape-output-stream-mixin :new-output-buffer) ()
  (or (zerop (tpb-count stream-buf))
      (ferror nil
	      "No right to ask for new output buffer, neither sent nor discarded old one"))
  (values (tpb-8ary stream-buf) (tpb-bufx stream-buf) record-length))

)

; From file tapestr.lisp >lmtape POINTER:
#8R TAPE:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TAPE")))

;;;
;;; Embedded-length stuff
;;; 12/23/81
;;;

(defflavor tape-embedded-length-output-mixin () ()
  (:required-flavors tape-output-stream-mixin tape-stream-mixin))

)

