;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Tape version 6.5
;;; Reason: Yet more 6.3.
;;; Written 12/24/81 11:22:02 by BSG,
;;; while running on Beagle from band 3
;;; with System 78.32, ZMail 38.5, Symbolics 8.7, Tape 6.4, LMFS 21.18, Canon 9.5, microcode 840.



; From file tapestr.lisp >lmtape POINTER:
#8R TAPE:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TAPE")))

(defun open-tape (&optional (unit 0) &rest rest)
  (lexpr-funcall #'make-instance
		 (if (eq (get (locf rest) ':mode) ':write)
		     'tape-output-stream
		     'tape-input-stream) 
		 ':unit unit
		 rest))

)

; From file tapestr.lisp >lmtape POINTER:
#8R TAPE:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TAPE")))

;;;
;;; Embedded-length stuff
;;; 12/23/81
;;;

(defflavor tape-embedded-length-output-mixin () ()
  (:required-flavors tape-output-stream-mixin tape-stream-mixin)
  (:default-init-plist :mode ':write))

)

; From file tapestr.lisp >lmtape POINTER:
#8R TAPE:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TAPE")))

(defflavor tape-embedded-length-input-mixin () ()
  (:required-flavors tape-input-stream-mixin tape-stream-mixin)
  (:default-init-plist :mode ':read))

)

; From file tapestr.lisp >lmtape POINTER:
#8R TAPE:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TAPE")))

(defun open-embedded-length-tape (&optional (unit 0) &rest rest)
  (lexpr-funcall #'make-instance
		 (if (eq (get (locf rest) ':mode) ':write)
		     'tape-embedded-length-output-stream
		     'tape-embedded-length-input-stream) 
		 ':unit unit
		 rest))

)

