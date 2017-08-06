;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.22
;;; Reason: Bumb priority of control connection process a little
;;; Written 12/25/81 22:26:00 by HIC,
;;; while running on Basset from band 2
;;; with System 78.38, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.21, Canon 9.7, microcode 841.


; From file server.lisp >lmfs POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

;; This gets slightly higher priority because the control connection doesn't have much
;; to do, but when asked it should perform promptly.
(add-initialization "FILE" '(PROCESS-RUN-FUNCTION '(:NAME "File Server" :priority 5)
						  'file-server)
		    nil 'chaos:server-alist)

)
