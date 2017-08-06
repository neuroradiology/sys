;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.4
;;; Reason: Assume current userid, no password, when chaosfiling to Lispm host.
;;; Written 12/16/81 09:55:23 by BSG,
;;; while running on Basset from band 1
;;; with System 78.16, ZMail 38.2, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.3, Canon 9.0, microcode 840.



; From file FS-USER.LISP DSK:<LMFS> SCRC:
#8R FILE-SYSTEM:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))

;;;Automatic user identity establishing for lispm hosts.
(defmethod (lispm-chaos-host :before :login-unit) (ignore login-p)
  (cond (login-p
	 (force-user-to-login)
	 (file-host-user-id user-id self))))
(compile-flavor-methods lispm-chaos-host)
)
