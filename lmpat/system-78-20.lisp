;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.20
;;; Reason: New site setting scheme
;;; Written 12/17/81 23:56:21 by MMcM,
;;; while running on Lisp Machine One from band 3
;;; with System 78.19, ZMail 38.3, microcode 836, 60Hz.


; From file LTOP > LISPM; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(LET ((ELEM (ASSOC "HOST-TABLE-INITIALIZATION" SITE-INITIALIZATION-LIST)))
  (SETQ SITE-INITIALIZATION-LIST (CONS ELEM (DELQ ELEM SITE-INITIALIZATION-LIST))))

;; Now fix up the site initialization list.  The host table loading initialization
;; is the first one, from QMISC.  Next comes the host reseting one from HOST.  Then
;; the SYS: initialization from PATHNM.  When loading the system now, the host
;; table is loaded via MINI with explicit host pathnames, and so must come before
;; SYS:.  When changing the site, the host table is loaded from SYS:, so that must
;; be setup before.  Therefore, move the host table initialization (the CAR) to
;; just after the SYS: one.
(LET ((POS (LET ((ELEM (ASSOC "DEFINE-SYS-LOGICAL-DEVICE"
			      SITE-INITIALIZATION-LIST)))
	     (MEMQ ELEM SITE-INITIALIZATION-LIST))))
  ;; If there is a chaosnet, move the setting up of hosts to after loading the
  ;; host table.
  (LET ((ELEM (ASSOC "SITE-CHAOS-PATHNAME-INITIALIZE" SITE-INITIALIZATION-LIST)))
    (COND (ELEM
	   (SETQ SITE-INITIALIZATION-LIST (DELQ ELEM SITE-INITIALIZATION-LIST))
	   (PUSH ELEM (CDR POS)))))
  (PUSH (POP SITE-INITIALIZATION-LIST) (CDR POS)))

)
