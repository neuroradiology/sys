;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.5
;;; Reason: Distribution changes for 78.20
;;; Written 12/18/81 01:48:57 by MMcM,
;;; while running on Beagle from band 4
;;; with System 78.19, ZMail 38.3, Experimental Symbolics 8.4, Experimental Tape 6.1, Experimental LMFS 21.10, Canon 9.0, microcode 840.



; From file DIST.LISP DSK:<DISTRIBUTION> SCRC:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;;;-*- Mode:LISP; Package:SYSTEM-INTERNALS -*-
;;; (c) Copyright 1981, Symbolics, Inc.
;;; Distribution system

;;; Procedure for making a distribution band:
;;; Copy all sources onto the local file system.
;;; Boot the regular SCRC system, and do (PRESET-FOR-DISTRIBUTION), which
;;; puts the distribution files on local for site setup.
;;; Boot a system without the Canon, and do (SET-FOR-DISTRIBUTION).  This
;;; can then be saved as the distribution band.

;;; This is called to setup the local file system for making the distribution band
(DEFUN PRESET-FOR-DISTRIBUTION ()
  (DECLARE (SPECIAL LISPM-LOGICAL-PATHNAME-TRANSLATIONS))
  (WITH-OPEN-FILE (FILE "LOCAL:>DISTRIBUTION>DISTRIBUTION>HOSTS.TEXT" '(:PRINT))
    (FUNCALL FILE ':LINE-OUT "; Host table for Distribution systems")
    (FORMAT FILE "~%HOST DISTRIBUTION-LM1,	CHAOS ~O,USER,LISPM,LISPM,[LM1]~%"
	    CHAOS:MY-ADDRESS))
  (COPY-SITE-FILE "LOCAL:>DISTRIBUTION>DISTRIBUTION>SITE.LISP"
		  "LOCAL:>LMSITE>SITE.LISP")
  (COPY-SITE-FILE "LOCAL:>DISTRIBUTION>DISTRIBUTION>HOSTS.TEXT"
		  "LOCAL:>CHAOS>HOSTS.TEXT")
  (COPY-SITE-FILE "LOCAL:>DISTRIBUTION>DISTRIBUTION>LMLOCS.LISP"
		  "LOCAL:>LMSITE>LMLOCS.LISP")
  (READFILE "LOCAL:>LMSITE>SITE.LISP")
  (FS:ADD-LOGICAL-PATHNAME-HOST "SYS" "LOCAL" LISPM-LOGICAL-PATHNAME-TRANSLATIONS)
  (RECOMPILE-SITE-FILES)
  )

)

; From file DIST.LISP DSK:<DISTRIBUTION> SCRC:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;;; This is called in a cold booted SCRC system to setup for the distribution band
(DEFUN SET-FOR-DISTRIBUTION ()
  (SET-SYS-HOST "DISTRIBUTION-LM1" ':LISPM CHAOS:MY-ADDRESS ">LMSITE>")
  (UPDATE-SITE-CONFIGURATION-INFO)
  (SETQ HOST-ALIST NIL)
  (SETQ MACHINE-LOCATION-ALIST (CDR MACHINE-LOCATION-ALIST))
  (SETQ FS:*PATHNAME-HOST-LIST* (DELQ SI:LOCAL-HOST FS:*PATHNAME-HOST-LIST*))
  (SETQ SI:ASSOCIATED-MACHINE "LOCAL")
  (ADD-INITIALIZATION "DISTRIBUTION-SYS"
		      '(FS:CHANGE-LOGICAL-PATHNAME-HOST "SYS" "LOCAL")
		      '(COLD)))

)

; From file DIST.LISP DSK:<DISTRIBUTION> SCRC:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFSYSTEM SITE-INTERNAL
  (:PACKAGE SI)
  (:MODULE HOST-TABLE (("SYS: CHAOS; HOSTS" "SYS: SITE; HSTTBL")) :PACKAGE CHAOS)
  (:COMPILE-LOAD (:GENERATE-HOST-TABLE HOST-TABLE))
  (:COMPILE-LOAD ("SYS: SITE; SITE"))
  (:COMPILE-LOAD ("SYS: SITE; LMLOCS"))
  )

)

