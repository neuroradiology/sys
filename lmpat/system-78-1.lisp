;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.1
;;; Reason: Fix band transfer server
;;; Written 12/07/81 21:39:32 by MMcM,
;;; while running on Lisp Machine Seven from band 7
;;; with Experimental System 78.0, Experimental ZMail 38.0, microcode 836.



(GLOBALIZE 'ERRORP)
