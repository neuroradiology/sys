;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.40
;;; Reason: Make system processes have meaningful priorities
;;; Written 12/25/81 20:12:24 by HIC,
;;; while running on Basset from band 2
;;; with System 78.38, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.21, Canon 9.7, microcode 841.


(FUNCALL TV:MOUSE-PROCESS ':SET-PRIORITY 30.)
(FUNCALL TV:KBD-PROCESS ':SET-PRIORITY 30.)
(FUNCALL CHAOS:RECEIVER ':SET-PRIORITY 35.)
(FUNCALL CHAOS:BACKGROUND ':SET-PRIORITY 25.)
