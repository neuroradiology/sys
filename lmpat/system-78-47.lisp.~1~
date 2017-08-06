;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.47
;;; Reason: Fix minor bug in VMS-DEVNAME-CHECK
;;; Written 1/10/82 22:38:42 by HIC,
;;; while running on Spaniel from band 1
;;; with System 78.44, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.29, Canon 9.11, microcode 841.



; From file PATHNM.LISP >LMIO POINTER:
#8R FILE-SYSTEM:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))

(DEFUN VMS-DEVNAME-CHECK (DEV)
  (IF (STRING-SEARCH-NOT-SET VMS-DEVNAME-CHARSET DEV)
      (FERROR ':PATHNAME-PARSE-ERROR "/"~A/" -- illegal character in VMS device name" DEV))
  (IF (> (STRING-LENGTH DEV) 63.)
      (FERROR ':PATHNAME-PARSE-ERROR "/"~A/" -- VMS device name too long" DEV)))

)
