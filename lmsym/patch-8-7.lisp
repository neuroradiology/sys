;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.7
;;; Reason: Convert the SYS host to File computer
;;; Written 12/22/81 11:34:40 by BEE,
;;; while running on Retriever from band 6
;;; with System 78.24, ZMail 38.4, Symbolics 8.6, Tape 6.2, LMFS 21.15, Canon 9.3, microcode 841.

; From file fixsys.LISP >bee POINTER:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(comment This is the file read in to convert to Pointer system host

(DEFCONST LISPM-LOGICAL-PATHNAME-TRANSLATIONS
  '(("CANON" ">CANON>")
    ("CC" ">LMCONS>")
    ("CHAOS" ">CHAOS>")
    ("DEMO" ">LMDEMO>")
    ("DISTRIBUTION" ">DISTRIBUTION>")
    ("FONTS" ">LMFONTS>")
    ("IO" ">LMIO>")
    ("IO1" ">LMIO1>")
    ("LMFS" ">LMFS>")
    ("LMSYM" ">LMSYM>")
    ("LMTAPE" ">LMTAPE>")
    ("PATCH" ">LMPAT>")
    ("PRESS-FONTS" ">FONTS>")
    ("SYS" ">LISPM>")
    ("SYS2" ">LISPM2>")
    ("UBIN" ">LISPM1>")
    ("UCADR" ">LCADR>")
    ("WINDOW" ">LMWIN>")
    ("ZMAIL" ">ZMAIL>")
    ("ZWEI" ">ZWEI>")
    ))

(DEFSITE :SYM
  ;; For PRINT-HERALD
  (:SITE-PRETTY-NAME "Symbolics")
  ;; Pointer is the file computer
  (:SYS-HOST "POINTER")
  ;; SYS: translations to use
  (:SYS-DIRECTORY-TRANSLATIONS LISPM-LOGICAL-PATHNAME-TRANSLATIONS)
  ;; Has a local chaosnet
  (:CHAOS T)
  ;; File computers using the chaosnet file server protocol
  (:CHAOS-FILE-SERVER-HOSTS '("SCRC"))
  ;; Hosts suspected of supporting time servers
  (:CHAOS-TIME-SERVER-HOSTS '("SCRC"))
  ;; Hosts that know about hosts off the chaosnet
  (:CHAOS-HOST-TABLE-SERVER-HOSTS '("SCRC"))
  ;; Hosts that have mail servers capable of forwarding mail anyplace
  (:CHAOS-MAIL-SERVER-HOSTS '("SCRC"))
  ;; EST
  (:TIMEZONE 5)
  ;; Destination for mail to BUG-FOOBAR
  (:HOST-FOR-BUG-REPORTS "SCRC-TENEX")
  ;; "Local sites", used by ZMail summary display
  (:LOCAL-MAIL-HOSTS '("SCRC-TENEX"))
  ;; Has a Canon laser printer
  (:LBP T)
  ;; That is how you print things
  (:DEFAULT-HARDCOPY-MODE ':LBP)
  ;; Also for ESC Q
  (:HARDCOPY-SCREEN-MODE ':LBP)
  ;; Arg to ESC F
  (:ESC-F-ARG-ALIST '((NIL . :LOGIN) (1 . :LISP-MACHINES) (0 . :READ)))
  )

(FS:ADD-LOGICAL-PATHNAME-HOST "SYS" "Pointer" LISPM-LOGICAL-PATHNAME-TRANSLATIONS)

(SET-SYS-HOST "Pointer")
))

