;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.46
;;; Reason: VMS Patch pathnames
;;; Written 1/09/82 19:38:07 by RWK,
;;; while running on Retriever from band 1
;;; with System 78.45, ZMail 38.5, Symbolics 8.12, Tape 6.5, LMFS 21.33, Canon 9.11, microcode 841.



; From file PATHNM.LISP >LMIO POINTER:
#8R FILE-SYSTEM:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))

  
;;; Patch system interface, more kludges for only 9 character VMS filenames
(DEFMETHOD (VMS-PATHNAME-MIXIN :PATCH-FILE-PATHNAME) (NAM SAME-DIRECTORY-P PATOM TYP
						      &REST ARGS)
  (SELECTQ TYP
    (:SYSTEM-DIRECTORY
     (FUNCALL-SELF ':NEW-PATHNAME ':NAME (IF SAME-DIRECTORY-P PATOM NAM)
		   ':TYPE "(PDIR)" ':VERSION ':NEWEST))
    (:VERSION-DIRECTORY
     (FUNCALL-SELF ':NEW-PATHNAME ':NAME (WITH-OUTPUT-TO-STRING (STREAM)
					   (LET ((SNAME (IF SAME-DIRECTORY-P PATOM
							  (SI:SYSTEM-SHORT-NAME NAM))))
					     (DOTIMES (I (MIN (STRING-LENGTH SNAME) 6))
					       (FUNCALL STREAM ':TYO (AREF SNAME I))))
					   (LET ((BASE 10.) (*NOPOINT T))
					     (PRIN1 (\ (CAR ARGS) 1000.) STREAM)))
		   ':TYPE "(PDIR)" ':VERSION ':NEWEST))
    (:PATCH-FILE
     (FUNCALL-SELF ':NEW-PATHNAME ':NAME (FORMAT NIL "~:[~*~;~C~]~DX~D"
						 SAME-DIRECTORY-P PATOM
						 (\ (CAR ARGS) 100.)
						 (\ (CADR ARGS)
						    (IF SAME-DIRECTORY-P 100. 1000.)))
		   ':TYPE (CADDR ARGS) ':VERSION ':NEWEST))))

)

; From file PATHNM.LISP >LMIO POINTER:
#8R FILE-SYSTEM:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))

(DEFVAR *VMS-FILE-TYPE-ALIST*
	'(("LISP" . "LSP")
	  ("TEXT" . "TXT")
	  ("MIDAS" . "MID")
	  ("QFASL" . "QFS")
	  ("PRESS" . "PRS")
	  ("(PDIR)" . "PDR")))

)

; From file PATHNM.LISP >LMIO POINTER:
#8R FILE-SYSTEM:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))

(DEFMETHOD (VMS-PATHNAME-MIXIN :PRIMARY-DEVICE) () "SYS$SYSDISK")

)
