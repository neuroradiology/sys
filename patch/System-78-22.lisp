;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.22
;;; Reason: Fix READ-SYNTAX-PLIST's lossage with :LINE-IN on binary files.
;;; Written 12/19/81 17:05:45 by DLA,
;;; while running on Lisp Machine Six from band 7
;;; with System 78.21, ZMail 38.4, microcode 836.



; From file PATHNM > LMIO; AI:
#8R FILE-SYSTEM:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FILE-SYSTEM")))

(DEFUN FILE-READ-PROPERTY-LIST (PATHNAME STREAM &AUX WO PLIST PATH MODE)
  (SETQ WO (FUNCALL STREAM ':WHICH-OPERATIONS))
  (COND ((MEMQ ':SYNTAX-PLIST WO)
	 (SETQ PLIST (FUNCALL STREAM ':SYNTAX-PLIST)))
	;; This way of reading the property list is sort of a kludge, but it has
	;; the very important advantage that it won't ever read the whole file like :LINE-IN
	;; would be prone to do if the file had no carriage returns in it.
	((MEMQ ':READ-INPUT-BUFFER WO)
	 (MULTIPLE-VALUE-BIND (BUFFER START END)
	     (FUNCALL STREAM ':READ-INPUT-BUFFER)
	   (AND BUFFER (SETQ PLIST (FILE-PARSE-PROPERTY-LIST BUFFER START END)))))
	(T (DO ((LINE) (EOF)) (NIL)
	     (MULTIPLE-VALUE (LINE EOF) (FUNCALL STREAM ':LINE-IN NIL))
	     (COND ((STRING-SEARCH "-*-" LINE)
		    (SETQ PLIST (FILE-PARSE-PROPERTY-LIST LINE))
		    (FUNCALL STREAM ':SET-POINTER 0)
		    (RETURN NIL))
		   ((OR EOF (STRING-SEARCH-NOT-SET '(#\SP #\TAB) LINE))
		    (FUNCALL STREAM ':SET-POINTER 0)
		    (RETURN NIL))))))
  (AND (NOT (GET (LOCF PLIST) ':MODE))
       (MEMQ ':PATHNAME WO)
       (SETQ PATH (FUNCALL STREAM ':PATHNAME))
       (SETQ MODE (CDR (ASSOC (FUNCALL PATH ':TYPE) *FILE-TYPE-MODE-ALIST*)))
       (PUTPROP (LOCF PLIST) MODE ':MODE))
  (AND PATHNAME
       (DO ((L PLIST (CDDR L)))
	   ((NULL L))
	 (FUNCALL PATHNAME ':PUTPROP (SECOND L) (FIRST L))))
  PLIST)

)

