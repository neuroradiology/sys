;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.1
;;; Reason: Fsedit lossages -BSG.
;;; Written 12/14/81 12:58:49 by BEE,
;;; while running on Beagle from band 4
;;; with System 78.14, ZMail 38.1, Experimental Symbolics 8.3, Experimental Tape 6.0, Experimental LMFS 21.0, Experimental Canon 9.0, microcode 840.



; From file FSMAINT.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (fsmaint-hiered-pane :demand-tree-edit) (path)
  (setq path (fsmaint-dir-verify path))
  (and path
       (progn
	 (funcall-self ':set-tree (make-instance 'tv:tree-list-topnode
						 ':dir-in-dir-form path))
	 t)))					;indicate success

)

; From file FSEDIT.LISP DSK:<LMFS> SCRC:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFUN TREE-EDIT-CREATE-DIR (PARCOND)		;in typeout window now
  (IF (AND (MEMQ ':DIRECTORY-PATHNAME-AS-FILE (FUNCALL PARCOND ':WHICH-OPERATIONS))
	   (GET (FUNCALL (FUNCALL PARCOND ':DIRECTORY-PATHNAME-AS-FILE) ':PROPERTIES T)
		':DELETED))
      (FORMAT T "~&~A has been deleted" (FUNCALL PARCOND ':STRING-FOR-DIRECTORY))
      (LET ((PARSED (TREE-EDIT-READ-LOCAL-PATH
		      PARCOND
		      "~&Please type file name for new directory, a son of ~A:~%"
		      (FUNCALL PARCOND ':STRING-FOR-DIRECTORY))))
	(COND ((NULL PARSED)
	       (FORMAT T "~&Invalid file name."))
	      ((OR (FUNCALL PARSED ':DIRECTORY)
		   (FUNCALL PARSED ':TYPE)
		   (FUNCALL PARSED ':VERSION))
	       (FORMAT T "~&A file name only, please."))
	      (T
	       (LET ((RESULT (OPEN (FUNCALL PARCOND ':NEW-NAME (FUNCALL PARSED ':NAME))
				   ':FLAVOR ':DIRECTORY)))
		 (OR (EQ RESULT T)
		     (FORMAT T "~&~A" RESULT))))))))

)

; From file FSEDIT.LISP DSK:<LMFS> SCRC:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFMETHOD (TREE-LIST-ROOT-TOPNODE :DEFAULT-MATCH-PATHNAME) (&REST IGNORE)
  (SETQ MATCH-PATHNAME
	(FUNCALL SAMPLE-PATH ':NEW-PATHNAME ':DIRECTORY ':ROOT
		 ':NAME ':WILD ':TYPE ':WILD ':VERSION ':WILD))
  (IF ROOT-MEANINGFUL-P
      (SETQ OPEN-PRINREP (FUNCALL MATCH-PATHNAME ':STRING-FOR-PRINTING))))

)

; From file FSEDIT.LISP DSK:<LMFS> SCRC:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFMETHOD (TREE-LIST-TOPNODE :AFTER :DEFAULT-MATCH-PATHNAME) (&REST IGNORE)
  (SETQ OBJECT (LIST MATCH-PATHNAME ':DIRECTORY ':SORT-OF)))

)
