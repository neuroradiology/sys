;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.3
;;; Reason: "Link transparencies" in FSEdit was busted.
;;; Written 12/15/81 14:04:29 by BSG,
;;; while running on Beagle from band 4
;;; with System 78.14, ZMail 38.1, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.2, Experimental Canon 9.0, microcode 840.



; From file FSEDIT.LISP DSK:<LMFS> SCRC:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFUN TREE-EDIT-DIRECTORY (TREE WINDOW)
  (LET* ((OBJECT (FUNCALL TREE ':OBJECT))
	 (PATHNAME (CAR OBJECT))
	 (DIRPATH (FUNCALL TREE ':DIR-IN-DIR-FORM))
	 (CHOICE (MENU-CHOOSE
		   `(,(IF (GET OBJECT ':DELETED)
			  '("Undelete" :VALUE :UNDELETE
			    :DOCUMENTATION "Undelete this directory.")
			  '("Delete" :VALUE :DELETE
			    :DOCUMENTATION "Mark this directory as deleted."))
		     ,@(IF (FUNCALL TREE ':INFERIORS-VISIBLE)
			   (LIST '("Close" :VALUE :CLOSE
				   :DOCUMENTATION
				   "Remove listing of inferiors from display.")
				 '("Decache" :VALUE :DECACHE
				 :DOCUMENTATION
				 "Recompute display of this directory from latest data"))
			   (LIST '("Open" :VALUE :OPEN
				   :DOCUMENTATION "List inferiors to this display.")
				 '("Selective open" :VALUE :SEL-OPEN
				   :DOCUMENTATION
				   "Open to selected files in this directory.")))
		     ("Expunge" :VALUE :EXPUNGE :DOCUMENTATION
		      "Remove all deleted files in this directory")
		     ("Create Inferior Directory" :VALUE :CRDIR :DOCUMENTATION
		      "Create a new directory inferior to this directory")
		     ("View Properties" :VALUE :VIEW-PROPERTIES
		      :DOCUMENTATION "View all available information about this directory.")
		     ("Edit Properties" :VALUE :EDIT-PROPERTIES
		      :DOCUMENTATION "Edit properties of directory")
		     ("New Property" :VALUE :PUTPROP
		      :DOCUMENTATION
		      "Add or remove a user-defined file property from this directory")
		     ("Create link" :VALUE :LINK
		      :DOCUMENTATION "Create a file system link.")
		     ("Rename" :VALUE :RENAME :DOCUMENTATION "Rename this directory.")
		     ("Link Transparencies" :VALUE :LINK-XPAR
		      :DOCUMENTATION "Edit default link transparency attributes.")
		     ("Dump" :VALUE :DUMP :DOCUMENTATION
		      "Invoke the backup dumper on this directory and all its inferiors."))
		   (STRING-APPEND "Directory operations: " (FUNCALL DIRPATH
								    ':STRING-FOR-DIRECTORY))
		   '(:MOUSE) NIL WINDOW)))
    (SELECTQ CHOICE
      (:LINK-XPAR
            (COND ((MEMQ ':DIRECTORY-PATHNAME-AS-FILE (FUNCALL DIRPATH ':WHICH-OPERATIONS))
		   (LET ((INFPATH (FUNCALL DIRPATH ':DIRECTORY-PATHNAME-AS-FILE)))
		     (LET ((CHANGE-RESULT
			     (TREE-EDIT-TRANSPARENCIES
			       (FORMAT NIL "Default link transparencies for ~A"
				       (FUNCALL DIRPATH ':STRING-FOR-DIRECTORY))
			       (TREE-EDIT-ATTRIBUTE-UPDATE
				 (NCONS INFPATH) ':DEFAULT-LINK-TRANSPARENCIES))))
		       (IF CHANGE-RESULT
			   (FS:CHANGE-FILE-PROPERTIES INFPATH T ':DEFAULT-LINK-TRANSPARENCIES
						      CHANGE-RESULT)))))
		  (T (FERROR NIL "Can't operate on directories in this system - ~A"
			     PATHNAME))))
      (:LINK     (LET ((FILEPATH
			 (TREE-EDIT-READ-LOCAL-PATH DIRPATH
						    "File name of the link itself? ")))
		   (COND ((NULL FILEPATH))	;punt
			 ((FUNCALL FILEPATH ':DIRECTORY)
			  (FORMAT T "You may not specify a directory here."))
			 (T
			  (LET ((TARGET
				  (TREE-EDIT-READ-LOCAL-PATH
				    FILEPATH "Path to link to? (target) ")))
			    (IF TARGET
				(LET ((RESULT
					(FUNCALL
					  (FUNCALL FILEPATH ':NEW-DIRECTORY
						   (FUNCALL DIRPATH ':DIRECTORY))
					  ':CREATE-LINK TARGET)))
				  (IF (EQ RESULT T)
				      (FUNCALL TREE ':DECACHE-INFERIORS)
				      (FORMAT T "~&~A" RESULT)))))))
		   (TREE-EDIT-END-TYPEOUT)))
      (:EXPUNGE
		(MULTIPLE-VALUE-BIND
		  (RECORDS ERRORS)
		    (FUNCALL (FUNCALL TREE ':MATCH-PATHNAME) ':EXPUNGE)
		  (FORMAT T "~&~D record~:P reclaimed." RECORDS)
		  (IF (AND ERRORS (LISTP ERRORS))
		      (PROGN
			(FORMAT T "~&There were errors encountered:")
			(MAPC 'PRINT ERRORS))
		      (FORMAT T "~&There were no errors encountered.")))
		(FUNCALL TREE ':DECACHE-INFERIORS)
		(TREE-EDIT-END-TYPEOUT))
      (:CRDIR (IF (EQ (TREE-EDIT-CREATE-DIR DIRPATH) T)
		  (FUNCALL TREE ':DECACHE-INFERIORS))
	      (TREE-EDIT-END-TYPEOUT))
      (:DECACHE  (FUNCALL TREE ':DECACHE-INFERIORS))
      (:OPEN (FUNCALL TREE ':DEFAULT-MATCH-PATHNAME)
	     (FUNCALL TREE ':SET-INFERIORS-VISIBLE T))
      (:SEL-OPEN
             (DO () (())
	       (LET ((STARPATH (TREE-EDIT-READ-LOCAL-PATH DIRPATH
				 "File name to match as starname:")))
		 (IF STARPATH
		     (IF (FUNCALL STARPATH ':DIRECTORY)
			 (TV:NOTIFY NIL "Don't specify a directory, please")
			 (PROGN
			   (FUNCALL TREE
				    ':SET-MATCH-PATHNAME
				    (FUNCALL STARPATH ':NEW-PATHNAME
					     ':DIRECTORY (FUNCALL DIRPATH ':DIRECTORY)
					     ':DEVICE (FUNCALL DIRPATH ':DEVICE)))
			   (FUNCALL TREE ':SET-INFERIORS-VISIBLE T)
			   (RETURN)))))))
      (:CLOSE (FUNCALL TREE ':SET-INFERIORS-VISIBLE NIL))
      (:DUMP  (LMFS:BACKUP-DUMPER ':DUMP-TYPE ':COMPLETE
				  ':START-PATH (WILDIFY-PATHNAME DIRPATH))
	      (TREE-EDIT-END-TYPEOUT))
      (T      (COND ((MEMQ ':DIRECTORY-PATHNAME-AS-FILE (FUNCALL DIRPATH ':WHICH-OPERATIONS))
		     (TREE-EDIT-COMMON CHOICE OBJECT
				       (FUNCALL DIRPATH ':DIRECTORY-PATHNAME-AS-FILE) TREE))
		    (T
		     (FORMAT T "~&Directory attribute operations are not supported on this file system.")
		     (TREE-EDIT-END-TYPEOUT))))))) 
(COMPILE-FLAVOR-METHODS FILE-TREE DIRECTORY-TREE TREE-LIST-TOPNODE TREE-LIST-ROOT-TOPNODE)
)
