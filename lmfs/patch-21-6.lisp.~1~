;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.6
;;; Reason: FSEdit menus will remember last selection.
;;; Written 12/16/81 13:03:12 by BSG,
;;; while running on Pointer from band 1
;;; with System 78.16, ZMail 38.2, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.5, Canon 9.0, microcode 840.



; From file FSEDIT.LISP DSK:<LMFS> SCRC:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))


(DEFVAR *TREE-EDIT-DIRECTORY-ITEM* NIL)

(DEFUN TREE-EDIT-DIRECTORY (TREE WINDOW)
  (LET* ((OBJECT (FUNCALL TREE ':OBJECT))
	 (PATHNAME (CAR OBJECT))
	 (DIRPATH (FUNCALL TREE ':DIR-IN-DIR-FORM)))
    (MULTIPLE-VALUE-BIND (CHOICE ITEM)
	        (MENU-CHOOSE
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
		   '(:MOUSE) *TREE-EDIT-DIRECTORY-ITEM* WINDOW)
      (IF CHOICE (SETQ *TREE-EDIT-DIRECTORY-ITEM* ITEM))
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
	(:EXPUNGE  (MULTIPLE-VALUE-BIND
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
	(T      (COND ((MEMQ ':DIRECTORY-PATHNAME-AS-FILE
			     (FUNCALL DIRPATH ':WHICH-OPERATIONS))
		       (TREE-EDIT-COMMON CHOICE OBJECT
					 (FUNCALL DIRPATH ':DIRECTORY-PATHNAME-AS-FILE) TREE))
		      (T
		       (FORMAT T
			       "~&Directory attribute operations are not~
			 supported on this file system.")     
		       (TREE-EDIT-END-TYPEOUT))))))))

(DEFVAR *TREE-EDIT-FILE-ITEM* NIL)

(DEFUN TREE-EDIT-FILE (TREE WINDOW)
  (LET* ((OBJECT (FUNCALL TREE ':OBJECT))
	 (PATHNAME (CAR OBJECT)))
    (MULTIPLE-VALUE-BIND (CHOICE ITEM)
	(MENU-CHOOSE
	            `(,(IF (GET OBJECT ':DELETED)
			  '("Undelete" :VALUE :UNDELETE
			    :DOCUMENTATION "Undelete this file.")
			  '("Delete" :VALUE :DELETE :DOCUMENTATION "Delete this file"))
		     ,@ (IF (GET OBJECT ':LINK-TO)
			    (LIST '("Edit Link Transparencies"
				    :VALUE :EDIT-LINK-TRANSPARENCIES
				    :DOCUMENTATION "Edit link transparency properties")))
		     ("View" :VALUE :VIEW :DOCUMENTATION
		      "Print out the contents of this file.")
		     ("Rename":VALUE :RENAME :DOCUMENTATION "Rename this file.")
		     ("View Properties" :VALUE :VIEW-PROPERTIES
		      :DOCUMENTATION "View all known information about this file")
		     ("Edit Properties" :VALUE :EDIT-PROPERTIES
		      :DOCUMENTATAION "Edit properties of file")
		     ("New Property" :VALUE :PUTPROP
		      :DOCUMENTATION
		      "Add or remove a user-defined file property from this file")
		     ("Hardcopy" :VALUE :HARDCOPY
		      "Print this file on the local hardcopy device")
		     ("Dump" :VALUE :DUMP :DOCUMENTATION "Dump this file to tape."))
		   (STRING-APPEND "File operations: " (STRING PATHNAME))
		   '(:MOUSE) *TREE-EDIT-FILE-ITEM* WINDOW)
      (IF CHOICE (SETQ *TREE-EDIT-FILE-ITEM* ITEM))
      (SELECTQ CHOICE
	(:EDIT-LINK-TRANSPARENCIES
	 (LET ((CHANGE-RESULT
		 (TREE-EDIT-TRANSPARENCIES
		   (FORMAT NIL "Link transparency attributes for ~A" PATHNAME)
		   (TREE-EDIT-ATTRIBUTE-UPDATE OBJECT ':LINK-TRANSPARENCIES))))
	   (IF CHANGE-RESULT
	       (FS:CHANGE-FILE-PROPERTIES PATHNAME T
					  ':LINK-TRANSPARENCIES CHANGE-RESULT))))
	(:HARDCOPY (PROCESS-RUN-FUNCTION
		     "FSEdit Hardcopy" 'PRESS:HARDCOPY-VIA-MENUS PATHNAME))
	(:VIEW   (WITH-OPEN-FILE
		   (STREAM PATHNAME ':PRESERVE-DATES T ':DELETED T)
		   (STREAM-COPY-UNTIL-EOF STREAM TERMINAL-IO))
		 (TREE-EDIT-END-TYPEOUT))
	(:DUMP (LMFS:BACKUP-DUMPER ':DUMP-TYPE ':COMPLETE ':START-PATH PATHNAME)
	       (TREE-EDIT-END-TYPEOUT))
	(T     (TREE-EDIT-COMMON CHOICE OBJECT PATHNAME TREE))))))
)
