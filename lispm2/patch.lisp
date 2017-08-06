;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS -*-
;;; More winning Lisp Machine software patch facility.   DLW & BEE 10/24/80
;;; The functions in this file manage the patch files

;;; Lossages:
;;; rename buffer to be constant name i.e. PATCHES or the like
;;; commands to write out the patches and compile them, or compile them into
;;;   your machine for testing

;;; There are 3 kinds of files for each system. There is a patch description file.
;;; This file contains a version of the patch-system structure. This gives the
;;; format of the patch, and also contains the major version counter for the system
;;; In addition, it contains the format strings to generate the file names for the other
;;; files. The other files are: System directories which contain the list of patches
;;; and their descriptions for a given major version, and the patch files, which contain
;;; the actual patches

;;; Format of main patch directory
(DEFSTRUCT (PATCH-MAJOR :LIST (:CONC-NAME PATCH-))
  NAME				;system to be patched
  VERSION			;most recent version of the system
  )

;;; Internal format of each patch system
(DEFSTRUCT (PATCH-SYSTEM :LIST (:CONC-NAME PATCH-) (:INCLUDE PATCH-MAJOR))
  STATUS			;a keyword
  (VERSION-LIST NIL)		;list of versions loaded and explanations
  )

;;; Format of patch directory
(DEFSTRUCT (PATCH-DIR :LIST :CONC-NAME)
  STATUS
  VERSION-LIST)

;;; Information for each patch
(DEFSTRUCT (PATCH-VERSION :LIST (:CONC-NAME VERSION-))
  NUMBER			;minor number of the patch
  EXPLANATION			;explanation of the patch
  (AUTHOR USER-ID)		;who dun it
  )

;;; List of systems that are legal to patch, i.e. list of PATCH-SYSTEM's
(DEFVAR PATCH-SYSTEMS-LIST NIL)

;;; Given the name of a system, or the system itself, return the patch system
(DEFUN GET-PATCH-SYSTEM-NAMED (NAME &OPTIONAL NO-ERROR-P &AUX SYSTEM)
  (COND ((AND (SETQ SYSTEM (FIND-SYSTEM-NAMED NAME NO-ERROR-P))
	      (ASS #'STRING-EQUAL (SYSTEM-NAME SYSTEM) PATCH-SYSTEMS-LIST)))
	(NO-ERROR-P NIL)
	(T (FERROR NIL "The system ~A is not patchable" SYSTEM))))

;;; Called when actually loading a system, create the PATCH-SYSTEM.  Returns the version.
(DEFUN ADD-PATCH-SYSTEM (NAME &AUX PATCH-SYSTEM VERSION FIRST-VERS PATCH-DIR STATUS)
  ;; Flush old patch system if there is one
  (AND (SETQ PATCH-SYSTEM (GET-PATCH-SYSTEM-NAMED NAME T))
       (SETQ PATCH-SYSTEMS-LIST (DELQ PATCH-SYSTEM PATCH-SYSTEMS-LIST)))
  (SETQ VERSION (GET-PATCH-SYSTEM-MAJOR-VERSION NAME)
	PATCH-SYSTEM (MAKE-PATCH-SYSTEM NAME NAME VERSION VERSION)
	PATCH-DIR (READ-PATCH-DIRECTORY PATCH-SYSTEM)
	FIRST-VERS (FIRST (PATCH-DIR-VERSION-LIST PATCH-DIR)))
  (OR (EQ (VERSION-NUMBER FIRST-VERS) 0)
      (FERROR NIL "Patch directory for ~A messed up: ~S" NAME FIRST-VERS))
  (SETF (PATCH-STATUS PATCH-SYSTEM) (SETQ STATUS (PATCH-DIR-STATUS PATCH-DIR)))
  (SETF (PATCH-VERSION-LIST PATCH-SYSTEM) (NCONS FIRST-VERS))
  (SETQ PATCH-SYSTEMS-LIST (NCONC PATCH-SYSTEMS-LIST (NCONS PATCH-SYSTEM)))
  (VALUES VERSION STATUS))

;;; Given the name of a system, this increments the major version as seen in the master
;;; directory on the file computer.
(DEFUN INCREMENT-PATCH-SYSTEM-MAJOR-VERSION (NAME STATUS &AUX VERSION PATCH-MAJOR)
  (SETQ VERSION (GET-PATCH-SYSTEM-MAJOR-VERSION NAME T))
  (COND ((NULL VERSION)
	 (FORMAT T "~&No master directory for system ~A, creating one" NAME)
	 (SETQ VERSION 0)))
  (SETQ VERSION (1+ VERSION)
	PATCH-MAJOR (MAKE-PATCH-MAJOR NAME NAME VERSION VERSION))
  (WITH-OPEN-FILE (FILE (PATCH-SYSTEM-PATHNAME NAME ':SYSTEM-DIRECTORY) '(:WRITE))
    (WRITE-RESPONSIBILITY-COMMENT FILE)
    (LET ((BASE 10.))
      (PRINT PATCH-MAJOR FILE)))
  (LET ((FIRST-VERS (MAKE-PATCH-VERSION NUMBER 0
					EXPLANATION (FORMAT NIL "~A Loaded" NAME))))
    (WRITE-PATCH-DIRECTORY PATCH-MAJOR (MAKE-PATCH-DIR STATUS STATUS
						       VERSION-LIST (NCONS FIRST-VERS))))
  VERSION)

;;; Given the name of a system, this returns the major version as seen in the master
;;; directory on the file computer.
(DEFUN GET-PATCH-SYSTEM-MAJOR-VERSION (NAME &OPTIONAL NO-ERROR-P)
  (LET ((PATHNAME (PATCH-SYSTEM-PATHNAME NAME ':SYSTEM-DIRECTORY)))
    (WITH-OPEN-FILE (FILE PATHNAME '(:READ :NOERROR))
      (COND ((NOT (STRINGP FILE))
	     (LET ((PATCH-MAJOR (LET ((IBASE 10.))
				  (READ FILE))))
	       (IF (NOT (STRING-EQUAL NAME (PATCH-NAME PATCH-MAJOR)))
		   (FERROR NIL
		       "~A name does not agree with ~A the name in the patch descriptor file"
		       NAME (PATCH-NAME PATCH-MAJOR)))
	       (PATCH-VERSION PATCH-MAJOR)))
	    ((NOT NO-ERROR-P)
	     (FS:FILE-PROCESS-ERROR FILE PATHNAME NIL))))))

;;; Read in a patch directory file, returning the list-structure representation.
(DEFUN READ-PATCH-DIRECTORY (PATCH &AUX DIR)
  (WITH-OPEN-FILE (PATCH-DIR (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH) ':VERSION-DIRECTORY
						    (PATCH-VERSION PATCH))
			     '(:READ))
    (LET ((IBASE 10.) (PACKAGE PKG-USER-PACKAGE))
      (SETQ DIR (READ PATCH-DIR))))
  ;;*** Temporary, reformat old patch directories ***
  (OR (SYMBOLP (PATCH-DIR-STATUS DIR))
      (SETQ DIR (MAKE-PATCH-DIR STATUS ':RELEASED VERSION-LIST DIR)))
  DIR)

;;; Write out a patch directory file from the list-structure representation.
(DEFUN WRITE-PATCH-DIRECTORY (PATCH PATCH-DIR)
  (LET ((BASE 10.) (PACKAGE PKG-USER-PACKAGE) (*NOPOINT T))
    (WITH-OPEN-FILE (STREAM (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH) ':VERSION-DIRECTORY
						      (PATCH-VERSION PATCH))
			       '(:WRITE))
       (FORMAT STREAM
	       ";;; -*- Mode: Lisp; Package: User; Base: 10.; Patch-File: T -*-
;;; Patch directory for ~A version ~D
"
	       (PATCH-NAME PATCH) (PATCH-VERSION PATCH))
       (WRITE-RESPONSIBILITY-COMMENT STREAM)
       (FUNCALL STREAM ':TYO #/()
       (PRIN1 (PATCH-DIR-STATUS PATCH-DIR) STREAM)
       (FUNCALL STREAM ':STRING-OUT "
 (")
       (DOLIST (PATCH (PATCH-DIR-VERSION-LIST PATCH-DIR))
	 (PRIN1 PATCH STREAM)
	 (FUNCALL STREAM ':STRING-OUT "
  "))
       (FUNCALL STREAM ':STRING-OUT "))"))))

(DEFUN PRINT-PATCHES (&OPTIONAL (SYSTEM "System"))
  (LET ((PATCH (GET-PATCH-SYSTEM-NAMED SYSTEM T)))
    (COND ((NULL PATCH)
	   (FORMAT T "~%No ~A system loaded~%" SYSTEM))
	  (T (FORMAT T "~%~A ~9TModification:~%" (PATCH-NAME PATCH))
	     (DO ((V (REVERSE (PATCH-VERSION-LIST PATCH)) (CDR V))
		  (MAJOR (PATCH-VERSION PATCH)))
		 ((NULL V))
	       (FORMAT T "~D.~D ~9T~A -- ~A~%"
		       MAJOR
		       (VERSION-NUMBER (FIRST V))
		       (VERSION-EXPLANATION (FIRST V))
		       (VERSION-AUTHOR (FIRST V))))))))

;;; Ask user if he wants the patches that he hasn't loaded yet for a given system
;;; SILENT implies NOSELECTIVE, since it is silly to ask questions when no info
;;; was printed.  SILENT does not currently suppress "Loading..." messages.
;;; In place of an option you may give the names of systems you want patches for.
(DEFUN LOAD-PATCHES (&REST OPTIONS &AUX TEM)
  (LET ((SYSTEM-NAMES NIL)			;A-list of systems to load patches for.
	(SELECTIVE-P T)				;Ask the user.
	(VERBOSE-P T)				;Tell the user what's going on.
	(SILENT-P NIL))
    (DO ((OPTS OPTIONS (CDR OPTS)))
	((NULL OPTS))
      (SELECTQ (CAR OPTS)
	(:SYSTEMS
	 (SETQ OPTS (CDR OPTS))
	 (SETQ SYSTEM-NAMES
	       (LOOP FOR SYS-NAME IN (CAR OPTS)
		     COLLECT (GET-PATCH-SYSTEM-NAMED SYS-NAME))))
	(:SILENT (SETQ VERBOSE-P NIL SELECTIVE-P NIL SILENT-P T))
	(:VERBOSE (SETQ VERBOSE-P T))
	(:SELECTIVE (SETQ SELECTIVE-P T))
	(:NOSELECTIVE (SETQ SELECTIVE-P NIL))
	(OTHERWISE
	  (COND ((AND (OR (SYMBOLP (CAR OPTS)) (STRINGP (CAR OPTS)))
		      (SETQ TEM (GET-PATCH-SYSTEM-NAMED (CAR OPTS) T)))
		 (PUSH TEM SYSTEM-NAMES))
		(T (FERROR NIL "~S is not a LOAD-PATCHES option and not a system name"
			       (CAR OPTS)))))))
    (OR SYSTEM-NAMES (SETQ SYSTEM-NAMES PATCH-SYSTEMS-LIST))
    (LET ((FIRST-SYSTEM T))			; This is the first system being patched.
      (DOLIST (PATCH SYSTEM-NAMES)
	(LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH))
	       (NEW-VERS (PATCH-DIR-VERSION-LIST PATCH-DIR))
	       (MAJOR (PATCH-VERSION PATCH))
	       (FIRST-PATCH T)			; This is the first patch to this system.
	       (CHANGE-STATUS T)		;Ok to change the system status
	       (PROCEED-FLAG (NOT SELECTIVE-P)))	; Has the user said to proceed?
	  (DOLIST (VERSION (CDR (MEMASSQ (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST PATCH)))
					 NEW-VERS)))
	    (LET* ((STR (FORMAT NIL "~D.~D" MAJOR (VERSION-NUMBER VERSION)))
		   (FILENAME (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH)
						    ':PATCH-FILE
						    (PATCH-VERSION PATCH)
						    (VERSION-NUMBER VERSION)
						    "QFASL")))
	      (COND ((NULL (VERSION-EXPLANATION VERSION))
		     ;; NIL is used to mark patches that are reserved, but not finished.
		     ;; We can't load any more patches without this one, in order to
		     ;; make sure that any two systems claiming to be version xx.yy
		     ;; always have exactly the same set of patches loaded.  Punt.
		     ;; If someone forgets to finish a patch, we assume a hacker will
		     ;; eventually see what is happening and fix the directory to unstick
		     ;; things.
		     (RETURN))
		    (T
		     (COND ((AND FIRST-PATCH VERBOSE-P)
			    (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
			    (IF (NOT FIRST-SYSTEM)
				(TERPRI))
			    (FORMAT T "Patches for ~A (Current version is ~D.~D):"
				    (PATCH-NAME PATCH) MAJOR (CAAR (LAST NEW-VERS)))))
		     (IF VERBOSE-P
			 (FORMAT T "~&~A ~5T~A -- ~A" STR (VERSION-EXPLANATION VERSION)
						      (VERSION-AUTHOR VERSION)))
		     (SELECTQ-EVERY
		       (COND (PROCEED-FLAG)
			     (T (FORMAT T ".  ")
				(FQUERY '(:FRESH-LINE NIL
					  :CHOICES (((T "Yes.") #/Y #\SP #/T #\HAND-UP)
						    ((NIL "No.") #/N #\RUBOUT #\HAND-DOWN)
						    ((PROCEED "Proceed.") #/P)))
					"Load? ")))
		       (NIL
			 ;; "No", don't load any more for this system.
			 ;; Also don't change the status.
			 (SETQ CHANGE-STATUS NIL)
			 (RETURN NIL))
		       (PROCEED
			 ;; "Proceed" with the rest for this system.
			 (SETQ PROCEED-FLAG T))
		       ((T PROCEED)
			;; "Yes" or "Proceed", do this one.
			(LOAD FILENAME NIL NIL T SILENT-P)	; Don't set default,
			(PUSH VERSION (PATCH-VERSION-LIST PATCH)))))))
	    (SETQ FIRST-PATCH NIL))
	  (AND CHANGE-STATUS
	       (LET ((NEW-STATUS (PATCH-DIR-STATUS PATCH-DIR)))
		 (COND ((NEQ (PATCH-STATUS PATCH) NEW-STATUS)
			(AND VERBOSE-P
			     (FORMAT T "~&~A is now ~A."
				     (PATCH-NAME PATCH)
				     (FOURTH (ASSQ NEW-STATUS
						   SYSTEM-STATUS-ALIST))))
			;; Update the status.
			(SETF (PATCH-STATUS PATCH) NEW-STATUS)))))
	  ;; Update the status.
	  (SETF (PATCH-STATUS PATCH) (PATCH-DIR-STATUS PATCH-DIR)))
	(SETQ FIRST-SYSTEM NIL)))))

;;; Say who did it: which hardware, which firmware, which software, and which meatware.
(DEFUN WRITE-RESPONSIBILITY-COMMENT (STREAM)
  (FORMAT STREAM ";;; Written ~\DATIME\ by ~A,
;;; while running on ~A from band ~C
;;; with ~A.~2%"
	  USER-ID LOCAL-PRETTY-HOST-NAME
	  (LDB 2010 CURRENT-LOADED-BAND) (SYSTEM-VERSION-INFO)))

;;; Allocate a minor system number.  Mark it in the directory with a NIL.
;;; Returns the version number
(DEFUN RESERVE-PATCH (PATCH)
  (LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH))
	 (PATCHES (PATCH-DIR-VERSION-LIST PATCH-DIR))
	 (LAST-PATCH (LAST PATCHES))
	 (NEW-VERSION (1+ (VERSION-NUMBER (FIRST LAST-PATCH)))))
    (RPLACD LAST-PATCH
	    (NCONS (MAKE-PATCH-VERSION NUMBER NEW-VERSION EXPLANATION NIL)))
    (WRITE-PATCH-DIRECTORY PATCH PATCH-DIR)
    NEW-VERSION))

;;; Finish up making the patch for the specified minor system version number.
;;; "message" is the message to be displayed to the user in GET-PATCHES.
;;; This replaces the NILs left by RESERVE-PATCH with the message.
;;; Returns NIL if successful, otherwise returns a string with an error
;;; message.
(DEFUN CONSUMMATE-PATCH (PATCH NUMBER MESSAGE)
  (LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH))
	 (PATCHES (PATCH-DIR-VERSION-LIST PATCH-DIR))
	 (FILENAME (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH) ':PATCH-FILE
					  (PATCH-VERSION PATCH) NUMBER
					  "LISP")))
    (QC-FILE FILENAME)
    (SETF (CADR (ASSQ NUMBER PATCHES)) MESSAGE)
    (WRITE-PATCH-DIRECTORY PATCH PATCH-DIR)
    NIL))	;Despite the comment above, this function doesnt seem to detect any
		; meaningful errors and returns randomness.  So return NIL for success.

;;; Utilities for system versions

;;; Additional info is printed after the version when the system is booted.
(DEFVAR SYSTEM-ADDITIONAL-INFO "")

;;; This function updates the system version, asking the user.  If this is a fresh
;;; cold-load, the major_version stored on the file system is incremented.
;;; Returns string to go in the disk label.
;;; The user is allowed to add additional commentary.  If the whole string
;;; won't fit in the silly 16-character disk-label comment field, the user
;;; is asked to retype it in an abbreviated form.
(DEFUN GET-NEW-SYSTEM-VERSION ()
  (FORMAT T "~&This is ~A~%Add additional comment? (Return => ~S): "
	    (SYSTEM-VERSION-INFO) SYSTEM-ADDITIONAL-INFO)
  (LET ((TEM (READLINE)))
    (OR (EQUAL TEM "") (SETQ SYSTEM-ADDITIONAL-INFO TEM)))
  (LET ((VERS (SYSTEM-VERSION-INFO T)))
    ;; If short version doesn't fit, allow user to edit it (e.g. abbreviate system names)
    (DO ((SHORT) (USER)) (( (STRING-LENGTH VERS) 16.))
      (SETQ SHORT (SUBSTRING VERS 0 16.))
      (FORMAT T "~%~S will not fit in disk label, please abbreviate
to 16 characters.  (Return => ~S): "
	        VERS SHORT)
      (SETQ USER (READLINE))
      (SETQ VERS (IF (EQUAL USER "") SHORT USER)))
    VERS))

(DEFVAR SYSTEM-STATUS-ALIST '((:EXPERIMENTAL "Experimental" "Exp" "experimental")
			      (:RELEASED "" "" "released")
			      (:OBSOLETE "Obsolete" "Obs" "obsolete")
			      (:BROKEN "Broken" "Broke" "broken")))

;;; Format and return a string that describes the current system.
;;; This string lists all the systems in this world with their versions,
;;; the microcode version, and any additional information.
;;; With BRIEF-P, return stuff suitable for disk label comment.
(DEFUN SYSTEM-VERSION-INFO (&OPTIONAL (BRIEF-P NIL) &AUX (FIRST T) TEM)
  (WITH-OUTPUT-TO-STRING (S)
    (DOLIST (SYS PATCH-SYSTEMS-LIST)
      (COND ((NOT (AND BRIEF-P (SYSTEM-SHOULD-NOT-APPEAR-IN-DISK-LABEL (PATCH-NAME SYS))))
	     (IF (NOT FIRST)
		 (FUNCALL S ':STRING-OUT (IF BRIEF-P " " ", ")))
	     (SETQ FIRST NIL)
	     (COND ((NULL (SETQ TEM (ASSQ (PATCH-STATUS SYS) SYSTEM-STATUS-ALIST)))
		    (SETQ TEM (STRING (PATCH-STATUS SYS))))
		   (BRIEF-P
		    (SETQ TEM (THIRD TEM)))
		   (T
		    (SETQ TEM (SECOND TEM))))
	     (COND ((NOT (EQUAL TEM ""))
		    (FUNCALL S ':STRING-OUT TEM)
		    (FUNCALL S ':TYO #\SP)))
	     (IF (NOT (AND BRIEF-P (EQUAL (PATCH-NAME SYS) "System")))
		 (FORMAT S "~A " (IF (NOT BRIEF-P) (PATCH-NAME SYS)
				     (SYSTEM-SHORT-NAME (PATCH-NAME SYS)))))
	     (FORMAT S "~D.~D"
		     (PATCH-VERSION SYS) (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST SYS)))))))
    (IF (NOT BRIEF-P)
	(FORMAT S ", microcode ~D" %MICROCODE-VERSION-NUMBER))
    (AND (PLUSP (STRING-LENGTH SYSTEM-ADDITIONAL-INFO))
	 (FORMAT S ", ~A" SYSTEM-ADDITIONAL-INFO))))

(DEFUN DESCRIBE-SYSTEM-VERSIONS (&OPTIONAL (S STANDARD-OUTPUT)
				 &AUX (MAX 9) NAME-LIST STATUS)
  (SETQ NAME-LIST (MAKE-LIST (LENGTH PATCH-SYSTEMS-LIST)))
  (DO ((SYS PATCH-SYSTEMS-LIST (CDR SYS))
       (NAM NAME-LIST (CDR NAM)))
      ((NULL SYS))
    (SETQ STATUS (SECOND (ASSQ (PATCH-STATUS (CAR SYS)) SYSTEM-STATUS-ALIST)))
    (SETF (CAR NAM)
	  (WITH-OUTPUT-TO-STRING (S1)
	    (COND ((PLUSP (ARRAY-ACTIVE-LENGTH STATUS))
		   (FUNCALL S1 ':STRING-OUT STATUS)
		   (FUNCALL S1 ':TYO #\SP)))
	    (FUNCALL S1 ':STRING-OUT (PATCH-NAME (CAR SYS)))))
    (SETQ MAX (MAX (ARRAY-ACTIVE-LENGTH (CAR NAM)) MAX)))
  (DO ((SYS PATCH-SYSTEMS-LIST (CDR SYS))
       (NAM NAME-LIST (CDR NAM)))
      ((NULL SYS))
    (FORMAT S "~% ~A" (CAR NAM))
    (DOTIMES (I (- MAX (ARRAY-ACTIVE-LENGTH (CAR NAM))))
      (FUNCALL S ':TYO #\SP))
    (FORMAT S " ~3D.~D"
	    (PATCH-VERSION (CAR SYS))
	    (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST (CAR SYS))))))
  (FORMAT S "~% Microcode")
  (DOTIMES (I (- MAX 9))
    (FUNCALL S ':TYO #\SP))
  (FORMAT S " ~3D" %MICROCODE-VERSION-NUMBER))

(DEFUN PRINT-SYSTEM-MODIFICATIONS (&REST SYSTEM-NAMES)
  (IF (NULL SYSTEM-NAMES)
      (DOLIST (PATCH PATCH-SYSTEMS-LIST)
	(PRINT-PATCHES (PATCH-NAME PATCH)))
      (DOLIST (PAT-NAME SYSTEM-NAMES)
	(PRINT-PATCHES PAT-NAME))))

;;; Returns three values, the major and minor version of a given system and its status
(DEFUN GET-SYSTEM-VERSION (&OPTIONAL (SYSTEM "System"))
  (LET ((PATCH (GET-PATCH-SYSTEM-NAMED SYSTEM T)))
    (IF PATCH
	(VALUES (PATCH-VERSION PATCH)
		(VERSION-NUMBER (FIRST (PATCH-VERSION-LIST PATCH)))
		(PATCH-STATUS PATCH)))))

;;; Change the status of a system
(DEFUN SET-SYSTEM-STATUS (SYSTEM NEW-STATUS &OPTIONAL MAJOR-VERSION &AUX PATCH PATCH-DIR)
  (OR (ASSQ NEW-STATUS SYSTEM-STATUS-ALIST)
      (FERROR NIL "~S is not a defined system status." NEW-STATUS))
  (IF MAJOR-VERSION
      (SETQ PATCH (MAKE-PATCH-SYSTEM NAME SYSTEM VERSION MAJOR-VERSION STATUS NEW-STATUS))
      (SETQ PATCH (GET-PATCH-SYSTEM-NAMED SYSTEM))
      ;; Also change in core copy
      (SETF (PATCH-STATUS PATCH) NEW-STATUS))
  (SETQ PATCH-DIR (READ-PATCH-DIRECTORY PATCH))
  (SETF (PATCH-DIR-STATUS PATCH-DIR) NEW-STATUS)
  (WRITE-PATCH-DIRECTORY PATCH PATCH-DIR))
