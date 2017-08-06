;;; -*-Mode:LISP; Package:ZWEI; Base:8.-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; This file provides the initial simple buffer/file stuff for ZWEI.
;;; It uses the utility file stuff in ZWEI; FILES.
;;; Some of the section specific functions are in ZWEI; SECTIO.

;;; A buffer may be in any of the following states, based on the BUFFER-FILE-ID:
;;; * BUFFER-FILE-ID is NIL.  There is no corresponding file.  The group symbol
;;;    is a gensym.  BUFFER-NAME is simply considered the name of the buffer,
;;;    and BUFFER-TICK is the time the buffer was created.
;;; * BUFFER-FILE-ID is T.  There is a file, but no I/O has been done to it.
;;;    That is, it is a "(New File)".  The BUFFER-GENERIC-PATHNAME is for real, but
;;;    ZWEI put relevant properties onto the group symbol itself, rather than
;;;    finding them in the file.  The buffer name is the file name. BUFFER-TICK
;;;    is the time the file was created.
;;; * BUFFER-FILE-ID is a list.  There is a file, we have done I/O to it.
;;;    The last I/O was at time BUFFER-TICK; at that time the copy on disk
;;;    was the same as the copy in the machine.  the BUFFER-NAME is the name
;;;    of the file, and the BUFFER-GENERIC-PATHNAME is for real.
;;; In any case, the group symbol is used to remember the package and the major
;;; mode of the text in the buffer.

;;; The fundamental operations to provide are:
;;; NOTE: these are not the names or calling sequences of the real functions.
;;; c-X c-F: (FIND-FILE <pathname>).  Select or create appropriate buffer.
;;;           Merges the pathname default with argument.  Reads in file if
;;;           creting and the file exists.
;;; c-X B:   (FIND-BUFFER <buffer-name>)  Select or create appropriate buffer.
;;; c-X c-W: (WRITE-BUFFER <buffer> <pathname>)  Write to specified file, altering the
;;;           BUFFER-NAME if asked to.
;;; c-X c-S: (SAVE-BUFFER <buffer>)  Write to its file.  If ID is NIL, turn into WRITE-BUFFER.
;;; Revert:  (REVERT-BUDFER <buffer>)  Read in from most recent version.  If ID = NIL,
;;;           error.
;;; Not Modified: (NOT-MODIFIED <buffer>)  Forget this was modified, by making TICK
;;;           be the present time.
;;; c-X c-B: (LIST-BUFFERS)  List all buffers.
;;; c-X K:   (KILL-BUFFER <buffer>)  Kills it.
;;; Save All Files: (obvious)

;;; Utility functions.

;;; Create an empty buffer, setting up none of the BUFFER- fields
;;; except the BPs and mode defaults
(DEFUN CREATE-BUFFER (&OPTIONAL (ACTIVATE-P T))
  (LET ((LINE (CREATE-LINE 'ART-STRING 0 NIL))
	BUFFER)
    (SETQ BUFFER (MAKE-BUFFER NODE-TICK (TICK)
			      BUFFER-SAVED-MAJOR-MODE *DEFAULT-MAJOR-MODE*))
    (SETF (INTERVAL-FIRST-BP BUFFER) (CREATE-BP LINE 0 ':NORMAL))
    (SETF (INTERVAL-LAST-BP BUFFER) (CREATE-BP LINE 0 ':MOVES))
    (SETF (BUFFER-SAVED-POINT BUFFER) (CREATE-BP LINE 0 ':NORMAL))
    (SETF (BUFFER-SAVED-MARK BUFFER) (CREATE-BP LINE 0 ':NORMAL))
    (SETF (BUFFER-SAVED-WINDOW-START-BP BUFFER) (CREATE-BP LINE 0 ':NORMAL))
    (SETF (LINE-NODE LINE) BUFFER)
    (SETF (BUFFER-SAVED-MODE-LIST BUFFER) (STICKY-MODE-LIST))
    (AND ACTIVATE-P (PUSH BUFFER *ZMACS-BUFFER-LIST*))
    BUFFER))

;;; Create an empty buffer, ready to be used to the Luser.
;;; Used by c-X B when it creates a new buffer.
(DEFUN CREATE-ONE-BUFFER-TO-GO (&OPTIONAL (NAME (GENERATE-BUFFER-NAME)))
  (LET ((BUFFER (CREATE-BUFFER))
	(DUMMY-PATHNAME (FS:MAKE-DUMMY-PATHNAME NAME)))
    (SETF (BUFFER-NAME BUFFER) NAME)
    (SETF (BUFFER-GENERIC-PATHNAME BUFFER) DUMMY-PATHNAME)
    (SETF (BUFFER-FILE-ID BUFFER) NIL)
    (SETF (BUFFER-TICK BUFFER) (TICK))
    (INITIALIZE-GENERIC-PATHNAME DUMMY-PATHNAME)
    (PUSH (CONS NAME BUFFER) *ZMACS-BUFFER-NAME-ALIST*)
    BUFFER))

(DEFUN GENERATE-BUFFER-NAME ()
  (SETQ *ZMACS-BUFFER-COUNTER* (1+ *ZMACS-BUFFER-COUNTER*))
  (FORMAT NIL "BUFFER-~D" *ZMACS-BUFFER-COUNTER*))

;;; Put initial properties on this file group symbol.
;;; This is used when creating new buffers in c-X B, or when
;;; there is a "(New File)" in c-X c-F.
;;; If TYPE is present, it is the type of the pathname which was read
;;; with c-X c-F.  This attempts to make the mode "correct".
(DEFUN INITIALIZE-GENERIC-PATHNAME (GENERIC-PATHNAME &OPTIONAL TYPE)
  (FUNCALL GENERIC-PATHNAME ':PUTPROP
	   (OR (CDR (ASSOC TYPE FS:*FILE-TYPE-MODE-ALIST*))
	       *DEFAULT-MAJOR-MODE*)
	   ':MODE)
  (AND *DEFAULT-PACKAGE*
       (FUNCALL GENERIC-PATHNAME ':PUTPROP *DEFAULT-PACKAGE* ':PACKAGE)))

;;; Search for a buffer with the given name.  Return the buffer, or NIL if not found.
;;; If CREATE-P is specified, the buffer will be created if it does not already exist.
(DEFUN FIND-BUFFER-NAMED (NAME &OPTIONAL CREATE-P)
  (OR (STRINGP NAME)
      (SETQ NAME (FUNCALL NAME ':STRING-FOR-EDITOR)))
  (DO ((L *ZMACS-BUFFER-LIST* (CDR L)))
      ((NULL L)
       (AND CREATE-P (CREATE-ONE-BUFFER-TO-GO NAME)))
    (AND (STRING-EQUAL (BUFFER-NAME (CAR L)) NAME)
	 (RETURN (CAR L)))))

;;; This prevents you from being permanently shafted if you somehow select the
;;; mini-buffer as the current buffer.  The (ED) function sends this message.
(DEFMETHOD (ZMACS-TOP-LEVEL-EDITOR :MINI-BUFFER-ACCIDENTALLY-SELECTED-KLUDGE) ()
  (OR (TYPEP *INTERVAL* 'BUFFER)
      (FUNCALL-SELF ':SET-INTERVAL (CREATE-ONE-BUFFER-TO-GO
				     "MINI-BUFFER-ACCIDENTALLY-SELECTED-KLUDGE"))))

;;; Make BUFFER be the currently selected buffer.
(DEFMETHOD (ZMACS-EDITOR :SET-INTERVAL) (INTERVAL)
  (IF (TYPEP INTERVAL 'BUFFER)
      (MAKE-BUFFER-CURRENT INTERVAL)
      (SETQ *INTERVAL* INTERVAL)))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Select"
			  TYPEOUT-MAKE-BUFFER-CURRENT T "Select this buffer.")

(DEFVAR *READ-BUFFER-KLUDGE* NIL)

(DEFUN TYPEOUT-MAKE-BUFFER-CURRENT (BUFFER)
  (COND ((NEQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
	 (MAKE-BUFFER-CURRENT BUFFER))
	(*READ-BUFFER-KLUDGE*
	 (*THROW 'RETURN-FROM-COMMAND-LOOP BUFFER))
	(T
	 (FUNCALL STANDARD-INPUT ':UNTYI *LAST-COMMAND-CHAR*)
	 (*THROW 'TOP-LEVEL T))))

(DEFUN MAKE-BUFFER-CURRENT (BUFFER)
  ;; Save away the major and minor modes, and turn them off.
  (SETF (BUFFER-SAVED-MODE-LIST *INTERVAL*) *MODE-LIST*)
  (SETF (BUFFER-SAVED-MAJOR-MODE *INTERVAL*) *MAJOR-MODE*)
  (UN-SET-MODES)
  ;; Update *ZMACS-BUFFER-HISTORY*, for C-X B <Newline>
  (UPDATE-BUFFER-HISTORY *INTERVAL* BUFFER)
  ;; Point the window at this interval, and make it the default interval.
  ;; If called from the two-window commands, the window may already be
  ;; pointing to this buffer, in which case don't change it
  (OR (EQ (WINDOW-INTERVAL *WINDOW*) BUFFER)
      (FUNCALL-SELF ':SET-WINDOW-INTERVAL *WINDOW* BUFFER))
  (SETQ *INTERVAL* BUFFER)
  (UPDATE-BUFFER-NAMES BUFFER)
  ;; Recompute which package READs should be done in.
  (COMPUTE-BUFFER-PACKAGE BUFFER)
  ;; Restore the old major and minor modes.
  (SET-MODES (BUFFER-SAVED-MODE-LIST *INTERVAL*) (BUFFER-SAVED-MAJOR-MODE *INTERVAL*))
  NIL)

;;; This updates *ZMACS-BUFFER-HISTORY*, which is the thing used when you type
;;; "c-X B RETURN" to go to the previous buffer.  It is a bit better
;;; then the one in EMACS, in that if you kill buffers, it will remember
;;; two buffers ago, etc.  It is called by MAKE-BUFFER-CURRENT.
(DEFUN UPDATE-BUFFER-HISTORY (OLD-BUFFER NEW-BUFFER)
  (AND (NEQ OLD-BUFFER NEW-BUFFER) (NEQ NEW-BUFFER (CAR *ZMACS-BUFFER-HISTORY*))
       (SETQ *ZMACS-BUFFER-HISTORY*
	     (CONS NEW-BUFFER (DELQ NEW-BUFFER *ZMACS-BUFFER-HISTORY*)))))

;;; This updates *ZMACS-BUFFER-NAME* and *ZMACS-BUFFER-NAME-ALIST*
(DEFUN UPDATE-BUFFER-NAMES (BUFFER &OPTIONAL NAME &AUX TEM)
  (OR NAME (SETQ NAME (BUFFER-NAME BUFFER)))
  (IF (SETQ TEM (RASSOC BUFFER *ZMACS-BUFFER-NAME-ALIST*))
      (RPLACA TEM NAME)
      (PUSH (CONS NAME BUFFER) *ZMACS-BUFFER-NAME-ALIST*))
  (SETQ *ZMACS-BUFFER-VERSION-STRING* (BUFFER-VERSION-STRING BUFFER)
	*ZMACS-BUFFER-NAME* NAME))

(DEFUN ROTATE-BUFFER-HISTORY (N)
  (AND (> N (LENGTH *ZMACS-BUFFER-HISTORY*)) (BARF))
  (ROTATE-TOP-OF-LIST *ZMACS-BUFFER-HISTORY* N)
  (MAKE-BUFFER-CURRENT (CAR *ZMACS-BUFFER-HISTORY*))
  DIS-TEXT)

;;; Associate BUFFER with WINDOW.
(DEFMETHOD (ZMACS-EDITOR :SET-WINDOW-INTERVAL) (WINDOW INTERVAL)
  (IF (TYPEP INTERVAL 'BUFFER)
      (SET-WINDOW-BUFFER WINDOW INTERVAL)
      (SET-WINDOW-INTERVAL WINDOW INTERVAL)))

(DEFUN SET-WINDOW-BUFFER (WINDOW BUFFER)
  (MOVE-BP (BUFFER-SAVED-POINT (WINDOW-INTERVAL WINDOW)) (WINDOW-POINT WINDOW))
  (MOVE-BP (BUFFER-SAVED-MARK (WINDOW-INTERVAL WINDOW)) (WINDOW-MARK WINDOW))
  (MOVE-BP (BUFFER-SAVED-WINDOW-START-BP (WINDOW-INTERVAL WINDOW)) (WINDOW-START-BP WINDOW))
  (SETF (BUFFER-SAVED-FONT-ALIST (WINDOW-INTERVAL WINDOW)) (WINDOW-FONT-ALIST WINDOW))
  (SETF (WINDOW-INTERVAL WINDOW) BUFFER)
  (MOVE-BP (WINDOW-POINT WINDOW) (BUFFER-SAVED-POINT BUFFER))
  (MOVE-BP (WINDOW-MARK WINDOW) (BUFFER-SAVED-MARK BUFFER))
  (MOVE-BP (WINDOW-START-BP WINDOW) (BUFFER-SAVED-WINDOW-START-BP BUFFER))
  (REDEFINE-WINDOW-BACKSPACE-FLAG WINDOW (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER)
						  ':GET ':BACKSPACE))
  (REDEFINE-WINDOW-TAB-NCHARS WINDOW (OR (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER)
						  ':GET ':TAB-WIDTH)
					 8))
  (REDEFINE-FONTS WINDOW (BUFFER-SAVED-FONT-ALIST BUFFER))
  (SETQ *FONT* 0)
  (UPDATE-FONT-NAME)
  (MUST-REDISPLAY WINDOW DIS-TEXT)
  (CHANGE-WINDOW-LABEL WINDOW))

;;; If we ask to create a package from the typein window, when the main window is
;;; selected back, we may call COMPUTE-BUFFER-PACKAGE again before the question
;;; has taken effect.
(DEFVAR *RECURSIVE-PKG-FIND-PACKAGE-KLUDGE* NIL)

;;; Update PACKAGE from the current buffer.  This is called whenever either
;;; the current buffer changes, or the package of the buffer changes.
(DEFUN COMPUTE-BUFFER-PACKAGE (BUFFER)
  (OR *RECURSIVE-PKG-FIND-PACKAGE-KLUDGE*
      (LET ((GENERIC-PATHNAME (BUFFER-GENERIC-PATHNAME BUFFER))
	    (*RECURSIVE-PKG-FIND-PACKAGE-KLUDGE* T)
	    N)
	(MULTIPLE-VALUE-BIND (VARS VALS)
	    (FS:FILE-PROPERTY-BINDINGS GENERIC-PATHNAME)
	  (IF (SETQ N (FIND-POSITION-IN-LIST 'PACKAGE VARS))
	      (SETQ PACKAGE (NTH N VALS))
	      (FUNCALL GENERIC-PATHNAME ':PUTPROP (PKG-NAME PACKAGE) 'PACKAGE)))))
  NIL)

;;; Return T if the buffer has been changed since the last time it
;;; was read or written.  If it never has been read or written, ERROR.
(DEFUN BUFFER-MUNGED-P (BUFFER &OPTIONAL SPECIAL-TOO)
  (AND (SYMBOLP (BUFFER-FILE-ID BUFFER))
       (FERROR NIL "BUFFER-MUNGED-P on a file that has not been read or written."))
  (AND (OR SPECIAL-TOO (NLISTP (BUFFER-FILE-ID BUFFER))
	   (NEQ (CAR (BUFFER-FILE-ID BUFFER)) ':SPECIAL-BUFFER))
       (NEQ (NODE-TICK BUFFER) ':READ-ONLY)
       (> (NODE-TICK BUFFER) (BUFFER-TICK BUFFER))))

;;; Return T is a buffer should be offered for saving.  If it has been touched since it
;;; was last read or created (if it was a (New File)).
(DEFUN BUFFER-NEEDS-SAVING-P (BUFFER &OPTIONAL SPECIAL-TOO)
  (AND (BUFFER-FILE-ID BUFFER)
       (OR SPECIAL-TOO (NLISTP (BUFFER-FILE-ID BUFFER))
	   (NEQ (CAR (BUFFER-FILE-ID BUFFER)) ':SPECIAL-BUFFER))
       (NEQ (NODE-TICK BUFFER) ':READ-ONLY)
       (> (NODE-TICK BUFFER) (BUFFER-TICK BUFFER))))

(DEFUN PATHNAME-DEFAULTS (&OPTIONAL (DEFAULTS *PATHNAME-DEFAULTS*))
  (AND (TYPEP *INTERVAL* 'FILE-BUFFER)
       (FS:SET-DEFAULT-PATHNAME (IF (BUFFER-FILE-ID *INTERVAL*)
				    (BUFFER-PATHNAME *INTERVAL*)
				    (FUNCALL (FS:DEFAULT-PATHNAME DEFAULTS)
					     ':NEW-NAME (BUFFER-NAME *INTERVAL*)))
				DEFAULTS))
  DEFAULTS)

(DEFUN DEFAULT-PATHNAME (&OPTIONAL (DEFAULTS *PATHNAME-DEFAULTS*))
  (FS:DEFAULT-PATHNAME (PATHNAME-DEFAULTS DEFAULTS)))

(DEFUN MAKE-BUFFER-WINDOW-OR-BROADCAST-STREAM (BUFFER-NAME CONCATENATE-P &AUX BUFFER)
  (SETQ BUFFER (FIND-BUFFER-NAMED BUFFER-NAME T))
  (OR CONCATENATE-P (DELETE-INTERVAL BUFFER))
  (DO ((WINDOWS (SYMEVAL-IN-INSTANCE *ZMACS-COMMAND-LOOP* '*WINDOW-LIST*) (CDR WINDOWS)))
      ((NULL WINDOWS)
       (LET ((ISTREAM (INTERVAL-STREAM BUFFER)))
	 (FUNCALL ISTREAM ':SET-BP (IF (EQ CONCATENATE-P ':POINT) (BUFFER-SAVED-POINT BUFFER)
				       (INTERVAL-LAST-BP BUFFER)))
	 (VALUES (MAKE-BROADCAST-STREAM ISTREAM STANDARD-OUTPUT) ISTREAM)))
    (AND (EQ (WINDOW-INTERVAL (CAR WINDOWS)) BUFFER)
	 (WINDOW-EXPOSED-P (CAR WINDOWS))
	 (LET ((WSTREAM (MAKE-EDITOR-STREAM-FROM-WINDOW (CAR WINDOWS))))
	   (SETF (WINDOW-REDISPLAY-DEGREE (CAR WINDOWS))
		 (MAX (WINDOW-REDISPLAY-DEGREE (CAR WINDOWS)) DIS-TEXT))
	   (OR (EQ CONCATENATE-P ':POINT)
	       (MOVE-BP (SYMEVAL-IN-INSTANCE WSTREAM '*STREAM-BP*)
			(INTERVAL-LAST-BP BUFFER)))
	   (RETURN WSTREAM WSTREAM)))))

(DEFUN MAKE-FILE-BUFFER-STREAM (PATHNAME &OPTIONAL (CONCATENATE-P T)
					 &AUX BUFFER ISTREAM)
  (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME *PATHNAME-DEFAULTS*))
  (SETQ BUFFER (FIND-BUFFER-NAMED PATHNAME T))
  (IF (BUFFER-FILE-ID BUFFER)
      (OR CONCATENATE-P (DELETE-INTERVAL BUFFER))
      (SET-BUFFER-FILE-ID BUFFER T)
      (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)
      (LET ((GENERIC-PATHNAME (FUNCALL PATHNAME ':GENERIC-PATHNAME)))
	(SETF (BUFFER-GENERIC-PATHNAME BUFFER) GENERIC-PATHNAME)
	(INITIALIZE-GENERIC-PATHNAME GENERIC-PATHNAME)))
  (SETQ ISTREAM (INTERVAL-STREAM BUFFER))
  (FUNCALL ISTREAM ':SET-BP (IF (EQ CONCATENATE-P ':POINT) (BUFFER-SAVED-POINT BUFFER)
				(INTERVAL-LAST-BP BUFFER)))
  ISTREAM)

;;; The commands.

(DEFCOM COM-FIND-FILE "Visits a file in its own buffer.
Reads in a filename from the minibuffer.  If the file is already
in a buffer, selects that buffer.  Otherwise creates a buffer
whose name is the name of the file, reads the file into
that buffer and selects it." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Find file:" (PATHNAME-DEFAULTS)
					   NIL NIL ':NEW-OK)))
    (FIND-FILE PATHNAME))
  (MAYBE-DISPLAY-DIRECTORY ':READ)
  DIS-TEXT)

;;; This should only be used when the user spazzes from a C-X C-F
(DEFCOM COM-VISIT-FILE "Visit a file in the current buffer.
Use this if you give the wrong filename in C-X C-F and it fails or
you get a nonexistent file.
It is also allowed on a buffer which is not associated with any file.
Then, the specified file is read in but not associated with the buffer.
This is for reading in old versions of files, etc." ()
  (OR (SYMBOLP (BUFFER-FILE-ID *INTERVAL*))
      (BARF "This buffer is already editing a real file"))
  (OR (BP-= (INTERVAL-FIRST-BP *INTERVAL*) (INTERVAL-LAST-BP *INTERVAL*))
      (BARF "This buffer is not empty"))
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Visit file:" (PATHNAME-DEFAULTS)
					   NIL NIL ':NEW-OK)))
    (AND (BUFFER-FILE-ID *INTERVAL*)
	 (FIND-BUFFER-NAMED PATHNAME)
	 (BARF "~A is already being edited in another buffer" PATHNAME))
    (REVERT-BUFFER *INTERVAL* PATHNAME))
  (COMPUTE-BUFFER-PACKAGE *INTERVAL*)
  (LET ((NEW-MODE (BUFFER-SAVED-MAJOR-MODE *INTERVAL*)))
    (TURN-OFF-MODE *MAJOR-MODE*)
    (DOLIST (MODE *UNSTICKY-MINOR-MODES*)
      (TURN-OFF-MODE MODE))
    (TURN-ON-MODE NEW-MODE))
  (COND ((BUFFER-FILE-ID *INTERVAL*)
	 (LET ((NAME (BUFFER-NAME *INTERVAL*)))
	   (LET ((ELEM (RASSOC *INTERVAL* *ZMACS-BUFFER-NAME-ALIST*)))
	     (AND ELEM (SETF (CAR ELEM) NAME)))
	   (SETQ *ZMACS-BUFFER-NAME* NAME))
	 (CHANGE-WINDOW-LABEL *WINDOW*)))
  (MAYBE-DISPLAY-DIRECTORY ':READ)
  DIS-TEXT)

;; This does the real work for C-X C-F.  Given a pathname, it finds or makes
;; a buffer for the file and selects it, unless SELECT-P is NIL
(DEFUN FIND-FILE (PATHNAME &OPTIONAL (SELECT-P T) &AUX BUFFER STREAM OLD-DESC NEW-DESC)
  (SETQ BUFFER (FIND-BUFFER-NAMED PATHNAME))
  (OR BUFFER (SETQ BUFFER (CREATE-BUFFER NIL)))
  (IF (NULL (BUFFER-FILE-ID BUFFER))
      (REVERT-BUFFER BUFFER PATHNAME T)
      (COND ((STRINGP (SETQ STREAM (OPEN PATHNAME '(:PROBE))))
	     (AND (NOT (SYMBOLP (BUFFER-FILE-ID BUFFER)))
		  (TYPEIN-LINE "Note: File ~A has been deleted." PATHNAME)))
	    ((MULTIPLE-VALUE (NEW-DESC OLD-DESC)
	       (STREAM-CHECK-FILE-ID STREAM (BUFFER-FILE-ID BUFFER)))
	     (BEEP)
	     (COND ((BUFFER-MUNGED-P BUFFER)
		    (FORMAT T
			    "Since you last read or wrote ~A
 (~A),
while you've been editing,someone has written a new copy out 
 (~A).
You will lose some work if you are not careful.
I will leave you your old copy instead of reading the new one.
I suggest that you file this out under a different name and then SRCCOM the two files.
Do M-X Revert if you really want the new one.~%" PATHNAME OLD-DESC NEW-DESC))
		   (T
		    (FORMAT T
			    "Since you last read or wrote ~A 
 (~A),
someone else wrote a new version on disk 
 (~A).
Luckily, you haven't edited the buffer since then.
Your old copy is still in the buffer.  " PATHNAME OLD-DESC NEW-DESC)
		     (COND ((TYPEOUT-YES-OR-NO-P
			      "Do you want the new version instead? ")
			    (REVERT-BUFFER BUFFER))))))))
  (PUSH* BUFFER *ZMACS-BUFFER-LIST*)
  (COND (SELECT-P (MAKE-BUFFER-CURRENT BUFFER))
	((RASSOC BUFFER *ZMACS-BUFFER-NAME-ALIST*))
	(T (PUSH (CONS (BUFFER-NAME BUFFER) BUFFER) *ZMACS-BUFFER-NAME-ALIST*)))
  (VALUES NIL BUFFER))

;;; Given a stream and an old file-id, check that the file has not changed.
;;; If it has, return two strings, one describing the new file and one the
;;; old file.
(DEFUN STREAM-CHECK-FILE-ID (STREAM FILE-ID &AUX FILE-FILE-ID)
  (COND ((SYMBOLP FILE-ID) NIL)
	((EQUAL FILE-ID (SETQ FILE-FILE-ID (FUNCALL STREAM ':INFO))) NIL)
	(T
	 (VALUES (DESCRIBE-FILE-ID FILE-FILE-ID)
		 (DESCRIBE-FILE-ID FILE-ID)))))

(DEFUN DESCRIBE-FILE-ID (FILE-ID)
  (FORMAT NIL "~A, created ~@[by ~A at ~]~\TIME\"
	  (CAR FILE-ID) NIL (CDR FILE-ID)))

(DEFCOM COM-SECTIONIZE-BUFFER "Reparse a buffer for definitions.
Repeat the processing normally done only when the file is visited
which finds the definitions in the file so that M-. can work.
This is useful if you have added functions to the file." ()
  (LET ((BUFFER (READ-BUFFER-NAME "Sectionize Buffer:"
		     *INTERVAL*               ;Default is current buffer.
		     NIL)))
    (MAKE-BUFFER-CURRENT BUFFER)
    (SECTIONIZE-BUFFER BUFFER))
  DIS-TEXT)

(DEFCOM COM-SELECT-BUFFER "Select the specified buffer.
Does a completing read of the buffer name in the echo area.
With a numeric argument, allows you to create a new buffer." ()
  (SELECT-BUFFER "Select buffer:" (IF *NUMERIC-ARG-P* T 'MAYBE)))

(DEFUN SELECT-BUFFER (PROMPT ALLOW-CREATE-NEW)
  (MAKE-BUFFER-CURRENT (READ-BUFFER-NAME PROMPT T ALLOW-CREATE-NEW))
  DIS-TEXT)

;; Find a buffer, by asking the user the name.  If the buffer is not found,
;; action depends on IMPOSSIBLE-IS-OK-P.  If IMPOSSIBLE-IS-OK-P is NIL, it
;; is an error (BARF is called).  Otherwise the buffer is created.
;; If the user types a null string, DEFAULT is returned.
(DEFUN READ-BUFFER-NAME (PROMPT DEFAULT &OPTIONAL IMPOSSIBLE-IS-OK-P)
  (AND (EQ DEFAULT T)	; Select most recent buffer other than this one
       (SETQ DEFAULT (DOLIST (BUF *ZMACS-BUFFER-HISTORY*)
		       (OR (EQ BUF *INTERVAL*)
			   (RETURN BUF)))))
  (AND DEFAULT
       (SETQ PROMPT (STRING-APPEND PROMPT
				   " ("
				   (BUFFER-NAME DEFAULT)
				   ")")))
  (LET* ((*READ-BUFFER-KLUDGE* T)
	 (NAME (COMPLETING-READ-FROM-MINI-BUFFER PROMPT *ZMACS-BUFFER-NAME-ALIST*
						 IMPOSSIBLE-IS-OK-P)))
    (COND ((EQUAL NAME "") (SETQ NAME DEFAULT))
	  ((LISTP NAME) (SETQ NAME (CDR NAME))))		;Existing buffer
    (COND ((NULL NAME) (BARF))
	  ((STRINGP NAME)
	   (TYPEIN-LINE "(New Buffer)")
	   (SETQ NAME (CREATE-ONE-BUFFER-TO-GO NAME)))
	  (T
	   (TYPEIN-LINE "")))
    NAME))	;which by now is a buffer

(DEFCOM COM-SELECT-PREVIOUS-BUFFER "Select the previously selected buffer.
A numeric argument selects the argth previous buffer (the default argument
is 2).  With an argument of 1, rotates the entire buffer history, and
a negative argument rotates the other way." ()
  (ROTATE-BUFFER-HISTORY (IF (MEMQ *NUMERIC-ARG-P* '(:SIGN NIL))
			     (* 2 *NUMERIC-ARG*) *NUMERIC-ARG*)))

(DEFVAR *DEFAULT-PREVIOUS-BUFFER-ARG* 3)
(DEFCOM COM-SELECT-DEFAULT-PREVIOUS-BUFFER "Rotate the stack of previously selected buffers.
A numeric argument specifies the number of entries to rotate, and sets the new default." ()
  (OR (MEMQ *NUMERIC-ARG-P* '(:SIGN NIL))
      (SETQ *DEFAULT-PREVIOUS-BUFFER-ARG* *NUMERIC-ARG*))
  (ROTATE-BUFFER-HISTORY (IF (EQ *NUMERIC-ARG-P* ':SIGN)
			     (* *NUMERIC-ARG* *DEFAULT-PREVIOUS-BUFFER-ARG*)
			     *DEFAULT-PREVIOUS-BUFFER-ARG*)))

(DEFCOM COM-WRITE-FILE "Write out the buffer to the specified file." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Write File:" (PATHNAME-DEFAULTS)
					   NIL NIL ':WRITE)))
    (SET-BUFFER-PATHNAME PATHNAME)
    (WRITE-FILE-INTERNAL PATHNAME))
  (MAYBE-DISPLAY-DIRECTORY ':WRITE)
  DIS-NONE)

(DEFUN SET-BUFFER-PATHNAME (PATHNAME &OPTIONAL (BUFFER *INTERVAL*) &AUX STRING)
  (SETQ STRING (FUNCALL PATHNAME ':STRING-FOR-EDITOR))
  (COND ((NOT (EQUAL (BUFFER-NAME BUFFER) STRING))
	 (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)
	 (SETF (BUFFER-NAME BUFFER) STRING)
	 (UPDATE-BUFFER-NAMES BUFFER STRING)
	 (DOLIST (WINDOW *WINDOW-LIST*)
	   (AND (EQ (WINDOW-INTERVAL WINDOW) BUFFER)
		(CHANGE-WINDOW-LABEL WINDOW)))
	 (LET ((GENERIC-PATHNAME (FUNCALL PATHNAME ':GENERIC-PATHNAME))
	       (PRESENT-PACKAGE (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER) ':GET ':PACKAGE)))
	   (OR (FUNCALL GENERIC-PATHNAME ':GET ':PACKAGE)
	       (NULL PRESENT-PACKAGE)
	       (FUNCALL GENERIC-PATHNAME ':PUTPROP PRESENT-PACKAGE ':PACKAGE))
	   (SETF (BUFFER-GENERIC-PATHNAME BUFFER) GENERIC-PATHNAME)))))

(DEFCOM COM-SAVE-FILE "Write out changes to current file.
If the current buffer has no file, reads in a file name from the mini buffer." ()
  (COND ((AND (NOT (SYMBOLP (BUFFER-FILE-ID *INTERVAL*)))
	      (NOT (BUFFER-MUNGED-P *INTERVAL*)))
	 (TYPEIN-LINE "(No changes need to be written.)")
	 DIS-NONE)
	(T
	 (SAVE-BUFFER *INTERVAL*)
	 (MAYBE-DISPLAY-DIRECTORY ':WRITE)
	 DIS-NONE)))

(DEFCOM COM-SAVE-ALL-FILES "Offer to write out any changed buffers.
A numeric argument causes the query to be skipped." ()
  (LET ((QUERY-IO *TYPEOUT-WINDOW*))
    (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
      (SAVE-BUFFER-IF-NECESSARY BUFFER (NOT *NUMERIC-ARG-P*)))
    (COM-SAVE-WORD-ABBREV-FILE-INTERNAL ':ASK))
  (AND *WINDOW* (FORMAT T "~&Done.~%"))
  DIS-NONE)

(DEFUN SAVE-BUFFER-IF-NECESSARY (BUFFER &OPTIONAL CONFIRM)
  (AND (BUFFER-NEEDS-SAVING-P BUFFER)
       (OR (NOT CONFIRM)
	   (FQUERY '(:SELECT T) "Save file ~A ? " (BUFFER-NAME BUFFER)))
       (SAVE-BUFFER BUFFER)))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Save" SAVE-BUFFER NIL
			  "Save this buffer.")

(DEFUN SAVE-BUFFER (BUFFER &AUX FILE-ID PATHNAME)
  (SETQ FILE-ID (BUFFER-FILE-ID BUFFER)  
	PATHNAME (BUFFER-PATHNAME BUFFER))
  (COND ((NULL FILE-ID)
	 (FS:SET-DEFAULT-PATHNAME (FUNCALL (DEFAULT-PATHNAME) ':NEW-NAME (BUFFER-NAME BUFFER))
				  *PATHNAME-DEFAULTS*)
	 (SETQ PATHNAME (READ-DEFAULTED-PATHNAME "Save File:" (PATHNAME-DEFAULTS)
						 NIL NIL ':WRITE))
	 (SET-BUFFER-PATHNAME PATHNAME BUFFER)))
  (AND (OR (SYMBOLP FILE-ID)
	   (EQUAL FILE-ID (WITH-OPEN-FILE (S PATHNAME '(:PROBE :ASCII))
			    (AND (NOT (STRINGP S)) (FUNCALL S ':INFO))))
	   (FQUERY '#,`(:SELECT T
			:BEEP T
		        :TYPE READLINE
		        :CHOICES ,FORMAT:YES-OR-NO-P-CHOICES)
		   "~A has been changed on disk since you last read or wrote it.~@
		    Save it anyway? "
		   PATHNAME))
       (WRITE-FILE-INTERNAL PATHNAME BUFFER))
  T)

(DEFUN WRITE-FILE-INTERNAL (PATHNAME &OPTIONAL (BUFFER *INTERVAL*))
  (WITH-OPEN-FILE (STREAM PATHNAME '(OUT))
    (STREAM-OUT-INTERVAL STREAM BUFFER NIL T
			 (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER) ':GETL '(:FONTS :DIAGRAM)))
    (CLOSE STREAM)
    (SET-BUFFER-FILE-ID BUFFER (FUNCALL STREAM ':INFO))
    (SETF (BUFFER-TICK BUFFER) (TICK))
    (TYPEIN-LINE "Written: ~A" (FUNCALL STREAM ':TRUENAME))))

(DEFUN SET-BUFFER-FILE-ID (BUFFER INFO)
  (SETF (BUFFER-FILE-ID BUFFER) INFO)
  (LET ((VERSION-STRING (AND (LISTP INFO)
			     (TYPEP (CAR INFO) 'FS:PATHNAME)
			     (BUFFER-PATHNAME BUFFER)
			     (NOT (NUMBERP (FUNCALL (BUFFER-PATHNAME BUFFER) ':VERSION)))
			     (LET ((VERSION (FUNCALL (CAR INFO) ':VERSION)))
			       (AND (NUMBERP VERSION) (FORMAT NIL " (~D)" VERSION))))))
    (SETF (BUFFER-VERSION-STRING BUFFER) VERSION-STRING)
    (AND (EQ BUFFER *INTERVAL*) (SETQ *ZMACS-BUFFER-VERSION-STRING* VERSION-STRING)))
  INFO)

;;; This can be called from top-level to try to save a bombed ZMACS
(DEFUN SAVE-ALL-FILES ()
  (LET ((*WINDOW* NIL)
	(*TYPEOUT-WINDOW* STANDARD-OUTPUT)
	(*TYPEIN-WINDOW* STANDARD-OUTPUT)
	(*NUMERIC-ARG-P* NIL))
    (COM-SAVE-ALL-FILES)))

(DEFCOM COM-REVERT-BUFFER "Forgets changes to a specified buffer.
Reads the name of the buffer from the mini-buffer and reads back in the file
or function." ()
  (LET ((BUFFER (READ-BUFFER-NAME "Buffer to revert:" *INTERVAL*)))
    (REVERT-BUFFER BUFFER)
    (IF (EQ BUFFER *INTERVAL*)
	DIS-TEXT
	(LET ((WINDOW (DOLIST (WINDOW *WINDOW-LIST*)
			(AND (EQ (WINDOW-INTERVAL WINDOW) BUFFER)
			     (RETURN WINDOW)))))
	  (AND WINDOW (MUST-REDISPLAY WINDOW DIS-TEXT)))
	DIS-NONE)))

;; Read a file into a buffer, or read back in the file that was in the buffer.
;; Connect-flag means mark the buffer as associated with the file
;; (including changing its name).  Otherwise, leave buffer name alone
;; and keep it marked as not associated with a file, although the major mode
;; and properties are processed.  FIND-FILE calls this with CONNECT-FLAG = T,
;; and VISIT-FILE sometimes calls with CONNECT-FLAG = NIL.
;; Think of CONNECT-FLAG as saying whether BUFFER-FILE-ID should become non-NIL.
;; The default for CONNECT-FLAG is the old BUFFER-FILE-ID;  ie, things stay the same.
(DEFUN REVERT-BUFFER (BUFFER &OPTIONAL (PATHNAME (BUFFER-PATHNAME BUFFER))
				       (CONNECT-FLAG (BUFFER-FILE-ID BUFFER))
                             &AUX GENERIC-PATHNAME PATHNAME-ACTOR PATHNAME-STRING TRUENAME)
  (COND ((AND (NULL (BUFFER-FILE-ID BUFFER)) (NULL PATHNAME))
	 (BARF "The buffer ~A is not associated with a file." (BUFFER-NAME BUFFER))))
  (WITH-OPEN-FILE (STREAM PATHNAME '(:READ :NOERROR))
    (MULTIPLE-VALUE (PATHNAME-ACTOR PATHNAME-STRING)
      (EDITOR-FILE-NAME PATHNAME))
    (COND ((NOT (STRINGP STREAM))
	   (SETQ TRUENAME (FUNCALL STREAM ':TRUENAME))
	   (AND (MEMQ (FUNCALL PATHNAME-ACTOR ':TYPE) '(NIL :UNSPECIFIC))
		(MULTIPLE-VALUE (PATHNAME-ACTOR PATHNAME-STRING)
		  (EDITOR-FILE-NAME (FUNCALL PATHNAME-ACTOR ':NEW-TYPE
					     (FUNCALL TRUENAME ':TYPE)))))))
    (COND (CONNECT-FLAG
	   (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
	   (SETF (BUFFER-PATHNAME BUFFER) PATHNAME-ACTOR)))
    (SETQ PATHNAME PATHNAME-ACTOR
	  GENERIC-PATHNAME (FUNCALL PATHNAME-ACTOR ':GENERIC-PATHNAME))
    (SETF (BUFFER-GENERIC-PATHNAME BUFFER) GENERIC-PATHNAME)
    (COND ((NOT (STRINGP STREAM))
	   (TYPEIN-LINE "Reading ~A" TRUENAME)
	   (LET ((THIS-VERSION (FUNCALL TRUENAME ':VERSION))
		 (INSTALLED-TRUENAME (FILE-LOADED-TRUENAME TRUENAME))
		 INSTALLED-VERSION)
	     (AND INSTALLED-TRUENAME
		  (NUMBERP THIS-VERSION)
		  (NUMBERP (SETQ INSTALLED-VERSION (FUNCALL INSTALLED-TRUENAME ':VERSION)))
		  ( INSTALLED-VERSION THIS-VERSION)
		  (TYPEIN-LINE-MORE " (installed version is ~D)" INSTALLED-VERSION)))
	   (DOLIST (PROP *MODE-LINE-PROPERTIES*)
	     (FUNCALL GENERIC-PATHNAME ':REMPROP PROP))
	   (FS:FILE-READ-PROPERTY-LIST GENERIC-PATHNAME STREAM)
	   (AND (TYPEP BUFFER 'BUFFER)
		(LET ((MODE (GET-FILE-MAJOR-MODE GENERIC-PATHNAME)))
		  (AND MODE (SETF (BUFFER-SAVED-MAJOR-MODE BUFFER) MODE))))
	   (PRESERVE-BUFFER-POINT (BUFFER)
	    (DELETE-INTERVAL BUFFER)
	    (SETF (BUFFER-TICK BUFFER) (TICK))	;For SECTIONIZE-BUFFER
	    (LET ((FONTS (SET-BUFFER-FONTS-FROM-FILE BUFFER GENERIC-PATHNAME))
		  FONTS-P)
	      (SETQ FONTS-P (OR FONTS (FUNCALL GENERIC-PATHNAME ':GET ':DIAGRAM)))
	      (COND (CONNECT-FLAG
		     (SECTIONIZE-BUFFER BUFFER STREAM FONTS-P)
		     (SET-BUFFER-FILE-ID BUFFER (FUNCALL STREAM ':INFO))
		     (AND FONTS (REDEFINE-FONTS *WINDOW* FONTS))
		     (REDEFINE-WINDOW-BACKSPACE-FLAG *WINDOW*
						     (FUNCALL GENERIC-PATHNAME
							      ':GET ':BACKSPACE))
		     (REDEFINE-WINDOW-TAB-NCHARS *WINDOW*
						 (OR (FUNCALL GENERIC-PATHNAME
							      ':GET ':TAB-WIDTH)
						     8)))
		    (T (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)))))
	   (COND ((AND (EQ BUFFER *INTERVAL*) (TYPEP BUFFER 'BUFFER))
		  (TURN-OFF-MODE *MAJOR-MODE*)
		  (TURN-ON-MODE (BUFFER-SAVED-MAJOR-MODE BUFFER)))))
	  (T
	   (MULTIPLE-VALUE-BIND (ERR NIL MSG)
	       (FS:FILE-PROCESS-ERROR STREAM PATHNAME NIL T)
	     (COND ((STRING-EQUAL ERR "FNF")	;The error was "File Not Found" or something
						;similar.  All errors of this type should give
						;the "FNF" string even if their user string
						;if different.
		    (TYPEIN-LINE "(New File)")
		    (DELETE-INTERVAL BUFFER)
		    (AND CONNECT-FLAG (SET-BUFFER-FILE-ID BUFFER T))
		    (INITIALIZE-GENERIC-PATHNAME GENERIC-PATHNAME
						 (FUNCALL PATHNAME-ACTOR ':TYPE))
		    (LET ((MODE (GET-FILE-MAJOR-MODE GENERIC-PATHNAME)))
		      (AND MODE (SETF (BUFFER-SAVED-MAJOR-MODE BUFFER) MODE))))
		   (T (BARF "Error: ~A" MSG)))))))
  (SETF (BUFFER-TICK BUFFER) (TICK)))		;Buffer is same as file

(DEFUN FILE-LOADED-TRUENAME (PATHNAME)
  (OR (LET* ((GENERIC-PATHNAME (FUNCALL PATHNAME ':GENERIC-PATHNAME))
	     (SOURCE-PATHNAME (FUNCALL GENERIC-PATHNAME ':GET ':QFASL-SOURCE-FILE-UNIQUE-ID)))
	(AND (STRINGP SOURCE-PATHNAME)		;Old versions of the compiler
	     (SETQ SOURCE-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS SOURCE-PATHNAME PATHNAME)))
	(AND (NOT (NULL SOURCE-PATHNAME))
	     (LET ((TYPE-1 (FUNCALL SOURCE-PATHNAME ':TYPE))
		   (TYPE-2 (FUNCALL PATHNAME ':TYPE)))
	       (OR (EQUAL TYPE-1 TYPE-2)
		   (AND (OR (EQ TYPE-1 ':UNSPECIFIC)
			    (MEMBER TYPE-1 FS:*ITS-UNINTERESTING-TYPES*))
			(OR (EQ TYPE-2 ':UNSPECIFIC)
			    (MEMBER TYPE-2 FS:*ITS-UNINTERESTING-TYPES*)))))
	     SOURCE-PATHNAME))
      (LET* ((NEWEST-PATHNAME (FUNCALL PATHNAME ':NEW-VERSION ':NEWEST))
	     (ID (SI:GET-FILE-LOADED-ID NEWEST-PATHNAME PACKAGE)))
	(AND ID (CAR ID)))))

(DEFCOM COM-REPARSE-MODE-LINE "Look at the -*- line again" ()
  (LET ((GENERIC-PATHNAME (BUFFER-GENERIC-PATHNAME *INTERVAL*)))
    (DOLIST (PROP *MODE-LINE-PROPERTIES*)
      (FUNCALL GENERIC-PATHNAME ':REMPROP PROP))
    (FS:FILE-READ-PROPERTY-LIST GENERIC-PATHNAME (INTERVAL-STREAM *INTERVAL*))
    (COMPUTE-BUFFER-PACKAGE *INTERVAL*)
    (LET ((MODE (GET-FILE-MAJOR-MODE GENERIC-PATHNAME)))
      (TURN-OFF-MODE *MAJOR-MODE*)
      (DOLIST (MODE *UNSTICKY-MINOR-MODES*)
	(TURN-OFF-MODE MODE))
      (TURN-ON-MODE (OR MODE *DEFAULT-MAJOR-MODE*)))
    (LET ((FONTS (SET-BUFFER-FONTS-FROM-FILE *INTERVAL* GENERIC-PATHNAME)))
      (AND FONTS (REDEFINE-FONTS *WINDOW* FONTS)))
    (REDEFINE-WINDOW-BACKSPACE-FLAG *WINDOW* (FUNCALL GENERIC-PATHNAME ':GET ':BACKSPACE))
    (REDEFINE-WINDOW-TAB-NCHARS *WINDOW* (OR (FUNCALL GENERIC-PATHNAME ':GET ':TAB-WIDTH) 8)))
  DIS-NONE)

(DEFCOM COM-NOT-MODIFIED "Pretend that this buffer has not been modified." ()
  (COND (NIL;(NULL (BUFFER-FILE-ID *INTERVAL*))
	 (BARF "This buffer is not being used to edit a file."))
	(T
	 (SETF (BUFFER-TICK *INTERVAL*) (TICK))
	 ;; Is this right?
	 (DOLIST (SECTION (NODE-INFERIORS *INTERVAL*))
	   (AND (TYPEP SECTION 'SECTION-NODE)
		(SETF (SECTION-NODE-COMPILE-TICK SECTION) *TICK*)))
	 (TYPEIN-LINE "Not modified")))
  DIS-NONE)

(DEFCOM COM-LIST-BUFFERS "Print a list of the all buffers and their files (or sizes)." ()
  (SETQ *ZMACS-BUFFER-LIST* (SORT *ZMACS-BUFFER-LIST*
				  #'(LAMBDA (BUF1 BUF2)
				      (< (OR (FIND-POSITION-IN-LIST BUF1
								    *ZMACS-BUFFER-HISTORY*)
					     177777)
					 (OR (FIND-POSITION-IN-LIST BUF2
								    *ZMACS-BUFFER-HISTORY*)
					     177777)))))
  (LET ((STAR-FLAG NIL) (PLUS-FLAG NIL) (EQV-FLAG NIL))
    (FORMAT *TYPEOUT-WINDOW* "~&Buffers in ZWEI:~%  Buffer name:~12XMode:~10XFile name:~2%")
    (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
      (LET ((FILE-ID (BUFFER-FILE-ID BUFFER))
	    PATHNAME TEM)
	(TYO (COND ((EQ FILE-ID T)
		    (SETQ PLUS-FLAG T) #/+)	;+ means new file, never written out
		   ((EQ (NODE-TICK BUFFER) ':READ-ONLY)
		    (SETQ EQV-FLAG T) #/)	; means read-only
		   ((AND FILE-ID (BUFFER-MUNGED-P BUFFER T))
		    (SETQ STAR-FLAG T) #/*)	;* means needs to be written out
		   (T #\SP))			;blank if unmodified or not a file
	     *TYPEOUT-WINDOW*)
	(TYO #\SP *TYPEOUT-WINDOW*)
        (FUNCALL *TYPEOUT-WINDOW* ':ITEM 'ZMACS-BUFFER BUFFER "~A" (BUFFER-NAME BUFFER))
        (FORMAT *TYPEOUT-WINDOW* "~26T(~A)~41T~:[~A ~*~;~*[~D Line~P]~]~%"
		(SYMEVAL (IF (EQ BUFFER *INTERVAL*) *MAJOR-MODE*
			     (BUFFER-SAVED-MAJOR-MODE BUFFER)))
		(NOT (AND FILE-ID (SETQ PATHNAME (BUFFER-PATHNAME BUFFER))))
		(AND PATHNAME (FUNCALL PATHNAME ':STRING-FOR-PRINTING))
		(AND (NULL PATHNAME)
		     (SETQ TEM (COUNT-LINES-BUFFER BUFFER)))
		TEM)))
    (TERPRI *TYPEOUT-WINDOW*) ;extra TERPRI to show you that it's finished.
    (AND PLUS-FLAG (PRINC "+ means new file.  " *TYPEOUT-WINDOW*))
    (AND STAR-FLAG (PRINC "* means modified file.  " *TYPEOUT-WINDOW*))
    (AND EQV-FLAG (PRINC " means read-only.  " *TYPEOUT-WINDOW*))
    (AND (OR PLUS-FLAG STAR-FLAG EQV-FLAG) (TERPRI *TYPEOUT-WINDOW*))
    DIS-NONE))

(DEFUN COUNT-LINES-BUFFER (BUFFER &AUX TICK TEM LINES)
  (COND ((AND (NEQ (SETQ TICK (NODE-TICK BUFFER)) ':READ-ONLY)
	      (SETQ TEM (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER) ':GET 'LAST-LINE-COUNT))
	      (= (CADR TEM) TICK))
	 (CAR TEM))
	(T
	 (SETQ LINES (COUNT-LINES (INTERVAL-FIRST-BP BUFFER)
				  (INTERVAL-LAST-BP BUFFER) T)
	       TEM (LIST LINES TICK))
	 (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER) ':PUTPROP TEM 'LAST-LINE-COUNT)
	 LINES)))

(DEFCOM COM-KILL-OR-SAVE-BUFFERS "Put up a choice window for saving modified files" ()
  (LET ((BUFFER-ALIST (DO ((BUFFER-LIST *ZMACS-BUFFER-LIST* (CDR BUFFER-LIST))
			   (RET NIL) (TEM)
			   (BUFFER) (FILE-ID))
			  ((NULL BUFFER-LIST) RET)
			(SETQ BUFFER (CAR BUFFER-LIST))
			(SETQ TEM (STRING-APPEND "  " (BUFFER-NAME BUFFER))
			      FILE-ID (BUFFER-FILE-ID BUFFER))
			(ASET (COND ((EQ FILE-ID T)
				     #/+)
				    ((EQ (NODE-TICK BUFFER) ':READ-ONLY)
				     #/)
				    ((AND FILE-ID (BUFFER-MUNGED-P BUFFER))
				     #/*)
				    (T
				     #\SP))
			      TEM 0)
			(PUSH (LIST BUFFER TEM (IF (BUFFER-NEEDS-SAVING-P BUFFER)
						   '((:SAVE T) :KILL :NOT-MODIFIED)
						   '(:SAVE :KILL :NOT-MODIFIED)))
			      RET)))
	CHOICES)
    (SETQ BUFFER-ALIST
	  (SORT BUFFER-ALIST #'(LAMBDA (X Y &AUX STR1 STR2 CH1 CH2)
				 (IF (= (SETQ CH1 (AREF (SETQ STR1 (CADR X)) 0))
					(SETQ CH2 (AREF (SETQ STR2 (CADR Y)) 0)))
				     (STRING-LESSP STR1 STR2)
				     (< (SELECTQ CH1
					  (#/* 0)
					  (#/+ 1)
					  (#\SP 2)
					  (#/ 3))
					(SELECTQ CH2
					  (#/* 0)
					  (#/+ 1)
					  (#\SP 2)
					  (#/ 3)))))))
    (SETQ CHOICES (TV:MULTIPLE-CHOOSE "  Buffer" BUFFER-ALIST
				      '((:SAVE "Save") (:KILL "Kill")
					(:NOT-MODIFIED "UnMod"))))
    ;; Make sure the current buffer gets done last
    (LET ((ELEM (ASSQ *INTERVAL* CHOICES)))
      (AND ELEM (SETQ CHOICES (NCONC (DELQ ELEM CHOICES) (NCONS ELEM)))))
    (DOLIST (CHOICE CHOICES)
      (LET ((BUFFER (CAR CHOICE)))
	(DOLIST (KEYWORD (CDR CHOICE))
	  (SELECTQ KEYWORD
	    (:SAVE (SAVE-BUFFER BUFFER))
	    (:KILL (KILL-BUFFER BUFFER T))
	    (:NOT-MODIFIED (SETF (BUFFER-TICK BUFFER) (TICK)))
	    (OTHERWISE (FERROR NIL "~S is not a recognized option" KEYWORD))))))
    (TYPEIN-LINE "Done."))
  DIS-NONE)

(DEFCOM COM-LIST-FUNCTIONS "List the functions in a specified buffer." ()
  (LET ((BUFFER (READ-BUFFER-NAME "List functions in buffer:" *INTERVAL*)))
    (LET ((LIST (DO ((FIRST-P T NIL)
		     (SYM-LIST))
		    (NIL)
		    (AND (SETQ SYM-LIST (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER)
						 ':GET 'ZMACS-SECTION-LIST))
			 (RETURN SYM-LIST))
		    (OR FIRST-P (BARF "~A doesnt seem to have any functions in it"
				      (BUFFER-NAME BUFFER)))
		    (SECTIONIZE-BUFFER BUFFER))))
      (PROMPT-LINE "Functions in buffer ~A" (BUFFER-NAME BUFFER))
      (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FUNCTION-NAME LIST)
      (FORMAT *TYPEOUT-WINDOW* "~&Done.~%")))
  DIS-NONE)

(DEFCOM COM-KILL-BUFFER "Kill a specified buffer.
Reads the name of the buffer to kill from the mini-buffer." ()
  (LET ((BUFFER (READ-BUFFER-NAME "Buffer to kill (RETURN to kill current buffer):"
				  *INTERVAL*)))
    (KILL-BUFFER BUFFER))
  DIS-NONE)

(DEFCOM COM-KILL-SOME-BUFFERS "Offer to kill each buffer.
For each buffer, ask whether to kill it, and for each one to be killed, offer to write
out any changes." ()
  (LET ((QUERY-IO *TYPEOUT-WINDOW*))
    (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
      (AND (FQUERY '(:SELECT T) "Buffer ~A ~A, kill it? "
		   (BUFFER-NAME BUFFER)
		   (COND ((BP-= (INTERVAL-FIRST-BP BUFFER)
				(INTERVAL-LAST-BP BUFFER))
			  "is empty")
			 ((NULL (BUFFER-FILE-ID BUFFER))
			  "has no file associated with it")
			 ((EQ (BUFFER-FILE-ID BUFFER) T)
			  "is a new file")
			 ((BUFFER-MUNGED-P BUFFER)
			  "has been edited")
			 (T "is unmodified")))
	   (KILL-BUFFER BUFFER))))
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Kill" KILL-BUFFER NIL
			  "Kill this buffer.")

(DEFUN KILL-BUFFER (BUFFER &OPTIONAL NO-SAVE-P)
  ;; If the buffer is associated with a file and contains changes, offer to write it out.
  (AND (BUFFER-FILE-ID BUFFER)
       (IF (SYMBOLP (BUFFER-FILE-ID BUFFER))
	   (NOT (BP-= (INTERVAL-FIRST-BP BUFFER) (INTERVAL-LAST-BP BUFFER)))
	   (BUFFER-MUNGED-P BUFFER))
       (NOT NO-SAVE-P)
       (FQUERY `(:SELECT T
		 :BEEP T
		 :TYPE READLINE
		 :CHOICES ,FORMAT:YES-OR-NO-P-CHOICES)
	       "Buffer ~A has been modified, save it first? "
	       (BUFFER-NAME BUFFER))
       (SAVE-BUFFER BUFFER))
  ;; If buffer is current, select something else before killing.
  (AND (EQ BUFFER *INTERVAL*)
       (MUST-REDISPLAY *WINDOW*
		       (SELECT-BUFFER
			 "Killing the current buffer, select which other buffer?"
			 'MAYBE)))
  ;; Anybody who refers to this buffer should be redirected.
  (SETQ *ZMACS-BUFFER-HISTORY*
	(DELQ BUFFER *ZMACS-BUFFER-HISTORY*))
  (LET ((ELEMENT (RASSOC BUFFER *ZMACS-BUFFER-NAME-ALIST*)))
       (AND ELEMENT
	    (SETQ *ZMACS-BUFFER-NAME-ALIST*
		  (DELQ ELEMENT *ZMACS-BUFFER-NAME-ALIST*))))
  (SETQ *ZMACS-BUFFER-LIST* (DELQ BUFFER *ZMACS-BUFFER-LIST*))
  (DOLIST (SYM (FUNCALL (BUFFER-GENERIC-PATHNAME BUFFER) ':GET 'ZMACS-SECTION-LIST))
    (SETF (SI:FUNCTION-SPEC-GET SYM 'ZMACS-BUFFERS)
	  (DEL #'(LAMBDA (BUF PROP)
		   (EQ BUF (CAR PROP)))
	       BUFFER (SI:FUNCTION-SPEC-GET SYM 'ZMACS-BUFFERS))))
  (POINT-PDL-PURGE BUFFER)
  ;; Any other windows lying around should not have pointers to this window.
  ;; *WINDOW* will have been taken care of specially above.
  (DOLIST (WINDOW *WINDOW-LIST*)
    (AND (EQ (WINDOW-INTERVAL WINDOW) BUFFER)
	 (SET-WINDOW-BUFFER WINDOW (CAR *ZMACS-BUFFER-HISTORY*))))
  T)

(DEFCOM	COM-APPEND-TO-BUFFER "Append region to the specified buffer.
The name of the buffer is read from the kbd; it is created if non-existent.
With an argument, we /"prepend/" instead.  Inserts the text at that buffer's
point, but when prepending leaves the point before the inserted text." ()
  (REGION (BP1 BP2)
    (LET ((POINT) (MARK)
	  (BUFFER (READ-BUFFER-NAME
		    (IF *NUMERIC-ARG-P* "Prepend to buffer:" "Append to buffer:")
		    NIL T)))
      (COND ((EQ BUFFER *INTERVAL*)
	     (BARF "That is the current buffer.")))
      ;; Try to find a window pointing to this buffer and use its point and mark
      (DO ((WL *WINDOW-LIST* (CDR WL))
	   (W))
	  ((NULL WL)
	   (SETQ POINT (BUFFER-SAVED-POINT BUFFER)
		 MARK (BUFFER-SAVED-MARK BUFFER)))
	(COND ((EQ (WINDOW-INTERVAL (SETQ W (CAR WL))) BUFFER)
	       (SETQ POINT (WINDOW-POINT W) MARK (WINDOW-MARK W))
	       (RETURN NIL))))
      (MOVE-BP MARK (INSERT-INTERVAL POINT BP1 BP2 T))
      (OR *NUMERIC-ARG-P* (SWAP-BPS MARK POINT))
      (MUST-REDISPLAY-OTHER-WINDOWS BUFFER *WINDOW* DIS-TEXT)))
  DIS-NONE)

(DEFCOM COM-INSERT-BUFFER "Insert the specified buffer at point." ()
  (LET ((BUFFER (READ-BUFFER-NAME "Insert buffer:" T))
	(POINT (POINT)) (MARK (MARK)))
    (MOVE-BP MARK (INSERT-INTERVAL POINT BUFFER))
    (OR *NUMERIC-ARG-P* (SWAP-BPS MARK POINT)))
  DIS-TEXT)

(DEFCOM COM-SET-PACKAGE "Change the current package" ()
  (LET ((PKG (COMPLETING-READ-FROM-MINI-BUFFER
		   "Set package:"
		   (PKG-REFNAME-ALIST PKG-GLOBAL-PACKAGE))))
    (OR (STRINGP PKG) (SETQ PKG (CADR PKG)))
    (PKG-GOTO PKG))
  (FUNCALL (BUFFER-GENERIC-PATHNAME *INTERVAL*) ':PUTPROP PACKAGE 'PACKAGE)
  DIS-NONE)

(DEFCOM COM-SET-DEFAULT-FILE-NAME "Change the default filename for most file commands" ()
  (READ-DEFAULTED-PATHNAME "Set default file name:" (PATHNAME-DEFAULTS) NIL NIL ':NEW-OK)
  DIS-NONE)

(DEFCOM COM-SET-VISITED-FILE-NAME "Change the file associated with this buffer" ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Set visited file name:" (PATHNAME-DEFAULTS)
					    NIL NIL ':NEW-OK)))
    (SET-BUFFER-PATHNAME PATHNAME)
    (SET-BUFFER-FILE-ID *INTERVAL* T))
  DIS-NONE)

(DEFCOM COM-RENAME-BUFFER "Rename the current buffer" ()
  (LET ((STRING (TEMP-KILL-RING (BUFFER-NAME *INTERVAL*)
		    (TYPEIN-LINE-READLINE "Rename buffer to:"))))
    (RENAME-BUFFER *INTERVAL* STRING)
    (SETQ *ZMACS-BUFFER-NAME* STRING))
  (CHANGE-WINDOW-LABEL *WINDOW*)
  DIS-NONE)

(DEFUN RENAME-BUFFER (BUFFER NEW-NAME)
  (AND (NOT (STRING-EQUAL NEW-NAME (BUFFER-NAME BUFFER)))	    ;Allow "foo" -> "FOO"
       (FIND-BUFFER-NAMED NEW-NAME)
       (BARF "There is already another buffer named ~A" NEW-NAME))
  (LET ((ELEMENT (RASSOC BUFFER *ZMACS-BUFFER-NAME-ALIST*)))
    (AND ELEMENT (RPLACA ELEMENT NEW-NAME)))
  (SETF (BUFFER-NAME BUFFER) NEW-NAME)
  (SET-BUFFER-FILE-ID BUFFER NIL)
  (SETF (BUFFER-GENERIC-PATHNAME BUFFER) (FS:MAKE-DUMMY-PATHNAME NEW-NAME)))

(DEFCOM COM-UPDATE-MODE-LINE "Update the -*-Mode-*- line.
Update the mode line of the file, or create one if there isn't one, using
the current set of parameters.  The new mode line will always have the
Package and Mode properties, and will also have the Backspace and Fonts
properties if they have interesting values.  Other property values that
are already in the mode line will be left alone." ()
  (LET ((ALIST NIL)
	(START-BP (INTERVAL-FIRST-BP *INTERVAL*))
	LINE END-BP FLAG)
    (AND (WINDOW-BACKSPACE-OVERPRINTING-FLAG *WINDOW*)
	 (PUSH '("Backspace" T) ALIST))
    (LET ((TEM (WINDOW-TAB-NCHARS *WINDOW*)))
      (AND ( TEM 8)
	   (PUSH `("Tab-width" ,(FORMAT NIL "~D." TEM)) ALIST)))
    (AND (WINDOW-FONT-ALIST *WINDOW*)
	 (PUSH `("Fonts" ,(FORMAT NIL "~{~A~^,~}"
				  (MAPCAR 'CAR (WINDOW-FONT-ALIST *WINDOW*))))
	       ALIST))
    (AND (EQ *MAJOR-MODE* 'LISP-MODE)
	 (MULTIPLE-VALUE-BIND (VARS VALS)
	     (FS:FILE-PROPERTY-BINDINGS (BUFFER-GENERIC-PATHNAME *INTERVAL*))
	   (PROGV VARS VALS
	     (PUSH `("Base" ,(FORMAT NIL "~D." IBASE)) ALIST)
	     (PUSH `("Package" ,(PKG-NAME PACKAGE)) ALIST))))
    (PUSH `("Mode" ,(SYMEVAL *MAJOR-MODE*)) ALIST)
    (SETQ LINE (BP-LINE START-BP))
    (LET (IDX)
      (COND ((SETQ IDX (STRING-SEARCH "-*-" LINE))
	     (SETQ START-BP (CREATE-BP LINE (SETQ IDX (+ IDX 3))))
	     (SETQ IDX (STRING-SEARCH "-*-" LINE IDX))
	     (SETQ END-BP (CREATE-BP LINE IDX))
	     ;; Convert -*-LISP-*- into -*-Mode:LISP-*-
	     (COND ((NULL (STRING-SEARCH-CHAR #/: LINE (BP-INDEX START-BP) (BP-INDEX END-BP)))
		    ;; There is a mode line with no colon; presume that it is an old-style
		    ;; mode line and just contains a major mode name.
		    (INSERT START-BP "Mode: ")
		    (MOVE-BP END-BP LINE (+ IDX 6)))))
	    (T
	     (AND *COMMENT-START* (SETQ START-BP (INSERT START-BP *COMMENT-BEGIN*)))
	     (SETQ START-BP (INSERT START-BP "-*- "))
	     (INSERT START-BP " -*-
")
	     (SETQ END-BP (FORWARD-CHAR START-BP 2)))))
    (WITH-BP (THE-END-BP END-BP ':MOVES)
      (DO ((I (BP-INDEX START-BP))
	   (J) (K) (END-IDX) (ITEM))
	  (NIL)
	(SETQ END-IDX (BP-INDEX THE-END-BP))
	(OR (SETQ J (STRING-SEARCH-CHAR #/: LINE I END-IDX))
	    (RETURN NIL))
	(SETQ FLAG T)
	(SETQ K (STRING-SEARCH-CHAR #/; LINE J END-IDX))
	;; I keeps track of where we are up to.  J is where the next colon is,
	;; and K is where the next semicolon after that is.
	(COND ((SETQ ITEM (ASSOC (STRING-TRIM *BLANKS* (SUBSTRING LINE I J)) ALIST))
	       ;; The next keyword is something we recognize.
	       (SETQ I (1+ (BP-INDEX (CASE-REPLACE (CREATE-BP LINE (1+ J))
						   (CREATE-BP LINE (OR K END-IDX))
						   (STRING-DOWNCASE (CADR ITEM)) T))))
	       (SETQ ALIST (DELQ ITEM ALIST)))
	      (K
	       (SETQ I (1+ K))))
	(OR K (RETURN NIL))))
    (AND ALIST				;Still some leftovers
	 (DO LIST ALIST (CDR LIST) (NULL LIST)
	   (INSERT-MOVING START-BP (CAAR LIST))
	   (INSERT-MOVING START-BP #/:)
	   (INSERT-MOVING START-BP (CADAR LIST))
	   (AND (OR (CDR LIST) FLAG)		;If more to come or some there already
		(INSERT-MOVING START-BP "; ")))))
  ;; The user may have started out the line by editing, or have hooks like font-lock
  ;; that go off based on it.  Make everything consistent based on the new mode line.
  (COM-REPARSE-MODE-LINE)
  DIS-TEXT)

(DEFCOM COM-EDIT-ZMACS-COMMAND "Edit the function installed on a specified key." ()
  (PROMPT-LINE "Key whose command to edit: ")
  (PROMPT-LINE-ACTIVATE
    (DO ((COMTAB *COMTAB*)
	 (KEY (FUNCALL STANDARD-INPUT ':MOUSE-OR-KBD-TYI)
	      (FUNCALL STANDARD-INPUT ':TYI))
	 (COMMAND))
	(NIL)
      (PROMPT-LINE-MORE "~:@C" KEY)
      (SETQ COMMAND (COMMAND-LOOKUP KEY COMTAB))
      (COND ((AND (PREFIX-COMMAND-P COMMAND) (NOT *NUMERIC-ARG-P*))
	     (SETQ COMTAB (SYMEVAL-IN-CLOSURE COMMAND 'COMTAB))
	     (FUNCALL *MODE-LINE-WINDOW* ':TYO #\SP))
	    ((MEMQ COMMAND '(NIL :UNDEFINED))
	     (BARF "~:@C is not a defined key." KEY))
	    ((AND (SYMBOLP COMMAND) (NOT (FBOUNDP COMMAND)))
	     (BARF "~S is not implemented." COMMAND))
	    (T
	     (AND (CLOSUREP COMMAND)
		  (SETQ COMMAND (EH:FUNCTION-NAME (CAR (%MAKE-POINTER DTP-LIST COMMAND)))))
	     (FORMAT *MODE-LINE-WINDOW* " (~S)" COMMAND)
	     (RETURN (EDIT-DEFINITION COMMAND))))))
  DIS-NONE)

(DEFCOM COM-COMPILE-FILE "Compile the file you are visiting" ()
  (LET ((BUFFER (READ-BUFFER-NAME "Compile file of buffer:"
		     *INTERVAL*               ;Default is current buffer.
		     NIL)))
    (OR (BUFFER-FILE-ID BUFFER)
	(BARF "That buffer is not associated with a file"))
    (AND (OR (SYMBOLP (BUFFER-FILE-ID BUFFER)) (BUFFER-MUNGED-P BUFFER))
	 (FQUERY '(:SELECT T) "Save file ~A first? " (BUFFER-NAME BUFFER))
	 (SAVE-BUFFER BUFFER))
    (QC-FILE (BUFFER-PATHNAME BUFFER)))
  DIS-NONE)

(DEFCOM COM-MINI-VISITED-FILE "Evaluate a form having to do with the current file." ()
  (EVALUATE-MINI-BUFFER
    (FORMAT NIL "( /"~A/")"
	    (DEFAULT-PATHNAME (IF *NUMERIC-ARG-P*
				  *AUX-PATHNAME-DEFAULTS* *PATHNAME-DEFAULTS*)))
    1))

(DEFCOM COM-VIEW-BUFFER "Look at the contents of the specified buffer" ()
  (LET ((BUFFER (READ-BUFFER-NAME "View buffer:" *INTERVAL*))
	BP CH)
    (PROMPT-LINE "Viewing buffer ~A" (BUFFER-NAME BUFFER))
    (MULTIPLE-VALUE (BP CH)
      (VIEW-BUFFER BUFFER))
    (COND ((EQ CH #\CR)
	   (FUNCALL STANDARD-INPUT ':TYI)
	   (OR (EQ BUFFER *INTERVAL*) (MAKE-BUFFER-CURRENT BUFFER))
	   (MOVE-BP (POINT) BP)
	   DIS-TEXT)
	  (T
	   DIS-NONE))))

(DEFUN VIEW-BUFFER (BUFFER)
  (IF (EQ BUFFER (WINDOW-INTERVAL *WINDOW*))
      (PROG MUMBLE ()
	(WITH-BP (OLD-POINT (POINT) ':NORMAL)
	  (UNWIND-PROTECT
	    (RETURN-FROM MUMBLE (VIEW-WINDOW *WINDOW*))
	    (CLOBBER-WINDOW-LABEL *WINDOW*)
	    (MOVE-BP (POINT) OLD-POINT)
	    (MUST-REDISPLAY *WINDOW* DIS-BPS))))
      (LET ((WINDOW (CREATE-OVERLYING-WINDOW *WINDOW*)))
	(SET-WINDOW-INTERVAL WINDOW BUFFER)
	(MOVE-BP (WINDOW-POINT WINDOW) (INTERVAL-FIRST-BP BUFFER))
	(TEMPORARY-WINDOW-SELECT (WINDOW)
	  (REDISPLAY WINDOW ':POINT NIL NIL T)
	  (VIEW-WINDOW WINDOW)))))

(DEFCOM COM-BACKSPACE-OVERPRINTS "Change whether backspace overprints in text.
Explicit positive argument means yes, zero no, else toggle." (KM)
  (LET ((ON-P (COND ((NOT *NUMERIC-ARG-P*)
		     (NOT (FUNCALL (BUFFER-GENERIC-PATHNAME *INTERVAL*) ':GET ':BACKSPACE)))
		    ((ZEROP *NUMERIC-ARG*) NIL)
		    (T T))))
    (FUNCALL (BUFFER-GENERIC-PATHNAME *INTERVAL*) ':PUTPROP ON-P ':BACKSPACE)
    (REDEFINE-WINDOW-BACKSPACE-FLAG *WINDOW* ON-P))
  DIS-NONE)

(DEFCOM COM-SET-TAB-WIDTH "Set the number of space characters displayed for a tab.
Takes numberic argument or none for 8" (KM)
  (LET ((N (IF *NUMERIC-ARG-P* *NUMERIC-ARG* 8)))
    (FUNCALL (BUFFER-GENERIC-PATHNAME *INTERVAL*) ':PUTPROP N ':TAB-WIDTH)
    (REDEFINE-WINDOW-TAB-NCHARS *WINDOW* N))
  DIS-NONE)

(DEFCOM COM-PRINT-MODIFICATIONS "Show lines that have changed since file was last saved" ()
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
       (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
       (BUFFER-TICK (BUFFER-TICK *INTERVAL*))
       (CONTIG-P T)
       (DEFUN-LINE NIL))
      (NIL)
    (AND (PLUSP (LINE-LENGTH LINE))
	 (SELECTQ *MAJOR-MODE*
	   ((LISP-MODE ZTOP-MODE)
	    (STRING-EQUAL LINE "(DEF" 0 0 4 4))
	   ((TEXT-MODE BOLIO-MODE)
	    (STRING-EQUAL LINE ".def" 0 0 4 4))
	   (OTHERWISE
	    NIL))
	 (SETQ DEFUN-LINE LINE))
    (IF ( (LINE-TICK LINE) BUFFER-TICK)	;Old line?
	(SETQ CONTIG-P NIL)			;Yes, remember to type ... next time
	(OR CONTIG-P (FUNCALL *TYPEOUT-WINDOW* ':LINE-OUT "..."))
	(SETQ CONTIG-P T)
	(COND (DEFUN-LINE
	       (AND (NEQ DEFUN-LINE LINE)
		    (FUNCALL *TYPEOUT-WINDOW* ':LINE-OUT DEFUN-LINE))
	       (SETQ DEFUN-LINE NIL)))
	(FUNCALL *TYPEOUT-WINDOW* ':ITEM 'BP (CREATE-BP LINE 0) "~A" LINE)
	(FUNCALL *TYPEOUT-WINDOW* ':TYO #\CR))
    (AND (EQ LINE LAST-LINE)
	 (RETURN)))
  (FUNCALL *TYPEOUT-WINDOW* ':LINE-OUT "Done.")
  DIS-NONE)

;;; SRCCOM'ing
(DEFCOM COM-SOURCE-COMPARE "Compare two files or buffers" ()
  (LET (FILE-1 FILE-2 NAME TYPE DEFAULT)
    (UNWIND-PROTECT
      (PROGN
	(MULTIPLE-VALUE (FILE-1 NAME TYPE DEFAULT)
	  (GET-BUFFER-OR-FILE-FILE "Compare" NIL T))
	(SETQ FILE-2 (GET-BUFFER-OR-FILE-FILE (FORMAT NIL "Compare ~A ~A with" TYPE NAME)
					      DEFAULT))
	(SRCCOM:SOURCE-COMPARE-FILES FILE-1 FILE-2)
	(FORMAT T "~&Done."))
      (AND FILE-1 (FUNCALL (SRCCOM:FILE-STREAM FILE-1) ':CLOSE))
      (AND FILE-2 (FUNCALL (SRCCOM:FILE-STREAM FILE-2) ':CLOSE))))
  DIS-NONE)

(DEFUN GET-BUFFER-OR-FILE-FILE (PROMPT &OPTIONAL DEFAULT OLDEST-P)
  (DECLARE (RETURN-LIST FILE NAME TYPE DEFAULT BUFFER))
  (IF (PROMPT-LINE-ACTIVATE
	(DO ((CH)) (NIL)
	  (PROMPT-LINE "~A file or buffer (F or B): " PROMPT)
	  (SETQ CH (FUNCALL STANDARD-INPUT ':TYI))
	  (SELECTQ CH
	    ((#/G #/g) (BARF))
	    ((#/F #/f) (RETURN T))
	    ((#/B #/b) (RETURN NIL)))
	  (FUNCALL STANDARD-INPUT ':CLEAR-INPUT)
	  (TYPEIN-LINE "Type F or B please")))
      (LET ((PATHNAME (READ-DEFAULTED-PATHNAME (FORMAT NIL "~A ~A" PROMPT "File")
					       (OR DEFAULT (PATHNAME-DEFAULTS))
					       NIL (IF OLDEST-P ':OLDEST ':NEWEST)
					       ':READ NIL)))
	(VALUES (SRCCOM:CREATE-FILE PATHNAME) PATHNAME "File" PATHNAME))
      (LET* ((BUFFER (READ-BUFFER-NAME (FORMAT NIL "~A ~A" PROMPT "Buffer") *INTERVAL*))
	     (NAME (BUFFER-NAME BUFFER)))
	(VALUES (SRCCOM:MAKE-FILE
		  SRCCOM:FILE-NAME NAME
		  SRCCOM:FILE-TYPE "Buffer"
		  SRCCOM:FILE-STREAM (INTERVAL-STREAM BUFFER)
		  SRCCOM:FILE-MAJOR-MODE (INTERN (STRING-UPCASE
						   (SYMEVAL (BUFFER-SAVED-MAJOR-MODE
							      BUFFER)))
						 ""))
		NAME
		"Buffer"
		(AND (BUFFER-FILE-ID BUFFER) (BUFFER-PATHNAME BUFFER))
		BUFFER))))

(DEFCOM COM-SOURCE-COMPARE-MERGE
	"Compare two files or buffers and merge the differences into the specified buffer" ()
  (LET (FILE-1 FILE-2 NAME TYPE DEFAULT TEM OUTPUT-BUFFER)
    (UNWIND-PROTECT
      (PROGN
	(MULTIPLE-VALUE (FILE-1 NAME TYPE DEFAULT OUTPUT-BUFFER)
	  (GET-BUFFER-OR-FILE-FILE "Merge"))
	(MULTIPLE-VALUE (FILE-2 NIL NIL NIL TEM)
	  (GET-BUFFER-OR-FILE-FILE (FORMAT NIL "Merge ~A ~A with" TYPE NAME) DEFAULT))
	(OR OUTPUT-BUFFER (SETQ OUTPUT-BUFFER TEM))
	(SETQ OUTPUT-BUFFER (READ-BUFFER-NAME "Put merged version into buffer"
					      OUTPUT-BUFFER T))
	(MAKE-BUFFER-CURRENT OUTPUT-BUFFER)
	(LET ((INTERVAL (CREATE-INTERVAL))
	      MARKS)
	  (SETQ MARKS (SRCCOM:SOURCE-COMPARE-AUTOMATIC-MERGE-RECORDING
			FILE-1 FILE-2 (INTERVAL-STREAM INTERVAL)))
	  (REPLACE-INTERVALS OUTPUT-BUFFER INTERVAL)
	  (SOURCE-COMPARE-MERGE-QUERY MARKS)
	  (TYPEIN-LINE "Done.  Resectionizing the buffer.")
	  (SECTIONIZE-BUFFER OUTPUT-BUFFER)))
      (AND FILE-1 (FUNCALL (SRCCOM:FILE-STREAM FILE-1) ':CLOSE))
      (AND FILE-2 (FUNCALL (SRCCOM:FILE-STREAM FILE-2) ':CLOSE))))
  DIS-NONE)

;;; Destructive insertion
(DEFUN REPLACE-INTERVALS (OLD-INTERVAL NEW-INTERVAL)
  (DELETE-INTERVAL OLD-INTERVAL)
  (LET ((FIRST-BP (INTERVAL-FIRST-BP NEW-INTERVAL))
	(LAST-BP (INTERVAL-LAST-BP NEW-INTERVAL)))
    (DOLIST (BP (LINE-BP-LIST (BP-LINE (INTERVAL-FIRST-BP OLD-INTERVAL))))
      (MOVE-BP BP (IF (EQ (BP-STATUS BP) ':MOVES) LAST-BP FIRST-BP)))
    (MOVE-BP (INTERVAL-FIRST-BP OLD-INTERVAL) FIRST-BP)
    (MOVE-BP (INTERVAL-LAST-BP OLD-INTERVAL) LAST-BP)))

(DEFUN SOURCE-COMPARE-MERGE-QUERY (MARKS)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (UNWIND-PROTECT
    (DO ((MARK) (DO-THE-REST)) ((NULL MARKS))
      (POP MARKS MARK)
      (UNWIND-PROTECT
	(LET ((BP1 (FIRST MARK))
	      (BP2 (SECOND MARK))
	      (BP3 (THIRD MARK))
	      (BP4 (FOURTH MARK))
	      (BP5 (FIFTH MARK))
	      (BP6 (SIXTH MARK)))
	  (COND ((NOT DO-THE-REST)
		 (DO-NAMED ONE-MARK
			   ((REDISPLAY-P T REDISPLAY-NEXT-P)
			    (REDISPLAY-NEXT-P NIL NIL)
			    (DO-IT NIL NIL))
			   (NIL)
		   (COND (REDISPLAY-P
			  (MOVE-BP (POINT) BP1)
			  (MUST-REDISPLAY *WINDOW* DIS-BPS)
			  (LET ((*CENTERING-FRACTION* 0.10s0))
			    (RECENTER-WINDOW *WINDOW* ':ABSOLUTE))))
		   (REDISPLAY *WINDOW* ':POINT)
		   (SELECTQ (PROMPT-LINE-ACTIVATE
			      (PROMPT-LINE "1, 2, *, SPACE, RUBOUT, !, R or HELP: ")
			      (CHAR-UPCASE (TYI-WITH-SCROLLING T)))
		     (#/G (BARF))
		     (#/1 (SETQ DO-IT 1))
		     (#/2 (SETQ DO-IT 2))
		     (#/* (SETQ DO-IT '*))
		     (#\MOUSE-1-1
		      (OR (LET ((BP (MOUSE-BP *WINDOW*)))
			    (SETQ DO-IT (COND ((BP-< BP BP2) NIL)
					      ((BP-< BP BP3) 1)
					      ((BP-< BP BP4) '*)
					      ((BP-< BP BP5) 2)
					      (T NIL))))
			  (BEEP)))
		     (#\SP (RETURN NIL))
		     (#\RUBOUT
		      (DELETE-INTERVAL BP2 BP5 T)
		      (RETURN))
		     (#/!
		      (DO () (NIL)
			(SELECTQ (PROMPT-LINE-ACTIVATE
				  (PROMPT-LINE "1, 2, *, RUBOUT or HELP: ")
				  (CHAR-UPCASE (FUNCALL STANDARD-INPUT ':TYI)))
			  (#/G (BARF))
			  (#/1
			   (SETQ DO-THE-REST 1)
			   (RETURN-FROM ONE-MARK))
			  (#/2
			   (SETQ DO-THE-REST 2)
			   (RETURN-FROM ONE-MARK))
			  (#/*
			   (SETQ DO-THE-REST '*)
			   (RETURN-FROM ONE-MARK))
			  (#\RUBOUT (RETURN))
			  (#\HELP
			   (TYPEIN-LINE "1 - rest from file #1, 2 - rest from file #2,~
					 * - rest from both, RUBOUT abort")))))
		     (#/R
		      (CONTROL-R)
		      (SETQ REDISPLAY-NEXT-P T))
		     (#\FF
		      (MUST-REDISPLAY *WINDOW* DIS-ALL))
		     (#/L
		      (MUST-REDISPLAY *WINDOW* (COM-RECENTER-WINDOW)))
		     (#\HELP
		      (TYPEIN-LINE "1 - file #1, 2 - file #2, * - both, ~@
				    SPACE - both without query, RUBOUT - neither, ~@
				    ! - rest of 1 or 2, R - edit"))
		     (OTHERWISE
		      (BEEP)))
		   (AND DO-IT
			(LET (OK CONTROL-R-P)
			  (SELECTQ DO-IT
				(* (MULTIPLE-VALUE (OK CONTROL-R-P)
				     (SOURCE-COMPARE-MERGE-QUERY-1 BP1 BP2 BP3 BP4 BP5 BP6)))
				(1 (MULTIPLE-VALUE (OK CONTROL-R-P)
				     (SOURCE-COMPARE-MERGE-QUERY-1 BP1 BP2 BP3 BP6)))
				(2 (MULTIPLE-VALUE (OK CONTROL-R-P)
				     (SOURCE-COMPARE-MERGE-QUERY-1 BP1 BP4 BP5 BP6))))
			  (SETQ REDISPLAY-NEXT-P CONTROL-R-P)
			  OK)
			(RETURN)))))
	  (COND (DO-THE-REST
		 (SELECTQ DO-THE-REST
		   (* )
		   (1 (DELETE-INTERVAL BP4 BP5 T))
		   (2 (DELETE-INTERVAL BP2 BP3 T)))
		 (MUST-REDISPLAY *WINDOW* DIS-TEXT))))
	(FLUSH-SOURCE-COMPARE-MARK MARK)))
    (MAPCAR #'FLUSH-SOURCE-COMPARE-MARK MARKS)))

(DEFUN SOURCE-COMPARE-MERGE-QUERY-1 (&REST START-AND-END-BPS &AUX INTS FLAG)
  (SETQ INTS (DO ((BPS START-AND-END-BPS (CDDR BPS))
		  (LIST NIL))
		 ((NULL BPS) (NREVERSE LIST))
	       (PUSH (COPY-INTERVAL (CAR BPS) (CADR BPS) T) LIST)))
  (UNWIND-PROTECT
    (PROGN
      (DO ((BPS START-AND-END-BPS (CDDR BPS)))
	  ((NULL BPS))
	(DELETE-INTERVAL (CAR BPS) (CADR BPS) T))
      (MUST-REDISPLAY *WINDOW* DIS-TEXT)
      (REDISPLAY *WINDOW* ':POINT)
      (DO ((CONTROL-R-P NIL)) (NIL)
	(SELECTQ (PROMPT-LINE-ACTIVATE
		  (PROMPT-LINE "SPACE, RUBOUT, R or HELP: ")
		  (CHAR-UPCASE (TYI-WITH-SCROLLING T)))
	  (#/G (BARF))
	  ((#\SP #\MOUSE-1-1) (SETQ FLAG T) (RETURN T))
	  ((#\RUBOUT #\MOUSE-2-1) (RETURN NIL CONTROL-R-P))
	  (#/R (CONTROL-R) (SETQ CONTROL-R-P T))
	  (#\HELP (TYPEIN-LINE "SPACE confirm, RUBOUT abort, R edit"))
	  (OTHERWISE (BEEP)))))
    (OR FLAG
	(DO ((BPS START-AND-END-BPS (CDDR BPS))
	     (INTS INTS (CDR INTS))
	     (BP1) (BP2))
	    ((NULL BPS)
	     (MUST-REDISPLAY *WINDOW* DIS-TEXT))
	  (SETQ BP1 (CAR BPS)
		BP2 (CADR BPS))
	  (UNWIND-PROTECT
	    (PROGN
	      (SETF (BP-STATUS BP1) ':NORMAL)
	      (INSERT-INTERVAL-MOVING BP2 (CAR INTS)))
	    (SETF (BP-STATUS BP1) ':MOVES))))))

(DEFUN FLUSH-SOURCE-COMPARE-MARK (MARK)
  (DO MARK MARK (CDDR MARK) (NULL MARK)
      (LET ((BP1 (CAR MARK))
	    (BP2 (CADR MARK)))
	(DELETE-INTERVAL BP1 BP2 T)))
  (MAPCAR #'FLUSH-BP MARK))

(DEFUN INITIALIZE-ZMACS-COMTABS ()
  (SETQ *ZMACS-CONTROL-X-COMTAB*
	(SET-COMTAB NIL '(#/F COM-FIND-FILE
			  #/V COM-VISIT-FILE
			  #/B COM-SELECT-BUFFER
			  #/W COM-WRITE-FILE
			  #/S COM-SAVE-FILE
			  #/B COM-LIST-BUFFERS
			  #/K COM-KILL-BUFFER
			  #/A COM-APPEND-TO-BUFFER
			  #/ COM-FASL-UPDATE
			  #/1 COM-ONE-WINDOW
			  #/2 COM-TWO-WINDOWS
			  #/3 COM-VIEW-TWO-WINDOWS
			  #/4 COM-MODIFIED-TWO-WINDOWS
			  #/^ COM-GROW-WINDOW
			  #/O COM-OTHER-WINDOW
			  #/M COM-MAIL
			  #/D COM-R-DIRED
			  #/V COM-VIEW-BUFFER
			  #/8 COM-TWO-WINDOWS-SHOWING-REGION
			  #/L COM-SELECT-DEFAULT-PREVIOUS-BUFFER)))
  (SET-COMTAB-INDIRECTION *ZMACS-CONTROL-X-COMTAB* *STANDARD-CONTROL-X-COMTAB*)
  (SETQ *ZMACS-COMTAB*
	(SET-COMTAB NIL '(#/V COM-SCROLL-OTHER-WINDOW
			  #/. COM-EDIT-DEFINITION
			  #/L COM-SELECT-PREVIOUS-BUFFER)
		    (MAKE-COMMAND-ALIST
		     '(COM-REVERT-BUFFER COM-NOT-MODIFIED COM-VISIT-TAG-TABLE
		       COM-LIST-BUFFERS COM-SAVE-ALL-FILES COM-KILL-SOME-BUFFERS
		       COM-KILL-OR-SAVE-BUFFERS COM-SPLIT-SCREEN
		       COM-LIST-FUNCTIONS COM-LIST-TAG-TABLES COM-SELECT-TAG-TABLE
		       COM-SET-PACKAGE COM-SET-DEFAULT-FILE-NAME COM-RENAME-BUFFER
		       COM-SET-VISITED-FILE-NAME
		       COM-TAGS-SEARCH COM-TAGS-QUERY-REPLACE COM-NEXT-FILE
		       COM-EDIT-CALLERS COM-LIST-CALLERS
		       COM-EDIT-MULTIPLE-CALLERS COM-LIST-MULTIPLE-CALLERS
		       COM-LIST-MATCHING-FUNCTIONS COM-FUNCTION-APROPOS COM-SECTIONIZE-BUFFER
		       COM-DESCRIBE-CLASS COM-DESCRIBE-FLAVOR
		       COM-LIST-METHODS COM-EDIT-METHODS
		       COM-LIST-COMBINED-METHODS COM-EDIT-COMBINED-METHODS
		       COM-FASL-UPDATE COM-UPDATE-MODE-LINE COM-EDIT-ZMACS-COMMAND
		       COM-COMPILE-FILE COM-BUG COM-REPARSE-MODE-LINE
		       COM-LIST-FONTS COM-DISPLAY-FONT
		       COM-DIRED COM-REAP-FILE COM-CLEAN-DIRECTORY COM-CHANGE-FILE-PROPERTIES
		       COM-EXPUNGE-DIRECTORY
		       COM-MAIL COM-EDIT-COMPILER-WARNINGS
		       COM-SELECT-SYSTEM-AS-TAG-TABLE COM-SELECT-ALL-BUFFERS-AS-TAG-TABLE
		       COM-TAGS-MULTIPLE-QUERY-REPLACE
		       COM-TAGS-MULTIPLE-QUERY-REPLACE-FROM-BUFFER COM-WHERE-IS-SYMBOL
		       COM-ZTOP-MODE COM-SELECT-LAST-ZTOP-BUFFER
		       COM-BACKSPACE-OVERPRINTS COM-SET-TAB-WIDTH
		       COM-VIEW-BUFFER COM-INSERT-BUFFER COM-PRINT-MODIFICATIONS
		       COM-COMPILE-CHANGED-FUNCTIONS COM-COMPILE-BUFFER-CHANGED-FUNCTIONS
		       COM-EVALUATE-CHANGED-FUNCTIONS COM-EVALUATE-BUFFER-CHANGED-FUNCTIONS
		       COM-LIST-CHANGED-FUNCTIONS COM-EDIT-CHANGED-FUNCTIONS
		       COM-LIST-BUFFER-CHANGED-FUNCTIONS COM-EDIT-BUFFER-CHANGED-FUNCTIONS
		       COM-SOURCE-COMPARE COM-SOURCE-COMPARE-MERGE
		       COM-ADD-PATCH COM-FINISH-PATCH
		       ))))
  (SET-COMTAB-INDIRECTION *ZMACS-COMTAB* *STANDARD-COMTAB*)
  (SET-COMTAB *ZMACS-COMTAB*
	      (LIST #/X (MAKE-EXTENDED-COMMAND *ZMACS-CONTROL-X-COMTAB*)))
  (SET-COMTAB *ZMACS-COMTAB*
	      (LIST #\MOUSE-3-1
	       (MAKE-MENU-COMMAND '(COM-ARGLIST COM-EDIT-DEFINITION
				    COM-LIST-CALLERS COM-LIST-FUNCTIONS COM-LIST-BUFFERS
				    COM-KILL-OR-SAVE-BUFFERS COM-SPLIT-SCREEN
				    COM-COMPILE-DEFUN COM-INDENT-REGION
				    COM-CHANGE-DEFAULT-FONT COM-CHANGE-FONT-REGION
				    COM-UPPERCASE-REGION COM-LOWERCASE-REGION
				    COM-MOUSE-INDENT-RIGIDLY COM-MOUSE-INDENT-UNDER)))))

(DEFVAR *ZMACS-NEEDS-INITIALIZATION* T)		;T means initialize ZMACS on next entry.
(DEFVAR *ZMACS-CALLER-WINDOW* NIL)		;Window selected when ZMACS was entered

;;; Normal user function, calls ZED unless already inside the editor.
(DEFUN ED (&OPTIONAL THING)
  (COND ((NEQ CURRENT-PROCESS *ZMACS-WINDOW-PROCESS*)	;Outside, enter the editor
	 (ZED THING))
	((EQ SYS:%CURRENT-STACK-GROUP (PROCESS-INITIAL-STACK-GROUP *ZMACS-WINDOW-PROCESS*))
	 (AND THING (EDIT-THING THING))
	 (AND (OR *INSIDE-BREAK* *MINI-BUFFER-COMMAND-IN-PROGRESS*)
	      (*THROW 'ZWEI-COMMAND-LOOP T)))
	((Y-OR-N-P "You are already running inside the editor,
if you edit that you will lose the state of the error handler.
Ok to go ahead? ")					;Right process, but wrong stack group
	 (AND THING (FUNCALL *ZMACS-COMMAND-LOOP* ':EDIT-THING THING))
	 (FUNCALL CURRENT-PROCESS ':RESET ':ALWAYS))))	;Will see command in I/O buffer

;;; This is the normal top-level function, it takes the optional name of a thing to munge
(DEFUN ZED (&OPTIONAL THING)
  (SETQ *ZMACS-CALLER-WINDOW* TV:SELECTED-WINDOW)
  (COND ((EQ THING 'RELOAD)
	 (SETQ *ZMACS-NEEDS-INITIALIZATION* T)
	 (MAKUNBOUND '*TICK*)))
  ;; Stop the ZMACS window process for now, to avoid timing screws.
  ;; It will start up again when we reset it.
  (AND *ZMACS-WINDOW-PROCESS* (FLUSH-PROCESS *ZMACS-WINDOW-PROCESS*))
  ;; Now we can safely do things like initialize
  (INITIALIZE-ZMACS-IF-NECESSARY)
  (FUNCALL *ZMACS-COMMAND-LOOP* ':MINI-BUFFER-ACCIDENTALLY-SELECTED-KLUDGE)
  ;; And select something according to our argument.
  (AND THING (NEQ THING 'RELOAD)
       (FUNCALL *ZMACS-COMMAND-LOOP* ':EDIT-THING THING))
  ;; Now start the ZMACS process up again and select the appropriate windows.
  (UNWIND-PROTECT
    (PROGN
      (FUNCALL *ZMACS-WINDOW-PROCESS* ':ARREST-REASON 'ZED)
      (FUNCALL (WINDOW-FRAME (SYMEVAL-IN-INSTANCE *ZMACS-COMMAND-LOOP* '*WINDOW*)) ':SELECT))
    (RESET-PROCESS *ZMACS-WINDOW-PROCESS*)
    (FUNCALL *ZMACS-WINDOW-PROCESS* ':REVOKE-ARREST-REASON 'ZED))
  (TV:AWAIT-WINDOW-EXPOSURE))

(DEFUN EDIT-FUNCTIONS (FUNCTIONS)
  (ED (CONS 'CALLERS-TO-BE-EDITED FUNCTIONS)))

;;; Called by ZED and by user's init files
(DEFUN INITIALIZE-ZMACS-IF-NECESSARY (&AUX INTERVAL WINDOW)
  (COND (*ZMACS-NEEDS-INITIALIZATION*
	 (PKG-GOTO "USER")
	 (OR (BOUNDP '*TICK*) (INITIALIZE-ZWEI-GLOBALS))
	 (INITIALIZE-ZMACS-COMTABS)
	 (AND *ZMACS-WINDOW-PROCESS*
	      (FLUSH-PROCESS-WINDOWS *ZMACS-WINDOW-PROCESS*))
	 ;; I hope no one starts up the process until all special variables are initialized.
	 (SETQ *ZMACS-WINDOW-PROCESS* (MAKE-PROCESS "ZMACS-WINDOWS"
						    ':FLAVOR 'SI:COROUTINING-PROCESS
						    ':INITIAL-FORM '(ZMACS-WINDOW-TOP-LEVEL)
						    ':REGULAR-PDL-SIZE 10000
						    ':SPECIAL-PDL-SIZE 10000))
	 (SETQ *ZMACS-MAIN-FRAME* (TV:MAKE-WINDOW 'ZMACS-FRAME
						  ':NUMBER-OF-MINI-BUFFER-LINES 3
						  ':IO-BUFFER NIL ':ACTIVATE-P T))
	 (SETQ WINDOW (FUNCALL *ZMACS-MAIN-FRAME* ':EDITOR-WINDOW))
	 (FUNCALL *ZMACS-MAIN-FRAME* ':CREATE-WINDOW 'ZMACS-WINDOW-PANE)
	 (CREATE-OVERLYING-WINDOW WINDOW)
	 (SETQ *ZMACS-BUFFER-LIST* NIL
	       *ZMACS-BUFFER-NAME-ALIST* NIL *ZMACS-TAG-TABLE-ALIST* NIL)
	 (SETQ *ZMACS-COMPLETION-AARRAY* (MAKE-ARRAY 100 ':TYPE 'ART-Q-LIST
							 ':LEADER-LIST '(0 T)))
	 (SETQ INTERVAL (CREATE-ONE-BUFFER-TO-GO))
	 (SETQ *ZMACS-BUFFER-HISTORY* (LIST INTERVAL))	;in case C-X M used very soon
	 (SET-WINDOW-INTERVAL WINDOW INTERVAL)
	 (UPDATE-BUFFER-NAMES INTERVAL)
	 (SETQ *ZMACS-STREAM*
	       (MAKE-MACRO-STREAM
		 (MAKE-RECORDING-STREAM (WINDOW-SHEET WINDOW) 100.)))
	 (SETQ *ZMACS-COMMAND-LOOP*
	       (MAKE-COMMAND-LOOP *ZMACS-COMTAB* WINDOW
				  'ZMACS-TOP-LEVEL-EDITOR
				  ':STANDARD-INPUT *ZMACS-STREAM*))
	 ;; This causes the pane to become exposed relative to the frame, which means the
	 ;; :START-DELAYED-SELECT message will not cause it to become selected when the
	 ;; editor starts running.
	 (FUNCALL (WINDOW-SHEET WINDOW) ':EXPOSE)
	 (SETQ *ZMACS-NEEDS-INITIALIZATION* NIL))))

(ADD-INITIALIZATION "INITIALIZE-ZMACS-IF-NECESSARY" '(INITIALIZE-ZMACS-IF-NECESSARY)
		    '(:NORMAL) '*EDITOR-INITIALIZATION-LIST*)

(DEFMETHOD (ZMACS-EDITOR :COMPUTE-PACKAGE) (OUTSIDE-PACKAGE)
  (SETQ PACKAGE OUTSIDE-PACKAGE)
  (COMPUTE-BUFFER-PACKAGE *INTERVAL*))

(DEFMETHOD (ZMACS-EDITOR :EDIT-THING) (THING)
  (MUST-REDISPLAY *WINDOW* DIS-ALL)
  (LOCAL-DECLARE ((SPECIAL TV:IO-BUFFER))
    (TV:IO-BUFFER-PUT TV:IO-BUFFER `(:EXECUTE EDIT-THING ,THING))))

(DEFUN EDIT-THING (THING &AUX (*CURRENT-COMMAND* 'ZED))
  (COND ((EQ THING T)
	 (MAKE-BUFFER-CURRENT (CREATE-ONE-BUFFER-TO-GO)))
	((OR (STRINGP THING) (TYPEP THING 'FS:PATHNAME))
	 (FIND-DEFAULTED-FILE THING))
	((AND (LISTP THING) (EQ (CAR THING) 'CALLERS-TO-BE-EDITED))
	 (FUNCALL (SETUP-ZMACS-CALLERS-TO-BE-EDITED (CDR THING))))
	((SYS:VALIDATE-FUNCTION-SPEC THING)
	 (EDIT-DEFINITION THING T))
	(T
	 (BARF "Dont know what to do with ~S" THING))))

(DEFMETHOD (ZMACS-EDITOR :ADD-WINDOW) (WINDOW)
  (OR (MEMQ WINDOW *WINDOW-LIST*) (PUSH WINDOW *WINDOW-LIST*)))

(DEFMETHOD (ZMACS-EDITOR :FIND-SPECIAL-BUFFER) (TYPE NEW-P NAME SPECIAL-LENGTH
						*DEFAULT-MAJOR-MODE*)
  (MAKE-BUFFER-CURRENT
    (LOOP FOR BUFFER IN *ZMACS-BUFFER-LIST*
	  WITH PROSPECT = NIL
	  AS FILE-ID = (BUFFER-FILE-ID BUFFER)
	  WHEN (AND (LISTP FILE-ID)
		    (EQ (FIRST FILE-ID) ':SPECIAL-BUFFER)
		    (EQ (SECOND FILE-ID) TYPE))
	  DO (SETQ PROSPECT BUFFER)
	  (AND (EQ (NOT NEW-P)
		   (LET ((NODE-TICK (NODE-TICK BUFFER)))
		     (> (IF (NEQ NODE-TICK ':READ-ONLY) NODE-TICK (THIRD FILE-ID))
			(BUFFER-TICK BUFFER))))
	       (RETURN BUFFER))
	  FINALLY (COND ((OR NEW-P (NULL PROSPECT))
			 (SETQ PROSPECT (CREATE-ONE-BUFFER-TO-GO
					  (LOOP FOR I FROM 1
						AS BUFNAM = (FORMAT NIL "*~A-~D*" NAME I)
						UNLESS (FIND-BUFFER-NAMED BUFNAM)
						RETURN BUFNAM)))
			 (LET ((FILE-ID (MAKE-LIST SPECIAL-LENGTH)))
			   (SETF (FIRST FILE-ID) ':SPECIAL-BUFFER)
			   (SETF (SECOND FILE-ID) TYPE)
			   (SET-BUFFER-FILE-ID PROSPECT FILE-ID))))
		  (RETURN PROSPECT))))

(DEFMETHOD (ZMACS-EDITOR :EXIT-SPECIAL-BUFFER) (&OPTIONAL UPDATE-P)
  (LET ((SPECIAL-BUFFER *INTERVAL*))
    (AND UPDATE-P (SETF (BUFFER-TICK SPECIAL-BUFFER)
			(LET ((NODE-TICK (NODE-TICK SPECIAL-BUFFER)))
			  (IF (NEQ NODE-TICK ':READ-ONLY) NODE-TICK
			      (THIRD (BUFFER-FILE-ID SPECIAL-BUFFER))))))
    (MAKE-BUFFER-CURRENT (OR (CADR *ZMACS-BUFFER-HISTORY*) *INTERVAL*))
    (SETQ *ZMACS-BUFFER-HISTORY* (DELQ SPECIAL-BUFFER *ZMACS-BUFFER-HISTORY*))
    (POINT-PDL-PURGE SPECIAL-BUFFER))
  DIS-TEXT)

(DEFMETHOD (ZMACS-EDITOR :FIND-BUFFER-NAMED) (NAME &OPTIONAL CREATE-P
					      (*DEFAULT-MAJOR-MODE* *DEFAULT-MAJOR-MODE*))
  (MAKE-BUFFER-CURRENT (FIND-BUFFER-NAMED NAME CREATE-P)))

(DEFUN LOAD-FILE-INTO-ZMACS (PATHNAME)
  (FUNCALL *ZMACS-COMMAND-LOOP* ':FIND-FILE PATHNAME))

(DEFMETHOD (ZMACS-EDITOR :FIND-FILE) (PATHNAME)
  (FIND-FILE PATHNAME NIL))

(DEFMETHOD (ZMACS-EDITOR :FIND-EMPTY-FILE) (PATHNAME &AUX BUFFER)
  (MULTIPLE-VALUE (NIL BUFFER)
    (FIND-FILE PATHNAME NIL))
  (DELETE-INTERVAL BUFFER))

(DEFMETHOD (ZMACS-EDITOR :SAVE-FILE) (PATHNAME &OPTIONAL CONFIRM &AUX BUFFER)
  (SETQ BUFFER (FIND-BUFFER-NAMED PATHNAME))
  (LET ((*WINDOW* NIL)
	(*TYPEOUT-WINDOW* STANDARD-OUTPUT)
	(*TYPEIN-WINDOW* STANDARD-OUTPUT)
	(*NUMERIC-ARG-P* NIL))
    (SAVE-BUFFER-IF-NECESSARY BUFFER CONFIRM)))

