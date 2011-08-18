;;; -*- Mode:LISP; Package:ZWEI -*-
;;; More winning Lisp Machine software patch facility.   DLW & BEE 10/24/80
;;; The functions in this file manage the patch files

;;; The buffer holding the patch file being accumulated.
(DEFVAR *PATCH-BUFFER* NIL)

;;; The system we are trying to patch
(DEFVAR *PATCH-PATCH* NIL)

;;; The system minor version number we are patching
(DEFVAR *PATCH-NUMBER*)

(DEFCOM COM-ADD-PATCH "Add the current defun or the region (if any) to the patch buffer.
If there is no patch buffer, ask the user for the system to patch. Then reserve a new
version number, and create a buffer whose pathname is the source file name for the
patch creating that version number.  If there is a region, append it to the end of the
patch buffer; otherwise append the current defun to the end of the patch buffer." ()
  (LET (BP1 BP2 DEFUN-NAME)
    (COND ((WINDOW-MARK-P *WINDOW*)
	   ;; there is a region, use it.
	   (SETQ BP1 (MARK) BP2 (POINT))
	   (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
	   (SETQ DEFUN-NAME "the region"))
	  ((MULTIPLE-VALUE (BP1 DEFUN-NAME) (DEFUN-INTERVAL (BEG-LINE (POINT))))
	   ;; No region, try to get containing defun.
	   (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
	   (SETQ DEFUN-NAME (GET-DEFUN-NAME DEFUN-NAME)))
	  (T
	   (BARF "Unbalanced parentheses")))
    (ADD-PATCH-INTERVAL BP1 BP2 T DEFUN-NAME *INTERVAL*))
  DIS-MARK-GOES)

(DEFUN ADD-PATCH-INTERVAL (BP1 BP2 IN-ORDER-P DEFUN-NAME BUFFER &AUX NEW-PATCH-BUFFER-P)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (SETQ NEW-PATCH-BUFFER-P (NULL *PATCH-BUFFER*))
  (AND NEW-PATCH-BUFFER-P (CREATE-PATCH-BUFFER))
  (TYPEIN-LINE "Adding ~A to patch file for version ~D.~D of ~A~:[~;~%(New patch file.)~]"
	       DEFUN-NAME (SI:PATCH-VERSION *PATCH-PATCH*) *PATCH-NUMBER*
	       (SI:PATCH-NAME *PATCH-PATCH*) NEW-PATCH-BUFFER-P)
  (LET ((BP (INTERVAL-LAST-BP *PATCH-BUFFER*)))
    ;; Put into the patch buffer, making sure the right package and base will be used.
    (MULTIPLE-VALUE-BIND (VARS VALS)
	(FS:FILE-PROPERTY-BINDINGS (BUFFER-GENERIC-PATHNAME BUFFER))
      (PROGV VARS VALS
	     (INSERT BP (FORMAT NIL "~%; From file ~A~%#~DR ~A:~
				(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE /"~:*~A/")))~2%"
				(BUFFER-NAME BUFFER) IBASE PACKAGE))))
    (INSERT-INTERVAL BP BP1 BP2 T)
    (INSERT BP "
)
")))

(DEFUN CREATE-PATCH-BUFFER ()
  (LET ((PATCH (COMPLETING-READ-FROM-MINI-BUFFER "System to patch"
		     (MAPCAR #'(LAMBDA (X) (CONS (SI:PATCH-NAME X) X))
			     SI:PATCH-SYSTEMS-LIST))))
    ;; Even though impossible-is-ok-p is NIL, can still return ""
    (AND (STRINGP PATCH) (BARF))
    ;; Some day maybe allow creating of a patch here
    (SETQ PATCH (CDR PATCH))
    (SETQ *PATCH-NUMBER* (SI:RESERVE-PATCH PATCH))
    (LET* ((FILENAME (SI:PATCH-SYSTEM-PATHNAME (SI:PATCH-NAME PATCH) ':PATCH-FILE
					       (SI:PATCH-VERSION PATCH) *PATCH-NUMBER*
					       "LISP"))
	   (PROBED (OPEN FILENAME '(:PROBE))))
      (COND ((NOT (STRINGP PROBED))
	     (BARF "The file ~A already exists -- you'd better fix it!" FILENAME))
	    ((NOT (STRING-EQUAL "FNF" (FS:FILE-PROCESS-ERROR PROBED NIL NIL T)))
	     (BARF "Error probing file ~A: ~A" FILENAME PROBED))
	    (T
	     (MULTIPLE-VALUE (NIL *PATCH-BUFFER*)
	       (FIND-FILE FILENAME NIL)))))	; The NIL means "don't select this buffer".
    (FUNCALL (BUFFER-GENERIC-PATHNAME *PATCH-BUFFER*) ':PUTPROP T ':PATCH-FILE)
    (LET ((STREAM (INTERVAL-STREAM *PATCH-BUFFER*)))
      (FORMAT STREAM ";;; -*- Mode: Lisp; Package: User; Base: ~D.; Patch-File: T -*-
;;; Patch file for ~A version ~D.~D
"
	      (OR (FUNCALL (BUFFER-GENERIC-PATHNAME *INTERVAL*) ':GET ':BASE) IBASE)
	      (SI:PATCH-NAME PATCH) (SI:PATCH-VERSION PATCH) *PATCH-NUMBER*)
      (SI:WRITE-RESPONSIBILITY-COMMENT STREAM)
      (TERPRI STREAM))
    (SETQ *PATCH-PATCH* PATCH)))

(DEFCOM COM-FINISH-PATCH "Finish off the current patch file. This assumes that the
existing qfasl file has been tested, and it installs it as a permanent patch file." ()
   (OR *PATCH-BUFFER*
       (BARF "There is no current patch buffer"))
   (LET ((DESCRIPTION (TYPEIN-LINE-READLINE "Description of changes")))
     (LET ((BP (FORWARD-LINE (INTERVAL-FIRST-BP *PATCH-BUFFER*) 2)))
       (INSERT-MOVING BP ";;; Reason: ")
       (INSERT-MOVING BP DESCRIPTION)
       (INSERT BP #\RETURN))
     (SAVE-BUFFER-IF-NECESSARY *PATCH-BUFFER*)
     (AND (EQ *PATCH-BUFFER* *INTERVAL*)
	  (MUST-REDISPLAY *WINDOW* DIS-TEXT))
     (LET ((ERROR-MESSAGE (SI:CONSUMMATE-PATCH *PATCH-PATCH* *PATCH-NUMBER* DESCRIPTION)))
       (COND ((STRINGP ERROR-MESSAGE)
	      (BARF ERROR-MESSAGE))
	     (T
	      (IF (LOOP FOR BUFFER IN *ZMACS-BUFFER-LIST*
			THEREIS (BUFFER-NEEDS-SAVING-P BUFFER))
		  (TYPEIN-LINE "Don't forget to save your files!"))
	      (SETQ *PATCH-BUFFER* NIL)))))
     DIS-NONE)

(DEFCOM COM-ADD-PATCH-CHANGED-FUNCTIONS "Do add patch for all changed sections" ()
  (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
    (AND (EQ (IF (EQ BUFFER *INTERVAL*) *MAJOR-MODE*
		 (BUFFER-SAVED-MAJOR-MODE BUFFER))
	     'LISP-MODE)
	 (ADD-PATCH-BUFFER-CHANGED-FUNCTIONS BUFFER)))
  DIS-NONE)

(DEFCOM COM-ADD-PATCH-BUFFER-CHANGED-FUNCTIONS
	"Do add patch for all changed functions in the buffer" ()
  (ADD-PATCH-BUFFER-CHANGED-FUNCTIONS *INTERVAL*)
  DIS-NONE)

(DEFUN ADD-PATCH-BUFFER-CHANGED-FUNCTIONS (BUFFER &OPTIONAL (SELECTIVE *NUMERIC-ARG-P*))
  (DOLIST (SECTION (NODE-INFERIORS BUFFER))
    (AND (TYPEP SECTION 'SECTION-NODE)
	 (> (NODE-TICK SECTION) (MIN (BUFFER-TICK BUFFER)
				     (SECTION-NODE-COMPILE-TICK SECTION)))
	 (SECTION-NODE-DEFUN-LINE SECTION)
	 (LET ((NAME (SECTION-NODE-NAME SECTION)))
	   (AND (OR (NOT SELECTIVE)
		    (FQUERY NIL "Add patch ~A? " NAME))
		(ADD-PATCH-INTERVAL SECTION NIL T NAME BUFFER))))))
