;;; -*- Mode:LISP; Package:USER -*-

(DEFUN COPY-PDP10-FILE (FROM-PATHNAME TO-PATHNAME &OPTIONAL TELL-P)
  (SETQ FROM-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS FROM-PATHNAME)
	TO-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS TO-PATHNAME))
  (AND TELL-P (FORMAT T "~&Copying ~A -> ~A" FROM-PATHNAME TO-PATHNAME))
  (WITH-OPEN-FILE (INFILE FROM-PATHNAME '(:READ :FIXNUM :BYTE-SIZE 9.))
    (WITH-OPEN-FILE (OUTFILE TO-PATHNAME '(:WRITE :FIXNUM :BYTE-SIZE 9.))
;      (STREAM-COPY-UNTIL-EOF INFILE OUTFILE)
      (DO ((CH)) ((NULL (SETQ CH (FUNCALL INFILE ':TYI))))
	(FUNCALL OUTFILE ':TYO CH))
      )))

(DEFUN COPY-LISPM-FILE (FROM-PATHNAME TO-PATHNAME BINARY-P &OPTIONAL TELL-P)
  (SETQ FROM-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS FROM-PATHNAME)
	TO-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS TO-PATHNAME))
  ;; Totally cretinous local file system.
  (LET ((DIR (FUNCALL TO-PATHNAME ':DIRECTORY)))
    (COND ((FUNCALL (FUNCALL TO-PATHNAME ':HOST) ':PATHNAME-HOST-NAMEP "local")
	   (AND ( (AREF DIR 0) #/>)
		(SETQ TO-PATHNAME (FUNCALL TO-PATHNAME ':NEW-DIRECTORY
					   (STRING-APPEND #/> DIR)))))
	  ((FUNCALL (FUNCALL FROM-PATHNAME ':HOST) ':PATHNAME-HOST-NAMEP "local")
	   (AND (= (AREF DIR 0) #/>)
		(SETQ TO-PATHNAME (FUNCALL TO-PATHNAME ':NEW-DIRECTORY
					   (SUBSTRING DIR 1)))))))
  (AND TELL-P (FORMAT T "~&Copying ~A -> ~A (~:[ascii~;binary~])"
		      FROM-PATHNAME TO-PATHNAME BINARY-P))
  (WITH-OPEN-FILE (INFILE FROM-PATHNAME ':DIRECTION ':INPUT ':CHARACTERS (NOT BINARY-P))
    (WITH-OPEN-FILE (OUTFILE TO-PATHNAME ':DIRECTION ':OUTPUT ':CHARACTERS (NOT BINARY-P))
      (STREAM-COPY-UNTIL-EOF INFILE OUTFILE)))
  (LET ((PLIST (FS:FILE-PROPERTIES FROM-PATHNAME)))
    (FS:CHANGE-PATHNAME-PROPERTIES TO-PATHNAME NIL
				   ':CREATION-DATE (GET PLIST ':CREATION-DATE)
				   ':AUTHOR (GET PLIST ':AUTHOR))))

(DEFUN COPY-LISPM-DIRECTORY (FROM-DIRECTORY TO-DIRECTORY &AUX BS TYP)
  (SETQ TO-DIRECTORY (FS:PARSE-PATHNAME TO-DIRECTORY))
  (DOLIST (FILE (CDR (FS:DIRECTORY-LIST FROM-DIRECTORY)))
    (LET* ((FROM-PATHNAME (CAR FILE))
	   (TO-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS TO-DIRECTORY FROM-PATHNAME))
	   (BINARY-P (COND ((MEMQ (SETQ BS (GET FILE ':BYTE-SIZE)) '(8 7)) NIL)
			   ((EQ BS 16.) T)
			   ((EQUAL (SETQ TYP (FUNCALL FROM-PATHNAME ':TYPE)) "QFASL") T)
			   ((EQUAL TYP "LISP") NIL)
			   ((FQUERY '(:BEEP T)
				    "Binary-P for ~A? " FROM-PATHNAME)))))
      (COPY-LISPM-FILE FROM-PATHNAME TO-PATHNAME BINARY-P T))))

(DEFUN COPY-PDP10-DIRECTORY (FROM-DIRECTORY TO-DIRECTORY)
  (SETQ TO-DIRECTORY (FS:PARSE-PATHNAME TO-DIRECTORY))
  (DOLIST (FILE (CDR (FS:DIRECTORY-LIST FROM-DIRECTORY)))
    (OR (GET FILE ':LINK-TO)
	(LET* ((FROM-PATHNAME (CAR FILE))
	       (TO-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS TO-DIRECTORY FROM-PATHNAME)))
	  (COPY-PDP10-FILE FROM-PATHNAME TO-PATHNAME T)
	  (FS:CHANGE-PATHNAME-PROPERTIES TO-PATHNAME NIL
					 ':BYTE-SIZE (GET FILE ':BYTE-SIZE)
					 ':LENGTH-IN-BYTES (GET FILE ':LENGTH-IN-BYTES))))))

(DEFUN COPY-LMPAT (MAJOR START END)
  (COPY-PDP10-FILE (FORMAT NIL "AI:LMPAT;SYS~D (PDIR)" MAJOR)
		   (FORMAT NIL "XX:SS:<MMCM.LMPAT>SYSTEM-~D.PATCH-DIRECTORY.~D" MAJOR END)
		   T)
  (LOOP FOR I FROM START TO END
	DO (COPY-PDP10-FILE (FORMAT NIL "AI:LMPAT;~D.~D >" MAJOR I)
			    (FORMAT NIL "XX:SS:<MMCM.LMPAT>SYSTEM-~D-~D.LISP" MAJOR I)
			    T)
	   (COPY-PDP10-FILE (FORMAT NIL "AI:LMPAT;~D.~D QFASL" MAJOR I)
			    (FORMAT NIL "XX:SS:<MMCM.LMPAT>SYSTEM-~D-~D.QFASL" MAJOR I)
			    T)))

(DEFUN COPY-ZMPAT (MAJOR START END)
  (COPY-PDP10-FILE (FORMAT NIL "AI:ZMAIL;PAT~D (PDIR)" MAJOR)
		   (FORMAT NIL "XX:SS:<MMCM.ZMAIL>PATCH-~D.DIRECTORY.~D" MAJOR END)
		   T)
  (LOOP FOR I FROM START TO END
	DO (COPY-PDP10-FILE (FORMAT NIL "AI:ZMAIL;P~D.~D >" MAJOR I)
			    (FORMAT NIL "XX:SS:<MMCM.ZMAIL>PATCH-~D-~D.LISP" MAJOR I)
			    T)
	   (COPY-PDP10-FILE (FORMAT NIL "AI:ZMAIL;P~D.~D QFASL" MAJOR I)
			    (FORMAT NIL "XX:SS:<MMCM.ZMAIL>PATCH-~D-~D.QFASL" MAJOR I)
			    T)))

(DEFCONST *EMACS-FILE-TRANSLATIONS*
	  '(((:HOST "AI") (:HOST "XX"))
	    ((:HOST "MC") (:HOST "XX"))
	    ((:DEVICE "DSK") (:DEVICE "PS"))
	    ((:DIRECTORY "EMACS1") (:DIRECTORY "EMACS"))
	    ((:DIRECTORY "EMACS1" :TYPE :UNSPECIFIC) (:TYPE "EMACS"))
	    ((:DIRECTORY "INFO" :TYPE :UNSPECIFIC) (:TYPE "INFO"))
	    ((:NAME "AUTO-S") (:NAME "AUTO-SAVE-MODE"))
	    ((:NAME "FORTRA") (:NAME "FORTRAN"))
	    ((:NAME "COLUMN") (:NAME "COLUMNS"))
	    ((:NAME "JOURNA") (:NAME "JOURNAL"))
	    ((:NAME "OUTLIN") (:NAME "OUTLINE"))
	    ((:NAME "PICTUR") (:NAME "PICTURE"))
	    ((:NAME "JUSTIF") (:NAME "JUSTIFY"))
	    ((:NAME "NCOLUM") (:NAME "NCOLUMNS"))
	    ((:NAME "@") (:NAME "ATSIGN"))
	    ((:NAME "INFOD") (:NAME "INFO" :TYPE "DOCOND"))
	    ((:NAME "EMACSD") (:NAME "EMACS" :TYPE "DOCOND"))
	    ((:NAME "TAGSD") (:NAME "TAGS" :TYPE "DOCOND"))	   
	    ((:NAME "TEACH") (:NAME "TEACH-EMACS"))
	    ((:NAME "TEACH" :TYPE "EMACS") (:TYPE "INIT"))
	    ((:NAME "TEACH" :TYPE :UNSPECIFIC) (:TYPE "TXT"))
	    ((:NAME "EMACS" :TYPE "TECO") (:NAME "TECO" :TYPE "INIT"))
	    ((:DIRECTORY "INFO" :NAME "INFO" :TYPE "EMACS") (:NAME "EMACS" :TYPE "INIT"))
	    ((:NAME "[PRFY]") (:NAME "PURIFY" :TYPE ":EJ"))
	    ((:NAME "[PURE]") (:NAME "EMACS" :TYPE ":EJ"))
	    ((:NAME "[TMCS]") (:NAME "TMACS" :TYPE ":EJ"))
	    ((:NAME "[WORD]") (:NAME "WORDAB" :TYPE ":EJ"))
	    ((:NAME "TECORD") (:DIRECTORY "INFO" :TYPE "INFO"))
	    ((:NAME "TECO" :TYPE "ARCHIV") (:DIRECTORY "INFO"))
	    ((:NAME "INFO" :TYPE "EMACS") (:NAME "INFO-EMACS" :TYPE "TXT"))
	    ((:DIRECTORY "GLS") (:DIRECTORY "INFO" :TYPE "TXT"))
	    ((:DIRECTORY "EMACS" :TYPE :UNSPECIFIC) (:TYPE ""))
	    ((:DIRECTORY ".TECO.") (:DIRECTORY "EMACS"))
	    ))

(DEFCONST *EMACS-DO-NOT-COPY*
	  '((:NAME "RMAIL")
	    (:NAME "RMAILL")
	    (:NAME "RMAILX")
	    (:NAME "RMAILZ")
	    (:NAME "TS")
	    (:NAME "TECPUR")
	    (:DIRECTORY "INFO" :NAME "DIR")
	    (:NAME "-READ-" :TYPE "-THIS-")
	    (:NAME "*")
	    (:NAME "NEWS")
	    (:NAME "[DIRE]")
	    (:NAME "[RMAI]")
	    (:NAME "[TIME]")
	    (:NAME "QSEND")
	    (:NAME "DIRED")
	    (:NAME "LEDIT")
	    (:NAME "LEDMAC")
	    (:NAME "LISTP")
	    (:NAME "RUNOFF")
	    (:NAME "DOCIX")
	    (:TYPE "COMPRS")
	    (:NAME "TSINFO")
	    (:NAME "TSRMAI")
	    (:NAME "TSTCH")
	    (:NAME "VMAIL")
	    (:NAME "XLMS")
	    (:NAME "SCHEME")
	    (:NAME "SCHEDI")
	    (:NAME "SKETCH")
	    (:NAME "MAXIMA")
	    (:NAME "LISPT")
	    (:DIRECTORY "INFO" :NAME "EMACS")
	    (:DIRECTORY "INFO" :NAME "TAGS")
	    (:DIRECTORY "INFO" :NAME "INFO" :TYPE :UNSPECIFIC)
	    ))

(DEFCONST *EMACS-COPY-ALL-FILES*
	  '("AI: EMACS; * *"
	    "AI: EMACS1; * *"
	    "AI: INFO; * *"
	    "AI: .TECO.; * *"
	    "MC: EMACS1; * *"
	    "AI: GLS; JARGON *"
	    ))

(DEFUN TRANSLATE-PATHNAME (PATHNAME TRANSLATIONS &AUX OPTIONS PLIST)
  (SETQ PLIST (LOCF OPTIONS))
  (LOOP FOR XL IN TRANSLATIONS
	WHEN (LOOP FOR (IND PROP) ON (CAR XL) BY 'CDDR
		   ALWAYS (IF (EQ IND ':HOST)
			      (FUNCALL (FS:PATHNAME-HOST PATHNAME)
				       ':PATHNAME-HOST-NAMEP PROP)
			      (EQUAL PROP (FUNCALL PATHNAME IND))))
	DO (LOOP FOR (IND PROP) ON (CADR XL) BY 'CDDR
		 WHEN (NULL (GET PLIST IND))
		 DO (PUTPROP PLIST PROP IND)))
  (LOOP FOR IND IN '(:HOST :DEVICE :DIRECTORY :NAME :TYPE :VERSION)
	WHEN (NULL (GET PLIST IND))
	DO (PUTPROP PLIST (FUNCALL PATHNAME IND) IND))
  (APPLY #'FS:MAKE-PATHNAME OPTIONS))

(DEFUN PATHNAMES-TO-BE-COPIED (FILES TRANSLATIONS DONT-COPY)
  (SETQ FILES (LOOP FOR FILE IN FILES
		    NCONC (CDR (FS:DIRECTORY-LIST FILE))))
  (LOOP FOR FILE IN FILES
	AS PATHNAME = (CAR FILE)
	WHEN (LOOP FOR NOCOPY IN DONT-COPY
		   THEREIS (LOOP FOR (IND PROP) ON NOCOPY BY 'CDDR
				 ALWAYS (EQUAL PROP (FUNCALL PATHNAME IND))))
	DO (SETQ FILES (DELQ FILE FILES)))
  (SETQ FILES (SORTCAR FILES #'ZWEI:DIRED-PATHNAME-LESSP))
  (ZWEI:DIRED-COMPUTE-GREATER-THANS FILES)
  (SETQ FILES (DEL-IF #'(LAMBDA (FILE)
			  (OR (GET FILE ':LINK-TO)
			      (AND (NOT (GET FILE ':NEWEST))
				   (NUMBERP (FUNCALL (CAR FILE) ':VERSION)))))
		      FILES))
  (LOOP FOR FILE IN FILES
	DO (PUTPROP FILE (TRANSLATE-PATHNAME (CAR FILE) TRANSLATIONS) ':COPY-TO))
  FILES)

(DEFUN COPY-FILES (FILES TRANSLATIONS DONT-COPY &OPTIONAL NOQUERY
		   &AUX DESTINATION-SETTABLE-PROPERTIES)
  (SETQ FILES (PATHNAMES-TO-BE-COPIED FILES TRANSLATIONS DONT-COPY))
  ;This would be nice and modular, but doesn't work because on TOPS-20
  ;you aren't allowed to list an empty directory for some reason
  ;(SETQ DESTINATION-SETTABLE-PROPERTIES
  ;	(GET (CAR (FS:DIRECTORY-LIST (GET (CAR FILES) ':COPY-TO))) ':SETTABLE-PROPERTIES))
  ;So kludge it instead...
  (OR (TYPEP (GET (CAR FILES) ':COPY-TO) 'FS:ITS-PATHNAME)
      (SETQ DESTINATION-SETTABLE-PROPERTIES '(:BYTE-SIZE :LENGTH-IN-BYTES)))
  (SETQ FILES (LOOP FOR FILE IN FILES
		    AS FROM = (CAR FILE)
		    AND TO = (GET FILE ':COPY-TO)
		    WHEN (OR NOQUERY
			     (AND (LET ((STREAM (OPEN TO '(:PROBE :NOERROR))))
				    (OR (STRINGP STREAM)
					(> (GET FILE ':CREATION-DATE)
					   (FUNCALL STREAM ':CREATION-DATE))))
				  (FQUERY NIL "Copy ~A  ~A? " FROM TO)))
		    COLLECT FILE))
  (LOOP FOR FILE IN FILES
	AS FROM = (CAR FILE)
	AND TO = (GET FILE ':COPY-TO)
	DO (FORMAT T "~&Copying ~A  ~A~%" FROM TO)
	   (COPY-PDP10-FILE FROM TO)
	   (AND (MEMQ ':BYTE-SIZE DESTINATION-SETTABLE-PROPERTIES)
		(MEMQ ':LENGTH-IN-BYTES DESTINATION-SETTABLE-PROPERTIES)
		(FS:CHANGE-PATHNAME-PROPERTIES TO T
					  ':BYTE-SIZE (GET FILE ':BYTE-SIZE)
					  ':LENGTH-IN-BYTES (GET FILE ':LENGTH-IN-BYTES)))))

(DEFUN COPY-LISPM-FILES (FILES TRANSLATIONS DONT-COPY &OPTIONAL NOQUERY)
  (SETQ FILES (PATHNAMES-TO-BE-COPIED FILES TRANSLATIONS DONT-COPY))
  (SETQ FILES (LOOP FOR FILE IN FILES
		    AS FROM = (CAR FILE)
		    AND TO = (GET FILE ':COPY-TO)
		    WHEN (AND (STRINGP (OPEN TO '(:PROBE :NOERROR)))
			      (OR NOQUERY (FQUERY NIL "Copy ~A  ~A? " FROM TO)))
		    COLLECT FILE))
  (LOOP FOR FILE IN FILES
	AS FROM = (CAR FILE)
	AND TO = (GET FILE ':COPY-TO)
	WITH BS AND TYP
	AS BINARY-P = (COND ((MEMQ (SETQ BS (GET FILE ':BYTE-SIZE)) '(8 7)) NIL)
			    ((EQ BS 16.) T)
			    ((EQUAL (SETQ TYP (FUNCALL FROM ':TYPE)) "QFASL") T)
			    ((EQUAL TYP "LISP") NIL)
			    ((FQUERY '(:BEEP T)
				     "Binary-P for ~A? " FROM)))
	DO (COPY-LISPM-FILE FROM TO BINARY-P T)))

(DEFUN COPY-ALL (FROM-PATHNAME TO-PATHNAME &OPTIONAL NOQUERY)
  (SETQ FROM-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS FROM-PATHNAME
			(FS:MAKE-PATHNAME ':HOST FS:USER-LOGIN-MACHINE
					  ':DIRECTORY ':WILD
					  ':NAME ':WILD
					  ':TYPE ':WILD
					  ':VERSION ':WILD)
			':WILD ':WILD)
	TO-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS TO-PATHNAME FROM-PATHNAME ':WILD ':WILD))
  (COPY-FILES (LIST FROM-PATHNAME)
	      (LOOP FOR FIELD IN '(:HOST :DEVICE :DIRECTORY :NAME :TYPE :VERSION)
		    AS FROM = (FUNCALL FROM-PATHNAME FIELD)
		    AS TO = (FUNCALL TO-PATHNAME FIELD)
		    UNLESS (EQUAL FROM TO)
		      UNLESS (MEMBER FROM '(:WILD NIL :UNSPECIFIC))
		        UNLESS (MEMBER TO '(:WILD NIL :UNSPECIFIC))
			  COLLECT `((,FIELD ,FROM) (,FIELD ,TO)))
	      NIL
	      NOQUERY))

(DEFUN COPY-EMACS ()
  (COPY-FILES *EMACS-COPY-ALL-FILES* *EMACS-FILE-TRANSLATIONS* *EMACS-DO-NOT-COPY*))

(DEFCONST *LISPM-FILE-TRANSLATIONS*
	'(((:HOST "AI") (:HOST "XX"))
	  ((:DEVICE "DSK") (:DEVICE "SS"))
	  ((:DIRECTORY "ZMAIL") (:DIRECTORY "MMCM.ZMAIL"))
	  ((:DIRECTORY "LMWIN") (:DIRECTORY "MMCM.LMWIN"))
	  ((:DIRECTORY "LMPAT") (:DIRECTORY "MMCM.LMPAT"))
	  ((:DIRECTORY "LCADR") (:DIRECTORY "MMCM.LCADR"))
	  ((:DIRECTORY "LISPM") (:DIRECTORY "MMCM.LISPM"))
	  ((:DIRECTORY "LISPM2") (:DIRECTORY "MMCM.LISPM2"))
	  ((:DIRECTORY "LMCONS") (:DIRECTORY "MMCM.LMCONS"))
	  ((:DIRECTORY "LMDEMO") (:DIRECTORY "MMCM.LMDEMO"))
	  ((:DIRECTORY "LMIO") (:DIRECTORY "MMCM.LMIO"))
	  ((:DIRECTORY "LMIO1") (:DIRECTORY "MMCM.LMIO1"))
	  ((:DIRECTORY "ZWEI") (:DIRECTORY "MMCM.ZWEI"))
	  ((:DIRECTORY "LISPM1") (:DIRECTORY "MMCM.LISPM1"))
	  ((:TYPE :UNSPECIFIC) (:TYPE "LISP"))))

(DEFCONST *LISPM-LISPM-FILE-TRANSLATIONS*
	'(((:HOST "AI") (:HOST "local"))
	  ((:DEVICE "DSK") (:DEVICE :UNSPECIFIC))
	  ((:DIRECTORY "LISPM2") (:DIRECTORY ">LISPM2"))
	  ((:DIRECTORY "LMIO") (:DIRECTORY ">LMIO"))
	  ((:DIRECTORY "LISPM") (:DIRECTORY ">LISPM"))
	  ((:DIRECTORY "LMWIN") (:DIRECTORY ">LMWIN"))
	  ((:DIRECTORY "LMIO1") (:DIRECTORY ">LMIO1"))
	  ((:DIRECTORY "ZWEI") (:DIRECTORY ">ZWEI"))
	  ((:DIRECTORY "ZMAIL") (:DIRECTORY ">ZMAIL"))
	  ((:DIRECTORY "LCADR") (:DIRECTORY ">LCADR"))
	  ((:DIRECTORY "LMCONS") (:DIRECTORY ">LMCONS"))
	  ((:DIRECTORY "LMDEMO") (:DIRECTORY ">LMDEMO"))
	  ((:DIRECTORY "LISPM1") (:DIRECTORY ">LISPM1"))
	  ((:DIRECTORY "LMPAT") (:DIRECTORY ">LMPAT"))
	  ((:TYPE :UNSPECIFIC) (:TYPE "LISP"))))

(DEFCONST *LISPM-DO-NOT-COPY*
	'((:NAME "TS")
	  (:NAME "-READ-" :TYPE "-THIS-")
	  (:NAME "*")
	  (:TYPE "FASL")
	  ))

(DEFCONST *LISPM-COPY-ALL-FILES*
	'(
	  "AI: LISPM2; * *"
	  "AI: LMIO; * *"
	  "AI: LISPM; * *"
	  "AI: LMWIN; * *"
	  "AI: LMIO1; * *"
	  "AI: ZWEI; * *"
	  "AI: ZMAIL; * *"
	  "AI: LCADR; * *"
	  "AI: LMCONS; * *"
	  "AI: LMDEMO; * *"
	  "AI: LISPM1; * *"
;	  "AI: LMPAT; * *"
	  ))

(DEFUN COPY-LISPM ()
  (COPY-FILES *LISPM-COPY-ALL-FILES* *LISPM-FILE-TRANSLATIONS* *LISPM-DO-NOT-COPY* T))

(DEFUN COPY-ZMAIL ()
  (COPY-FILES '("AI: ZMAIL; * *") *LISPM-FILE-TRANSLATIONS* *LISPM-DO-NOT-COPY*))

(DEFCONST *LMFONT-DO-NOT-COPY*
	'((:NAME "-READ-")
	  (:NAME "AR1")
	  (:NAME "ARC")
	  (:TYPE "XFILE")))

(DEFCONST *LMFONT-TRANSLATIONS*
	'(((:HOST "AI") (:HOST "XX"))
	  ((:DEVICE "DSK") (:DEVICE "SS"))
	  ((:DIRECTORY "LMFONT") (:DIRECTORY "MMCM.LMFONT"))
	  ((:DEVICE "AR1") (:DEVICE "SS"))
	  ((:DEVICE "ARC") (:DEVICE "SS"))
	  ((:DEVICE "AR1" :TYPE :UNSPECIFIC) (:TYPE "AL"))
	  ((:DEVICE "ARC" :TYPE :UNSPECIFIC) (:TYPE "AST"))
	  ))

(DEFCONST *LMFONT-COPY-ALL-FILES*
	'("AI: LMFONT; * *"
	  "AI: AR1: LMFONT; * *"
	  "AI: ARC: LMFONT; * *"))

(DEFUN COPY-LMFONT ()
  (COPY-FILES *LMFONT-COPY-ALL-FILES* *LMFONT-TRANSLATIONS* *LMFONT-DO-NOT-COPY*))

(DEFCONST *LMMAN-FILE-TRANSLATIONS*
	'(((:HOST "AI") (:HOST "local"))
	  ((:TYPE :UNSPECIFIC) (:TYPE "TEXT"))))

(DEFCONST *LMMAN-DO-NOT-COPY*
	'((:NAME "-READ-" :TYPE "-THIS-")
	  ))

(DEFUN CREATE-DIRECTORIES-FOR-SYSTEM (SYSTEM &AUX DIRS)
  (SETQ DIRS (LOOP FOR FILE IN (SI:SYSTEM-SOURCE-FILES SYSTEM)
		   COLLECT (FUNCALL (FUNCALL FILE ':TRANSLATED-PATHNAME) ':DIRECTORY)))
  (SETQ DIRS (SI:ELIMINATE-DUPLICATES DIRS 'EQUAL))
  (LOOP FOR DIR IN DIRS
	AS PATH = (FS:MAKE-PATHNAME ':HOST "local" ':DIRECTORY (STRING-APPEND #/> DIR)
				    ':NAME "foo" ':TYPE "temp" ':VERSION ':NEWEST)
	AS MSG = (OPEN PATH '(:PROBE))
	WHEN (AND (STRINGP MSG)
		  (EQUAL (FS:FILE-PROCESS-ERROR MSG PATH NIL T) "NSD"))
	DO (FORMAT T "~&Creating ~A" DIR)
	   (FUNCALL PATH ':CREATE-DIRECTORY)))


(DEFUN COPY-SYSTEM (SYSTEM &OPTIONAL (FULL-P T))
  (CREATE-DIRECTORIES-FOR-SYSTEM SYSTEM)
  (LOOP FOR FILE IN (SI:SYSTEM-SOURCE-FILES SYSTEM)
	DO (IF FULL-P
	       (COPY-SOURCE-FILE FILE)
	       (COPY-FILE-TO-LOCAL-WITH-VERSION (FUNCALL FILE ':NEW-PATHNAME ':TYPE "LISP"
							 ':VERSION ':NEWEST)
						NIL))))

(DEFUN COPY-SOURCE-FILE (FILE &AUX NEWEST)
  (SETQ NEWEST (PROBEF (FUNCALL FILE ':NEW-PATHNAME ':TYPE "LISP" ':VERSION ':NEWEST)))
  (COPY-FILE-TO-LOCAL NEWEST NIL)
  (LET ((QFASL (PROBEF (FUNCALL FILE ':NEW-PATHNAME ':TYPE "QFASL" ':VERSION ':NEWEST))))
    (COND (QFASL
	   (LET* ((PLIST (FS:FILE-PROPERTY-LIST QFASL))
		  (INSTALLED (GET (LOCF PLIST) ':QFASL-SOURCE-FILE-UNIQUE-ID)))
	     (AND INSTALLED (EQ (FUNCALL QFASL ':VERSION) ':NEWEST)
		  (SETQ QFASL (FUNCALL QFASL ':NEW-VERSION (FUNCALL INSTALLED ':VERSION))))
	     (AND INSTALLED (NEQ INSTALLED NEWEST) (PROBEF INSTALLED)
		  (COPY-FILE-TO-LOCAL INSTALLED NIL)))
	   (COPY-FILE-TO-LOCAL QFASL T)))))

(DEFUN COPY-FILE-TO-LOCAL-WITH-VERSION (PATH BINARY-P)
  (COPY-FILE-TO-LOCAL (PROBEF PATH) BINARY-P))

(DEFUN COPY-FILE-TO-LOCAL (PATH BINARY-P)
  (AND (EQUAL (FUNCALL PATH ':TYPE) ':UNSPECIFIC)
       (SETQ PATH (FUNCALL PATH ':NEW-TYPE "LISP")))
  (COPY-LISPM-FILE PATH (FS:MAKE-PATHNAME
			  ':HOST "local"
			  ':DIRECTORY (STRING-APPEND #/> (FUNCALL PATH ':DIRECTORY))
			  ':NAME (FUNCALL PATH ':NAME) ':TYPE (FUNCALL PATH ':TYPE)
			  ':VERSION (FUNCALL PATH ':VERSION))
		   BINARY-P T))

(DEFUN COPY-LISPM-LISPM (&OPTIONAL (NOQUERY T))
  (COPY-LISPM-FILES *LISPM-COPY-ALL-FILES* *LISPM-LISPM-FILE-TRANSLATIONS* *LISPM-DO-NOT-COPY*
		    NOQUERY))

(DEFUN COPY-LMMAN ()
  (COPY-LISPM-FILES '("AI: LMMAN; * *") *LMMAN-FILE-TRANSLATIONS* *LMMAN-DO-NOT-COPY*
		    T))

(DEFUN COPY-UCADR (&OPTIONAL (VERSION SYS:%MICROCODE-VERSION-NUMBER))
  (COPY-FILE-TO-LOCAL (FS:MERGE-PATHNAME-DEFAULTS (FORMAT NIL "AI: LCADR; UCADR ~D" VERSION))
		      NIL)
  (LOOP FOR TYPE IN '("MCR" "SYM" "TBL")
	AS BIN IN '(T NIL NIL)
	WITH FROM = (FS:PARSE-PATHNAME "AI: LISPM1; UCADR")
	AND TO = (FS:PARSE-PATHNAME "local:>lispm1>ucadr")
	DO (COPY-LISPM-FILE (FUNCALL FROM ':NEW-TYPE-AND-VERSION TYPE VERSION)
			    (FUNCALL TO ':NEW-TYPE-AND-VERSION TYPE VERSION)
			    BIN T)))

(DEFUN COPY-FILES-FROM-LOCAL-TO-HOST (HOST)
  (LOOP FOR DIR IN (LOOP FOR X IN (CDR (FS:DIRECTORY-LIST "local:>*.*.*"))
			 COLLECT (FUNCALL (CAR X) ':NAME))
	DO (LOOP FOR FILE IN (CDR (FS:DIRECTORY-LIST
				    (FS:MAKE-PATHNAME ':HOST "local"
						      ':DIRECTORY (STRING-APPEND #/> DIR)
						      ':NAME ':WILD ':TYPE ':WILD
						      ':VERSION ':WILD)))
		 AS FROM = (CAR FILE)
		 AS TO = (FUNCALL FROM ':NEW-PATHNAME ':HOST HOST ':DIRECTORY DIR)
		 WHEN (NOT (PROBEF TO))
		 DO (COPY-LISPM-FILE FROM TO (= (GET FILE ':BYTE-SIZE) 16.) T))))

(DEFCONST *DPL-COPY-ALL-FILES*
	'(
	  "SCRC:<DPL>*.*;*"
	  "SCRC:<DPL1>*.*;*"
	  ))

(DEFCONST *DPL-FILE-TRANSLATIONS*
	'(((:HOST "SCRC") (:HOST "local"))
	  ((:DEVICE "DSK") (:DEVICE :UNSPECIFIC))
	  ((:DIRECTORY "DPL") (:DIRECTORY ">DPL"))
	  ((:DIRECTORY "DPL1") (:DIRECTORY ">DPL1"))))

(DEFUN COPY-DPL ()
  (COPY-LISPM-FILES *DPL-COPY-ALL-FILES* *DPL-FILE-TRANSLATIONS* NIL T))

(DEFCONST *SCRC-LMFONT-TRANSLATIONS*
	'(((:HOST "SCRC") (:HOST "local"))
	  ((:DEVICE "DSK") (:DEVICE :UNSPECIFIC))
	  ((:DIRECTORY "LMFONTS") (:DIRECTORY ">LMFONTS"))
	  ))

(DEFCONST *SCRC-LMFONT-COPY-ALL-FILES*
	'(
	  "SCRC:<LMFONTS>*.QFASL;0"
	  ))

(DEFUN COPY-SCRC-LMFONT ()
  (COPY-LISPM-FILES *SCRC-LMFONT-COPY-ALL-FILES* *SCRC-LMFONT-TRANSLATIONS* NIL T))

(DEFCONST *CUBE-TRANSLATIONS*
	'(((:HOST "SCRC") (:HOST "local"))
	  ((:DEVICE "DSK") (:DEVICE :UNSPECIFIC))
	  ((:DIRECTORY "CUBE") (:DIRECTORY ">CUBE"))
	  ))

(DEFCONST *CUBE-COPY-ALL-FILES*
	'(
	  "SCRC:<CUBE>*.*;0"
	  ))

(DEFUN COPY-CUBE ()
  (COPY-LISPM-FILES *CUBE-COPY-ALL-FILES* *CUBE-TRANSLATIONS* NIL T))

(DEFUN COPY-DISTRIBUTION-FILES (SITE)
  (LOOP FOR NAME IN '("DEFAULT-SITE" "SITE" "HOSTS" "LMLOCS" "INSTALLATION")
	FOR TYPE IN '("LISP" "LISP" "TEXT" "LISP" "TEXT")
	AS FROM = (FS:MAKE-PATHNAME ':HOST "SCRC" ':DIRECTORY "DISTRIBUTION"
				    ':NAME (STRING SITE) ':TYPE NAME ':VERSION ':NEWEST)
	WHEN (SETQ FROM (PROBEF FROM))
	DO (LET ((TO (FS:MAKE-PATHNAME ':HOST "local"
				       ':DIRECTORY (STRING-APPEND ">DISTRIBUTION>" SITE)
				       ':NAME NAME ':TYPE TYPE
				       ':VERSION (FUNCALL FROM ':VERSION))))
	     (AND (NOT (PROBEF TO))
		  (COPY-LISPM-FILE FROM TO NIL T)))))

(DEFUN COPY-DISTS (SITES)
  (MAPC 'COPY-DISTRIBUTION-FILES SITES))

(DEFUN COPY-PATCHES (&OPTIONAL SYSTEMS)
  (OR SYSTEMS (SETQ SYSTEMS (MAPCAR #'SI:PATCH-NAME SI:PATCH-SYSTEMS-LIST)))
  (LOOP FOR SYSTEM IN SYSTEMS
	DO (COPY-PATCHES-FOR-SYSTEM SYSTEM "local")))

(DEFUN COPY-PATCHES-FOR-SYSTEM (SYSTEM-NAME TO-HOST
				&AUX MAJOR MAXMINOR PATCH-DIRECTORY FROM-DIR TO-DIR
				     SAME-DIRECTORY-P)
  (FORMAT T "~&Patches for ~A:~%" SYSTEM-NAME)
  (OR (SETQ PATCH-DIRECTORY (SI:SYSTEM-PATCH-DIRECTORY (SI:FIND-SYSTEM-NAMED SYSTEM-NAME)))
      (FERROR NIL "System ~A not patchable" SYSTEM-NAME))
  (SETQ SAME-DIRECTORY-P (SI:PATCH-DIRECTORY-SAME-DIRECTORY-P PATCH-DIRECTORY)
	FROM-DIR (FUNCALL (SI:PATCH-DIRECTORY-PATHNAME PATCH-DIRECTORY) ':TRANSLATED-PATHNAME)
	TO-DIR (FUNCALL FROM-DIR ':NEW-PATHNAME ':HOST TO-HOST))
  ;; Totally cretinous local file system.
  (LET ((DIR (FUNCALL TO-DIR ':DIRECTORY)))
    (COND ((FUNCALL (FUNCALL TO-DIR ':HOST) ':PATHNAME-HOST-NAMEP "local")
	   (AND ( (AREF DIR 0) #/>)
		(SETQ TO-DIR (FUNCALL TO-DIR ':NEW-DIRECTORY (STRING-APPEND #/> DIR)))))
	  ((FUNCALL (FUNCALL FROM-DIR ':HOST) ':PATHNAME-HOST-NAMEP "local")
	   (AND (= (AREF DIR 0) #/>)
		(SETQ TO-DIR (FUNCALL TO-DIR ':NEW-DIRECTORY (SUBSTRING DIR 1)))))))
  (MULTIPLE-VALUE (MAJOR MAXMINOR)
    (SI:GET-SYSTEM-VERSION SYSTEM-NAME))
  (COPY-PATCH-FILE-1 FROM-DIR TO-DIR SYSTEM-NAME SAME-DIRECTORY-P NIL
		     ':SYSTEM-DIRECTORY)
  (COPY-PATCH-FILE-1 FROM-DIR TO-DIR SYSTEM-NAME SAME-DIRECTORY-P NIL
		     ':VERSION-DIRECTORY MAJOR)
  (LOOP FOR I FROM 1 TO MAXMINOR
	DO (COPY-PATCH-FILE-1 FROM-DIR TO-DIR SYSTEM-NAME SAME-DIRECTORY-P NIL
			      ':PATCH-FILE MAJOR I "LISP")
	   (COPY-PATCH-FILE-1 FROM-DIR TO-DIR SYSTEM-NAME SAME-DIRECTORY-P T
			      ':PATCH-FILE MAJOR I "QFASL"))
  )

(DEFUN COPY-PATCH-FILE-1 (FROM-DIR TO-DIR SYSTEM-NAME SAME-DIRECTORY-P BINARY-P TYPE
			  &REST ARGS &AUX FROM TO)
  (SETQ FROM (LEXPR-FUNCALL FROM-DIR ':PATCH-FILE-PATHNAME SYSTEM-NAME SAME-DIRECTORY-P
			    TYPE ARGS))
  (COND ((SETQ FROM (PROBEF FROM))
	 (SETQ TO (LEXPR-FUNCALL TO-DIR ':PATCH-FILE-PATHNAME SYSTEM-NAME SAME-DIRECTORY-P
				 TYPE ARGS))
	 (SETQ TO (FUNCALL TO ':NEW-VERSION (FUNCALL FROM ':VERSION)))
	 (AND (NOT (PROBEF TO))
	      (COPY-LISPM-FILE FROM TO BINARY-P T)))))

(DEFUN MAKE-CTL-FILE-FOR-PATCHES (OUTFILE &OPTIONAL SYSTEMS)
  (OR SYSTEMS (SETQ SYSTEMS (MAPCAR #'SI:PATCH-NAME SI:PATCH-SYSTEMS-LIST)))
  (LET ((STREAM (ZWEI:MAKE-FILE-BUFFER-STREAM OUTFILE)))
    (FORMAT STREAM "TAPEX
CW")
    (LOOP FOR SYSTEM IN SYSTEMS
	  DO (MAKE-CTL-FILE-FOR-SYSTEM SYSTEM STREAM))
    (FORMAT STREAM "~%N~%")))

(DEFUN MAKE-CTL-FILE-FOR-SYSTEM (SYSTEM STREAM &AUX NOW-MAJOR MAXMINOR THEN-MAJOR THEN-MINOR)
  (MULTIPLE-VALUE (NOW-MAJOR MAXMINOR)
    (SI:GET-SYSTEM-VERSION SYSTEM))
  (FORMAT T "~&Present version of ~A is ~D.~D~%" SYSTEM NOW-MAJOR MAXMINOR)
  (MULTIPLE-VALUE (THEN-MAJOR THEN-MINOR)
    (READ-SYSTEM-VERSION SYSTEM))
  (OR THEN-MAJOR (SETQ THEN-MAJOR NOW-MAJOR))
  (COND (( THEN-MAJOR NOW-MAJOR)
	 (WRITE-PATCH-FILE-FOR-SYSTEM SYSTEM STREAM ':SYSTEM-DIRECTORY)
	 (SETQ THEN-MINOR 0)))
  (COND (( THEN-MINOR MAXMINOR)
	 (WRITE-PATCH-FILE-FOR-SYSTEM SYSTEM STREAM ':VERSION-DIRECTORY NOW-MAJOR)
	 (LOOP FOR I FROM (1+ THEN-MINOR) TO MAXMINOR
	       DO (WRITE-PATCH-FILE-FOR-SYSTEM SYSTEM STREAM ':PATCH-FILE NOW-MAJOR I "QFASL")
		  (WRITE-PATCH-FILE-FOR-SYSTEM SYSTEM STREAM ':PATCH-FILE NOW-MAJOR I "LISP")		  ))))

(DEFUN READ-SYSTEM-VERSION (SYSTEM &AUX MAJOR MINOR)
  (FORMAT T "~&  Distributed version of ~A: (Minor or Major.Minor) " SYSTEM)
  (LET ((LINE (READLINE))
	(IDX 0))
    (LET ((DOT-IDX (STRING-SEARCH-CHAR #/. LINE)))
      (COND (DOT-IDX
	     (SETQ MAJOR (PARSE-NUMBER LINE IDX DOT-IDX 10.))
	     (SETQ IDX (1+ DOT-IDX)))))
    (SETQ MINOR (PARSE-NUMBER LINE IDX NIL 10.)))
  (VALUES MAJOR MINOR))

(DEFUN WRITE-PATCH-FILE-FOR-SYSTEM (SYSTEM STREAM TYPE &REST ARGS)
  (FUNCALL STREAM ':LINE-OUT (FUNCALL (LEXPR-FUNCALL #'SI:PATCH-SYSTEM-PATHNAME
						     SYSTEM TYPE ARGS)
				      ':STRING-FOR-HOST))
  (FUNCALL STREAM ':TYO #\CR))

(DEFUN COPY-ALL (FROM TO)
  (SETQ FROM (FS:GET-PATHNAME-HOST FROM)
	TO (FS:GET-PATHNAME-HOST TO))
  (DOLIST (FILE (CDR (FS:DIRECTORY-LIST (FS:MAKE-PATHNAME ':HOST FROM
							  ':DIRECTORY ':WILD
							  ':NAME ':WILD ':TYPE ':WILD
							  ':VERSION ':WILD))))
    (LET ((FROM-FILE (CAR FILE)))
      (OR (EQ (FUNCALL FROM-FILE ':TYPE) ':DIRECTORY)
	  (LET ((TO-FILE (FUNCALL FROM-FILE ':NEW-PATHNAME ':HOST TO)))
	    (OR (PROBEF TO-FILE)
		(WITH-OPEN-FILE (ISTREAM FROM-FILE ':DIRECTION ':INPUT ':CHARACTERS ':DEFAULT)
		  (WITH-OPEN-FILE (OSTREAM TO-FILE ':DIRECTION ':OUTPUT
					   ':CHARACTERS (FUNCALL ISTREAM ':CHARACTERS))
		    (FORMAT T "~&Copying ~A to ~A (~:[binary~;ascii~])"
			    FROM-FILE TO-FILE
			    (FUNCALL OSTREAM ':CHARACTERS))
		    (STREAM-COPY-UNTIL-EOF ISTREAM OSTREAM))))
	    (FS:CHANGE-FILE-PROPERTIES TO-FILE NIL
				       ':CREATION-DATE (GET FILE ':CREATION-DATE)
				       ':AUTHOR (GET FILE ':AUTHOR)))))))
