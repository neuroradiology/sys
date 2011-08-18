;;; Font hacking function and commands -*-Mode:LISP;Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Change the font in the given area
(DEFUN CHANGE-FONT-INTERVAL (START-BP &OPTIONAL END-BP IN-ORDER-P (FONT-NUM *FONT*))
  (GET-INTERVAL START-BP END-BP IN-ORDER-P)
  (MUNG-BP-INTERVAL START-BP)
  (DO ((LINE (BP-LINE START-BP) (LINE-NEXT LINE))
       (LIMIT-LINE (BP-LINE END-BP))
       (START-INDEX (BP-INDEX START-BP) 0)
       (LAST-LINE-P))
      (NIL)
      (SETQ LAST-LINE-P (EQ LINE LIMIT-LINE))
      (OR (ZEROP FONT-NUM) (EQ (ARRAY-TYPE LINE) 'ART-FAT-STRING)
	  (SETQ LINE (SET-LINE-ARRAY-TYPE LINE 'ART-FAT-STRING)))
      (MUNG-LINE LINE)
      (DO ((INDEX START-INDEX (1+ INDEX))
	   (LIMIT-INDEX (IF LAST-LINE-P (BP-INDEX END-BP) (LINE-LENGTH LINE))))
	  (( INDEX LIMIT-INDEX))
	  (ASET (DPB FONT-NUM %%CH-FONT (AREF LINE INDEX)) LINE INDEX))
      (AND LAST-LINE-P (RETURN NIL)))
  DIS-TEXT)						;For the sake of commands

;;; Input the name of a font, either as a letter from A through Z,  for a mini-buffer,
;;; mouse-left for the font of some other character in the window, mouse-right for a
;;; menu of the current window's fonts.
;;; USE-PREVIOUS-P means dont ask again with successive font change commands
(LOCAL-DECLARE ((SPECIAL *SAVE-FONT-NUM*))
(OR (BOUNDP '*SAVE-FONT-NUM*) (SETQ *SAVE-FONT-NUM* 0))

(DEFUN INPUT-FONT-NAME (USE-PREVIOUS-P &OPTIONAL (PROMPT "Font ID: ") &AUX NUM)
  (SETQ *CURRENT-COMMAND-TYPE* 'FONT-CHANGE)
  (COND ((AND USE-PREVIOUS-P (EQ *LAST-COMMAND-TYPE* 'FONT-CHANGE))
	 *SAVE-FONT-NUM*)
	(T
	 (TYPEIN-LINE PROMPT)
	 (DO ((CH)
	      (ALIST (WINDOW-FONT-ALIST *WINDOW*)))
	     (NIL)
	     (LET-GLOBALLY ((*GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*
		    (IF (NULL ALIST)
			"Use font of character pointed at."
			"Use font of character pointed at, click right for menu of fonts.")))
	       (TYPEIN-LINE-ACTIVATE
		 (SETQ CH (FUNCALL STANDARD-INPUT ':MOUSE-OR-KBD-TYI))))
	     (COND ((OR (= CH #/G) (= CH #/g))
		    (BARF))
		   ((= CH #/)
		    (SETQ NUM (INPUT-FONT-NAME-FROM-MINI-BUFFER))
		    (RETURN NIL))
		   ((= CH #\MOUSE-1-1)
		    (COND ((SETQ CH (MOUSE-CHAR *WINDOW*))
			   (SETQ NUM (LDB %%CH-FONT CH))
			   (RETURN NIL))))
		   ((= CH #\MOUSE-3-1)
		    (COND ((NULL ALIST)
			   (BEEP))
			  ((SETQ CH (TV:MENU-CHOOSE ALIST))
			   (DO ((I 0 (1+ I))	;Have the font itself, but want the number
				(L (WINDOW-FONT-ALIST *WINDOW*) (CDR L)))
			       ((EQ (CDAR L) CH) (SETQ NUM I)))
			   (RETURN NIL))))
		   ((AND ( (SETQ CH (CHAR-UPCASE CH)) #/A) ( CH #/Z))
		    (SETQ NUM (- CH #/A))
		    (RETURN NIL))
		   ((OR (= CH #\HELP) (= CH #/?))
		    (TYPEIN-LINE "Type a font letter, ~
				  or altmode to enter a new font in a mini-buffer, ~@
				  or mouse a character left for its font~:[~;, ~
				  or mouse-right for a menu~].~%" ALIST)
		    (TYPEIN-LINE-MORE "Font ID: "))
		   (T
		    (BEEP))))
	 (TYPEIN-LINE-MORE "~C (~A)" (+ NUM #/A) (CAR (NTH NUM (WINDOW-FONT-ALIST *WINDOW*))))
	 (SETQ *SAVE-FONT-NUM* NUM))))
);LOCAL-DECLARE

(DEFUN INPUT-FONT-NAME-FROM-MINI-BUFFER (&AUX FONT NEW-P)
  (SETQ FONT (COMPLETING-READ-FROM-MINI-BUFFER "Font ID:" (WINDOW-FONT-ALIST *WINDOW*) T))
  (COND ((EQUAL FONT "")
	 *FONT*)
	((STRINGP FONT)
	 (SETQ NEW-P T)	;Wasn't previously in the A-list, add it
	 (PKG-BIND "FONTS"
	   (SETQ FONT (READ-FROM-STRING FONT '*EOF*)))
	 (SETQ FONT (FONT-NAME (FUNCALL (TV:SHEET-GET-SCREEN (WINDOW-SHEET *WINDOW*))
					':PARSE-FONT-DESCRIPTOR FONT)))
	 (SETQ FONT (CONS (GET-PNAME FONT) (SYMEVAL FONT)))
	 (LET ((OLD-LIST (WINDOW-FONT-ALIST *WINDOW*)))
	   (AND ( (LENGTH OLD-LIST) 26.) (BARF "The maximum number of fonts is 26."))
	   (OR OLD-LIST (SETQ OLD-LIST (LET ((FONT0 (CURRENT-FONT *WINDOW* 0)))
					 (LIST (CONS (GET-PNAME (FONT-NAME FONT0)) FONT0)))))
	   (REDEFINE-FONTS *WINDOW* (APPEND OLD-LIST	;Copy OLD-LIST so won't be EQUAL
					    (LIST FONT))))))
  (SETQ FONT (FIND-POSITION-IN-LIST FONT (WINDOW-FONT-ALIST *WINDOW*)))
  (TYPEIN-LINE (IF NEW-P "Added as font " "Font "))  ;Caller will append letter & name of font
  FONT)

(DEFUN REDEFINE-FONTS (WINDOW FONT-ALIST)
  (COND ((NOT (EQUAL FONT-ALIST (WINDOW-FONT-ALIST WINDOW)))
	 (LET ((SHEET (WINDOW-SHEET WINDOW) ))
	   (FUNCALL SHEET ':SET-FONT-MAP (MAPCAR #'CDR FONT-ALIST))
	   (FUNCALL SHEET ':SET-CURRENT-FONT (AREF (TV:SHEET-FONT-MAP SHEET) 0)))
	 (MUST-REDISPLAY WINDOW DIS-ALL)
	 (SETF (WINDOW-FONT-ALIST WINDOW) FONT-ALIST))))

(DEFUN UPDATE-FONT-NAME ()
  (SETQ *FONT-NAME* (AND (WINDOW-FONT-ALIST *WINDOW*)
			 (FORMAT NIL "~C (~A)" (+ *FONT* #/A)
				 (CAR (NTH *FONT* (WINDOW-FONT-ALIST *WINDOW*)))))))

;;; This is used by ZMACS
(DEFUN SET-BUFFER-FONTS-FROM-FILE (BUFFER GENERIC-PATHNAME &AUX FONTS)
  (SETQ FONTS (FUNCALL GENERIC-PATHNAME ':GET ':FONTS))
  (COND ((AND FONTS (SYMBOLP FONTS))
         (SETQ FONTS (INTERN (GET-PNAME FONTS) "FONTS"))
         (SETQ FONTS (AND (BOUNDP FONTS) (LIST (CONS (GET-PNAME FONTS) (SYMEVAL FONTS))))))
        (T
         (DO ((FL FONTS (CDR FL))
              (L NIL)
              (F))
             ((NULL FL)
              (SETQ FONTS (NREVERSE L)))
             (SETQ F (INTERN (GET-PNAME (CAR FL)) "FONTS"))
	     (COND ((NOT (BOUNDP F))
		    (LOAD (FORMAT NIL "SYS: FONTS; ~A" F) "FONTS" T)
		    (OR (BOUNDP F)
			;;If font not loaded, substitute default to keep font numbers ok
			(SETQ F (FONT-NAME TV:(SCREEN-DEFAULT-FONT DEFAULT-SCREEN))))))
	     (PUSH (CONS (GET-PNAME F) (SYMEVAL F)) L))))
  (AND (TYPEP BUFFER 'BUFFER)
       (SETF (BUFFER-SAVED-FONT-ALIST BUFFER) FONTS))
  FONTS)

;;; Font hacking stream
(LOCAL-DECLARE ((SPECIAL *LINE* *INDEX* *LAST-LINE* *LAST-INDEX* *STOP-INDEX* *UNRCHF*
			 *FONT-FLAG*))

;;; *LINE*, *INDEX* point to the next character to be returned.
;;; *STOP-INDEX* is the place on the current line at which to stop (usually the end).
;;; *LAST-LINE*, *LAST-INDEX* is where the interval ends.
;;; If *INDEX* is NIL, we are at the end-of-file.
;;; *FONT-FLAG* is the current font-number, or T if we have written the  and need to write
;;; the number;  this should really be incorporated with INTERVAL-IO and hacked to
;;; be faster (by supporting LINE-OUT, looking ahead somehow)
;;; *FONT-FLAG* is even hairier for diagrams and i dont feel like describing it now.

(DEFSELECT (INTERVAL-WITH-FONTS-IO INTERVAL-WITH-FONTS-IO-DEFAULT-HANDLER)
  (:TYI (&OPTIONAL EOF &AUX CH)
   (COND (*UNRCHF*
	  (PROG1 *UNRCHF* (SETQ *UNRCHF* NIL)))
	 ((STRINGP *FONT-FLAG*)
	  (SETQ CH (AREF *FONT-FLAG* *INDEX*))
	  (AND ( (SETQ *INDEX* (1+ *INDEX*)) *STOP-INDEX*)
	       (SETQ *FONT-FLAG* -1
		     *INDEX* 0
		     *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*) *LAST-INDEX*
							      (LINE-LENGTH *LINE*))))
	  CH)
	 ((NULL *INDEX*)
	  (AND EOF (ERROR EOF)))
	 ((< *INDEX* *STOP-INDEX*)
	  (SETQ CH (AREF *LINE* *INDEX*))
	  (COND ((EQ *FONT-FLAG* T)
		 (SETQ *FONT-FLAG* (LDB %%CH-FONT CH))
		 (+ #/0 *FONT-FLAG*))
		((LDB-TEST 2001 *FONT-FLAG*)
		 (PROG1 (LDB %%CH-CHAR *FONT-FLAG*)
			(SETQ *FONT-FLAG* (LDB %%CH-FONT *FONT-FLAG*))))
		(( *FONT-FLAG* (LDB %%CH-FONT CH))
		 (SETQ *FONT-FLAG* T)
		 #/)
		(T
		 (SETQ *INDEX* (1+ *INDEX*))
		 (AND (= (SETQ CH (LDB %%CH-CHAR CH)) #/)
	              (SETQ *FONT-FLAG* (DPB 1 2001 (DPB *FONT-FLAG* %%CH-FONT CH))))
	         CH)))
	 ((EQ *LINE* *LAST-LINE*)
	  (SETQ *INDEX* NIL)
	  (AND EOF (ERROR EOF)))
	 (T
	  (SETQ *LINE* (LINE-NEXT *LINE*))
	  (IF (SETQ CH (GET (LOCF (LINE-PLIST *LINE*)) ':DIAGRAM))
	      (LET* ((STRING (FUNCALL CH ':STRING-FOR-FILE *LINE*))
		     (LENGTH (STRING-LENGTH STRING)))
		(AND (PLUSP LENGTH)
		     (SETQ *FONT-FLAG* STRING *INDEX* 0 *STOP-INDEX* LENGTH)))
	      (SETQ *INDEX* 0 *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
					       *LAST-INDEX*
					       (LINE-LENGTH *LINE*))))
	  #\CR)))
  (:UNTYI (CH)
   (SETQ *UNRCHF* CH))
  (:TYO (CH)
   (AND (NUMBERP *FONT-FLAG*) (LDB-TEST 2001 *FONT-FLAG*)
	(IF (= CH #/)
	    (SETQ *FONT-FLAG* (LDB 0020 *FONT-FLAG*) CH (DPB -1 %%CH-FONT CH))
	    (SETQ *FONT-FLAG* T)))
   (COND ((EQ *FONT-FLAG* T)
	  (SETQ *FONT-FLAG* (IF (= CH #/#) 'DIAG-1 (- CH #/0))))
	 ((EQ *FONT-FLAG* 'DIAG-1)
	  (SETQ *FONT-FLAG* 'DIAG-2 *STOP-INDEX* 0))
	 ((EQ *FONT-FLAG* 'DIAG-2)
	  (IF (= CH #\SP)
	      (SETQ *FONT-FLAG* (MAKE-ARRAY NIL ART-STRING 10. NIL '(0)))
	      (SETQ *STOP-INDEX* (+ (* *STOP-INDEX* 10.) (- CH #/0)))))
	 ((STRINGP *FONT-FLAG*)
	  (IF (= CH #\CR)
	      (SETQ *INDEX* NIL
		    *FONT-FLAG* (MAKE-INSTANCE (READ-FROM-STRING *FONT-FLAG*)
					       ':NUMBER-OF-LINES *STOP-INDEX*))
	      (ARRAY-PUSH-EXTEND *FONT-FLAG* CH)))
; The flavor definition for RESTORABLE-LINE-DIAGRAM-MIXIN has been commented out
; at the end of this file.  Since that broke this, this goes too.
;	 ((TYPEP *FONT-FLAG* 'RESTORABLE-LINE-DIAGRAM-MIXIN)
;	  (PROG ()
;	    (OR *INDEX*
;		(COND ((< (SETQ *STOP-INDEX* (1- *STOP-INDEX*)) 0)
;		       (SETQ *INDEX* 0 *FONT-FLAG* (OR (EQ CH #/) 0))
;		       (RETURN))
;		      (T
;		       (SETQ *INDEX* (CREATE-LINE ART-STRING 0 NIL))
;		       (INSERT-LINE-WITH-LEADER *INDEX* *LINE*))))
;	    (COND ((= CH #\CR)
;		   (PUTPROP (LOCF (LINE-PLIST *INDEX*)) *FONT-FLAG* ':DIAGRAM)
;		   (FUNCALL *FONT-FLAG* ':ADD-LINE *INDEX* *INDEX*)
;		   (SETF (LINE-LENGTH *INDEX*) 0)
;		   (SETQ *INDEX* NIL))
;		  (T
;		   (ARRAY-PUSH-EXTEND *INDEX* CH)))))
	 ((= CH #/)
	  (SETQ *FONT-FLAG* (DPB 1 2001 *FONT-FLAG*)))
	 (T
	  (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*) (IN-CURRENT-FONT CH *FONT-FLAG*))))
	    (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))))
  (:UNTYO-MARK ()
   (CREATE-BP *LINE* *INDEX*))
  (:UNTYO (MARK)
   (DELETE-INTERVAL MARK (CREATE-BP *LINE* *INDEX*) T)
   (SETQ *LINE* (BP-LINE MARK) *INDEX* (BP-INDEX MARK)))
  (:READ-BP ()
   (CREATE-BP *LINE* *INDEX*))
  (:LINE-OUT (LINE)
   (INTERVAL-WITH-FONTS-IO ':STRING-OUT LINE)
   (PROG1 *LINE* (INTERVAL-WITH-FONTS-IO ':TYO #\CR))))
);LOCAL-DECLARE

(DEFUN INTERVAL-WITH-FONTS-IO-DEFAULT-HANDLER (OP &OPTIONAL ARG1 &REST REST)
  (STREAM-DEFAULT-HANDLER 'INTERVAL-WITH-FONTS-IO OP ARG1 REST))

(DEFCOM COM-CHANGE-FONT-CHAR "Change the font of one or more characters forward.
Reads the name of the new font in the echo area." ()
  (LET ((BP1 (FORWARD-CHAR (POINT) *NUMERIC-ARG* T)))
    (CHANGE-FONT-INTERVAL (POINT) BP1 NIL (INPUT-FONT-NAME
					    T
					    "Change font of characters to (Font ID): "))
    (MOVE-BP (POINT) BP1)
    DIS-TEXT))
  
(DEFCOM COM-CHANGE-FONT-WORD "Change the font of one or more words forward.
Reads the name of the new font in the echo area." ()
  (LET ((BP1 (FORWARD-WORD (POINT) *NUMERIC-ARG* T)))
    (CHANGE-FONT-INTERVAL (POINT) BP1 NIL (INPUT-FONT-NAME
					    T
					    "Change for of words to (Font ID): "))
    (AND (PLUSP *NUMERIC-ARG*) (MOVE-BP (POINT) BP1))
    DIS-TEXT))

(DEFCOM COM-CHANGE-FONT-REGION "Change the font between point and the mark.
Reads the name of the new font in the echo area." ()
  (REGION (BP1 BP2)
      (CHANGE-FONT-INTERVAL BP1 BP2 T (INPUT-FONT-NAME
					NIL
					"Change font of region to (Font ID): "))))

(DEFCOM COM-CHANGE-DEFAULT-FONT "Set the default font.
Reads the name of the new font in the echo area." ()
  (SETQ *FONT* (INPUT-FONT-NAME NIL "Set default font to (Font ID): "))
  (UPDATE-FONT-NAME)
  DIS-BPS)					;This may change the size of the blinker

(DEFCOM COM-SET-FONTS "Change the set of fonts to use.
Reads a list of fonts from the mini-buffer." ()
  (LET ((TEM (DO ((FL (WINDOW-FONT-ALIST *WINDOW*) (CDR FL))
		  (STR (MAKE-ARRAY NIL 'ART-STRING 100 NIL '(0)))
		  (FIL "" " "))
		 ((NULL FL) STR)
	       (SETQ STR (STRING-NCONC STR FIL (CAAR FL))))))
    (TEMP-KILL-RING TEM
       (SETQ TEM (TYPEIN-LINE-READLINE "font1 font2 ...:")))
    (PKG-BIND "FONTS"
       (SETQ TEM (READ-FROM-STRING (STRING-APPEND "(" TEM ")"))))
    (OR (LISTP TEM)
	(BARF "Please type in the printed represetation of a list of at least one element."))
    (DO ((L TEM (CDR L))
	 (FONT)
	 (AL NIL))
	((NULL L)
	 (SETQ TEM (NREVERSE AL)))
      (SETQ FONT (CAR L))
      (COND ((NOT (SYMBOLP FONT))
	     (BARF "~S is not the name of a font" FONT))
	    ((NOT (BOUNDP FONT))
	     (LOAD (FORMAT NIL "SYS: FONTS; ~A" FONT) "FONTS" T)
	     (OR (BOUNDP FONT) (BARF "~S is not a defined font" FONT))))
      (PUSH (CONS (GET-PNAME FONT) (SYMEVAL FONT)) AL))
    (REDEFINE-FONTS *WINDOW* TEM)
    (UPDATE-FONT-NAME))
  DIS-ALL)

(DEFCOM COM-LIST-FONTS "List the loaded fonts.
With an argument, also lists the font files on the file computer." ()
  (FORMAT T "Loaded fonts:~%")
  (FUNCALL STANDARD-OUTPUT ':ITEM-LIST 'FONT
	   (LOCAL-DECLARE ((SPECIAL LIST))
	     (LET ((LIST NIL))
	       (MAPATOMS-ALL #'(LAMBDA (X) (AND (BOUNDP X) (TYPEP (SYMEVAL X) 'FONT)
						(PUSH X LIST)))
			     "FONTS")
	       (SORT LIST #'STRING-LESSP))))
  (COND (*NUMERIC-ARG-P*
	 (FORMAT T "~&Plus fonts on the file computer:~%")
	 (FUNCALL STANDARD-OUTPUT ':ITEM-LIST 'FONT
		  (LOOP FOR FILE IN (FS:DIRECTORY-LIST (FS:MAKE-PATHNAME
							 ':HOST "SYS"
							 ':DIRECTORY "FONTS"
							 ':NAME ':WILD
							 ':TYPE "QFASL"
							 ':VERSION ':NEWEST)
						       ':FAST)
			WHEN (CAR FILE)
			COLLECT (INTERN (STRING-UPCASE (FUNCALL (CAR FILE) ':NAME))
					"FONTS")))))
  DIS-NONE)

(DEFCOM COM-DISPLAY-FONT "Sample a font." ()
  (LET ((FONT (COMPLETING-READ-FROM-MINI-BUFFER "Font to display:"
		(LOCAL-DECLARE ((SPECIAL LIST))
		  (LET ((LIST NIL))
		    (MAPATOMS-ALL #'(LAMBDA (X) (AND (BOUNDP X) (TYPEP (SYMEVAL X) 'FONT)
						     (PUSH (CONS (GET-PNAME X) X) LIST)))
				  "FONTS")
		    LIST))
		T)))
    (SETQ FONT (IF (STRINGP FONT) (INTERN (STRING-UPCASE FONT) "FONTS") (CDR FONT)))
    (DISPLAY-FONT FONT))
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FONT "Display" DISPLAY-FONT T
			  "Display this font.")

(DEFUN DISPLAY-FONT (FONT-SYMBOL &AUX FONT)
  (SETQ FONT (FUNCALL (TV:SHEET-GET-SCREEN *TYPEOUT-WINDOW*)
		      ':PARSE-FONT-DESCRIPTOR FONT-SYMBOL))
  (FED:COM-DISPLAY-FONT FONT *TYPEOUT-WINDOW* NIL NIL)
  (FUNCALL *TYPEOUT-WINDOW* ':CLEAR-EOF)
  NIL)

;;; Diagram stuff
(DEFFLAVOR LINE-DIAGRAM-MIXIN () ()
  (:REQUIRED-METHODS :DRAW))

(DEFMETHOD (LINE-DIAGRAM-MIXIN :ADD-LINE) (LINE &OPTIONAL CONTENTS)
  LINE CONTENTS
  )

(DEFMETHOD (LINE-DIAGRAM-MIXIN :STRING-FOR-FILE) (LINE)
  LINE
  "")

(DEFMETHOD (LINE-DIAGRAM-MIXIN :NUMBER-OF-LINES) ()
  1)

(DEFUN INSERT-DIAGRAM (BP DIAGRAM &REST OPTIONS)
  (AND (SYMBOLP DIAGRAM)
       (SETQ DIAGRAM (LEXPR-FUNCALL #'MAKE-INSTANCE DIAGRAM OPTIONS)))
  (DO ((I 0 (1+ I))
       (AT-LINE (BP-LINE BP))
       (NUMBER-OF-LINES (FUNCALL DIAGRAM ':NUMBER-OF-LINES))
       (LINE))
      (( I NUMBER-OF-LINES) DIAGRAM)
    (SETQ LINE (MAKE-LINE :MAKE-ARRAY (:AREA *LINE-AREA* :TYPE ART-STRING)
			  LINE-TICK *TICK* LINE-LENGTH 0))
    (PUTPROP (LOCF (LINE-PLIST LINE)) DIAGRAM ':DIAGRAM)
    (FUNCALL DIAGRAM ':ADD-LINE LINE)
    (INSERT-LINE-WITH-LEADER LINE AT-LINE)))

(DEFFLAVOR BLACK-BLOCK-DIAGRAM () (LINE-DIAGRAM-MIXIN))

(DEFMETHOD (BLACK-BLOCK-DIAGRAM :DRAW) (IGNORE SHEET)
  (TV:PREPARE-SHEET (SHEET)
    (SYS:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH SHEET) (TV:SHEET-LINE-HEIGHT SHEET)
			 (TV:SHEET-INSIDE-LEFT SHEET) (TV:SHEET-CURSOR-Y SHEET)
			 (TV:SHEET-CHAR-ALUF SHEET) SHEET)))

(DEFVAR *BLACK-BLOCK-DIAGRAM* 'BLACK-BLOCK-DIAGRAM)

(DEFUN INSERT-BLACK-BLOCK-DIAGRAM (BP)
  (SETQ *BLACK-BLOCK-DIAGRAM* (INSERT-DIAGRAM BP 'BLACK-BLOCK-DIAGRAM)))

(DEFFLAVOR GREY-BLOCK-DIAGRAM () (LINE-DIAGRAM-MIXIN))

(DEFMETHOD (GREY-BLOCK-DIAGRAM :DRAW) (IGNORE SHEET)
  (TV:PREPARE-SHEET (SHEET)
    (SYS:BITBLT TV:ALU-SETA (TV:SHEET-INSIDE-WIDTH SHEET) (TV:SHEET-LINE-HEIGHT SHEET)
		TV:50%-GRAY 0 0
		(TV:SHEET-SCREEN-ARRAY SHEET)
		(TV:SHEET-INSIDE-LEFT SHEET) (TV:SHEET-CURSOR-Y SHEET))))

(DEFVAR *GREY-BLOCK-DIAGRAM* 'GREY-BLOCK-DIAGRAM)

(DEFUN INSERT-GREY-BLOCK-DIAGRAM (BP)
  (SETQ *GREY-BLOCK-DIAGRAM* (INSERT-DIAGRAM BP 'GREY-BLOCK-DIAGRAM)))

(COMMENT					;Until someone needs this stuff again
(DEFFLAVOR RESTORABLE-LINE-DIAGRAM-MIXIN () ()
  (:INCLUDED-FLAVORS LINE-DIAGRAM-MIXIN)
  (:INIT-KEYWORDS :NUMBER-OF-LINES)
  (:DEFAULT-INIT-PLIST :NUMBER-OF-LINE 1)
  (:REQUIRED-METHODS :CONTENTS))

(DEFMETHOD (RESTORABLE-LINE-DIAGRAM-MIXIN :STRING-FOR-FILE) (LINE)
  (IF (FUNCALL-SELF ':FIRST-LINE-P LINE)
      (FORMAT NIL "#~D ~A~%~A" (FUNCALL-SELF ':NUMBER-OF-LINES) (TYPEP SELF)
	      (FUNCALL-SELF ':CONTENTS LINE))
      (FUNCALL-SELF ':CONTENTS LINE)))
)
