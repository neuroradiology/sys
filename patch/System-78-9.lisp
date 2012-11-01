;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.9
;;; Reason: Redisplay enhancement
;;; Written 12/10/81 14:43:26 by MMcM,
;;; while running on Lisp Machine Seven from band 7
;;; with System 78.8, ZMail 38.1, microcode 836.



; From file DISPLA > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

;;;   This function is the only entrypoint into redisplay proper.  It does
;;; anything anyone would ever need (ha ha).
;;; The first argument is the window to be redisplayed.
;;; The second argument is a keyword which says what kind of recentering
;;; is desired, and the third and fourth arguments are parameters whose meanings depend
;;; on the second argument.  Valid keywords are:
;;;   Recentering type:       Parameters:      Meaning:
;;;         :NONE             (none)           Do not recenter at all.
;;;         :POINT            raster_fraction  Keep POINT's blinker on the window.  If
;;;                                            neccesary, recenter.  Recenter in such a
;;;                                            way that the blinker ends up near the given
;;;                                            raster line.  If raster_fraction is NIL, 
;;;                                            the *CENTERING-FRACTION* is used.
;;;         :START            line, index      Recenter so that the position (line, index)
;;;                            (or a bp)       appears at the top of the window.
;;;         :RELATIVE         fixnum           Recenter to scroll window up <fixnum> lines.
;;;                                            If <fixnum> is negative, scroll down.
;;;         :ABSOLUTE         fixnum           Recenter so that point is at raster <fixnum>.

;;; The elements of a window PLINE are the:
;;; PLINE-LINE			;Editor line displayed, NIL if blank
;;; PLINE-FROM-INDEX		;First character displayed
;;; PLINE-TO-INDEX		;Last character displayed+1
;;; PLINE-TICK			;TICK as of last time pline updated on display
;;; PLINE-MARKING-LEFT		;NIL no marking, or X coord of start of region-marking
;;; PLINE-MARKING-WIDTH		;Horizontal extent of marking
;;; PLINE-TEXT-WIDTH		;Horizontal extent of text
;;; Note that for non-continuation lines, PLINE-TEXT-WIDTH includes a little
;;; extra for the pseudo-space at the end of the line which corresponds to the #\CR.
;;; But for continuation lines, it does not include the ! at the end of the line.
;;; (It does now, but that should be regarded as a bug in SHEET-LINE-OUT)
;;; PLINE-TEXT-WIDTH is used only for region marking.

(DEFUN REDISPLAY (WINDOW &OPTIONAL (RECENTER-TYPE ':POINT)
                         RC1 RC2 (FORCE-TO-COMPLETION-P NIL))
  (PREPARE-WINDOW-FOR-REDISPLAY WINDOW)
  (LET ((N-PLINES (WINDOW-N-PLINES WINDOW))
	(POINT (WINDOW-POINT WINDOW))
	(SHEET (WINDOW-SHEET WINDOW))
	(DEGREE (WINDOW-REDISPLAY-DEGREE WINDOW))
	(POINT-BLINKER (WINDOW-POINT-BLINKER WINDOW))
	(INTERVAL (WINDOW-INTERVAL WINDOW))
	(START-BP (WINDOW-START-BP WINDOW))
	(NOW (TICK))
	POINT-PLINE)
    (PROG ABORT-REDISPLAY
	  ((LH (TV:SHEET-LINE-HEIGHT SHEET))
	   (POINT-LINE (BP-LINE POINT))
	   (POINT-INDEX (BP-INDEX POINT))
	   (TOP-LINE (BP-LINE START-BP))
	   (TOP-INDEX (BP-INDEX START-BP))
	   (LAST-BP (INTERVAL-LAST-BP INTERVAL))
	   (INITIAL-DEGREE DEGREE)
	   ;; Bind *INTERVAL* in case we decide to call any primitives, e.g. inside the
	   ;; special-blinker which blinks matching parens.  This is an implicit argument.
	   (*INTERVAL* INTERVAL))
      ;; We prefer not to start redisplay in the middle of a line.
      ;; The start-bp of the window may have ended up there via a command like rubout at
      ;; the beginning of the window or may have been scrolled there explicitly.  If the
      ;; top line has changed so that continuation may not be necessary any more, attempt
      ;; recentering.
      (AND (EQ RECENTER-TYPE ':POINT)
	   (NOT (ZEROP TOP-INDEX))
	   (> N-PLINES 1)
	   (> (LINE-TICK TOP-LINE) (PLINE-TICK WINDOW 0))
	   (LET ((NEW-TOP-INDEX (IF (EQ TOP-LINE (BP-LINE (INTERVAL-FIRST-BP INTERVAL)))
				    (BP-INDEX (INTERVAL-FIRST-BP INTERVAL))
				    0)))
	     (AND ( TOP-INDEX NEW-TOP-INDEX)
		  (< (MULTIPLE-VALUE-BIND (NIL Y)
			 (TV:SHEET-COMPUTE-MOTION SHEET 0 0 TOP-LINE NEW-TOP-INDEX POINT-INDEX
						  NIL 0 17777777 17777777)
		       Y)
		     (* LH N-PLINES))
		  (SETQ RECENTER-TYPE ':ABSOLUTE))))
      ;; :POINT recentering is a conditional sort of :ABSOLUTE recentering.
      ;; So decide here whether :ABSOLUTE recentering should be done.
      (AND (EQ RECENTER-TYPE ':POINT)
	   (COND (( DEGREE DIS-MARK-GOES))
		 ;; When typing at the end of the line, dont try to compute POINT-PLINE yet,
		 ;; but wait till after we have faked out the pline-text-width correctly.
		 ;; Otherwise it will be much, much slower
		 ((AND (= DEGREE DIS-LINE)
		       (EQ (WINDOW-REDISPLAY-LINE WINDOW) POINT-LINE)
		       (NEQ POINT-LINE (PLINE-LINE WINDOW (1- N-PLINES)))
		       (OR ( (1+ (WINDOW-REDISPLAY-INDEX WINDOW)) POINT-INDEX)
			   (= (MULTIPLE-VALUE-BIND (NIL Y)
				  (TV:SHEET-COMPUTE-MOTION SHEET 0 0 POINT-LINE 0 POINT-INDEX
							   T)
				Y)
			      0))))
		 ((SETQ POINT-PLINE (PLINE-OF-POINT T WINDOW POINT)))
		 (T (SETQ RECENTER-TYPE ':ABSOLUTE))))
      ;; If recentering is needed, do it, and see what changes it made.
      (COND ((MEMQ RECENTER-TYPE '(:NONE :POINT)))
	    (T (RECENTER-WINDOW WINDOW RECENTER-TYPE RC1 RC2)
	       (SETQ DEGREE (WINDOW-REDISPLAY-DEGREE WINDOW)
		     START-BP (WINDOW-START-BP WINDOW)
		     TOP-LINE (BP-LINE START-BP)
		     TOP-INDEX (BP-INDEX START-BP)
		     POINT-LINE (BP-LINE POINT)
		     POINT-INDEX (BP-INDEX POINT))
	       ;; Gobble point-pline as computed by recenter-window
	       ;; if it is accurate.
	       (SETQ POINT-PLINE (WINDOW-LAST-POINT-PLINE WINDOW))
	       (OR (AND (EQ POINT-LINE (PLINE-LINE WINDOW POINT-PLINE))
			( (PLINE-FROM-INDEX WINDOW POINT-PLINE) POINT-INDEX)
			(< POINT-INDEX (PLINE-TO-INDEX WINDOW POINT-PLINE)))
		   (SETQ POINT-PLINE NIL))))
      ;; Now we have TOP-LINE and TOP-INDEX, and possibly POINT-PLINE.

      ;; First, handle the case where just one line needs to be updated.
      (AND (= DEGREE DIS-LINE)
	   (LET ((LINE (WINDOW-REDISPLAY-LINE WINDOW))
		 (INDEX (WINDOW-REDISPLAY-INDEX WINDOW)))
	     (LET ((P (FIND-BP-IN-WINDOW WINDOW LINE INDEX))
		   (LINE-LENGTH (IF (EQ LINE (BP-LINE LAST-BP)) (BP-INDEX LAST-BP)
				    (LINE-LENGTH LINE)))
		   LEN DWID)
	       ;; LEN gets the raster position in the pline P
	       ;; of the character in LINE at position INDEX.
	       (AND P (SETQ LEN (STRING-WIDTH LINE (PLINE-FROM-INDEX WINDOW P) INDEX SHEET)))
	       (COND ((AND P
			   ;; If P and LEN say we are at the start of a continuation line,
			   ;; then maybe they are wrong
			   ;; (if the contin line has been exactly deleted).
			   (OR (NOT (ZEROP LEN))
			       (ZEROP INDEX)))
		      ;; Go to the place in the line where changes start. Clear from there.
		      ;; This means that any region marking from there on is gone now.
		      (COND ((AND (PLINE-MARKING-LEFT WINDOW P)
				  (< (PLINE-MARKING-LEFT WINDOW P) LEN))
			     (SETF (PLINE-MARKING-WIDTH WINDOW P)
				   (MIN (- LEN (PLINE-MARKING-LEFT WINDOW P))
					(PLINE-MARKING-WIDTH WINDOW P))))
			    (T (SETF (PLINE-MARKING-LEFT WINDOW P) NIL)
			       (SETF (PLINE-MARKING-WIDTH WINDOW P) NIL)))
		      ;; If the character is wider than it claims to be, draw an extra
		      ;; character, since the clear-eol will erase data.
		      (OR (ZEROP INDEX)
			  (LET ((CH (AREF LINE (1- INDEX))))
			    (AND (< CH 200)
				 (LET ((FONT (AREF (TV:SHEET-FONT-MAP SHEET)
						   (LDB %%CH-FONT CH)))
				       CWT)
				   (AND (SETQ CWT (FONT-CHAR-WIDTH-TABLE FONT))
					(LET ((CWID (AREF CWT (SETQ CH (LDB %%CH-CHAR CH))))
					      (RWID (FED:FONT-CHAR-MIN-RASTER-WIDTH FONT CH)))
					  (AND (> RWID CWID) (SETQ DWID CWID))))))))
		      (MULTIPLE-VALUE-BIND (I TW)
			  (TV:SHEET-LINE-OUT SHEET LINE INDEX LINE-LENGTH LEN (* LH P) DWID)
			;; We have output the first PLINE of this line
			(SETF (PLINE-TO-INDEX WINDOW P) I)
			(SETF (PLINE-TEXT-WIDTH WINDOW P)
			      (IF ( I LINE-LENGTH) TW	;Continuation needed
				  (+ TW (TV:SHEET-CHAR-WIDTH SHEET)))) ;Allow for CR
			(SETF (PLINE-TICK WINDOW P) NOW)
			;; See if plines below this need to be redisplayed, due
			;; to line-continuation issues
			(COND ((AND (< (1+ P) N-PLINES)
				    (OR ( I LINE-LENGTH)
					( (+ TW (TV:SHEET-INSIDE-LEFT SHEET))
					   (TV:SHEET-INSIDE-RIGHT SHEET))
					(EQ (PLINE-LINE WINDOW (1+ P)) LINE)))
			       (SETQ DEGREE DIS-TEXT POINT-PLINE NIL)
			       ;; If we are just creating a new continuation line, make it
			       ;; still look munged, so REDISPLAY-BLT can understand.
			       (OR (EQ (PLINE-LINE WINDOW (1+ P)) LINE)
				   (SETF (PLINE-TICK WINDOW P) -1))))))
		     (T
		      (SETQ DEGREE DIS-TEXT POINT-PLINE NIL))))))
	  ;; If all the window should be redisplayed, mark each pline as unknown.
	  (COND (( DEGREE DIS-ALL)
		 (DO I 0 (1+ I) (= I N-PLINES)
		     (SETF (PLINE-TICK WINDOW I) -1))))
	  (COND (( DEGREE DIS-TEXT)
		 ;; In case we abort before we are done, don't forget what's needed.
		 (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-TEXT)
		 (SETF (WINDOW-LAST-BP-DISPLAYED-P WINDOW) NIL)
		 (DO L (WINDOW-SPECIAL-BLINKER-LIST WINDOW) (CDR L) (NULL L)
		   (TV:BLINKER-SET-VISIBILITY (CDAR L) NIL))
		 ;; Abort now if input available
		 (AND (NOT FORCE-TO-COMPLETION-P)
		      (FUNCALL STANDARD-INPUT ':LISTEN)
		      (RETURN-FROM ABORT-REDISPLAY NIL))
		 ;; Attempt to do insert and delete line cleverness.
		 (REDISPLAY-BLT WINDOW)
		 ;; This might have invalidated the value of POINT-PLINE.
		 ;; It won't be hard to recompute, so do so.
		 (SETQ POINT-PLINE NIL)
		 ;; First loop over actual lines.
		 (DO-NAMED LINES
			   ((LINE TOP-LINE (LINE-NEXT LINE))
			    (FROM-INDEX TOP-INDEX 0)
			    (TO-INDEX)
			    (PLINE 0)
			    (STOP-LINE (BP-LINE LAST-BP)))
			   (NIL)
		   ;; Between lines, check for input available and abort if so.
		   (AND (NOT FORCE-TO-COMPLETION-P)
			(ZEROP (\ PLINE 30.))
			(FUNCALL STANDARD-INPUT ':LISTEN)
			(RETURN-FROM ABORT-REDISPLAY NIL))
		   (SETQ TO-INDEX (IF (EQ LINE STOP-LINE) (BP-INDEX LAST-BP)
				      (LINE-LENGTH LINE)))
		   ;; Now loop over the plines of this line.
		   (DO NIL (NIL)
		     (AND ( PLINE N-PLINES) (RETURN-FROM LINES))
		     ;; Check for a line that has not been changed.
		     (COND ((AND (EQ LINE (PLINE-LINE WINDOW PLINE))
				 (> (PLINE-TICK WINDOW PLINE) (LINE-TICK LINE))
				 (= (PLINE-FROM-INDEX WINDOW PLINE) FROM-INDEX))
			    (SETQ FROM-INDEX (PLINE-TO-INDEX WINDOW PLINE)))
			   (T
			    ;; This should work differently
			    (LET ((FROB (GET (LOCF (LINE-PLIST LINE)) ':DIAGRAM)) I TW)
			      (COND (FROB
				     (TV:SHEET-SET-CURSORPOS SHEET 0 (* LH PLINE))
				     (TV:SHEET-CLEAR-EOL SHEET)
				     (FUNCALL FROB ':DRAW LINE SHEET)
				     (SETQ I 1 TW 0))
				    (T
				     (MULTIPLE-VALUE (I TW)
				       (TV:SHEET-LINE-OUT SHEET LINE
							  FROM-INDEX TO-INDEX
							  0 (* LH PLINE)))))
			      (SETF (PLINE-LINE WINDOW PLINE) LINE)
			      (SETF (PLINE-FROM-INDEX WINDOW PLINE) FROM-INDEX)
			      (SETF (PLINE-TO-INDEX WINDOW PLINE) I)
			      (SETF (PLINE-TICK WINDOW PLINE) NOW)
			      (SETF (PLINE-MARKING-LEFT WINDOW PLINE) NIL)
			      (SETF (PLINE-TEXT-WIDTH WINDOW PLINE)
				    (IF ( I (LINE-LENGTH LINE)) TW	;Continuation needed
					(+ TW (TV:SHEET-CHAR-WIDTH SHEET))))	;Allow for CR
			      (SETQ FROM-INDEX I))))
		     (SETQ PLINE (1+ PLINE))
		     ;; This is >, not , because if line isn't cont'd then PLINE-TO-PLINE
		     ;; counts the phony CR which is output by SHEET-LINE-OUT.
		     (AND (> FROM-INDEX TO-INDEX) (RETURN)))
		   ;; Check for the last line in the interval.
		   (COND ((EQ LINE STOP-LINE)
			  (SETF (WINDOW-LAST-BP-DISPLAYED-P WINDOW) T)
			  (OR (< PLINE N-PLINES) (RETURN-FROM LINES))
			  (AND (NULL (PLINE-LINE WINDOW PLINE))
			       (PLINE-TICK WINDOW PLINE) (> (PLINE-TICK WINDOW PLINE) 0)
			       (RETURN-FROM LINES)) ;Return if screen already blanked
			  ;; Clean out the rest of the window beneath it.  Then exit.
			  (TV:SHEET-SET-CURSORPOS SHEET 0 (* LH PLINE))
			  (TV:SHEET-CLEAR-EOF SHEET)
			  (DO PLINE PLINE (1+ PLINE) ( PLINE N-PLINES)
			      (SETF (PLINE-LINE WINDOW PLINE) NIL)
			      (SETF (PLINE-TICK WINDOW PLINE) NOW)
			      (SETF (PLINE-MARKING-LEFT WINDOW PLINE) NIL))
			  (RETURN-FROM LINES))))))
	  (COND (( DEGREE DIS-BPS)
		 ;; BPs have moved.  Reposition the POINT blinker.
		 (OR POINT-PLINE
		     (SETQ POINT-PLINE (FIND-BP-IN-WINDOW WINDOW POINT-LINE POINT-INDEX))
		     (EQ RECENTER-TYPE ':NONE)
		     (IF (AND (= INITIAL-DEGREE DIS-LINE) (= DEGREE DIS-TEXT))
			 ;;Somewhat anomalous case, try again with greater redisplay degree
			 (RETURN (REDISPLAY WINDOW RECENTER-TYPE RC1 RC2
					    FORCE-TO-COMPLETION-P))
			 (FERROR NIL "Recenter type ~S left point outside the window"
				 RECENTER-TYPE)))
		 (COND ((NULL POINT-PLINE)
			;; POINT is not on the window, so make it go away.
			(TV:BLINKER-SET-VISIBILITY POINT-BLINKER NIL))
		       (T
			;; POINT is on the window, find its Y position.
			(TV:BLINKER-SET-VISIBILITY
			  POINT-BLINKER (IF (EQ SHEET TV:SELECTED-WINDOW) ':BLINK
					    (TV:BLINKER-DESELECTED-VISIBILITY POINT-BLINKER)))
			(COND ((NOT (EQ POINT-LINE (PLINE-LINE WINDOW POINT-PLINE)))
			       (DPRINT POINT-LINE POINT-PLINE (PLINE-LINE WINDOW POINT-PLINE))
			       (FERROR NIL "Position of POINT on window is screwed up:")))
			(SET-BLINKER-SIZE POINT WINDOW POINT-BLINKER
					  (TV:SHEET-COMPUTE-MOTION SHEET 0 0 POINT-LINE
							(PLINE-FROM-INDEX WINDOW POINT-PLINE)
							POINT-INDEX)
					  (* LH POINT-PLINE) SHEET)
			(SETF (WINDOW-LAST-POINT-PLINE WINDOW) POINT-PLINE)))
		 ;; Blink the parens, etc.
		 (DOLIST (BL (WINDOW-SPECIAL-BLINKER-LIST WINDOW))
		   (FUNCALL (CAR BL) (CDR BL) WINDOW POINT START-BP))))
	  (COND (( DEGREE DIS-MARK-GOES)
		 ;; The region marking may have changed.
		 (UPDATE-REGION-MARKING WINDOW)))
	  ;;The character under the mouse also
	  (AND ( DEGREE DIS-BPS) (MOUSE-RETHINK WINDOW))
	  (AND ( DEGREE DIS-TEXT) (NOTIFY-SCROLL-BAR WINDOW))
	  (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-NONE)
	  )))

)

