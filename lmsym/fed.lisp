;;;The font editor -*- Mode:LISP; Package:FED; Base:8 -*-
;;; Enhancements  (c) Copyright Symbolics, Inc., 1981

(DEFCONST FED-HELP-STRING
	"This is the Symbolics version of standard FED.  BSG is the developer of this
revision, all complaints to him.  Works just like the old one, except, majorly,

   1) It is mouse- and menu-driven.
   2) It is a frame, and all known window-system misinteractions have been fixed. Works
      to select it by mouse, select menu, TERMINAL S, etc., and has a EXIT command.
   3) Many minor blowages, lossages, faults, breakpoints, etc. have been fixed.
   4) Real-timeness of mouse-handling has been totally redone and improved.
   5) There is a moby, winning new multiplane feature, described at the end of this info.
   6) It is much, much faster, esp. rotating, reflecting, etc.

You must select a font before editing. Keyboard commands are still accepted, but
mousing is preferred.  The control//meta bits are inspected at mousing time for
commands that have control//meta modification of their meaning.
To edit a character, you may either mouse it up in the Select Character menu, or mouse
it out of the Show Font (d) display,or  use the old C command.  The Select Character
menu can be used to answer Merge as well.  Moon's Stretch Character command is
incorporated.  The P (Font Parameters) command has been removed, use the Change
Variable values window instead.

Draw mode works as it did (although only the grid frame is sensitive to such
mousings).  The highlighted draw mode menu ALSO works to select draw mode.
/"Hand Keys/" are accepted for the non-mouse-cursor movers.
CONTROL, META, and CONTROL-META during mousing while drawing select
CLEAR,   SET,  and DO-NOTHING   modes temporarily while they are down.
USE SCROLL-BAR SCROLLING TO MOVE THE GRID WINDOW UP AND DOWN ( and  are obsolete).

Left click on read//write file means QFASL.  Right click gives a file type menu.
Click on sample-string window to set sample-string.
Merge is obsolete - M fetches to the gray plane- see the latter part of this info.
Right click on Gray Char to fetch gray from other font and//or scale.
(fed /"fontname/") or (fed 'fontname) works too.

The following are the remaining key commands, mainly for the non-mouse cursor:

/"HAND KEYS/" (or, oldly [, ], \, //) - move non-mouse cursor
. - complement dot under non-mouse cursor
, , - move window sideways (awaiting new window flavor)
[, ], \, //, , , hands take numeric arg or meta bits
X - set hand cursor x pos     Y - set y cursor y pos
The following are new keyboard commands:

Q- Quit   QUOTE - Lisp Eval  G - Gray Char

The remaining key commands are obsoleted by menu items, but still exist.

F - select Font   C - select Character
S - Store back edited character   E - Erase all dots
D - Display entire font   V - set sample string
M - Merge in character (fetch to gray) K - Stretch character
H - move window to Home
@ - set scale (size of box) to numeric arg
R - Read file   W - Write KST file
R - Read QFASL file	W - Write QFASL file
 - reflect character    - rotate character

Multiplane feature:

FED now deals with two planes, the BLACK PLANE (which is what it has
always dealt with, and the GRAY PLANE, which is always visible
/"behind/" the black plane.  Gray points show in light gray, points on
in both the gray and black plane show in dark gray. This allows you to
view contemplated changes, other characters, etc.  The /"Gray Char/"
command, replacing /"Merge/", with all its options, brings a character
into the gray plane.  The /"Clear Gray/" menu item clears the gray
plane.  Other than that, all drawing, undrawing, rotating, etc.  is done
to the black plane.  The black and gray planes can be exchanged (this is
how you edit the gray plane, and is a fast operation) by the /"Swap
Planes/" menu item.  You can move the gray plane with respect to the
black plane with the /"Move Gray/" menu item; this is normally what you
will do before actually merging.  You merge the gray plane INTO the
black plane (probably after positioning it) with the /"Add in Gray/"
menu item.  Click left IORs the gray into the black, leaving the gray.
Click middle does the same, but clears the gray afterward. Click right
gives a menu in which all of the above as well as ANDCA and XOR
combination may be selected.

There is a register pane - it has character-storing registers.  Mouse
left on an empty register to store (nondestructively) the black plane in it.
If you mouse left on a nonempty register, it retrieves it to black.
Mousing right on any register pops up a menu of more options, such
as popping or saving to gray.   The upper-leftmost register is magic, and gets
the previous value of the black plane whenever you click left to store the
black plane into a register, in case it had something wierd in it which you
couldn't see.

Type any char to flush.")


;;; Keyboard commands and Mouse commands. Note: We DON'T use :MOUSE-BUTTONS-MIXIN,
;;; but the command-interpreter maps the mouse clicks.

(DEFCONST COMMAND-LIST '(#\MOUSE-1-1 COM-MOUSE-DRAW
			 #\MOUSE-2-1 COM-CHANGE-DRAW-MODE
			 #\MOUSE-3-1 COM-MOUSE-MOVE-CHAR-BOX
			 (#/ #/) (:BARF "Obsolete: Use scroll bar in grid window instead.")
			 (#/ #/) COM-SHIFT-WINDOW
			 (#/[ #/] #/\ #//) COM-SHIFT-CURSOR
			 (#\HAND-UP #\HAND-DOWN #\HAND-RIGHT #\HAND-LEFT) COM-SHIFT-CURSOR
			 #\SP FALSE		;Noop command
			 #/H COM-HOME
			 #/@ COM-SCALE
			 #/F COM-SPECIFY-FONT
			 #/G COM-MERGE-CHARACTER
			 #/C COM-SPECIFY-CHARACTER
			 #/M COM-MERGE-CHARACTER
			 #/S COM-SAVE-CHARACTER
			 #/E COM-ERASE-ALL
			 #/P (:BARF "Obsolete: Mouse upon the Font Parameters menu instead.")
			 #/K COM-STRETCH-CHARACTER
			 #/D COM-DISPLAY-FONT
			 #/V COM-SET-SAMPLE
			 #/ COM-REFLECT
			 #/ COM-ROTATE-CHARACTER-RIGHT
			 #/R COM-READ-FILE
			 #/X COM-SET-X
			 #/Y COM-SET-Y
			 #/W COM-WRITE-FILE
			 #/Q COM-QUIT
			 #/. COM-COMPLEMENT-SQUARE
			 (#/? #\HELP) COM-HELP
			 #\FORM COM-REFRESH
			 #\QUOTE COM-EVAL
			 ))

;;; Configuration and black-plane picture manipulation commands.

(DEFCONST MENU-COMMAND-ALIST-1
	  '(
	    ("Configure" :VALUE COM-CONFIGURE
	     :DOCUMENTATION "Reconfigure the FED frame")
	    ("Grid Size" :VALUE COM-SET-GRID-SIZE
	     :DOCUMENTATION "Set the size of grid boxes")
	    ("Center View" :VALUE COM-HOME
	     :DOCUMENTATION "Move drawing so char box moves to center")
	    ("Move View" :VALUE COM-MOUSE-SHIFT-WINDOW
	     :DOCUMENTATION "Move drawing in window arbitrarily.")
	    ("Draw Line" :VALUE COM-MOUSE-DRAW-LINE
	     :DOCUMENTATION "Click on two points; draws line between them.")
	    ("Draw Spline" :VALUE COM-MOUSE-DRAW-SPLINE
	     :DOCUMENTATION "Click on points; draws spline thru them.")
	    ("Erase All" :VALUE COM-ERASE-ALL
	     :DOCUMENTATION "Clear all dots and reset character width.")
	    ("Stretch"
	     :BUTTONS
	     ((NIL :VALUE COM-STRETCH-HORIZONTALLY)
	      (NIL :VALUE COM-STRETCH-VERTICALLY)
	      (NIL :VALUE BEEP))
	     :DOCUMENTATION
	     "L: Stretch character horizontally M: Stretch character vertically")
	    ("Rotate"
	     :BUTTONS
	     ((NIL :VALUE COM-ROTATE-CHARACTER-LEFT)
	      (NIL :VALUE COM-ROTATE-CHARACTER-180)
	      (NIL :VALUE COM-ROTATE-CHARACTER-RIGHT))
	     :DOCUMENTATION "Rotate the character: L: 90 left, M: 180, R: 90 right")
	    ("Reflect" :VALUE COM-REFLECT
	     :DOCUMENTATION "Reflect the char about axis prompted for")	
            ("Move Black" :VALUE COM-MOVE-BLACK-PLANE
	     :DOCUMENTATION "Move the black plane with respect to gray and the char box")))

;;; Gray plane manipulation commands.

(DEFCONST MENU-COMMAND-ALIST-2
	  `(
	    ("Gray Char"
	     :BUTTONS
	     ((NIL :VALUE COM-MERGE-THIS)
	      (NIL :VALUE BARF)
	      (NIL :VALUE COM-MERGE-OTHER))
	     :DOCUMENTATION
	     "L:Fetch a character to gray  R:Fetch gray from another font//scaled.")
	    ("Clear Gray" :VALUE COM-CLEAR-GRAY-PLANE
	     :DOCUMENTATION "Clear the gray plane")
	    ("Swap Gray" :VALUE COM-XCH-PLANES
	     :DOCUMENTATION "Exchange the black and gray planes")
	    ("Move Gray" :VALUE COM-MOVE-GRAY-PLANE
	     :DOCUMENTATION
	     "Move the gray plane with respect to the black plane and char box")
	    ("Add in Gray"
	     :BUTTONS
	     ((NIL :VALUE COM-MERGE-PLANES)
	      (NIL :VALUE COM-MERGE-PLANES-CLEAR-GRAY)
	      (NIL :VALUE COM-ADD-GRAY-MENU))
	     :DOCUMENTATION
	     "L: Merge the gray plane by ORing into black. M: Merge and clear gray plane R:Menu")))

;;; Bookkeeping and external world commands, and help.

(DEFCONST MENU-COMMAND-ALIST-3
	  '(("Edit Font" :VALUE COM-SPECIFY-FONT
	     :DOCUMENTATION "Select a font to edit")
	    ("List Fonts"
	     :BUTTONS
	     ((NIL :VALUE COM-LIST-FONTS)
	      (NIL :VALUE TV:BEEP)
	      (NIL :VALUE COM-LIST-ALL-FONTS))
	     :DOCUMENTATION
	     "L: List all loaded fonts R: List all fonts  -- mouse one to select")
	    ("Save Char" :VALUE COM-SAVE-CHARACTER
	     :DOCUMENTATION "Make edits to this character permanent.")
	    ("Rename Char" :VALUE COM-RENAME-CHARACTER
	     :DOCUMENTATION "Call the current image some other character")
	    ("Show Font" :VALUE COM-DISPLAY-FONT
	     :DOCUMENTATION "Show the entire font")
	    ("Set Sample" :VALUE COM-SET-SAMPLE
	     :DOCUMENTATION "Set the sample string")
	    ("Read File"
	     :BUTTONS
	     ((NIL :VALUE COM-READ-QFASL)
	      (NIL :VALUE BARF)
	      (NIL :VALUE COM-READ-ANY-FILE))
	     :DOCUMENTATION "L: Read QFASL file    R: Read some other kind menu")
	    ("Write File"
	     :BUTTONS
	     ((NIL :VALUE COM-WRITE-QFASL)
	      (NIL :VALUE BARF)
	      (NIL :VALUE COM-WRITE-ANY-FILE))
	     :DOCUMENTATION "L: Write QFASL file    R: Write some other kind menu")
	    ("EXIT" :VALUE COM-QUIT
	     :DOCUMENTATION "Bury to font editor, return to other activity")
	    ("HELP" :VALUE COM-HELP
	     :DOCUMENTATION "More info")))



(DEFCONST DRAW-MODE-MENU-LIST
	  '(("Set Points" :VALUE  COM-SET-SETA-DRAW-MODE
	     :DOCUMENTATION "Make the mouse SET points")
	    ("Clear Points" :VALUE  COM-SET-ANDCA-DRAW-MODE
	     :DOCUMENTATION "Make the mouse CLEAR points")
	    ("Flip Points" :VALUE  COM-SET-XOR-DRAW-MODE
	     :DOCUMENTATION "Make the mouse FLIP points")))

;;; Code starts on next page!!!


(OR (BOUNDP '*FED-GREAT-FRAME*) (SETQ FED-WINDOW NIL))	;flush pre-bsg fed if one exists--
(DEFVAR *FED-GREAT-FRAME* NIL)

(DEFVAR COMMAND-TABLE NIL)

(DEFUN FED (&OPTIONAL FONT)
  (IF (EQ FONT ':REINITIALIZE)
      (SETQ FONT (SETQ *FED-GREAT-FRAME* NIL)))
  (OR *FED-GREAT-FRAME*
      (SETQ *FED-GREAT-FRAME* (TV:MAKE-WINDOW 'FED-GREAT-FRAME)))
  (LET ((FED (FUNCALL *FED-GREAT-FRAME* ':GET-PANE 'FED)))
    (IF FONT (FUNCALL FED ':FORCE-KBD-INPUT `(:FED-COM COM-SPECIFY-FONT ,FONT)))
    (FUNCALL FED ':SELECT))
  NIL)						;fewer values to listener

;;; All commands are called within the FED-PANE -- these specials are
;;; being obsoleted, and (DECLARE-FLAVOR-INSTANCE-VARIABLES) going into use
;;; where appropriate.

(DECLARE (SPECIAL DRAW-MODE PROMPT-WINDOW CURSOR-ON CURSOR-X CURSOR-Y
		  CURRENT-FONT CURRENT-CHARACTER REDISPLAY-DEGREE PLANE
		  TV:TYPEOUT-WINDOW))

(DECLARE (SPECIAL WINDOW-X-POS WINDOW-Y-POS WINDOW-X-SIZE WINDOW-Y-SIZE
		  BOX-X-SIZE BOX-Y-SIZE CHAR-BOX-X1 CHAR-BOX-X2
		  CHAR-BOX-Y1 CHAR-BOX-Y2 CHAR-BOX-Y3))

;;;Windows that display a bunch of points inside a grid
(DEFFLAVOR GRID-MIXIN
       (WINDOW-ARRAY				;This represents the displayed image
	WINDOW-X-SIZE				;Its virtual dimensions
	WINDOW-Y-SIZE
	(BOX-X-SIZE DEFAULT-BOX-SIZE)		;The size of an element of the grid
	(BOX-Y-SIZE DEFAULT-BOX-SIZE)
	(WINDOW-X-POS 0)			;The offset position of our array
	(WINDOW-Y-POS 0)
	(REDISPLAY-DEGREE REDISPLAY-NONE)	;A number, REDISPLAY-x
	(MIN-CHANGED-X 0)			;Range of area to bother checking
	(MIN-CHANGED-Y 0)
	(MAX-CHANGED-X 0)
	(MAX-CHANGED-Y 0)
	REDISPLAY-SUPPRESSED			;The last redisplay did not complete
        (DRAW-MODE TV:ALU-IOR)
	)
       ()
  (:SETTABLE-INSTANCE-VARIABLES DRAW-MODE)
  (:INCLUDED-FLAVORS TV:ESSENTIAL-WINDOW NOOP-LISTEN-MIXIN)
  (:INIT-KEYWORDS :WINDOW-ARRAY-TYPE)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL :MORE-P NIL)
  (:REQUIRED-METHODS :AREF :ASET)		;These access the other data structure
  (:DOCUMENTATION :MIXIN "Displays a set of points within a grid
Allows for incremental redisplay of points and updating the data structure for changes
in the display."))

;;;Some random constants
(DEFCONST MIN-BOX-SIZE 6)			;If you're smaller than this, no grid shown
(DEFCONST DEFAULT-BOX-SIZE 12.)			;How big to create things
(DEFCONST GRID-POINT-SIZE 2)
(DEFCONST REDISPLAY-NONE 0)			;No redisplay needed
(DEFCONST REDISPLAY-ONE 1)			;Only one box wrong
(DEFCONST REDISPLAY-SOME 2)			;A few boxes wrong
(DEFCONST REDISPLAY-MAXIMALLY 3)		;make a good guess, DONT clear screen
(DEFCONST REDISPLAY-ALL 4)			;Everything you know is wrong, clear screen

(DEFMETHOD (GRID-MIXIN :AFTER :INIT) (INIT-PLIST)
  (FUNCALL-SELF ':DEDUCE-WINDOW-ARRAY-SIZE (OR (GET INIT-PLIST ':WINDOW-ARRAY-TYPE) 'ART-1B)))

(DEFMETHOD (GRID-MIXIN :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (FUNCALL-SELF ':DEDUCE-WINDOW-ARRAY-SIZE))

;;;Figure out how many boxes to make with these edges
(DEFMETHOD (GRID-MIXIN :DEDUCE-WINDOW-ARRAY-SIZE) (&OPTIONAL ARRAY-TYPE)
  (DECLARE-FLAVOR-INSTANCE-VARIABLES (GRID-MIXIN)
    (OR ARRAY-TYPE (SETQ ARRAY-TYPE (ARRAY-TYPE WINDOW-ARRAY)))
    (LET ((LAST-ROW-OF-DOTS
	    (IF (AND (> BOX-X-SIZE MIN-BOX-SIZE)
		     (> BOX-Y-SIZE MIN-BOX-SIZE))
		2
		0)))
      (SETQ WINDOW-X-SIZE (// (- (TV:SHEET-INSIDE-WIDTH) LAST-ROW-OF-DOTS) BOX-X-SIZE)
	    WINDOW-Y-SIZE (// (- (TV:SHEET-INSIDE-HEIGHT) LAST-ROW-OF-DOTS) BOX-Y-SIZE))
      (OR (AND (BOUNDP 'WINDOW-ARRAY)
	       ( WINDOW-X-SIZE (ARRAY-DIMENSION-N 1 WINDOW-ARRAY))
	       ( WINDOW-Y-SIZE (ARRAY-DIMENSION-N 2 WINDOW-ARRAY)))
	  (SETQ WINDOW-ARRAY (MAKE-ARRAY NIL ARRAY-TYPE
					 (LIST WINDOW-X-SIZE WINDOW-Y-SIZE)))))))

;;;If we didn't come back, remember that the screen is clobbered
(DEFMETHOD (GRID-MIXIN :AFTER :REFRESH) (&OPTIONAL TYPE)
  (OR (AND TV:RESTORED-BITS-P (NEQ TYPE ':SIZE-CHANGED))
      (SETQ REDISPLAY-DEGREE REDISPLAY-ALL)))

;;;Note that something has changed for the redisplay loop
(DEFMETHOD (GRID-MIXIN :MUST-REDISPLAY) (DEGREE &OPTIONAL MIN-X MIN-Y MAX-X MAX-Y)
  (IF (= DEGREE REDISPLAY-ONE)		;Just one box to hack
      (PROGN
	(SETQ MAX-X MIN-X MAX-Y MIN-Y)		;makes sense, no?
	(COND ((= REDISPLAY-DEGREE REDISPLAY-NONE)
	       (SETQ MIN-CHANGED-X MIN-X
		     MIN-CHANGED-Y MIN-Y
		     MAX-CHANGED-X MAX-X
		     MAX-CHANGED-Y MAX-Y
		     REDISPLAY-DEGREE REDISPLAY-ONE))
	      ((AND (= REDISPLAY-DEGREE REDISPLAY-ONE)
		    (= MIN-CHANGED-X MIN-X)	;Same point as before is ok too
		    (= MIN-CHANGED-Y MIN-Y)))
	      (T
	       (SETQ REDISPLAY-DEGREE REDISPLAY-SOME))))
      (SETQ REDISPLAY-DEGREE (MAX REDISPLAY-DEGREE DEGREE)))
  (COND ((> REDISPLAY-DEGREE REDISPLAY-ONE)
	 (AND MIN-X (SETQ MIN-CHANGED-X (MIN MIN-CHANGED-X MIN-X)))
	 (AND MIN-Y (SETQ MIN-CHANGED-Y (MIN MIN-CHANGED-Y MIN-Y)))
	 (AND MAX-X (SETQ MAX-CHANGED-X (MAX MAX-CHANGED-X MAX-X)))
	 (AND MAX-Y (SETQ MAX-CHANGED-Y (MAX MAX-CHANGED-Y MAX-Y))))))

;;;Function is an argument of the two grid points which returns the correct array value
;;;from the other data structure
(DEFMETHOD (GRID-MIXIN :REDISPLAY) (&OPTIONAL (FORCE-TO-COMPLETION))
  (SETQ REDISPLAY-SUPPRESSED NIL)
  (COND ((= REDISPLAY-DEGREE REDISPLAY-NONE))	;No redisplay needed
	((AND (NOT FORCE-TO-COMPLETION) (FUNCALL-SELF ':LISTEN))
	 (SETQ REDISPLAY-SUPPRESSED T))
	(T
	 (COND ((= REDISPLAY-DEGREE REDISPLAY-ALL)
		(FUNCALL-SELF ':REDISPLAY-ALL))
	       ((= REDISPLAY-DEGREE REDISPLAY-MAXIMALLY)
		(SETQ MIN-CHANGED-X 0 MIN-CHANGED-Y 0
		      MAX-CHANGED-X (1- WINDOW-X-SIZE)
		      MAX-CHANGED-Y (1- WINDOW-Y-SIZE))))
	 ;; Since the commands don't seem to clip the change boundaries, do so here
	 ;; in case the font is too big to fit in the window
	 (SETQ MIN-CHANGED-X (MAX MIN-CHANGED-X 0)
	       MIN-CHANGED-Y (MAX MIN-CHANGED-Y 0)
	       MAX-CHANGED-X (MIN MAX-CHANGED-X (1- WINDOW-X-SIZE))
	       MAX-CHANGED-Y (MIN MAX-CHANGED-Y (1- WINDOW-Y-SIZE)))
	 ;; Now, for each box which isn't already displayed in the right state,
	 ;; update it.

	 ;; Feature for FED maintaners..
	 ;;(PROMPT-LINE "minx ~S miny ~S maxx ~S maxy ~S"
	 ;; MIN-CHANGED-X MIN-CHANGED-Y MAX-CHANGED-X MAX-CHANGED-Y)

	 (FUNCALL-SELF ':HEART-OF-REDISPLAY FORCE-TO-COMPLETION))))

(DEFMETHOD (GRID-MIXIN :REDISPLAY-ALL) ()
  (FUNCALL-SELF ':DRAW-GRID)
  ;; Every box is now clear on the screen
  (FILLARRAY WINDOW-ARRAY '(0))
  ;; but every box must be checked for redisplay.
  (SETQ MIN-CHANGED-X 0 MIN-CHANGED-Y 0
	MAX-CHANGED-X (1- WINDOW-X-SIZE)
	MAX-CHANGED-Y (1- WINDOW-Y-SIZE))
  (SETQ REDISPLAY-DEGREE REDISPLAY-SOME))

(DEFMETHOD (GRID-MIXIN :HEART-OF-REDISPLAY) (FORCE-TO-COMPLETION)
  (TV:PREPARE-SHEET (SELF)
    (DO-NAMED ABORT-REDISPLAY
	      ((J MIN-CHANGED-Y (1+ J))
	       (AREF-HANDLER (GET-HANDLER-FOR SELF ':AREF))	;For speed
	       (LISTEN-HANDLER (GET-HANDLER-FOR SELF ':LISTEN))
	       (DRAW-HANDLER (GET-HANDLER-FOR SELF ':DRAW-POINT)))
	      ((> J MAX-CHANGED-Y)
	       (SETQ REDISPLAY-DEGREE REDISPLAY-NONE))
      (DO ((I MIN-CHANGED-X (1+ I))
	   (NEW-VALUE))
	  ((> I MAX-CHANGED-X))
	(OR (= (SETQ NEW-VALUE (FUNCALL AREF-HANDLER ':AREF (+ I WINDOW-X-POS)
					(+ J WINDOW-Y-POS)))
	       (AREF WINDOW-ARRAY I J))
	    (FUNCALL DRAW-HANDLER ':DRAW-POINT I J NEW-VALUE T)))
      (COND ((AND (NOT FORCE-TO-COMPLETION) (FUNCALL LISTEN-HANDLER ':LISTEN))
	     (SETQ MIN-CHANGED-Y (1+ J))
	     (SETQ REDISPLAY-SUPPRESSED T)
	     (RETURN-FROM ABORT-REDISPLAY))))))


;;;This exists so that there is always a listen message, it is only a :INCLUDED-FLAVOR
(DEFFLAVOR NOOP-LISTEN-MIXIN () ()
  (:DOCUMENTATION :MIXIN "To assure the presence of a :LISTEN message
The :listen method defined is a no-op."))

(DEFMETHOD (NOOP-LISTEN-MIXIN :LISTEN) ())

;;;This is a message so you can put some daemons on it to draw other things (like the
;;;character box)
(DEFVAR GRID-BITBLT-KLUDGE (MAKE-ARRAY NIL 'ART-1B '(64. 64.)))
(DEFVAR GRID-BITBLT-ONES (MAKE-ARRAY NIL 'ART-1B '(32. 32.)))

(DEFMETHOD (GRID-MIXIN :DRAW-GRID) ()
  (FUNCALL-SELF ':CLEAR-SCREEN)
  ;; Now add in the grid points, unless the grid is too small.
  (COND ((NOT (OR (< BOX-X-SIZE MIN-BOX-SIZE) (< BOX-Y-SIZE MIN-BOX-SIZE)))
	 ;; Make an array containing the necessary dots
	 (BITBLT 0 64. 64. GRID-BITBLT-KLUDGE 0 0 GRID-BITBLT-KLUDGE 0 0)
	 (BITBLT 17 32. 32. GRID-BITBLT-ONES 0 0 GRID-BITBLT-ONES 0 0)
	 (DO I 0 (+ I BOX-X-SIZE) (> (+ I GRID-POINT-SIZE) 64.)
	   (DO J 0 (+ J BOX-Y-SIZE) (> (+ J GRID-POINT-SIZE) 64.)
	     (BITBLT TV:ALU-IOR GRID-POINT-SIZE GRID-POINT-SIZE
		     GRID-BITBLT-ONES 0 0 GRID-BITBLT-KLUDGE I J)))
	 ;; Smear the array over the window
	 (LET* ((X-BOXES-PER-ARRAY (// 64. BOX-X-SIZE))
		(Y-BOXES-PER-ARRAY (// 64. BOX-Y-SIZE))
		(X-ARRAYS-PER-WINDOW
		  (// (+ (1+ WINDOW-X-SIZE) X-BOXES-PER-ARRAY -1)	;1+ for last row dots
		      X-BOXES-PER-ARRAY))
		(Y-ARRAYS-PER-WINDOW
		  (// (+ (1+ WINDOW-Y-SIZE) Y-BOXES-PER-ARRAY -1)	;1+ for last row dots
		      Y-BOXES-PER-ARRAY))
		(X-GRID-SIZE (* X-BOXES-PER-ARRAY BOX-X-SIZE))
		(Y-GRID-SIZE (* Y-BOXES-PER-ARRAY BOX-Y-SIZE))
		(FINAL-X (1- X-ARRAYS-PER-WINDOW))
		(FINAL-Y (1- Y-ARRAYS-PER-WINDOW))
		(LAST-X-WIDTH (IF (ZEROP (\ (TV:SHEET-INSIDE-WIDTH) X-GRID-SIZE))
				  X-GRID-SIZE
				  (\ (TV:SHEET-INSIDE-WIDTH) X-GRID-SIZE)))
		(LAST-Y-WIDTH (IF (ZEROP (\ (TV:SHEET-INSIDE-HEIGHT) Y-GRID-SIZE))
				  Y-GRID-SIZE
				  (\ (TV:SHEET-INSIDE-HEIGHT) Y-GRID-SIZE))))
	   (DOTIMES (I X-ARRAYS-PER-WINDOW)
	     (LET ((XORIGIN (* I X-GRID-SIZE)))
	       (DOTIMES (J Y-ARRAYS-PER-WINDOW)
		 (LET ((YORIGIN (* J Y-GRID-SIZE)))
		   (FUNCALL-SELF ':BITBLT TV:ALU-SETA
				 (IF (= I FINAL-X) LAST-X-WIDTH X-GRID-SIZE)
				 (IF (= J FINAL-Y) LAST-Y-WIDTH Y-GRID-SIZE)
				 GRID-BITBLT-KLUDGE 0 0 XORIGIN YORIGIN)))))))))

;;;Draw a point in the grid, and store the new value in our array
;;;FROM-REDISPLAY means that this value came from the other data structure, so don't
;;;bother trying to update the plane.

(DEFMETHOD (GRID-MIXIN :DRAW-POINT) (I J &OPTIONAL NEW-VALUE FROM-REDISPLAY &REST IGNORE)
  (IF (NOT (= NEW-VALUE (AREF WINDOW-ARRAY I J)))
      (TV:PREPARE-SHEET (SELF)			;note this is recursive
	(LET ((XS (+ (TV:SHEET-INSIDE-LEFT) (* I BOX-X-SIZE)))
	      (YS (+ (TV:SHEET-INSIDE-TOP) (* J BOX-Y-SIZE))))
	  (%DRAW-RECTANGLE BOX-X-SIZE BOX-Y-SIZE XS YS TV:ALU-XOR SELF))))
  (ASET NEW-VALUE WINDOW-ARRAY I J)
  (OR FROM-REDISPLAY
      (FUNCALL-SELF ':ASET NEW-VALUE (+ I WINDOW-X-POS) (+ J WINDOW-Y-POS))))

(DEFVAR GRAY-ARRAY)
(DEFMETHOD (GRID-MIXIN :GRAY-POINT) (X Y)
  (COND ((NOT (BOUNDP 'GRAY-ARRAY))
	 (SETQ GRAY-ARRAY (MAKE-ARRAY NIL 'ART-1B '(40 4)))
	 (DOTIMES (I 40) (DOTIMES (J 4) (ASET (LOGXOR I J) GRAY-ARRAY I J)))))
  (FUNCALL-SELF ':BITBLT TV:ALU-XOR BOX-X-SIZE BOX-Y-SIZE GRAY-ARRAY 0 0
		(* X BOX-X-SIZE) (* Y BOX-Y-SIZE)))

(DEFMETHOD (GRID-MIXIN :SET-BOX-SIZE) (&OPTIONAL (NEW-X-SIZE DEFAULT-BOX-SIZE)
						  (NEW-Y-SIZE NEW-X-SIZE))
  (COND ((NOT (AND (= BOX-X-SIZE NEW-X-SIZE) (= BOX-Y-SIZE NEW-Y-SIZE)))
	 (SETQ BOX-X-SIZE NEW-X-SIZE
	       BOX-Y-SIZE NEW-Y-SIZE
	       REDISPLAY-DEGREE REDISPLAY-ALL)
	 (FUNCALL-SELF ':DEDUCE-WINDOW-ARRAY-SIZE))))


;;; This macro ought be installed.

(DEFMACRO WITH-MOUSE-GRABBED-ON-SHEET ((SHEET) &BODY BODY &AUX (OV (GENSYM)))
  (SETQ SHEET (OR SHEET 'SELF))
  `(LET ((,OV TV:MOUSE-SHEET))
     (UNWIND-PROTECT
       (TV:WITH-MOUSE-GRABBED
	 (TV:MOUSE-SET-SHEET ,SHEET)
	 . ,BODY)
       (TV:MOUSE-SET-SHEET ,OV))))

;;; Actual mouse-button drawing implemented here.  Mouse followed (not tracked)
;;; in real time by this loop, and keyboard shift keys watched as modifiers.

(DEFMETHOD (GRID-MIXIN :MOUSE-BOOLE-SQUARES) (X0 Y0 BOOLE)
  (SETQ X0 (- X0 (TV:SHEET-INSIDE-LEFT)) Y0 (- Y0 (TV:SHEET-INSIDE-TOP)))
  (FUNCALL-SELF ':REDISPLAY T)			;Force redisplay to completion first
  (WITH-MOUSE-GRABBED-ON-SHEET (SELF)
    (UNWIND-PROTECT
      (DO ((FIRST T NIL)
	   (X)
	   (Y)
	   (KEYS)
	   (LAST-KEYS -1 KEYS)
	   (OLD-X -1 X)
	   (OLD-Y -1 Y)
	   (OLD-M-X)
	   (OLD-M-Y)
	   (OLD-VALUE)
	   (NEW-VALUE))
	  ((AND (NOT FIRST) (ZEROP (TV:MOUSE-BUTTONS)))
	   (VALUES OLD-X OLD-Y))
	(OR FIRST (PROCESS-WAIT "FED draw"
				#'(LAMBDA (AX AY AK AB)
				    (NOT (AND (= TV:MOUSE-X AX) (= TV:MOUSE-Y AY)
					      (= (KEY-ANALYZER) AK)
					      (= TV:MOUSE-LAST-BUTTONS AB))))
				OLD-M-X OLD-M-Y KEYS TV:MOUSE-LAST-BUTTONS))
	(SETQ KEYS (KEY-ANALYZER))
	(SETQ OLD-M-X TV:MOUSE-X OLD-M-Y TV:MOUSE-Y)
	(SETQ X (// (IF FIRST X0
			(- TV:MOUSE-X TV:LEFT-MARGIN-SIZE))
		    BOX-X-SIZE))
	(SETQ Y (// (IF FIRST Y0
			(- TV:MOUSE-Y TV:TOP-MARGIN-SIZE))
		    BOX-Y-SIZE))
	(IF (AND (LESSP -1 X WINDOW-X-SIZE) (LESSP -1 Y WINDOW-Y-SIZE))	;ignore oob
	    (LET ((BOOLE (COND ((AND (LDB-TEST %%KBD-CONTROL KEYS) (LDB-TEST %%KBD-META KEYS))
				#2r0011)		;copy oldn
			       ((LDB-TEST %%KBD-CONTROL KEYS)
				TV:ALU-ANDCA)
			       ((LDB-TEST %%KBD-META KEYS)
				TV:ALU-IOR)
			       (T BOOLE))))
	      (OR (= KEYS LAST-KEYS) (FUNCALL-SELF ':BOOLE-MODE-CHANGE BOOLE))
	      (OR (AND (NOT FIRST) (= X OLD-X) (= Y OLD-Y) (= KEYS LAST-KEYS))
		  (= (SETQ OLD-VALUE (AREF WINDOW-ARRAY X Y))
		     (SETQ NEW-VALUE (BOOLE BOOLE 1 OLD-VALUE)))
		  (FUNCALL-SELF ':DRAW-POINT X Y NEW-VALUE)))))
      (FUNCALL-SELF ':BOOLE-MODE-CHANGE DRAW-MODE))))

(DEFMETHOD (GRID-MIXIN :BOOLE-MODE-CHANGE) (BOOLE) BOOLE)	;ensure handler

(DEFMETHOD (GRID-MIXIN :SET-OFFSET) (NEW-X-POS NEW-Y-POS)
  (OR (AND (= WINDOW-X-POS NEW-X-POS)
	   (= WINDOW-Y-POS NEW-Y-POS))
      (PROGN
	(SETQ WINDOW-X-POS NEW-X-POS
	      WINDOW-Y-POS NEW-Y-POS)
	(FUNCALL-SELF ':MUST-REDISPLAY REDISPLAY-ALL))))

(DEFMETHOD (GRID-MIXIN :DRAW-GRID-LINE) (X0 Y0 X1 Y1 DRAW-MODE &AUX DX DY YI FLAG
					    (HANDLER (FUNCALL-SELF ':GET-HANDLER-FOR
								   ':DRAW-POINT)))
  (SETQ DX (- X1 X0)
	DY (- Y1 Y0))
  (AND (MINUSP DX)
       (SETQ DX (- DX) X0 X1 DY (- DY) Y0 Y1))
  (IF (MINUSP DY)
      (SETQ DY (- DY) YI -1)
      (SETQ YI 1))
  (AND (SETQ FLAG (> DY DX)) (PSETQ DX DY DY DX))
  (TV:PREPARE-SHEET (SELF)
    (DO ((A (// DX 2))
	 (C DX (1- C)))
	((< C 0))
      (SELECTQ DRAW-MODE
	;; IOR
	(7 (AND (ZEROP (AREF WINDOW-ARRAY X0 Y0))
		(FUNCALL HANDLER ':DRAW-POINT X0 Y0 1)))
	;; ANDCA
	(2 (OR (ZEROP (AREF WINDOW-ARRAY X0 Y0))
	       (FUNCALL HANDLER ':DRAW-POINT X0 Y0 0)))
	;; XOR
	(6 (FUNCALL HANDLER ':DRAW-POINT X0 Y0
		    (- 1 (AREF WINDOW-ARRAY X0 Y0)))))
      (COND ((MINUSP (SETQ A (- A DY)))
	     (SETQ A (+ A DX))
	     (SETQ X0 (1+ X0) Y0 (+ Y0 YI)))
	    (FLAG
	     (SETQ Y0 (+ Y0 YI)))
	    (T
	     (SETQ X0 (1+ X0)))))))

(DEFMETHOD (GRID-MIXIN :DRAW-CURVE) (PX PY &OPTIONAL END (DRAW-MODE 7))
  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH PX)))
  (DO ((I 1 (1+ I))
       (X0)
       (X1 (FIX (AREF PX 0)))
       (Y0)
       (Y1 (FIX (AREF PY 0)))
       (HANDLER (GET-HANDLER-FOR SELF ':DRAW-GRID-LINE)))
      (( I END))
    (SETQ X0 X1)
    (OR (SETQ X1 (AREF PX I)) (RETURN NIL))
    (SETQ X1 (FIX X1))
    (SETQ Y0 Y1)
    (OR (SETQ Y1 (AREF PY I)) (RETURN NIL))
    (SETQ Y1 (FIX Y1))
    (FUNCALL HANDLER ':DRAW-GRID-LINE X0 Y0 X1 Y1 DRAW-MODE)))

(DEFCONST *RDIS-LIMIT-CONSTANT* '(99999. 99999. -99999. -99999.))

;;;Grid windows that display a plane
(DEFFLAVOR PLANE-GRID-MIXIN
       ((PLANE (MAKE-PLANE 2 ':TYPE 'ART-1B ':DEFAULT-VALUE 0))
						;The plane being displayed
	(PREVIOUS-RDIS-LIMITS *RDIS-LIMIT-CONSTANT*))
       (GRID-MIXIN)
  (:DOCUMENTATION :SPECIAL-PURPOSE "A grid window that displays a plane
The PLANE instance variable is displayed in the grid and updated when it
is changed via the mouse."))


;;; This macro is used to spill out all dimensions and bounds of PLANE, so that
;;; we can do plane references and stores inline.  PLANE-ASET and PLANE-AREF
;;; are provably far too inefficient, and doing this plane-hacking in line
;;; makes FED truly snappy as opposed to sluggish.  Note we still use planes
;;; to create and occasionally (usually initially) grow planes.

(DEFMACRO SPLAT-OUT-PLANE-DATA BODY
  `(LET ((DIM-1 (ARRAY-DIMENSION-N 1 PLANE))
	 (DIM-2 (ARRAY-DIMENSION-N 2 PLANE))
	 (ORIGINS (PLANE-ORIGIN PLANE)))
     (LET ((XSTART (FIRST ORIGINS))
	   (XEND   (+ (FIRST ORIGINS) DIM-1))
	   (YSTART (SECOND ORIGINS))
	   (YEND   (+ (SECOND ORIGINS) DIM-2)))
       . ,BODY)))



;;; Make the first stab at the maximal limits guess for REDISPLAY-MAXIMALLY.
;;; :MINIMIZE-REDISPLAY-LIMITS (as mixed in appropriately) will chop it down as needed.
(DEFMETHOD (PLANE-GRID-MIXIN :BEFORE :REDISPLAY) (&OPTIONAL IGNORE)
  (SETQ PLANE (FOLLOW-STRUCTURE-FORWARDING PLANE))
  (IF (= REDISPLAY-DEGREE REDISPLAY-MAXIMALLY)
      (SETQ MIN-CHANGED-X WINDOW-X-POS
	    MIN-CHANGED-Y WINDOW-Y-POS
	    MAX-CHANGED-X (+ WINDOW-X-POS WINDOW-X-SIZE -1)
	    MAX-CHANGED-Y (+ WINDOW-Y-POS WINDOW-Y-SIZE -1)))
  (IF ( REDISPLAY-DEGREE REDISPLAY-SOME)
      (FUNCALL-SELF ':MINIMIZE-REDISPLAY-LIMITS)))

;;; If redisplay was successful, no more worry about previous black box.
(DEFMETHOD (PLANE-GRID-MIXIN :AFTER :REDISPLAY) (&OPTIONAL IGNORE)
  (IF (NOT REDISPLAY-SUPPRESSED)
      (SETQ PREVIOUS-RDIS-LIMITS *RDIS-LIMIT-CONSTANT*)))

;;; "Clear the plane" by installing a new empty one.  This is very salutary for
;;; FED, because it reduces the limits of the possible character.  We maximize our
;;; possible redisplay limits between the old plane and the current new one.
;;; Note that the register stuff feeds in the optional NEW-PLANE argument.
(DEFMETHOD (PLANE-GRID-MIXIN :NEW-PLANE) (&OPTIONAL DIMLIST NEW-PLANE)
  (SPLAT-OUT-PLANE-DATA
    (SETQ PREVIOUS-RDIS-LIMITS
	  (LIST (MIN XSTART (FIRST PREVIOUS-RDIS-LIMITS))
		(MIN YSTART (SECOND PREVIOUS-RDIS-LIMITS))
		(MAX XEND (THIRD PREVIOUS-RDIS-LIMITS))
		(MAX YEND (FOURTH PREVIOUS-RDIS-LIMITS)))))
  (SETQ PLANE
	(OR NEW-PLANE (MAKE-PLANE 2 ':TYPE 'ART-4B ':DEFAULT-VALUE 0
				  ':EXTENSION 8.)))
  (IF (AND DIMLIST (NULL NEW-PLANE))
      (PROGN
	(PLANE-ASET 0 PLANE (FIRST (FIRST DIMLIST)) (SECOND (FIRST DIMLIST)))
	(PLANE-ASET 0 PLANE (1- (FIRST (SECOND DIMLIST))) (1- (SECOND (SECOND DIMLIST))))))
  (SETQ PLANE (FOLLOW-STRUCTURE-FORWARDING PLANE))
  (FUNCALL-SELF ':MUST-REDISPLAY REDISPLAY-MAXIMALLY)
  PLANE)

;;; For FED maintainers
(DEFMETHOD (PLANE-GRID-MIXIN :DEBUG-TRACE-RDIS-LIMITS) ()
  (PROMPT-LINE "minx ~D maxx ~D miny ~D maxy ~D" MIN-CHANGED-X
	       MAX-CHANGED-X MIN-CHANGED-Y MAX-CHANGED-Y))

;;;Take advantage of knowing that there can't be any points in nonexistent part of plane.
(DEFMETHOD (PLANE-GRID-MIXIN :MINIMIZE-REDISPLAY-LIMITS) ()
  (SPLAT-OUT-PLANE-DATA
    (SETQ MIN-CHANGED-X (MAX MIN-CHANGED-X (- (MIN XSTART (FIRST PREVIOUS-RDIS-LIMITS))
					      WINDOW-X-POS) 0)
	  MIN-CHANGED-Y (MAX MIN-CHANGED-Y (- (MIN YSTART (SECOND PREVIOUS-RDIS-LIMITS))
					      WINDOW-Y-POS) 0)
	  MAX-CHANGED-X (MIN MAX-CHANGED-X (- (MAX XEND (THIRD PREVIOUS-RDIS-LIMITS))
					      WINDOW-X-POS 1) (1- WINDOW-X-SIZE))
	  MAX-CHANGED-Y (MIN MAX-CHANGED-Y (- (MAX YEND (FOURTH PREVIOUS-RDIS-LIMITS))
					      WINDOW-Y-POS 1) (1- WINDOW-Y-SIZE)))))

;;; Move the actual data in the black plane.
(DEFMETHOD (PLANE-GRID-MIXIN :MOVE-PLANE) (XFROM YFROM XTO YTO)
  (SPLAT-OUT-PLANE-DATA
    (LET ((OLD (FOLLOW-STRUCTURE-FORWARDING PLANE))
	  (XDISP (- XTO XFROM))
	  (YDISP (- YTO YFROM)))
      (FUNCALL-SELF  ':NEW-PLANE
		     `((,(+ XDISP XSTART) ,(+ YDISP YSTART))	;xmin ymin
		       (,(+ XDISP XEND) ,(+ YDISP YEND))))	;xEND yEND
      (LET* ((OORIGIN (PLANE-ORIGIN PLANE))	;Loop invariants.
	    (OXOF (FIRST OORIGIN))
	    (OYOF (SECOND OORIGIN))
	    (ORIGINS (PLANE-ORIGIN OLD))
	    (IXOF (FIRST ORIGINS))
	    (IYOF (SECOND ORIGINS))
	    (XDIM (ARRAY-DIMENSION-N 1 OLD))
	    (YDIM (ARRAY-DIMENSION-N 2 OLD))
	    (XCOFF (+ XDISP (- IXOF OXOF)))
	    (YCOFF (+ YDISP (- IYOF OYOF))))
	(DOTIMES (I XDIM)
	  (DOTIMES (J YDIM)
	    (ASET (AREF OLD I J) PLANE (+ I XCOFF) (+ J YCOFF))))))))

(DEFMETHOD (PLANE-GRID-MIXIN :AREF) (I J)
  (PLANE-AREF PLANE I J))

(DEFMETHOD (PLANE-GRID-MIXIN :ASET) (VAL I J)
  (PLANE-ASET VAL PLANE I J))


;;;Plane windows with a special outline someplace (the character box and baseline)

(DEFFLAVOR CHAR-BOX-GRID-MIXIN
       ((CHAR-BOX-X1 0) (CHAR-BOX-Y1 0)		;The real position
	(CHAR-BOX-X2 0) (CHAR-BOX-Y2 0)
	(CHAR-BOX-Y3 0)
	DISPLAYED-CHAR-BOX-X1 DISPLAYED-CHAR-BOX-Y1	;The displayed position
	DISPLAYED-CHAR-BOX-X2 DISPLAYED-CHAR-BOX-Y2
	DISPLAYED-CHAR-BOX-Y3)
       ()
  (:INCLUDED-FLAVORS GRID-MIXIN)
  (:REQUIRED-METHODS :FATAL-ERROR)
  (:DOCUMENTATION :SPECIAL-PURPOSE "Grind windows with a special outline
The outline is used to show the actual character area and baseline by the font-editor."))

;;;When the grid gets drawn, draw the character box as well
(DEFMETHOD (CHAR-BOX-GRID-MIXIN :AFTER :DRAW-GRID) ()
  (SETQ DISPLAYED-CHAR-BOX-X1 CHAR-BOX-X1)
  (SETQ DISPLAYED-CHAR-BOX-X2 CHAR-BOX-X2)
  (SETQ DISPLAYED-CHAR-BOX-Y1 CHAR-BOX-Y1)
  (SETQ DISPLAYED-CHAR-BOX-Y2 CHAR-BOX-Y2)
  (SETQ DISPLAYED-CHAR-BOX-Y3 CHAR-BOX-Y3)
  (FUNCALL-SELF ':DISPLAY-CHAR-BOX))

;;;After redisplay, check that the character box is correct
(DEFMETHOD (CHAR-BOX-GRID-MIXIN :AFTER :REDISPLAY) (&OPTIONAL IGNORE)
  (COND ((OR REDISPLAY-SUPPRESSED (= BOX-X-SIZE 1) (= BOX-Y-SIZE 1)))
	((AND (= DISPLAYED-CHAR-BOX-X1 CHAR-BOX-X1)
	      (= DISPLAYED-CHAR-BOX-X2 CHAR-BOX-X2)
	      (= DISPLAYED-CHAR-BOX-Y1 CHAR-BOX-Y1)
	      (= DISPLAYED-CHAR-BOX-Y2 CHAR-BOX-Y2)
	      (= DISPLAYED-CHAR-BOX-Y3 CHAR-BOX-Y3)))
	(T
	 (FUNCALL-SELF ':DISPLAY-CHAR-BOX)
	 (SETQ DISPLAYED-CHAR-BOX-X1 CHAR-BOX-X1)
	 (SETQ DISPLAYED-CHAR-BOX-X2 CHAR-BOX-X2)
	 (SETQ DISPLAYED-CHAR-BOX-Y1 CHAR-BOX-Y1)
	 (SETQ DISPLAYED-CHAR-BOX-Y2 CHAR-BOX-Y2)
	 (SETQ DISPLAYED-CHAR-BOX-Y3 CHAR-BOX-Y3)
	 (FUNCALL-SELF ':DISPLAY-CHAR-BOX))))

;;;XOR the char box and the baseline line in
(DEFMETHOD (CHAR-BOX-GRID-MIXIN :DISPLAY-CHAR-BOX) (&AUX X1 Y1 X2 Y2 Y3)
  (SETQ X1 (1- (* BOX-X-SIZE (- DISPLAYED-CHAR-BOX-X1 WINDOW-X-POS)))
	Y1 (1- (* BOX-Y-SIZE (- DISPLAYED-CHAR-BOX-Y1 WINDOW-Y-POS)))
	X2 (1- (* BOX-X-SIZE (- DISPLAYED-CHAR-BOX-X2 WINDOW-X-POS)))
        Y2 (1- (* BOX-Y-SIZE (- DISPLAYED-CHAR-BOX-Y2 WINDOW-Y-POS)))
        Y3 (1- (* BOX-Y-SIZE (- DISPLAYED-CHAR-BOX-Y3 WINDOW-Y-POS))))
  (FUNCALL-SELF ':DRAW-RECTANGLE 2 (- Y2 Y1) X1 Y1 TV:ALU-XOR)
  (COND ((= X1 X2))
	(T
	 (FUNCALL-SELF ':DRAW-RECTANGLE (- X2 X1) 2 (+ 2 X1) Y1 TV:ALU-XOR)
	 (FUNCALL-SELF ':DRAW-RECTANGLE 2 (- Y2 Y1) X2 (+ 2 Y1) TV:ALU-XOR)
	 (FUNCALL-SELF ':DRAW-RECTANGLE (- X2 X1) 2 X1 Y2 TV:ALU-XOR)
	 (OR (= Y2 Y3)
	     (FUNCALL-SELF ':DRAW-RECTANGLE (- X2 -2 X1) 2 X1 Y3 TV:ALU-XOR)))))

(DEFMETHOD (CHAR-BOX-GRID-MIXIN	:AFTER :DRAW-POINT) (&REST IGNORE))


;Push this button when the mouse is near an edge or corner of the character box,
;and then as long as you hold the button down you are moving that corner.
(DEFMETHOD (CHAR-BOX-GRID-MIXIN :MOUSE-MOVE-CHAR-BOX)
	   (X0 Y0 &AUX X-POS-NAME Y-POS-NAME XP YP)
  (SETQ XP (- X0 (TV:SHEET-INSIDE-LEFT)) YP (- Y0 (TV:SHEET-INSIDE-TOP)))

  ;; Decide which corner or edge of the character box we will move
  ;; (or maybe we aren't in range of any of them).
  ;; All horizontal edges move together, since the vertical dimensions
  ;; are not changeable for individual characters in a font.


  (LET ((XHALF (// BOX-X-SIZE 2))
	(YHALF (// BOX-Y-SIZE 2))
	(X1X (* (- CHAR-BOX-X1 WINDOW-X-POS) BOX-X-SIZE))
	(X2X (* (- CHAR-BOX-X2 WINDOW-X-POS) BOX-X-SIZE))
	(Y1Y (* (- CHAR-BOX-Y1 WINDOW-Y-POS) BOX-Y-SIZE))
	(Y2Y (* (- CHAR-BOX-Y2 WINDOW-Y-POS) BOX-Y-SIZE))
	(Y3Y (* (- CHAR-BOX-Y3 WINDOW-Y-POS) BOX-Y-SIZE)))
    ;;See if in y range if x box is a possibility
    
    (IF (< (- Y1Y YHALF) YP (+ Y2Y YHALF))
	(SETQ X-POS-NAME (COND  ((< (ABS (- XP X1X)) XHALF) 'CHAR-BOX-X1)
				((< (ABS (- XP X2X)) XHALF) 'CHAR-BOX-X2)
				(T NIL))))
    (IF (AND X-POS-NAME (= CHAR-BOX-X1 CHAR-BOX-X2))	;EVIL case
	(SETQ X-POS-NAME (IF (MINUSP (- XP X1X)) 'CHAR-BOX-X1 'CHAR-BOX-X2)))
    (IF (< (- X1X XHALF) XP (+ X2X XHALF))
	(SETQ Y-POS-NAME (COND ((< (ABS (- YP Y1Y)) YHALF) 'CHAR-BOX-Y1)
			       ((< (ABS (- YP Y2Y)) YHALF) 'CHAR-BOX-Y2)
			       ((< (ABS (- YP Y3Y)) YHALF) 'CHAR-BOX-Y3)
			       (T NIL))))
    (IF (AND (EQ Y-POS-NAME 'CHAR-BOX-Y1) (= CHAR-BOX-Y1 CHAR-BOX-Y2))
	(SETQ Y-POS-NAME (IF (MINUSP (- YP Y1Y)) 'CHAR-BOX-Y1 'CHAR-BOX-Y2)))
    (IF (AND (EQ Y-POS-NAME 'CHAR-BOX-Y2) (= CHAR-BOX-Y2 CHAR-BOX-Y3))
	(SETQ Y-POS-NAME (IF (MINUSP (- YP Y2Y)) 'CHAR-BOX-Y2 'CHAR-BOX-Y3))))
  (IF (NOT (OR X-POS-NAME Y-POS-NAME))
      (FUNCALL-SELF ':FATAL-ERROR "You're not near any char box edge that I see."))
  (WITH-MOUSE-GRABBED-ON-SHEET (SELF)
    (DO ((NOT-FIRST NIL T) (X) (Y) (OX) (OY) (OLD-M-X) (OLD-M-Y)
	 DELTA-Y)
	((AND NOT-FIRST (ZEROP TV:MOUSE-LAST-BUTTONS)))
      (AND NOT-FIRST (TV:MOUSE-WAIT OLD-M-X OLD-M-Y))
      (SETQ OLD-M-X TV:MOUSE-X OLD-M-Y TV:MOUSE-Y)
      (SETQ X (// (+ (// BOX-X-SIZE 2) TV:MOUSE-X) BOX-X-SIZE))
      (SETQ Y (// (+ (// BOX-Y-SIZE 2) TV:MOUSE-Y) BOX-Y-SIZE))
      ;; Exit if mouse is outside of FED grid area.
      (IF (AND (LESSP -1 X (1+ WINDOW-X-SIZE)) (LESSP -1 Y (1+ WINDOW-Y-SIZE)))
	  (PROGN
	    (SETQ X (+ X WINDOW-X-POS) Y (+ Y WINDOW-Y-POS))
	    ;; Try moving the edges, remember where they used to be.
	    (SETQ OX (SYMEVAL X-POS-NAME) OY (SYMEVAL Y-POS-NAME))
	    (AND X-POS-NAME (SET X-POS-NAME X))
	    (SETQ DELTA-Y (IF Y-POS-NAME (- Y OY) 0))
	    (INCF CHAR-BOX-Y1 DELTA-Y)
	    (INCF CHAR-BOX-Y2 DELTA-Y)
	    (INCF CHAR-BOX-Y3 DELTA-Y)
	    ;; Don't move an edge past or up to its opposite edge.
	    (OR (AND ( CHAR-BOX-X1 CHAR-BOX-X2)
		     (< CHAR-BOX-Y1 CHAR-BOX-Y2)
		     ( CHAR-BOX-Y2 CHAR-BOX-Y3))
		(PROGN (AND X-POS-NAME (SET X-POS-NAME OX))
		       (DECF CHAR-BOX-Y1 DELTA-Y)
		       (DECF CHAR-BOX-Y2 DELTA-Y)
		       (DECF CHAR-BOX-Y3 DELTA-Y)
		       (FUNCALL-SELF ':FATAL-ERROR "Won't squeeze box negative")))
	    ;; If we are really moving an edge to a new place, redisplay.
	    (OR (AND (OR (NOT X-POS-NAME)
			 (= (SYMEVAL X-POS-NAME) OX))
		     (ZEROP DELTA-Y))
		(PROGN
		  (FUNCALL-SELF ':CHAR-PARAM-REDISPLAY CHAR-BOX-X1 CHAR-BOX-X2
				CHAR-BOX-Y1 CHAR-BOX-Y2 CHAR-BOX-Y3)
		  (FUNCALL-SELF ':REDISPLAY))))))))

(DEFMETHOD (CHAR-BOX-GRID-MIXIN :CHAR-PARAM-REDISPLAY) (&REST IGNORE))	;ensure handler

(DEFMETHOD (CHAR-BOX-GRID-MIXIN :CHAR-BOX-PARAMETERS) ()
  (VALUES CHAR-BOX-X1 CHAR-BOX-X2 CHAR-BOX-Y1 CHAR-BOX-Y2 CHAR-BOX-Y3))

;;;
;;;  Gray Plane hacking
;;;

;;;Same basic hacks as now in PLANE-GRID-MIXIN -- Note the use of XORS only
;;;to update the screen, howsoever hairy.  This prevents the char-box drawer
;;;from having to be kludged.  "XORING is the only reasonable way to draw --"

(DEFCONST WHITE-KLUDGE-ARRAY (MAKE-ARRAY '(8. 4) ':TYPE ':ART-4B))
(DEFCONST BLACK-KLUDGE-ARRAY (LET ((A (MAKE-ARRAY '(8. 4) ':TYPE ':ART-4B)))
			       (FILLARRAY A '(#O17)) A))
(DEFCONST *GRAY-RDIS-LIMIT-CONSTANT* '(99999. 99999. -99999. -99999.))

(DEFFLAVOR GRAY-PLANE-MIXIN
	((GRAY-PLANE (MAKE-PLANE 2 ':TYPE 'ART-4B ':DEFAULT-VALUE 0 ':EXTENSION 8.))
							;the actual gray plane
	 GRAY-WINDOW-ARRAY			;the redisplay bit image
	 LIGHT-GRAY DARK-GRAY			;stipple-patterns
	 (STIPPLE-XOR-ARRAY (MAKE-ARRAY '(2 2 2 2) ':TYPE 'ART-Q))
	 (STIPPLE-ARRAY-ARRAY (MAKE-ARRAY '(2 2) ':TYPE 'ART-Q))
	 (PREVIOUS-GRAY-RDIS-LIMITS	;when replace gray, keeps old limits
	   *GRAY-RDIS-LIMIT-CONSTANT*))
	()
  (:INCLUDED-FLAVORS PLANE-GRID-MIXIN GRID-MIXIN))


;; Figure out stipple array sizes.

(DEFMETHOD (GRAY-PLANE-MIXIN :AFTER :DEDUCE-WINDOW-ARRAY-SIZE)
	   (&OPTIONAL (ARRAY-TYPE 'ART-1B))
  (SETQ LIGHT-GRAY (MAKE-ARRAY '(32. 2) ':TYPE ARRAY-TYPE)
	DARK-GRAY (MAKE-ARRAY '(32. 2) ':TYPE ARRAY-TYPE))
  (DOTIMES (I 32.)
    (ASET 0 LIGHT-GRAY I 1)
    (ASET 1 DARK-GRAY I 1)
    (LET* ((X (\ I 2))
	   (X (1- X)))
      (ASET X LIGHT-GRAY I 0)
      (ASET X DARK-GRAY I 0)))

  (ASET WHITE-KLUDGE-ARRAY STIPPLE-ARRAY-ARRAY 0 0)
  (ASET LIGHT-GRAY STIPPLE-ARRAY-ARRAY 0 1)
  (ASET BLACK-KLUDGE-ARRAY STIPPLE-ARRAY-ARRAY 1 0)
  (ASET DARK-GRAY STIPPLE-ARRAY-ARRAY 1 1)
  ;; Fill in no-change diagonals with constants
  (DOTIMES (BLACK 2)
    (DOTIMES (GRAY 2)
      (ASET WHITE-KLUDGE-ARRAY STIPPLE-XOR-ARRAY BLACK BLACK GRAY GRAY)))
  (DOTIMES (OLDBLACK 2)
    (DOTIMES (NEWBLACK 2)
      (DOTIMES (OLDGRAY 2)
	(DOTIMES (NEWGRAY 2)
	  (ASET
	    (OR (AREF STIPPLE-XOR-ARRAY NEWBLACK OLDBLACK NEWGRAY OLDGRAY)
		(LET ((A (MAKE-ARRAY '(32. 4) ':TYPE ARRAY-TYPE)))
		  (BITBLT TV:ALU-SETA BOX-X-SIZE 4
			  (AREF STIPPLE-ARRAY-ARRAY OLDBLACK OLDGRAY)
			  0 0 A 0 0)
		  (BITBLT TV:ALU-XOR BOX-X-SIZE 4
			  (AREF STIPPLE-ARRAY-ARRAY NEWBLACK NEWGRAY)
			  0 0 A 0 0)
		  A))
	    STIPPLE-XOR-ARRAY OLDBLACK NEWBLACK OLDGRAY NEWGRAY)))))
	  
  (SETQ GRAY-WINDOW-ARRAY (MAKE-ARRAY `(,WINDOW-X-SIZE ,WINDOW-Y-SIZE) ':TYPE ARRAY-TYPE)))

;;; See SPLAT-OUT-PLANE-DATA in PLANE-GRID-MIXIN
(DEFMACRO SPLAT-OUT-ALL-PLANE-DATA BODY
  `(LET ((GRAY-DIM-1 (ARRAY-DIMENSION-N 1 GRAY-PLANE))
	 (GRAY-DIM-2 (ARRAY-DIMENSION-N 2 GRAY-PLANE))
	 (GRAY-ORIGINS (PLANE-ORIGIN GRAY-PLANE))
	 (BLACK-DIM-1 (ARRAY-DIMENSION-N 1 PLANE))
	 (BLACK-DIM-2 (ARRAY-DIMENSION-N 2 PLANE))
	 (BLACK-ORIGINS (PLANE-ORIGIN PLANE)))
     (LET ((GRAY-XSTART (FIRST GRAY-ORIGINS))
	   (GRAY-XEND   (+ (FIRST GRAY-ORIGINS) GRAY-DIM-1))
	   (GRAY-YSTART (SECOND GRAY-ORIGINS))
	   (GRAY-YEND   (+ (SECOND GRAY-ORIGINS) GRAY-DIM-2))
	   (BLACK-XSTART (FIRST BLACK-ORIGINS))
	   (BLACK-XEND   (+ (FIRST BLACK-ORIGINS) BLACK-DIM-1))
	   (BLACK-YSTART (SECOND BLACK-ORIGINS))
	   (BLACK-YEND   (+ (SECOND BLACK-ORIGINS) BLACK-DIM-2)))
       (LET ((MINX (MIN BLACK-XSTART GRAY-XSTART))
	     (MAXX (MAX BLACK-XEND GRAY-XEND))
	     (MINY (MIN BLACK-YSTART GRAY-YSTART))
	     (MAXY (MAX BLACK-YEND GRAY-YEND)))
	 . ,BODY))))

(DEFMETHOD (GRAY-PLANE-MIXIN :CLEAR-GRAY-PLANE) (&OPTIONAL DIMLIST NEW-PLANE)	;compatibility
  (FUNCALL-SELF ':NEW-GRAY-PLANE DIMLIST NEW-PLANE))

;;; "Clear" the gray plane by making a new one.  Maximize redisplay limits.
;;; See general remarks at (:METHOD PLANE-GRID-MIXIN :NEW-PLANE).
(DEFMETHOD (GRAY-PLANE-MIXIN :NEW-GRAY-PLANE) (&OPTIONAL DIMLIST NEW-PLANE)
  (SPLAT-OUT-ALL-PLANE-DATA
    (SETQ PREVIOUS-GRAY-RDIS-LIMITS
	  (LIST (MIN MINX (FIRST PREVIOUS-GRAY-RDIS-LIMITS))
		(MIN MINY (SECOND PREVIOUS-GRAY-RDIS-LIMITS))
		(MAX MAXX (THIRD PREVIOUS-GRAY-RDIS-LIMITS))
		(MAX MAXY (FOURTH PREVIOUS-GRAY-RDIS-LIMITS)))))
  (SETQ GRAY-PLANE
	(OR NEW-PLANE (MAKE-PLANE 2 ':TYPE (ARRAY-TYPE GRAY-PLANE) ':DEFAULT-VALUE 0
				  ':EXTENSION 8.)))
  (IF (AND DIMLIST (NULL NEW-PLANE))
      (PROGN
	(PLANE-ASET 0 GRAY-PLANE (FIRST (FIRST DIMLIST)) (SECOND (FIRST DIMLIST)))
	(PLANE-ASET 0 GRAY-PLANE
		    (1- (FIRST (SECOND DIMLIST))) (1- (SECOND (SECOND DIMLIST))))))
  (SETQ GRAY-PLANE (FOLLOW-STRUCTURE-FORWARDING GRAY-PLANE))
  (FUNCALL-SELF ':MUST-REDISPLAY REDISPLAY-MAXIMALLY)
  GRAY-PLANE)

;;; Draw an appropriate point by xoring.  Note that access of the old values
;;; is cheap, as the screen array images are not planes, and are guaranteed
;;; to "be there".

(DEFMETHOD (GRAY-PLANE-MIXIN :DRAW-POINT) (I J &OPTIONAL NEW-VALUE FROM-REDISPLAY GRAY-VAL)
  (IF (NULL GRAY-VAL)
      (SETQ GRAY-VAL (PLANE-AREF GRAY-PLANE (+ I WINDOW-X-POS) (+ J WINDOW-Y-POS))))
  (TV:PREPARE-SHEET (SELF)			;note this is recursive
    (LET ((XS (+ (TV:SHEET-INSIDE-LEFT) (* I BOX-X-SIZE)))
	  (YS (+ (TV:SHEET-INSIDE-TOP) (* J BOX-Y-SIZE))))
      (BITBLT TV:ALU-XOR BOX-X-SIZE BOX-Y-SIZE
	      (AREF STIPPLE-XOR-ARRAY (AREF WINDOW-ARRAY I J) NEW-VALUE
		    (AREF GRAY-WINDOW-ARRAY I J) GRAY-VAL)
	      0 0 TV:SCREEN-ARRAY XS YS)))
  (ASET NEW-VALUE WINDOW-ARRAY I J)
  (ASET GRAY-VAL GRAY-WINDOW-ARRAY I J)
  (OR FROM-REDISPLAY
      (PROGN
	(FUNCALL-SELF ':ASET NEW-VALUE (+ I WINDOW-X-POS) (+ J WINDOW-Y-POS))
	;; Gray plane asets are not "modularized" as above- these msgpasses COST.
	(PLANE-ASET GRAY-VAL GRAY-PLANE (+ I WINDOW-X-POS) (+ J WINDOW-Y-POS)))))

(DEFMETHOD (GRAY-PLANE-MIXIN :GRAY-AREF) (I J)
  (PLANE-AREF GRAY-PLANE I J))
			      
(DEFMETHOD (GRAY-PLANE-MIXIN :BEFORE :REDISPLAY-ALL) ()
  (FILLARRAY GRAY-WINDOW-ARRAY '(0)))





;;;
;;; The actual box-by-box update code -- this is a primary method.
;;;

(DEFMETHOD (GRAY-PLANE-MIXIN :HEART-OF-REDISPLAY) (FORCE-TO-COMPLETION)
  (TV:PREPARE-SHEET (SELF)
;;; Here is even more baloney to open-code the plane-arefs to make this
;;; impossibly slow thing go even faster. This really is beyond barfuciousness.
;;; "Mussolini was no good at all, but he made the trains run on time"
;;;						   -Folk Wisdom

    (SPLAT-OUT-ALL-PLANE-DATA
      MAXY MAXX MINY MINX
      (DO-NAMED ABORT-REDISPLAY
		((J MIN-CHANGED-Y (1+ J))
;	     (AREF-HANDLER (GET-HANDLER-FOR SELF ':AREF))	;For speed
;	     (GRAY-AREF-HANDLER (GET-HANDLER-FOR SELF ':GRAY-AREF))
		 (LISTEN-HANDLER (GET-HANDLER-FOR SELF ':LISTEN))
		 (DRAW-HANDLER (GET-HANDLER-FOR SELF ':DRAW-POINT)))
		((> J MAX-CHANGED-Y)
		 (SETQ REDISPLAY-DEGREE REDISPLAY-NONE))
	(DO ((I MIN-CHANGED-X (1+ I))
	     (NEW-BLACK-VALUE)(NEW-GRAY-VALUE))
	    ((> I MAX-CHANGED-X))
	  ;; I have taken the considerable liberty of shooting generality to hell here,
	  ;; and not funcalled AREF handlers, because this redisplay is ALREADY
	  ;; too %$#@ slow, and this "optimization" helps visibly.. -bsg
	  
	  (LET ((PI (+ I WINDOW-X-POS))
		(PJ (+ J WINDOW-Y-POS)))
	    (OR (AND (= (SETQ NEW-BLACK-VALUE
			      (IF (OR (< PI BLACK-XSTART)
				      (< PJ BLACK-YSTART)
				      ( PI BLACK-XEND)
				      ( PJ BLACK-YEND))
				  0
				  (AREF PLANE (- PI BLACK-XSTART) (- PJ BLACK-YSTART))))
			(AREF WINDOW-ARRAY I J))
		     (= (SETQ NEW-GRAY-VALUE
			      (IF (OR (< PI GRAY-XSTART)
				      (< PJ GRAY-YSTART)
				      ( PI GRAY-XEND)
				      ( PJ GRAY-YEND))
				  0
				  (AREF GRAY-PLANE (- PI GRAY-XSTART) (- PJ GRAY-YSTART))))
			(AREF GRAY-WINDOW-ARRAY I J)))
		;;For some reason, passing the last arg fails to actually pass it, **********
		;;so --NEW-GRAY-VALUE) was removed. Beats me.
		(FUNCALL DRAW-HANDLER ':DRAW-POINT I J NEW-BLACK-VALUE T))))
	(COND ((AND (NOT FORCE-TO-COMPLETION) (FUNCALL LISTEN-HANDLER ':LISTEN)
		    (SETQ MIN-CHANGED-Y (1+ J))
		    (SETQ REDISPLAY-SUPPRESSED T)
		    (RETURN-FROM ABORT-REDISPLAY))))))))

;;; User ops which principally do their thing by sending :NEW-GRAY-PLANE - note that
;;; that worries about redisplay degree all it has to.

(DEFMETHOD (GRAY-PLANE-MIXIN :EXCHANGE-PLANES) ()
  (PSETQ PLANE GRAY-PLANE GRAY-PLANE PLANE)
  (FUNCALL-SELF ':MUST-REDISPLAY REDISPLAY-MAXIMALLY))


(DEFMETHOD (GRAY-PLANE-MIXIN :MERGE-PLANES) (&OPTIONAL (ALU TV:ALU-IOR) OLD-PLANE)
  (SETQ GRAY-PLANE (FOLLOW-STRUCTURE-FORWARDING GRAY-PLANE))
  (SPLAT-OUT-ALL-PLANE-DATA
    ;;(PROMPT-LINE "minx ~D miny ~D maxx ~D maxy ~D" MINX MINY MAXX MAXY)
    (SETQ OLD-PLANE (FOLLOW-STRUCTURE-FORWARDING PLANE))
    (FUNCALL-SELF ':NEW-PLANE `((,MINX ,MINY) (,MAXX ,MAXY)))
    (LET* ((OORIG (PLANE-ORIGIN PLANE))
	   (OXOF (FIRST OORIG))
	   (OYOF (SECOND OORIG)))
      (LOOP FOR I FROM MINX BELOW MAXX
	    DO
	    (LOOP FOR J FROM MINY BELOW MAXY
		  DO
		  ;; Apologia for open-coding plane-areffers (maybe they shd be macroes,
		  ;; or even macros? See above comment on Mussolini.
		  (ASET (BOOLE ALU
			       (IF (OR (< I GRAY-XSTART)
				       (< J GRAY-YSTART)
				       ( I GRAY-XEND)
				       ( J GRAY-YEND))
				   0
				   (AREF GRAY-PLANE (- I GRAY-XSTART) (- J GRAY-YSTART)))
			       (IF (OR (< I BLACK-XSTART)
				       (< J BLACK-YSTART)
				       ( I BLACK-XEND)
				       ( J BLACK-YEND))
				   0
				   (AREF OLD-PLANE (- I BLACK-XSTART) (- J BLACK-YSTART))))
			PLANE (- I OXOF) (- J OYOF)))))))

(DEFMETHOD (GRAY-PLANE-MIXIN :BEFORE :REDISPLAY) (&OPTIONAL IGNORE)
  (SETQ GRAY-PLANE (FOLLOW-STRUCTURE-FORWARDING GRAY-PLANE)))


;;; If all gray updated, then there is no old limit to remember.

(DEFMETHOD (GRAY-PLANE-MIXIN :AFTER :REDISPLAY) (&REST IGNORE)
  (IF (NOT REDISPLAY-SUPPRESSED)
      (SETQ PREVIOUS-GRAY-RDIS-LIMITS *GRAY-RDIS-LIMIT-CONSTANT*)))
	   


;;; See similar method in PLANE-GRID-MIXIN -- note we include his limits!
(DEFMETHOD (GRAY-PLANE-MIXIN :MINIMIZE-REDISPLAY-LIMITS) ()
  (SPLAT-OUT-ALL-PLANE-DATA
    (SETQ MIN-CHANGED-X (MAX MIN-CHANGED-X (- (MIN MINX (FIRST PREVIOUS-GRAY-RDIS-LIMITS)
						   (FIRST PREVIOUS-RDIS-LIMITS))
					      WINDOW-X-POS) 0)
	  MIN-CHANGED-Y (MAX MIN-CHANGED-Y (- (MIN MINY (SECOND PREVIOUS-GRAY-RDIS-LIMITS)
						   (SECOND PREVIOUS-RDIS-LIMITS))
					      WINDOW-Y-POS) 0)
	  MAX-CHANGED-X (MIN MAX-CHANGED-X (- (MAX MAXX (THIRD PREVIOUS-GRAY-RDIS-LIMITS)
						   (THIRD PREVIOUS-RDIS-LIMITS))
					      WINDOW-X-POS 1) (1- WINDOW-X-SIZE))
	  MAX-CHANGED-Y (MIN MAX-CHANGED-Y (- (MAX MAXY (FOURTH PREVIOUS-GRAY-RDIS-LIMITS)
						   (FOURTH PREVIOUS-RDIS-LIMITS))
					      WINDOW-Y-POS 1) (1- WINDOW-Y-SIZE)))))

(DEFMETHOD (GRAY-PLANE-MIXIN :MOVE-GRAY-PLANE) (XFROM YFROM XTO YTO)
  (SPLAT-OUT-ALL-PLANE-DATA
    MINX MAXX MINY MAXY
    (LET ((OLD (FOLLOW-STRUCTURE-FORWARDING GRAY-PLANE))
	  (XDISP (- XTO XFROM))
	  (YDISP (- YTO YFROM)))
      (FUNCALL-SELF  ':NEW-GRAY-PLANE
		     `((,(+ XDISP GRAY-XSTART) ,(+ YDISP GRAY-YSTART))	;xmin ymin
		       (,(+ XDISP GRAY-XEND) ,(+ YDISP GRAY-YEND))))	;xEND yEND
      (LET* ((OORIGIN (PLANE-ORIGIN GRAY-PLANE))
	    (OXOF (FIRST OORIGIN))
	    (OYOF (SECOND OORIGIN))
	    (ORIGINS (PLANE-ORIGIN OLD))
	    (IXOF (FIRST ORIGINS))
	    (IYOF (SECOND ORIGINS))
	    (XDIM (ARRAY-DIMENSION-N 1 OLD))
	    (YDIM (ARRAY-DIMENSION-N 2 OLD))
	    (XCOFF (+ XDISP (- IXOF OXOF)))
	    (YCOFF (+ YDISP (- IYOF OYOF))))
	(DOTIMES (I XDIM)
	  (DOTIMES (J YDIM)
	    (ASET (AREF OLD I J) GRAY-PLANE (+ I XCOFF) (+ J YCOFF))))))))



;;;The font editor grid pane itself

(DEFFLAVOR BASIC-FED ((CURRENT-FONT NIL)
		(CURRENT-CHARACTER NIL)
		(CURSOR-X 0)
		(CURSOR-Y 0)
		(CURSOR-ON NIL)
		BLINKER
		BLINKER-MARGIN)
	       (TV:STREAM-MIXIN			;get this guy's listen to override grid-noop
		CHAR-BOX-GRID-MIXIN
		GRAY-PLANE-MIXIN
		PLANE-GRID-MIXIN
		TV:BASIC-SCROLL-BAR
	        TV:LIST-MOUSE-BUTTONS-MIXIN TV:ANY-TYI-MIXIN)
  (:GETTABLE-INSTANCE-VARIABLES CURRENT-CHARACTER)
  (:DOCUMENTATION :SPECIAL-PURPOSE "The font editor itself
Uses its grid for displaying the character being edited."))

;;; Scroll bar stuff

(DEFMETHOD (BASIC-FED :SCROLL-TO) (TO TYPE)
  (IF (EQ TYPE ':ABSOLUTE)
      (FUNCALL-SELF ':SET-OFFSET WINDOW-X-POS
		    (IF (MINUSP WINDOW-Y-POS)
			(+ TO WINDOW-Y-POS)
			TO))
      (FUNCALL-SELF ':SET-OFFSET WINDOW-X-POS (+ WINDOW-Y-POS TO)))
  (TV:SHEET-FORCE-ACCESS (SELF)
    (FUNCALL-SELF ':REDISPLAY)))

(DEFMETHOD (BASIC-FED :SCROLL-POSITION) ()
  (LET* ((LARGENESS (SECOND (ARRAY-DIMENSIONS PLANE)))
	 (SIZE (MAX LARGENESS (+ WINDOW-Y-POS (* 2 WINDOW-Y-SIZE)))))
    (IF (MINUSP WINDOW-Y-POS)
	(VALUES 0 SIZE BOX-Y-SIZE WINDOW-Y-SIZE)
    (VALUES WINDOW-Y-POS SIZE BOX-Y-SIZE WINDOW-Y-SIZE))))

(DEFMETHOD (BASIC-FED :AFTER :INIT) (&REST IGNORE)
  (FUNCALL-SELF ':ERASE-ALL)
  (SETQ BLINKER (TV:MAKE-BLINKER SELF
				 'TV:RECTANGULAR-BLINKER
				 ':VISIBILITY NIL ':DESELECTED-VISIBILITY NIL
				 ':HALF-PERIOD 10.)))

(DEFMETHOD (BASIC-FED :SET-HAND-BLINKER-MARGIN) ()
  (SETQ BLINKER-MARGIN (// (MIN BOX-X-SIZE BOX-Y-SIZE) 4))
  (FUNCALL BLINKER
	   ':SET-SIZE
	   (- BOX-X-SIZE (* 2 BLINKER-MARGIN))
	   (- BOX-Y-SIZE (* 2 BLINKER-MARGIN))))

(DEFMETHOD (BASIC-FED :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (FUNCALL-SELF ':SET-HAND-BLINKER-MARGIN)
  (FUNCALL-SELF ':HOME-BOX))

;;; KLUDGE to run all of the FED commands in the FED-PANE flavor-instance environment,

(DEFMETHOD (BASIC-FED :FED-PANE-FUNCALL) (&REST ARGS)
  (APPLY 'FUNCALL ARGS))

(DEFMETHOD (BASIC-FED :FED-PANE-APPLY) (FCN ARGLIST)
  (APPLY FCN ARGLIST))

(DEFMETHOD (BASIC-FED :AFTER :REFRESH) (&OPTIONAL IGNORE)
  (OR (FUNCALL-SELF ':LISTEN)
      (FUNCALL-SELF ':FORCE-KBD-INPUT '(:FED-BLIP :AFTER-REFRESH))))	;cause redisp in
						;the right process by waking cmd loop
 ;; I think you should even do this if tv:restored-bits-p, to cause char pane update.


(DEFMETHOD (BASIC-FED :CHAR-PARAM-REDISPLAY) (&REST IGNORE)
  (FUNCALL TV:SUPERIOR ':UPDATE-STATUS-PANE))

(DEFMETHOD (BASIC-FED :FATAL-ERROR) (&REST ARGS)
  (APPLY #'BARPH ARGS))				;handle char-box errors

(DEFMETHOD (BASIC-FED :WHO-LINE-DOCUMENTATION-STRING) ()
  (IF (BOUNDP 'DRAW-MODE)
      (SELECTQ DRAW-MODE
	(7 "L:Draw dots, M:Change mode (Draw//Erase//Flip), R:Move edges of char box, R2:System menu")
	(2 "L:Erase dots, M:Change mode (Draw//Erase//Flip), R:Move edges of char box, R2:System menu")
	(6 "L:Flip dots, M:Change mode (Draw//Erase//Flip), R:Move edges of char box, R2:System menu"))))


(DEFMETHOD (BASIC-FED :SPECIFY-FONT) (FONT)
  (SETQ CURRENT-FONT FONT CURRENT-CHARACTER NIL))

;;changed not to home box. do yourself, if that be your fancy.
(DEFMETHOD (BASIC-FED :ERASE-ALL) ()
  (FUNCALL-SELF ':NEW-PLANE)
  (IF (NULL CURRENT-FONT)
      (SETQ CHAR-BOX-X1 0 CHAR-BOX-Y1 0 CHAR-BOX-X2 7 CHAR-BOX-Y2 11 CHAR-BOX-Y3 14)
      (LET ((FD (FONT-GET-FD CURRENT-FONT)))
	(IF (NULL CURRENT-CHARACTER)
	    (SETQ CHAR-BOX-Y2 (FD-BASELINE FD)	;no cc, eat font parms, else leave ALONE.
		  CHAR-BOX-X2 (FIXR (FD-SPACE-WIDTH FD))
		  CHAR-BOX-Y3 (FD-LINE-SPACING FD))))))



;;Return the window of the fed window to home position, which is centered.
(DEFMETHOD (BASIC-FED :HOME-BOX) ()
  (LET ((CHAR-BOX-WIDTH (- CHAR-BOX-X2 CHAR-BOX-X1))
	(CHAR-BOX-HEIGHT (- CHAR-BOX-Y3 CHAR-BOX-Y1)))
    (FUNCALL-SELF ':SET-OFFSET
		  (+ CHAR-BOX-X1 (// (- CHAR-BOX-WIDTH WINDOW-X-SIZE) 2))
		  (+ CHAR-BOX-Y1 (// (- CHAR-BOX-HEIGHT WINDOW-Y-SIZE) 2))))
  (SETQ CURSOR-X 0 CURSOR-Y 0))

;;;Screw around with the non-mouse cursor.
(DEFMETHOD (BASIC-FED :BEFORE :SET-OFFSET) (X Y)
  (SETQ CURSOR-X (MAX 0 (MIN WINDOW-X-SIZE (- CURSOR-X (- X WINDOW-X-POS)))))
  (SETQ CURSOR-Y (MAX 0 (MIN WINDOW-Y-SIZE (- CURSOR-Y (- Y WINDOW-Y-POS))))))

;;; Actually update the visible position of the non-mouse cursor.
(DEFMETHOD (BASIC-FED :AFTER :REDISPLAY) (&OPTIONAL IGNORE)
  (IF (NOT REDISPLAY-SUPPRESSED)
      (IF CURSOR-ON
	  (PROGN
	    (FUNCALL BLINKER ':SET-CURSORPOS
		     (+ (* BOX-X-SIZE CURSOR-X)
			BLINKER-MARGIN
			(IF (< BOX-X-SIZE MIN-BOX-SIZE) 0 GRID-POINT-SIZE))
		     (+ (* BOX-Y-SIZE CURSOR-Y)
			BLINKER-MARGIN
			(IF (< BOX-Y-SIZE MIN-BOX-SIZE) 0 GRID-POINT-SIZE)))
	    (FUNCALL BLINKER ':SET-VISIBILITY ':BLINK))
	  (FUNCALL BLINKER ':SET-VISIBILITY NIL))))

;Return  the X and Y co-ords of the grid point the user clicks on
;if he clicks with the left button.  Return NIL, ENCODING if he clicks anything else.
;If he types something, untyi it and return NIL.

(DEFMETHOD (BASIC-FED :MOUSE-SELECT-POINT) (&AUX CH X Y ENCODE)
  (SETQ CH (FUNCALL-SELF ':ANY-TYI))
  (COND ((AND (LISTP CH)			;mousing upon us
	      (EQ (CAR CH) ':MOUSE-BUTTON)
	      (EQ (THIRD CH) SELF))
	 (SETQ ENCODE (SECOND CH))
	 (IF  (= (LDB %%KBD-MOUSE-BUTTON ENCODE) 0)
	      (PROGN
		(SETQ X (// (- (FOURTH CH) TV:LEFT-MARGIN-SIZE) BOX-X-SIZE)
		      Y (// (- (FIFTH CH) TV:TOP-MARGIN-SIZE) BOX-Y-SIZE))
		(COND ((AND (LESSP -1 X WINDOW-X-SIZE) (LESSP -1 Y WINDOW-Y-SIZE))
		       (VALUES X Y))
		      (T (VALUES NIL ENCODE))))
	      (VALUES NIL ENCODE)))
	(T (FUNCALL-SELF ':UNTYI CH)
	   NIL)))

(COMPILE-FLAVOR-METHODS BASIC-FED)



;;;
;;; These things define a flavor, BASIC-FED-PANE, which is a fed grid window,
;;; but knows how to act like part of the FED frame family, and send messages
;;; left and right to the other panes to say what's going on and
;;; "provide amusing and engaging displays".
;;;

(DEFFLAVOR BASIC-FED-PANE () (TV:PANE-MIXIN TV:DONT-SELECT-WITH-MOUSE-MIXIN
			      BASIC-FED TV:WINDOW)
  (:DEFAULT-INIT-PLIST :LABEL NIL))

;;;Update char pane whenn [sic] a real redisplay did something and finished it.
(DEFWRAPPER (BASIC-FED-PANE :REDISPLAY) (IGNORE . BODY)
  `(LET ((WAS-DEGREE REDISPLAY-DEGREE))
     (PROGN . ,BODY)
     (IF (AND (NOT REDISPLAY-SUPPRESSED)
	      (> WAS-DEGREE REDISPLAY-NONE))
	 (FUNCALL TV:SUPERIOR ':REDISPLAY-CHAR-PANE))))

(DEFMETHOD (BASIC-FED-PANE :AFTER :ERASE-ALL) ()
  (FUNCALL TV:SUPERIOR ':CLEAR-CHAR-PANE))

(DEFMETHOD (BASIC-FED-PANE :AFTER :MOUSE-BOOLE-SQUARES) (&REST IGNORE)
  (FUNCALL TV:SUPERIOR ':REDISPLAY-CHAR-PANE))

(DEFMETHOD (BASIC-FED-PANE :AFTER :BOOLE-MODE-CHANGE) (BOOLE)
  (FUNCALL TV:SUPERIOR ':UPDATE-DRAW-MODE-PANE BOOLE))

(DEFMETHOD (BASIC-FED-PANE :AFTER :EXPOSE) (&REST IGNORE)
  (OR (FUNCALL-SELF ':LISTEN)
      ;;Make the command loop wake up
      (FUNCALL-SELF ':FORCE-KBD-INPUT '(:FED-BLIP :AFTER-EXPOSE))))


(COMPILE-FLAVOR-METHODS BASIC-FED-PANE)

;;; Dealings with the prompt line pane, which is NOT a (really) special flavor.
;;; This is still dealt with kludgily via special variables, last thing in
;;; FED which is still so, preventing multi-FEDs. To be fixed..

(DEFVAR PROMPT-LINE-USED)			;Non-NIL when the prompt-window was typed on

(DEFUN PROMPT-LINE (STRING &REST FORMAT-ARGS)
  (FUNCALL PROMPT-WINDOW ':CLEAR-SCREEN)
  (LEXPR-FUNCALL #'FORMAT PROMPT-WINDOW STRING FORMAT-ARGS)
  (SETQ PROMPT-LINE-USED T))

(DEFUN PROMPT-LINE-TYO (X)			;assume to be cleared by above call
  (FUNCALL PROMPT-WINDOW ':TYO X))

(DEFUN PROMPT-LINE-READLINE (&OPTIONAL STRING &REST FORMAT-ARGS)
  (TV:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (LEXPR-FUNCALL #'PROMPT-LINE STRING FORMAT-ARGS))
    (SETQ PROMPT-LINE-USED T)
    (READLINE PROMPT-WINDOW)))

(DEFUN PROMPT-LINE-DEFAULTED-READLINE (STRING DEFAULT &REST MORE-FORMAT-ARGS)
  (LET ((ANS (LEXPR-FUNCALL #'PROMPT-LINE-READLINE STRING DEFAULT MORE-FORMAT-ARGS)))
    (IF (STRING-SEARCH-NOT-SET '(#\SP #\TAB) ANS)
	ANS
      (LET ((BASE 10.))
	(FORMAT NIL "~A" DEFAULT)))))		;caller EXPECTS chars...

(DEFUN PROMPT-LINE-READ (&OPTIONAL STRING &REST FORMAT-ARGS)
  (TV:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (LEXPR-FUNCALL #'PROMPT-LINE STRING FORMAT-ARGS))
    (SETQ PROMPT-LINE-USED T)
    (READ PROMPT-WINDOW)))

(DEFUN PROMPT-LINE-Y-OR-N-P (&OPTIONAL STRING &REST FORMAT-ARGS)
  (TV:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (LEXPR-FUNCALL #'PROMPT-LINE STRING FORMAT-ARGS))
    (SETQ PROMPT-LINE-USED T)
    (Y-OR-N-P NIL PROMPT-WINDOW)))


;;; Read a char from the KBD or the Character Select Menu.
(DEFUN PROMPT-CHARACTER (&OPTIONAL STRING &REST FORMAT-ARGS &AUX CH)
  (DECLARE (SPECIAL TV:SUPERIOR))
  (TV:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (FUNCALL
		  #'PROMPT-LINE
		  (FORMAT NIL "~A (type one char, or mouse char select menu): "
			  (LEXPR-FUNCALL ':FORMAT NIL STRING FORMAT-ARGS)))) 
    (DO () (())
      (SETQ CH (FUNCALL-SELF ':ANY-TYI))
      (COND ((FIXP CH)
	     (RETURN CH))
	    ((AND (LISTP CH)
		  (EQ (CAR CH) ':MENU)
		  (EQ (FOURTH CH) (FUNCALL TV:SUPERIOR ':ALPHABET-MENU)))
	     (RETURN (SETQ CH (CHARACTER (SECOND CH)))))
	    ((AND (LISTP CH)
		  (EQ (CAR CH) ':FED-BLIP)))	;ignore this, ok
	    (T (TV:BEEP))))
    (FORMAT PROMPT-WINDOW "~:C" CH)
    (SETQ PROMPT-LINE-USED T)
    CH))


;;;
;;; The typeout window for FED needs all kind of special treatment.
;;; Regular typeout windows are no good, cause they have KLUDGE-INFERIOR-MIXIN,
;;; which causes and infinite recursion at deselect time.  The superior
;;; of our typeout window will be the fed frame, but he will not be that
;;; which is selected (some pane will) at expose time.   Therefore, we
;;; roll our own.  (MELTWAX does this too).

;;; He is an inferior of the frame, but NOT a pane.

(DEFFLAVOR AUTOSELECTING-TYPEOUT-WINDOW-NON-PANE ()	;no kludge-inferiors mixin
	   (TV:ANY-TYI-MIXIN TV:PANE-MIXIN TV:BASIC-TYPEOUT-WINDOW TV:WINDOW)
    (:DEFAULT-INIT-PLIST :LABEL NIL :BORDERS NIL))

(DEFMETHOD (AUTOSELECTING-TYPEOUT-WINDOW-NON-PANE :AFTER :INIT) (&REST IGNORE)
  (FUNCALL-SELF ':SET-IO-BUFFER (FUNCALL TV:SUPERIOR ':IO-BUFFER)))

(DEFMETHOD (AUTOSELECTING-TYPEOUT-WINDOW-NON-PANE :AFTER :EXPOSE) (&REST IGNORE)
  (FUNCALL-SELF ':SELECT))
  
;;;
;;; Now mouse-sensitivity is needed for the font display.
;;;

(DEFFLAVOR FED-MOUSABLE-TYPEOUT-WINDOW ()
	   (TV:BASIC-MOUSE-SENSITIVE-ITEMS AUTOSELECTING-TYPEOUT-WINDOW-NON-PANE))

(DEFMETHOD (FED-MOUSABLE-TYPEOUT-WINDOW :AFTER :INIT) (PLIST)
  (SETQ TV:ITEM-TYPE-ALIST			;
	(OR (GET PLIST 'TV:ITEM-TYPE-ALIST)
	    '((FONT :SELECT-FONT "Select this font for editing")
	      (CHARACTER :SELECT-CHAR "Select this character")))))
						       

(COMPILE-FLAVOR-METHODS AUTOSELECTING-TYPEOUT-WINDOW-NON-PANE FED-MOUSABLE-TYPEOUT-WINDOW)


;;; Miscellaneous flavors ... needed by some members of the frame.

(DEFFLAVOR HIGHLIGHTING-COMMAND-MENU-PANE ()
	   (TV:MENU-HIGHLIGHTING-MIXIN TV:DONT-SELECT-WITH-MOUSE-MIXIN TV:COMMAND-MENU-PANE))

;;; The status pane - like most special FED panes, the FED frame forwards it messages.
;;; 

(DEFFLAVOR FED-STATUS-PANE
	()
	(TV:DONT-SELECT-WITH-MOUSE-MIXIN TV:WINDOW-PANE)
  (:DEFAULT-INIT-PLIST :MORE-P NIL))

(DEFMETHOD (FED-STATUS-PANE :AFTER :REFRESH) (&REST IGNORE)
  (IF (NOT TV:RESTORED-BITS-P)
      (PROGN
	(FUNCALL-SELF ':DRAW)
	(FUNCALL-SELF ':REDISPLAY))))


(DEFMETHOD (FED-STATUS-PANE :DRAW) ()
  (FUNCALL-SELF ':HOME-CURSOR)
  (FUNCALL-SELF ':CLEAR-EOF)
  (FORMAT SELF "Font:~%Character:~%Width:"))

(DEFMETHOD (FED-STATUS-PANE :REDISPLAY) ()
  (TV:SHEET-FORCE-ACCESS (SELF)
    (FUNCALL-SELF ':SET-CURSORPOS 12. 0 ':CHARACTER)
    (FUNCALL-SELF ':CLEAR-EOL)
    (LET ((F (FUNCALL TV:SUPERIOR ':CURRENT-FONT)))
      (FORMAT SELF (IF F (STRING F) "No Font")))
    (FUNCALL-SELF ':SET-CURSORPOS 12. 1 ':CHARACTER)
    (FUNCALL-SELF ':CLEAR-EOL)
    (LET ((FED-PANE (FUNCALL TV:SUPERIOR ':GET-PANE 'FED-PANE)))
      (LET ((C (FUNCALL FED-PANE ':CURRENT-CHARACTER)))
	(IF C (FORMAT SELF "~C (~3,48O)" C C) (FORMAT SELF "None"))
	(FUNCALL-SELF ':SET-CURSORPOS 12. 2 ':CHARACTER)
	(FUNCALL-SELF ':CLEAR-EOL)
	(MULTIPLE-VALUE-BIND (X1 X2)
	    (FUNCALL FED-PANE ':CHAR-BOX-PARAMETERS)
	  (FORMAT SELF "~D" (- X2 X1)))))))

(COMPILE-FLAVOR-METHODS FED-STATUS-PANE)

;;;
;;; The character/sample-text pane.
;;;

(DEFFLAVOR CHARACTER-PANE ((SAMPLE-STRING NIL) (CHAR NIL) (EVAL-PANE NIL))
	   (TV:DONT-SELECT-WITH-MOUSE-MIXIN TV:LIST-MOUSE-BUTTONS-MIXIN TV:WINDOW-PANE)
  (:DEFAULT-INIT-PLIST :MORE-P NIL :SAVE-BITS ':DELAYED)
  ;;NECESSARY or no update on reselect.
  :SETTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (CHARACTER-PANE :WHO-LINE-DOCUMENTATION-STRING) ()
  "Click any button to set sample string")

;;; Don't need (:AFTER :REFRESH), other window will send msgs as needed.

(DEFMETHOD (CHARACTER-PANE :REDISPLAY) ()
  (TV:SHEET-FORCE-ACCESS (SELF)
    (AND EVAL-PANE
	 CHAR
	 (PROGN
	   (FUNCALL-SELF ':CLEAR-SCREEN)		;wd like to do better than this
	   (FUNCALL-SELF ':SET-CURSORPOS 0 0)
	   (FUNCALL EVAL-PANE ':FED-PANE-FUNCALL #'FED-TYO SELF CHAR)))
    (IF (AND EVAL-PANE SAMPLE-STRING)
	(PROGN
	  (FUNCALL-SELF ':SET-CURSORPOS 5 0 ':CHARACTER)
	  (FUNCALL EVAL-PANE ':FED-PANE-FUNCALL #'FED-PRINC SAMPLE-STRING SELF)))))

;;; Used at configuration compute size. Make sure one line of current font will fit.
(DEFMETHOD (CHARACTER-PANE :WANTED-SIZE) (&REST IGNORE)
  (MULTIPLE-VALUE-BIND (IGNORE YLOW IGNORE YHIGH)
      (FUNCALL-SELF ':EDGES)
    (MULTIPLE-VALUE-BIND (IGNORE YSIZE)
	(FUNCALL-SELF ':INSIDE-SIZE)
    (+ 3 TV:LINE-HEIGHT (- YHIGH YLOW YSIZE)))))

(DEFMETHOD (CHARACTER-PANE :SET-SAMPLE-STRING) (STRING)
  (SETQ SAMPLE-STRING STRING)
  (FUNCALL-SELF ':REDISPLAY))


(DEFUN FED-PRINC (STRING W)
  (DOTIMES (I (STRING-LENGTH STRING)) (FED-TYO W (AREF STRING I))))

;;;
;;; Character "Q Registers"  28 Sept 1981


(DEFFLAVOR REGISTER-MIXIN ((REGISTERS) NCOLS NROWS BOX-HEIGHT BOX-WIDTH
			   X-CENTERING-OFFSET Y-CENTERING-OFFSET)
	   (TV:BASIC-MOUSE-SENSITIVE-ITEMS)
  (:INCLUDED-FLAVORS TV:WINDOW)
  (:DEFAULT-INIT-PLIST :MORE-P NIL :SAVE-BITS ':DELAYED :BLINKER-P NIL)  
  (:SETTABLE-INSTANCE-VARIABLES REGISTERS))

(DEFMETHOD (REGISTER-MIXIN :AFTER :INIT) (&REST IGNORE)
  (SETQ TV:ITEM-TYPE-ALIST
	'((REGISTER				;this is mishegoss.
	    :DEFAULT-FETCH-REGISTER
	    "L: Save black to empty reg, retrieve non-empty.    R: Menu"
	    ("Store Black" :REG-STORE-BLACK)
	    ("Store Gray" :REG-STORE-GRAY)
	    ("Fetch Black" :REG-FETCH-BLACK)
	    ("Fetch Gray" :REG-FETCH-GRAY)))))

;;; When stacked vertically, let one line of the current font fit.  When
;;; not stacked vertically, this isn't :ASKed, and it gets a :EVEN constraint spec.
(DEFMETHOD (REGISTER-MIXIN :PANE-SIZE) (IGNORE IGNORE IGNORE IGNORE STACKING
					      &REST IGNORE)
  ;;only called in vertical case
  (IF (NEQ STACKING ':VERTICAL)
      (FERROR NIL "Called for non-vertical stacking"))

  (+ 2
     (TV:SHEET-LINE-HEIGHT SELF)
     (TV:SHEET-TOP-MARGIN-SIZE SELF)
     (TV:SHEET-BOTTOM-MARGIN-SIZE SELF)))

;;; Figure out how many visible registers and centering info.
(DEFMETHOD (REGISTER-MIXIN :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (MULTIPLE-VALUE-BIND (WIDTH HEIGHT) (FUNCALL-SELF ':INSIDE-SIZE)
    (SETQ BOX-HEIGHT (TV:SHEET-LINE-HEIGHT SELF) BOX-WIDTH (TV:SHEET-CHAR-WIDTH SELF))
    (SETQ NCOLS (// (1- WIDTH) (+ 1 BOX-WIDTH))
	  NROWS (// (1- HEIGHT) (+ 1 BOX-HEIGHT)) 
	  X-CENTERING-OFFSET
	  (// (- (TV:SHEET-INSIDE-WIDTH) (+ 1 (* NCOLS (1+ BOX-WIDTH)))) 2)
	  Y-CENTERING-OFFSET
	  (// (- (TV:SHEET-INSIDE-HEIGHT) (+ 1 (* NROWS (1+ BOX-HEIGHT)))) 2))))

;;; Reconfigure thyself.
(DEFMETHOD (REGISTER-MIXIN :SET-FONT) (FONT)
  (FUNCALL-SELF ':SET-FONT-MAP (LIST FONT))
  (FUNCALL-SELF ':CHANGE-OF-SIZE-OR-MARGINS)
  (FUNCALL-SELF ':REFRESH))

;;; Draw all boxes and  known characters.
(DEFMETHOD (REGISTER-MIXIN :AFTER :REFRESH) (&OPTIONAL TYPE)
  (OR (AND TV:RESTORED-BITS-P (NEQ TYPE ':SIZE-CHANGED))
      (TV:PREPARE-SHEET (SELF)
	(FUNCALL-SELF ':CLEAR-SCREEN)		;when bits are restored, WE alone do this..
	(DOTIMES (I NCOLS)
	  (DOTIMES (J NROWS)
	    (REGISTER-PANE-DRAW-BOX I J)
	    (FUNCALL-SELF
	      ':PRIMITIVE-ITEM
	      'REGISTER
	      (+ I (* J NCOLS))
	      (+ (* I (1+ BOX-WIDTH)) (TV:SHEET-INSIDE-LEFT) X-CENTERING-OFFSET)
	      (+ (* J (1+ BOX-HEIGHT)) (TV:SHEET-INSIDE-TOP) Y-CENTERING-OFFSET)
	      (+ (- (* (1+ I) (1+ BOX-WIDTH)) 2) (TV:SHEET-INSIDE-LEFT) X-CENTERING-OFFSET)
	      (+ (- (* (1+ J) (1+ BOX-HEIGHT)) 2)
		 (TV:SHEET-INSIDE-LEFT) Y-CENTERING-OFFSET))
	    (REGISTER-PANE-DRAW-REGISTER I J)))
        (LET ((XFINAL (+ (* NCOLS (1+ BOX-WIDTH)) X-CENTERING-OFFSET))
	      (YFINAL (+ (* NROWS (1+ BOX-HEIGHT)) Y-CENTERING-OFFSET)))
	  (FUNCALL-SELF ':DRAW-LINE X-CENTERING-OFFSET YFINAL XFINAL YFINAL)
	  (FUNCALL-SELF ':DRAW-LINE XFINAL Y-CENTERING-OFFSET XFINAL YFINAL)))))

;;;Note we only draw a single-width L-shaped halfbox, they mesh and the outside is
;;;handled above.
(DECLARE-FLAVOR-INSTANCE-VARIABLES (REGISTER-MIXIN)
(DEFUN REGISTER-PANE-DRAW-BOX (I J)
  (LET ((GROSS-X (+ (* I (+ 1 BOX-WIDTH)) (TV:SHEET-INSIDE-LEFT SELF) X-CENTERING-OFFSET))
	(GROSS-Y (+ (* J (+ 1 BOX-HEIGHT)) (TV:SHEET-INSIDE-TOP SELF) Y-CENTERING-OFFSET)))
    (LET ((FINAL-X (+ GROSS-X (TV:SHEET-CHAR-WIDTH SELF) 1))
	  (FINAL-Y (+ GROSS-Y (TV:SHEET-LINE-HEIGHT SELF) 1)))
      (%DRAW-LINE GROSS-X
		  GROSS-Y
		  FINAL-X
		  GROSS-Y
		  TV:ALU-IOR T SELF)
      (%DRAW-LINE GROSS-X
		  GROSS-Y
		  GROSS-X
		  FINAL-Y
		  TV:ALU-IOR T SELF)))))



;;; Interpret a mouse-sensitive item click.
(DEFMETHOD (REGISTER-MIXIN :INTERPRET-MOUSING) (BLIP)
  (OR REGISTERS (SETQ REGISTERS
		      (MAKE-PLANE 1 ':TYPE ':ART-Q ':EXTENSION 10.)))
  (LIST SELF (PLANE-AREF REGISTERS (THIRD BLIP)) (THIRD BLIP) (SECOND BLIP)))

;;; The commands cons these up.
(DEFSTRUCT (FEDREG :CONC-NAME :NAMED) X1 X2 Y1 Y2 Y3 PLANE)

;;; Store a FEDREG in the REGISTERS array//instance variable.
(DEFMETHOD (REGISTER-MIXIN :SET-FEDREG) (N R)
  (IF (NULL REGISTERS)
      (SETQ REGISTERS (MAKE-PLANE 1 ':TYPE ':ART-Q ':EXTENSION 10.)))
  (PLANE-ASET R REGISTERS N)
  (FUNCALL-SELF ':DRAW-RECTANGLE
		(TV:SHEET-CHAR-WIDTH SELF)
		(TV:SHEET-LINE-HEIGHT SELF)
		(+ (* (\ N NCOLS) (1+ BOX-WIDTH)) 1 X-CENTERING-OFFSET)
		(+ (* (// N NCOLS) (1+ BOX-HEIGHT)) 1 Y-CENTERING-OFFSET)
		TV:ALU-ANDCA)
  (REGISTER-PANE-DRAW-REGISTER (\ N NCOLS) (// N NCOLS)))

;;; Draw a register.  Try every conceivable trick for centering it,
;;; showing the middle of it, or showing the middle of its char-box in
;;; both dimensions.  Blankness here would be a big loss.
(DECLARE-FLAVOR-INSTANCE-VARIABLES (REGISTER-MIXIN)
(DEFUN REGISTER-PANE-DRAW-REGISTER (I J &AUX (R (AND REGISTERS
						     (PLANE-AREF REGISTERS
								 (+ I (* J NCOLS))))))
  (IF R
      (LET ((WXSTART (+ (TV:SHEET-INSIDE-LEFT SELF)
			X-CENTERING-OFFSET
			1 (* I (1+ BOX-WIDTH))))
	    (WYSTART (+ (TV:SHEET-INSIDE-TOP SELF)
			Y-CENTERING-OFFSET
			1 (* J (1+ BOX-HEIGHT))))
	    (X1 (FEDREG-X1 R))
	    (X2 (FEDREG-X2 R))
	    (Y1 (FEDREG-Y1 R))
	    (Y3 (FEDREG-Y3 R))
	    (W (TV:SHEET-CHAR-WIDTH SELF))
	    (H (TV:SHEET-LINE-HEIGHT SELF))
	    (PLANE (FEDREG-PLANE R))
	    X0 XEXT Y0 YEXT XD0 YD0)
	(SPLAT-OUT-PLANE-DATA			;hah, pretty clever, huh?
	  (COND (( DIM-1 W)			;great, use this centering?
		 (SETQ X0 XSTART XEXT DIM-1 XD0 (// (- W DIM-1) 2)))
		(T (LET* ((XC (// (+ X1 X2) 2))
			  (XGO (- XC (// W 2))))
		     (IF (< XGO XSTART)
			 (SETQ X0 XSTART XEXT W XD0 0)
			 (LET ((XFIN (+ XGO W)))
			   (IF ( XFIN XEND)
			       (SETQ XC (// (+ XSTART XEND) 2)
				     X0 (- XC (// W 2))
				     XEXT W
				     XD0 0)
			       (SETQ X0 XGO XEXT W XD0 0)))))))
	  (COND (( DIM-2 H)			;great, use this centering?
		 (SETQ Y0 YSTART YEXT DIM-2 YD0 (// (- H DIM-2) 2)))
		(T (LET* ((YC (// (+ Y1 Y3) 2))
			  (YGO (- YC (// H 2))))
		     (IF (< YGO YSTART)
			 (SETQ Y0 YSTART YEXT H YD0 0)
			 (LET ((YFIN (+ YGO H)))
			   (IF ( YFIN YEND)
			       (SETQ YC (// (+ YSTART YEND) 2)
				     Y0 (- YC (// H 2))
				     YEXT H
				     YD0 0)
			       (SETQ Y0 YGO YEXT H YD0 0)))))))
	  (TV:PREPARE-SHEET (SELF)
	    (DOTIMES (I XEXT)
	      (DOTIMES (J YEXT)
		(IF (NOT (ZEROP (AREF PLANE (- (+ I X0) XSTART) (- (+ J Y0) YSTART))))
		    (%DRAW-RECTANGLE 1 1
				     (+ WXSTART I XD0)
				     (+ WYSTART J YD0) TV:ALU-IOR SELF))))))))))




(DEFFLAVOR REGISTER-PANE ()
	   (REGISTER-MIXIN TV:DONT-SELECT-WITH-MOUSE-MIXIN TV:WINDOW-PANE))

(COMPILE-FLAVOR-METHODS REGISTER-MIXIN REGISTER-PANE)


;;; This is total dead-bearage and should be fixed in the window-system.

(DEFFLAVOR UNSELECTABLE-PANE ()
	   (TV:DONT-SELECT-WITH-MOUSE-MIXIN TV:ANY-TYI-MIXIN TV:WINDOW-PANE))

(DEFFLAVOR UNSELECTABLE-CHVV-PANE ()
	   (TV:DONT-SELECT-WITH-MOUSE-MIXIN TV:CHOOSE-VARIABLE-VALUES-PANE))

(COMPILE-FLAVOR-METHODS CHARACTER-PANE UNSELECTABLE-PANE UNSELECTABLE-CHVV-PANE)

;;;
;;; Constants and auxiliary code needed for windows created at FED frame
;;; creation time.

(DEFCONST FONT-PARAMETERS-CHVV-LIST		;dat's choose-variable-values
	  '((*CURFONT-BLINKER-HEIGHT* "Blinker Height" FED-DECNUM)
	    (*CURFONT-BLINKER-WIDTH* "Blinker Width" FED-DECNUM)
	    (*CURFONT-BASELINE* "Base Line" FED-DECNUM)
	    (*CURFONT-CHAR-HEIGHT* "Character Height" FED-DECNUM)))

(DEFPROP FED-DECNUM (FED-DECNUM-PRINT FED-DECNUM-READ NIL NIL NIL "A number, in decimal")
	 TV:CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFUN FED-DECNUM-PRINT (X STREAM) (FORMAT STREAM "~D" X))
(DEFUN FED-DECNUM-READ (STREAM)
  (LET ((VAL (READLINE STREAM)))
    (OR (ZWEI:PARSE-NUMBER VAL) (FERROR NIL "Bad decimal number: ~A" VAL))))

;;;
;;; FED great frame - contains real FED frame and typeout window so the latter
;;; can deexpose the former and get it back in one fell BITBLT.
;;;
;;;

(DEFFLAVOR FED-GREAT-FRAME (FED)
	   (TV:BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER
	    TV:WINDOW-WITH-TYPEOUT-MIXIN
	    TV:WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS ':DELAYED
    :LABEL NIL
    :TYPEOUT-WINDOW '(FED-MOUSABLE-TYPEOUT-WINDOW
		       :DEEXPOSED-TYPEOUT-ACTION (:EXPOSE-FOR-TYPEOUT)
		       :IO-BUFFER NIL)))

(DEFMETHOD (FED-GREAT-FRAME :BEFORE :INIT) (&REST IGNORE)
  (SETQ TV:SELECTED-PANE 'FED
	TV:PANES         '((FED   FED))
	TV:CONSTRAINTS   '((FED . ((FED)
				   ((FED :EVEN)))))))

(DEFMETHOD (FED-GREAT-FRAME :AFTER :INIT) (&REST IGNORE)
  (SETQ FED (FUNCALL-SELF ':GET-PANE ':FED)))

(DEFMETHOD (FED-GREAT-FRAME :NAME-FOR-SELECTION) ()
  (LET ((FONT (FUNCALL FED ':CURRENT-FONT)))
    (FORMAT NIL "FED~:[~; (~A)~]" FONT (GET-PNAME FONT))))
;;;
;;;  The actual FED frame itself.
;;;

(FUNCALL TV:DEFAULT-SCREEN ':PARSE-FONT-DESCRIPTOR 'FONTS:TR12I)	;load at load time
(DEFFLAVOR FED
       ((CURRENT-FONT NIL)
	TYPEOUT-WINDOW
	PROMPT-WINDOW FED-PANE
	STATUS-PANE
	DRAW-MODE-MENU
	PARAM-CHVV
	REGISTER-PANE
	ALPHABET-MENU CHARACTER-PANE)
       (TV:ANY-TYI-MIXIN TV:STREAM-MIXIN
	TV:PROCESS-MIXIN
	TV:PANE-MIXIN TV:CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER	;look ma no borders
	TV:WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES CURRENT-FONT STATUS-PANE DRAW-MODE-MENU ALPHABET-MENU)
  (:DEFAULT-INIT-PLIST :MENU-COMMAND-ALIST MENU-COMMAND-ALIST :LABEL NIL
		       :SAVE-BITS ':DELAYED :BORDER-MARGIN-WIDTH 0
)

  (:INIT-KEYWORDS :MENU-COMMAND-ALIST)
  (:DOCUMENTATION :COMBINATION "The actual fed window"))

(DEFMETHOD (FED :WHO-LINE-DOCUMENTATION-STRING)() NIL)	;old one will err out in this env

(DEFMETHOD (FED :NAME-FOR-SELECTION) () NIL)	;pass it up, and obsolete old methods



;;;
;;; The constraints and pane specs for the FED frame.
;;;

(DEFMETHOD (FED :BEFORE :INIT) (IGNORE)
  (SETQ TV:PROCESS (MAKE-PROCESS TV:NAME ':SPECIAL-PDL-SIZE 4000 ':REGULAR-PDL-SIZE 10000))
  (SETQ TV:SELECTED-PANE 'FED-PANE
	TV:PANES `((FED-PANE      BASIC-FED-PANE)
		   (PARAM-CHVV    UNSELECTABLE-CHVV-PANE
				  :VARIABLES ,FONT-PARAMETERS-CHVV-LIST
				  :STACK-GROUP ,(FUNCALL TV:PROCESS ':STACK-GROUP)
				  :LABEL (:STRING "Font parameters (decimal)"
						  :FONT FONTS:TR12I))
		   (STATUS-PANE    FED-STATUS-PANE :BLINKER-P NIL :LABEL NIL)
		   (DRAW-MODE-MENU HIGHLIGHTING-COMMAND-MENU-PANE
				   :ITEM-LIST
				   ,DRAW-MODE-MENU-LIST)
		   (ALPHABET-MENU TV:COMMAND-MENU-PANE
				  :ITEM-LIST
				  ,(DO ((I 0 (1+ I)) (L NIL (CONS (STRING I) L)))
				       ((= I 200) (NREVERSE L)))
				  :LABEL (STRING "Select Character to edit"
						 :FONT FONTS:TR12I))
		   (CHARACTER-PANE CHARACTER-PANE :LABEL NIL :BLINKER-P NIL)
		   (REGISTER-PANE  REGISTER-PANE :LABEL (STRING "Registers"
								:FONT FONTS:TR12I))
		   (PROMPT-WINDOW UNSELECTABLE-PANE :MORE-P NIL
				  :LABEL NIL :BLINKER-DESELECTED-VISIBILITY :OFF)
		   
		   (COMMAND-MENU-1  TV:COMMAND-MENU-PANE
				    :ITEM-LIST ,MENU-COMMAND-ALIST-1)
		   (COMMAND-MENU-2  TV:COMMAND-MENU-PANE
				  :ITEM-LIST ,MENU-COMMAND-ALIST-2)
		   (COMMAND-MENU-3  TV:COMMAND-MENU-PANE
				  :ITEM-LIST ,MENU-COMMAND-ALIST-3))
	TV:CONSTRAINTS
	`((:STANDARD
	    . ((CHARACTER-PANE PROMPT-WINDOW TOP-SECTION)
	       ((CHARACTER-PANE :ASK :WANTED-SIZE))
	       ((PROMPT-WINDOW 2 :LINES))
	       ((TOP-SECTION :HORIZONTAL (:EVEN)
			     (FED-PANE OTHER-SLAB)
			     ((OTHER-SLAB :VERTICAL (:LIMIT (20. 100. :CHARACTERS
								 PROMPT-WINDOW)
							    .3)
					  (DRAW-MODE-MENU
					    COMMAND-MENU-1 COMMAND-MENU-2 COMMAND-MENU-3
					    STATUS-PANE ALPHABET-MENU PARAM-CHVV
					    REGISTER-PANE)
					  ((DRAW-MODE-MENU :ASK :PANE-SIZE))
					  ((COMMAND-MENU-1 :ASK :PANE-SIZE))
					  ((COMMAND-MENU-2 :ASK :PANE-SIZE))
					  ((COMMAND-MENU-3 :ASK :PANE-SIZE))
					  ((STATUS-PANE 3 :LINES))
					  ((ALPHABET-MENU :ASK :PANE-SIZE))
					  ((PARAM-CHVV :ASK :PANE-SIZE))
					  ((REGISTER-PANE :EVEN))))
			     ((FED-PANE :EVEN))))))
	  (:WIDE
	    . ((CHARACTER-PANE PROMPT-WINDOW FED-PANE REGISTER-PANE BOTTOM-SLAB)
	       ((CHARACTER-PANE :ASK :WANTED-SIZE))
	       ((REGISTER-PANE :ASK :PANE-SIZE))
	       ((PROMPT-WINDOW 2 :LINES))
	       ((BOTTOM-SLAB :HORIZONTAL (15. :LINES PROMPT-WINDOW)
			     (ALPHABET-MENU MISC COMMAND-MENU-1 COMMAND-MENU-2 COMMAND-MENU-3)
			     ((COMMAND-MENU-1 :ASK :PANE-SIZE))
			     ((COMMAND-MENU-2 :ASK :PANE-SIZE))
			     ((COMMAND-MENU-3 :ASK :PANE-SIZE))
			     ((ALPHABET-MENU :ASK :PANE-SIZE))
			     ((MISC :VERTICAL (30. :CHARACTERS PROMPT-WINDOW)
				    (DRAW-MODE-MENU STATUS-PANE PARAM-CHVV)
				    ((DRAW-MODE-MENU :ASK :PANE-SIZE))
				    ((PARAM-CHVV :ASK :PANE-SIZE))
				    ((STATUS-PANE :EVEN))))))
	       ((FED-PANE :EVEN)))))))


(DEFMETHOD (FED :AFTER :INIT) (IGNORE)
  (PROCESS-PRESET TV:PROCESS 'FED-TOP-LEVEL SELF)
  (SETQ TYPEOUT-WINDOW (FUNCALL TV:SUPERIOR ':TYPEOUT-WINDOW)
	REGISTER-PANE (FUNCALL-SELF ':GET-PANE 'REGISTER-PANE)
	FED-PANE (FUNCALL-SELF ':GET-PANE 'FED-PANE)
	PARAM-CHVV (FUNCALL-SELF ':GET-PANE 'PARAM-CHVV)
	STATUS-PANE (FUNCALL-SELF ':GET-PANE 'STATUS-PANE)
        DRAW-MODE-MENU (FUNCALL-SELF ':GET-PANE 'DRAW-MODE-MENU)
	ALPHABET-MENU (FUNCALL-SELF ':GET-PANE 'ALPHABET-MENU)
	CHARACTER-PANE (FUNCALL-SELF ':GET-PANE 'CHARACTER-PANE)
	PROMPT-WINDOW (FUNCALL-SELF ':GET-PANE 'PROMPT-WINDOW))
  (FUNCALL CHARACTER-PANE ':SET-EVAL-PANE FED-PANE)	;crocque. see line 1.
  (FUNCALL TYPEOUT-WINDOW ':SET-IO-BUFFER TV:IO-BUFFER))

(DEFMETHOD (FED :AFTER :EXPOSE) (&REST IGNORE))	;nuke out old one
  
;;;
;;;  FED frame redistributors of impulses
;;;

(DEFMETHOD (FED :SPECIFY-FONT) (FONT)
  (SETQ CURRENT-CHARACTER NIL CURRENT-FONT FONT)
  (FUNCALL-SELF ':UPDATE-BASE-CONFIG))

(DEFMETHOD (FED :UPDATE-BASE-CONFIG) ()
  (IF (FUNCALL TYPEOUT-WINDOW ':INCOMPLETE-P)	;this seems to lose bad, selects prompt-wdw
      (FUNCALL-SELF ':ABOLISH-TYPEOUT-WINDOW))
  (FUNCALL CHARACTER-PANE ':SET-FONT-MAP (LIST CURRENT-FONT))
  (FUNCALL CHARACTER-PANE ':SET-VSP 0)
  (FUNCALL REGISTER-PANE ':SET-FONT-MAP (LIST CURRENT-FONT))
  (FUNCALL REGISTER-PANE ':SET-VSP 0)
  (MULTIPLE-VALUE-BIND (IGNORE HEIGHT)
      (FUNCALL CHARACTER-PANE ':INSIDE-SIZE)
    (LET ((CHAR-HEIGHT (FUNCALL CHARACTER-PANE ':LINE-HEIGHT)))
      (COND ((AND (< CHAR-HEIGHT HEIGHT)
		  (NOT (> HEIGHT (* 2 CHAR-HEIGHT)))))
	    (T (FUNCALL-SELF ':SET-CONFIGURATION
			(FUNCALL-SELF ':CONFIGURATION))))))
  (FUNCALL-SELF ':SELECT-PANE FED-PANE)		;should not be necessary. window sys screw?
  (IF CURRENT-CHARACTER (FUNCALL CHARACTER-PANE ':SET-CHAR CURRENT-CHARACTER))
  (FUNCALL REGISTER-PANE ':SET-FONT CURRENT-FONT)
  (FUNCALL STATUS-PANE ':REDISPLAY)
  (FUNCALL FED-PANE ':SPECIFY-FONT CURRENT-FONT)
  (FUNCALL PARAM-CHVV ':REFRESH))


(DEFMETHOD (FED :UPDATE-DRAW-MODE-PANE) (&OPTIONAL TMODE)
  (FUNCALL DRAW-MODE-MENU
	   ':SET-HIGHLIGHTED-VALUES
	   (LET ((X
		   (CDR (ASSQ (OR TMODE (FUNCALL FED-PANE ':DRAW-MODE))
			      '((2 . COM-SET-ANDCA-DRAW-MODE)
				(6 . COM-SET-XOR-DRAW-MODE)
				(7 . COM-SET-SETA-DRAW-MODE))))))
	   (AND X (LIST X)))))


(DEFMETHOD (FED :AFTER :REFRESH) (&REST IGNORE)
  (FUNCALL-SELF ':UPDATE-DRAW-MODE-PANE))

(DEFMETHOD (FED :UPDATE-STATUS-PANE) ()
  (FUNCALL STATUS-PANE ':REDISPLAY))

(DEFMETHOD (FED :CLEAR-CHAR-PANE) ()
  (AND (BOUNDP 'CHARACTER-PANE)
       (TV:SHEET-FORCE-ACCESS (CHARACTER-PANE)
	 (FUNCALL CHARACTER-PANE ':CLEAR-SCREEN))))

(DEFMETHOD (FED :REDISPLAY-CHAR-PANE) ()
  (FUNCALL CHARACTER-PANE ':REDISPLAY))

(DEFMETHOD (FED :GOBBLE-CHARACTER) (CHAR)
  (FUNCALL CHARACTER-PANE ':SET-CHAR CHAR))

(DEFMETHOD (FED :SET-SAMPLE-STRING) (S)
  (FUNCALL CHARACTER-PANE ':SET-SAMPLE-STRING S))

;;;
;;;  The COMMAND LOOP!!
;;;

(DEFVAR *CURFONT-CHAR-HEIGHT* 12.)		;the MIRACLE of choose-variable-values
(DEFVAR *CURFONT-BLINKER-HEIGHT* 12.)
(DEFVAR *CURFONT-BLINKER-WIDTH* 7.)
(DEFVAR *CURFONT-BASELINE* 9.)

(DEFVAR NUMERIC-ARG)				;The numeric argument to a command
(DEFVAR NUMERIC-ARG-P)
(DEFVAR COMMAND-CHAR)				;The character that invoked this command

(DEFUN FED-TOP-LEVEL (WINDOW)
  (OR COMMAND-TABLE (SETUP-COMMAND-TABLE))
  (LET((*CURFONT-CHAR-HEIGHT* *CURFONT-CHAR-HEIGHT*)	;eat "default box"
       (*CURFONT-BASELINE* *CURFONT-BASELINE*)
       (*CURFONT-BLINKER-WIDTH* *CURFONT-BLINKER-WIDTH*)
       (*CURFONT-BLINKER-HEIGHT* *CURFONT-BLINKER-HEIGHT*))
    (FUNCALL (FUNCALL WINDOW ':GET-PANE 'PARAM-CHVV) ':SETUP FONT-PARAMETERS-CHVV-LIST
	     '(:STRING "Font parameters (decimal)"  :FONT FONTS:TR12I) NIL NIL)	;get sg right
    (DO ()
	(())
      (*CATCH 'SYS:COMMAND-LEVEL
	(FUNCALL WINDOW ':COMMAND-LOOP)))))




(DEFMETHOD (FED :COMMAND-LOOP) (&AUX (TERMINAL-IO TYPEOUT-WINDOW)
				     (PROMPT-LINE-USED NIL)
				     (PROMPT-LINE-WAS-USED T)
				     COMMAND-CHAR COMMAND BLIP
				     NUMERIC-ARG NUMERIC-ARG-P)
  (PROG ()					;need tags
	;; Entry from startup, or ABORTing.  Bury the typeout window/non-pane,
	
	(IF (FUNCALL TYPEOUT-WINDOW ':INCOMPLETE-P)
	    (FUNCALL-SELF ':ABOLISH-TYPEOUT-WINDOW))
	
	;; Top of command loop.  Redisplay the FED-PANE.
	
     TOP
	(FUNCALL FED-PANE ':REDISPLAY)
	(AND PROMPT-LINE-WAS-USED (NOT PROMPT-LINE-USED)
	     (FUNCALL PROMPT-WINDOW ':CLEAR-SCREEN))	;Make sure the prompt window is clear
	(SETQ PROMPT-LINE-WAS-USED PROMPT-LINE-USED
	      PROMPT-LINE-USED NIL)
	(SETQ NUMERIC-ARG 1 NUMERIC-ARG-P NIL)
	
	;; Collect the char.
	
     ARG
	(*CATCH 'BARF
	  (LET ((WASX (FUNCALL TYPEOUT-WINDOW ':INCOMPLETE-P)))
	    (SETQ COMMAND-CHAR (FUNCALL-SELF ':ANY-TYI))
	    (IF (AND (NOT WASX)			;expose during tyi common case
		     (FUNCALL TYPEOUT-WINDOW ':INCOMPLETE-P))
		(FUNCALL-SELF ':ABOLISH-TYPEOUT-WINDOW)))
	  
	  (IF (LISTP COMMAND-CHAR)
	      
	      ;; Handle n kinds of mousing, menu selections, etc.  Map into
	      ;; simpler, older command type if appropriate.
	      
	      (SELECTQ (FIRST (SETQ BLIP COMMAND-CHAR))
		(:TYPEOUT-EXECUTE (SELECTQ (SECOND BLIP)
				    (:SELECT-CHAR
				     (FUNCALL FED-PANE ':FED-PANE-FUNCALL #'GOBBLE-CHARACTER
					      CURRENT-FONT (THIRD BLIP)))
				    (:SELECT-FONT
				     (FUNCALL FED-PANE ':FED-PANE-FUNCALL #'COM-SPECIFY-FONT
					      (THIRD BLIP)))
				    ((:DEFAULT-FETCH-REGISTER
				       :REG-STORE-GRAY
				       :REG-STORE-BLACK
				       :REG-FETCH-GRAY
				       :REG-FETCH-BLACK)
				     (LET ((X (FUNCALL REGISTER-PANE
						       ':INTERPRET-MOUSING BLIP)))
				       (AND X (FUNCALL FED-PANE ':FED-PANE-APPLY
						       #'COM-REGISTER-MOUSE X))))))
		(:VARIABLE-CHOICE (IF (NULL CURRENT-FONT) (BARPH "No current font"))
				  (TV:CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE	;throws
				    (SECOND BLIP) BLIP)
				  (IF (EQ (SECOND BLIP) PARAM-CHVV)
				      (FUNCALL FED-PANE ':FED-PANE-FUNCALL
					       #'UPDATE-FONT-PARAMETERS-I)))
		(:MOUSE-BUTTON    (SETQ COMMAND-CHAR (SECOND BLIP))
				  (COND ((EQ (THIRD BLIP) CHARACTER-PANE)
					 (FUNCALL FED-PANE ':FED-PANE-FUNCALL
						  #'COM-SET-SAMPLE))
					(T (GO ICHAR))))
		(:MENU            (IF (EQ (FOURTH BLIP) ALPHABET-MENU)
				      (FUNCALL FED-PANE ':FED-PANE-FUNCALL
					       #'GOBBLE-CHARACTER
					       CURRENT-FONT
					       (CHARACTER (SECOND BLIP)))
				      (GO ICHAR)))
		(:FED-BLIP	 (GO CL-END))	;currently, only to force redisplay
		(:FED-COM         (FUNCALL FED-PANE ':FED-PANE-APPLY (SECOND BLIP)
					   (CDDR BLIP))))
	      (GO ICHAR)))
	(GO CL-END)
	
	;; Interpret a basic command, being either a command-menu item or
	;; a kbd char.  Maybe output from above.
	
     ICHAR
	(*CATCH 'BARF				;BARPH throws to this tag.
	  (SETQ COMMAND (COMMAND-LOOKUP COMMAND-CHAR))
	  (COND ((EQ COMMAND 'COM-NUMBER)
		 (IF (NULL NUMERIC-ARG-P) (PROMPT-LINE ""))	;clear area
		 (SETQ NUMERIC-ARG (+ (IF NUMERIC-ARG-P (* NUMERIC-ARG 10.) 0)
				      (- COMMAND-CHAR #/0))
		       NUMERIC-ARG-P T)
		 (PROMPT-LINE-TYO COMMAND-CHAR)
		 (SETQ COMMAND-CHAR NIL)
		 (GO ARG))
		((AND (LISTP COMMAND) (EQ (CAR COMMAND) ':BARF))
		 (BARPH (SECOND COMMAND)))
		(COMMAND
		 (IF (LISTP COMMAND-CHAR)
		     (SETQ COMMAND-CHAR (KEY-ANALYZER)))
		 (IF (GET COMMAND 'MOUSE-COMMAND)
		     (FUNCALL FED-PANE ':FED-PANE-FUNCALL COMMAND BLIP)
		     (FUNCALL FED-PANE ':FED-PANE-FUNCALL COMMAND)))
		(T
		 (BARF "~C is not a defined command" COMMAND-CHAR))))
	
	;; End of loop. Bury the typeout window of it's there.
	
     CL-END
	(COND ((FUNCALL TYPEOUT-WINDOW ':INCOMPLETE-P)
	       (DO () ((NOT (FUNCALL-SELF ':LISTEN)))
		 (LET ((NNCH (FUNCALL-SELF ':ANY-TYI)))
		   (IF (AND (LISTP NNCH)
			    (EQ (CAR NNCH) ':FED-BLIP))
		       NIL
		       (PROGN (FUNCALL-SELF ':UNTYI NNCH)
			      (RETURN)))))
	       (LET ((NEXTCH (FUNCALL TYPEOUT-WINDOW ':ANY-TYI)))
		 ;; Prevent the window system from restoring an old char we don't
		 ;; want to see if we just selected one and the fed frame has
		 ;; Save-bits.  This is colloquially known as a "kludge"
		 ;; (< Ger., "Klug", clever?)
		 
		 (IF (AND (LISTP NEXTCH)
			  (EQ (FIRST NEXTCH) ':TYPEOUT-EXECUTE))
		     (TV:SHEET-FORCE-ACCESS (FED-PANE)	;EXACTLY the right thing
		       (FUNCALL FED-PANE ':CLEAR-SCREEN)
		       (FUNCALL FED-PANE ':MUST-REDISPLAY REDISPLAY-ALL)))
		 
		 (FUNCALL-SELF ':ABOLISH-TYPEOUT-WINDOW)
		 (OR (EQ NEXTCH #\SP)		;get rid of space so next rdis sees no tpahd
		     (FUNCALL-SELF ':UNTYI NEXTCH))
		 (GO TOP))))
	(GO TOP)))


(DEFMETHOD (FED :ABOLISH-TYPEOUT-WINDOW) ()
  (TV:DELAYING-SCREEN-MANAGEMENT
    (FUNCALL TYPEOUT-WINDOW ':MAKE-COMPLETE)
    (FUNCALL-SELF ':SELECT)))

(COMPILE-FLAVOR-METHODS FED)

(DEFUN KEY-ANALYZER ()
  (LOGIOR
    (IF (TV:KEY-STATE ':META) (DPB 1 %%KBD-META 0) 0)
    (IF (TV:KEY-STATE ':CONTROL) (DPB 1 %%KBD-CONTROL 0) 0)
    (IF (TV:KEY-STATE ':SUPER) (DPB 1 %%KBD-SUPER 0) 0)
    (IF (TV:KEY-STATE ':HYPER) (DPB 1 %%KBD-HYPER 0) 0)))

(DEFUN BARF (&OPTIONAL STRING &REST FORMAT-ARGS)
  (TV:BEEP)
  (AND STRING (LEXPR-FUNCALL #'PROMPT-LINE STRING FORMAT-ARGS)))

(DEFUN BARPH (&REST ARGS)
  (APPLY #'BARF ARGS)
  (*THROW 'BARF 'BARF))

(DEFVAR MOUSE-COMMAND-TABLE)
(SETQ COMMAND-TABLE NIL)			;cause regenerate when loaded

(DEFUN COMMAND-LOOKUP (CHAR)
  (COND ((LISTP CHAR)
	 (SELECTQ (CAR CHAR)
           (:BARF CHAR)
	   (:MENU (CADDR (CADR CHAR)))))
	((LDB-TEST %%KBD-MOUSE CHAR)
	 (LET ((NCLICKS (LDB %%KBD-MOUSE-N-CLICKS CHAR)))
	   (IF (> NCLICKS 2)
	       (PROGN
		 (TV:BEEP)
		 NIL)
	       
	       (AREF MOUSE-COMMAND-TABLE (LDB %%KBD-MOUSE-BUTTON CHAR) NCLICKS))))
	(T
	 (SETQ CHAR (LDB %%CH-CHAR CHAR))
	 (AND (< CHAR (ARRAY-DIMENSION-N 1 COMMAND-TABLE))
	      (DO () (NIL)
	       (OR (NUMBERP (SETQ CHAR (AREF COMMAND-TABLE CHAR))) (RETURN CHAR)))))))

(DEFUN SETUP-COMMAND-TABLE (&AUX (TBLMAX 0))
  (SETQ MOUSE-COMMAND-TABLE (MAKE-ARRAY NIL 'ART-Q '(3 3)))
  (TV:DOPLIST (COMMAND-LIST COMMAND CHAR)
    (COND ((LISTP CHAR)
	   (DOLIST (CHAR CHAR) (SETQ TBLMAX (MAX CHAR TBLMAX))))
	  ((LDB-TEST %%KBD-MOUSE CHAR))
	  (T
	   (SETQ TBLMAX (MAX CHAR TBLMAX)))))
  (SETQ COMMAND-TABLE (MAKE-ARRAY (1+ TBLMAX) ':TYPE ':ART-Q))
  (TV:DOPLIST (COMMAND-LIST COMMAND CHAR)
    (COND ((LISTP CHAR)
	   (DOLIST (CHAR CHAR) (ASET COMMAND COMMAND-TABLE CHAR)))
	  ((LDB-TEST %%KBD-MOUSE CHAR)
	   (ASET COMMAND MOUSE-COMMAND-TABLE (LDB %%KBD-MOUSE-BUTTON CHAR)
		 (LDB %%KBD-MOUSE-N-CLICKS CHAR)))
	  (T
	   (ASET COMMAND COMMAND-TABLE CHAR))))
  (DO CHAR #/0 (1+ CHAR) (> CHAR #/9)
    (ASET 'COM-NUMBER COMMAND-TABLE CHAR))
  (DO CHAR #/a (1+ CHAR) (> CHAR #/z)
    (ASET (- CHAR 40) COMMAND-TABLE CHAR)))

;;;
;;; The commands and their related routines
;;; They all expect to run in the BASIC-FED.
;;;

(DEFUN COM-HELP ()
  (PRINC FED-HELP-STRING))


(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED)
(DEFUN COM-SET-SAMPLE ()
  (IF (NULL CURRENT-FONT) (BARPH "No current font."))
  (LET ((SAMPLE-STRING (PROMPT-LINE-READLINE "String to display in ~A: " CURRENT-FONT)))
    (FUNCALL TV:SUPERIOR ':SET-SAMPLE-STRING (IF (ZEROP (STRING-LENGTH SAMPLE-STRING))
						 NIL
						 SAMPLE-STRING)))))


;Complement the square which the not-mouse is on.
(DEFUN COM-COMPLEMENT-SQUARE (&AUX OLD-INTEN X Y)
  (IF (NOT CURSOR-ON) (BARPH "No non-mouse cursor set."))
  (SETQ X (+ WINDOW-X-POS CURSOR-X) Y (+ WINDOW-Y-POS CURSOR-Y))
  (SETQ OLD-INTEN (PLANE-AREF PLANE X Y))
  (PLANE-ASET (IF (ZEROP OLD-INTEN) 1 0) PLANE X Y)
  (FUNCALL-SELF ':MUST-REDISPLAY REDISPLAY-ONE CURSOR-X CURSOR-Y))

(DEFUN COM-ERASE-ALL ()
  (AND (OR (NOT (NUMBERP COMMAND-CHAR))
	   (PROMPT-LINE-Y-OR-N-P "Erase all these dots? "))
       (FUNCALL-SELF ':ERASE-ALL)))

(DEFUN COM-HOME ()
  (FUNCALL-SELF ':HOME-BOX))


;Set the position of the cursor, which is used as an alternate to the mouse
;for complementing squares.  Also say that the cursor ought to be displayed.
(DEFUN COM-SET-X (&OPTIONAL (XPOS NUMERIC-ARG))
  (COND ((OR (< XPOS 0) ( XPOS WINDOW-X-SIZE))
	 (BARF "X out of range: ~D" XPOS)))
  (SETQ CURSOR-X (MAX 0 (MIN (1- WINDOW-X-SIZE) XPOS))))

(DEFUN COM-SET-Y (&OPTIONAL (YPOS NUMERIC-ARG))
  (COND ((OR (< YPOS 0) ( YPOS WINDOW-Y-SIZE))
	 (BARF "Y out of range: ~D" YPOS)))
  (SETQ CURSOR-Y (MAX 0 (MIN (1- WINDOW-Y-SIZE) YPOS))))

(DEFUN COM-SHIFT-CURSOR (&AUX (DISTANCE NUMERIC-ARG) DX DY ARROW)
  (OR NUMERIC-ARG-P (SETQ DISTANCE (LSH 1 (LDB %%KBD-CONTROL-META COMMAND-CHAR))))
  (SETQ ARROW (LDB %%KBD-CHAR COMMAND-CHAR))
  (SETQ DX (* DISTANCE (OR (CADR (ASSQ ARROW '((#/[ -1) (#/] 1)
					       (#\HAND-LEFT -1) (#\HAND-RIGHT 1)))) 0)))
  (SETQ DY (* DISTANCE (OR (CADR (ASSQ ARROW '((#/\ -1) (#// 1)
					       (#\HAND-UP -1) (#\HAND-DOWN 1)))) 0)))
  (COM-SET-X (+ CURSOR-X DX))
  (COM-SET-Y (+ CURSOR-Y DY))
  (SETQ CURSOR-ON T))

(DEFUN COM-SHIFT-WINDOW (&AUX DISTANCE DX DY ARROW)
  (SETQ DISTANCE (IF NUMERIC-ARG-P NUMERIC-ARG (LSH 1 (LDB %%KBD-CONTROL-META COMMAND-CHAR))))
  (SETQ ARROW (LDB %%KBD-CHAR COMMAND-CHAR))
  (SETQ DX (* DISTANCE (OR (CADR (ASSQ ARROW '((#/ 1) (#/ -1)))) 0)))
  (SETQ DY (* DISTANCE (OR (CADR (ASSQ ARROW '((#/ 1) (#/ -1)))) 0)))
  (FUNCALL-SELF ':SET-OFFSET (+ WINDOW-X-POS DX) (+ WINDOW-Y-POS DY)))

(DEFUN COM-MOUSE-SHIFT-WINDOW (&AUX OX OY X Y)
  (PROG ()
	(PROMPT-LINE "Select point, and another point to move it to, with left button")
	(SETF (VALUES OX OY) (FUNCALL-SELF ':MOUSE-SELECT-POINT))
	(OR OX (RETURN (BARF "Aborted")))
	(FUNCALL-SELF ':GRAY-POINT OX OY)
	(SETF (VALUES X Y) (FUNCALL-SELF ':MOUSE-SELECT-POINT))
	(FUNCALL-SELF ':GRAY-POINT OX OY)
	(OR X (RETURN (BARF "Aborted")))
	(FUNCALL-SELF ':SET-OFFSET
		      (- WINDOW-X-POS (- X OX))
		      (- WINDOW-Y-POS (- Y OY)))
	(FUNCALL PROMPT-WINDOW ':CLEAR-SCREEN)))

;Set the box-size (in both X and Y) of the fed window to SCALE.
;We try to keep the center of the window in the center.  [Sure we do] Yes, we do.

(DEFUN COM-SCALE ()				;old kbd command
  (SET-SCALE (IF NUMERIC-ARG-P NUMERIC-ARG 14)))

(DEFUN COM-SET-GRID-SIZE ()			;menu xface
  (LET ((Z (STRING-TRIM " " (PROMPT-LINE-DEFAULTED-READLINE
			      "Pixel size of box, default ~D., max 64.: " DEFAULT-BOX-SIZE))))
    (LET ((N (PARSE-NUMBER Z)))
      (IF N
	  (SET-SCALE N)
	(BARPH "Invalid number: ~A" Z)))))

(DEFUN SET-SCALE (SCALE)
    (COND (( SCALE 64.)			;bitblt kludge
	   (BARF "Scale must be under 64."))
	  ((AND (> SCALE 0)
		(< SCALE (// (TV:SHEET-INSIDE-WIDTH SELF) 2))
		(< SCALE (// (TV:SHEET-INSIDE-HEIGHT SELF) 2)))
	   (SETQ BOX-X-SIZE SCALE BOX-Y-SIZE SCALE
		 REDISPLAY-DEGREE REDISPLAY-ALL)
	   (FUNCALL-SELF ':CHANGE-OF-SIZE-OR-MARGINS)
	   (FUNCALL-SELF ':HOME-BOX))
	  ((BARF "Bad scale: ~D" SCALE))))

(DEFVAR *FONT-PATHNAME-TEMPLATE*
	(FS:PARSE-PATHNAME "SYS: FONTS; FOO QFASL >"))

(DEFUN FONT-PATHNAME-TEMPLATE ()
  (FUNCALL *FONT-PATHNAME-TEMPLATE* ':TRANSLATED-PATHNAME))

;Read the name of a font and select it.
(DEFUN COM-SPECIFY-FONT (&OPTIONAL FONT-NAME &AUX (NEW NIL))
  (COND ((TYPEP FONT-NAME 'FONT-DESCRIPTOR) (SETQ FONT-NAME (FD-NAME FONT-NAME)))
	((TYPEP FONT-NAME 'FONT) (SETQ FONT-NAME (FONT-NAME FONT-NAME)))
	((SYMBOLP FONT-NAME))
	((STRINGP FONT-NAME))
	(T (BARPH "Invalid font spec: ~S" FONT-NAME)))
  (LET ((FONT-NAME (STRING-UPCASE
		     (IF FONT-NAME
		       (STRING FONT-NAME)
		       (STRING-TRIM " " (PROMPT-LINE-READLINE "Font Name: "))))))
    (IF (ZEROP (STRING-LENGTH FONT-NAME))
	(BARPH))
    (LET* ((FONT-SYM (INTERN FONT-NAME "FONTS"))
	   (FONT-PATH (FUNCALL (FONT-PATHNAME-TEMPLATE) ':NEW-NAME (STRING FONT-SYM))))
      (COND ((BOUNDP FONT-SYM))			;Font already exists
	    ((AND (PROBEF FONT-PATH)
		  (PROMPT-LINE-Y-OR-N-P "Load ~A? " FONT-PATH))
	     (LOAD FONT-PATH "FONTS"))
	    (T
	     (IF (PROMPT-LINE-Y-OR-N-P "~A does not exist, create it? " FONT-SYM)
		 (SETQ NEW (SET FONT-SYM (FONT-DESCRIPTOR-INTO-FONT (FONT-GET-FD FONT-SYM))))
		 (BARPH))))
      (SELECT-FONT FONT-SYM NEW))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN SELECT-FONT (FONT-SYM &OPTIONAL NEW &AUX FD PLIST)
  (COND ((AND FONT-SYM (BOUNDP FONT-SYM))
	 (SETQ FD (FONT-GET-FD FONT-SYM))
	 (SETQ *CURFONT-CHAR-HEIGHT* (FD-LINE-SPACING FD)
	       *CURFONT-BASELINE* (FD-BASELINE FD)
	       *CURFONT-BLINKER-HEIGHT* (FD-BLINKER-HEIGHT FD)
	       *CURFONT-BLINKER-WIDTH* (FD-BLINKER-WIDTH FD))
	 (FUNCALL TV:SUPERIOR ':SPECIFY-FONT FONT-SYM)
	 (FUNCALL-SELF ':REDEFINE-MARGINS (LOCF PLIST))
	 (OR NEW (COM-DISPLAY-FONT)) 
	 (FUNCALL-SELF ':ERASE-ALL)
	 (FUNCALL-SELF ':HOME-BOX)
	 (SETQ REDISPLAY-DEGREE REDISPLAY-ALL)))))



(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN UPDATE-FONT-PARAMETERS-I (&AUX FONT FD)
  (IF (NULL CURRENT-FONT)
      (BARF "No font specified yet")
      (PROGN
	(SETQ FONT (SYMEVAL CURRENT-FONT) FD (FONT-GET-FD CURRENT-FONT))
	(LET ((OLD-BASELINE (FONT-BASELINE FONT))
	      (OLD-CHAR-HEIGHT (FONT-CHAR-HEIGHT FD)))
	  (SETF (FONT-CHAR-HEIGHT FONT) *CURFONT-CHAR-HEIGHT*)
	  (SETF (FD-LINE-SPACING FD) *CURFONT-CHAR-HEIGHT*)
	  (SETF (FONT-BASELINE FONT) *CURFONT-BASELINE*)
	  (SETF (FD-BASELINE FD) *CURFONT-BASELINE*)
	  (SETF (FONT-BLINKER-HEIGHT FONT) *CURFONT-BLINKER-HEIGHT*)
	  (SETF (FD-BLINKER-HEIGHT FD) *CURFONT-BLINKER-HEIGHT*)
	  (SETF (FONT-BLINKER-WIDTH FONT) *CURFONT-BLINKER-WIDTH*)
	  (SETF (FD-BLINKER-WIDTH FD) *CURFONT-BLINKER-WIDTH*)
	  (SETQ CHAR-BOX-Y1 (- CHAR-BOX-Y2 (FD-BASELINE FD))
		CHAR-BOX-Y3 (+ CHAR-BOX-Y1 (FD-LINE-SPACING FD)))
	  (LET (FOO) (FUNCALL-SELF ':REDEFINE-MARGINS (LOCF FOO)))
	  (SETQ REDISPLAY-DEGREE REDISPLAY-ALL)
	  (IF (NOT (AND (= *CURFONT-BASELINE* OLD-BASELINE)
			(= *CURFONT-CHAR-HEIGHT* OLD-CHAR-HEIGHT)))
	      (FUNCALL TV:SUPERIOR ':UPDATE-BASE-CONFIG)))))))

(DEFUN READ-DEFAULTED-FONT-PARAMETER (NAME CURRENT-VALUE &OPTIONAL (STREAM T))
  (FORMAT STREAM "~&Font ~A (now ~D) = " NAME CURRENT-VALUE)
  (LET ((TEM (READLINE STREAM)))
    (AND (NOT (ZEROP (STRING-LENGTH TEM)))
	 (NUMBERP (SETQ TEM (READ-FROM-STRING TEM '(()))))
	 TEM)))

;;; Vestige interfaces for keyboard commands

(DEFUN COM-READ-FILE ()
  (FED-READ-FILE (KBD-SELECT-FILE-TYPE)))

(DEFUN COM-WRITE-FILE ()
  (OR CURRENT-FONT (BARPH "No current font"))
  (FED-WRITE-FILE (KBD-SELECT-FILE-TYPE)));

(DEFUN KBD-SELECT-FILE-TYPE ()
  (COND ((AND (LDB-TEST %%KBD-CONTROL COMMAND-CHAR)
	      (LDB-TEST %%KBD-META COMMAND-CHAR))
	 ':AL)
	((LDB-TEST %%KBD-META COMMAND-CHAR) ':AC)
	((LDB-TEST %%KBD-CONTROL COMMAND-CHAR) ':QFASL)
	(T ':KST)))

;;; Menu-driven commands

(DEFUN COM-READ-QFASL ()
  (FED-READ-FILE ':QFASL))

(DEFUN COM-WRITE-QFASL ()
  (FED-WRITE-FILE ':QFASL))

(DEFUN COM-READ-ANY-FILE ()
  (LET ((F (TV:MENU-CHOOSE '(:QFASL :KST :AC :AL) "File type to read" '(:MOUSE) ':KST)))
    (AND F (FED-READ-FILE F))))			;punt menu-offing

(DEFUN COM-WRITE-ANY-FILE ()
  (OR CURRENT-FONT (BARPH "No current font."))
  (LET ((F (TV:MENU-CHOOSE '(:QFASL :KST :AC) "File type to write" '(:MOUSE) ':KST)))
    (AND F (FED-WRITE-FILE F))))

(DEFUN FED-READ-FILE (TYPE)
  (LET* ((FILENAME (READ-DEFAULTED-FILENAME CURRENT-FONT "Read" TYPE))
	 (DEFAULT (STRING-UPCASE (FUNCALL FILENAME ':NAME)))
	 (FONT (INTERN (IF (MEMQ TYPE '(:KST :QFASL)) DEFAULT
			   (STRING-UPCASE (STRING-TRIM " " (PROMPT-LINE-DEFAULTED-READLINE
							     "Font name (default ~A): "
							     DEFAULT))))
		       "FONTS")))
    (SELECTQ TYPE
      (:KST
       (LET ((FD (READ-KST-INTO-FONT-DESCRIPTOR FILENAME FONT)))
	 (PUTPROP FONT FILENAME 'KST-FILE)
	 (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONT FD)))
      (:QFASL
       (LOAD FILENAME))
      (:AC
       (READ-AC-INTO-FONT FILENAME FONT))
      (:AL
       (READ-AL-INTO-FONT FILENAME FONT)))
    (COND ((AND (BOUNDP FONT) (SYMEVAL FONT))
	   (SELECT-FONT FONT)
	   (LET ((FOO NIL)) (FUNCALL-SELF ':REDEFINE-MARGINS (LOCF FOO)))))))

(DEFUN FED-WRITE-FILE (TYPE)
  (LET ((FILENAME (READ-DEFAULTED-FILENAME CURRENT-FONT "Write" (STRING TYPE))))
    (SELECTQ TYPE
      (:KST
       (WRITE-FONT-INTO-KST CURRENT-FONT FILENAME)
       (PUTPROP CURRENT-FONT FILENAME 'KST-FILE))
      (:QFASL
       (COMPILER:FASD-SYMBOL-VALUE FILENAME CURRENT-FONT))
      (:AC
       (WRITE-AC-FONT FILENAME CURRENT-FONT)))))



;Returns a filename object which is the user's typein merged with the default
;for the font.

(DEFVAR PATHNAME-DEFAULTS NIL)

(DEFUN PATHNAME-DEFAULTS ()
  ;; First time, make the defaults with default directory LMFONT.
  (IF (NULL PATHNAME-DEFAULTS)
      (PROGN
	(SETQ PATHNAME-DEFAULTS (FS:MAKE-PATHNAME-DEFAULTS))
	(FS:SET-DEFAULT-PATHNAME (FONT-PATHNAME-TEMPLATE) PATHNAME-DEFAULTS)))
  PATHNAME-DEFAULTS)

(DEFUN READ-DEFAULTED-FILENAME (FONT OPERATION TYPE &AUX TEM TEM1 SPEC)
  (SETQ TEM (FS:MAKE-PATHNAME ':DEFAULTS (PATHNAME-DEFAULTS)
			      ':NAME (STRING FONT)
			      ':TYPE TYPE))
  (IF (AND (STRING-EQUAL TYPE "KST") (SETQ TEM1 (GET FONT 'KST-FILE)))
      (SETQ TEM (FS:MERGE-PATHNAME-DEFAULTS TEM1 TEM))
      (SETQ TEM (FS:MERGE-PATHNAME-DEFAULTS TEM PATHNAME-DEFAULTS)))
  (SETQ SPEC (PROMPT-LINE-READLINE "~A ~A file: (default ~A) " OPERATION TYPE TEM))
  (SETQ TEM (FS:MERGE-PATHNAME-DEFAULTS SPEC TEM TYPE))
  (FS:SET-DEFAULT-PATHNAME TEM PATHNAME-DEFAULTS)
  TEM)

;C => Read the name of a character and select it in the current font.
;C-C => Read name of character and select it, keeping data in fed-buffer
;instead of gobbling the current definition of the new character.
;Typing a control or mouse character as the arg to the C command aborts it.

;;;Auld crufte

(DEFUN COM-SPECIFY-CHARACTER ()
  (IF (LDB-TEST %%KBD-CONTROL COMMAND-CHAR)	;char merge
      (COM-RENAME-CHARACTER)
      (COM-SELECT-CHARACTER)))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN COM-RENAME-CHARACTER ()
  (IF (NULL CURRENT-FONT) (BARPH "No current font."))
  (LET ((CH (IF NUMERIC-ARG-P NUMERIC-ARG (PROMPT-CHARACTER
					    "Character to call this pattern"))))
    (IF (> CH 400)(BARPH "Bad character"))
    (SETQ CURRENT-CHARACTER CH)
    (FUNCALL TV:SUPERIOR ':GOBBLE-CHARACTER CH)
    (FUNCALL TV:SUPERIOR ':REDISPLAY-CHAR-PANE)
    (FUNCALL TV:SUPERIOR ':UPDATE-STATUS-PANE))))

(DEFUN COM-SELECT-CHARACTER ()
  (LET ((CH (IF NUMERIC-ARG-P NUMERIC-ARG (PROMPT-CHARACTER "Character to Edit"))))
    (IF (> CH 400)(BARPH "Bad character"))
    (GOBBLE-CHARACTER CURRENT-FONT CH)))

;Copy the data from character CHAR in font FONT
;into the fed window to be edited.
(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED)
(DEFUN GOBBLE-CHARACTER (FONT CHAR &AUX FD CD)
  (IF (NULL FONT) (BARPH "No current font"))
  (SETQ CURRENT-CHARACTER CHAR)			;in case called from other than above
  (FUNCALL TV:SUPERIOR ':UPDATE-STATUS-PANE)
  ;; If we have no FD format array for this font, make one.
  
  (SETQ FD (FONT-GET-FD FONT))
  ;; Get the character descriptor for the desired character out of the FD.
  (FUNCALL TV:SUPERIOR ':GOBBLE-CHARACTER CHAR)	;will improve to do all this and dist.
  (IF (AND (< CHAR (ARRAY-LENGTH FD))
	   (SETQ CD (AREF FD CHAR)))
      (PROGN
	;; Put sides of character frame at right place, according to char width and left kern.
	(SETQ CHAR-BOX-X1 (CD-CHAR-LEFT-KERN CD)
	      CHAR-BOX-X2 (+ (FIXR (CD-CHAR-WIDTH CD)) (CD-CHAR-LEFT-KERN CD)))
	;; Put top of character at top of font line, and bottom at baseline
	;; so that descenders go below the "bottom".
	(SETQ CHAR-BOX-Y1 0
	      CHAR-BOX-Y2 (FD-BASELINE FD)
	      CHAR-BOX-Y3 (FD-LINE-SPACING FD))
	;; Now XWIDTH and YWIDTH get the size of the character's raster,
	;; and copy the data into the plane in CHARACTER-ARRAY.
	(LET ((XWIDTH (SECOND (ARRAY-DIMENSIONS CD)))
	      (YWIDTH (FIRST (ARRAY-DIMENSIONS CD))))
	(FUNCALL-SELF ':NEW-PLANE `((0 0) (,XWIDTH ,YWIDTH)))
	  (DO I 0 (1+ I) (= I XWIDTH)
	      (DO J 0 (1+ J) (= J YWIDTH)
		  ;; offset ought be 0, use PLAIN aset, not PLANE-ASET.
		  (ASET (AREF CD J I) PLANE I J))))
	(FUNCALL-SELF ':HOME-BOX))
      (PROGN
	(PROMPT-LINE "~C, New Character in ~A" CHAR FONT)
	(FUNCALL-SELF ':ERASE-ALL)
	(FUNCALL-SELF ':HOME-BOX)
	(FUNCALL TV:SUPERIOR ':UPDATE-STATUS-PANE)))))

;M => Read the name of a character and merge it into the data already there. 
;Control asks for a font, Meta asks for a scale
;Typing a control or mouse character as the arg to the M command aborts it.
(DEFUN COM-MERGE-CHARACTER (&AUX CH FONT NUM DENOM)
  (IF (NULL CURRENT-FONT)
      (BARF "No current font")
      (PROGN
	(SETQ FONT (IF (LDB-TEST %%KBD-CONTROL COMMAND-CHAR)
		       (INTERN
			 (STRING-UPCASE (PROMPT-LINE-READLINE "Font to merge character from: "))
			 "FONTS")
		       CURRENT-FONT))
	(COND (NUMERIC-ARG-P
	       (SETQ CH NUMERIC-ARG))
	      (T (SETQ CH (PROMPT-CHARACTER "Character to merge"))))
	(COND ((> CH 400)
	       (BARF "Bad character, Aborted"))
	      ((LDB-TEST %%KBD-META COMMAND-CHAR)
	       (LET ((IBASE 10.))
		 (SETQ NUM (PROMPT-LINE-READ "Scale numerator: "))
		 (SETQ DENOM (PROMPT-LINE-READ "Scale denominator: ")))
	       (MERGE-CHARACTER-SCALED FONT CH NUM DENOM))
	      (T (MERGE-CHARACTER FONT CH))))))

(DEFUN COM-MERGE-THIS ()
  (MERGE-CHARACTER (OR CURRENT-FONT (BARPH "No current font"))
		   (PROMPT-CHARACTER "Character to draw in gray")))



(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN MERGE-CHARACTER-SCALED (FONT CHAR  NUM DENOM &AUX FD CD)
  (LET ((XOFFS CHAR-BOX-X1)
	(YOFFS CHAR-BOX-Y1))
    ;; If we have no FD format array for this font, make one.
    (SETQ FD (FONT-GET-FD FONT))
    ;; Get the character descriptor for the desired character out of the FD.
    (COND ((AND (< CHAR (ARRAY-LENGTH FD))
		(SETQ CD (AREF FD CHAR)))
	   (SETQ XOFFS (+ (- XOFFS (CD-CHAR-LEFT-KERN CD)) CHAR-BOX-X1))
	   ;; Now XWIDTH and YWIDTH get the size of the character's raster,
	   ;; and copy the data into the plane in PLANE.
	   (LET ((XEND (+ XOFFS (// (* (ARRAY-DIMENSION-N 2 CD) NUM) DENOM)))
		 (YEND (+ YOFFS (// (* (ARRAY-DIMENSION-N 1 CD) NUM) DENOM)))
		 (BIG (MAKE-ARRAY NIL 'ART-1B (LIST (* (ARRAY-DIMENSION-N 2 CD) NUM)
						    (* (ARRAY-DIMENSION-N 1 CD) NUM)))))
	     (DO I (1- (ARRAY-DIMENSION-N 2 CD)) (1- I) (MINUSP I)
		 (DO J (1- (ARRAY-DIMENSION-N 1 CD)) (1- J) (MINUSP J)
		     (IF (NOT (ZEROP (AREF CD J I)))
			 (DO M 0 (1+ M) (= M NUM)
			     (DO N 0 (1+ N) (= N NUM)
				 (ASET 1 BIG (+ (* I NUM) M) (+ (* J NUM) N)))))))
	     (FUNCALL-SELF ':NEW-GRAY-PLANE `((,XOFFS ,YOFFS) (,XEND ,YEND)))
	     (LET* ((PORIGIN (PLANE-ORIGIN GRAY-PLANE))
		    (NXSTART (FIRST PORIGIN))
		    (NYSTART (SECOND PORIGIN)))
	       (DO I XOFFS (1+ I) (= I XEND)
		   (DO J YOFFS (1+ J) (= J YEND)
		       
		       (IF (> (LOOP FOR X FROM (* (- I XOFFS) DENOM)
				    BELOW (* (- I XOFFS -1) DENOM)
				    SUMMING (LOOP FOR Y FROM (* (- J YOFFS) DENOM)
						  BELOW (* (- J YOFFS -1) DENOM)
						  COUNT (NOT (ZEROP (AREF BIG X Y)))))
			      (// (* DENOM DENOM) 2))
			   (ASET 1 GRAY-PLANE (- I NXSTART) (- J NYSTART))))))))))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN MERGE-CHARACTER (FONT CHAR &AUX FD CD)
  (LET ((XOFFS CHAR-BOX-X1)
	(YOFFS CHAR-BOX-Y1))
    ;; If we have no FD format array for this font, make one.
    (SETQ FD (FONT-GET-FD FONT))
    ;; Get the character descriptor for the desired character out of the FD.
    (COND ((AND (< CHAR (ARRAY-LENGTH FD))
		(SETQ CD (AREF FD CHAR)))
	   (SETQ XOFFS (+ (- XOFFS (CD-CHAR-LEFT-KERN CD)) CHAR-BOX-X1))
	   ;; Now XWIDTH and YWIDTH get the size of the character's raster,
	   ;; and copy the data into the plane in PLANE.
	   (LET ((XEND (+ XOFFS (SECOND (ARRAY-DIMENSIONS CD))))
		 (YEND (+ YOFFS (FIRST (ARRAY-DIMENSIONS CD)))))
	     (FUNCALL-SELF ':NEW-GRAY-PLANE
			   `((,XOFFS ,YOFFS) (,XEND ,YEND)))
	     (LET* ((PORIGIN (PLANE-ORIGIN GRAY-PLANE))
		    (NXSTART (FIRST PORIGIN))
		    (NYSTART (SECOND PORIGIN)))
	       (DO I XOFFS (1+ I) (= I XEND)
		   (DO J YOFFS (1+ J) (= J YEND)
		       (ASET (AREF CD (- J YOFFS) (- I XOFFS))
			     GRAY-PLANE (- I NXSTART) (- J NYSTART)))))))))))


(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN COM-MERGE-OTHER ()
  (DECLARE (SPECIAL VVCH VVFONT VVNUM VVDEN))
  (IF (NULL CURRENT-FONT) (BARPH "No current font"))
  (DO ((VVCH #\SP) (VVFONT CURRENT-FONT) (VVNUM 1) (VVDEN 1)) (())
    (TV:CHOOSE-VARIABLE-VALUES
      '((VVCH "Character" :CHARACTER)
	(VVFONT "Font Name" :STRING)
	(VVNUM "Scale Numerator" FED-DECNUM)
	(VVDEN "Scale Denominator" FED-DECNUM))
      ':LABEL "Fetch Gray Character Options"
      ':SUPERIOR TV:SUPERIOR
      ':MARGIN-CHOICES '("Do It" ("Abort" (*THROW 'BARF 'BARF))))
    (COND ((NULL VVCH) (BARF "A character must be specified"))
	  ((OR (ZEROP VVNUM) (ZEROP VVDEN))
	   (BARF "Zero is not acceptable in the scale"))
	  ((NOT (AND (BOUNDP (SETQ VVFONT (INTERN (STRING-UPCASE VVFONT) "FONTS")))
		     (TYPEP (SYMEVAL VVFONT) 'FONT)))
	   (BARF "~A is not a loaded font." VVFONT))
	  ((AND (= VVNUM 1) (= VVDEN 1))
	   (RETURN (MERGE-CHARACTER VVFONT VVCH)))
	  (T (RETURN (MERGE-CHARACTER-SCALED VVFONT VVCH VVNUM VVDEN)))))))


;;;
;;;   Reflections....
;;;

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED)
(DEFUN COM-REFLECT ()
  (LET ((AXIS
	  (TV:MENU-CHOOSE
	    '(("The X axis      (-)" :VALUE "X")
	      ("The Y axis      (|)" :VALUE "Y")
	      ("The line X = Y  (//)" :VALUE "XY")
	      ("The line X = -Y (\)" :VALUE "X-Y"))
	    "Line about which to reflect char box"
	    '(:MOUSE)
	    NIL
	    TV:SUPERIOR)))
    (AND AXIS (REFLECT-CHARACTER AXIS)))))

(DEFUN REFLECT-CHARACTER (AXIS &AUX (OLD-PLANE PLANE))
  (SETQ AXIS (CDR (ASSOC AXIS '(("X". :X) ("Y" . :Y) ("X-Y" . :X-Y) ("XY" . :XY)))))
  (SPLAT-OUT-PLANE-DATA
    (FUNCALL-SELF ':NEW-PLANE
		  (SELECTQ AXIS
		    (:X `((,XSTART ,(- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) (1- YEND)))
			  (,XEND ,(- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) (1- YSTART)))))
		    (:Y `((,(- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) (1- XEND)) ,YSTART)
			  (,(- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) (1- XSTART)) ,YEND)))
		    (:X-Y `((,(+ CHAR-BOX-X1 (- YSTART CHAR-BOX-Y1))
			     ,(+ CHAR-BOX-Y1 (- XSTART CHAR-BOX-X1)))
			    (,(+ CHAR-BOX-X1 (- YEND CHAR-BOX-Y1))
			     ,(+ CHAR-BOX-Y1 (- XEND CHAR-BOX-X1)))))
		    (:XY  `((,(+ CHAR-BOX-X1 (- (- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) (1- YEND))
						CHAR-BOX-Y1))
			     ,(+ CHAR-BOX-Y1 (- (- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) (1- XEND))
						CHAR-BOX-X1)))
			    (,(+ CHAR-BOX-X1 (- (- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) (1- YSTART))
						CHAR-BOX-Y1))
			     ,(+ CHAR-BOX-Y1 (- (- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) (1- XSTART))
						CHAR-BOX-X1)))))))
    (LET* ((PORIGIN (PLANE-ORIGIN PLANE))
	   (XPO (FIRST PORIGIN))
	   (YPO (SECOND PORIGIN)))
      (DO ((HPOS XSTART (1+ HPOS)))
	  (( HPOS XEND))
	(DO ((VPOS YSTART (1+ VPOS)))
	    (( VPOS YEND))
	  (LET ((NEWVPOS VPOS) (NEWHPOS HPOS))
	    (SELECTQ AXIS
	      (:X
	       (SETQ NEWVPOS
		     (- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) VPOS)))
	      (:Y
	       (SETQ NEWHPOS
		     (- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) HPOS)))
	      (:X-Y
	       (SETQ NEWHPOS (+ CHAR-BOX-X1 (- VPOS CHAR-BOX-Y1))
		     NEWVPOS (+ CHAR-BOX-Y1 (- HPOS CHAR-BOX-X1))))
	      (:XY
	       ;; Invert in the origin, then reflect in X-Y.
	       (SETQ NEWVPOS
		     (- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) VPOS))
	       (SETQ NEWHPOS
		     (- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) HPOS))
	       (PSETQ NEWHPOS (+ CHAR-BOX-X1 (- NEWVPOS CHAR-BOX-Y1))
		      NEWVPOS (+ CHAR-BOX-Y1 (- NEWHPOS CHAR-BOX-X1)))))
	    (ASET (AREF OLD-PLANE (- HPOS XSTART) (- VPOS YSTART))
		  PLANE (- NEWHPOS XPO) (- NEWVPOS YPO))))))))


;;; Rotators

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN COM-ROTATE-CHARACTER-RIGHT (&AUX (OLD-PLANE PLANE))
  (SPLAT-OUT-PLANE-DATA
    (FUNCALL-SELF ':NEW-PLANE
		  `((,(- CHAR-BOX-X2 1 (- (1- YEND) CHAR-BOX-Y1))
		     ,(+ CHAR-BOX-Y1 (- XSTART CHAR-BOX-X1)))
		    (,(- CHAR-BOX-X2 1 (- (1- YSTART) CHAR-BOX-Y1))
		     ,(+ CHAR-BOX-Y1 (- XEND CHAR-BOX-X1)))))
    (LET* ((NPORG (PLANE-ORIGIN PLANE))
	   (NXSTART (FIRST NPORG))
	   (NYSTART (SECOND NPORG)))
      (DO ((HPOS XSTART (1+ HPOS)))
	  (( HPOS XEND))
	(DO ((VPOS YSTART (1+ VPOS)))
	    (( VPOS YEND))
	  (LET ((NEWVPOS (+ CHAR-BOX-Y1 (- HPOS CHAR-BOX-X1)))
		(NEWHPOS (- CHAR-BOX-X2 1 (- VPOS CHAR-BOX-Y1))))
	    (ASET (AREF OLD-PLANE (- HPOS XSTART) (- VPOS YSTART))
		  PLANE
		  (- NEWHPOS NXSTART)
		  (- NEWVPOS NYSTART)))))))))


(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN COM-ROTATE-CHARACTER-LEFT (&AUX (OLD-PLANE PLANE))
  (SPLAT-OUT-PLANE-DATA
    (FUNCALL-SELF ':NEW-PLANE
		  `((,(+ CHAR-BOX-X1 (- YSTART CHAR-BOX-Y1))
		     ,(- CHAR-BOX-Y1 (- (1- (1+ XEND)) CHAR-BOX-X2)))
		    (,(+ CHAR-BOX-X1 (- YEND CHAR-BOX-Y1))
		     ,(- CHAR-BOX-Y1 (- (1- (1+ XSTART)) CHAR-BOX-X2)))))
    (LET* ((NPORG (PLANE-ORIGIN PLANE))
	   (NXSTART (FIRST NPORG))
	   (NYSTART (SECOND NPORG)))
      (DO ((HPOS XSTART (1+ HPOS)))
	  (( HPOS XEND))
	(DO ((VPOS YSTART (1+ VPOS)))
	    (( VPOS YEND))
	  (LET ((NEWVPOS (- CHAR-BOX-Y1 (- (1+ HPOS) CHAR-BOX-X2)))
		(NEWHPOS (+ CHAR-BOX-X1 (- VPOS CHAR-BOX-Y1))))
	    (ASET (AREF OLD-PLANE (- (1+ HPOS) (1+ XSTART)) (- VPOS YSTART))
		  PLANE
		  (- NEWHPOS NXSTART)
		  (- NEWVPOS NYSTART)))))))))


(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN COM-ROTATE-CHARACTER-180 (&AUX (OLD-PLANE PLANE)
				 (CBY2X (+ CHAR-BOX-Y1 (- CHAR-BOX-X2 CHAR-BOX-X1))))
  (SPLAT-OUT-PLANE-DATA
    (FUNCALL-SELF ':NEW-PLANE
		  `((,(- CHAR-BOX-X1 1 (- (1- XEND) CHAR-BOX-X2))
		     ,(- CHAR-BOX-Y1 1 (- (1- YEND) CBY2X)))
		    (,(- CHAR-BOX-X1 1 (- (1- XSTART) CHAR-BOX-X2))
		     ,(- CHAR-BOX-Y1 1 (- (1- YSTART) CBY2X)))))
    (LET* ((NPORG (PLANE-ORIGIN PLANE))
	   (NXSTART (FIRST NPORG))
	   (NYSTART (SECOND NPORG)))
      (DO ((HPOS XSTART (1+ HPOS)))
	  (( HPOS XEND))
	(DO ((VPOS YSTART (1+ VPOS)))
	    (( VPOS YEND))
	  (LET ((NEWVPOS (- CHAR-BOX-Y1 1 (- VPOS CBY2X)))
		(NEWHPOS (- CHAR-BOX-X1 1 (- HPOS CHAR-BOX-X2))))
	    (ASET (AREF OLD-PLANE (- HPOS XSTART) (- VPOS YSTART))
		  PLANE
		  (- NEWHPOS NXSTART)
		  (- NEWVPOS NYSTART)))))))))


;Stretching and contracting, by Dave Moon.

(DEFUN COM-STRETCH-HORIZONTALLY ()
  (STRETCH-COMMON NIL))

(DEFUN COM-STRETCH-VERTICALLY ()
  (STRETCH-COMMON T))

(DEFUN COM-STRETCH-CHARACTER ()
  (STRETCH-COMMON (LDB-TEST %%KBD-CONTROL COMMAND-CHAR)))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN STRETCH-COMMON (VERTICAL &AUX HOW-FAR ORIGIN (OLD-PLANE PLANE))
  (PROG ABORT (X0 Y0 X1 Y1)
	(PROMPT-LINE "~&Mouse select starting ~:[column~;row~]: " VERTICAL)
	(MULTIPLE-VALUE (X0 Y0) (FUNCALL-SELF ':MOUSE-SELECT-POINT))
	(OR X0 (RETURN-FROM ABORT (BARF "Aborted")))
	(FUNCALL-SELF ':GRAY-POINT X0 Y0)
	(PROMPT-LINE "~&Mouse select which ~:[column~;row~] to move that to: " VERTICAL)
	(MULTIPLE-VALUE (X1 Y1) (FUNCALL-SELF ':MOUSE-SELECT-POINT))
	(OR X1 (RETURN-FROM ABORT (BARF "Aborted")))
	(FUNCALL-SELF ':GRAY-POINT X1 Y1)
	(PROCESS-SLEEP 20.)
	(FUNCALL-SELF ':GRAY-POINT X0 Y0)
	(FUNCALL-SELF ':GRAY-POINT X1 Y1)
	(INCF X0 WINDOW-X-POS)
	(INCF X1 WINDOW-X-POS)
	(INCF Y0 WINDOW-Y-POS)
	(INCF Y1 WINDOW-Y-POS)
	(IF VERTICAL
	    (SETQ ORIGIN (IF (> Y1 Y0)
			     Y0			;there is a subtle asymmetry here
			     (1+ Y1))
		  HOW-FAR (- Y1 Y0))
	    (SETQ ORIGIN (IF (> X1 X0)
			     (1+ X0)
			     (1+ X1))
		  HOW-FAR (- X1 X0)))
	(IF (ZEROP HOW-FAR) (RETURN-FROM ABORT NIL))
	(SPLAT-OUT-PLANE-DATA
	  (FUNCALL-SELF ':NEW-PLANE
			`((,XSTART ,YSTART)
			  (,(+ XEND (IF VERTICAL 0 HOW-FAR))
			   ,(+ YEND (IF VERTICAL HOW-FAR 0)))))
	  (LET* ((NPORIGIN (PLANE-ORIGIN PLANE))
		 (NXSTART (FIRST NPORIGIN))
		 (NYSTART (SECOND NPORIGIN)))
	    (DO ((HPOS XSTART (1+ HPOS))		;Copy with offset
		 (NEW-HPOS XSTART (1+ NEW-HPOS))
		 (HBOUND (IF (NOT VERTICAL) ORIGIN 1000000)))
		(( HPOS XEND))
	      (DO ((VPOS YSTART (1+ VPOS))
		   (NEW-VPOS YSTART (1+ NEW-VPOS))
		   (VBOUND (IF VERTICAL ORIGIN 1000000)))
		  (( VPOS YEND))
		(ASET (AREF OLD-PLANE (- HPOS XSTART)
			    (- VPOS YSTART))
		      PLANE (- NEW-HPOS NXSTART) (- NEW-VPOS NYSTART))
		(IF (= HPOS HBOUND)		;Horizontal stretch
		    (OR ( NEW-HPOS (+ HBOUND HOW-FAR)) (DECF HPOS)))
		(IF (= NEW-HPOS HBOUND)		;Horizontal shrink
		    (OR ( HPOS (- HBOUND HOW-FAR)) (DECF NEW-HPOS)))
		(IF (= VPOS VBOUND)		;Vertical stretch
		    (OR ( NEW-VPOS (+ VBOUND HOW-FAR)) (DECF VPOS)))
		(IF (= NEW-VPOS VBOUND)		;Vertical shrink
		    (OR ( VPOS (- VBOUND HOW-FAR)) (DECF NEW-VPOS))))))))) )


(DEFUN COM-REGENERATE-FONT ()
  (AND CURRENT-CHARACTER (FONT-STORE-CD CURRENT-FONT CURRENT-CHARACTER NIL))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR CURRENT-FONT (FONT-GET-FD CURRENT-FONT)))

;Save the editing that has been done on the current character.
(DEFUN COM-SAVE-CHARACTER ()
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (PROMPT-LINE "Saving ~C (~O) in ~A" CURRENT-CHARACTER CURRENT-CHARACTER CURRENT-FONT)
      (FONT-STORE-CD CURRENT-FONT CURRENT-CHARACTER)))

;Store the current FED data buffer into character CHAR of the font descriptor
;array for font FONT.
(DEFUN FONT-STORE-CD (FONT CHAR &OPTIONAL (UPDATE-FONT-FLAG T)
				&AUX FD CD YSTART XSTART YWIDTH XWIDTH KERN
				     PLANE-X1 PLANE-Y1 PLANE-WIDTH PLANE-HEIGHT)
  (PROG FONT-STORE-CD ()
    ;; Find the FD format array for this font.
    (SETQ FD (FONT-GET-FD FONT))
    ;; Warn if char box now displayed is incompatible with the font.
    (COND ((OR ( (- CHAR-BOX-Y2 CHAR-BOX-Y1) (FD-BASELINE FD))
	       ( (- CHAR-BOX-Y3 CHAR-BOX-Y1) (FD-LINE-SPACING FD)))
	   (OR (Y-OR-N-P "/
Character height and baseline are incompatible with font.
If actually stored, the character will be aligned by the top of its box.
;;;---!!! Corrupt file. (27-Jan-2017 AMS)
(LIST YWIDTH XWIDTH))
                      CD-CHAR-WIDTH (- CHAR-BOX-X2 CHAR-BOX-X1)
                      CD-CHAR-LEFT-KERN KERN))
    (DO I 0 (1+ I) (= I XWIDTH)
       (DO J 0 (1+ J) (= J YWIDTH)
          (AS-2 (AR-2 PLANE (+ I XSTART) (+ J YSTART))
                CD J I)))
    (COND (UPDATE-FONT-FLAG
           ;; Use the CD just made to update the font itself,or make a new font.
           (FONT-NAME-STORE-CD FONT CD CHAR))
          (T
           ;; Store the CD in the FD.
	   (AND (>= CHAR (ARRAY-LENGTH FD))
		(ADJUST-ARRAY-SIZE FD (1+ CHAR)))
           (AS-1 CD FD CHAR)
           (AND (= CHAR #\SP)
                (SETF (FD-SPACE-WIDTH FD) (CD-CHAR-WIDTH CD)))))))

;; Display all of the characters of the font being edited, to show what they look like.
;; Above each one is the corresponding character of CPTFONT, so you
;; can see which character is which in non-alphabetic fonts.
;; Note 1. ZMACS CALLS THIS!!!!! (M-X Display Font)
;; Note 2. Drawn chars are mouse-sensitive, except when ZMACS calls it.

(DEFUN COM-DISPLAY-FONT (&OPTIONAL FONT (WINDOW TERMINAL-IO) (CHARACTER CURRENT-CHARACTER)
				   (CLEAR-FIRST-P T)
				   &AUX
				   (NOT-FED-P (NOT (TYPEP WINDOW
							  'FED-MOUSABLE-TYPEOUT-WINDOW))))
  (OR FONT (SETQ FONT (AND (BOUNDP CURRENT-FONT) (SYMEVAL CURRENT-FONT))))
  (IF FONT
      (LET* ((NAME (FONT-NAME FONT))
	     (FD (FONT-GET-FD NAME))
	     (DF (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN WINDOW))))
	(AND CLEAR-FIRST-P (FUNCALL WINDOW ':CLEAR-SCREEN))
	(FORMAT WINDOW "~2&Font ~A:~%" NAME)
	(OR NOT-FED-P (FORMAT WINDOW "~&Mouse any character to begin editing it."))
	(DO ((CH 0) (OCH) (LEN (ARRAY-LENGTH FD))) ((= CH LEN))
	  (TV:SHEET-CRLF WINDOW)
	  ;; If there is not room for a line in the default font
	  ;; followed by a line in the font being edited
	  ;; before we would need to **more**,
	  ;; then **more** right now, and go to top of window afterward.
	  (COND ((TV:PREPARE-SHEET (WINDOW)
		   (> (+ (TV:SHEET-CURSOR-Y WINDOW)
			 (TV:SHEET-LINE-HEIGHT WINDOW)
			 (FONT-CHAR-HEIGHT FONT))
		      (- (TV:SHEET-INSIDE-BOTTOM WINDOW)
			 (TV:SHEET-LINE-HEIGHT WINDOW))))
		 (SETF (TV:SHEET-MORE-FLAG WINDOW) 1)
		 (TV:SHEET-HANDLE-EXCEPTIONS WINDOW)
		 (SETF (TV:SHEET-END-PAGE-FLAG WINDOW) 1)))
	  (TV:PREPARE-SHEET (WINDOW)
	    (SETQ OCH CH)
	    ;; Output one line of chars in the default font,
	    ;; spaced so that they lie above the corresponding chars in the next line.
	    ;; Stop at margin, or when we reach a char code that's a multiple of 32.
	    (DO ()
		((> (+ (TV:SHEET-CURSOR-X WINDOW)
		       (MAX (FED-CHAR-DISPLAY-WIDTH FD CH)
			    (FONT-CHARACTER-WIDTH DF CH)))
		    (TV:SHEET-INSIDE-RIGHT WINDOW)))
	      (COND ((OR (AREF FD CH)
			 (EQ CH CHARACTER))
		     (TV:SHEET-TYO WINDOW CH)
		     (TV:SHEET-INCREMENT-BITPOS WINDOW
						(- (MAX (FED-CHAR-DISPLAY-WIDTH FD CH)
							(FONT-CHARACTER-WIDTH DF CH))
						   (FONT-CHARACTER-WIDTH DF CH))
						0)))
	      (SETQ CH (1+ CH))
	      (AND (= CH LEN) (RETURN))
	      (AND (ZEROP (\ CH 32.)) (RETURN)))
	    (TV:SHEET-CRLF WINDOW)
	    ;; Clear out what we will move down over with SHEET-INCREMENT-BITPOS.
	    (TV:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH WINDOW)
				(FONT-CHAR-HEIGHT FONT)
				(TV:SHEET-INSIDE-LEFT WINDOW)
				(+ (TV:SHEET-CURSOR-Y WINDOW) (TV:SHEET-LINE-HEIGHT WINDOW))
				TV:ALU-ANDCA WINDOW)
	    ;; Now output the corresponding chars in the font being edited.
	    ;; First leave space so it won't overlap if font is taller.
	    (TV:SHEET-INCREMENT-BITPOS WINDOW 0 (- (FONT-BASELINE FONT)
						   (TV:SHEET-BASELINE WINDOW)))
	    (TV:SHEET-SET-FONT WINDOW FONT)
	    (DO ()
		((> (+ (TV:SHEET-CURSOR-X WINDOW)
		       (MAX (FED-CHAR-DISPLAY-WIDTH FD OCH)
			    (FONT-CHARACTER-WIDTH DF OCH)))
		    (TV:SHEET-INSIDE-RIGHT WINDOW)))
	      (COND ((OR (AREF FD OCH)
			 (EQ CH CHARACTER))
		     (LET ((WIDTH (MAX (FED-CHAR-DISPLAY-WIDTH FD OCH)
				       (FONT-CHARACTER-WIDTH DF OCH))))
		       (OR NOT-FED-P
			   (FUNCALL
			     WINDOW ':PRIMITIVE-ITEM
			     'CHARACTER OCH
			     (- (TV:SHEET-CURSOR-X WINDOW) (TV:SHEET-INSIDE-LEFT WINDOW))
			     (- (TV:SHEET-CURSOR-Y WINDOW) (TV:SHEET-INSIDE-TOP WINDOW)
				(- (FONT-BASELINE FONT) (TV:SHEET-BASELINE WINDOW)))
			     (- (+ (TV:SHEET-CURSOR-X WINDOW) WIDTH)
				(TV:SHEET-INSIDE-LEFT WINDOW))
			     (+ (- (TV:SHEET-CURSOR-Y WINDOW) (TV:SHEET-INSIDE-TOP WINDOW))
				(FONT-BLINKER-HEIGHT FONT)
				(- (TV:SHEET-BASELINE WINDOW) (FONT-BASELINE FONT)))))
		       (FED-TYO WINDOW OCH CHARACTER)
		       (TV:SHEET-INCREMENT-BITPOS
			 WINDOW
			 (- WIDTH (FONT-CHARACTER-WIDTH FONT OCH)) 0))))
	      (SETQ OCH (1+ OCH))
	      (AND (= OCH LEN) (RETURN))
	      (AND (ZEROP (\ OCH 32.)) (RETURN)))
	    (TV:SHEET-SET-FONT WINDOW DF)
	    ;; Move down, leaving space for font's descenders.
	    (TV:SHEET-INCREMENT-BITPOS WINDOW 0 (- (FONT-CHAR-HEIGHT FONT)
						   (- (FONT-BASELINE FONT)
						      (TV:SHEET-BASELINE WINDOW))))
	    (SETF (TV:SHEET-CURSOR-X WINDOW) (TV:SHEET-INSIDE-LEFT WINDOW))))
	(FORMAT WINDOW "~&(Type any char to flush)"))
      (BARF "No current font")))

(DEFUN FED-CHAR-DISPLAY-WIDTH (FD CHAR)
  (COND ((AND (< CHAR (ARRAY-LENGTH FD))
	      (AREF FD CHAR))
	 (+ 3 (ARRAY-DIMENSION-N 2 (AREF FD CHAR))
	    (MAX 0 (- (CD-CHAR-LEFT-KERN (AREF FD CHAR))))))
	(T 0)))

;; Return the width of a given char in a given font.
(DEFUN FONT-CHARACTER-WIDTH (FONT CHAR)
  (LET ((CWT (FONT-CHAR-WIDTH-TABLE FONT)))
    (IF CWT (AREF CWT CHAR)
	(FONT-CHAR-WIDTH FONT))))

;Get the font descriptor corresponding to the specified font.
;If the font is a nonexistent one (being created), make a default empty FD.
(DEFUN FONT-GET-FD (FONT-SYMBOL &AUX FD)
  (IF (BOUNDP FONT-SYMBOL)
      (FONT-NAME-FONT-DESCRIPTOR FONT-SYMBOL)
      (SETQ FD (MAKE-FONT-DESCRIPTOR MAKE-ARRAY (:LENGTH 200)
				     FD-FILL-POINTER (1+ #\SP)	;let sp be seen - if you don't
						;do this, conversion gets 0 max raster
				     FD-NAME FONT-SYMBOL
				     FD-LINE-SPACING 14
				     FD-BASELINE 11
				     FD-BLINKER-HEIGHT 14
				     FD-BLINKER-WIDTH 7
				     FD-SPACE-WIDTH 7))
      (ASET
	(MAKE-CHAR-DESCRIPTOR
	  :MAKE-ARRAY (:TYPE 'ART-4B :DIMENSIONS '(11 7))	;
	  CD-CHAR-WIDTH 7
	  CD-CHAR-LEFT-KERN 0)
	FD
        #\SP)
      (PUTPROP FONT-SYMBOL FD 'FONT-DESCRIPTOR)
      (SET FONT-SYMBOL NIL)
      (PUTPROP FONT-SYMBOL NIL 'FONT-DESCRIBED)
      FD))

;; Print a character on SHEET, assuming that SHEET's current font is the
;; font being edited.  If the character is the one being edited,
;; the picture being edited is displayed.

(DEFUN FED-TYO (SHEET CH &OPTIONAL (CHARACTER CURRENT-CHARACTER))
  (IF (NEQ CH CHARACTER)
      (TV:SHEET-TYO SHEET CH)
      (LET (;; Offset from horiz idx in plane to hpos of dot on screen.
	    (LEFT (+ (- (TV:SHEET-CURSOR-X SHEET) CHAR-BOX-X1)
		     (FIRST (PLANE-ORIGIN PLANE))))
	    ;; Offset from vert idx in plane to vpos of dot on screen.
	    
	    (TOP (+ (- (TV:SHEET-CURSOR-Y SHEET) CHAR-BOX-Y2)
		    (TV:SHEET-BASELINE SHEET)
		    (SECOND (PLANE-ORIGIN PLANE))))
	    (PLANE-WIDTH (ARRAY-DIMENSION-N 1 PLANE))
	    ;; First vertical idx to print from in plane.
	    (PLANE-TOP (MAX 0 (- CHAR-BOX-Y1 (SECOND (PLANE-ORIGIN PLANE)))))
	    ;; Last+1 vertical idx to print from in plane.
	    (PLANE-BOTTOM (MIN (ARRAY-DIMENSION-N 2 PLANE)
			       (- CHAR-BOX-Y3 (SECOND (PLANE-ORIGIN PLANE))))))
	(TV:PREPARE-SHEET (SHEET)
	  (DOTIMES (HPOS PLANE-WIDTH)
	    (DO ((VPOS PLANE-TOP (1+ VPOS)))
		((>= VPOS PLANE-BOTTOM))
	      (OR (ZEROP (AREF PLANE HPOS VPOS))
		  (%DRAW-RECTANGLE 1 1 (+ HPOS LEFT) (+ VPOS TOP) TV:ALU-IOR SHEET))))
	  (TV:SHEET-INCREMENT-BITPOS SHEET (- CHAR-BOX-X2 CHAR-BOX-X1) 0)))))


;;;
;;; Commands mousables
;;;

(DEFPROP COM-MOUSE-DRAW T MOUSE-COMMAND)	;tells interp to pass mousing

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED)
(DEFUN COM-MOUSE-DRAW (BLIP)
  (SETQ CURSOR-ON NIL)
  (FUNCALL BLINKER ':SET-VISIBILITY NIL)
  (FUNCALL-SELF ':MOUSE-BOOLE-SQUARES (FOURTH BLIP) (FIFTH BLIP) DRAW-MODE)))

(DEFPROP COM-MOUSE-MOVE-CHAR-BOX T MOUSE-COMMAND)

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED)
(DEFUN COM-MOUSE-MOVE-CHAR-BOX (BLIP) 
  (SETQ CURSOR-ON NIL)
  (FUNCALL BLINKER ':SET-VISIBILITY NIL)
  (FUNCALL-SELF ':MOUSE-MOVE-CHAR-BOX (FOURTH BLIP) (FIFTH BLIP))
  (FUNCALL TV:SUPERIOR ':REDISPLAY-CHAR-PANE)))

(DEFPROP COM-CHANGE-DRAW-MODE T MOUSE-COMMAND)

(DEFUN COM-CHANGE-DRAW-MODE (IGNORE)
  (CHANGE-DRAW-MODE))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED)
(DEFUN CHANGE-DRAW-MODE (&OPTIONAL MODE)
  (FUNCALL-SELF ':SET-DRAW-MODE
	   (OR MODE (SELECTQ DRAW-MODE (2 6) (6 7) (7 2) (OTHERWISE 6))))
  (FUNCALL TV:SUPERIOR ':UPDATE-DRAW-MODE-PANE)))



(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN COM-MOUSE-DRAW-LINE (&AUX X0 Y0 X1 Y1)
  (PROMPT-LINE "Select end points with left mouse button")
  (MULTIPLE-VALUE (X0 Y0) (FUNCALL-SELF ':MOUSE-SELECT-POINT))
  (OR X0 (BARPH "Aborted"))
  (FUNCALL-SELF ':GRAY-POINT X0 Y0)		;Mark first endpoint
  (MULTIPLE-VALUE (X1 Y1) (FUNCALL-SELF ':MOUSE-SELECT-POINT))
  (OR X1 (BARPH "Aborted"))
  (FUNCALL PROMPT-WINDOW ':CLEAR-SCREEN)	;Make extraneous prompt go away
  (FUNCALL-SELF ':GRAY-POINT X0 Y0)		;Erase first point
  (FUNCALL-SELF ':DRAW-GRID-LINE X0 Y0 X1 Y1 DRAW-MODE)
  (FUNCALL TV:SUPERIOR ':REDISPLAY-CHAR-PANE)))

(DEFVAR SPLINE-X)
(DEFVAR SPLINE-Y)
(DEFVAR SPLINE-CX NIL)



(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN COM-MOUSE-DRAW-SPLINE (&AUX I Y)
  (COND ((NOT (BOUNDP 'SPLINE-X))
	 (SETQ SPLINE-X (MAKE-ARRAY NIL 'ART-Q 100. NIL '(0))
	       SPLINE-Y (MAKE-ARRAY NIL 'ART-Q 100. NIL '(0)))))
  (STORE-ARRAY-LEADER 0 SPLINE-X 0)
  (STORE-ARRAY-LEADER 0 SPLINE-Y 0)
  (SETQ SPLINE-CX NIL SPLINE-CY NIL)
  (PROMPT-LINE "Select points with left mouse button.  Middle to abort.  Click right when done.")
  (DO ((X)) (NIL)
    (MULTIPLE-VALUE (X Y) (FUNCALL-SELF ':MOUSE-SELECT-POINT))
    (OR X (RETURN NIL))
    (FUNCALL-SELF ':GRAY-POINT X Y)
    (ARRAY-PUSH-EXTEND SPLINE-X X)
    (ARRAY-PUSH-EXTEND SPLINE-Y Y))
  (DOTIMES (I (ARRAY-ACTIVE-LENGTH SPLINE-X))	;Erase old marks
    (FUNCALL-SELF ':GRAY-POINT (AREF SPLINE-X I) (AREF SPLINE-Y I)))
  (IF (< (ARRAY-ACTIVE-LENGTH SPLINE-X) 2)
      (BARPH "We need at least two points to draw a curve!"))
  (COND ((AND Y (= (LDB %%KBD-MOUSE-BUTTON Y) 2))
	 (MULTIPLE-VALUE (SPLINE-CX SPLINE-CY I)
	   (TV:SPLINE SPLINE-X SPLINE-Y 10. SPLINE-CX SPLINE-CY))
	 (FUNCALL PROMPT-WINDOW ':CLEAR-SCREEN)
	 (FUNCALL-SELF ':DRAW-CURVE SPLINE-CX SPLINE-CY I DRAW-MODE)
	 (FUNCALL TV:SUPERIOR ':REDISPLAY-CHAR-PANE))
	(T (BARF "Aborted")))))

(DEFUN COM-SET-SETA-DRAW-MODE () (CHANGE-DRAW-MODE  7))
(DEFUN COM-SET-ANDCA-DRAW-MODE () (CHANGE-DRAW-MODE 2))
(DEFUN COM-SET-XOR-DRAW-MODE () (CHANGE-DRAW-MODE  6))

(DEFUN COM-QUIT () (FUNCALL *FED-GREAT-FRAME* ':BURY))
(DEFUN COM-REFRESH () (FUNCALL-SELF ':REFRESH))


(DEFUN COM-LIST-FONTS ()
  (LIST-FONTS-LOADED)
  (FORMAT T "~&Type any character to flush:"))

(DEFUN COM-LIST-ALL-FONTS ()
  (LIST-FONTS-LOADED)
  (LIST-FONTS-FILE-COMPUTER)
  (FORMAT T "~&Type any character to flush:"))

(DEFUN LIST-FONTS-LOADED ()
  (FORMAT T "Loaded fonts - Mouse one to select~%")
  (FUNCALL STANDARD-OUTPUT ':ITEM-LIST 'FONT
	   (LOCAL-DECLARE ((SPECIAL LIST))
	     (LET ((LIST NIL))
	       (MAPATOMS-ALL #'(LAMBDA (X) (AND (BOUNDP X) (TYPEP (SYMEVAL X) 'FONT)
						(PUSH X LIST)))
			     "FONTS")
	       (SORT LIST #'STRING-LESSP)))))

(DEFUN LIST-FONTS-FILE-COMPUTER ()
  (FORMAT T "~%Fonts on file computer - Mouse one to select~%")
  (FUNCALL STANDARD-OUTPUT ':ITEM-LIST 'FONT
	   (SORT (MAPCAR #'(LAMBDA (X) (INTERN (STRING (FUNCALL (CAR X) ':NAME)) "FONTS"))
			 (CDR (FS:DIRECTORY-LIST "SYS:FONTS;* QFASL >" ':FAST)))
		 #'ALPHALESSP)))


(DEFUN COM-EVAL ()
  (PROMPT-LINE "~S" (EVAL (PROMPT-LINE-READ "Eval: "))))



(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED)
(DEFUN COM-ADD-GRAY-MENU ()
  (LET ((X (TV:MENU-CHOOSE
	     '(("OR Gray plane into black" :VALUE :COM-MERGE-PLANES
		:DOCUMENTATION
		"Wherever a gray point is, turn on the black square there")
	       ("XOR Gray plane into black" :VALUE XOR-PLANES
		:DOCUMENTATION
		"Wherever a gray point is, flip the black square there")
	       ("ANDCA Gray plane into black" :VALUE ANDCA-PLANES
		:DOCUMENTATION
		"Wherever a gray point is, turn OFF the black square there")
	       ("OR Gray and clear gray" :VALUE COM-MERGE-PLANES-CLEAR-GRAY
		:DOCUMENTATION
		"Merge gray into black by ORing and clear gray plane when done"))
	     "Plane Combination Options"
	     '(:MOUSE) NIL TV:SUPERIOR)))
    (SELECTQ X
      (NIL)
      (XOR-PLANES   (FUNCALL-SELF ':MERGE-PLANES TV:ALU-XOR))
      (ANDCA-PLANES (FUNCALL-SELF ':MERGE-PLANES TV:ALU-ANDCA))
      (OTHERWISE    (FUNCALL X))))))

(DEFUN COM-XCH-PLANES ()
  (FUNCALL-SELF ':EXCHANGE-PLANES))

(DEFUN COM-MERGE-PLANES ()
  (FUNCALL-SELF ':MERGE-PLANES))

(DEFUN COM-MERGE-PLANES-CLEAR-GRAY ()
  (FUNCALL-SELF ':MERGE-PLANES)
  (FUNCALL-SELF ':CLEAR-GRAY-PLANE))

(DEFUN COM-CLEAR-GRAY-PLANE ()
  (FUNCALL-SELF ':CLEAR-GRAY-PLANE))

(DEFUN COM-MOVE-GRAY-PLANE ()
  (PROG ABORT ()
	(PROMPT-LINE "Select with mouse the point on GRAY PLANE to be moved...")
	(MULTIPLE-VALUE-BIND (XFROM YFROM)
	    (FUNCALL-SELF ':MOUSE-SELECT-POINT)
	  (OR XFROM (RETURN-FROM ABORT NIL))
	  (FUNCALL-SELF ':GRAY-POINT XFROM YFROM)
	  (PROMPT-LINE "Select with mouse the point on the BLACK PLANE to move that to...")
	  (MULTIPLE-VALUE-BIND (XTO YTO)
	      (FUNCALL-SELF ':MOUSE-SELECT-POINT)
	    (OR XTO (RETURN-FROM ABORT NIL))
	    (FUNCALL-SELF ':GRAY-POINT XTO YTO)
	    (PROCESS-SLEEP 20.)
	    (FUNCALL-SELF ':GRAY-POINT XFROM YFROM)
	    (FUNCALL-SELF ':GRAY-POINT XTO YTO)
	    (FUNCALL-SELF ':MOVE-GRAY-PLANE XFROM YFROM XTO YTO)))))

(DEFUN COM-MOVE-BLACK-PLANE ()
  (PROG ABORT ()
	(PROMPT-LINE "Select with mouse the point on BLACK PLANE to be moved...")
	(MULTIPLE-VALUE-BIND (XFROM YFROM)
	    (FUNCALL-SELF ':MOUSE-SELECT-POINT)
	  (OR XFROM (RETURN-FROM ABORT NIL))
	  (FUNCALL-SELF ':GRAY-POINT XFROM YFROM)
	  (PROMPT-LINE "Select with mouse the point on the BLACK PLANE to move that to...")
	  (MULTIPLE-VALUE-BIND (XTO YTO)
	      (FUNCALL-SELF ':MOUSE-SELECT-POINT)
	    (OR XTO (RETURN-FROM ABORT NIL))
	    (FUNCALL-SELF ':GRAY-POINT XTO YTO)
	    (PROCESS-SLEEP 20.)
	    (FUNCALL-SELF ':GRAY-POINT XFROM YFROM)
	    (FUNCALL-SELF ':GRAY-POINT XTO YTO)
	    (FUNCALL-SELF ':MOVE-PLANE XFROM YFROM XTO YTO)))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN COM-CONFIGURE ()
  (LET ((C (TV:MENU-CHOOSE '(("Tall Aspect" :VALUE :STANDARD :DOCUMENTATION
			      "Tall edit pane")
			     ("Wide Aspect" :VALUE :WIDE
			      :DOCUMENTATION "Wide edit pane"))
			   "FED frame configuration"
			   '(:MOUSE)
			   (CDR (ASSQ (FUNCALL TV:SUPERIOR ':CONFIGURATION)
				      '((:WIDE . :STANDARD) (:STANDARD . :WIDE))))
			   TV:SUPERIOR)))
    (IF C (FUNCALL TV:SUPERIOR ':SET-CONFIGURATION C)))))



;;;Register stuff 28 Sept 1981

(DEFUN *FED-REGISTERS* NIL)			;truly global?

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN REG-STORE (PANE N WHICH-PLANE)
  (SETQ WHICH-PLANE (FED-PLANE-COPY WHICH-PLANE))
  (FUNCALL PANE
	   ':SET-FEDREG N
	   (MAKE-FEDREG
	     X1 CHAR-BOX-X1
	     X2 CHAR-BOX-X2
	     Y1 CHAR-BOX-Y1
	     Y2 CHAR-BOX-Y2
	     Y3 CHAR-BOX-Y3
	     PLANE WHICH-PLANE))))

(DEFUN FED-PLANE-COPY (PLANE)
  (LET ((NEW-PLANE (MAKE-ARRAY (ARRAY-DIMENSIONS PLANE)
			       ':LEADER-LENGTH (ARRAY-LEADER-LENGTH PLANE)
			       ':TYPE (ARRAY-TYPE PLANE))))
    (COPY-ARRAY-CONTENTS-AND-LEADER PLANE NEW-PLANE)
    NEW-PLANE))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN REG-RESTORE (REG MSG)
  (FUNCALL-SELF MSG NIL (FED-PLANE-COPY (FEDREG-PLANE REG)))
  (COMMENT(IF (EQ MSG ':NEW-PLANE)		;this feature is problematic
	      (SETQ CHAR-BOX-X1 (FEDREG-X1 REG)
		    CHAR-BOX-X2 (FEDREG-X2 REG)
		    CHAR-BOX-Y1 (FEDREG-Y1 REG)
		    CHAR-BOX-Y2 (FEDREG-Y2 REG)
		    CHAR-BOX-Y3 (FEDREG-Y3 REG))))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-FED-PANE)
(DEFUN COM-REGISTER-MOUSE (REGPANE REG N OP)
  (SELECTQ OP
    (:DEFAULT-FETCH-REGISTER
     (COND ((NULL REG)
	    (IF (= N 0)
		(BARPH "Don't store into the upper left-hand register! Type HELP for info.")
		(REG-STORE REGPANE N PLANE)))	     
	   (T (IF ( N 0) (REG-STORE REGPANE 0 PLANE))
	      (REG-RESTORE REG ':NEW-PLANE))))
    (:REG-FETCH-BLACK
     (COND ((NULL REG) (BARPH "Reg ~D is an empty register" N))
	   (T (IF ( N 0) (REG-STORE REGPANE 0 PLANE))
	      (REG-RESTORE REG ':NEW-PLANE))))
    (:REG-FETCH-GRAY  (COND ((NULL REG) (BARPH "Reg ~D is an empty register" N))
			    (T (REG-RESTORE REG ':NEW-GRAY-PLANE))))
    (:REG-STORE-BLACK  (REG-STORE REGPANE N PLANE))
    (:REG-STORE-GRAY  (REG-STORE REGPANE N GRAY-PLANE)))))

