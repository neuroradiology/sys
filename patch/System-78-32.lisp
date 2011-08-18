;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.32
;;; Reason: New Terminal B command: bury the selected window.
;;; Written 12/24/81 10:22:42 by dlw,
;;; while running on Spaniel from band 2
;;; with System 78.30, ZMail 38.5, Symbolics 8.7, Tape 6.4, LMFS 21.18, Canon 9.5, microcode 841.



; From file BASSTR.LISP >LMWIN POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

(DEFUN KBD-BURY (ARG) ;esc B
  ARG ;unused for now
  (COND (SELECTED-WINDOW
	 (FUNCALL (FUNCALL SELECTED-WINDOW ':ALIAS-FOR-SELECTED-WINDOWS) ':BURY)))
  (SETQ KBD-ESC-TIME NIL))

)

; From file BASSTR.LISP >LMWIN POINTER:
#8R TV:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TV")))

;;; "Escape key"

; A list of elements (char function documentation . options).
; Typing [terminal] char activates this element.  If function is a list it is
; evaluated, otherwise it is a function to be applied to one argument, which
; is NIL or the numeric-arg typed by the user.  In either case it happens
; in a separate process.  documentation is a form to evaluate to get the
; documentation, a string or NIL to leave this key undocumented.
; Documentation can be a list of strings to go on separate lines.
; The following options in the CDDDR of the list are:
;    :TYPEAHEAD - copy the contents of the
;	software buffer into the currently selected IO-BUFFER.  This has the
;	effect of treating everything typed before the ESC as typeahead to
;	the currently selected window.  Useful for ESC commands that
;	change the selected window.  These commands should set KBD-ESC-TIME to NIL
;       as soon as they change the selected window, unless they complete quickly
;       (input should never be done with KBD-ESC-TIME non-NIL).
;    :KEYBOARD-PROCESS - run the function in the keyboard process instead of starting
;	a new process for it.

; Unknown or misspelled keywords are ignored.
(DEFVAR *ESCAPE-KEYS*
     '( (#\CLEAR KBD-ESC-CLEAR "Discard type-ahead" :KEYBOARD-PROCESS)
	(#\FORM (KBD-SCREEN-REDISPLAY)
		"Clear and redisplay all windows (Page = Clear Screen)")
	(#/A KBD-ESC-ARREST
	     "Arrest process in who-line (minus means unarrest)" :KEYBOARD-PROCESS)
	(#/B KBD-BURY
	     "Bury the selected window" :TYPEAHEAD)
	(#/C KBD-COMPLEMENT
	     '("Complement video black-on-white state"
	       "With an argument, complement the who-line documentation window")
	      :KEYBOARD-PROCESS)
	(#/D (SI:BUZZ-DOOR) (AND (SI:TECH-SQUARE-FLOOR-P 9) "Open the door"))
	(#/E (SI:CALL-ELEVATOR) (AND (OR (SI:TECH-SQUARE-FLOOR-P 8)
					 (SI:TECH-SQUARE-FLOOR-P 9))
				     "Call the elevator"))
	(#/F KBD-FINGER (FINGER-ARG-PROMPT)
			:TYPEAHEAD)
	(#/H (KBD-HOSTAT) "Show status of CHAOSnet hosts" :TYPEAHEAD)
	(#/M KBD-ESC-MORE "**MORE** enable (complement, or arg=1:on, 0 off)"
			  :KEYBOARD-PROCESS)
	(#/O KBD-OTHER-EXPOSED-WINDOW "Select another exposed window" :TYPEAHEAD)
	(#/Q KBD-ESC-Q
	     (AND *SCREEN-HARDCOPY-MODE*
		  (FORMAT NIL "Hardcopy the screen on the ~A" *SCREEN-HARDCOPY-MODE*)))
	(#/S KBD-SWITCH-WINDOWS
	 '("Select the most recently selected window.  With an argument, select the nth"
	   "previously selected window and rotate the top n windows.  (Default arg is 2)."
	   "With an arg of 1, rotate through all the windows.  With a negative arg rotate"
	   "in the other direction.  With an argument of 0, select a window that wants"
	   "attention, e.g. to report an error.")
	   :TYPEAHEAD)
	(#/T KBD-ESC-T
	 '("Control the selected window's notification properties."
	   "Toggle output notification, and make input the same as output."
	   "0 Turn both off; 1 turn both on; 2 output on, input off; 3 output off, input on."
	   "4 Let output proceed with with window deexposed, input on; 5 Same, input off."
	   "(You can also use the Attribute command in the Screen Editor.)"))
	(#/W KBD-ESC-W
	 '("Switch which process the wholine looks at.  Default is just to refresh it"
	   " 1 means selected-window's process, 2 means freeze on this process,"
	   " 3 means rotate among all processes, 4 means rotate other direction,"
	   " 0 gives a menu of all processes"))
	(#\HOLD-OUTPUT KBD-ESC-OUTPUT-HOLD "Expose window on which we have /"Output Hold/"")
	(#/? KBD-ESC-HELP NIL :TYPEAHEAD)
	(#\HELP KBD-ESC-HELP NIL :TYPEAHEAD)
	(NIL) ;Ones after here are "for wizards"
	(#\CALL (KBD-USE-COLD-LOAD-STREAM) "Get to cold-load stream" :TYPEAHEAD)
	(#/T KBD-CLEAR-TEMPORARY-WINDOWS "Flush temporary windows")
	(#\CLEAR KBD-CLEAR-LOCKS "Clear window-system locks")
	(#/G (BEEP) "Beep the beeper")))  ;Should this be flushed now?

)
