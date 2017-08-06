;;;-*- Mode:LISP; Package:HACKS -*-

;;; Shared definitions for the hacks.

(DEFMACRO WITH-REAL-TIME BODY
  `(LET ((OLD-SB-STATE (SI:SB-ON)))
     (UNWIND-PROTECT
       (PROGN
	 (SI:SB-ON '(:KEYBOARD))
	 . ,BODY)
       (SI:SB-ON OLD-SB-STATE))))

;;; System for menu of demos

(DEFVAR *DEMO-ALIST*
	NIL
  "Menu item list.  Elements are (name :VALUE <value> :DOCUMENTATION <string>).
   <value>s are either forms to evaluate or lists of shape (MENU name . elements),
   where elements are recursively the same thing.")


(DEFMACRO DEFDEMO (NAME DOCUMENTATION &REST ARGS)
  "For a simple demo, (DEFDEMO <name> <documentation> <form>).
   For a sub-menu, (DEFDEMO <name> <documentation> <sub-menu title> . <elements>)
   where each <element> is a list that looks like the cdr of a defdemo form."
  `(SETQ *DEMO-ALIST* (ADD-OR-UPDATE-DEMO *DEMO-ALIST* ',NAME ',DOCUMENTATION ',ARGS)))

(DEFSTRUCT (DEMO-LIST-ELEMENT (:TYPE :LIST))
  DEMO-NAME
  (DEMO-VALUE-SYMBOL ':VALUE)
  DEMO-VALUE
  (DEMO-DOCUMENTATION-SYMBOL ':DOCUMENTATION)
  DEMO-DOCUMENTATION)

;;; Given a demo list, add the new demo, or update the old demo of the same
;;; name, and return the updated demo list.
(DEFUN ADD-OR-UPDATE-DEMO (DEMO-LIST NAME DOCUMENTATION ARGS)
  (LET ((ELEMENT (OR (ASSOC NAME DEMO-LIST)
		     (CAR (PUSH (MAKE-DEMO-LIST-ELEMENT DEMO-NAME NAME) DEMO-LIST)))))
    (SETF (DEMO-DOCUMENTATION ELEMENT) DOCUMENTATION)
    (SETF (DEMO-VALUE ELEMENT)
	  (IF (= (LENGTH ARGS) 1)
	      ;; This is the simple form.
	      (FIRST ARGS)
	      ;; This is the hairy form.
	      `(MENU ,(FIRST ARGS) . ,(LET ((LIST (CDDR (DEMO-VALUE ELEMENT))))
					(DOLIST (X (REST1 ARGS))
					  (SETQ LIST
						(ADD-OR-UPDATE-DEMO
						  LIST
						  (FIRST X)
						  (SECOND X)
						  (REST2 X))))
					LIST)))))
  DEMO-LIST)

(DEFUN DEMO (&OPTIONAL (ALIST *DEMO-ALIST*) (NAME "Demo:"))
  (SETQ ALIST (SORTCAR (COPYLIST ALIST) #'STRING-LESSP))
  (LOOP AS CHOICE = (TV:MENU-CHOOSE ALIST NAME)
	UNTIL (NULL CHOICE)
	DO (IF (EQ (CAR CHOICE) 'MENU)
	       (DEMO (CDDR CHOICE) (CADR CHOICE))
	       (*CATCH 'SYS:COMMAND-LEVEL (EVAL CHOICE)))))
