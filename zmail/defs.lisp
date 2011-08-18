;;; Lisp Machine mail reader -*- Mode:LISP; Package:ZWEI -*- 
;;; Definitions for ZMail
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFMACRO DEFINE-ZMAIL-TOP-LEVEL-COMMAND (FN DOC OPTIONS &BODY DEF)
  `(PROGN 'COMPILE
     (ZMAIL-TOP-LEVEL-COMMAND-DEFINE ',FN ',DOC ',OPTIONS)
     (DEFUN ,FN ()
       ,@(PROCESS-ZMAIL-TOP-LEVEL-COMMAND-OPTIONS OPTIONS)
       . ,DEF)))

(DEFVAR *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST* NIL)

(DEFUN ZMAIL-TOP-LEVEL-COMMAND-DEFINE (COMMAND DOC IGNORE)
  (COND ((STRINGP DOC)
	 (PUTPROP COMMAND DOC 'DOCUMENTATION))
	((OR (SYMBOLP DOC)
	     (AND (NOT (ATOM DOC))
		  (EQ (CAR DOC) 'LAMBDA)))
	 (PUTPROP COMMAND DOC 'DOCUMENTATION-FUNCTION))
	(T
	 (FERROR NIL "The command ~S has invalid self-documentation ~S" COMMAND DOC)))
  (LET ((NAME (MAKE-ZMAIL-TOP-LEVEL-COMMAND-NAME COMMAND)))
    (PUTPROP COMMAND NAME 'COMMAND-NAME)
    (OR (ASSOC NAME *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*)
	(PUSH (CONS NAME COMMAND) *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*))))

(DEFUN PROCESS-ZMAIL-TOP-LEVEL-COMMAND-OPTIONS (OPTIONS
						&AUX (CONTEXT-CONDITION 'MUST-HAVE-MSG)
						     (ARGUMENT-CONDITION 'NO-ARG))
  (DOLIST (OP OPTIONS)
    (SELECTQ OP
      ((NO-MAIL-FILE-OK NO-MSG-OK MUST-HAVE-MSG)
       (SETQ CONTEXT-CONDITION OP))
      ((NUMERIC-ARG-OK NO-ARG)
       (SETQ ARGUMENT-CONDITION OP))
      (OTHERWISE
       (FERROR NIL "~S is not a recognized option" OP))))
  (LIST (CADR (ASSQ CONTEXT-CONDITION '((MUST-HAVE-MSG
					 (OR *MSG*
					     (BARF "There is no current message")))
					(NO-MSG-OK
					 (OR *MAIL-FILE*
					     (BARF "There is no current mail file")))
					(NO-MAIL-FILE-OK))))
	(CADR (ASSQ ARGUMENT-CONDITION '((NO-ARG
					  (AND *NUMERIC-ARG-P*
					       (BARF "This command does not take an argument")
					       ))
					 (NUMERIC-ARG-OK))))
	))

;;; Convert a string into human-readable form.  Remove leading COM-, or leading
;;; and trailing *'s.  Conver hyphens into spaces, and capitalize each word.
;;; This is used both for command names and variable names.
(DEFUN MAKE-ZMAIL-TOP-LEVEL-COMMAND-NAME (COMMAND)
  (SETQ COMMAND (STRING COMMAND))
  (LET ((CLEN (STRING-LENGTH COMMAND)))
    (LET ((STR (SUBSTRING COMMAND
			  (COND ((STRING-EQUAL "COM-ZMAIL-" COMMAND 0 0 12 12) 12)
				((STRING-EQUAL "COM-MOUSE-" COMMAND 0 0 12 12) 12)
				((STRING-EQUAL "COM-" COMMAND 0 0 4 4) 4)
				((STRING-EQUAL "*" COMMAND 0 0 1 1) 1)
				(T 0))
			  (COND ((CHAR-EQUAL #/* (AREF COMMAND (1- CLEN))) (1- CLEN))
				(T CLEN)))))
       (DO ((I 0 (1+ I))
            (FLAG T)
            (CHAR)
            (LIM (STRING-LENGTH STR)))
           (( I LIM) STR)
         (SETQ CHAR (AREF STR I))
         (COND ((= CHAR #/-)
                (ASET #\SP STR I)
                (SETQ FLAG T))
               (FLAG
                (SETQ FLAG NIL))
               ((AND ( CHAR #/A)
                     ( CHAR #/Z))
                (ASET (+ 40 CHAR) STR I)))))))

;;; Top-level-commands
(DEFVAR *ZMAIL-COMMAND-ALIST* '(("Profile" . COM-ZMAIL-PROFILE)
				("Quit" . COM-ZMAIL-QUIT)
				("Delete" . COM-ZMAIL-DELETE)
				("Undelete" . COM-ZMAIL-UNDELETE)
				("Reply" . COM-ZMAIL-REPLY)
				("Configure" . COM-ZMAIL-CONFIGURE)
				("Save files" . COM-ZMAIL-SAVE)
				("Next" . COM-ZMAIL-NEXT)
				("Previous" . COM-ZMAIL-PREVIOUS)
				("Continue" . COM-ZMAIL-CONTINUE)
				("Survey" . COM-ZMAIL-SURVEY)
				("Get new mail" . COM-GET-NEW-MAIL)
				("Jump" . COM-ZMAIL-GOTO)
				("Keywords" . COM-ZMAIL-KEYWORDS)
				("Mail" . COM-ZMAIL-MAIL)
				("Sort" . COM-ZMAIL-SORT)
				("Map over" . COM-ZMAIL-MAP)
				("Move to file" . COM-ZMAIL-MOVE)
				("Select file" . COM-ZMAIL-SELECT)
				("Other" . COM-ZMAIL-OTHER-COMMANDS)))

;;; Commands without message arguments
(DEFVAR *ZMAIL-NO-FILTER-COMMAND-ALIST* '(("Configure" . COM-ZMAIL-CONFIGURE)
					  ("Get new mail" . COM-GET-NEW-MAIL)
					  ("Save files" . COM-ZMAIL-SAVE)
					  ("Sort" . COM-ZMAIL-SORT)
					  ("Profile" . COM-ZMAIL-PROFILE)
					  ("Quit" . COM-ZMAIL-QUIT)))

(DEFVAR *ZMAIL-FILTER-COMMAND-ALIST* '(("Concatenate" . COM-ZMAIL-CONCATENATE)
				       ("Survey" . COM-ZMAIL-SURVEY)
				       ("Delete" . COM-ZMAIL-DELETE)
				       ("Undelete" . COM-ZMAIL-UNDELETE)
				       ("Reply" . COM-ZMAIL-REPLY)
				       ("Other" . COM-ZMAIL-OTHER-COMMANDS)
				       ("Type" . COM-ZMAIL-TYPE)
				       ("Next" . COM-ZMAIL-NEXT)
				       ("Previous" . COM-ZMAIL-PREVIOUS)
				       ("Continue" . COM-ZMAIL-CONTINUE)
				       ("" :NO-SELECT T)
				       ("Select" . COM-ZMAIL-SELECT)
				       ("Keywords" . COM-ZMAIL-KEYWORDS)
				       ("Move to file" . COM-ZMAIL-MOVE)
				       ("Mail" . COM-ZMAIL-MAIL)))

(DEFVAR *OTHER-COMMAND-ALIST*)

(DEFVAR *ZMAIL-WINDOW*)

(DEFVAR *ZMAIL-TYPEOUT-ITEM-ALIST* NIL)

(DEFVAR *ZMAIL-MSG-AREA* (MAKE-AREA ':NAME 'ZMAIL-MSG-AREA))
(DEFVAR *ZMAIL-MSG-LINE-AREA* (MAKE-AREA ':NAME 'ZMAIL-MSG-LINE-AREA))

(DEFVAR *HEADER-NAME-ALIST*
  '(
    ("Bcc" . :BCC)
    ("Cc" . :CC)
    ("Date" . :DATE)
    ("Draft-Composition-Date" . :DRAFT-COMPOSITION-DATE)
    ("Expiration-date" . :EXPIRATION-DATE)
    ("Fcc" . :FCC)
    ("From" . :FROM)
    ("In-reply-to" . :IN-REPLY-TO)
    ("Message-ID" . :MESSAGE-ID)
    ("Redistributed-by" . :REDISTRIBUTED-BY)
    ("Redistributed-date" . :REDISTRIBUTED-DATE)
    ("Redistributed-to" . :REDISTRIBUTED-TO)
    ("References" . :REFERENCES)
    ("Reply-to" . :REPLY-TO)
    ("Sender" . :SENDER)
    ("Subject" . :SUBJECT)
    ("To" . :TO)
    ))

(DEFVAR *RECIPIENT-TYPE-HEADERS* '(:TO :CC :BCC :REDISTRIBUTED-TO :FORWARDED-TO-TO))
(DEFVAR *SENDER-TYPE-HEADERS* '(:FROM :SENDER :REPLY-TO :REDISTRIBUTED-BY :FORWARDED-TO-BY))
(DEFVAR *SENDER-OR-RECIPIENT-TYPE-HEADERS* `(,@*SENDER-TYPE-HEADERS*
					     . ,*RECIPIENT-TYPE-HEADERS*))
;;; Text of these headers is parsed as RFC733 headers
(DEFVAR *ADDRESS-TYPE-HEADERS* *SENDER-OR-RECIPIENT-TYPE-HEADERS*)
;;; Text of these headers is parsed as a date/time specification
(DEFVAR *DATE-TYPE-HEADERS* '(:DATE :DRAFT-COMPOSITION-DATE :EXPIRATION-DATE
			      :REDISTRIBUTED-DATE :FORWARDED-TO-DATE))
;;; These reference other messages in some way
(DEFVAR *REFERENCE-TYPE-HEADERS* '(:IN-REPLY-TO :REFERENCES))
;;; Several instances within these properties are separated by ",<CR>".
(DEFVAR *SINGLE-LINE-TYPE-HEADERS* *REFERENCE-TYPE-HEADERS*)

;;; These are properties which are not dependent on the text of the message
;;; Other properties which gotten from the text indirectly and hence not in the
;;; keyword package are: LOSING-HEADERS, REFERENCES, ITS-HEADER-P, HEADERS-END-BP, LENGTH,
;;; HASH-ID
(DEFVAR *INTERNAL-TYPE-PROPERTIES*
	'(DELETED UNSEEN REFORMATTED ANSWERED FILED PROCESSED MARKED FORWARDED
	  RECENT LENGTH KEYWORDS KEYWORDS-STRING DRAFT-MSG))

(DEFVAR *HANG-BACKGROUND-PROCESS-WHEN-DEEXPOSED* T)

(DEFVAR *SAVED-INTERNAL-PROPERTIES-ALIST*
	'(("last" . LAST)			;This is really treated differently
	  ("unseen" . UNSEEN)
	  ("deleted" . DELETED)
	  ("bad-header" . LOSING-HEADERS)
	  ("answered" . ANSWERED)
	  ("forwarded" . FORWARDED)
	  ("redistributed" . REDISTRIBUTED)
	  ("filed" . FILED)
	  ("recent" . RECENT)))

(DEFVAR *SORT-KEY-ALIST-1*
	'(("Date" :VALUE MSG-DATE-SORT-LESSP :DOCUMENTATION "Chronologically by date.")
	  ("To" :VALUE MSG-TO-STRING-LESSP :DOCUMENTATION "Alphabetically by To: field.")
	  ("From" :VALUE MSG-FROM-STRING-LESSP :DOCUMENTATION
	   "Alphabetically by From: field.")
	  ("Subject" :VALUE MSG-SUBJECT-STRING-LESSP :DOCUMENTATION
	   "Alphabetically by Subject: field.")
	  ("Keywords" :VALUE MSG-KEYWORD-LESSP :DOCUMENTATION
	   "Alphabetically by keywords present.")
	  ("Text" :VALUE MSG-TEXT-STRING-LESSP :DOCUMENTATION
	   "Alphabetically by actual message text.")
	  ("Length" :VALUE MSG-LENGTH-LESSP
		    :DOCUMENTATION "Numerically by length of message in characters.")))

(DEFINE-USER-OPTION-ALIST *ZMAIL-HARDCOPY-OPTION-ALIST* DEFINE-ZMAIL-HARDCOPY-OPTION)

;;; Mail file/buffer, actual array contains messages themselves
(DEFFLAVOR MAIL-FILE
	(NAME					;Name of the mail file
	 ARRAY					;Where actual messages live
	 (OPTIONS NIL)				;Property list
	 (SAVED-CURRENT-MSG NIL))		;When switching back
	()
  :ORDERED-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  (:INIT-KEYWORDS :APPEND-P))

(DEFFLAVOR DISK-MAIL-FILE
	(;; Those really inherited
	 NAME
	 ARRAY
	 OPTIONS
	 SAVED-CURRENT-MSG
	 ;; Our own
	 INTERVAL				;The actual text of the file
	 PATHNAME
	 (LOCK NIL)
	 (ID NIL)
	 (TICK NIL)
	 MSG-UPDATE-TICK
	 (STATUS NIL)				;Special state or NIL
						;States for old mail are :LOADING and :SAVING
						;for new mail :NEW-MAIL, :LOADING-NEW-MAIL,
						;and :AWAITING-SAVE.
	 (STREAM NIL)
	 (OTHER-MAIL-FILE NIL))			;New mail if old file, old mail if new file
	()
  (:INCLUDED-FLAVORS MAIL-FILE)
  :ORDERED-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFMACRO MAIL-FILE-NMSGS (MAIL-FILE)
  `(ARRAY-LEADER (MAIL-FILE-ARRAY ,MAIL-FILE) 0))

(DEFMACRO MAIL-FILE-START-BP (MAIL-FILE)
  `(INTERVAL-FIRST-BP (DISK-MAIL-FILE-INTERVAL ,MAIL-FILE)))

(DEFMACRO MAIL-FILE-END-BP (MAIL-FILE)
  `(INTERVAL-LAST-BP (DISK-MAIL-FILE-INTERVAL ,MAIL-FILE)))

;;; Lock a mail file around BODY
(DEFMACRO LOCK-MAIL-FILE ((MAIL-FILE) &BODY BODY)
  (LET ((LOCK (GENSYM)) (LOCKED-P (GENSYM)))
    `(LET ((,LOCK (LOCF (DISK-MAIL-FILE-LOCK ,MAIL-FILE)))
	   (,LOCKED-P NIL))
       (UNWIND-PROTECT
	 (PROGN
	   (COND ((NEQ (CAR ,LOCK) CURRENT-PROCESS)
		  (PROCESS-LOCK ,LOCK)
		  (SETQ ,LOCKED-P T)))
	   . ,BODY)
	 (AND ,LOCKED-P (PROCESS-UNLOCK ,LOCK))))))

(DEFMACRO MAIL-FILE-DISK-P (MAIL-FILE)
  `(TYPEP ,MAIL-FILE 'DISK-MAIL-FILE))

(DEFMACRO MAIL-FILE-APPEND-P (MAIL-FILE)
  `(GET (LOCF (MAIL-FILE-OPTIONS ,MAIL-FILE)) ':APPEND))

(DEFMACRO DOMSGS ((MSG MAIL-FILE) &BODY BODY)
  `(LET ((.ARRAY. (MAIL-FILE-ARRAY ,MAIL-FILE)))
     (DO ((.I. 0 (1+ .I.))
	  (.NMSGS. (ARRAY-ACTIVE-LENGTH .ARRAY.))	
	  (,MSG))
	 (( .I. .NMSGS.))
       (SETQ ,MSG (AREF .ARRAY. .I.))
       . ,BODY)))

(DEFINE-LOOP-PATH MSGS MSG-PATH (IN))

(DEFUN MSG-PATH (PATH-NAME VARIABLE IGNORE PREP-PHRASES INCLUSIVE-P IGNORE IGNORE)
  (OR PREP-PHRASES
      (FERROR NIL "Missing IN between ~S and ~S" PATH-NAME VARIABLE))
  (AND INCLUSIVE-P
       (FERROR NIL "Inclusive not supported"))
  (LET ((ARRAY (SI:LOOP-NAMED-VARIABLE 'ARRAY))
	(SIZE (SI:LOOP-NAMED-VARIABLE 'SIZE))
	(INDEX (SI:LOOP-NAMED-VARIABLE 'INDEX)))
    (LIST `((,VARIABLE NIL)
	    (,ARRAY (MAIL-FILE-ARRAY ,(CADAR PREP-PHRASES)))
	    (,SIZE NIL)
	    (,INDEX 0))
	  `((SETQ ,SIZE (ARRAY-ACTIVE-LENGTH ,ARRAY)))
	  `( ,INDEX ,SIZE)
	  NIL
	  NIL
	  `(,VARIABLE (AREF ,ARRAY ,INDEX)
	    ,INDEX (1+ ,INDEX)))))

;;; Messages
(DEFSTRUCT (MSG :ARRAY :NAMED :CONC-NAME
		(:MAKE-ARRAY (:AREA *ZMAIL-MSG-AREA*)))
  REAL-INTERVAL					;Where the message starts in the file itself
  INTERVAL					;Displayed portion of message
  TICK						;Last time something munged in some way
  MAIL-FILE					;The file this lives in
  SUMMARY-LINE					;String displayed in summary window
  DISPLAYED-INDEX				;Number for when displayed in summary window
  STATUS					;Alist of keywords for message
  PARSED-P)					;NIL, T, or :IN-PROGRESS

(DEFMACRO MSG-REAL-START-BP (MSG)
  `(INTERVAL-FIRST-BP (MSG-REAL-INTERVAL ,MSG)))

(DEFMACRO MSG-REAL-END-BP (MSG)
  `(INTERVAL-LAST-BP (MSG-REAL-INTERVAL ,MSG)))

(DEFMACRO MSG-START-BP (MSG)
  `(INTERVAL-FIRST-BP (MSG-INTERVAL ,MSG)))

(DEFMACRO MSG-END-BP (MSG)
  `(INTERVAL-LAST-BP (MSG-INTERVAL ,MSG)))

;;; Get something off a message's "property list"
(DEFMACRO MSG-GET (MSG PROPNAME)
  `(GET (ASSURE-MSG-PARSED ,MSG) ,PROPNAME))

(DEFMACRO MSG-DRAFT-MSG-P (MSG)
  `(NOT (NULL (MSG-GET ,MSG ':DRAFT-COMPOSITION-DATE))))

(DEFSTRUCT (DRAFT-MSG :ARRAY :NAMED :CONC-NAME (:INCLUDE NODE)
					       (:CONSTRUCTOR MAKE-DRAFT-MSG-INTERNAL))
  HEADER-INTERVAL				;Headers of message
  REPLY-INTERVAL				;Body of text
  SUMMARY-STRING				;For continue command
  SUMMARY-STRING-TICK				;When last valid
  MSGS-BEING-REPLIED-TO				;If from reply command
  MSGS-BEING-FORWARDED				;If from forward command
  SENT-P					;Sent successfully
  LAST-WINDOW-CONFIGURATION			;Value of *WINDOW-CONFIGURATION* when done
  PATHNAME					;When saved out in a file
  MSG						;In which it is saved
  WINDOW-POINTS					;Saved positions in the various windows
  )

(DEFSTRUCT (ZMAIL-INTERVAL :ARRAY :NAMED (:INCLUDE NAMED-BUFFER))
  )

(DEFMACRO ZMAIL-INTERVAL-P (INTERVAL)
  `(TYPEP ,INTERVAL 'ZMAIL-INTERVAL))

(DEFSTRUCT (ZMAIL-PROFILE-INTERVAL :ARRAY :NAMED (:INCLUDE FILE-BUFFER))
  )

(DEFSTRUCT (SUMMARY-LINE :ARRAY-LEADER :CONC-NAME
			 (:MAKE-ARRAY (:LENGTH 140 :TYPE 'ART-STRING)))
  (LENGTH 0)
  TEMPLATE)

(DEFMACRO ZMAIL-BACKGROUND-REQUEST-PUSH (THING)
  `(WITHOUT-INTERRUPTS
     (PUSH ,THING (CAR *ZMAIL-BACKGROUND-REQUEST-CELL*))))

(DEFMACRO ZMAIL-BACKGROUND-RESPONSE-PUSH (THING)
  `(LOCAL-DECLARE ((SPECIAL TV:IO-BUFFER))
     (COMMAND-BUFFER-PUSH (CONS 'BACKGROUND ,THING))))

(DEFMACRO USING-OVERLYING-WINDOW (&BODY BODY)
  `(UNWIND-PROTECT
     (PROGN
       (FUNCALL *OVERLYING-WINDOW* ':DELETE-TEXT)
       (FUNCALL *OVERLYING-WINDOW* ':SELECT NIL)
       (LET ((TERMINAL-IO *OVERLYING-WINDOW*)
	     (STANDARD-INPUT SI:SYN-TERMINAL-IO)
	     (STANDARD-OUTPUT SI:SYN-TERMINAL-IO))
	 . ,BODY))
;     (FUNCALL *ZMAIL-WINDOW* ':DEEXPOSE NIL ':NOOP)
     (FUNCALL *OVERLYING-WINDOW* ':DEACTIVATE)
     (FUNCALL *ZMAIL-WINDOW* ':SELECT NIL)))

(DEFMACRO WITH-BACKGROUND-PROCESS-LOCKED (&BODY BODY)
  `(LET ((.LOCKED-P. NIL))
     (UNWIND-PROTECT
       (PROGN
	 (SETQ .LOCKED-P. (LOCK-BACKGROUND-PROCESS))
	 . ,BODY)
       (AND .LOCKED-P. (PROCESS-UNLOCK *ZMAIL-BACKGROUND-PROCESS-LOCK*)))))

(DEFMACRO MAKE-EMPTY-STRING (LENGTH)
  `(MAKE-ARRAY ,LENGTH ':TYPE 'ART-STRING ':LEADER-LIST '(0)))

;;; This is a separate flavor to distinguish the instance variables that should be global
;;; so that commands, etc. can access them (as opposed to window-system type variables).
(DEFFLAVOR BASIC-ZMAIL
       (*HEADER-WINDOW*				;Headers when sending
	*HEADER-INTERVAL*
	*REPLY-WINDOW*				;Text when sending
	*REPLY-INTERVAL*
	*MSG-WINDOW*				;Text of message
	*MSG-INTERVAL*
	*SUMMARY-WINDOW*			;Summary of messages
	*FILTER-WINDOW*				;Hairy menu pane for filter mode
	*PROFILE-WINDOW*			;Frame for changing profile
	*PROFILE-EDITOR*			;Editor that goes with above and below
	*PROFILE-EDITOR-WINDOW*
	*COMMAND-MENU*				;The main command menu
	*KEYWORD-WINDOW*			;Menu for hacking keywords
	(*WINDOW-CONFIGURATION* NIL)		;The present configuration
	*CURRENT-MSG-NAME*			;The number of the current msg as a string
	*ZMAIL-INTERVAL-NAME*			;Name of interval in reply for mode line
	(*MAIL-FILE* NIL)			;The mail file being hacked
	(*MAIL-FILE-LIST* NIL)			;The list of ones known about
	(*PRIMARY-MAIL-FILE* NIL)		;The one associated with new mail
	*ZMAIL-FILE-NAME*			;Name of that file for the mode line
	(*MSG* NIL)				;The current message
	*MSG-NO*				;Numerical index of above
	(*MSG-POINT-PDL*)			;Saved positions
	*ZMAIL-BACKGROUND-PROCESS*		;Handles asynchronous tasks
	*ZMAIL-BACKGROUND-PROCESS-LOCK*		;Lock for synchronizing
	*ZMAIL-BACKGROUND-REQUEST-CELL*		;Locative for communication
	*CURRENT-MSG-KEYWORDS-STRING*		;String of current messages keywords
	*SELECTABLE-MODE-LINE-ELEMENTS*		;Alist of modeline element and command
	(*MSG-MORE-STRING* NIL)			;When can scroll message from mode line
	*DRAFT-MSG*				;Current message being sent
	(*DRAFT-LIST* NIL)			;List of drafts for continue
	(*DEFAULT-MOVE-MAIL-FILE* NIL)		;When clicking left on move command
	*MOVE-MAIL-FILE-MENU*			;Pop-up for choosing where
	*ZMAIL-MAP-COMMAND-MENU*		;Things you can do to all messages
	*SELECT-MAIL-FILE-MENU*			;For select command
	*FILTER-SELECTION-FRAME*		;Frame for choosing a filter
	*UNIVERSE-SELECTION-MENU*		;Menu for choosing a universe for filter
	*UNIVERSE-DEFINITION-FRAME*		;Frame for defining a new universe
	*OVERLYING-WINDOW*			;For scrolling typeout of message texts
	*POP-UP-MINI-BUFFER-EDITOR*		;For asking temporary questions
	)
       ())
		    
(GLOBALLY-DECLARE-FLAVOR-INSTANCE-VARIABLES BASIC-ZMAIL)

(DEFVAR *WINDOW-CONFIGURATION-ALIST* '(("Summary only" :VALUE :SUMMARY
					:DOCUMENTATION "Just the summary window.")
				       ("Both" :VALUE :BOTH
					:DOCUMENTATION
				       "Summary window at the top and message at the bottom.")
				       ("Message only" :VALUE :MSG
					:DOCUMENTATION "Just display current message.")
				       ("Experimental" :VALUE :NEW
					:DOCUMENTATION "Every command takes a filter.")))

(DEFVAR *DELETE-DIRECTION-ALIST* '(("Backward" :VALUE :BACKWARD :DOCUMENTATION
				    "Move backward after deleting this message.")
				   ("Forward" :VALUE :FORWARD :DOCUMENTATION
				    "Move forward after deleting this message.")
				   ("Remove" :VALUE :REMOVE :DOCUMENTATION
			       "Actually remove this message from this temporary mail file.")
				   ("No" :VALUE :NO :DOCUMENTATION
				    "Do not move after the deletion.")))

(DEFVAR *REPLY-MODES-ALIST*
	'((("All" :VALUE :ALL :DOCUMENTATION "To Sender and original To, Cc original Cc.")
	   ("All-Cc" :VALUE :ALL-CC :DOCUMENTATION "To Sender, Cc original To and Cc.")
	   ("Cc-All" :VALUE :CC-ALL
		     :DOCUMENTATION "To original To, Cc Sender and original Cc.")
	   ("To" :VALUE :TO :DOCUMENTATION "To Sender and original To.")
	   ("To-Cc" :VALUE :TO-CC :DOCUMENTATION "To Sender, Cc original To.")
	   ("Cc-To" :VALUE :CC-TO :DOCUMENTATION "To original To, Cc Sender.")
	   ("Sender" :VALUE :SENDER :DOCUMENTATION "To Sender."))
	  (("Two-windows" :VALUE :TWO-WINDOWS
	    :DOCUMENTATION "Message being replied to on top and reply below.")
	   ("One-window" :VALUE :ONE-WINDOW :DOCUMENTATION "Just text of reply.")
	   ("Yank" :VALUE :YANK
	    :DOCUMENTATION "Just text of reply with message being replies to yanked in."))))

(DEFVAR *YES-NO-ASK-ALIST*
	'(("Yes" :VALUE T) ("No" :VALUE NIL) ("Ask" . :ASK)))

(DEFVAR *MAIL-SENDING-MODE-ALIST*
	'(("COMSAT" :VALUE FILE-SEND-IT :SITE-KEYWORD :COMSAT
	   :DOCUMENTATION "Write a request file for the mailer via the file job.")
	  ("Chaos" :VALUE CHAOS-SEND-IT :SITE-KEYWORD :CHAOS
	   :DOCUMENTATION "Mail via the chaosnet mail protocol to some MAIL server.")
	  ("Chaos Direct" :VALUE CHAOS-DIRECT-SEND-IT :SITE-KEYWORD :CHAOS
						      :DEFAULT-SITE-KEYWORD :DIRECT-CHAOS
	   :DOCUMENTATION "Mail via the chaosnet mail protocol to each host specified.")
	  ("Ether" :VALUE ETHER-SEND-IT :SITE-KEYWORD :ETHER
	   :DOCUMENTATION "Mail via the ethernet mail protocol.")
	  ))

(DEFVAR *HEADER-FORCE-ALIST* '(("None" :VALUE :NONE
				:DOCUMENTATION "No special header force, COMSAT chooses.")
			       ("RFC733" :VALUE :RFC733
				:DOCUMENTATION "RFC733 standard headers.")
			       ("Network" :VALUE :NETWORK
				:DOCUMENTATION "Network standard headers.")
			       ("ITS" :VALUE :ITS
				:DOCUMENTATION "Single line ITS headers.")))

(DEFVAR *HEADER-FORMAT-ALIST* '(("Short" :VALUE :SHORT :DOCUMENTATION
  "Use /"@/" to separate user and host, no personal names.")
				("Long" :VALUE :LONG :DOCUMENTATION
  "Use /" at /" to separate user and host, no personal names.")
				("Include personal" :VALUE :INCLUDE-PERSONAL :DOCUMENTATION
  "Include the user's personal name if any.")
				("Use original" :VALUE :USE-ORIGINAL :DOCUMENTATION
  "Use the exact text of the address from the original.")))

(DEFVAR *BUG-DOCUMENTATION* (MAKE-EMPTY-STRING 95.))

(DEFVAR *ZMAIL-MAIL-MENU-ALIST*
	`(("Bug" :VALUE :BUG
	   :DOCUMENTATION ,*BUG-DOCUMENTATION*)
	  ("Mail" :VALUE :MAIL :DOCUMENTATION "Normal mail.")
	  ("Forward" :VALUE :FORWARD :DOCUMENTATION 
  "Forward this message.  Starts sending a message with this as its text.")
	  ("Redistribute" :VALUE :REDISTRIBUTE :DOCUMENTATION 
  "Redistribute this message.  Sends a message with the original headers plus Redistributed.")
	  ("Local" :VALUE :LOCAL
	   :DOCUMENTATION "Create new message in current mail file and edit it.")))

(DEFVAR *MOVE-TO-NEXT-MENU-ALIST*
	'(("Next undeleted" :VALUE :NEXT-UNDELETED :DOCUMENTATION
	   "Move to next undeleted message.")
	  ("Next" :VALUE :NEXT :DOCUMENTATION
	   "Move to next message, even if deleted.")
	  ("Last undeleted" :VALUE :LAST-UNDELETED :DOCUMENTATION
	   "Move to last undeleted message in the file.")
	  ("Last" :VALUE :LAST :DOCUMENTATION
	   "Move to last message in file, even if deleted")))

(DEFVAR *MOVE-TO-PREVIOUS-MENU-ALIST*
	'(("Previous undeleted" :VALUE :PREVIOUS-UNDELETED :DOCUMENTATION
	   "Move to previous undeleted message.")
	  ("Previous" :VALUE :PREVIOUS :DOCUMENTATION
	   "Move to previous message, even if deleted.")
	  ("First undeleted" :VALUE :FIRST-UNDELETED :DOCUMENTATION
	   "Move to first undeleted message in the file.")
	  ("First" :VALUE :FIRST :DOCUMENTATION
	   "Move to last message in file, even if deleted")))

(DEFVAR *ZMAIL-MAP-COMMAND-ALIST*
	'(("Delete" . COM-ZMAIL-DELETE-ALL)
	  ("Undelete" . COM-ZMAIL-UNDELETE-ALL)
	  ("Type" . COM-ZMAIL-TYPE-ALL)
	  ("Find string" . COM-ZMAIL-OCCUR)
	  ("Keywords" . COM-ZMAIL-KEYWORDS-ALL)
	  ("Unkeywords" . COM-ZMAIL-UNKEYWORDS-ALL)
	  ("Move to file" . COM-ZMAIL-MOVE-ALL-TO-FILE)
	  ("Forward" . COM-ZMAIL-FORWARD-ALL)
	  ("Redistribute" . COM-ZMAIL-REDISTRIBUTE-ALL)
	  ("Reply" . COM-ZMAIL-REPLY-ALL)
	  ("Concatenate" . COM-ZMAIL-CONCATENATE-ALL)))

(DEFVAR *KEYWORDS-DOCUMENTATION* (MAKE-EMPTY-STRING 95.))
(DEFVAR *SUMMARY-MOVE-DOCUMENTATION* (MAKE-EMPTY-STRING 95.))
(DEFVAR *SUMMARY-REPLY-DOCUMENTATION* (MAKE-EMPTY-STRING 95.))
(DEFVAR *EDIT-MSG-DOCUMENTATION* (MAKE-EMPTY-STRING 95.))

(DEFVAR *UNIVERSE-BUTTON-DOCUMENTATION* (MAKE-EMPTY-STRING 95.))
(DEFVAR *FILTER-BUTTON-DOCUMENTATION* (MAKE-EMPTY-STRING 95.))

;;; If you change this list, look at ZMAIL-SUMMARY-MOUSE, which knows the order of the
;;; elements
(DEFVAR *SUMMARY-MOUSE-MENU-ALIST*
	`(("Continue" :VALUE :REPLY :DOCUMENTATION "Continue sending this draft message.")
	  ("Keywords" :VALUE :KEYWORDS
		      :DOCUMENTATION ,*KEYWORDS-DOCUMENTATION*)
	  ("Delete" :VALUE :DELETE
		    :DOCUMENTATION "Delete this message.")
	  ("Undelete" :VALUE :UNDELETE
		      :DOCUMENTATION "Undelete this message.")
	  ("Remove" :VALUE :REMOVE
		    :DOCUMENTATION "Remove this message from this temporary mail file.")
	  ("Reply" :VALUE :REPLY :DOCUMENTATION ,*SUMMARY-REPLY-DOCUMENTATION*)
	  ("Move to file" :VALUE :MOVE
			  :DOCUMENTATION ,*SUMMARY-MOVE-DOCUMENTATION*)
	  ("Append" :VALUE :APPEND :DOCUMENTATION
  "Append this message to the end of another:  L: current message; R: specify from summary.")
	  ("Filter" :VALUE :FILTER
		    :DOCUMENTATION "Filter according to some attribute of this message.")))

(DEFVAR *SUMMARY-MOUSE-MIDDLE-MENU-ALIST*
	`(("Delete//Undelete" :VALUE :DELETE-OR-UNDELETE :DOCUMENTATION
	   "Delete if not deleted, else undelete.")
	  ("Delete//Remove" :VALUE :DELETE-OR-REMOVE :DOCUMENTATION
	   "Delete if from disk mail file, else remove.")
	  . ,(CDR *SUMMARY-MOUSE-MENU-ALIST*)))

(DEFVAR *REQUIRE-SUBJECTS-ALIST*
	'(("Yes" :VALUE T)
	  ("No" :VALUE NIL)
	  ("On bug reports" :VALUE :BUG)))

;;; This is for defining things that should be reset when the user changes
(DEFVAR *ZMAIL-GLOBAL-INITIALIZATION-LIST* NIL)
(DEFMACRO DEFINE-ZMAIL-GLOBAL (VAR &OPTIONAL (INITIAL-VALUE NIL IVP))
  `(PROGN 'COMPILE
     (DEFVAR ,VAR)
     ,(AND IVP `(PUSH (CONS ',VAR ,INITIAL-VALUE) *ZMAIL-GLOBAL-INITIALIZATION-LIST*))))

(DEFVAR *ZMAIL-WHO-LINE-DOCUMENTATION-SYMBOLS* NIL)
(DEFMACRO DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER (COMMAND ARGLIST &BODY BODY)
  `(PROGN 'COMPILE
     (PUSH* ',COMMAND *ZMAIL-WHO-LINE-DOCUMENTATION-SYMBOLS*)
     (DEFUN (,COMMAND WHO-LINE-DOCUMENTATION-UPDATER) ,ARGLIST
       . ,BODY)))

(DEFMACRO DEFINE-COMMAND-WHO-LINE-DOCUMENTATION (COMMAND STRING)
  `(PUTPROP ',COMMAND ,STRING ':WHO-LINE-DOCUMENTATION))

(DEFVAR *OPTIONS-NOT-IN-ALIST* NIL)
(DEFMACRO ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION (OPTION COMMAND)
  `(PROGN 'COMPILE
     (PUSH ',COMMAND (GET ',OPTION 'DOCUMENTATION-ASSOCIATED-COMMANDS))
     (OR (ASSQ ',OPTION *ZMAIL-USER-OPTION-ALIST*)
	 (PUSH ',OPTION *OPTIONS-NOT-IN-ALIST*))))

;;; Random variables
(DEFVAR *ZMAIL-PATHNAME-DEFAULTS*)		;For pathname parsing
(DEFVAR *ZMAIL-COMTAB*)				;Main keyboard comtab
(DEFVAR *MSG-COMTAB*)				;COMTAB in the message window
(DEFVAR *MSG-CONTROL-X-COMTAB*)			;C-X comtab in message window
(DEFVAR *REPLY-COMTAB*)				;COMTAB in the sending window
(DEFVAR *REPLY-CONTROL-X-COMTAB*)		;ditto
(DEFVAR *ZMAIL-COMMAND-BUTTON*)			;Extended commands
(DEFVAR *ZMAIL-BACKGROUND-P* NIL)		;T if within background process
(DEFVAR *MY-ADDRESS*)				;String of network address
(DEFINE-ZMAIL-GLOBAL *KEYWORD-ALIST* NIL)	;Currently defined keywords
(DEFINE-ZMAIL-GLOBAL *USER-FILTER-ALIST* NIL)	;Currently defined user filters
(DEFINE-ZMAIL-GLOBAL *UNIVERSE-LIST* NIL)	;and user defined mapping for universe

;;; User options
(DEFINE-USER-OPTION-ALIST *ZMAIL-USER-OPTION-ALIST* DEFINE-ZMAIL-USER-OPTION)

;;; These are user options, in that they are automatically written into the file,
;;; but they are modified by special means
(DEFINE-ZMAIL-USER-OPTION *OTHER-MAIL-FILE-NAMES* NIL :PATHNAME-LIST)
(TV:RESTRICT-USER-OPTION *OTHER-MAIL-FILE-NAMES* :NEVER)
(DEFINE-ZMAIL-USER-OPTION *FILTER-KEYWORDS-ALIST* NIL :SEXP)
(TV:RESTRICT-USER-OPTION *FILTER-KEYWORDS-ALIST* :NEVER)
(DEFINE-ZMAIL-USER-OPTION *FILTER-MOVE-MAIL-FILE-ALIST* NIL :SEXP)
(TV:RESTRICT-USER-OPTION *FILTER-MOVE-MAIL-FILE-ALIST* :NEVER)
(DEFINE-ZMAIL-USER-OPTION *FILTER-REFERENCE-UNIVERSE-ALIST* NIL :SEXP)
(TV:RESTRICT-USER-OPTION *FILTER-REFERENCE-UNIVERSE-ALIST* :NEVER)

;;; Real user options
(DEFINE-ZMAIL-USER-OPTION *FILTER-SUMMARY-WINDOW-FRACTION* NIL :NUMBER-OR-NIL
			  "Fraction of the frame occupied by the summary in filter mode")
(DEFINE-ZMAIL-USER-OPTION *SUMMARY-WINDOW-FRACTION* 0.45s0 :NUMBER
			  "Fraction of the frame occupied by the summary")
(DEFINE-ZMAIL-USER-OPTION *SUMMARY-SCROLL-FRACTION* 0.2s0 :NUMBER
			  "Amount by which to glitch summary window")
(DEFINE-ZMAIL-USER-OPTION *NEW-MAIL-FILE-APPEND-P* ':STICKY :MENU-ALIST
			  "Appending to new mail files"
			  '(("Append" :VALUE :APPEND
			     :DOCUMENTATION "New mail files append messages.")
			    ("Prepend" :VALUE :PREPEND
			     :DOCUMENTATION "New mail files prepend messages.")
			    ("Sticky" :VALUE :STICKY :DOCUMENTATION
  "New mail files inherit whether they append messages from the current mail file.")
			    ("Ask" :VALUE :ASK :DOCUMENTATION
  "User is always queried when creating a new mail file as to whether it appends messages.")))
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-SUMMARY-TEMPLATE* T :SEXP
			  "Default summary display format")
;;; Old name which may be in some init files
(FORWARD-VALUE-CELL '*SUMMARY-INCLUDE-DATE* '*DEFAULT-SUMMARY-TEMPLATE*)

(DEFINE-ZMAIL-USER-OPTION *GMSGS-OTHER-SWITCHES* "//Z" :STRING
			  "Other switches to supply to GMSGS server")
(TV:RESTRICT-USER-OPTION *GMSGS-OTHER-SWITCHES* :IF :GMSGS)
(DEFINE-ZMAIL-USER-OPTION *RUN-GMSGS-P* ':NO :ASSOC
			  "Run GMSGS before getting new mail"
			  '(("Yes" . :YES) ("No" . :NO) ("Once only" . :ONCE-ONLY)))
(TV:RESTRICT-USER-OPTION *RUN-GMSGS-P* :IF :GMSGS)

(DEFINE-ZMAIL-USER-OPTION *MAIL-FILE-FOR-DRAFTS* NIL :PATHNAME-OR-NIL
			  "Mail file to store drafts in")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-DRAFT-FILE-NAME* NIL :PATHNAME-OR-NIL
			   "Default file for saving draft")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-MOVE-MAIL-FILE-NAME* NIL :PATHNAME-OR-NIL
			  "Default file for moving to a new file")
(DEFINE-ZMAIL-USER-OPTION *MOVE-FILE-NAME-STICKY-FN2* T :BOOLEAN
			  "Take file type for moving to a new file from default")
(DEFINE-ZMAIL-USER-OPTION *TEXT-MAIL-FILE-SEPARATOR* "" :STRING
			  "Line between messages in text mail file")
(DEFINE-ZMAIL-USER-OPTION *ZMAIL-STARTUP-FILE-NAME* NIL :PATHNAME-OR-NIL
			  "File read in at startup")
(DEFINE-ZMAIL-USER-OPTION *FORWARDED-MESSAGE-END* "" :STRING
			  "Format line after forwarded messages")
(DEFINE-ZMAIL-USER-OPTION *FORWARDED-MESSAGE-SEPARATOR* "" :STRING
			  "Format line between forwarded messages")
(DEFINE-ZMAIL-USER-OPTION *FORWARDED-MESSAGE-BEGIN* "" :STRING
			  "Format line before forwarded messages")
(DEFINE-ZMAIL-USER-OPTION *DONT-REPLY-TO* '("INFO-*") :STRING-LIST  "People not to reply to")

(DEFINE-ZMAIL-USER-OPTION *MIDDLE-REPLY-MODE* ':SENDER :MENU-ALIST
			  "Default reply to for middle button"
		          (CAR *REPLY-MODES-ALIST*))
(DEFINE-ZMAIL-USER-OPTION *MIDDLE-REPLY-WINDOW-MODE* ':TWO-WINDOWS :MENU-ALIST
			  "Default reply window setup for middle button"
			  (CADR *REPLY-MODES-ALIST*)) 
(DEFINE-ZMAIL-USER-OPTION *1R-REPLY-MODE* ':SENDER :MENU-ALIST
			  "Default reply with argument of 1 to"
			  (CAR *REPLY-MODES-ALIST*))
(DEFINE-ZMAIL-USER-OPTION *REPLY-MODE* ':ALL :MENU-ALIST
			  "Default reply to"
		          (CAR *REPLY-MODES-ALIST*))
(DEFINE-ZMAIL-USER-OPTION *REPLY-WINDOW-MODE* ':TWO-WINDOWS :MENU-ALIST
			  "Default reply window setup"
			  (CADR *REPLY-MODES-ALIST*))
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-MAIL-WINDOW-CONFIGURATION* ':SEND :MENU-ALIST
			  "Default window configuration when mailing"
			  `(("Send" :VALUE :SEND
			     :DOCUMENTATION "Headers and Reply windows.")
			    . ,(CDR *WINDOW-CONFIGURATION-ALIST*)))
(DEFINE-ZMAIL-USER-OPTION *SEND-HEADER-FORMAT* ':INCLUDE-PERSONAL :MENU-ALIST
			  "Format of headers sent (except via COMSAT)"
			  *HEADER-FORMAT-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *REPLY-HEADER-FORMAT* ':SHORT :MENU-ALIST
			  "Format of headers inserted for reply"
			  *HEADER-FORMAT-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-HEADER-FORCE* ':NONE :MENU-ALIST
			  "Default header force (via COMSAT)"
			  *HEADER-FORCE-ALIST*)
(TV:RESTRICT-USER-OPTION *DEFAULT-HEADER-FORCE* :IF :COMSAT)
(DEFINE-ZMAIL-USER-OPTION *LOCAL-MAIL-HEADER-FORCE* ':ITS :MENU-ALIST
			  "Header force for local messages" *HEADER-FORCE-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *LOCAL-MAIL-INCLUDE-SUBJECT* T :BOOLEAN
			  "Local mail starts out with a subject")
(DEFINE-SITE-ALIST-USER-OPTION (*MAIL-SENDING-MODE* *ZMAIL-USER-OPTION-ALIST*)
			       "Mail sending mode" *MAIL-SENDING-MODE-ALIST*
			       :DEFAULT-MAIL-MODE)
(DEFINE-ZMAIL-USER-OPTION *DELETE-EXPIRED-MSGS* ':PER-FILE :MENU-ALIST
			  "Automatically delete expired messages"
			  `(,@*YES-NO-ASK-ALIST*
			    ("Per file" . :PER-FILE)))
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-FCC-LIST* NIL :PATHNAME-LIST
			  "Default initial Fcc list")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-CC-LIST* NIL :ADDRESS-LIST
			  "Default initial Cc list")
(DEFINE-ZMAIL-USER-OPTION *REQUIRE-SUBJECTS* NIL :MENU-ALIST
			  "Require subjects on outgoing messages"
			  *REQUIRE-SUBJECTS-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *GENERATE-IN-REPLY-TO-FIELD* NIL :BOOLEAN
			  "Automatically generate In-reply-to fields")

(DEFINE-ZMAIL-USER-OPTION *SUMMARY-MOUSE-MIDDLE-MODE* ':DELETE-OR-UNDELETE :MENU-ALIST
			  "Middle button on summary window"
			  *SUMMARY-MOUSE-MIDDLE-MENU-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *NEXT-MIDDLE-MODE* ':LAST-UNDELETED :MENU-ALIST
			  "Middle button on Next command"
			  *MOVE-TO-NEXT-MENU-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *PREVIOUS-MIDDLE-MODE* ':FIRST-UNDELETED :MENU-ALIST
			  "Middle button on Previous command"
			  *MOVE-TO-PREVIOUS-MENU-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *MAP-MIDDLE-MODE* NIL :ASSOC
			  "Middle button on Map command"
			  *ZMAIL-MAP-COMMAND-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *MAIL-MIDDLE-MODE* ':BUG :MENU-ALIST
			  "Middle button on Mail command"
			  *ZMAIL-MAIL-MENU-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-INITIAL-WINDOW-CONFIGURATION* ':BOTH :MENU-ALIST
			  "Default startup window setup"
			  *WINDOW-CONFIGURATION-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *DELETE-MIDDLE-MODE* ':BACKWARD :MENU-ALIST
			  "Direction to move for click middle on delete"
			  *DELETE-DIRECTION-ALIST*)
(DEFINE-ZMAIL-USER-OPTION  *NEXT-AFTER-DELETE* ':FORWARD :MENU-ALIST
			   "Direction to move after delete"
			   *DELETE-DIRECTION-ALIST*)

(DEFINE-ZMAIL-USER-OPTION *INHIBIT-BACKGROUND-MAIL-CHECKS* NIL :BOOLEAN
			  "Do not check for new mail in the background")
(DEFINE-ZMAIL-USER-OPTION *INHIBIT-BACKGROUND-SAVES* NIL :BOOLEAN
			  "Do not automatically save after get new mail")
(DEFINE-ZMAIL-USER-OPTION *ONE-WINDOW-AFTER-YANK* T :BOOLEAN
			  "Just show headers and text after yanking in message")
(DEFINE-ZMAIL-USER-OPTION *ALWAYS-JUMP-AFTER-GET-NEW-MAIL* NIL :BOOLEAN
			  "Move to first message even when no new mail")
(DEFINE-ZMAIL-USER-OPTION *FORWARDED-ADD-SUBJECT* T :BOOLEAN
			  "Forwarded messages are supplied with a subject")
(DEFINE-ZMAIL-USER-OPTION *QUERY-BEFORE-EXPUNGE* NIL :BOOLEAN
			  "Show headers and ask before expunging deleted messages")
(DEFINE-ZMAIL-USER-OPTION *DELETE-AFTER-MOVE-TO-FILE* T :BOOLEAN
			  "Delete message when moved into file")
