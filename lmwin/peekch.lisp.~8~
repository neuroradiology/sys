;;; -*- Mode: LISP;  Package: CHAOS;  Base: 8 -*-
;;;	** (c) Copyright 1981 Massachusetts Institute of Technology **
;;; Chaosnet peek functions

(TV:DEFINE-PEEK-MODE PEEK-HOSTAT #/H "Hostat" T)

(DEFUN PEEK-HOSTAT (&REST IGNORE)
  (HOSTAT))

(DEFUN PEEK-CHAOS-PACKET-ITEM (PKT &OPTIONAL (INDENT 0))
  "Returns an item that describes a chaosnet packet.  Mouseable subfields are:
   The host:  Left: Causes info about the host to displayed inferior to the packet.
	      Middle: Causes a static hostat to be displayed inferior to the packet.
  	      Right (menu): Typeout Hostat, Supdup, Telnet, Qsend

Sample output:
Pkt [to ! from] <name> (number){, transmitted <n> times (at <time>)}{, being retransmitted}{, released}{, fowarded <n> times}
    <op> (<number>), <n> bytes, number <n>, acking <n>, source idx <n>, dest idx <n>
    Words from <n>: <wordn> ... <wordn+m>
    String: <string>

Packet: to AI (2026), transmitted 27 times (at 1231232), being retransmitted
 CLS (11), 432 bytes, number 3422, acking 3221, source idx 177777, dest idx 177777
 Words from 0: 123123 12371 1227 272727 272626
 String: 'Now is the time for all good men'

Packet: from MC (1440), released, forwarded 17 times
 DAT (201), 100 bytes, number 432, acking 102, source idx 123451, dest idx 123441
 Words from 0: 123123 64532
 String: 'FUKT!'

"
  (LET ((TO-US (AND (ZEROP (PKT-TIMES-TRANSMITTED PKT))
		    (= (PKT-DEST-ADDRESS PKT) MY-ADDRESS)))
	(OTHER-HOST))
    (SETQ OTHER-HOST (IF TO-US
			 (PKT-SOURCE-ADDRESS PKT)
			 (PKT-DEST-ADDRESS PKT)))
    (LIST ()
      (LIST '(:PRE-PROCESS-FUNCTION PEEK-CHAOS-PACKET-INSERT-HOSTAT)
	(TV:SCROLL-PARSE-ITEM
	  ':LEADER 4
	  `(:MOUSE-ITEM (NIL :EVAL (PEEK-CHAOS-HOST-MENU ',OTHER-HOST 'TV:ITEM 0 ,INDENT)
			     :DOCUMENTATION "Menu of useful things to do to this host.")
	    :STRING ,(FORMAT NIL "~VXPacket ~:[to~;from~] ~@[~A ~](~O)"
			     INDENT TO-US
			     (SI:GET-HOST-FROM-ADDRESS OTHER-HOST ':CHAOS) OTHER-HOST))
	  (AND (NOT TO-US)
	       `(:FUNCTION ,#'PKT-TIMES-TRANSMITTED (,PKT)
			   NIL (", transmitted ~D times")))
	  (AND (NOT TO-US)
	       `(:FUNCTION ,#'PKT-TIME-TRANSMITTED (,PKT) NIL (" (at ~O)")))
	  (AND (NOT TO-US)
	       `(:FUNCTION ,#'PKT-BEING-RETRANSMITTED (,PKT)
			   NIL ("~:[, being retransmitted~;~]")))
	  `(:FUNCTION ,#'PKT-STATUS (,PKT) NIL ("~:[~;, Status: ~0G~A~]"))
	  (AND TO-US
	       (FORMAT NIL ", fowarded ~D times" (PKT-FWD-COUNT PKT)))))

      ;; Second line
      (LET ((OP (PKT-OPCODE PKT)))
       (TV:SCROLL-PARSE-ITEM
	(FORMAT NIL
		"~VX~A (~O), ~O bytes, number ~O, acking ~O, source idx ~O, dest idx ~O"
		INDENT
		(IF ( OP DAT-OP)
		    "Data"
		    (NTH OP OPCODE-LIST))
		OP
		(PKT-NBYTES PKT)
		(PKT-NUM PKT) (PKT-ACK-NUM PKT)
		(PKT-SOURCE-INDEX-NUM PKT) (PKT-DEST-INDEX-NUM PKT))))
      (TV:SCROLL-PARSE-ITEM (FORMAT NIL "~VX" INDENT) (PEEK-CHAOS-PKT-WORDS PKT 0 6))
      (TV:SCROLL-PARSE-ITEM (FORMAT NIL "~VXString: " INDENT) (PEEK-CHAOS-PKT-STRING PKT)))))

(DEFUN PEEK-CHAOS-PKT-WORDS (PKT START NUMBER &AUX STRING)
  "Returns a string consisting of words from the packet."
  (SETQ STRING (FORMAT NIL "Words from ~O: " START))
  (DO ((I START (1+ I))
       (LEN (ARRAY-DIMENSION-N 1 PKT)))
      ((OR ( I (+ START NUMBER)) ( I LEN))
       STRING)
    (SETQ STRING
	  (STRING-APPEND STRING
			 (FORMAT NIL "~6O" (AREF PKT (+ FIRST-DATA-WORD-IN-PKT I)))
			 " "))))

;;; Boy, is this piece of shit ad hoc!!
(DEFUN PEEK-CHAOS-PKT-STRING (PKT &OPTIONAL COUNT)
  "Returns a 'safe' string as far as the scrolling stuff is concerned"
  (DO ((STRING (MAKE-ARRAY NIL 'ART-STRING 100 NIL '(0)))
       (PKT-STRING (PKT-STRING PKT))
       (CHAR)
       (I 0 (1+ I))
       (LEN (STRING-LENGTH (PKT-STRING PKT))))
      ((OR ( I LEN) (AND COUNT ( I COUNT)))
       STRING)
      (SETQ CHAR (AREF PKT-STRING I))
      (IF (AND (< CHAR 200) ( CHAR #/))
	  (ARRAY-PUSH-EXTEND STRING CHAR)
	  (ARRAY-PUSH-EXTEND STRING #/)
	  (IF ( CHAR #/)
	      (ARRAY-PUSH-EXTEND STRING (LOGIOR 100 (LOGAND CHAR 77)))
	      (ARRAY-PUSH-EXTEND STRING #/)))))

(DEFUN PEEK-CHAOS-CONN (CONN)
  "Format is:

Host <name> (<number>), <state>, local idx <n>, foreign idx <n>
Windows: local <n>, foreign <n> (<n> available)
Received: pkt <n> (time <n>), read pkt <n>, ack pkt <n>, <n> queued
Sent: pkt <n>, ack for pkt <n>, <n> queued
"
  (LIST ()
    (LIST '(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-INSERT-HOSTAT)
	  (TV:SCROLL-PARSE-ITEM
	    ':LEADER 3
	    (LOCAL-DECLARE ((SPECIAL PEEK-CHAOS-HOST))
	      (LET ((PEEK-CHAOS-HOST (CONS -1 NIL)))
		`(:MOUSE-ITEM
		   (NIL :EVAL (PEEK-CHAOS-HOST-MENU (CAR ',(LOCF (CAR PEEK-CHAOS-HOST)))
						    'TV:ITEM 0)
			:DOCUMENTATION "Menu of useful things to do to this host.")
		  :FUNCTION ,(CLOSURE '(PEEK-CHAOS-HOST)
			       #'(LAMBDA (CONN)
				   (AND ( (CAR PEEK-CHAOS-HOST)
					   (PROG2 (RPLACA PEEK-CHAOS-HOST
							  (FOREIGN-ADDRESS CONN))
						  (CAR PEEK-CHAOS-HOST)))
					(RPLACD PEEK-CHAOS-HOST
						(FORMAT NIL "Host ~@[~A ~](~O), "
						   (SI:GET-HOST-FROM-ADDRESS
						     (CAR PEEK-CHAOS-HOST) ':CHAOS)
						   (CAR PEEK-CHAOS-HOST))))
				   (CDR PEEK-CHAOS-HOST)))
		  (,CONN) NIL)))
	    `(:FUNCTION ,#'STATE (,CONN) NIL)
	    `(:FUNCTION ,#'LOCAL-INDEX-NUM (,CONN) NIL (", local idx ~O, "))
	    `(:FUNCTION ,#'FOREIGN-INDEX-NUM (,CONN) NIL ("foreign idx ~O"))))
    (TV:SCROLL-PARSE-ITEM
      `(:FUNCTION ,#'LOCAL-WINDOW-SIZE (,CONN) NIL ("Windows: local ~D, "))
      `(:FUNCTION ,#'FOREIGN-WINDOW-SIZE (,CONN) NIL ("foreign ~D, "))
      `(:FUNCTION ,#'WINDOW-AVAILABLE (,CONN) NIL ("(~D available)")))
    (LIST `(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-RECEIVED-PKTS :CONNECTION ,CONN)
      (TV:SCROLL-PARSE-ITEM
	':LEADER 1
	':MOUSE-SELF '(NIL :EVAL (TV:PEEK-MOUSE-CLICK 'SELF 0)
			   :DOCUMENTATION
			   "Insert//remove display of packets on receive list.")
	`(:FUNCTION ,#'PKT-NUM-RECEIVED (,CONN) NIL ("Received: pkt ~O"))
	`(:FUNCTION ,#'TIME-LAST-RECEIVED (,CONN) NIL (" (time ~O), "))
	`(:FUNCTION ,#'PKT-NUM-READ (,CONN) NIL ("read pkt ~O, "))
	`(:FUNCTION ,#'PKT-NUM-ACKED (,CONN) NIL ("ack pkt ~O, "))
	`(:FUNCTION ,#'(LAMBDA (CONN)
			 (- (PKT-NUM-RECEIVED CONN) (PKT-NUM-READ CONN)))
		    (,CONN) NIL ("~D queued"))))
    (LIST `(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-SEND-PKTS :CONNECTION ,CONN)
      (TV:SCROLL-PARSE-ITEM
	':LEADER 1
	':MOUSE-SELF '(NIL :EVAL (TV:PEEK-MOUSE-CLICK 'SELF 0)
			   :DOCUMENTATION
			   "Insert//remove display of packets on transmit list.")
	`(:FUNCTION ,#'PKT-NUM-SENT (,CONN) NIL ("Sent: pkt ~O, "))
	`(:FUNCTION ,#'SEND-PKT-ACKED (,CONN) NIL ("ack for pkt ~O, "))
	`(:FUNCTION ,#'SEND-PKTS-LENGTH (,CONN) NIL ("~D queued"))))
    (TV:SCROLL-PARSE-ITEM "")))

(TV:DEFINE-PEEK-MODE PEEK-CHAOS #/C "Chaosnet Connections")

(DEFUN PEEK-CHAOS (IGNORE)
  "Displays state of all chaos net connections"
  (LIST NIL
	(TV:SCROLL-PARSE-ITEM
	  "Chaos connections at "
	  `(:FUNCTION ,#'TIME () NIL ("~O")))
	(TV:SCROLL-PARSE-ITEM "")
	(TV:SCROLL-MAINTAIN-LIST #'(LAMBDA () CONN-LIST)
			      #'PEEK-CHAOS-CONN)
	(TV:SCROLL-PARSE-ITEM "Interesting meters")
	(TV:SCROLL-MAINTAIN-LIST #'(LAMBDA () PEEK-A-BOO-LIST)
			      #'(LAMBDA (COUNTER)
				  (TV:SCROLL-PARSE-ITEM
				    `(:STRING ,(STRING COUNTER) 35.)
				    `(:FUNCTION SYMEVAL (,COUNTER) NIL ("~@15A" 10. T)))))
	(TV:SCROLL-PARSE-ITEM '(:STRING "%COUNT-CHAOS-TRANSMIT-ABORTS" 35.)
			   '(:FUNCTION READ-METER (SYS:%COUNT-CHAOS-TRANSMIT-ABORTS) NIL
				       ("~@15A" 10. T)))))

(DEFUN PEEK-CHAOS-HOST-MENU (&REST ARGS)
  (LEXPR-FUNCALL #'PROCESS-RUN-FUNCTION "Peek Chaos Menu"
		 SELF ':FUNCALL-INSIDE-YOURSELF #'PEEK-CHAOS-HOST-MENU-INTERNAL ARGS))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (TV:BASIC-PEEK)
(DEFUN PEEK-CHAOS-HOST-MENU-INTERNAL (HOST ITEM &OPTIONAL (OFFSET 0) &REST ADDITIONAL-STUFF)
  "Menu for interesting operations on hosts in a peek chaos display"
  (LET ((CHOICE (TV:MENU-CHOOSE
		  '(("Hostat One" :VALUE HOSTAT-ONE
		     :DOCUMENTATION "Show Hostat for selected host in typeout window.")
		    ("Hostat All" :VALUE HOSTAT-ALL
		     :DOCUMENTATION "Show Hostat for all hosts in typeout window.")
		    ("Insert Hostat" :VALUE HOSTAT-INSERT
		     :DOCUMENTATION "Insert static Hostat for selected host in the display.")
		    ("Remove Hostat" :VALUE HOSTAT-REMOVE
		     :DOCUMENTATION "Remove static Hostat.")
		    ("Supdup" :VALUE HOSTAT-SUPDUP :DOCUMENTATION "SUPDUP to selected host.")
		    ("Telnet" :VALUE HOSTAT-TELNET :DOCUMENTATION "TELNET to selected host.")
		    ("Qsend" :VALUE HOSTAT-QSEND
		     :DOCUMENTATION "Send a message to user on selected host."))))
	(TERMINAL-IO TV:TYPEOUT-WINDOW))
    (SELECTQ CHOICE
      (HOSTAT-ONE (HOSTAT HOST))
      (HOSTAT-ALL (HOSTAT))
      ((HOSTAT-INSERT HOSTAT-REMOVE)
       (SETF (ARRAY-LEADER ITEM (+ TV:SCROLL-ITEM-LEADER-OFFSET OFFSET))
	     (EQ CHOICE 'HOSTAT-INSERT))
       (SETF (ARRAY-LEADER ITEM (+ TV:SCROLL-ITEM-LEADER-OFFSET OFFSET 1)) HOST)
       (DOTIMES (I (LENGTH ADDITIONAL-STUFF))
	 (SETF (ARRAY-LEADER ITEM (+ TV:SCROLL-ITEM-LEADER-OFFSET OFFSET I 2))
	       (NTH I ADDITIONAL-STUFF)))
       (SETQ TV:NEEDS-REDISPLAY T))
      (HOSTAT-SUPDUP (FUNCALL-SELF ':FORCE-KBD-INPUT `(SUPDUP ,HOST)))
      (HOSTAT-TELNET (FUNCALL-SELF ':FORCE-KBD-INPUT `(TELNET ,HOST)))
      (HOSTAT-QSEND (FUNCALL-SELF ':FORCE-KBD-INPUT `(QSEND ,HOST)))
      (NIL)
      (OTHERWISE (BEEP))))))

(DEFUN PEEK-CHAOS-CONN-INSERT-HOSTAT (ITEM &AUX HOST)
  "A pre-process function to insert/remove a hostat from the display."
  (COND ((ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM)) TV:SCROLL-ITEM-LEADER-OFFSET)
	 ;; Want a hostat, make sure it's there and for the right host
	 (IF (AND (EQ (SETQ HOST (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM))
					       (1+ TV:SCROLL-ITEM-LEADER-OFFSET)))
		      (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM))
				    (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)))
		  (CDDR ITEM))
	     NIL
	     (RPLACD (CDR ITEM)
		     (PEEK-CHAOS-HOSTAT HOST 1))
	     (SETF (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM))
				 (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)) HOST)))
	(T (RPLACD (CDR ITEM) NIL)
	   (SETF (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM))
			       (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)) NIL))))

(DEFUN PEEK-CHAOS-PACKET-INSERT-HOSTAT (ITEM &AUX HOST SI)
  "A pre-process function to insert/remove a hostat from the display."
  (COND ((ARRAY-LEADER (SETQ SI (FIRST (TV:SCROLL-ITEMS ITEM))) TV:SCROLL-ITEM-LEADER-OFFSET)
	 ;; Want a hostat, make sure it's there and for the right host
	 (IF (AND (EQ (SETQ HOST (ARRAY-LEADER SI (1+ TV:SCROLL-ITEM-LEADER-OFFSET)))
		      (ARRAY-LEADER SI (+ TV:SCROLL-ITEM-LEADER-OFFSET 3)))
		  (CDDR ITEM))
	     NIL
	     (RPLACD (CDR ITEM)
		     (PEEK-CHAOS-HOSTAT HOST
					(1+ (ARRAY-LEADER SI
							  (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)))))
	     (SETF (ARRAY-LEADER SI (+ TV:SCROLL-ITEM-LEADER-OFFSET 3)) HOST)))
	(T (RPLACD (CDR ITEM) NIL)
	   (SETF (ARRAY-LEADER SI (+ TV:SCROLL-ITEM-LEADER-OFFSET 3)) NIL))))

(DEFVAR *PEEK-HOSTAT-LIST*)
(DEFVAR *PEEK-HOSTAT-STRING*)
(DEFVAR *PEEK-HOSTAT-INDENT*)

(DEFUN PEEK-CHAOS-HOSTAT (HOST *PEEK-HOSTAT-INDENT* &OPTIONAL PKT
			  &AUX (*PEEK-HOSTAT-LIST* NIL) (*PEEK-HOSTAT-STRING* NIL))
  (COND ((OR PKT (SETQ PKT (GET-HOST-STATUS-PACKET HOST)))
	 (PEEK-HOSTAT-STREAM ':TYO #\CR)
	 (HOSTAT-HEADING 'PEEK-HOSTAT-STREAM)
	 (HOSTAT-FORMAT-ANS (PKT-SOURCE-ADDRESS PKT) PKT 'PEEK-HOSTAT-STREAM)
	 ;; Parse the strings into scroll items, removing any blank lines
	 (SETQ *PEEK-HOSTAT-LIST* (NREVERSE *PEEK-HOSTAT-LIST*))
	 (DO ((L *PEEK-HOSTAT-LIST* (CDR L)))
	     ((NULL L) (LIST* () *PEEK-HOSTAT-LIST*))
	   (IF (STRING-SEARCH-NOT-CHAR #/  (CAR L))
	       (RPLACA L (TV:SCROLL-PARSE-ITEM (CAR L)))
	       (SETQ *PEEK-HOSTAT-LIST* (DELQ (CAR L) *PEEK-HOSTAT-LIST*)))))
	(T (NCONS (TV:SCROLL-PARSE-ITEM "Host data unavailable")))))

(DEFUN PEEK-HOSTAT-STREAM (OP &OPTIONAL ARG1 &REST REST)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:TYO :READ-CURSORPOS :SET-CURSORPOS))
    (:TYO
     (COND ((= ARG1 #\CR)
	    (AND *PEEK-HOSTAT-STRING*
		 (PUSH *PEEK-HOSTAT-STRING* *PEEK-HOSTAT-LIST*))
	    (SETQ *PEEK-HOSTAT-STRING* (MAKE-ARRAY NIL 'ART-STRING 50. NIL '(0)))
	    (PEEK-HOSTAT-STREAM ':SET-CURSORPOS *PEEK-HOSTAT-INDENT*))
	   (T
	    (ARRAY-PUSH-EXTEND *PEEK-HOSTAT-STRING* ARG1))))
    (:READ-CURSORPOS (STRING-LENGTH *PEEK-HOSTAT-STRING*))
    (:SET-CURSORPOS
     (LET ((SPACES (- ARG1 (STRING-LENGTH *PEEK-HOSTAT-STRING*))))
       (AND (> SPACES 0)
	    (DOTIMES (I SPACES) (PEEK-HOSTAT-STREAM ':TYO #/ )))))	      
    (T (STREAM-DEFAULT-HANDLER 'PEEK-HOSTAT-STREAM OP ARG1 REST))))

(DEFUN PEEK-CHAOS-CONN-RECEIVED-PKTS (ITEM &OPTIONAL (INDENT 0) &AUX CONN)
  "Show/unshow the received pkts of the connection"
  (OR (SETQ CONN (GET (LOCF (TV:SCROLL-FLAGS ITEM)) ':CONNECTION))
      (FERROR NIL "~S has no associated connection, can't display packets." ITEM))
  (COND ((NOT (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM)) TV:SCROLL-ITEM-LEADER-OFFSET))
	 ;; Want to leave state alone
	 )
	((CDR (TV:SCROLL-ITEMS ITEM))
	 ;; Remove display
	 (RPLACD (TV:SCROLL-ITEMS ITEM) NIL))
	(T
	 ;; Add display
	 (RPLACD (TV:SCROLL-ITEMS ITEM)
		 (NCONS
		   (TV:SCROLL-MAINTAIN-LIST `(LAMBDA () ',(READ-PKTS CONN))
					 `(LAMBDA (X)
					    (PEEK-CHAOS-PACKET-ITEM X ,(+ INDENT 2)))
					 NIL
					 #'(LAMBDA (STATE)
					     (PROG ()
					       (RETURN STATE (PKT-LINK STATE)
						       (NULL (PKT-LINK STATE))))))))))
  (SETF (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM)) TV:SCROLL-ITEM-LEADER-OFFSET) NIL))

(DEFUN PEEK-CHAOS-CONN-SEND-PKTS (ITEM &OPTIONAL (INDENT 0) &AUX CONN)
  "Show/unshow the send pkts of the connection"
  (OR (SETQ CONN (GET (LOCF (TV:SCROLL-FLAGS ITEM)) ':CONNECTION))
      (FERROR NIL "~S has no associated connection, can't display packets." ITEM))
  (COND ((NOT (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM)) TV:SCROLL-ITEM-LEADER-OFFSET))
	 ;; Want to leave state alone
	 )
	((CDR (TV:SCROLL-ITEMS ITEM))
	 ;; Remove display
	 (RPLACD (TV:SCROLL-ITEMS ITEM) NIL))
	(T
	 ;; Add display
	 (RPLACD (TV:SCROLL-ITEMS ITEM)
		 (NCONS
		   (TV:SCROLL-MAINTAIN-LIST `(LAMBDA () (SEND-PKTS ',CONN))
					 `(LAMBDA (X)
					    (PEEK-CHAOS-PACKET-ITEM X ,(+ INDENT 2)))
					 NIL
					 #'(LAMBDA (STATE)
					     (PROG ()
					       (RETURN STATE (PKT-LINK STATE)
						       (NULL (PKT-LINK STATE))))))))))
    (SETF (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM)) TV:SCROLL-ITEM-LEADER-OFFSET) NIL))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (HOST-CHAOS-MIXIN)
(DEFUN HOST-CHAOS-PEEK-FILE-SYSTEM-HEADER (IGNORE)
  (LIST '(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-INSERT-HOSTAT)
	(TV:SCROLL-PARSE-ITEM
	  ':LEADER 3
	  `(:MOUSE-ITEM
	     (NIL :EVAL (PEEK-CHAOS-HOST-MENU ',SELF 'TV:ITEM 0)
		  :DOCUMENTATION "Menu of useful things to do to this host.")
	     :STRING ,(FORMAT NIL "Host ~A" SELF))))))
