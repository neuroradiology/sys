;;;-*- Mode:LISP; Package:CADR -*-

;;; Save all files on the object machine
(DEFUN SALVAGE-EDITOR ()
  (PKG-GOTO "CADR")				;Lots of stuff doesn't work otherwise
  (DO ((BUFFER-LIST (CC-MEM-READ (1+ (QF-POINTER (QF-SYMBOL 'ZWEI:*ZMACS-BUFFER-LIST*))))
		    (QF-CDR BUFFER-LIST))
       BUFFER)
      ((CC-Q-NULL BUFFER-LIST))
    (SETQ BUFFER (QF-CAR BUFFER-LIST))
    (AND (LET ((NODE-TICK (QF-AR-1 BUFFER (GET-DEFSTRUCT-INDEX 'ZWEI:NODE-TICK 'AREF)))
	       (BUFFER-TICK (QF-AR-1 BUFFER (GET-DEFSTRUCT-INDEX 'ZWEI:BUFFER-TICK 'AREF))))
	   (AND (= DTP-FIX (LOGLDB %%Q-DATA-TYPE NODE-TICK))
		(= DTP-FIX (LOGLDB %%Q-DATA-TYPE BUFFER-TICK))
		(> (LOGLDB %%Q-POINTER NODE-TICK) (LOGLDB %%Q-POINTER BUFFER-TICK))))
	 (LET ((BUFFER-NAME (WITH-OUTPUT-TO-STRING (CC-OUTPUT-STREAM)
			      (CC-Q-PRINT-STRING
				(QF-AR-1 BUFFER
					 (GET-DEFSTRUCT-INDEX 'ZWEI:BUFFER-NAME 'AREF))))))
	   (AND (FQUERY NIL "Save buffer ~A? " BUFFER-NAME)
		(SALVAGE-INTERVAL BUFFER
				  (IF (NOT (CC-Q-NULL
					     (QF-AR-1 BUFFER
						      (GET-DEFSTRUCT-INDEX
							'ZWEI:BUFFER-FILE-ID 'AREF))))
				      BUFFER-NAME
				    (FORMAT QUERY-IO "~&Write ~A to file: " BUFFER-NAME)
				    (READLINE QUERY-IO))))))))

;;; Write out one file
(DEFUN SALVAGE-INTERVAL (BUFFER FILE-NAME)
  (WITH-OPEN-FILE (CC-OUTPUT-STREAM FILE-NAME '(:OUT))
    (DO ((LINE-NEXT (GET-DEFSTRUCT-INDEX 'ZWEI:LINE-NEXT 'ARRAY-LEADER))       
	 (LINE (QF-CAR (QF-AR-1 BUFFER (GET-DEFSTRUCT-INDEX 'ZWEI:INTERVAL-FIRST-BP 'AREF)))
	       (QF-ARRAY-LEADER LINE LINE-NEXT))
	 (LIMIT (QF-CAR (QF-AR-1 BUFFER (GET-DEFSTRUCT-INDEX 'ZWEI:INTERVAL-LAST-BP 'AREF)))))
	(NIL)
      (SALVAGE-LINE LINE)
      (FUNCALL CC-OUTPUT-STREAM ':TYO #\CR)
      (COND ((= LINE LIMIT)
	     (CLOSE CC-OUTPUT-STREAM)
	     (FORMAT T "~&Written: ~A~%" (FUNCALL CC-OUTPUT-STREAM ':TRUENAME))
	     (RETURN NIL))))))

;;; Figure out the index for array-leader or aref generated by defstruct
(DEFUN GET-DEFSTRUCT-INDEX (SYM TYPE)
  (LET ((DEF (FSYMEVAL SYM)))
    (OR (AND (EQ (CAR DEF) 'NAMED-SUBST)
	     (EQ (CAAR (CDDDR DEF)) TYPE)
	     (CADDR (CADDDR DEF)))
	(FERROR NIL "Unable to get defstruct index for ~S: get help!" SYM))))

(DEFUN SALVAGE-LINE (LINE)
  (SELECT (MASK-FIELD-FROM-FIXNUM %%ARRAY-TYPE-FIELD (CC-MEM-READ (LOGLDB %%Q-POINTER LINE)))
    (ART-STRING
     (LET ((CC-Q-PRINT-STRING-MAXL 177777))
       (CC-Q-PRINT-STRING LINE)))
    (ART-16B
     (QF-ARRAY-SETUP (QF-MAKE-Q (QF-POINTER LINE) DTP-ARRAY-POINTER))
     (DO ((LEN (QF-POINTER (QF-MEM-READ (- QF-ARRAY-HEADER-ADDRESS 2))))
	  (ADR QF-ARRAY-DATA-ORIGIN)
	  (I 0 (1+ I))
	  (CH) (WD)
	  (FONT-FLAG 0)
	  (FNT))
	 (( I LEN)
	  (OR (ZEROP LEN) (ZEROP FONT-FLAG)
	      (FUNCALL CC-OUTPUT-STREAM ':STRING-OUT "0")))
       (COND ((ZEROP (LOGAND 1 I))	;Get next word
	      (SETQ WD (QF-MEM-READ ADR)
		    ADR (1+ ADR))))
       (SETQ CH (LOGAND 177777 WD)
	     WD (CC-SHIFT WD -16.))
       (SETQ FNT (LSH CH -8))
       (COND (( FNT FONT-FLAG)
	      (FUNCALL CC-OUTPUT-STREAM ':TYO #/)
	      (FUNCALL CC-OUTPUT-STREAM ':TYO (+ #/0 FNT))
	      (SETQ FONT-FLAG FNT)))
       (FUNCALL CC-OUTPUT-STREAM ':TYO (LOGAND CH 377))))))
