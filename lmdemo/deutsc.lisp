;-*- MODE:LISP; Package:HACKS -*-

(DEFUN MAKE-LIST-ARRAY (LIST)
  (LET ((A (MAKE-ARRAY NIL 'ART-Q (LENGTH LIST))))
    (FILLARRAY A LIST)
    A))

(DEFCONST GERMAN-SMALL (MAKE-LIST-ARRAY '("ein" "zwei" "drei" "vier" "fuenf" "sechs"
					  "sieben" "acht" "neun" "zehn" "elf" "zwoelf"
					  "dreizehn" "veirzehn" "fuenfzehn" "sechszehn"
					  "siebzehn" "achtzehn" "neunzehn")))

(DEFCONST GERMAN-MEDIUM (MAKE-LIST-ARRAY '("zwanzig" "dreissig" "vierzig" "fuenfzig" "sechsig"
					   "siebzig" "achtzig" "neunzig")))

(DEFCONST GERMAN-LARGE (MAKE-LIST-ARRAY '("tausand" "Million" "Milliard" "Billion" "Billiard"
					  "Trillion" "Trilliard" "Quadrillion")))

(DEFCONST GERMAN-ORDINAL-SMALL (MAKE-LIST-ARRAY '("erste" "zwitte" "dritte" "vierte"
						  "fuenfste" "sechste" "siebenste" "achtste"
						  "neunte" "zehnte" "elfte" "zwoelfte"
						  "dreizehnte" "veirzehnte" "fuenfzehnte"
						  "sechszehnte" "siebzehnte" "achtzehnte"
						  "neunzehnte")))

(DEFUN GERMAN-PRINT-THOUSAND (N STREAM ORDINAL-P)
  (LET ((N (\ N 100.))
	(H (// N 100.)))
    (COND ((> H 0)
	   (FUNCALL STREAM ':STRING-OUT (AREF GERMAN-SMALL (1- H)))
	   (FUNCALL STREAM ':STRING-OUT "hundert")
	   (AND ORDINAL-P (ZEROP N)
		(FUNCALL STREAM ':TYO #/e))))
    (COND ((= N 0))
	  ((< N 20.)
	   (FUNCALL STREAM ':STRING-OUT (AREF (IF ORDINAL-P GERMAN-ORDINAL-SMALL GERMAN-SMALL)
					      (1- N))))
	  (T
	   (COND ((PLUSP (SETQ H (\ N 10.)))
		  (FUNCALL STREAM ':STRING-OUT (AREF GERMAN-SMALL (1- H)))
		  (AND (= H 1) (FUNCALL STREAM ':TYO #/s))	;Handle einsundzwanzig
		  (FUNCALL STREAM ':STRING-OUT "und")))
	   (FUNCALL STREAM ':STRING-OUT (AREF GERMAN-MEDIUM (- (// N 10.) 2)))
	   (AND ORDINAL-P (FUNCALL STREAM ':STRING-OUT "ste"))))))

(DEFUN GERMAN-PRINT (N &OPTIONAL (STREAM STANDARD-OUTPUT) (EINS-P T) ORDINAL-P)
  (COND ((ZEROP N)
	 (FUNCALL STREAM ':STRING-OUT (IF ORDINAL-P "nullte" "zero")))
	((< N 0)
	 (FUNCALL STREAM ':STRING-OUT "minus")
	 (FUNCALL STREAM ':TYO #\space)
	 (GERMAN-PRINT (MINUS N) STREAM))
	(T
	 (DO ((N N)
	      (P)
	      (FLAG)
	      (LIMIT (^ 10. 24.) (// LIMIT 1000.))
	      (I 7 (1- I)))
	     ((< I 0)
	      (COND ((> N 0)
		     (AND FLAG (FUNCALL STREAM ':TYO #\space))
		     (GERMAN-PRINT-THOUSAND N STREAM ORDINAL-P)
		     (AND (= N 1) EINS-P (FUNCALL STREAM ':TYO #/s)))
		    ((AND ORDINAL-P FLAG)
		     (FUNCALL STREAM ':STRING-OUT "te"))))
	   (COND ((NOT (< N LIMIT))
		  (SETQ P (// N LIMIT)
			N (\ N LIMIT))
		  (COND (FLAG (FUNCALL STREAM ':TYO #\space))
			(T (SETQ FLAG T)))
		  (GERMAN-PRINT P STREAM NIL)
		  (COND (( I 1)
			 (AND (= P 1)		;Past 1M are feminine
			      (FUNCALL STREAM ':TYO #/e))
			 (FUNCALL STREAM ':TYO #\space))
			(T
			 (SETQ FLAG NIL)))
		  (FUNCALL STREAM ':STRING-OUT (AREF GERMAN-LARGE I))))))))

(DEFPROP :GERMAN GERMAN-PRINC SI:PRINC-FUNCTION)

(DEFUN GERMAN-PRINC (N STREAM)
  (COND ((GET-HANDLER-FOR STREAM ':SET-FONT-MAP)
	 (LET ((OLD-FONT-MAP (FUNCALL STREAM ':FONT-MAP))
	       (OLD-FONT (FUNCALL STREAM ':CURRENT-FONT)))
	   (UNWIND-PROTECT
	     (PROGN
	       (FUNCALL STREAM ':SET-FONT-MAP '(FONTS:S35GER))
	       (FUNCALL STREAM ':SET-CURRENT-FONT 0)
	       (MULTIPLE-VALUE-BIND (X Y)
		   (FUNCALL STREAM ':READ-CURSORPOS)
		 (FUNCALL STREAM ':SET-CURSORPOS X (MAX Y 40.)))
	       (GERMAN-PRINT (IF (BIGP N) N (- N)) STREAM)
	       (TERPRI STREAM))
	     (FUNCALL STREAM ':SET-FONT-MAP OLD-FONT-MAP)
	     (FUNCALL STREAM ':SET-CURRENT-FONT OLD-FONT))))
	(T
	 (GERMAN-PRINT (IF (BIGP N) N (- N)) STREAM))))

(DEFPROP :ASK ASK-PRINC SI:PRINC-FUNCTION)

(DEFUN ASK-PRINC (N STREAM)
  (LET ((BASE (OR (TV:MENU-CHOOSE '(("Decimal" . 10.)
				    ("Octal" . 8.)
				    ("Binary" . 2.)
				    ("Roman" . :ROMAN)
				    ("Roman Old" . :ROMAN-OLD)
				    ("English" . :ENGLISH)
				    ("German" . :GERMAN)))
		  10.)))
    (PRINC (- N) STREAM)))

(DEFCONST GERMAN-QUARTERS (MAKE-LIST-ARRAY '("" "viertal " "halb " "dreiviertal ")))

(DEFUN GERMAN-PRINT-TIME (HOURS MINUTES &OPTIONAL (STREAM STANDARD-OUTPUT))
  (LET ((QUARTER (// MINUTES 15.))
	(MINUTES (\ MINUTES 15.))
	(BEFORE-P))
    (IF	(OR (> MINUTES 10.)
	    (AND (> MINUTES 5) (= (\ QUARTER 2) 1)))
	(SETQ QUARTER (1+ QUARTER)
	      BEFORE-P T))
    (IF ( QUARTER 0)
	(SETQ HOURS (1+ HOURS)))
    (IF ( MINUTES 0)
	(FORMAT STREAM "~A ~:[nach~;vor~] "
		(AREF GERMAN-SMALL (IF BEFORE-P
				       (- 14. MINUTES)
				       (1- MINUTES)))
		BEFORE-P))
    (FORMAT STREAM "~A" (AREF GERMAN-QUARTERS (\ QUARTER 4)))
    (IF (= (SETQ HOURS (\ HOURS 24.)) 0)
	(FORMAT STREAM "mitnacht")
	(GERMAN-PRINT HOURS STREAM NIL))
    ))

(DEFUN WIEVIEL-UHR (&OPTIONAL (STREAM STANDARD-OUTPUT))
  (AND (TIME:UPDATE-TIMEBASE)
       (MULTIPLE-VALUE-BIND (NIL MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
	   (TIME:GET-TIME)
         (FUNCALL STREAM ':STRING-OUT (TIME:DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK ':GERMAN))
	 (FUNCALL STREAM ':STRING-OUT " das ")
	 (GERMAN-PRINT DAY STREAM NIL T)
	 (FUNCALL STREAM ':TYO #\space)
	 (FUNCALL STREAM ':STRING-OUT (TIME:MONTH-STRING MONTH ':GERMAN))
	 (FUNCALL STREAM ':STRING-OUT ", ")
	 (GERMAN-PRINT (+ YEAR 1900.) STREAM NIL)
	 (FORMAT STREAM ";~%")
	 (GERMAN-PRINT-TIME HOURS MINUTES))))
