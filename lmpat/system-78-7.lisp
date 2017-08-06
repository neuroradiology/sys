;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.7
;;; Reason: Make format ~:C work with things such as #\hyper-super-meta-ctrl-mouse-1-1
;;; Written 12/10/81 13:12:47 by ALR,
;;; while running on Lisp Machine Nineteen from band 1
;;; with System 78.5, ZMail 38.0, microcode 836.



; From file FORMAT > LMIO; AI:
#8R FORMAT:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "FORMAT")))

(DEFUN FORMAT-CTL-CHARACTER (ARG IGNORE &AUX CHNAME BITS)
    (SETQ ARG (CHARACTER ARG))
    (COND ((LDB-TEST %%KBD-MOUSE ARG)
	   (COND ((AND (NOT COLON-FLAG) ATSIGN-FLAG)
		  (OR (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		      (FORMAT-ERROR "~O unknown mouse character given to ~~@C" ARG))
		  (FUNCALL STANDARD-OUTPUT ':STRING-OUT "#\")
		  (PRIN1 CHNAME))
		 (T (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
		    (AND (BIT-TEST 8 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Hyper-"))
		    (AND (BIT-TEST 4 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Super-"))
		    (AND (BIT-TEST 1 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Control-"))
		    (AND (BIT-TEST 2 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Meta-"))
		    (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Mouse-")
		    (FUNCALL STANDARD-OUTPUT ':STRING-OUT (NTH (LDB 0003 ARG)
							       '("Left" "Middle" "Right")))
		    (IF (SETQ CHNAME (NTH (SETQ BITS (LDB 0303 ARG))
					  '("" "-Twice" "-Thrice")))
			(FUNCALL STANDARD-OUTPUT ':STRING-OUT CHNAME)
			(FUNCALL STANDARD-OUTPUT ':TYO #/-)
			(ENGLISH-PRINT (1+ BITS))
			(FUNCALL STANDARD-OUTPUT ':STRING-OUT "-times")))))
          ((NOT COLON-FLAG)
	   (AND ATSIGN-FLAG (FUNCALL STANDARD-OUTPUT ':TYO #/#))
	   (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
	   (IF (NOT (ZEROP BITS))
	       ;; For efficiency, don't send :string-out message just for null string.
	       (FUNCALL STANDARD-OUTPUT
			':STRING-OUT
			(NTH BITS
			     '("" "C-" "M-" "C-M-"
			       "S-" "C-S-" "M-S-" "C-M-S-"
			       "H-" "C-H-" "M-H-" "C-M-H-"
			       "S-H-" "C-S-H-" "M-S-H-" "C-M-S-H-"))))
	   (COND ((AND ATSIGN-FLAG
		       (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME (LDB %%KBD-CHAR ARG))))
		  (FUNCALL STANDARD-OUTPUT ':TYO #/\)
		  (PRIN1 CHNAME))
		 (T (COND (ATSIGN-FLAG (FUNCALL STANDARD-OUTPUT ':TYO #//))
			  ((MEMQ ARG '(#/ #/ #/ #/ #/ #/))
			   (FUNCALL STANDARD-OUTPUT ':TYO #/)))
		    (FUNCALL STANDARD-OUTPUT ':TYO (LDB %%KBD-CHAR ARG)))))
	  (T
	   (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
	   (AND (BIT-TEST 8 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Hyper-"))
	   (AND (BIT-TEST 4 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Super-"))
	   (AND (BIT-TEST 1 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Control-"))
	   (AND (BIT-TEST 2 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Meta-"))
	   (SETQ ARG (LDB %%KBD-CHAR ARG))
	   (COND ((SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		  (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
		    (LET ((STR (STRING-DOWNCASE CHNAME)))
		      (ASET (CHAR-UPCASE (AREF STR 0)) STR 0)
		      (FUNCALL STANDARD-OUTPUT ':STRING-OUT STR)
		      (RETURN-ARRAY STR)))
		  (AND ATSIGN-FLAG (FORMAT-PRINT-TOP-CHARACTER ARG)))
                 ((AND ATSIGN-FLAG (< ARG 40) ( ARG #/))
		  (FUNCALL STANDARD-OUTPUT ':TYO ARG)
		  (FORMAT-PRINT-TOP-CHARACTER ARG))
                 (T (FUNCALL STANDARD-OUTPUT ':TYO ARG))))))

)
