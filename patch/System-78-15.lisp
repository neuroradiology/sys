;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.15
;;; Reason: PRESS:PRESS-LINE zero divide, overun.
;;; Written 12/14/81 12:22:49 by JerryB,
;;; while running on Lisp Machine Nine from band 7
;;; with System 78.14, ZMail 38.1, microcode 836, 60Hz.



; From file PRESS > LMIO1; AI:
#8R PRESS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "PRESS")))

;;;;Draw a line, using rectangles for straight lines and font for diagonal lines.
;;;    Coordinates in micas of course.

(DEFUN PRESS-LINE (X0 Y0 X1 Y1 &AUX (DX (ABS (- X0 X1))) (DY (ABS (- Y0 Y1))) FONT-NUMBER)
  (PRESS-PUT-PENDING-CHARS)
  (PRESS-MAYBE-NEW-ENTITY)			;This should make DPLT work better
  (COND (PRESS-LINE-USE-SPECIAL-OPCODE
	 (PRESS-SET-CURSOR X0 Y0)
	 (PRESS-ENTITY-BYTE 201)
	 (PRESS-ENTITY-WORD X1)
	 (PRESS-ENTITY-WORD Y1))
	((= X0 X1)				;Vertical line
	 (PRESS-SET-CURSOR (- X0 (// LINE-WIDTH 2)) (MIN Y0 Y1))	;Lower left corner
	 (PRESS-SHOW-RECT LINE-WIDTH DY))
	((= Y0 Y1)				;Horizontal line
	 (PRESS-SET-CURSOR (MIN X0 X1) (- Y0 (// LINE-WIDTH 2)))	;Lower left corner
	 (PRESS-SHOW-RECT DX LINE-WIDTH))
	(T					;Diagonal line, use the font
	 (OR (MEMQ PRESS-LINE-FONT PRESS-FONT-LIST)
	     (SETQ PRESS-FONT-LIST (NCONC PRESS-FONT-LIST (NCONS PRESS-LINE-FONT))))
	 (SETQ FONT-NUMBER (FIND-POSITION-IN-LIST PRESS-LINE-FONT PRESS-FONT-LIST))
	 (AND ( FONT-NUMBER 16.) (FERROR NIL "Maximum of 16 fonts allowed."))
	 (OR (EQ PRESS-CURRENT-FONT PRESS-LINE-FONT)
	     (PRESS-SELECT-FONT FONT-NUMBER))
	 (IF (< X1 X0) (PSETQ X0 X1 Y0 Y1 X1 X0 Y1 Y0))	;(X0,Y0) are left end
	 (PRESS-SET-CURSOR X0 Y0)		;Proceed inevitably toward the right
	 (AND (< Y1 Y0) (SETQ DY (- DY)))
	 ;; Always use 2 characters of the largest size except for finishing up
	 (DO ((CH2 1 (1+ CH2))
	      (CH1 0 CH2)
	      (SLOPE (// (SMALL-FLOAT DY) DX)))
	     ((OR (= CH2 100)
		  (< (AREF NEWVEC-SLOPE-TABLE CH2) SLOPE))
	      (DO ((X X0 (+ X XINC))
		   (Y Y0 (+ Y YINC))
		   (CH) (XINC) (YINC) (STOP NIL) (RUN)
		   (CDX1 (AREF NEWVEC-DX-TABLE CH1))
		   (CDY1 (AREF NEWVEC-DY-TABLE CH1))
		   (CDX2 (AREF NEWVEC-DX-TABLE CH2))
		   (CDY2 (AREF NEWVEC-DY-TABLE CH2))
		   (LENGTH (+ (ABS (- X1 X0)) (ABS (- Y1 Y0)))))
		  (STOP)
		;; If Y would be below the line, use CH1 else use CH2.
		;; Watch out for zero divide possibility.
		(IF (OR (= (SETQ RUN (- (+ X CDX2) X0)) 0)
			(< (// (SMALL-FLOAT (- (+ Y CDY2) Y0)) RUN) SLOPE))
		    (SETQ CH CH1 XINC CDX1 YINC CDY1)
		  (SETQ CH CH2 XINC CDX2 YINC CDY2))
		;; If getting too close to the endpoint, use a shorter line
		(DO ((STRTL '(0 120 170 214 226) (CDR STRTL))
		     (I CH)
		     (D 2 (* D 2)))
		    ;; This test minimizes distance to endpoint by taxi cab
		    ;; metric without overshooting.
		    ((OR ( (+ (ABS (- (+ Y YINC) Y0)) (ABS (- (+ X XINC) X0)))
			    LENGTH)
			 (SETQ STOP (NULL (CDR STRTL)))))
		  (SETQ CH (+ (// (- CH (CAR STRTL)) 2) (CADR STRTL))
			I (* (// I D) D)
			XINC (// (AREF NEWVEC-DX-TABLE I) D)
			YINC (// (AREF NEWVEC-DY-TABLE I) D)))
		(COND ((NOT STOP)
		       (FUNCALL PRESS-EFTP-STREAM ':TYO CH)
		       (SETQ PRESS-N-CHARS (1+ PRESS-N-CHARS))
		       (SETQ PRESS-PENDING-CHARS (1+ PRESS-PENDING-CHARS))))))))))

)

