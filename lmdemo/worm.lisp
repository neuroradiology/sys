;;; -*- Mode:Lisp; Package:Hacks; Base:8 -*-

(DECLARE (SPECIAL BITS ORDER WORMS WORM-TURNS FONTS:WORM WORM-X WORM-Y FITP X-WORM Y-WORM
 	  WORM-BIG-CHAR WORM-GRAY-CHAR WORM-BLACK-CHAR WORM-STRIPE-CHAR WORM-SG))

(DECLARE (SPECIAL WORM-ALU-FUNCTION CHAR ORDER WORM-X WORM-Y BITS DIR))
       
(SETQ WORM-BIG-CHAR 5
      WORM-STRIPE-CHAR 4
      WORM-GRAY-CHAR 3
      WORM-BLACK-CHAR 2)

(COND ((NOT (BOUNDP 'WORMS))
       (SETQ WORMS (MAKE-ARRAY NIL 'ART-Q-LIST 6))
       (DO I 0 (1+ I) (>= I 6)
         (AS-1 (MAKE-STACK-GROUP (FORMAT NIL "WORM-~D" I)) WORMS I))))

(DEFUN PRESET (SG CHAR ALU-FN N)
       (STACK-GROUP-PRESET SG
			   (FUNCTION FLOP)
			   (SYMEVAL CHAR)
			   (SYMEVAL ALU-FN)
			   ORDER
			   WORM-X
			   WORM-Y
			   BITS
			   (* N (^ 3 (1- ORDER)))
			   TERMINAL-IO
			   SYS:%CURRENT-STACK-GROUP))

(DEFUN WORM (&OPTIONAL (BITS 0) (ORDER 7) (WORM-X 222) (WORM-Y 777) (RUN NIL)
	     &AUX LENGTH (YPOS (- (TV:SHEET-INSIDE-HEIGHT TERMINAL-IO) 44)))
   (OR (BOUNDP 'FONTS:WORM) (LOAD "LMDEMO;WORMCH" 'FONTS))
   (SETQ LENGTH (^ 3 (1+ ORDER)))
   (PRESET (AR-1 WORMS 0) 'WORM-BIG-CHAR 'TV:ALU-IOR 0)
   (SETQ FITP NIL)
   (DO I 0 (1+ I) (OR FITP ( I 2))			; Paint blackness over whole worm
       (SETQ FITP T)
       (FUNCALL TERMINAL-IO ':CLEAR-SCREEN)
       (DO I 0 (1+ I) (> I LENGTH) (FUNCALL (AR-1 WORMS 0))))
   (SETQ WORM-X X-WORM WORM-Y Y-WORM)
   (MAPC (FUNCTION PRESET)				; Preset wormlets
         (G-L-P WORMS)
         '(WORM-GRAY-CHAR WORM-BLACK-CHAR WORM-STRIPE-CHAR WORM-BLACK-CHAR
			  WORM-BLACK-CHAR WORM-BLACK-CHAR)
	 '(TV:ALU-XOR TV:ALU-ANDCA TV:ALU-IOR TV:ALU-IOR TV:ALU-ANDCA TV:ALU-IOR)
         '(0 1 2 3 5 6))				;4 is intentionally missing!
   (*CATCH 'WORM-EXIT 
     (DO ((I 0 (1+ I))
          (STOP-VAL 0))
         (NIL)
       (COND (RUN (AND (FUNCALL TERMINAL-IO ':TYI-NO-HANG) (SETQ RUN NIL)))
	     ((< I STOP-VAL))
	     (T (FUNCALL TERMINAL-IO ':SET-CURSORPOS 40. YPOS)
		(FUNCALL TERMINAL-IO ':CLEAR-EOL)
		(LET ((BASE 9))
		     (FORMAT T "~8S   " I))
		(PROG (CH VAL)
		      (SETQ VAL 0)
		 LOOP (SETQ CH (CHAR-UPCASE (TYI)))
		      (COND ((AND ( CH #/0)( CH #/9))
			     (SETQ VAL (+ (* VAL 9) (- CH #/0)))
			     (GO LOOP))
			    ((EQ CH #/N) (SETQ STOP-VAL (+ VAL I)))
			    ((EQ CH #/R) (SETQ STOP-VAL VAL))
			    ((EQ CH #/S) (SETQ VAL (^ 3 VAL)
					       STOP-VAL (* VAL (1+ (// I VAL)))))
			    ((EQ CH #/P) (SETQ RUN T))
			    ((EQ CH #/Q) (*THROW 'WORM-EXIT  NIL))
                            ((OR (EQ CH #/?) (EQ CH #/H))
                             (FORMAT T "P: Run,~TnR: Run until n,~TnN: Run <n> steps")
                             (FORMAT T "~TnS: Run until n'th order,~TQ: Quit~%")
                             (SETQ VAL 0)
                             (GO LOOP))
                            ))))
       (DO I 0 (1+ I) (>= I 6) (FUNCALL (AR-1 WORMS I)))))
)

(OR (BOUNDP 'WORM-TURNS) (SETQ WORM-TURNS (MAKE-ARRAY NIL 'ART-Q-LIST 12.)))

(FillArray WORM-TURNS '( 6  0
			 3  5
			-3  5
			-6  0
			-3 -5
			 3 -5))

(DEFUN FLOP (CHAR WORM-ALU-FUNCTION ORDER WORM-X WORM-Y BITS SNOOZE TERMINAL-IO WORM-SG)
      (DO I 0 (1+ I) ( I SNOOZE)
          (STACK-GROUP-RETURN NIL))
      (DO ((I 0 (1+ I))
	   (DIR (BOOLE 4 (- ORDER) 1)))
	  (NIL)
	  (TERD ORDER BITS)
	  (WORM-STEP)
	  (SETQ X-WORM WORM-X Y-WORM WORM-Y)
          (SETQ DIR (+ DIR 4))))

(DEFUN TERD (N BITS)
   (IF (PLUSP N)
          (COND ((Bit-Test BITS 1)
                   (TERD (1- N) (LSH BITS -1))
                   (SETQ DIR (- DIR -2))
                   (WORM-STEP)
                   (SETQ DIR (- DIR 4))
                   (TERD (1- N) (LSH BITS -1))
                   (WORM-STEP)
                   (SETQ DIR (+ DIR 2))
                   (TERD (1- N) (LSH BITS -1)))
                (T (TERD (1- N) (LSH BITS -1))
                   (WORM-STEP)
                   (SETQ DIR (+ DIR 4))
                   (TERD (1- N) (LSH BITS -1))
                   (SETQ DIR (- DIR 2))
                   (WORM-STEP)
                   (SETQ DIR (- DIR 2))
                   (TERD (1- N) (LSH BITS -1))))))
	    
(DEFUN WORM-STEP ()
   (CLIP 'WORM-X (TV:SHEET-INSIDE-WIDTH TERMINAL-IO) (SETQ DIR (\ (+ 12. DIR) 12.)))
   (CLIP 'WORM-Y (- (TV:SHEET-INSIDE-HEIGHT TERMINAL-IO) 55) (1+ DIR))
   (TV:PREPARE-SHEET (TERMINAL-IO)
      (TV:%DRAW-CHAR FONTS:WORM CHAR WORM-X WORM-Y WORM-ALU-FUNCTION TERMINAL-IO))
   (STACK-GROUP-RETURN NIL))

(DEFUN CLIP (Z N D)
       (SELECTQ (// (+ N (SET Z (+ (SYMEVAL Z) (AR-1 WORM-TURNS D))))
                    N)
            (0 (SET Z 1) (SETQ FITP NIL))
            (1)
            (2 (SET Z (1- N)) (SETQ FITP NIL))))

(DEFDEMO "Worm" "Pretty fractal patters, by Gosper and Holloway." (WORM))
