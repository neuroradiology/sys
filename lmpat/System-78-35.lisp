;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.35
;;; Reason: Make completing reader more restrictive with delimiters
;;; Written 12/24/81 14:49:12 by MMcM,
;;; while running on Retriever from band 2
;;; with System 78.34, ZMail 38.5, Symbolics 8.7, Tape 6.5, LMFS 21.18, Canon 9.5, microcode 841.



; From file COMD.LISP >ZWEI POINTER:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))


;;;Complete a given STRING from an ALIST of strings.  CHAR-POS is position in string to be
;;;relocated with new things inserted.  TRUNC says dont complete more than one chunk at end
;;;Returns new STRING, matching subset of ALIST, COMPLETED-P if some completion was done,
;;;new CHAR-POS, and MAGIC-POS location of first point of ambiguity.  COMPLETED-P is 'NOSPACE
;;;if proper delimiter is already at end of string.
;;;For efficiency, if ALIST is an ART-Q-LIST array, it is assumed to be alphabetically
;;;sorted.  DONT-NEED-LIST says we really dont want all the possibilities, just not NIL.
(DEFUN COMPLETE-STRING (STRING ALIST DELIMS &OPTIONAL DONT-NEED-LIST CHAR-POS TRUNC
			       &AUX NCHUNKS CHUNKS CHUNK-DELIMS FILLS CHAMB TEMS RETS
			       RCHUNKS TEM LEN COMPLETED-P CHAR-CHUNK CHAR-OFFSET MAGIC-POS
			       TAIL)
  (SETQ CHUNKS (MAKE-ARRAY NIL 'ART-Q 20. NIL '(0))
	CHUNK-DELIMS (MAKE-ARRAY NIL 'ART-32B 20. NIL '(0)))
  (SETQ LEN (STRING-LENGTH STRING))
  (DO ((I 0 (1+ I))
       (J 0))
      ((> I LEN))
    (COND ((COND ((= I LEN)
		  (SETQ TEM -1))	;Last character delimits a chunk
		 (T
		  (MEMQ (SETQ TEM (AREF STRING I)) DELIMS)))
	   (AND CHAR-POS (> CHAR-POS J)	;Keep track of relative position
		(SETQ CHAR-CHUNK (ARRAY-LEADER CHUNKS 0)
		      CHAR-OFFSET (- CHAR-POS J)))
	   (ARRAY-PUSH-EXTEND CHUNKS (NSUBSTRING STRING J I))
	   (ARRAY-PUSH-EXTEND CHUNK-DELIMS TEM)
	   (SETQ J I))))
  (SETQ NCHUNKS (ARRAY-ACTIVE-LENGTH CHUNKS)
	FILLS (MAKE-ARRAY NIL 'ART-Q NCHUNKS)
	TEMS (MAKE-ARRAY NIL 'ART-Q NCHUNKS)
	RCHUNKS (MAKE-ARRAY NIL 'ART-Q NCHUNKS)
	CHAMB (MAKE-ARRAY NIL 'ART-1B NCHUNKS))
  (AND (ARRAYP ALIST)
       (MULTIPLE-VALUE (ALIST TAIL)
	 (COMPLETE-STRING-BOUNDS ALIST DELIMS NCHUNKS CHUNKS CHUNK-DELIMS)))
  (DO ((L ALIST (CDR L))
       (ALL-AMBIG))
      ((EQ L TAIL))
    (COND ((NLISTP L))				;Indirect through multiple alists
	  ((NULL (COMPLETE-CHUNK-COMPARE (CAAR L) DELIMS NCHUNKS CHUNKS CHUNK-DELIMS TEMS
					 (AND (NULL RETS) RCHUNKS)))
	   (OR RETS (SETQ CHUNKS RCHUNKS))	;First winner determines case of result
	   (PUSH (CAR L) RETS)		;add to list of partial matches
	   (SETQ ALL-AMBIG DONT-NEED-LIST)
	   (DO ((I 0 (1+ I))
		(FILL))
	       (( I NCHUNKS))
	     (SETQ TEM (AREF TEMS I)
		   FILL (AREF FILLS I))
	     (COND ((NULL FILL)		;First one to complete a chunk
		    (ASET TEM FILLS I)	;save for later ones
		    (AND (PLUSP (STRING-LENGTH TEM))
			 (SETQ ALL-AMBIG NIL)))	;This chunk not ambiguous yet
		   (T
		    (SETQ LEN (STRING-LENGTH FILL))
		    (DO ((J 0 (1+ J))
			 (LEN1 (STRING-LENGTH TEM)))
			(( J LEN)
			 (OR (ZEROP LEN)
			     (AND (= I (1- NCHUNKS))
				  (= LEN 1)
				  (MEMQ (AREF FILL 0) DELIMS))
			     (SETQ ALL-AMBIG NIL)))
		      (COND ((OR ( J LEN1)
				 (NOT (CHAR-EQUAL (AREF FILL J) (AREF TEM J))))
			     ;;Not the same completion, shorten final version
			     (ASET (NSUBSTRING FILL 0 J) FILLS I)
			     (ASET 1 CHAMB I)	;Remember this was ambiguous
			     (OR (ZEROP J) (SETQ ALL-AMBIG NIL))
			     (RETURN NIL)))))))
	   ;;If not going to complete and dont need actual list, finish up now.
	   (AND ALL-AMBIG (NULL (AREF FILLS (1- NCHUNKS))) (RETURN NIL)))))
  (COND ((AND TRUNC (SETQ TEMS (AREF FILLS (1- NCHUNKS))))
	 (SETQ LEN (STRING-LENGTH TEMS))
	 (AND (ZEROP (AREF CHAMB (1- NCHUNKS)))	;If last chunk wasnt ambigous,
	      (SETQ TRUNC 'NOSPACE))	;shouldnt have delimiter there
	 (DO I 0 (1+ I) ( I LEN)
	     (COND ((MEMQ (AREF TEMS I) DELIMS)
		    (ASET (NSUBSTRING TEMS 0 (1+ I)) FILLS (1- NCHUNKS))
		    (SETQ TRUNC 'NOSPACE)	;Already gave a delimiter
		    (RETURN NIL))))))
  (SETQ TEMS "")
  (DO I 0 (1+ I) ( I NCHUNKS)
      (AND CHAR-POS CHAR-CHUNK (= I CHAR-CHUNK)	;In case inside chunk not completed,
	   (SETQ CHAR-POS (+ (STRING-LENGTH TEMS) CHAR-OFFSET)))	;relocate
      (SETQ TEMS (STRING-APPEND TEMS (AREF CHUNKS I)))
      (COND ((AND (SETQ TEM (AREF FILLS I)) (> (STRING-LENGTH TEM) 0))
	     (SETQ TEMS (STRING-APPEND TEMS TEM)
		   COMPLETED-P T)
	     (AND CHAR-POS CHAR-CHUNK (= I CHAR-CHUNK)	;If inside completed chunk,
		  (SETQ CHAR-POS (STRING-LENGTH TEMS)))))	;move to end of it
      (OR MAGIC-POS (ZEROP (AREF CHAMB I))	;Remember end of leftmost ambigous chunk
	  (SETQ MAGIC-POS (STRING-LENGTH TEMS))))
  (AND COMPLETED-P (EQ TRUNC 'NOSPACE)
       (SETQ COMPLETED-P 'NOSPACE))
  (VALUES TEMS (NREVERSE RETS) COMPLETED-P CHAR-POS MAGIC-POS))

)

; From file COMD.LISP >ZWEI POINTER:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))


;;;Compare a STR with the given chunks and return NIL if it is a possible completion,
;;;else LESS or GREATER according as it is less or greater than the CHUNKS.
;;;T is returned for the indeterminate case, for the sake of the binary search in the
;;;array case.  The actual completer only checks NULL.
(DEFUN COMPLETE-CHUNK-COMPARE (STR DELIMS NCHUNKS CHUNKS CHUNK-DELIMS &OPTIONAL TEMS RCHUNKS
			       &AUX LEN2)
  (SETQ LEN2 (STRING-LENGTH STR))
  (DO ((I 0 (1+ I))
       (J 0)
       (K)
       (LEN1)
       (CHUNK)
       (DELIM)
       (FLAG))
      (( I NCHUNKS) NIL)		;Aligns with each chunk, a winner
    (SETQ CHUNK (AREF CHUNKS I)
	  LEN1 (STRING-LENGTH CHUNK))
    (SETQ K (DO ((J1 0 (1+ J1))
		 (K1 J (1+ K1))
		 (CH1)
		 (CH2))
		(( J1 LEN1) K1)
	      (AND ( K1 LEN2) (RETURN (OR FLAG 'LESS)))
	      (SETQ CH1 (LDB %%CH-CHAR (AREF CHUNK J1))
		    CH2 (LDB %%CH-CHAR (AREF STR K1)))
	      (AND ( CH1 #/a) ( CH1 #/z) (SETQ CH1 (LOGXOR CH1 40)))
	      (AND ( CH2 #/a) ( CH2 #/z) (SETQ CH2 (LOGXOR CH2 40)))
	      (COND ((= CH1 CH2))
		    (FLAG (RETURN T))
		    ((< CH1 CH2) (RETURN 'GREATER))
		    (T (RETURN 'LESS)))))
    (OR (NUMBERP K) (RETURN K))
    (AND RCHUNKS (ASET (NSUBSTRING STR J K) RCHUNKS I))
    (COND ((MINUSP (SETQ DELIM (AREF CHUNK-DELIMS I)))
	   (SETQ J NIL))		;For the last chunk, use rest of string
	  ((AND (SETQ J (STRING-SEARCH-SET DELIMS STR K LEN2))
		(= DELIM (AREF STR J)))
	   (OR FLAG (= J K) (SETQ FLAG T)))
	  ((OR FLAG ( (1+ I) NCHUNKS)) (RETURN T))	;If more could follow or ambig
	  ((= K LEN2) (RETURN 'LESS))
	  (T (RETURN 'GREATER)))	;Delim not found, if at end of STR, it's less
    (AND TEMS (ASET (NSUBSTRING STR K J) TEMS I))))

)

; From file COMD.LISP >ZWEI POINTER:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))


;;;Given an ART-Q-LIST array and the chunks to match, compute the subset of that array
;;;that could possibly be a completion of the string, and return an NTHCDR of the G-L-P
;;;and the appropriate tail to stop with.
(DEFUN COMPLETE-STRING-BOUNDS (ALIST DELIMS NCHUNKS CHUNKS CHUNK-DELIMS &AUX LO HI HIHI)
  (SETQ LO 0 HI 0
	HIHI (ARRAY-ACTIVE-LENGTH ALIST))
  (DO ((HILO HIHI)
       (IDX)
       (VAL T))
      (NIL)
    (AND (ZEROP (SETQ IDX (// (- HILO LO) 2)))	;binary search
	 (RETURN NIL))
    (SETQ IDX (+ LO IDX))
    (SETQ VAL (COMPLETE-CHUNK-COMPARE (CAR (AREF ALIST IDX)) DELIMS
				      NCHUNKS CHUNKS CHUNK-DELIMS))
    (COND ((EQ VAL 'LESS)
	   (SETQ LO IDX)
	   (SETQ HI IDX))
	  (T
	   (SETQ HILO IDX)
	   (COND ((NEQ VAL 'GREATER)
		  (SETQ HI IDX))
		 (T (SETQ HIHI IDX))))))
  (DO ((IDX)
       (VAL))
      (NIL)
    (AND (ZEROP (SETQ IDX (// (- HIHI HI) 2)))
	 (RETURN NIL))
    (SETQ IDX (+ HI IDX))
    (SETQ VAL (COMPLETE-CHUNK-COMPARE (CAR (AREF ALIST IDX)) DELIMS
				      NCHUNKS CHUNKS CHUNK-DELIMS))
    (COND ((NEQ VAL 'GREATER)
	   (SETQ HI IDX))
	  (T (SETQ HIHI IDX))))
  (SETQ ALIST (G-L-P ALIST))
  (VALUES (NTHCDR LO ALIST) (NTHCDR (1+ HI) ALIST)))

)
