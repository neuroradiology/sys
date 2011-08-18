; -*- Mode:Lisp; Package:Press; Lowercase:yes -*-

(eval-when (compile eval)
  (cond ((status feature lispm))
	((status macro /#))
	(t (load '|dsk:liblsp;sharpm|))))

(declare (special font-width-data))
#Q (declare (setq run-in-maclisp-switch t))

;Interesting functions:
; (LOAD-FONT-WIDTHS)
;	loads up the file.  Takes an optional argument of the filename
;	of the widths file (defaults to FONTS; FONTS WIDTHS).
;	If a second optional argument is supplied, it is a list of
;	lists (family-name face-name point-size) and only those
;	specific fonts are loaded, to avoid running out of pdp-10
;	address space.
;	Merges with pre-existing contents of FONT-WIDTH-DATA (set it
;	to NIL first if you want to flush the old data.)
; (GET-FONT-WIDTH-DATA family-name face-name point-size)
;	returns an array of widths in micas (-1 for non-existent chars)
; (GET-FONT-WIDTH-AND-HEIGHT family-name face-name point-size)
;	returns a list of width and height in micas
;	This is gotten from the bounding box since the fixed Y-width
;	seems to be garbage generally.

;Data structure desired.
;  FONT-WIDTH-DATA is a list of elements:
;    (family-name face-name size rotation bounding-box xwidths ywidths)
;	family-name is an interned symbol
;	face-name is an interned symbol (|| in the normal case)
;		structured as {B|L} {I} {C|E}
;	size is 0 for relative sizing, in which case the data are
;		in thousands of the nominal point-size of the font,
;		or else size is the point-size in micas and the data are
;		in micas.  A mica is 10 microns = 1/2540 inch
;	rotation is normally 0, it is minutes of arc countclockwise
;	bounding-box is a list of 4 numbers x-offset y-offset width height
;	xwidths is a single number or a zero-origin fixnum array
;	ywidths is a single number or a zero-origin fixnum array
; A width of -1 means a non-existent character
; In the bounding-box, the x-offset is negative to go to the left, and
; the y-offset is negative to go below the baseline.
; A point is exactly 2540./72. = approximately 35.27777 micas
; On the Lisp machine, strings are used rather than symbols
; to avoid any possible package problems.

#M (eval-when (compile eval)
	(defmacro dotimes ((var val) &rest forms)
		  `(do ((,var ,val (1- ,var))) ((not (> ,var 0))) ,.forms)))


#M (eval-when (compile eval)
	(defmacro with-open-file ((var filename options) . body)
		  `(let ((,var (open ,filename ,options)))
		     ,@body
		     (close ,var))))

(declare (special widths-file code-alist #M widths-file-next-word))

;Fixnum array (so no number cons) contains -1 or buffered word
#M (or (boundp 'widths-file-next-word)
       (setq widths-file-next-word (*array nil 'fixnum 1)))

#M (declare (fixnum (next-word) (widths-file-pos) i j k m n wd))

(eval-when (compile eval #q load)
(defmacro high-byte (word)
  `(lsh ,word -8))

(defmacro low-byte (word)
  `(boole 1 377 ,word))
);eval-when

;Get next 16-bit word from widths-file
(defun next-word ()
  #M (cond ((minusp (arraycall fixnum widths-file-next-word 0))
	    (let ((wd (in widths-file)))
	      (store (arraycall fixnum widths-file-next-word 0)
		     (boole 1 (lsh wd -4) 177777))
	      (lsh wd -24)))
	   (t (prog2 nil (arraycall fixnum widths-file-next-word 0)
		     (store (arraycall fixnum widths-file-next-word 0) -1))))
  #Q (funcall widths-file ':tyi "Unexpected EOF on widths file"))

(defun widths-file-pos ()
   #M (- (* 2 (filepos widths-file))
	 (cond ((minusp (arraycall fixnum widths-file-next-word 0)) 0)
	       (t 1)))
   #Q (funcall widths-file ':read-pointer))

;2's complement form of next-word
(defun next-word2 ()
  (let ((wd (next-word)))
    (and (> wd 77777) (setq wd (- wd 200000)))
    wd))

	     
(defun bcpl-string (n widths-file) ;n = max-length-including-header-byte and is even
  (let ((wd (next-word)))
      (do ((chlist #M nil #Q (make-array nil 'art-string (high-byte wd)))
	   (m (high-byte wd) (1- m))	;Number of characters
	   (i 0 (1+ i))
	   (k (// (- n (high-byte wd) 1) 2)))	;k is number of extra words
	  ((zerop m)
	   (do () ((zerop k))
	     (next-word) (setq k (1- k)))
	#M (prog2 (setq chlist (nreverse chlist))
		  (implode chlist)
		  (reclaim chlist t))
	#Q chlist)
	(cond ((oddp i)
	       (setq wd (next-word))
	    #M (push (high-byte wd) chlist)
	    #Q (aset (high-byte wd) chlist i))
	      (t
	    #M (push (low-byte wd) chlist)
	    #Q (aset (low-byte wd) chlist i))))))

(defun code-to-name (code)
  (or (cdr (assoc code code-alist))
      (list 'code code)))

;Normal face codes are || for normal, I for italic, B for bold, etc.
;These are strings rather than symbols on the Lisp machine.
;Alternatively the face code can be a list of the normal code,
;the CMU character-set-convention code, and the TEX logical size code.
(defun decode-face (face-code2)
  (declare (fixnum face-code2))
  (let ((l nil)
	(res nil)
	(face-code (\ face-code2 18.))
	(cmu-bullshit (\ (// face-code2 18.) 3))
	(tex-bullshit (if (< face-code2 54.) -1 (- face-code2 54.))))
    (declare (fixnum face-code cmu-bullshit tex-bullshit))
    (cond ((> face-code 11.)
	   (setq face-code (- face-code 12.))
	   (push #/E l))
	  ((> face-code 5)
	   (setq face-code (- face-code 6))
	   (push #/C l)))
     (cond ((oddp face-code)
	   (setq face-code (1- face-code))
	   (push #/I l)))
    (cond ((> face-code 3)
	   (setq face-code (- face-code 4))
	   (push #/L l))
	  ((> face-code 1)
	   (setq face-code (- face-code 2))
	   (push #/B l)))
    (cond ((not (zerop face-code))
	   (error '|extra garbage in face-code| face-code)))
    (setq res #M (implode l)
	      #Q (fillarray (make-array (length l) ':type 'art-string) l))
    (cond ((or (not (zerop cmu-bullshit)) (not (minusp tex-bullshit)))
	   (setq res (list res cmu-bullshit))
	   (or (minusp tex-bullshit)
	       (setq res (nconc res (list tex-bullshit))))))
    res))

;Load it up and make the data structure mentioned at front of file
(defun load-font-widths (&optional filename fonts-desired)
  (with-open-file (widths-file (or filename '((dsk fonts)fonts widths))
			       '(read fixnum))
    (let ((code-alist nil)
	  (segment-data nil)
       #M (noret t)	; don't play musical corblk while loading stuff
	  (wd 0))
      ; guestimate core needed:
   #M (let ((max-guess (* 3 (lengthf widths-file))))
	(declare (fixnum max-guess))
	(and fonts-desired (let ((new-guess (* 800 (length fonts-desired))))
			     (declare (fixnum new-guess))
			     (and (< new-guess max-guess)
				  (setq max-guess new-guess))))
	(getsp max-guess))
   #M (store (arraycall fixnum widths-file-next-word 0) -1)
      (setq wd (next-word))
      ;; Read IXN entries (type 1)
      (do () ((not (= (lsh wd -12.) 1)))
	(let ((code (next-word))
	      (name (bcpl-string 20. widths-file)))
	  (push (cons code name) code-alist))
	(setq wd (next-word)))
      ;; Read WidthIndexEntries (type 4)
      (do () ((not (= (lsh wd -12.) 4)))
	(setq wd (next-word))		;family,,face
	(push (list (code-to-name (high-byte wd))		;Family-name
		    (decode-face (low-byte wd))		;Face name
		    (progn (setq wd (next-word))		;bc,,ec
			   (high-byte wd))		;First code
		    (low-byte wd)				;Last code
		    (next-word)				;Size
		    (next-word)				;Rotation
		    (+ (lsh (next-word) 16.) (next-word))	;Segment SA
		    (+ (lsh (next-word) 16.) (next-word)));Segment Len
	      segment-data)
	(setq wd (next-word)))
      ;; Now should have type-0 entry (end of index)
      (or (zerop (lsh wd -12.))
	  (error '|Bullshit in file where type 0 IX expected| wd))
      ;; Now read out the WidthSegments, which should follow
      ;; immediately with no gaps.  Sort segments by SA
      ;; Hmm, now it seems gaps are allowed, so we skip them.
      ;; Also skip entries for fonts not in fonts-desired if it is non-nil.
      (setq segment-data (sort segment-data
			       #'(lambda (x y)
				   (< (cadddr (cdddr x)) (cadddr (cdddr y))))))
      (or (boundp 'font-width-data)
	  (setq font-width-data nil))
      (do ((segment-data segment-data (cdr segment-data))
	   (seg) (bb) (m 0) (xwidths) (ywidths))
	  ((null segment-data))
	(setq seg (car segment-data))
	(let ((gap (- (cadddr (cdddr seg)) (widths-file-pos))))
	  #M (declare (fixnum gap))
	  (cond ((minusp gap) (break file-out-of-phase t)))
	  (dotimes (i gap) (next-word)))
	(setq bb (list (next-word2) (next-word2) (next-word2) (next-word2)))
	(setq m (next-word))				;Flags
	  ;Note that the documentation on this flags word is wrong!
	(cond ((or (null fonts-desired)
		   (loop for f in fonts-desired
			 thereis (and (equal (car f) (car seg))  ;family
				      (equal (cadr f) (cadr seg)) ;face
				      (or (zerop (car (cddddr seg))) ;general
					  (point-size-equal	;specific
					      (car (cddddr seg)) (caddr f)))
				      (zerop (cadr (cddddr seg))) ;no rotation
				      )))
	       ;; Process X-data
	       (cond ((not (zerop (boole 1 100000 m)))
		      (setq xwidths (next-word)))
		     (t (setq xwidths (*array nil 'fixnum 400))
			(fillarray xwidths '(-1))	;Chars not in bc..ec
			(do ((j (caddr seg) (1+ j))
			     (k 0))
			    ((> j (cadddr seg)))
			  (setq k (next-word))
			  (and (= k 100000) (setq k -1))
			  (store (arraycall fixnum xwidths j) k))))
	       ;; Process Y-data
	       (cond ((not (zerop (boole 1 40000 m)))
		      (setq ywidths (next-word)))
		     (t (setq ywidths (*array nil 'fixnum 400))
			(fillarray xwidths '(-1))	;Chars not in bc..ec
			(do ((j (caddr seg) (1+ j))
			     (k 0))
			    ((> j (cadddr seg)))
			  (setq k (next-word))
			  (and (= k 100000) (setq k -1))
			  (store (arraycall fixnum ywidths j) k))))
	       ;; Make the data
	       (push (list (car seg) (cadr seg) (car (cddddr seg))
			   (cadr (cddddr seg)) bb xwidths ywidths)
		     font-width-data))
	      (t	;Skip this font
	       ;; Skip X-data
	       (cond ((not (zerop (boole 1 100000 m)))
		      (next-word))
		     (t (do ((j (caddr seg) (1+ j)))
			    ((> j (cadddr seg)))
			  (next-word))))
	       ;; Skip Y-data
	       (cond ((not (zerop (boole 1 40000 m)))
		      (next-word))
		     (t (do ((j (caddr seg) (1+ j)))
			    ((> j (cadddr seg)))
			  (next-word))))))))
 #Q (si:set-file-loaded-id (funcall widths-file ':pathname) (funcall widths-file ':info)
			   package)
    ))

;This will return the entry for the particular size if it
;can find it, otherwise the entry for relative size.
;Errors out if no info found.
(defun find-font-data (family-name face-name point-size)
  (or (find-font-data-1 family-name face-name point-size)
      (error '|No information for font|
	     (list family-name face-name point-size))))

;This will return the entry for the particular size if it
;can find it, otherwise the entry for relative size.
;Returns NIL if no info found.
(defun find-font-data-1 (family-name face-name point-size)
 #Q (setq family-name (string family-name) face-name (string face-name))
    (or (do l font-width-data (cdr l) (null l)
	  (and (equal (caar l) family-name)
	       (equal (cadar l) face-name)
	       (point-size-equal (caddar l) point-size)
	       (zerop (cadddr (car l)))			;No rotation
	       (return (car l))))
	(do l font-width-data (cdr l) (null l)
	  (and (equal (caar l) family-name)
	       (equal (cadar l) face-name)
	       (zerop (caddar l))
	       (zerop (cadddr (car l)))			;No rotation
	       (return (car l))))))

(defun point-size-equal (internal point-size)
  (and ;(= (// (* internal 72.) 2540.) point-size)
       ;The above does not work.  Apparently Xerox just plain is not consistent
       ;about how many points there are in an inch.  It doesn't help that their
       ;font documentation is riddled with errors.  So we'll do something
       ;extremely forgiving.
       (> internal (// (- (* point-size 2540.) 1270.) 72.))
       (< internal (// (+ (* point-size 2540.) 1270.) 72.))))

; (GET-FONT-WIDTH-DATA family-name face-name point-size)
;	returns an array of widths in micas (-1 for non-existent chars)
(defun get-font-width-data (family-name face-name point-size)
  (let ((dat (find-font-data family-name face-name point-size))
	(xwidths)(tem))
    (setq xwidths (cadr (cddddr dat)))
    (cond ((not (zerop (caddr dat)))	;Already got data in micas
	   (cond ((numberp xwidths)	;Fixed-width font
		  (setq tem (*array nil 'fixnum 400))
		  (fillarray tem (list xwidths))
		  (setq xwidths tem)))
	   xwidths)
	  ((numberp xwidths)		;Fixed-width font
	   (setq tem (// (* xwidths 2540. point-size) 72000.))
	   (setq xwidths (*array nil 'fixnum 400))
	   (fillarray xwidths (list tem))
	   xwidths)		    
	  ((let ((arr (*array nil 'fixnum
			      (cadr (arraydims xwidths)))))
	     (do ((i 0 (1+ i))
		  (m 0)
		  (n (cadr (arraydims arr))))
		 ((= i n))
	      (setq m (arraycall fixnum xwidths i))
	      (store (arraycall fixnum arr i)
		     (cond ((minusp m) -1)
			   ((// (* m point-size 2540.) 72000.)))))
	     arr)))))

; (GET-FONT-WIDTH-AND-HEIGHT family-name face-name point-size)
;	returns a list of width and height in micas
;	This is gotten from the bounding box since the fixed Y-width
;	seems to be garbage generally.
; However, the width returned here does not equal the width of all
; the characters in a fixed-width font.  It is not clear what
; it is generally useful for.  The height returned here, however,
; is the right height for a line of text in this font.
(defun get-font-width-and-height (family-name face-name point-size)
  (let ((dat (find-font-data family-name face-name point-size)))
    (let ((bb (car (cddddr dat))))	;Bounding box
      (cond ((not (zerop (caddr dat)))	;Already got data in micas
	     (list (caddr bb) (cadddr bb)))
	    ((list (// (* (caddr bb) point-size 2540.) 72000.)
		   (// (* (cadddr bb) point-size 2540.) 72000.)))))))
