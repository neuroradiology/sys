;-*-mode:lisp; package:format-*-

;This package consists mainly of a a collection of useful formatted output functions.
;All use STANDARD-OUTPUT; none require an argument to specify the stream.
;The arguments follow a convention, usually, but there are exceptions.
;The first argument is usually the datum to be output.
;The second is usually the number of columns (minimum) to use.
;Then come any number of options, which are keywords followed by values.

;Most functions accept an option :TAB-PERIOD, followed by a number,
;which specifies how far apart the padding points are.
;If the minimum width of the field is 10 and the tab-period is 5,
;then if the datum requires 12 columns it will be padded out to 15 columns
;(10 plus a multiple of 5).
;Another option which most functions allow is :PAD-CHAR, followed by
;the character to pad with (instead of Space).
;Another option is :MINPAD, followed by the minimum number of padding characters
;to use.
;Each function's documentation says which options it accepts.

;Use ONUM to output a number.  You can specify the radix, the number of columns,
; and other padding options.
;Use OFLOAT to output a floating point number with a specified number of digits.

;Use OCHAR to output a character.  Three formats are available:
; one is a READable format using #/ or #\, one is the form ZWEI uses
; to describe a character ("Control-Rubout", etc.), and one is the
; SAIL way of describing a character ("X", etc).

;Use OSTRING to output a string with padding.
; If no padding is desired PRINC is fine.
;Use OPRINT to PRIN1 an object with padding.

;Use PLURAL to output a word in either singular or plural
; according to the value of a number.  You specify only the
; singular and PLURAL computes the plural.  If the plural
; is too irregular, you can specify it too.
; (PLURAL (ONUM X) " frob") prints X followed by " frob" or " frobs".

;Use PAD to perform more complicated padding functions.
; PAD can print several things with padding between them
; to pad the whole thing out to a desired width.
; Special cases include right or left justification or centering
; of any output within a desired width.

;Use TAB to move to a specific column in the output (as opposed
; to padding a field to a specific width, which is independent
; of where the field starts).  Or move to the next multiple of
; a tab-period; or TERPRI if already past the column.

;Use BREAKLINE to go to a new line before some output
; if that output will not fit in the rest of the current line.
; You must specify the linel since the Lisp machine system has
; no convention for asking the stream.

;Use OUTPUT to concatenate several pieces of output.
; OUTPUT is unlike the other functions in that the first argument
; specifies a stream; or NIL means cons up a string, or T means
; use STANDARD-OUTPUT.  NIL and T must appear explicitly because
; they are checked for at macro expansion time.  The remaining args
; to OUTPUT perform output.  A string is simply printed.
; Anything else is executed to perform the output and its value is ignored.

;OUTFMT is like (LIST (OUTPUT NIL ...)).  It is good for places
;which expect a format control string; it causes FORMAT to be passes
;something which causes it to print exactly the output specified
;in the args to OUTFMT.

;There is nothing in this package for outputting a new line.
;TERPRI is fine for that.

;Example of use of this package: print the elements of a list,
;separated by commas, going to a new line before an element which doesn't fit.
(comment
(defun pcl (list linel)
  (do ((l list (cdr l))) ((null l))
    (breakline linel "  "
	       (princ (car l))
	       (and (cdr l) (princ ", "))))))

;Another example
(comment (output t "Total: " (plural (onum n-events) " event") "."))
;This prints "Total: 1 event." or "Total: 5 events."

(defmacro outfmt (&body forms)
  `(list (output nil . ,forms)))

(defmacro output (stream &body forms)
  (let ((do-the-work (mapcar 'output-expand forms)))
    (cond ((eq stream t)
	   `(progn . ,do-the-work))
	  ((null stream)
	   `(let ((standard-output 'format:format-string-stream)
		  (format:format-string (make-array nil art-string 200 nil '(0))))
	      (progn . ,do-the-work)
	      (adjust-array-size format:format-string
				 (array-active-length format:format-string))
	      format:format-string))
	  (t `(let ((standard-output ,stream))
		. ,do-the-work)))))

(defun output-expand (form)
  (cond ((stringp form) `(princ ,form))
	((numberp form) `(tyo ,form))
	(t form)))

;Output a word making it either singular or plural according to
;the value of the number.  Specify only the singular of the word
;and the plural is computed automatically.  Specify the plural as well
;if it is irregular and doesn't come out right automatically.
(defun plural (number singular &optional plural)
  (princ (cond ((= number 1) singular) (t (or plural (string-pluralize singular))))))

;Output a number.  Specify the desired-base, which can be a number,
;or :roman, :english or :ordinal.
;The value returned by onum is always the number printed.
;If minwidth is specified, we pad on the left to that width.
;Options for padding are :pad-char (the character to pad with),
;tab-period (the unit in which to add extra columns if we must go pas mincol),
;and minpad (the minimum number of padding characters to use).
;The option :signed with value t means always print a sign.
;The option :commas with value t means put a comma after every third digit.
(defun onum (number &optional (desired-base 10.) (minwidth 0) &rest options
		    &aux (*nopoint t) (padchar #/ ) commaflag signflag
		    (tab-period 1) (minpad 0))
  (do ((options options (cddr options))) ((null options))
    (selectq (car options)
      (:signed (setq signflag (cadr options)))
      (:commas (setq commaflag (cadr options)))
      (:pad-char (setq padchar (cadr options)))
      (:minpad (setq minpad (cadr options)))
      (:tab-period (setq tab-period (cadr options)))))
  (and (minusp number) (setq signflag nil))
  (cond ((and (eq desired-base ':roman)
	      (< number 4000.)
	      (> number 0))
	 (let ((format:roman-old nil))
	   (format:roman-step number 0)))
	((eq desired-base ':english)
	 (format:english-print number))
	((eq desired-base ':ordinal)
	 (format:english-ordinal-print number))
	(t
	  (let ((base desired-base))
	    (let ((pads
		    (- minwidth
		       (+ (flatc number)
			  minpad
			  (if signflag 1 0)
			  (if (and commaflag (numberp number))
			      (// (1- (flatc (abs number))) 3)	;number of commas
			      0)))))
	      (and (minusp pads)
		   (setq pads (+ pads (* tab-period (1+ (// (1- (- pads)) tab-period))))))
	      (dotimes (i pads)
		(funcall standard-output ':tyo padchar)))
	    (and signflag (funcall standard-output ':tyo #/+))
	    ;; this is princ rather than prin1 so you can have a string instead of a number
	    (cond ((not (and commaflag (numberp number))) (princ number))
		  ;; random hair with commas.  i'm not going to bother not consing.
		  (t (cond ((minusp number) (funcall standard-output ':tyo #/-) (setq number (- number))))
		     (setq number (nreverse (inhibit-style-warnings ;give up!
					      (exploden number))))
		     (do ((l number (cdr l))
			  (i 2 (1- i)))
			 ((null (cdr l)))
		       (cond ((zerop i)
			      (rplacd l (cons #/, (cdr l)))
			      (setq i 3 l (cdr l)))))
		     (dolist (ch (nreverse number))
		       (funcall standard-output ':tyo ch)))))))
  number)

(defun ofloat (number &optional digits force-exponential-notation minwidth &rest options)
  (and (numberp number) (not (floatp number)) (setq number (float number)))
  (cond ((or minwidth options)
	 (lexpr-funcall 'pad1
			(list (output nil (ofloat number digits force-exponential-notation)))
			minwidth
			options))
	((numberp number)
	 (si:print-flonum number standard-output nil (small-floatp number)
			  digits force-exponential-notation))
	(t (princ number))))

;Output a string.  If minwidth is non-nil it is the minimum number of columns
;to use (padding is used if the string is shorter).
;Options are :pad-char (the character to pad with),
;tab-period (the unit in which to add extra columns if we must go pas mincol),
;and minpad (the minimum number of padding characters to use).
;Including :right-justify t in the options means do that;
;otherwise we left-justify.

;This is like PRINC except for the padding options.
(defun ostring (string &optional minwidth &rest options)
  (ostring1 string 'princ 'flatc minwidth options))

;Like ostring but uses PRIN1 to output an arbitrary object.
(defun oprint (object &optional minwidth &rest options)
  (ostring1 object 'prin1 'flatsize minwidth options))

(defun ostring1 (object print-function flatsize-function minwidth options)
  (let (right-justify (tab-period 1) (pad-char #/ ) (minpad 0))
    (do ((options options (cddr options))) ((null options))
      (selectq (car options)
	(:right-justify (setq right-justify (cadr options)))
	(:tab-period (setq tab-period (cadr options)))
	(:pad-char (setq pad-char (cadr options)))
	(:minpad (setq minpad (cadr options)))))
    (and minwidth
	 (let ((size-excess (- (+ minpad (funcall flatsize-function object)) minwidth)))
	   (setq minpad (- minpad size-excess))
	   (and (> size-excess 0)
		(setq minpad (+ minpad (* (1+ (// (1- size-excess) tab-period)) tab-period))))))
    (and minwidth right-justify
	 (dotimes (i minpad)
	   (funcall standard-output ':tyo pad-char)))
    (funcall print-function object)
    (and minwidth (not right-justify)
	 (dotimes (i minpad)
	   (funcall standard-output ':tyo pad-char)))))

;Pad out to a certain column (mincol).
;Options are :pad-char (the character to pad with),
;tab-period (the unit in which to add extra columns if we must go pas mincol),
;and minpad (the minimum number of padding characters to use).
;Also, specifying a :unit of :pixel means work with pixels, not characters,
;as the unit of horizontal space.
;Specifying :terpri means if we are already past mincol, go to a new line
;and go to mincol on that line.
;When outputting to a string, we count the beginning of the string as column 0.
(defun tab (mincol &rest options)
  (let ((pad-char #/ ) (tab-period 1) (minpad 0) (unit ':character) terpri
	(ops (funcall standard-output ':which-operations)))
    (keyword-extract options key (pad-char tab-period minpad unit) (terpri))
    (cond ((memq ':read-cursorpos ops)
	   (multiple-value-bind (x y)
				(funcall standard-output ':read-cursorpos unit)
	     (let ((excess (- (+ minpad x) mincol)))
	       (setq minpad (- minpad excess))
	       (and (> excess 0)
		    (cond (terpri (terpri) (setq minpad mincol))
			  (t (setq minpad
				   (+ minpad
				      (* (1+ (// (1- excess) tab-period)) tab-period))))))
	       (cond ((and (or (eq unit ':pixel)
			       (= pad-char #/ ))
			   (memq ':increment-cursorpos ops))
		      (funcall standard-output ':increment-cursorpos
			       minpad 0 unit))
		     ((eq unit ':character)
		      (dotimes (i minpad) (funcall standard-output ':tyo pad-char)))
		     (t
		      (funcall standard-output ':set-cursorpos
			       (+ x minpad) y unit))))))
	  (t (funcall standard-output ':string-out "   ")))))

;Print a user-understandable name for a single character.
;Char is the character to print.
;Style selects among three styles:
;the default (called :read) is to use #/ or #\ to print something that can be read back in.
;Using :editor as the style means to spell everything out, as in "Meta-Return".
;:SAIL or :brief as the style means to use alpha, beta etc. for the control bits
;and not use any names like "Return" or "Rubout".

;top-explain, if T, means to explain how to type any character
;that requires using top or greek, as in " (Top-Z)".  This is useful with
;:editor and :sail, not with :read.

(defun ochar (char &optional style top-explain  minwidth &rest options
		   &aux chname bits char1)
  (setq char (character char))
  (setq char1 (ldb %%kbd-char char))
  (cond ((or minwidth options)
	 (lexpr-funcall 'pad1
			(list (output nil (ochar char style top-explain)) nil)
			minwidth
			options))
	((ldb-test %%kbd-mouse char)
	 (cond ((eq style ':read)
		(or (setq chname (ochar-get-character-name char))
		    (ferror nil "No name known for mouse character ~C" char))
		(funcall standard-output ':string-out "#\")
		(prin1 chname))
	       (t (funcall standard-output ':string-out "mouse-")
		  (funcall standard-output ':string-out (nth (ldb 0003 char)
							     '("left" "middle" "right")))
		  (funcall standard-output ':string-out (nth (ldb 0303 char)
							     '("" "-twice" "-thrice"))))))
	(t
	  (selectq style
	    (:editor
	      (setq bits (ldb %%kbd-control-meta char))
	      (and (bit-test 8 bits) (funcall standard-output ':string-out "Hyper-"))
	      (and (bit-test 4 bits) (funcall standard-output ':string-out "Super-"))
	      (and (bit-test 1 bits) (funcall standard-output ':string-out "Control-"))
	      (and (bit-test 2 bits) (funcall standard-output ':string-out "Meta-"))
	      (cond ((setq chname (ochar-get-character-name char1))
		     (let ((str (string-downcase chname)))
		       (aset (char-upcase (aref str 0)) str 0)
		       (funcall standard-output ':string-out str)
		       (return-array str)))
		    (t (funcall standard-output ':tyo char1))))
	    ((nil :read)
	     (funcall standard-output ':tyo #/#)
	     (funcall standard-output ':string-out (nth (ldb %%kbd-control-meta char)
							'("" "" "" ""
							  "" "" "" ""
							  "" "" "" ""
							  "" "" "" "")))
	     (cond ((setq chname (ochar-get-character-name char1))
		    (funcall standard-output ':tyo #/\)
		    (let ((str (string-downcase chname)))
		      (funcall standard-output ':string-out str)
		      (return-array str)))
		   (t (funcall standard-output ':tyo #//)
		      (funcall standard-output ':tyo char1))))
	    (:SAIL (funcall standard-output ':string-out (nth (ldb %%kbd-control-meta char)
							      '("" "" "" ""
								"" "" "" ""
								"" "" "" ""
								"" "" "" "")))
		   (and (memq char1 '(#/ #/ #/ #/ #/ #/))
			(funcall standard-output ':tyo #/))
		   (funcall standard-output ':tyo char1)))
	  (and top-explain 
	       (ochar-explain-top-character char1)))))

(defun ochar-get-character-name (char)
  (do ((l si:xr-special-character-names (cdr l)))
      ((null l) nil)
    (and (= (cdar l) char) (return (caar l)))))

;If char is a top or greek character, explain how to type it.
;Print " (Top-mumble)".  If char is not a top or greek char, do nothing.
(defun ochar-explain-top-character (char &aux name chname)
  (cond (( (ldb 0003 (%unibus-read 764102)) 1)	;last character new keyboard?
	 (setq name " (Top-"
	       chname (dotimes (i 100)
			(and (= char (aref si:kbd-translate-table 2 i))
			     (return (aref si:kbd-translate-table 1 i))))))
	((setq chname (dotimes (i 200)
			(and (= char (aref si:kbd-new-table 3 i))
			     (return (aref si:kbd-new-table 1 i)))))
	 (setq name " (Top-"))
	((setq chname (dotimes (i 200)
			(and (= char (aref si:kbd-new-table 4 i))
			     (return (aref si:kbd-new-table 0 i)))
			(and (= char (aref si:kbd-new-table 5 i))
			     (return (aref si:kbd-new-table 1 i)))))
	 (setq name (if (or (and ( chname #/a) ( chname #/z))
			    (and ( chname #/a) ( chname #/z)))
			" (Greek-" " (Front-"))))
  (cond (chname	 
	 (funcall standard-output ':string-out name)
	 (ochar chname ':editor)
	 (funcall standard-output ':tyo #/)))))

;Print several items in a fixed amount of horizontal space,
;padding between them to use up any excess space.
;Each of the forms prints one item.
;minwidth is the minimum amount of space.
;The options can include tab-period, which is the unit in which to add
;extra space to add if the minimum is not enough (default = 1 char);
;pad-char, which is the character to pad with (default = space);
;and minpad, which is the minimum number of pad characters between
;any two items (default = 0).
;To get padding at the beginning, include nil as an item at the beginning.
;To get padding at the end, include nil as an item at the end.
;If there is only one item, it is right-justified.
;If printing one item involves several actions,
;use a progn or (output t ...).
(defmacro pad ((minwidth . options) &body forms)
  `(pad1 (list . ,(mapcar '(lambda (f)
			     (cond ((stringp f) f)
				   (f `(output nil ,f))
				   (t "")))
			  forms))
	 ,minwidth . ,options))

(defun pad1 (strings minwidth &rest options)
  (let ((pad-char #/ ) (tab-period 1) (minpad 0) (strings-length 0) total-padding)
    (do ((options options (cdr options))) ((null options))
      (selectq (car options)
	(:tab-period (pop options) (setq tab-period (car options)))
	(:pad-char (pop options) (setq pad-char (car options)))
	(:minpad (pop options) (setq minpad (car options)))))
    (or (cdr strings) (setq strings (cons "" strings)))
    (dolist (string strings) (setq strings-length (+ (string-length string) strings-length)))
    ;; get the amount of space needed to print the strings and minpad padding
    (setq total-padding (+ (* (1- (length strings)) minpad) strings-length))
    ;; now bring in the minwidth and tab-period constraint, i.e. the total width is
    ;; at least minwidth and exceeds minwidth by a multiple of tab-period, and
    ;; get the total amount of padding to be divided among the padding points
    (setq total-padding (- (+ minwidth (* tab-period (// (+ (max (- total-padding minwidth) 0)
						      (1- tab-period))
						   tab-period)))
			   strings-length))
    ;; output the stuff
    (do ((strings strings (cdr strings))
	 (n-pads (// total-padding (1- (length strings))))
	 (j (\ total-padding (1- (length strings))) (1- j)))
	((null strings))
      (funcall standard-output ':string-out (car strings))
      (cond ((cdr strings)
	     (dotimes (i n-pads) (funcall standard-output ':tyo pad-char))
	     (and (> j 0)
		  (funcall standard-output ':tyo pad-char)))))))

;If the output printed by print-always does not fit on this line within linel,
;do a terpri and execute print-if-terpri beforehand.
(defmacro breakline (linel print-if-terpri &body print-always)
  `(let ((string1 (output nil . ,print-always)))
     (cond ((> (+ (funcall standard-output ':read-cursorpos ':character)
		  (string-length string1))
	       ,linel)
	    (terpri)
	    ,(output t print-if-terpri)))
     (princ string1)
     (return-array string1)))
