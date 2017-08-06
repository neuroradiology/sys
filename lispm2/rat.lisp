;;; -*-Mode:LISP; Package:SYSTEM-INTERNALS;  Lowercase: T -*-

;;; Right now integers are not normalized rationals.  This will probably change when
;;; / of two integers or ^ or an integer and a negative integer generate rationals.

(defun make-rational (num den)
  (let ((answer
	  (%allocate-and-initialize
	    dtp-extended-number
	    dtp-header
	    (dpb %header-type-rational-bignum %%header-type-field 0)
	    num
	    nil
	    3)))
    (%p-store-contents-offset den answer 2)
    answer))

(defmacro rational-numerator (number)
  `(%p-contents-offset ,number 1))

(defmacro rational-denominator (number)
  `(%p-contents-offset ,number 2))

(defun rationalp (x)
  (and (= (%data-type x) dtp-extended-number)
       (= (%p-ldb-offset %%header-type-field x 0) %header-type-rational-bignum)))

(defun numerator (number)
  (rational-numerator (rational number)))

(defun denominator (number)
  (rational-denominator (rational number)))

(defun rational (x)
  (cond ((rationalp x) x)
	;; This is necessary until // is different from TRUNC.
	((fixp x) (make-rational x 1))
	((small-floatp x) (make-rational-from-float (float x) 17.))
	((floatp x) (make-rational-from-float x 31.))
	(t (ferror nil "Cannot coerce ~S into a rational" x))))

(defun make-rational-from-float (x precision)
  ;;; Continued fraction expansion. This keeps track of precision 
  ;;; There should be macros for extracting the parts of flonums.
  ;;; It also assumes only loss of precision is in the subtraction, and one in
  ;;; the division. This seems to be a good assumption - BEE
  (loop named top
	with terms ()
	with pow2 = (%p-ldb-offset 1013 x 0)	;Original exponent 
	as int-part = (fix x)
	do (progn (push int-part terms)
		  (decf precision (1+ (haulong int-part)))
		  (decf x int-part))
	when (or (zerop x) (> (- pow2 (%p-ldb-offset 1013 x 0)) precision))
	do (loop for term in terms
		 with num = 1 and den = 0
		 do (psetq num (+ (* term num) den)
			   den num)
		 finally (return-from top (normalized-rational num den)))
	else do (setq x (// x))))


(defun numeric-one-argument (code number)
  (or (rationalp number)
      (ferror nil "Trap out on ~S" number))
  (let ((num (rational-numerator number))
	(den (rational-denominator number)))
    (selectq code
      (0 (if (or (= num 0)			;ABS
		 (and (> num 0) (> den 0))
		 (and (< den 0) (< num 0)))
	     number
	     (make-rational (abs num) (abs den))))
      (1 (make-rational (- num) den))		;MINUS
      (2 (= num 0))				;ZEROP
      (3 (or (and (> num 0) (> den 0))		;PLUSP
	     (and (< num 0) (< den 0))))
      (4 (or (and (> num 0) (< den 0))		;MINUSP
	     (and (< num 0) (> den 0))))
      (5 (make-rational (+ num den) den))	;ADD1
      (6 (make-rational (- num den) den))	;SUB1
      (7 (// num den))				;FIX
      (10 (// (float num) (float den)))		;FLOAT
      (11 (// (small-float num) (small-float den)))	;SMALL-FLOAT
      (12 (ferror nil "Illegal operation /"HAULONG/" on ~S" number))
      (13 (ferror nil "Illegal operation /"LDB/" on ~S" number))
      (14 (ferror nil "Illegal operation /"DPB/" on ~S" number))
      (15 (ferror nil "Illegal operation /"ASH/" on ~S" number))
      (16 (ferror nil "Illegal operation /"ODDP/" on ~S" number))
      (17 (ferror nil "Illegal operation /"EVENP/" on ~S" number))
      (t (ferror nil "Arith one-arg op code ~S on ~S" code number)))))

(defun print-rational (number stream ignore &aux (base 10.) (*nopoint t))
  (print-fixnum (rational-numerator number) stream)
  (funcall stream ':tyo #/\)
  (print-fixnum (rational-denominator number) stream))

(defun numeric-two-arguments (code number1 number2 &aux function)
  (setq function (nth code '(*plus *dif *times *quo = > < *min *max *boole)))
  (cond ((floatp number1)
	 (funcall function number1 (float number2)))
	((floatp number2)
	 (funcall function (float number1) number2))
	((small-floatp number1)
	 (funcall function number1 (small-float number2)))
	((small-floatp number2)
	 (funcall function (small-float number1) number2))
	((and (rationalp number1) (rationalp number2))
	 (rational-two-arguments code number1 number2))
	((rationalp number1)
	 (funcall function number1 (rational number2)))
	((rationalp number2)
	 (funcall function (rational number1) number2))
	(t
	 (ferror nil "Arith two-arg op code ~S on ~S and ~S" code number1 number2))))

(defun rational-two-arguments (code number1 number2)
  (let ((num1 (rational-numerator number1))
	(den1 (rational-denominator number1))
	(num2 (rational-numerator number2))
	(den2 (rational-denominator number2)))
    (selectq code
      (0 (normalized-rational (+ (* num1 den2) (* num2 den1)) (* den1 den2)))	;ADD
      (1 (normalized-rational (- (* num1 den2) (* num2 den1)) (* den1 den2)))	;SUB
      (2 (normalized-rational (* num1 num2) (* den1 den2)))	;MUL
      (3 (normalized-rational (* num1 den2) (* den1 num2)))	;DIV
      (4 (and (= num1 num2) (= den1 den2)))	;EQUAL
      (5 (> (* num1 den2) (* num2 den1)))	;GREATERP
      (6 (< (* num1 den2) (* num2 den1)))	;LESSP
      (7 (if (> number1 number2) number2 number1))	;MIN
      (10 (if (> number1 number2) number1 number2))	;MAX
      (otherwise (ferror nil "Rational two arg op code ~S on ~S and ~S"
			 code number1 number2)))))

;;; This does not convert n\1 to fix n, because of the // problem mentioned above.
(defun normalized-rational (num den)
  (and (minusp den) (setq num (- num)
			  den (- den)))
  (let ((gcd (\\ num den)))
    (or (= gcd 1) (setq num (// num gcd)
			den (// den gcd))))
  (make-rational num den))

(defun (rational standard-read-function) (stream string &aux num1 num2 len i (ibase 10.))
   stream
   (setq len (array-active-length string))
   (multiple-value (num1 i)
     (xr-read-fixnum-internal string 0 len))
   (multiple-value (num2 i)
     (xr-read-fixnum-internal string (1+ i) len))
   (values (normalized-rational num1 num2) 'fixnum))
