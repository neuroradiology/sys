;;; -*- package:lmfs;mode:lisp-*-


(defmacro typep-2 (a b)  `(eq (typep ,a) ,b))

;;dlw

(defmacro lmfs:defsetf (name pat var &body body &aux (ref-form-var (gensym)))
  (remprop name 'setf)
  `(eval-when (load eval compile)
	      (defun (,name setf) (,ref-form-var ,var)
		(destructure (,pat (cdr ,ref-form-var))
		  . ,body))))

(defmacro destructure ((pattern target) &body body)
  `(let ,(destructure-pattern pattern target)
     . ,body))

(defun destructure-pattern (pattern target)
  (cond ((null pattern)
	 nil)
	((atom pattern)
	 `((,pattern ,target)))
	(t (append (destructure-pattern (car pattern) `(car ,target))
		   (destructure-pattern (cdr pattern) `(cdr ,target))))))
