;; -*- Mode: LISP; Package: SYSTEM-INTERNALS; Lowercase: T -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This package provides for the definition and use of SELECTQ type
;;; objects that use the DTP-SELECT-METHOD microcode feature to allow
;;; destructuring of args on a per-operation basis

;--- Someone should fix this to use a new kind of function spec instead
;--- of using symbols, to hold its methods.

;(DEFSELECT <function-name or (<function-name> <function-to-be-called-if-no-match>)>
;  (<keyword or (<keyword> <keyword> ... <keyword>)> <arglist>
;    <forms>))

; (DEFSELECT FILE-CHAOSNET-COMMAND
;   (:FOO (BAZ &RESET BAR)
;         (DO-SOME-WORK))
;   (:SPAZZ (&OPTIONAL (BAR 1))
;           (SPZZA)))

(declare (special **defselect-alist** **defselect-fspec** **defselect-fname**))

(defmacro DEFSELECT (**defselect-fspec** &body methods &aux no-which-operations)
  (let ((**defselect-alist** nil)
        (tail-pointer)
        (wo-list-name)
	**defselect-fname**)
     (cond ((listp **defselect-fspec**)
            (setq tail-pointer (cadr **defselect-fspec**)
		  no-which-operations (caddr **defselect-fspec**))
            (setq **defselect-fspec** (car **defselect-fspec**))))
     (setq **defselect-fspec** (standardize-function-spec **defselect-fspec**))
     (setq **defselect-fname**
	   (if (listp **defselect-fspec**)
	       (intern (format nil "~A" **defselect-fspec**))
	       **defselect-fspec**))
     (setq wo-list-name
           (intern
	     (string-append **defselect-fname** "-DEFSELECT-" ':WHICH-OPERATIONS "-METHOD")
	     (symbol-package **defselect-fname**)))
    `(progn 'COMPILE
       ,@(mapcar (function defselect-internal) methods)
       (declare (special ,wo-list-name))
       ,@(cond ((not no-which-operations)
		(list
		 (defselect-internal
		   `(:which-operations (&rest ignore)
			;; Do this at runtime because of possible tail pointers.
			(cond (,wo-list-name)
			      (t (setq ,wo-list-name
				       (defselect-make-which-operations
					 ',**defselect-fspec**))))))
		 (defselect-internal
		   `(:operation-handled-p (operation)
			(if (null ,wo-list-name)
			    (setq ,wo-list-name
				  (defselect-make-which-operations
				    ',**defselect-fspec**)))
			(memq operation ,wo-list-name)))
		 (defselect-internal
		   `(:send-if-handles (operation &rest arguments)
			(if (null ,wo-list-name)
			    (setq ,wo-list-name
				  (defselect-make-which-operations
				    ',**defselect-fspec**)))
			(if (memq operation ,wo-list-name)
			    (lexpr-funcall (fdefinition ',**defselect-fspec**)
					   operation arguments)))))))
       ,(and (atom **defselect-fspec**) `(declare (*EXPR ,**defselect-fspec**)))
       (defselect-add-methods ',**defselect-fspec** ',**defselect-alist** ',tail-pointer)
       ,(and tail-pointer
	     `(eval-when (compile) (compiler:function-referenced ',tail-pointer
								 ',**defselect-fspec**)))
       ,(or no-which-operations
	    `(setq ,wo-list-name nil))
       ',**defselect-fspec**)))


(defun defselect-make-which-operations (fctn)
  ;; Ignore tracing, decode full hair, (:property foo bar), etc
  (setq fctn (fdefinition (unencapsulate-function-spec fctn)))
  (prog (ops subr)
    l   (cond ((or (null fctn)
		   (and (symbolp fctn)
			(not (fboundp fctn))))
	       ;; This cdr-codes the list, and conses it safely away from temporary areas.
	       (return (copylist ops permanent-storage-area)))
	      ((symbolp fctn)
	       (setq fctn (fsymeval fctn)))
	      ((listp fctn)
	       (cond ((symbolp (car fctn))
		      (cond (subr (setq fctn subr)	;Already one deep, return
				  (setq subr nil))
			    (t (setq subr (cdr fctn)	;explore subroutine
				     fctn (car fctn)))))
		     ((memq (caar fctn)   		;Dont add these
			    '(:which-operations :operation-handled-p :send-if-handles))
		      (setq fctn (cdr fctn)))
		     (t (setq ops (cons (caar fctn) ops))
			(setq fctn (cdr fctn)))))
	      ((eq (data-type fctn) 'dtp-select-method)
	       (setq fctn (%make-pointer dtp-list fctn)))
	      ((memq (data-type fctn) '(dtp-closure dtp-instance))
	       (setq fctn (car (%make-pointer dtp-list fctn))))
	      ((eq (data-type fctn) 'dtp-instance)
	       (setq fctn (flavor-select-method (get (typep fctn) 'flavor))))
	      (t (setq fctn nil)))
	(go l)))

(defun defselect-internal (method)
  (cond ((listp (car method))
         (prog1
          (defselect-internal (cons (caar method) (cdr method)))
          (do ((l (cdar method) (cdr l))
               (name (cond ((atom (cdr method)) (cdr method))
                           (t (intern (string-append **defselect-fname** "-DEFSELECT-"
                                                     (caar method) "-METHOD")
				      (symbol-package **defselect-fname**))))))
              ((null l))
              (setq **defselect-alist** (cons (cons (car l) name) **defselect-alist**)))))
        (t (cond ((atom (cdr method))
                  (setq **defselect-alist** (cons (cons (car method) (cdr method))
                                                  **defselect-alist**))
                  nil)
                 (t (let ((name (intern (string-append **defselect-fname** "-DEFSELECT-"
                                                       (car method) "-METHOD")
					(symbol-package **defselect-fname**))))
                      (setq **defselect-alist** (cons (cons (car method) name)
                                                      **defselect-alist**))
                      `(defun ,name
			 ,(cons '**defselect-op**	;First argument is operation
				(cadr method))		;Remaining args are those specified
			 (declare (function-parent ,**defselect-fname**))
			 **defselect-op**		;Suppress bound but not used warning
			 . ,(cddr method))))))))

(defun defselect-add-methods (function alist &optional tail-pointer)
  (let ((current-definition (if (listp function)
				(get (car function) (cadr function))
				(and (fboundp function)
				     (fsymeval function)))))
    (setq current-definition (if (= (%data-type current-definition) dtp-select-method)
				 (%make-pointer dtp-list current-definition)))
    (do ((next-alist alist (cdr next-alist)))
        ((null next-alist))
      (do ((next-current-def current-definition (cdr next-current-def)))
          ((cond ((atom next-current-def)
                  (setq current-definition (cons (car next-alist) current-definition))
                  t)
                 ((eq (caar next-alist) (caar next-current-def))
                  (rplaca next-current-def (car next-alist))
                  t)))))
    (and tail-pointer (rplacd (last current-definition) tail-pointer))
    (fset-carefully function (%make-pointer dtp-select-method current-definition))))
