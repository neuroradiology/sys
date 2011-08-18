;-*-Mode:Lisp; Package:System-Internals-*-

;;Implement the mechanism by which advised functions operate.

(deff advise-prog #'prog)
(deff advise-setq #'setq)
(deff advise-progn #'progn)
(deff advise-multiple-value-list #'multiple-value-list)
(deff advise-return-list #'return-list)
(deff advise-apply #'apply)
(deff advise-let #'let)
(deff advise-list* #'list*)

(defmacro-displace advised-function (before after around inner-function-expression)
 (let ((default-cons-area background-cons-area))
  `(advise-prog (values)
     (advise-setq values
		  (advise-multiple-value-list
		    (advise-progn
		      ,@before
		      ,(advise-merge-arounds around inner-function-expression))))
     ,@after
     (advise-return-list values))))

;; Take the list of around advise and merge it together
;; Producing a form which evaluates them around the body.

(defun advise-merge-arounds (advice-list inner-function-expression)
  (cond ((null advice-list) `(advise-apply ,inner-function-expression arglist))
	(t (subst (advise-merge-arounds (cdr advice-list) inner-function-expression)
		  ':do-it
		  (car advice-list)))))

(defun (advise encapsulation-grind-function) (function def width real-io untyo-p)
  (cond (def	; Print the advice as calls to advise.
	 ;; First undisplace the displacing macros.
	 (cond ((eq (car (cadddr def)) 'si:displaced)
		(rplaca (cdddr def)
			(cadr (cadddr def)))))
	 (cond ((eq (car (caddr (cadddr def))) 'si:displaced)
		(rplaca (cddr (cadddr def))
			(cadr (caddr (cadddr def))))))
	 (grind-print-advice-slot (cadr (caddr (cadddr def))) ':before
				  function width real-io untyo-p)
	 (grind-print-advice-slot (caddr (caddr (cadddr def))) ':after
				  function width real-io untyo-p)
	 (grind-print-advice-slot (cadddr (caddr (cadddr def))) ':around
				  function width real-io untyo-p))))

(defun grind-print-advice-slot (slot-contents slot-name function width real-io untyo-p)
  (do ((l slot-contents (cdr l)) (i 0 (1+ i)))  ((null l))
    (grind-top-level `(advise ,function ,slot-name ,(cadr (cadar l)) ,i . ,(cddar l))
		     width real-io untyo-p)))

;; List of all functions which have been advised - for UNADVISE.

(defvar advised-functions nil)

;; Make a specifed function into an advised function
;; (with no advice, as yet) if it isn't one already.
;; Undisplace the advised-function macro if it has displaced itself.

(defun advise-init (function)
  (let ((default-cons-area background-cons-area)
	(spec1 (unencapsulate-function-spec function 'advise)))
    (cond ((neq spec1 (unencapsulate-function-spec spec1 '(advise)))
	   (let ((def (fdefinition spec1)))
	     (and (listp def) (eq (car def) 'macro) (pop def))
	     (cond ((eq (car (cadddr def)) 'si:displaced)
		    (rplaca (cdddr def)
			    (cadr (cadddr def)))))
	     (cond ((eq (car (caddr (cadddr def))) 'si:displaced)
		    (rplaca (cddr (cadddr def))
			    (cadr (caddr (cadddr def))))))))
	  (t
	   (push function advised-functions)
	   (encapsulate spec1 function 'advise
			`(advised-function nil nil nil ,encapsulated-function))))))

(defmacro advise (function class name position &rest forms)
  `(advise-1 ',function ',class ',name ',position ',forms))

(defun advise-1 (function class name position forms)
  (advise-init function)
  (setq forms (rename-within-new-definition-maybe function forms))
  (advise-update-list (advise-find-slot (unencapsulate-function-spec function 'advise)
					class)
		      name position forms))

(defun advise-find-slot (function class &aux (def (fdefinition function)))
  (and (listp def) (eq (car def) 'macro) (pop def))
  (let ((slot-location (caddr (cadddr def))))
    (nthcdr (cond ((string-equal class "BEFORE") 1)
		  ((string-equal class "AFTER") 2)
		  ((string-equal class "AROUND") 3)
		  (t (ferror nil "Second argument is ~s, neither BEFORE, AFTER nor AROUND" class)))
	    slot-location)))

(defun advise-update-list (slot-location name position forms)
  (let* ((default-cons-area background-cons-area)
	 preceding (new-unit `(progn ',name . ,forms)))
    (cond ((numberp position)
	   (or (setq preceding (nthcdr position (locf (car slot-location))))
	       (progn (setq preceding (locf (car slot-location)))
		      (do () ((null (cdr preceding)))
			(pop preceding)))))
	  ((and (null name) (null position)))
	  ((or (symbolp position) (null position))
	   (setq preceding (mem #'(lambda (p elt) (eq p (cadadr elt)))
				(or position name) (locf (car slot-location))))))
    ;; If the symbol isn't found, or no position is specified,
    ;; inser new advice at the beginning.
    (or preceding (setq preceding (locf (car slot-location))))
    (push new-unit (cdr preceding))
    ;; If we have a name, delete any old advice with that name.
    (and name
	 (do ((l (locf (car slot-location)) (cdr l))) ((null l))
	   (and (eq (cadadr (cadr l)) name)
		(neq (cadr l) new-unit)
		(return (rplacd l (cddr l))))))
    nil))

(defmacro unadvise (&optional function class position)
  (cond ((null function)
	 '(mapc 'unadvise-1 advised-functions))
	(t `(unadvise-1 ',function ',class ',position))))

(defun unadvise-1 (function &optional class position)
  (and (member function advised-functions) (advise-init function))
  (let* ((spec1 (unencapsulate-function-spec function 'advise))
	 (slot-location (and class (advise-find-slot spec1 class))))
    (check-arg position (or (symbolp position) (and (fixp position) (>= position 0)))
			"a symbol or a positive fixnum")
    (cond ((null class)
	   (fdefine spec1 (fdefinition (unencapsulate-function-spec spec1 '(advise))))
	   (setq advised-functions (delete function advised-functions)))
	  ((null position)
	   (rplaca slot-location nil))
	  ((numberp position)
	   (let ((preceding (nthcdr position (locf (car slot-location)))))
	     (cond ((cdr preceding) (rplacd preceding (cddr preceding)))
		   (t (ferror nil "Function ~S has less than ~D pieces of ~S-advise"
			      function position class)))))
	  ((symbolp position)
	   (do ((l (locf (car slot-location)) (cdr l)))
	       ((null l)
		(ferror nil "Function ~S has no ~S-advice named ~S"
			function class position))
	     (and (eq (cadadr (cadr l)) position)
		  (return (rplacd l (cddr l)))))))
    (and class
	 (null (car (advise-find-slot spec1 ':before)))
	 (null (car (advise-find-slot spec1 ':after)))
	 (null (car (advise-find-slot spec1 ':around)))
	 (let ((olddef (fdefinition (unencapsulate-function-spec spec1 '(advise)))))
	   (cond ((eq (car (fdefinition spec1)) 'macro)
		  (setq olddef (cons 'macro olddef))))
	   (fdefine spec1 olddef)
	   (setq advised-functions (delete function advised-functions))))
    nil))

;;ADVISE-WITHIN: advise one function but only when called from another specific one.
;;An alternative to advisingg (:within foo bar).

(defmacro advise-within (within-function function-to-advise class name position &rest forms)
  `(advise-within-1 ',within-function ',function-to-advise ',class ',name ',position ',forms))

(defun advise-within-1 (within-function function-to-advise class name position forms)
  (advise-1 `(:within ,within-function ,function-to-advise)
	    class name position forms))

(defmacro unadvise-within (within-function &optional advised-function class position)
  `(unadvise-within-1 ',within-function ',advised-function ',class ',position))

;; UNADVISE-WITHIN is not superfluous because if you specify
;; just the within-function, or nothing at all,
;; it eliminates all advising of anything within that within-function,
;; or all advising within anything.

(defun unadvise-within-1 (within-function &optional advised-function class position)
    (cond ((null within-function)
	   (dolist (fn advised-functions)
	     (and (listp fn) (eq (car fn) ':within)
		  (unadvise-1 fn))))
	  ((null advised-function)
	   (dolist (fn advised-functions)
	     (and (listp fn) (eq (car fn) ':within)
		  (eq (cadr fn) within-function)
		  (unadvise-1 fn))))
	  (t (unadvise-1 `(:within ,within-function ,advised-function) class position))))
