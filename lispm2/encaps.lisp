;; -*- Mode: Lisp; Package: System-internals-*-

;This file implements "encapsulations" of functions.
;All symbols referred to in this documentation are in SYSTEM-INTERNALS by default.

;An encapsulation is a new function definition put around an old one
;to do certain things before and after calling the old definition.
;Encapsulations are used for tracing, advising, etc.
;An encapsulation is created with the macro ENCAPSULATE.
;It is a new definition for the encapsulated function, which
;replaces the old one.  It has a debugging-info item looking like
;(encapsulated-definition unencapsulated-symbol type).
;unencapsulated-symbol is an uninterned symbol whose definition
;is the original definition which was replaced.  The encapsulation
;also uses that symbol to call the original definition.
;The type is a user-chosen name to identify the purpose of this
;particular encapsulation.  (Examples: trace, advise).

;The encapsulation type symbol should have a encapsulation-grind-function
;property which tells grind what to do with one.
;See the example for rename-within in this file.

;Once an encapsulation is made, it stays around until deliberately flushed,
;even if the function is redefined.  The encapsulations are considered
;as in addition to the definition, not as part of the definition.

;Encapsulations are normally interpreted, but it is ok to compile one.
;At least, it is ok with the system.  The subsystem that manipulates
;a particular type of encapsulation might be confused.
;However, just calling COMPILE compiles only the original definition,
;not the encapsulations.

;It is possible for one function to be encapsulated more than once.
;In this case, the order of encapsulations is independent of the
;order in which they were made.  It depends instead on their types.
;All possible encapsulation types have a total order and a new
;encapsulation is put in the right place in the order.
;Here is the order (innermost to outermost).
;Any encapsulation type which anybody wants to use must be in this list.
;No knowledge of the ordering appears anywhere but in this variable.
(defconst encapsulation-standard-order '(advise breakon trace rename-within))

;To find the right place in the ordering to insert a new encapsulation,
;it is necessary to parse existing ones.  This is done with the function
;UNENCAPSULATE-FUNCTION-SPEC.  It takes a function spec as an argument
;and returns another function spec.  It may return the same one.
;However, if the arg is defined and its definition is an encapsulation,
;then the unencapsulated-symbol is returned.  This process repeats
;until a symbol is reached whose definition is not an encapsulation.
;A second argument to this function can be used to restrict which
;types of encapsulations to pass through.  If the second arg is a list,
;then that list says which types to process.  If the second arg is a symbol,
;then it should be an encapsulation type, and the types which are processed
;are those which are ordered outside of the specified one.  Thus, it takes
;you to the level at which an encapsulation of that type is to be found
;if there is one, or where a new encapsulation of that type should be created.

;Examples: (UNENCAPSULATE-FUNCTION-SPEC FN 'TRACE) returns a function spec.
;If there is any trace encapsulation anywhere in fn, then it appears
;as the fdefinition of the spec which is returned.  If the fdefinition
;of that spec is not a trace encapsulation, then there is none,
;and a new one could be created by using ENCAPSULATE on that spec.
;(UNENCAPSULATE-FUNCTION-SPEC (UNENCAPSULATE-FUNCTION-SPEC FN 'TRACE) '(TRACE))
;returns whatever is inside any encapsulation of type trace.
;Fdefining (UNENCAPSULATE-FUNCTION-SPEC FN 'TRACE) to that would be a way of
;getting rid of any such encapsulation.
;(EQ (UNENCAPSULATE-FUNCTION-SPEC FN 'TRACE)
;    (UNENCAPSULATE-FUNCTION-SPEC (UNENCAPSULATE-FUNCTION-SPEC FN 'TRACE) '(TRACE)))
;is T if an encapsulation of type TRACE exists in FN because one call to u.f.s.
;moves up to it, and the other moves past it.

;One special kind of encapsulation which is implemented in this file
;is the type SI:RENAME-WITHIN.  This encapsulation goes around a definition
;in which renamings of functions have been done.
;How is this used?
;Well, if you define, advise or trace (:WITHIN FOO BAR), then
;BAR gets renamed to ALTERED-BAR-WITHIN-FOO wherever it is called from FOO,
;and FOO gets a SI:RENAME-WITHIN encapsulation  to record the fact.
;This causes GRINDEF to do the right things.
;It also causes any changes to the definition of FOO to have
;the same old renaming of BAR redone in them, to avoid pardoxical results.
;This happens because everyone who inserts any new piece of list structure
;inside the definition of FOO or any of its encapsulations always does
;(RENAME-WITHIN-NEW-DEFINITION-MAYBE 'FOO NEW-STRUCTURE)
;which returns a copy of NEW-STRUCTURE in which any renamings recorded for FOO
;have been done.  For example, FSET-CAREFULLY does this.

;For the most part, RENAME-WITHIN encapsulations are maintained automatically
;by FDEFINE on function specs of the form (:WITHIN ...).  The only time any other code
;must be concerned with them is when it changes part of the definition or encapsulations
;of a function; then it must call RENAME-WITHIN-NEW-DEFINITION-MAYBE
;in the right way.  Perhaps a new interface to FDEFINE can be designed
;to make this unnecessary to worry about.

;Only FDEFINE et al and GRIND know about encapsulations in any way except
;as recomended above.

;;; Two functions for looking at and decoding encapsulations.
;;; UNENCAPSULATE-FUNCTION-SPEC returns the copied symbol whose definition
;;; is the original definition of the symbol supplied as argument.
;;; RENAME-WITHIN-NEW-DEFINITION-MAYBE propagates existing renamings
;;; to some list structure which is to become part of the definition
;;; of a function which has a rename-within encapsulation already.
;;; See below for more information.

;Given a function spec, if the definition is traced or advised,
;return a function spec (a symbol, actually) for the encapsulated definition.
;If the function spec is not defined,
;or the outer definition is not an encapsulation,
;return the original function spec.

;ENCAPSULATION-TYPES is a list of which types of encapsulations to process.
;If another type is encountered, its spec is returned.
;If a symbol rather than a list is supplied for this arg,
;it stands for all types of encapsulations which standardly
;come outside the one specified.  Thus, specifying ADVISE
;is equivalent to specifying (TRACE RENAME-WITHIN)
;and specifying TRACE is equivalent to specifying (RENAME-WITHIN).

;To examine the encapsulated definition, do 
;   (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC ...))
;To alter that definition while preserving any encapsulation, do
;   (FDEFINE (UNENCAPSULATE-FUNCTION-SPEC ...) ...)
;Note that FDEFINE with CAREFULLY-FLAG=T does this for you.
(defun unencapsulate-function-spec
       (function-spec &optional encapsulation-types
		      &aux def tem)
  (prog ()
    ;; If a symbol was specified as the type of encapsulation,
    ;; then process all types which come outside of that type,
    ;; not including that type itself.
    (and encapsulation-types (symbolp encapsulation-types)
	 (or (setq encapsulation-types
		   (cdr (memq encapsulation-types encapsulation-standard-order)))
	     (return function-spec)))
    (return
      (cond ((not (fdefinedp function-spec)) function-spec)
	    ((and (progn (setq def (fdefinition function-spec))
			 (and (listp def) (eq (car def) 'macro)
			      (setq def (cdr def)))
			 (not (symbolp def)))
		  (setq tem (assq 'encapsulated-definition
				  (function-debugging-info def)))
		  (or (null encapsulation-types)
		      (memq (caddr tem) encapsulation-types)))
	     (unencapsulate-function-spec (cadr tem) encapsulation-types))
	    (t function-spec)))))

;; When you alter any part of the definition of a function,
;; if the function has a rename-within encapsulation on it
;; then whatever renamings are recorded in it ought to be performed
;; on the new structure that you are putting into the definition.
;; To do that, use RENAME-WITHIN-NEW-DEFINITION-MAYBE.
;; Supply the function spec (outer, not unencapsulated at all)
;; and the new definition or part of a definition.
;; It returns a copy of the new definition or part with the right renamings done.

(defun rename-within-new-definition-maybe (function definition)
  (let ((renamings (cadr (rename-within-renamings-slot function)))
	(default-cons-area background-cons-area))
    (dolist (tem renamings)
      (rename-within-replace-function (cadr tem) (car tem)
				      `(:location ,(locf definition)))))
  definition)

;; Encapsulate the function named FUNCTION (actually, it can be a spec)
;; with an encapsulation whose body is BODY and whose type is TYPE.
;; The args are all evaluated.  Inside BODY, refer to the variable
;; ENCAPSULATED-FUNCTION to get an object which you can funcall
;; to invoke the original definition of the function.
;; The value returned is the symbol used to hold the original definition.
;; Within the code which constructs the body, this symbol is the value of COPY.

(defmacro encapsulate (function outer-function type body &optional extra-debugging-info)
  `(let* ((default-cons-area background-cons-area)
	  (copy (make-symbol (cond ((symbolp ,function) (get-pname ,function))
				   (t (format nil "~s" ,function)))))
	  (defp (fdefinedp ,function))
	  (def (and defp (fdefinition ,function)))
	  encapsulated-function
	  lambda-list arglist-constructor macro-def)
     ;; Figure out whether we are operating on a macro, and in any case
     ;; compute the lambda list which the encapsulation will use.
     (cond (defp (setq macro-def (encapsulation-macro-definition def))
		 (setq lambda-list (encapsulation-lambda-list def)))
	   (t (setq lambda-list '(&rest arglist))))
     (and (symbolp lambda-list)
	  (ferror nil "~s cannot be encapsulated due to hairy arg quoting" ,outer-function))
     (setq arglist-constructor
	   (cons 'encapsulation-list*
		 (cdr (encapsulation-arglist-constructor lambda-list))))
     ;; Copy the original definition, if any, to the copied symbol.
     (and defp (fset copy def))
     ;; Cons up what the body ought to use to call the original definition.
     (setq encapsulated-function (cond (macro-def
					`(encapsulation-macro-definition #',copy))
				       (t `#',copy)))
     (setq def
	   `(named-lambda (,,function (encapsulated-definition ,copy ,,type)
			   . ,,extra-debugging-info)
			  ,lambda-list
			  (encapsulation-let ((arglist ,arglist-constructor))
			    ,,body)))
     ;; If this encapsulation goes inside rename-withins,
     ;; then do any renamings on it.
     (and (memq 'rename-within (cdr (memq ,type encapsulation-standard-order)))
	  (setq def (rename-within-new-definition-maybe ,outer-function def)))
     (and macro-def (setq def (cons 'macro def)))
     (fdefine ,function def nil t)
     copy))

(deff encapsulation-let #'let)
(deff encapsulation-list* #'list*)

(comment ;These are really in QCOPT, since add-optimizer isn't defined yet
(compiler:add-optimizer encapsulation-let encapsulation-let-fix)
(compiler:add-optimizer encapsulation-list* encapsulation-list*-fix)
)

(defun encapsulation-let-fix (form)
  (cons 'let (cdr form)))

(defun encapsulation-list*-fix (form)
  (cons 'list* (cdr form)))

;ENCAPSULATION-MACRO-DEFINITION, given a function definition,
;if it is a macro, or a symbol whose definition is a symbol whose ... is a macro,
;then return the function definition for expanding the macro.
;Encapsulations of macros call this function.

;ENCAPSULATION-LAMBDA-LIST, given a function definition,
;returns a suitable arglist for an encapsulation of that function.

;ENCAPSULATION-ARGLIST-CONSTRUCTOR, given such an arglist,
;returns an expression which would cons the values of the args
;into one list of all the actual arguments to the function.

;Given a function definition, if it is a macro (directly or indirectly)
;then return the function which does the expansion for it.
;Otherwise return nil.
(defun encapsulation-macro-definition (def)
  (cond ((listp def)
	 (and (eq (car def) 'macro)
	      (cdr def)))
	((symbolp def)
	 (and (fboundp def) (encapsulation-macro-definition (fdefinition def))))))

;FUNCTION IN, LAMBDA LIST SUITABLE FOR TRACING
;THAT FUNCTION OUT, OR AN ERROR IS GENERATED
;TRIES TO GENERATE JUST (&EVAL &REST ARGLIST).

(DEFUN ENCAPSULATION-LAMBDA-LIST (FCN)
  (COND ((NULL FCN) '(&REST ARGLIST))   ;If fn is not defined, NIL is supplied to us.
					;Assume a typical function, since we can't know.
	((SYMBOLP FCN)
	 (COND ((FBOUNDP FCN) (ENCAPSULATION-LAMBDA-LIST (FSYMEVAL FCN)))
	       (T '(&REST ARGLIST))))
	((LISTP FCN)
	 (SELECTQ (CAR FCN)
	   (LAMBDA (ENCAPSULATION-CONVERT-LAMBDA (CADR FCN)))
	   (NAMED-LAMBDA (ENCAPSULATION-CONVERT-LAMBDA (CADDR FCN)))
	   (OTHERWISE '(&REST ARGLIST))))
	(T ;A compiled or microcode function
	  (LET ((ARGS-INFO (%ARGS-INFO FCN)))
	    (COND ((BIT-TEST %ARG-DESC-INTERPRETED ARGS-INFO)
		   ;; For a random type of function, assume no quoting
		   '(&EVAL &REST ARGLIST))
		  ((BIT-TEST %ARG-DESC-FEF-QUOTE-HAIR ARGS-INFO)
		   (ENCAPSULATION-CONVERT-FEF (GET-MACRO-ARG-DESC-POINTER FCN)))
		  ((BIT-TEST %ARG-DESC-QUOTED-REST ARGS-INFO)
		   ;; Quoted rest arg but everything else evalled.
		   (DO ((NALLOW (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO) (1- NALLOW))
			(NREQ (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO) (1- NREQ))
			(LL '(&QUOTE &REST ARGLIST)
			    (CONS (COND ((MINUSP NREQ)
					 (LIST (GENSYM) NIL (GENSYM)))
					(T (GENSYM)))
				  LL)))
		       ((= NALLOW 0) LL)))
		  (T ;an ordinary compiled function, no quoting to worry about
		   '(&EVAL &REST ARGLIST)))))))

;Given a lambda list, return an expression which will
;cons up a simple list of all the argument values.
;Not all lambda lists are guaranteed to work,
;but those returned by encapsulation-lambda-list will.
(DEFUN ENCAPSULATION-ARGLIST-CONSTRUCTOR (LAMBDA-LIST &AUX RESTARG OPTARGS SOFAR)
  (SETQ RESTARG (MEMQ '&REST LAMBDA-LIST))
  (COND (RESTARG (SETQ SOFAR (CADR RESTARG)
		       LAMBDA-LIST (LDIFF LAMBDA-LIST RESTARG))))
  (SETQ OPTARGS (MEMQ '&OPTIONAL LAMBDA-LIST))
  (COND (OPTARGS (SETQ LAMBDA-LIST (LDIFF LAMBDA-LIST OPTARGS))
		 (SETQ OPTARGS (SUBSET-NOT #'(LAMBDA (ELT) (MEMQ ELT LAMBDA-LIST-KEYWORDS))
					   OPTARGS))
		 (DOLIST (A (REVERSE (CDR OPTARGS)))
		   (SETQ SOFAR
			 `(ENCAPSULATION-CONS-IF ,(CADDR A) ,(CAR A) ,SOFAR)))))
  (SETQ LAMBDA-LIST (SUBSET-NOT #'(LAMBDA (ELT) (MEMQ ELT LAMBDA-LIST-KEYWORDS)) LAMBDA-LIST))
  `(LIST* ,@LAMBDA-LIST ,SOFAR))

(DEFUN ENCAPSULATION-CONS-IF (CONDITION NEW-CAR TAIL)
  (IF CONDITION (CONS NEW-CAR TAIL) TAIL))

(DEFUN ENCAPSULATION-CONVERT-LAMBDA (LL
	&AUX EVARG QUARG EVOPT QUOPT EVREST QUREST)
  ;FIRST DETERMINE WHAT TYPES OF EVALAGE AND QUOTAGE ARE PRESENT (SET ABOVE AUX VARS)
  (DO ((L LL (CDR L))
       (ITEM)
       (OPTIONALP NIL)
       (QUOTEP NIL)
       (RESTP NIL))
      ((NULL L))
    (SETQ ITEM (CAR L))
    (COND ((EQ ITEM '&AUX)
	   (RETURN NIL))
	  ((OR (EQ ITEM '&EVAL) (EQ ITEM '&QUOTE-DONTCARE))
	   (SETQ QUOTEP NIL))
	  ((EQ ITEM '&QUOTE)
	   (SETQ QUOTEP T))
	  ((EQ ITEM '&OPTIONAL)
	   (SETQ OPTIONALP T))
	  ((OR (EQ ITEM '&REST) (EQ ITEM '&KEY))
	   (SETQ RESTP T))
	  ((MEMQ ITEM LAMBDA-LIST-KEYWORDS))
	  (RESTP
	   (COND (QUOTEP (SETQ QUREST T))
		 (T (SETQ EVREST T)))
	   (RETURN NIL))
	  (OPTIONALP
	   (COND (QUOTEP (SETQ QUOPT T))
		 (T (SETQ EVOPT T))))
	  (T (COND (QUOTEP (SETQ QUARG T))
		   (T (SETQ EVARG T))))))
  ;DECIDE HOW HAIRY A LAMBDA LIST IS NEEDED
  (COND ((AND (NOT QUARG) (NOT QUOPT) (NOT QUREST))
	 '(&EVAL &REST ARGLIST))
	((AND (NOT EVARG) (NOT EVOPT) (NOT EVREST))
	 '(&QUOTE &REST ARGLIST))
	(T	;NEED A HAIRY ONE.
	  (NRECONC
	    (DO ((L LL (CDR L))
	         (LAMBDA-LIST NIL)
		 OPTIONALP
	         (ITEM))
	        ((NULL L) LAMBDA-LIST)
	      (SETQ ITEM (CAR L))
	      (COND ((MEMQ ITEM '(&AUX &REST &KEY))
		     (RETURN LAMBDA-LIST))
		    ((MEMQ ITEM '(&EVAL &QUOTE &QUOTE-DONTCARE))
		     (SETQ LAMBDA-LIST (CONS ITEM LAMBDA-LIST)))
		    ((EQ ITEM '&OPTIONAL)
		     (OR OPTIONALP (SETQ LAMBDA-LIST (CONS ITEM LAMBDA-LIST)))
		     (SETQ OPTIONALP T))
		    ((MEMQ ITEM LAMBDA-LIST-KEYWORDS))
		    (OPTIONALP
		     (SETQ LAMBDA-LIST (CONS (LIST (GENSYM) NIL (GENSYM)) LAMBDA-LIST)))
		    (T
		     (SETQ LAMBDA-LIST (CONS (GENSYM) LAMBDA-LIST)))))
	    '(&REST ARGLIST)))))

(DEFUN ENCAPSULATION-CONVERT-FEF (LL
	&AUX EVARG QUARG EVOPT QUOPT EVREST QUREST)
  ;FIRST DETERMINE WHAT TYPES OF EVALAGE AND QUOTAGE ARE PRESENT (SET ABOVE AUX VARS)
  (DO ((L LL (CDR L))
       (ITEM)
       (SYNTAX)
       (QUOTE))
      ((NULL L))
    (SETQ ITEM (CAR L))
    (AND (BIT-TEST %FEF-NAME-PRESENT ITEM) (SETQ L (CDR L)))
    (SETQ SYNTAX (MASK-FIELD %%FEF-INIT-OPTION ITEM))	;SKIP EXTRA INIT Q
    (OR (= SYNTAX FEF-INI-NONE) (= SYNTAX FEF-INI-NIL) (= SYNTAX FEF-INI-SELF)
	(SETQ L (CDR L)))
    (SETQ SYNTAX (MASK-FIELD %%FEF-ARG-SYNTAX ITEM))
    (SETQ QUOTE (> (MASK-FIELD %%FEF-QUOTE-STATUS ITEM) FEF-QT-EVAL))
    (COND ((> SYNTAX FEF-ARG-REST)
	   (RETURN NIL))
	  ((= SYNTAX FEF-ARG-REST)
	   (COND ((NOT QUOTE)
		  (SETQ EVREST T))
		 ((SETQ QUREST T)))
	   (RETURN NIL))
	  ((= SYNTAX FEF-ARG-OPT)
	   (COND ((NOT QUOTE)
		  (SETQ EVOPT T))
		 ((SETQ QUOPT T))))
	  ((COND ((NOT QUOTE)
		  (SETQ EVARG T))
		 ((SETQ QUARG T))))))
  ;DECIDE HOW HAIRY A LAMBDA LIST IS NEEDED
  (COND ((AND (NOT QUARG) (NOT QUOPT) (NOT QUREST))
	 '(&EVAL &REST ARGLIST))
	((AND (NOT EVARG) (NOT EVOPT) (NOT EVREST))
	 '(&QUOTE &REST ARGLIST))
	(T	;NEED A HAIRY ONE.
	  (NRECONC
	    (DO ((L LL (CDR L))
	         (LAMBDA-LIST NIL)
		 (OQUOTE NIL QUOTE)
		 INIT QUOTE ITEM)
	        ((NULL L) LAMBDA-LIST)
	      (SETQ ITEM (CAR L))
	      (OR (ZEROP (LOGAND %FEF-NAME-PRESENT ITEM)) (SETQ L (CDR L)))
	      (SETQ INIT (MASK-FIELD %%FEF-INIT-OPTION ITEM))	;SKIP EXTRA INIT Q
	      (OR (= INIT FEF-INI-NONE) (= INIT FEF-INI-NIL) (= INIT FEF-INI-SELF)
		  (SETQ L (CDR L)))
	      (SETQ QUOTE (> (MASK-FIELD %%FEF-QUOTE-STATUS ITEM) FEF-QT-EVAL))
	      (OR (EQ QUOTE OQUOTE)
		  (SETQ LAMBDA-LIST (CONS (COND ((NOT QUOTE) '&EVAL)
						(T '&QUOTE))
					  LAMBDA-LIST)))
	      (COND ((= FEF-ARG-REQ (MASK-FIELD %%FEF-ARG-SYNTAX ITEM))
		     (PUSH (GENSYM) LAMBDA-LIST))
		    ((= FEF-ARG-OPT (MASK-FIELD %%FEF-ARG-SYNTAX ITEM))
		     (PUSH (LIST (GENSYM) NIL (GENSYM)) LAMBDA-LIST))
		    (T (RETURN LAMBDA-LIST))))
	    '(&REST ARGLIST)))))

;;; Implement RENAME-WITHIN encapsulations.

;; Rename FUNCTION-TO-RENAME within WITHIN-FUNCTION
;; and make an entry in WITHIN-FUNCTION's encapsulation to record the act.
;; The renamed function is defined by a pointer
;; to the original symbol FUNCTION-TO-RENAME.
;; Return the renamed function name (a symbol).

(defun rename-within-add (within-function function-to-rename)
  (let (tem new
	(default-cons-area background-cons-area))
    (rename-within-init within-function)
    (setq tem (rename-within-renamings-slot within-function))
    (setq new (cadr (assoc function-to-rename (cadr tem))))
    (or new (progn (setq new (make-symbol (string-append "ALTERED-" function-to-rename
							 "-WITHIN-" within-function)))
		   (push (list function-to-rename new)
			 (cadr tem))
		   (rename-within-replace-function new function-to-rename
						   within-function)
		   (fset new function-to-rename)))
    new))

;; List of functions that have had a rename-within encapsulation made.
(defvar rename-within-functions nil)

;; Initialize a rename-within encapsulation on a given function.
;; This can record that within the function's definition
;; one or more other functions should be renamed
;; (that is, be replaced by other names).
;; The encapsulation contains a debugging info item called RENAMINGS
;; whose value is an alist of (function-to-rename new-name).
;; As created by this function, that alist is empty.

(defun rename-within-init (function &aux spec1)
  (setq spec1 (unencapsulate-function-spec function 'rename-within))
  (or (and (fdefinedp spec1)
	   (rename-within-renamings-slot spec1))
      (encapsulate spec1 function 'rename-within `(apply ,encapsulated-function arglist)
		   '((renamings nil)))))

;; Actually replace the function OLD with the function NEW
;; throughout the definition of WITHIN.

(defun rename-within-replace-function (new old within &aux tem)
  (prog* ((spec1 (unencapsulate-function-spec (unencapsulate-function-spec within
									   'rename-within)
					      '(rename-within)))
	  (def (fdefinition spec1)))
    (cond ((listp def)
	   (fdefine spec1 (subst new old def))
	   (and (eq (car def) 'macro) (pop def))
	   (and (not (symbolp def))
		(setq tem (assq 'encapsulated-definition (function-debugging-info def)))
		(rename-within-replace-function new old (cadr tem))))
	  ((typep def ':compiled-function)
	   (let ((len (%structure-boxed-size def)))
	     (dotimes (i len)
	       (let ((dtype (%p-ldb-offset %%q-data-type def i)))
		 (and (= dtype dtp-external-value-cell-pointer)
		      (let ((obj (%p-contents-as-locative-offset def i)))
			(eq obj (locf (fsymeval old))))
		      (let ((inhibit-scheduling-flag t))
			(%p-dpb-offset (%pointer (locf (fsymeval new))) %%q-pointer
				       def i)))))))
	  (t (return nil)))
    (return t)))

;; Given a function spec of the form (:within within-function renamed-function),
;; if such a renaming exists, flush it.
(defun rename-within-maybe-delete (function-spec)
  (let ((within-function (cadr function-spec))
	(renamed-function (caddr function-spec)))
    (and (fdefinedp within-function)
	 (let ((entry (assoc renamed-function
			     (cadr (rename-within-renamings-slot within-function)))))
	   (and entry
		(rename-within-delete within-function renamed-function (cadr entry)))))))

;; Unrename the function ORIGINAL within WITHIN-FUNCTION,
;; replacing the new name RENAMED-NAME with the ORIGINAL name,
;; and removing the renamings entry.
(defun rename-within-delete (within-function original renamed-name)
  (let ((renamingsslot (rename-within-renamings-slot within-function)))
    (rename-within-replace-function original renamed-name within-function)
    (setf (cadr renamingsslot)
	  (delq (assq original (cadr renamingsslot)) (cadr renamingsslot)))
    (or (cadr renamingsslot)
	(rename-within-flush within-function))))

;; Delete the rename-within encapsulation from WITHIN-FUNCTION.
(defun rename-within-flush (within-function &aux def)
  (setq within-function (unencapsulate-function-spec within-function 'rename-within))
  (setq def
	(fdefinition (unencapsulate-function-spec within-function '(rename-within))))
  (and (eq (car (fdefinition within-function)) 'macro)
       (setq def (cons 'macro def)))
  (fset within-function def)
  (setq rename-within-functions
	(delq within-function rename-within-functions)))

;Given a function which has a rename-within encapsulation,
;return the list (:RENAMINGS alist) from the debugging info
;which records which renamings are in effect.
;Given any other sort of function definition, return nil.
(defun rename-within-renamings-slot (function)
  (let ((definition (fdefinition (unencapsulate-function-spec function 'rename-within))))
    (and (not (symbolp definition))
	 (cond ((and (listp definition)
		     (eq (car definition) 'macro))
		(and (not (symbolp (cdr definition)))
		     (assq 'renamings (function-debugging-info (cdr definition)))))
	       (t
		(assq 'renamings (function-debugging-info definition)))))))

(defun (rename-within encapsulation-grind-function) (function def width real-io untyo-p)
  def
  (dolist (entry (cadr (rename-within-renamings-slot function)))
    (grind-1 `(:within ,function ,(car entry)) width real-io untyo-p)))

;Tell the function-spec system about it

;; (:WITHIN within-function renamed-function) refers to renamed-function,
;;   but only as called directly from within-function.
;;   Actually, renamed-function is replaced throughout within-function
;;   by an uninterned symbol whose definition is just renamed-function
;;   as soon as an attempt is made to do anything to a function spec
;;   of this form.  The function spec is from then on equivalent
;;   to that uninterned symbol.
(DEFPROP :WITHIN WITHIN-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN WITHIN-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((WITHIN-FUNCTION (SECOND FUNCTION-SPEC))
	(RENAMED-FUNCTION (THIRD FUNCTION-SPEC)))
    (SELECTQ FUNCTION
      (VALIDATE-FUNCTION-SPEC (AND (= (LENGTH FUNCTION-SPEC) 3)
				   (VALIDATE-FUNCTION-SPEC WITHIN-FUNCTION)
				   (VALIDATE-FUNCTION-SPEC RENAMED-FUNCTION)
				   (FDEFINEDP WITHIN-FUNCTION)))
      (FDEFINE (IF (EQ ARG1 RENAMED-FUNCTION)
		   (RENAME-WITHIN-MAYBE-DELETE FUNCTION-SPEC)
		   (FSET (RENAME-WITHIN-ADD WITHIN-FUNCTION RENAMED-FUNCTION) ARG1)))
      (FDEFINITION (LET* ((DEF (FDEFINITION WITHIN-FUNCTION))
			  (RENAMINGSSLOT (ASSQ 'RENAMINGS (DEBUGGING-INFO DEF)))
			  (ENTRY (CADR (ASSOC RENAMED-FUNCTION (CADR RENAMINGSSLOT)))))
		     (IF ENTRY (FSYMEVAL ENTRY)
			 (FERROR NIL "~S undefined function -- no renaming found"
				     FUNCTION-SPEC))))
      (FDEFINEDP (LET* ((DEF (FDEFINITION WITHIN-FUNCTION))
			(RENAMINGSSLOT (ASSQ 'RENAMINGS (DEBUGGING-INFO DEF)))
			(ENTRY (CADR (ASSOC RENAMED-FUNCTION (CADR RENAMINGSSLOT)))))
		   (AND ENTRY (FBOUNDP ENTRY))))
      (FDEFINITION-LOCATION
		(LOCF (FSYMEVAL (RENAME-WITHIN-ADD WITHIN-FUNCTION RENAMED-FUNCTION))))
	;--- Removes the renaming rather than renaming it to an undefined function
      (FUNDEFINE (RENAME-WITHIN-MAYBE-DELETE FUNCTION-SPEC))
      (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2)))))
