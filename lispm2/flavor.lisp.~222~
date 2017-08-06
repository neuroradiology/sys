; Tasteful Flavors			-*- Mode: Lisp; Package: SI; Base:8 -*-

; A flavor-name is a symbol which names a type of objects defined
; by the combination of several flavors.  The SI:FLAVOR
; property is a data-structure (of type FLAVOR) defining the
; nature of the flavor, as defined below.

; Flavors come in essentially three kinds.  The first kind defines a class
; of flavors, and provides the basic instance variables and methods for
; that class.  This kind typically includes only VANILLA-FLAVOR as a
; component, and uses the :REQUIRED-INSTANCE-VARIABLES and
; :REQUIRED-METHODS options.  The second kind of flavor represents a
; particular option that may be combined in (a "mix-in").  The third
; kind of flavor is the kind that can usefully be instantiated; it is
; a combination of one of the first kind and several of the second kind,
; to achieve the behavior desired for a particular application.

; The following symbols are interesting to outsiders:
; DEFFLAVOR - macro for defining a flavor
; DEFMETHOD - macro for defining a method
; DEFWRAPPER - macro for defining a flavor-wrapper
; INSTANTIATE-FLAVOR - create an object of a specified flavor
; MAKE-INSTANCE - easier to call version of INSTANTIATE-FLAVOR
; COMPILE-FLAVOR-METHODS - macro which does the right thing in the compiler
; RECOMPILE-FLAVOR - function to recompile a flavor and maybe any flavors
;		that depend on it.  Usually this happens automatically.
; DECLARE-FLAVOR-INSTANCE-VARIABLES - macro to put around a function
;		that will be called by methods and wants to access instance
;		variables.
; FUNCALL-SELF - a macro which, assuming you are a flavor instance, will
;		call yourself without bothering about rebinding the
;		variables.  Will do something totally random if SELF
;		isn't a flavor instance.
; LEXPR-FUNCALL-SELF - LEXPR-FUNCALL version of above
; *ALL-FLAVOR-NAMES* - list of all symbols which have been used as the name of a flavor
; *ALL-FLAVOR-NAMES-AARRAY* - completion aarray of flavor names to flavors.
;		Each flavor is included twice, once with and once without its package prefix.
; *FLAVOR-COMPILATIONS* - list of all methods which had to be compiled
;		this is useful for finding flavors which weren't compiled in qfasl files
;		or which need to be recompiled to bring them up to date.
; *FLAVOR-COMPILE-TRACE* - if non-NIL, a FORMAT destination for messages about
;		recompilation of combined methods
; FLAVOR-ALLOWS-INIT-KEYWORD-P - determine whether a certain flavor allows
;		a certain keyword in its init-plist.

; Roads not taken:
;  o Doesn't provide the ability to freeze all existing instances of a 
;    certain flavor; this can probably be managed by removing the flavor
;    from the property list of its name, copying the select-method
;    replacing symbols by their function bindings.
;  o Changing the size of all extant instances of a flavor.
;  o Nothing to stop you from instantiating a flavor of the first or
;    second kind.  In practice you will usually get an error if you try it.

; Philosophy with respect to multiple processes
;  Interrupts are inhibited such that multiple processes munging unrelated
;  flavors should work.  Multiple processes instantiating related flavors
;  will work, however multiple processes defining methods for the same
;  flavor at the same time, and things like that, will not.

; This macro is used to define a flavor.  Use DEFMETHOD to define
; methods (responses to messages sent to an instance of a flavor.)
(DEFMACRO DEFFLAVOR (NAME INSTANCE-VARIABLES COMPONENT-FLAVORS &REST OPTIONS)
  ;INSTANCE-VARIABLES can be symbols, or lists of symbol and initialization.
  ;COMPONENT-FLAVORS are searched from left to right for methods,
  ; and contribute their instance variables.
  ;OPTIONS are:
  ; (:GETTABLE-INSTANCE-VARIABLES v1 v2...) - enables automatic generation of methods
  ;   for retrieving the values of those instance variables
  ; :GETTABLE-INSTANCE-VARIABLES - (the atomic form) does it for all instance
  ;   variables local to this flavor (declared in this DEFFLAVOR).
  ; (:SETTABLE-INSTANCE-VARIABLES v1 v2...) - enables automatic generation of methods
  ;   for changing the values of instance variables
  ;   The atomic form works too.
  ; (:REQUIRED-INSTANCE-VARIABLES v1 v2...) - any flavor incorporating this
  ;   flavor and actually instantiated must have instance variables with
  ;   the specified names.  This is used for defining general types of
  ;   flavors.
  ; (:REQUIRED-METHODS m1 m2...) - any flavor incorporating this
  ;   flavor and actually instantiated must have methods for the specified
  ;   operations.  This is used for defining general types of flavors.
  ; (:REQUIRED-FLAVORS f1 f2...) - similar,  for component flavors
  ;   rather than methods.
  ; (:INITABLE-INSTANCE-VARIABLES v1 v2...) - these instance variables
  ;   may be initialized via the options to INSTANTIATE-FLAVOR.
  ;   The atomic form works too.
  ;   Settable instance variables are also initable.
  ; (:INIT-KEYWORDS k1 k2...) - specifies keywords for the :INIT operation
  ;   which are legal to give to this flavor.  Just used for error checking.
  ; (:DEFAULT-INIT-PLIST k1 v1 k2 v2...) - specifies defaults to be put
  ;   into the init-plist, if the keywords k1, k2, ... are not already
  ;   specified, when instantiating.  The values v1, v2, ... get evaluated
  ;   when and if they are used.
  ; (:DEFAULT-HANDLER function) - causes function to be called if a message
  ;   is sent for which there is no method.  Defaults to a function which
  ;   gives an error.
  ; (:INCLUDED-FLAVORS f1 f2...) - specifies flavors to be included in this
  ;   flavor.  The difference between this and specifying them as components
  ;   is that included flavors go at the end, so they act as defaults.  This
  ;   makes a difference when this flavor is depended on by other flavors.
  ; :NO-VANILLA-FLAVOR - do not include VANILLA-FLAVOR.
  ;   Normally it is included automatically.  This is for esoteric hacks.
  ; (:ORDERED-INSTANCE-VARIABLES v1 v2...) - requires that in any instance,
  ;   instance variables with these names must exist and come first.  This might
  ;   be for instance variable slots specially referenced by microcode.
  ;   The atomic form works too.
  ; (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES v1 v2...) - defines defsubsts which
  ;   act like defstruct accessors for the variables; that is, using these with
  ;   an argument of an instance gets the value of that variable in that instance.
  ;   The name of the defsubst is the flavor-name, hyphen, the variable name.
  ;   If the instance variable is ordered, the accessor will know its index
  ;   in the instance and access it directly, otherwise it will call SYMEVAL-IN-INSTANCE
  ;   at run-time.
  ;   The atomic form works too.
  ; (:ACCESSOR-PREFIX sym) - uses "sym" as the prefix on the names of the above
  ;   defsubsts instead of "flavor-".
  ; (:SELECT-METHOD-ORDER m1 m2...) - specifies that the keywords m1, m2, ... are
  ;   are important and should have their methods first in the select-method
  ;   table for increased efficiency.
  ; (:METHOD-COMBINATION (type order operation1 operation2...)...)
  ;   Specify ways of combining methods from different flavors.  :DAEMON NIL is the
  ;   the default.  order is usually :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST,
  ;   but this depends on type.
  ; (:DOCUMENTATION <args>...)
  ;   The list of args is simply put on the flavor's :DOCUMENTATION property.
  ;   The standard for this is that the arguments may include keyword symbols and
  ;   a documentation string.  To be specified more later.
  ; There may be more.
  `(PROGN 'COMPILE
     (EVAL-WHEN (COMPILE LOAD EVAL)
       ;; Define the flavor, parse up the options.  Needed at both compile and run times.
       (DEFFLAVOR1 ',NAME ',INSTANCE-VARIABLES ',COMPONENT-FLAVORS ',(COPYLIST OPTIONS))
       ;; Make any instance-variable accessor macros, needed at both compile and run times.
       . ,(DO ((VS (DO ((OPTS OPTIONS (CDR OPTS)))
		       ((NULL OPTS) NIL)
		     (AND (LISTP (CAR OPTS))
			  (EQ (CAAR OPTS) ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
			  (RETURN (CDAR OPTS)))
		     (AND (EQ (CAR OPTS) ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
			  (RETURN (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
					  INSTANCE-VARIABLES))))
		   (CDR VS))
	       (PREFIX (OR (CADR (ASSQ-CAREFUL ':ACCESSOR-PREFIX OPTIONS))
			   (STRING-APPEND NAME "-")))
	       (ORDS (DO ((OPTS OPTIONS (CDR OPTS)))
			 ((NULL OPTS) NIL)
		       (AND (LISTP (CAR OPTS))
			    (EQ (CAAR OPTS) ':ORDERED-INSTANCE-VARIABLES)
			    (RETURN (CDAR OPTS)))
		       (AND (EQ (CAR OPTS) ':ORDERED-INSTANCE-VARIABLES)
			    (RETURN (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
					    INSTANCE-VARIABLES)))))
	       (RES NIL (CONS `(DEFSUBST ,(INTERN1 (STRING-APPEND PREFIX (CAR VS))) (,NAME)
				 ,(IF (MEMQ (CAR VS) ORDS)
				      `(%INSTANCE-REF ,NAME
					   ,(1+ (FIND-POSITION-IN-LIST (CAR VS) ORDS)))
				      `(SYMEVAL-IN-INSTANCE ,NAME ',(CAR VS))))
			      RES)))
	      ((NULL VS) RES)))
     ,(AND (BOUNDP 'COMPILER:QC-FILE-IN-PROGRESS)
	   COMPILER:QC-FILE-IN-PROGRESS
	   (NOT COMPILER:QC-FILE-LOAD-FLAG)
	 `(EVAL-WHEN (COMPILE)	;This EVAL-WHEN is just for ordering
	    ;; Compile the automatic instance-variable get/set methods into QFASL file
	    (LET ((*JUST-COMPILING* T))
	      (COMPOSE-AUTOMATIC-METHODS (GET ',NAME 'FLAVOR)))))
     (EVAL-WHEN (LOAD EVAL)
       ;; Verify the existence of the instance-variable get/set methods at load time
       (COMPOSE-AUTOMATIC-METHODS (GET ',NAME 'FLAVOR)))
     ',NAME))

; This wraps a local-declare special of the instance variables around its body.
; It's good for things like defining functions that deal with a flavor but
; are not methods (generally they are called by methods.)
(DEFMACRO DECLARE-FLAVOR-INSTANCE-VARIABLES ((FLAVOR-NAME) &BODY BODY)
  (LET ((SPECIAL-DECLARATION (FLAVOR-SPECIAL-DECLARATION FLAVOR-NAME)))
    (IF SPECIAL-DECLARATION
	`(LOCAL-DECLARE (,SPECIAL-DECLARATION) . ,BODY)
	;Don't do a (LOCAL-DECLARE (NIL) ...)
	`(PROGN 'COMPILE . ,BODY))))


(DEFVAR *ALL-FLAVOR-NAMES* NIL)	;List of names of all flavors (mostly for editor)
(DEFVAR *ALL-FLAVOR-NAMES-AARRAY*	;For editor's completing reader
	(MAKE-ARRAY 2400		;736 flavors in system 75
		    ':TYPE 'ART-Q-LIST
		    ':LEADER-LIST '(0 NIL)))

(ADD-INITIALIZATION "Condense Flavor Name Tables"
		    '(PROGN (ZWEI:SORT-COMPLETION-AARRAY *ALL-FLAVOR-NAMES-AARRAY*)
			    (IF (= (%P-CDR-CODE *ALL-FLAVOR-NAMES*) CDR-NORMAL)
				(SETQ *ALL-FLAVOR-NAMES* (COPYLIST *ALL-FLAVOR-NAMES*))))
		    '(:BEFORE-COLD))

(DEFVAR *JUST-COMPILING* NIL)	;T means putting combined methods into qfasl file,
				; not updating the current flavor data-structure
(DEFVAR *USE-OLD-COMBINED-METHODS* T)	;T means recycle old, NIL means generate new.
					; This is an implicit argument to certain routines.
(DEFVAR *FLAVOR-PENDING-DEPENDS* NIL)	;Used by DEFFLAVOR1
(DEFVAR *FLAVOR-COMPILATIONS* NIL)	;List of methods compiled
(DEFVAR *FLAVOR-COMPILE-TRACE* NIL)

;This is an area in which to cons data internal to the flavor system.  It is used
;rather than default-cons-area as a hedge against temporary area lossage which can
;happen if you do things from an error in a compilation, or if you make instances
;in a temporary area and that requires composing flavors or methods.
(DEFVAR *FLAVOR-AREA* WORKING-STORAGE-AREA)

; These two functions are used when sending a message to yourself, for extra efficiency.
(DEFMACRO FUNCALL-SELF (&REST ARGS)
  `(FUNCALL (%FUNCTION-INSIDE-SELF)
	    . ,ARGS))

(DEFMACRO LEXPR-FUNCALL-SELF (&REST ARGS)
  `(LEXPR-FUNCALL (%FUNCTION-INSIDE-SELF)
		  . ,ARGS))

; The data-structure on the FLAVOR property of a flavor-name
; This must agree with INSTANCE-DESCRIPTOR-OFFSETS in LISPM;QCOM
(DEFSTRUCT (FLAVOR :NAMED :ARRAY (:CONSTRUCTOR MAKE-FLAVOR)
				 (:MAKE-ARRAY (:AREA PERMANENT-STORAGE-AREA)))
  FLAVOR-INSTANCE-SIZE		;1+ the number of instance variables
  FLAVOR-BINDINGS		;List of locatives to instance variable
				; internal value cells.  MUST BE CDR-CODED!!
  FLAVOR-SELECT-METHOD		;This gets called as handler function for instance.
				; NIL means method-combination not composed yet.
  FLAVOR-NAME			;Symbol which is the name of the flavor.
				; This is returned by TYPEP.
  ;; End of magic locations
  FLAVOR-LOCAL-INSTANCE-VARIABLES	;Names and initializations,
					; does not include inherited ones.
  FLAVOR-ALL-INSTANCE-VARIABLES	;Just names, only valid when flavor-combination composed.
				; Corresponds directly to FLAVOR-BINDINGS and the instances.
  FLAVOR-METHOD-TABLE		;Defined below.
  FLAVOR-DEPENDS-ON		;List of names of flavors incorporated into this flavor.
  FLAVOR-DEPENDED-ON-BY		;List of names of flavors which incorporate this one.
				;The above are only immediate dependencies.
  FLAVOR-INCLUDES		;List of names of flavors to include at the end
				; rather than as immediate depends-on's.
  FLAVOR-PACKAGE		;Package in which the DEFFLAVOR was done.
  FLAVOR-DEPENDS-ON-ALL		;Names of all flavors depended on, to all levels, including
				; this flavor itself.  NIL means flavor-combination not
				; composed yet.  This is used by TYPEP of 2 arguments.
  (FLAVOR-WHICH-OPERATIONS NIL)	;List of operations handled, created when needed.
				; This is NIL if it has not been computed yet.
  (FLAVOR-GETTABLE-INSTANCE-VARIABLES NIL)	;List of them
  (FLAVOR-SETTABLE-INSTANCE-VARIABLES NIL)	;List of them
  (FLAVOR-INITABLE-INSTANCE-VARIABLES NIL)
				;Alist from init keyword to name of variable
  (FLAVOR-INIT-KEYWORDS NIL)			;option
  (FLAVOR-PLIST NIL)		;Esoteric things stored here as properties
				;Known: :DEFAULT-HANDLER, :ORDERED-INSTANCE-VARIABLES,
				; :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES, :ACCESSOR-PREFIX,
				; :REQUIRED-INSTANCE-VARIABLES, :REQUIRED-METHODS,
				; :REQUIRED-FLAVORS, :SELECT-METHOD-ORDER,
				; :DEFAULT-INIT-PLIST, :DOCUMENTATION, :NO-VANILLA-FLAVOR
				; ADDITIONAL-SPECIAL-VARIABLES
				; COMPILE-FLAVOR-METHODS
				;The convention on these is supposed to be that
				;ones in the keyword packages are allowed to be
				;used by users.
				;Some of these are not used by the flavor system, they are
				;just remembered on the plist in case anyone cares.  The
				;flavor system does all its handling of them during the
				;expansion of the DEFFLAVOR macro.
  )

;Named-structure handler for above structure, to make it print nicer
(DEFUN (FLAVOR NAMED-STRUCTURE-INVOKE) (OPERATION &OPTIONAL SELF &REST ARGS)
  (SELECTQ OPERATION
    (:WHICH-OPERATIONS '(:PRINT-SELF :DESCRIBE))
    ((:PRINT-SELF)
     (SI:PRINTING-RANDOM-OBJECT (SELF (CAR ARGS))
       (FORMAT (CAR ARGS) "FLAVOR ~S" (FLAVOR-NAME SELF))))
    (:DESCRIBE (DESCRIBE-FLAVOR SELF))
    (OTHERWISE (FERROR NIL "~S unknown" OPERATION))))

;Format of flavor-method-table:
; New format of a flavor-method-table entry is:
;   (message combination-type combination-order meth...)
; A meth is:
;   (function-spec definition plist)
; Thus the second element of a meth is actually a function-cell.
; The meth's are stored in permanent-storage-area so that they will be compact.
;    [That might not be the best area, the select-methods, and component
;     lists, and instanc-variable lists, and which-operations's, are also there.]
; A magic-list entry is:
;   (message combination-type combination-order (method-type function-spec...)...)
; In the magic-list, there can be more than one method listed under a method-type,
; the base flavor always comes first.  The :COMBINED methods are elided from
; the magic-list.
;
; Special method-types:
;   NIL - no type specified
;   :DEFAULT - like NIL but only taken if there are no type-NIL methods
;   :WRAPPER - wrappers are remembered this way
;   :COMBINED - a daemon-caller; the symbol has a COMBINED-METHOD-DERIVATION property
;		whose value is the complete method table entry from the magic-list.
;		The CDDDR is canonicalized; each contained list of method symbols is
;		of course ordered by the order in which flavors are combined (base
;		flavor first).  Canonical order is alphabetical by method-type.
; Non-special method-types:
;   :BEFORE, :AFTER - these are used by the default combination-type, :DAEMON
;
; Special hair for wrappers: changing a wrapper can invalidate the combined method
; without changing anything in the flavor-method-table entry.  Rather than having
; it automatically recompile, which turns out to be a pain when the wrapper was
; just reloaded or changed trivially, it will fail to recompile and you must use
; RECOMPILE-FLAVOR with a 3rd argument of NIL.
;
; A combination-type of NIL means it has not been explicitly specified.

; Method-combination functions.  Found on the SI:METHOD-COMBINATION property
; of the combination-type.  These are passed the flavor structure, and the
; magic-list entry, and must return the symbol to go into the select-method,
; defining any necessary functions.  This function interprets combination-type-arg,
; which for many combination-types is either :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST.

;Definitions of a meth (the datum which stands for a method)

(DEFSTRUCT (METH :LIST :CONC-NAME (:CONSTRUCTOR NIL))
		;No constructor because defstruct doesn't let me specify the area
  FUNCTION-SPEC
  DEFINITION
  (PLIST NIL))

; If there is no definition, it contains DTP-NULL and a pointer to the meth

; Extract the method-type of a meth
(DEFSUBST METH-METHOD-TYPE (METH)
  (AND (CDDDR (METH-FUNCTION-SPEC METH))
       (THIRD (METH-FUNCTION-SPEC METH))))

; Return a meth of specified type from a list of meth's.
(DEFUN METH-LOOKUP (METHOD-TYPE METH-LIST)
  (LOOP FOR METH IN METH-LIST
	WHEN (EQ (METH-METHOD-TYPE METH) METHOD-TYPE)
	  RETURN METH))

(DEFUN NULLIFY-METHOD-DEFINITION (METH)
  (LET ((P (LOCF (METH-DEFINITION METH))))
    (%P-STORE-TAG-AND-POINTER P (+ (LSH (%P-CDR-CODE P) 6) DTP-NULL) METH)))

(DEFUN METH-DEFINEDP (METH)
  ( (%P-DATA-TYPE (LOCF (METH-DEFINITION METH))) DTP-NULL))

(DEFUN METHOD-PLIST (FUNCTION-SPEC)		;For debugging ease only
  (METH-PLIST (FLAVOR-METHOD-ENTRY FUNCTION-SPEC T)))

(DEFPROP DEFFLAVOR "Flavor" DEFINITION-TYPE-NAME)

;Function to define or redefine a flavor (used by DEFFLAVOR macro).
;Note that to ease initialization problems, the flavors depended upon need
;not be defined yet.  You will get an error the first time you try to create
;an instance of this flavor if a flavor it depends on is still undefined.
;When redefining a flavor, we reuse the same FLAVOR defstruct so that
;old instances continue to get the latest methods, unless you change
;something incompatibly, in which case you will get a warning.
(DEFUN DEFFLAVOR1 (FLAVOR-NAME INSTANCE-VARIABLES COMPONENT-FLAVORS OPTIONS
		   &AUX FFL ALREADY-EXISTS INSTV IDENTICAL-COMPONENTS
			GETTABLE SETTABLE INITABLE INIT-KEYWORDS INCLUDES METH-COMB
			NEW-PLIST (PL (LOCF NEW-PLIST))
			(DEFAULT-CONS-AREA *FLAVOR-AREA*))
  (RECORD-SOURCE-FILE-NAME FLAVOR-NAME 'DEFFLAVOR)
  (WITHOUT-INTERRUPTS
    (COND ((NOT (MEMQ FLAVOR-NAME *ALL-FLAVOR-NAMES*))
	   (PUSH FLAVOR-NAME *ALL-FLAVOR-NAMES*)
	   ;; Push on the name without the package prefix.
	   (ARRAY-PUSH-EXTEND *ALL-FLAVOR-NAMES-AARRAY*
			      (CONS (GET-PNAME FLAVOR-NAME) FLAVOR-NAME))
	   ;; Push on the name with the package prefix.
	   (ARRAY-PUSH-EXTEND *ALL-FLAVOR-NAMES-AARRAY*
			      (LET ((PACKAGE NIL))
				(CONS (FORMAT NIL "~S" FLAVOR-NAME) FLAVOR-NAME)))
	   ;; Array is no longer sorted.
	   (STORE-ARRAY-LEADER NIL *ALL-FLAVOR-NAMES-AARRAY* 1))))
  ;; Analyze and error check the instance-variable and component-flavor lists
  (SETQ INSTV (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X))) INSTANCE-VARIABLES))
  (DOLIST (IV INSTV)
    (IF (OR (NULL IV) (NOT (SYMBOLP IV)))
	(FERROR NIL "~S, which is not a symbol, was specified as an instance variable" IV)))
  (DOLIST (CF COMPONENT-FLAVORS)
    (IF (OR (NULL CF) (NOT (SYMBOLP CF)))
	(FERROR NIL "~S, which is not a symbol, was specified as a component flavor" CF)))
  ;; Certain properties are inherited from the old property list, while
  ;; others are generated afresh each time from the defflavor-options.
  (COND ((SETQ ALREADY-EXISTS (GET FLAVOR-NAME 'FLAVOR))
	 (LOOP FOR (IND VAL) ON (FLAVOR-PLIST ALREADY-EXISTS) BY 'CDDR
	       UNLESS (MEMQ IND '(:DEFAULT-HANDLER :ORDERED-INSTANCE-VARIABLES
				  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES :ACCESSOR-PREFIX
				  :REQUIRED-INSTANCE-VARIABLES :REQUIRED-METHODS
				  :REQUIRED-FLAVORS :SELECT-METHOD-ORDER
				  :DEFAULT-INIT-PLIST :DOCUMENTATION :NO-VANILLA-FLAVOR))
	         DO (PUTPROP PL VAL IND))))
  ;; First, parse all the defflavor options into local variables so we can see
  ;; whether the flavor is being redefined incompatibly.
  (DO ((L OPTIONS (CDR L))
       (OPTION) (ARGS))
      ((NULL L))
    (IF (ATOM (CAR L))
	(SETQ OPTION (CAR L) ARGS NIL)
	(SETQ OPTION (CAAR L) ARGS (CDAR L)))
    (SELECTQ OPTION
	(:GETTABLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ GETTABLE (OR ARGS INSTV)))
	(:SETTABLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ SETTABLE (OR ARGS INSTV)))
	(:INITABLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ INITABLE (OR ARGS INSTV)))
	(:INIT-KEYWORDS
	  (SETQ INIT-KEYWORDS ARGS))
	(:INCLUDED-FLAVORS
	  (SETQ INCLUDES ARGS))
	(:NO-VANILLA-FLAVOR
	  (PUTPROP PL T OPTION))
	(:ORDERED-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (PUTPROP PL (OR ARGS INSTV) ':ORDERED-INSTANCE-VARIABLES))
	(:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (PUTPROP PL (OR ARGS INSTV) ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES))
	(:METHOD-COMBINATION
	  (SETQ METH-COMB ARGS))
	(:DEFAULT-HANDLER
	  (PUTPROP PL (CAR ARGS) OPTION))
	((:REQUIRED-INSTANCE-VARIABLES :REQUIRED-METHODS :REQUIRED-FLAVORS :DOCUMENTATION
	  :DEFAULT-INIT-PLIST :SELECT-METHOD-ORDER :ACCESSOR-PREFIX)
	  (PUTPROP PL ARGS OPTION))
	(OTHERWISE (FERROR NIL "~S unknown option to DEFFLAVOR" OPTION))))
  ;; All settable instance variables should also be gettable and initable.
  (DOLIST (V SETTABLE)
    (OR (MEMQ V GETTABLE)
	(PUSH V GETTABLE))
    (OR (MEMQ V INITABLE)
	(PUSH V INITABLE)))
  ;; See whether there are any changes in component flavor structure from last time
  (SETQ IDENTICAL-COMPONENTS
	(AND ALREADY-EXISTS
	     (EQUAL COMPONENT-FLAVORS (FLAVOR-DEPENDS-ON ALREADY-EXISTS))
	     (EQUAL INCLUDES (FLAVOR-INCLUDES ALREADY-EXISTS))))
  ;; If the flavor is being redefined, and the number or order of instance variables
  ;; is being changed, and this flavor or any that depends on it
  ;; has a select-method table (i.e. has probably been instantiated), give a warning
  ;; and disconnect from the old FLAVOR defstruct so that old instances will
  ;; retain the old information.  The instance variables can get changed either
  ;; locally or by rearrangement of the component flavors.
  (AND ALREADY-EXISTS
       (NOT (AND (EQUAL (GET PL ':ORDERED-INSTANCE-VARIABLES)
			(GET (LOCF (FLAVOR-PLIST ALREADY-EXISTS))
			     ':ORDERED-INSTANCE-VARIABLES))
		 (OR (EQUAL (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
			    INSTANCE-VARIABLES)
		     (EQUAL (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
				    (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS))
			    INSTV))
		 (OR IDENTICAL-COMPONENTS
		     (EQUAL (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
							COMPONENT-FLAVORS INCLUDES)
			    (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
							(FLAVOR-DEPENDS-ON ALREADY-EXISTS)
							(FLAVOR-INCLUDES ALREADY-EXISTS))))))
       (SETQ ALREADY-EXISTS (PERFORM-FLAVOR-REDEFINITION FLAVOR-NAME)))
  ;; Make the information structure unless the flavor already exists.
  (LET ((FL (OR ALREADY-EXISTS (MAKE-FLAVOR FLAVOR-NAME FLAVOR-NAME))))
    (SETF (FLAVOR-PACKAGE FL) PACKAGE)
    (SETF (FLAVOR-LOCAL-INSTANCE-VARIABLES FL) INSTANCE-VARIABLES)
    (SETF (FLAVOR-DEPENDS-ON FL) COMPONENT-FLAVORS)
    (SETF (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL) GETTABLE)
    (SETF (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL) SETTABLE)
    (SETF (FLAVOR-INITABLE-INSTANCE-VARIABLES FL)
	  (LOOP FOR V IN INITABLE COLLECT (CONS (CORRESPONDING-KEYWORD V) V)))
    (SETF (FLAVOR-INIT-KEYWORDS FL) INIT-KEYWORDS)
    (SETF (FLAVOR-INCLUDES FL) INCLUDES)
    (SETF (FLAVOR-PLIST FL) NEW-PLIST)
    (DOLIST (DECL METH-COMB)
      (LET ((TYPE (CAR DECL)) (ORDER (CADR DECL)) ELEM)
	;; Don't error-check TYPE now, its definition might not be loaded yet
	(DOLIST (MSG (CDDR DECL))
	  (OR (SETQ ELEM (ASSQ MSG (FLAVOR-METHOD-TABLE FL)))
	      (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) (FLAVOR-METHOD-TABLE FL)))
	  (SETF (SECOND ELEM) TYPE)
	  (SETF (THIRD ELEM) ORDER))))
    ;; Make this a depended-on-by of its depends-on, or remember to do it later in
    ;; the case of depends-on's not yet defined.
    (DOLIST (COMPONENT-FLAVOR COMPONENT-FLAVORS)
      (WITHOUT-INTERRUPTS
	(COND ((SETQ FFL (GET COMPONENT-FLAVOR 'FLAVOR))
	       (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
		   (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
	      (T (PUSH (CONS COMPONENT-FLAVOR FLAVOR-NAME) *FLAVOR-PENDING-DEPENDS*)))))
    ;; Likewise for its includes
    (DOLIST (INCLUDED-FLAVOR (FLAVOR-INCLUDES FL))
      (WITHOUT-INTERRUPTS
	(COND ((SETQ FFL (GET INCLUDED-FLAVOR 'FLAVOR))
	       (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
		   (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
	      (T (PUSH (CONS INCLUDED-FLAVOR FLAVOR-NAME) *FLAVOR-PENDING-DEPENDS*)))))
    ;; If someone depends on this flavor, which wasn't defined until now, link them up.
    (WITHOUT-INTERRUPTS
      (DOLIST (X *FLAVOR-PENDING-DEPENDS*)
	(COND ((EQ (CAR X) FLAVOR-NAME)
	       (OR (MEMQ (CDR X) (FLAVOR-DEPENDED-ON-BY FL))
		   (PUSH (CDR X) (FLAVOR-DEPENDED-ON-BY FL)))
	       (SETQ *FLAVOR-PENDING-DEPENDS* (DELQ X *FLAVOR-PENDING-DEPENDS*))))))
    (PUTPROP FLAVOR-NAME FL 'FLAVOR)
    ;; Now, if the flavor was redefined in a way that changes the methods but doesn't
    ;; invalidate old instances, we have to propagate some changes.
    (AND ALREADY-EXISTS
	 (NOT IDENTICAL-COMPONENTS)
	 (PERFORM-FLAVOR-METHOD-ONLY-REDEFINITION FLAVOR-NAME))
    FLAVOR-NAME))

;Check for typos in user-specified lists of instance variables.
;This assumes that only locally-specified (not inherited) instance variables
;may be mentioned in DEFFLAVOR declaration clauses.
(DEFUN VALIDATE-INSTANCE-VARIABLES-SPEC (VARS-SPECD VARS-ALLOWED FLAVOR-NAME OPTION &AUX BAD)
  (DOLIST (VAR VARS-SPECD)
    (OR (MEMQ VAR VARS-ALLOWED) (PUSH VAR BAD)))
  (COND (BAD (FORMAT ERROR-OUTPUT "~&ERROR: Flavor ~S has misspelled :~A " FLAVOR-NAME OPTION)
	     (FORMAT:PRINT-LIST ERROR-OUTPUT "~S" (NREVERSE BAD)))))

;List of those components which affect the names, number, and ordering of the
;instance variables.  Don't worry about undefined components, by definition
;they must be different from the already-existing flavor, so the right
;thing will happen.  (I wonder what that comment means?  Undefined components
;will not even appear in the list.)
(DEFUN FLAVOR-RELEVANT-COMPONENTS (FL COMPONENT-FLAVORS INCLUDED-FLAVORS)
  (BIND (LOCF (FLAVOR-DEPENDS-ON FL)) COMPONENT-FLAVORS)
  (BIND (LOCF (FLAVOR-INCLUDES FL)) INCLUDED-FLAVORS)
  (DEL-IF-NOT #'(LAMBDA (FLAVOR)		;Splice out the uninteresting ones
		  (FLAVOR-LOCAL-INSTANCE-VARIABLES (GET FLAVOR 'FLAVOR)))
	      (COMPOSE-FLAVOR-INCLUSION (FLAVOR-NAME FL) NIL)))

;Return new copy of the FLAVOR defstruct, and propagate to those that depend on it.
;*** This should hack the old flavor's select-method and combined-methods ***
;*** so that the old methods will be called directly, not via symbols ***
(DEFUN PERFORM-FLAVOR-REDEFINITION (FLAVOR-NAME &AUX FL NFL)
  (SETQ FL (GET FLAVOR-NAME 'FLAVOR))
  (COND ((FLAVOR-SELECT-METHOD FL)
	 (SETQ NFL (MAKE-FLAVOR))
	 (COPY-ARRAY-CONTENTS FL NFL)
	 (SETQ FL NFL)
	 (SETF (FLAVOR-PLIST FL) (COPYLIST (FLAVOR-PLIST FL) PROPERTY-LIST-AREA))
	 (PUTPROP FLAVOR-NAME FL 'FLAVOR)
	 (FORMAT ERROR-OUTPUT "~&Flavor ~S changed incompatibly, old instances will not get the new version.~%"
		 FLAVOR-NAME)))
  (SETF (FLAVOR-INSTANCE-SIZE FL) NIL)	;Defuse error check
  (SETF (FLAVOR-DEPENDS-ON-ALL FL) NIL)	;Will need to be flavor-composed again
  (SETF (FLAVOR-SELECT-METHOD FL) NIL)	;Will need to be method-composed again
  (SETF (FLAVOR-WHICH-OPERATIONS FL) NIL)
  (DOLIST (FN (FLAVOR-DEPENDED-ON-BY FL))
    (PERFORM-FLAVOR-REDEFINITION FN))
  FL)

;This one is when the old instances don't have to be discarded, but recomposition
;does have to occur because something was changed in the order of flavor combination
(DEFUN PERFORM-FLAVOR-METHOD-ONLY-REDEFINITION (FLAVOR-NAME &AUX FL)
  (SETQ FL (GET FLAVOR-NAME 'FLAVOR))
  (IF (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (IF (FLAVOR-SELECT-METHOD FL) (COMPOSE-METHOD-COMBINATION FL))
  (LET ((FDEFINE-FILE-PATHNAME NIL))	;Don't give warnings for combined methods
    (DOLIST (FN (FLAVOR-DEPENDED-ON-BY FL))
      (PERFORM-FLAVOR-METHOD-ONLY-REDEFINITION FN))))

(DEFUN DESCRIBE-FLAVOR (FLAVOR-NAME &AUX FL)
  (CHECK-ARG FLAVOR-NAME (EQ 'FLAVOR (TYPEP (SETQ FL (IF (SYMBOLP FLAVOR-NAME)
							 (GET FLAVOR-NAME 'FLAVOR)
							 FLAVOR-NAME))))
	     "a flavor or the name of one")
  (FORMAT T "~&Flavor ~S directly depends on flavors: ~:[none~;~1G~{~S~^, ~}~]~%"
	    FLAVOR-NAME (FLAVOR-DEPENDS-ON FL))
  (AND (FLAVOR-INCLUDES FL)
       (FORMAT T " and directly includes ~{~S~^, ~}~%" (FLAVOR-INCLUDES FL)))
  (AND (FLAVOR-DEPENDED-ON-BY FL)
       (FORMAT T " and is directly depended on by ~{~S~^, ~}~%" (FLAVOR-DEPENDED-ON-BY FL)))
  (AND (FLAVOR-DEPENDS-ON-ALL FL)	;If this has been computed, show it
       (FORMAT T " and directly or indirectly depends on ~{~S~^, ~}~%"
	         (FLAVOR-DEPENDS-ON-ALL FL)))
  (AND (FLAVOR-INSTANCE-SIZE FL)	;If has been composed
       (FORMAT T "Flavor ~S has instance size ~D, instance variables ~:S~%"
	         FLAVOR-NAME (FLAVOR-INSTANCE-SIZE FL) (FLAVOR-ALL-INSTANCE-VARIABLES FL)))
  (COND ((NOT (NULL (FLAVOR-METHOD-TABLE FL)))
	 (FORMAT T "Not counting inherited methods, the methods for ~S are:~%" FLAVOR-NAME)
	 (DOLIST (M (FLAVOR-METHOD-TABLE FL))
	   (FORMAT T "   ")
	   (DO ((TPL (CDDDR M) (CDR TPL))) ((NULL TPL))
	     (FORMAT T "~@[:~A ~]:~A~:[~;, ~]"
		       (METH-METHOD-TYPE (CAR TPL)) (CAR M) (CDR TPL)))
	   (AND (CADR M)
		(FORMAT T "    :~A~@[ :~A~]" (CADR M) (CADDR M)))
	   (TERPRI))))
  (AND (FLAVOR-ALL-INSTANCE-VARIABLES FL)
       (FORMAT T "Instance variables: ~{~S~^, ~}~%" (FLAVOR-ALL-INSTANCE-VARIABLES FL)))
  (AND (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL)
       (FORMAT T "Automatically-generated methods to get instance variables: ~{~S~^, ~}~%"
	         (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL)))
  (AND (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL)
       (FORMAT T "Automatically-generated methods to set instance variables: ~{~S~^, ~}~%"
	         (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL)))
  (AND (FLAVOR-INITABLE-INSTANCE-VARIABLES FL)
       (FORMAT T "Instance variables that may be set by initialization: ~{~S~^, ~}~%"
	         (MAPCAR #'CDR (FLAVOR-INITABLE-INSTANCE-VARIABLES FL))))
  (AND (FLAVOR-INIT-KEYWORDS FL)
       (FORMAT T "Keywords in the :INIT message handled by this flavor: ~{~S~^, ~}~%"
	         (FLAVOR-INIT-KEYWORDS FL)))
  (FORMAT T "Defined in package ~A~%" (FLAVOR-PACKAGE FL))
  (COND ((FLAVOR-PLIST FL)
	 (FORMAT T "Properties:~%")
	 (DO L (FLAVOR-PLIST FL) (CDDR L) (NULL L)
	   (FORMAT T "~5X~S:	~S~%" (CAR L) (CADR L)))))
  (COND ((NULL (FLAVOR-SELECT-METHOD FL))
	 (FORMAT T "Flavor ~S does not yet have a select-method table~%" FLAVOR-NAME))
	(T (FORMAT T "Flavor ~S has select-method table:~%" FLAVOR-NAME)
	   (DESCRIBE (FLAVOR-SELECT-METHOD FL)))))

;; This is the standard way of defining a method of a class,
;; so that the code will be compiled.  Note that DEFMETHOD works for
;; both Class methods and Flavor methods.
;; If in place of the lambda-list you have a symbol, and the body
;; is null, that symbol is a function which stands in for the method.
(DEFMACRO DEFMETHOD (SPEC LAMBDA-LIST . BODY)
  (LET ((CLASS-NAME (CAR SPEC))
	(FUNCTION-SPEC (CONS ':METHOD SPEC)))
    `(PROGN 'COMPILE
       ;; At compile-time, add enough information so that combined-methods
       ;; can be compiled.  But don't recompile the flavor now, and don't define
       ;; methods interpretively.  Assume that the output of this compilation
       ;; will get loaded, so that the method is defined, before the flavor
       ;; next gets compiled, so that undefined methods don't get called.
       ,(AND (GET CLASS-NAME 'FLAVOR)
	     COMPILER:QC-FILE-IN-PROGRESS
	     (NEQ CLASS-NAME 'VANILLA-FLAVOR)	;This kludge avoids bootstrapping problems!
	     `(EVAL-WHEN (COMPILE)
		 (FLAVOR-NOTICE-METHOD ',FUNCTION-SPEC)))
       ;; At load-time, define the method function
       ,(COND ((AND (SYMBOLP LAMBDA-LIST) (NOT (NULL LAMBDA-LIST)) (NULL BODY))
	       `(FDEFINE ',FUNCTION-SPEC ',LAMBDA-LIST))
	      ((GET CLASS-NAME 'FLAVOR)
	       `(LOCAL-DECLARE (,(AND (NEQ CLASS-NAME 'VANILLA-FLAVOR) ;Bootstrap kludge
				      (FLAVOR-SPECIAL-DECLARATION CLASS-NAME)))
		  (DEFUN ,FUNCTION-SPEC (OPERATION . ,LAMBDA-LIST)
		    . ,BODY)))
	      (T ;; The non-flavor class system
		(IF (NOT (NULL (CDDR SPEC)))
		    (FERROR NIL "~S is not a flavor" (CAR SPEC)))
		(LET ((OPERATION (CADR SPEC)))
		  (COND ((ATOM OPERATION)
			 `(PROGN 'COMPILE
				 . ,(DEFMETHOD-1 CLASS-NAME OPERATION LAMBDA-LIST BODY)))
			(T
			  (COND ((EQ (CAR OPERATION) 'QUOTE)
				 (CERROR NIL NIL ':NO-VALUE
				     "Quote used in front of operation ~S in DEFMETHOD of ~S"
				     OPERATION CLASS-NAME)))
			  `(PROGN 'COMPILE
				  . ,(MAPCAN #'(LAMBDA (OP)
						 (DEFMETHOD-1 CLASS-NAME OP LAMBDA-LIST BODY))
					     OPERATION))))))))))

; This lets you specify code to be wrapped around the invocation of the
; various methods for an operation.  For example,
; (DEFWRAPPER (FOO-FLAVOR :OPERATION) ((ARG1 ARG2) . BODY)
;   `(WITH-FOO-LOCKED (SELF)
;      (PRE-FROBULATE SELF ARG1 ARG2)
;      ,@BODY
;      (POST-FROBULATE SELF ARG2 ARG1)))
;Note that the wrapper needs to be defined at both compile and run times
;so that compiling combined methods as part of the qfasl file works.
(DEFMACRO DEFWRAPPER ((FLAVOR-NAME OPERATION) (DEFMACRO-LAMBDA . GUTS)
		      &BODY BODY)
  (LET ((FUNCTION-SPEC `(:METHOD ,FLAVOR-NAME :WRAPPER ,OPERATION)))
    `(PROGN 'COMPILE
	 ;; At compile-time, add enough information so that combined-methods
	 ;; can be compiled.  The compile-time definition of macros does not
	 ;; go through FDEFINE, so this is necessary to record the existence
	 ;; of the wrapper.
	 ,(AND (GET FLAVOR-NAME 'FLAVOR)
	       COMPILER:QC-FILE-IN-PROGRESS
	       `(EVAL-WHEN (COMPILE)
		   (FLAVOR-NOTICE-METHOD ',FUNCTION-SPEC)))
	 (EVAL-WHEN (COMPILE LOAD EVAL) ;Wrapper defs needed to stay around between files
	   ;; The following optimization could go away if defmacro was made very smart
	   ,(IF (AND (SYMBOLP DEFMACRO-LAMBDA) (STRING-EQUAL DEFMACRO-LAMBDA 'IGNORE))
	       `(DEFMACRO ,FUNCTION-SPEC (IGNORE . ,GUTS)
		  . ,BODY)
	       `(DEFMACRO ,FUNCTION-SPEC (ARGLISTNAME . ,GUTS)
		  `(DESTRUCTURING-BIND ,',DEFMACRO-LAMBDA (CDR ,ARGLISTNAME)
					     ,,@BODY)))))))

;This just exists to be called at compile-time from the DEFMETHOD macro,
;so that any combined methods generated by COMPILE-FLAVOR-METHODS will
;know that this method will be around at run time and should be called.
(DEFUN FLAVOR-NOTICE-METHOD (FUNCTION-SPEC)
  (AND (BOUNDP 'COMPILER:FUNCTIONS-DEFINED)
       (PUSH FUNCTION-SPEC COMPILER:FUNCTIONS-DEFINED))
  (FLAVOR-METHOD-ENTRY FUNCTION-SPEC))

;Find or create a method-table entry for the specified method.
(DEFUN FLAVOR-METHOD-ENTRY (FUNCTION-SPEC &OPTIONAL DONT-CREATE)
  (LET ((FLAVOR-NAME (SECOND FUNCTION-SPEC))
	(TYPE (THIRD FUNCTION-SPEC))
	(MESSAGE (FOURTH FUNCTION-SPEC)))
    (IF (NULL MESSAGE) (SETQ MESSAGE TYPE TYPE NIL))	;If no type
    (IF (OR (NULL MESSAGE) (NEQ (FIRST FUNCTION-SPEC) ':METHOD) (> (LENGTH FUNCTION-SPEC) 4)
	    (NOT (SYMBOLP FLAVOR-NAME)) (NOT (SYMBOLP TYPE)) (NOT (SYMBOLP MESSAGE)))
	(FERROR NIL "~S is not a valid function-spec" FUNCTION-SPEC))
    (LET* ((FL (GET FLAVOR-NAME 'FLAVOR))
	   (MTE (ASSQ MESSAGE (FLAVOR-METHOD-TABLE FL))))
      (OR MTE DONT-CREATE
	  ;; Message not previously known about, put into table
	  (PUSH (SETQ MTE (LIST* MESSAGE NIL NIL NIL)) (FLAVOR-METHOD-TABLE FL)))
      ;; Message known, search for the type entry
      (OR (METH-LOOKUP TYPE (CDDDR MTE))
	  (AND (NOT DONT-CREATE)
	       ;; Type not known, create a new meth with an unbound definition cell
	       (LET ((METH (LIST-IN-AREA PERMANENT-STORAGE-AREA FUNCTION-SPEC NIL NIL)))
		 (NULLIFY-METHOD-DEFINITION METH)
		 (PUSH METH (CDDDR MTE))
		 (VALUES METH T)))))))

;;; See if a certain method exists in a flavor
(DEFUN FLAVOR-METHOD-EXISTS (FL TYPE OPERATION &AUX MTE)
  (AND (SETQ MTE (ASSQ OPERATION (FLAVOR-METHOD-TABLE FL)))
       (METH-LOOKUP TYPE (CDDDR MTE))))

;;; Forcibly remove a method definition from a flavor's method table
;;; Syntax is identical to the beginning of a defmethod for the same method.
(DEFMACRO UNDEFMETHOD (SPEC)
  `(FUNDEFINE '(:METHOD . ,SPEC)))

;;; Interface to function-spec system
;; (:METHOD class-name operation) refers to the method in that class for
;;   that operation; this works for both Class methods and Flavor methods.
;;   In the case of Flavor methods, the specification may also be of the form
;;   (:METHOD flavor-name method-type operation).
(DEFPROP :METHOD METHOD-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN METHOD-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((FLAVOR (SECOND FUNCTION-SPEC))
	(METHOD-TYPE (THIRD FUNCTION-SPEC))
	(MESSAGE (FOURTH FUNCTION-SPEC)))
    (IF (NULL (CDDDR FUNCTION-SPEC))
	(SETQ MESSAGE (THIRD FUNCTION-SPEC) METHOD-TYPE NIL))
    (COND ((AND (SYMBOLP FLAVOR) (GET FLAVOR 'FLAVOR))
	   (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
	       (AND (SYMBOLP METHOD-TYPE) (SYMBOLP MESSAGE) ( 3 (LENGTH FUNCTION-SPEC) 4))
	       (LET ((FL (GET FLAVOR 'FLAVOR))
		     (METH (FLAVOR-METHOD-ENTRY FUNCTION-SPEC 
						(NOT (MEMQ FUNCTION '(FDEFINE PUTPROP))))))
		 (OR (NOT (NULL METH))
		     (MEMQ FUNCTION '(FDEFINEDP COMPILER-FDEFINEDP GET FUNCTION-PARENT))
		     (FERROR NIL "~S is not a defined method; it is not possible to ~S it"
			         FUNCTION-SPEC FUNCTION))
		 (SELECTQ FUNCTION
		   (FDEFINE
		     (LET ((NEW-DEFINITION (NOT (METH-DEFINEDP METH))))
		       (SETF (METH-DEFINITION METH) ARG1)
		       ;; Incrementally recompile the flavor if this is a new method, unless
		       ;; it is a :COMBINED method, which is the result of compilation,
		       ;; not a client of it.
		       (AND NEW-DEFINITION
			    (NEQ METHOD-TYPE ':COMBINED)
			    (RECOMPILE-FLAVOR FLAVOR MESSAGE))))
		   (FDEFINITION (METH-DEFINITION METH))
		   (FDEFINEDP (AND METH (METH-DEFINEDP METH)))
		   (FDEFINITION-LOCATION (LOCF (METH-DEFINITION METH)))
		   (FUNDEFINE
		     (LET ((MTE (ASSQ MESSAGE (FLAVOR-METHOD-TABLE FL))) TEM)
		       (SETF (CDDDR MTE) (DELQ METH (CDDDR MTE)))	;Remove this method
		       (IF (OR (NULL (CDDDR MTE))	;No methods left for this operation?
							;or just a worthless combined method?
			       (AND (= (LENGTH (CDDDR MTE)) 1)
				    (EQ (METH-METHOD-TYPE (CADDDR MTE)) ':COMBINED)
				    (= (LENGTH (SETQ TEM
						 (CDDDR (FUNCTION-SPEC-GET
							  (METH-FUNCTION-SPEC (CADDDR MTE))
							  'COMBINED-METHOD-DERIVATION))))
				       1)
				    (IF METHOD-TYPE (EQ (CADDAR TEM) METHOD-TYPE)
					(NULL (CDDDAR TEM)))))
			   (SETF (FLAVOR-METHOD-TABLE FL)
				 (DELQ MTE (FLAVOR-METHOD-TABLE FL))))
		       (RECOMPILE-FLAVOR (FLAVOR-NAME FL) MESSAGE)	;Propagate the change
		       ;; In case anyone has their paws on the function cell
		       (NULLIFY-METHOD-DEFINITION METH)))
		   (FUNCTION-PARENT (VALUES FLAVOR 'DEFFLAVOR))	;Useful for automatic methods
		   (COMPILER-FDEFINEDP METH)
		   (GET (AND METH (GET (LOCF (METH-PLIST METH)) ARG1)))
		   (PUTPROP (PUTPROP (LOCF (METH-PLIST METH)) ARG1 ARG2))
		   (OTHERWISE
		     (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))))))
	  ((AND (SYMBOLP FLAVOR) (CLASS-SYMBOLP FLAVOR))
	   (CLASS-METHOD-FUNCTION-SPEC-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))
	  (T (FERROR NIL "In the function spec ~S,
    ~S is neither the name of a flavor nor the name of a class"
		     	 FUNCTION-SPEC FLAVOR)))))

;Make an object of a particular flavor, taking the init-plist options
;as a rest argument and sending the :INIT message if the flavor
;handles it.
(DEFUN MAKE-INSTANCE (FLAVOR-NAME &REST INIT-OPTIONS)
  (INSTANTIATE-FLAVOR FLAVOR-NAME (LOCF INIT-OPTIONS) 'MAYBE))

;Make an object of a particular flavor.
;If the flavor hasn't been composed yet, must do so now.
; Delaying it until the first time it is needed aids initialization,
; e.g. up until now we haven't depended on the depended-on flavors being defined yet.
;Note that INIT-PLIST can be modified, if the :DEFAULT-INIT-PLIST option was
; used or the init methods modify it.
(DEFUN INSTANTIATE-FLAVOR (FLAVOR-NAME INIT-PLIST
		           &OPTIONAL SEND-INIT-MESSAGE-P
				     RETURN-UNHANDLED-KEYWORDS-P ;as second value
				     AREA-TO-CONS-INSTANCE-IN
			   &AUX FL FFL UNHANDLED-KEYWORDS INSTANCE VARS N TEM)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (OR (FLAVOR-SELECT-METHOD FL) (COMPOSE-METHOD-COMBINATION FL))
  ;; Make the instance object, then fill in its various fields
  (SETQ INSTANCE (%ALLOCATE-AND-INITIALIZE DTP-INSTANCE DTP-INSTANCE-HEADER
			   FL NIL AREA-TO-CONS-INSTANCE-IN (FLAVOR-INSTANCE-SIZE FL)))
  (SETQ VARS (FLAVOR-ALL-INSTANCE-VARIABLES FL))
  ;; Default all instance variables to unbound
  (DO ((V VARS (CDR V))
       (I 1 (1+ I)))
      ((NULL V))
    (%P-STORE-TAG-AND-POINTER (%MAKE-POINTER-OFFSET DTP-LOCATIVE INSTANCE I)
			      DTP-NULL (CAR V)))
  ;; Put defaults into the INIT-PLIST
  (FLAVOR-DEFAULT-INIT-PLIST FLAVOR-NAME INIT-PLIST)
  ;; For each init keyword, either initialize the corresponding variable, remember
  ;; that it will be handled later by an :INIT method, or give an error for not being handled.
  (DO L (CDR INIT-PLIST) (CDDR L) (NULL L)
    (LET ((KEYWORD (CAR L)) (ARG (CADR L)))
      (DO ((FFLS (FLAVOR-DEPENDS-ON-ALL FL) (CDR FFLS)))
	  ((NULL FFLS) (PUSH KEYWORD UNHANDLED-KEYWORDS))
	(SETQ FFL (GET (CAR FFLS) 'FLAVOR))
	(COND ((SETQ TEM (ASSQ KEYWORD (FLAVOR-INITABLE-INSTANCE-VARIABLES FFL)))
	       (%P-STORE-CONTENTS-OFFSET ARG INSTANCE
					 (1+ (FIND-POSITION-IN-LIST (CDR TEM) VARS)))
	       (RETURN))
	      ((MEMQ KEYWORD (FLAVOR-INIT-KEYWORDS FFL)) (RETURN))))))
  ;; Do default initializations
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (GET FFL 'FLAVOR))
    (DOLIST (V (FLAVOR-LOCAL-INSTANCE-VARIABLES FFL))
      (AND (NOT (ATOM V))
	   (EQ DTP-NULL (%P-LDB-OFFSET %%Q-DATA-TYPE INSTANCE
				     (SETQ N (1+ (FIND-POSITION-IN-LIST (CAR V) VARS)))))
	   (%P-STORE-CONTENTS-OFFSET (EVAL (CADR V)) INSTANCE N))))
  ;; Complain if any keywords weren't handled, unless our caller
  ;; said it wanted to take care of this.
  (AND (NOT RETURN-UNHANDLED-KEYWORDS-P)
       UNHANDLED-KEYWORDS
       (FERROR NIL "Flavor ~S does not handle the init keyword~P ~{~S~^, ~}"
	       FLAVOR-NAME
	       (LENGTH UNHANDLED-KEYWORDS)
	       UNHANDLED-KEYWORDS))
  (AND (EQ SEND-INIT-MESSAGE-P 'MAYBE)
       (NOT (GET-HANDLER-FOR INSTANCE ':INIT))
       (SETQ SEND-INIT-MESSAGE-P NIL))
  (AND SEND-INIT-MESSAGE-P
       (FUNCALL INSTANCE ':INIT INIT-PLIST))
  (PROG () (RETURN INSTANCE UNHANDLED-KEYWORDS)))

(DEFUN FLAVOR-DEFAULT-INIT-PLIST (FLAVOR-NAME &OPTIONAL (INIT-PLIST (NCONS NIL)) &AUX FL)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (GET FFL 'FLAVOR))
    (DO L (GET (LOCF (FLAVOR-PLIST FFL)) ':DEFAULT-INIT-PLIST) (CDDR L) (NULL L)
      (DO ((M (CDR INIT-PLIST) (CDDR M)))
	  ((NULL M) (PUTPROP INIT-PLIST (EVAL (CADR L)) (CAR L)))
	(AND (EQ (CAR M) (CAR L)) (RETURN)))))
  INIT-PLIST)

;Returns non-NIL if the flavor allows the specified keyword in its init-plist,
;NIL if it doesn't.  The return value is the name of the component flavor
;that actually handles it.
(DEFUN FLAVOR-ALLOWS-INIT-KEYWORD-P (FLAVOR-NAME KEYWORD)
  (MAP-OVER-COMPONENT-FLAVORS 0 T T
      #'(LAMBDA (FL IGNORE KEYWORD)
	  (AND (OR (ASSQ KEYWORD (FLAVOR-INITABLE-INSTANCE-VARIABLES FL))
		   (MEMQ KEYWORD (FLAVOR-INIT-KEYWORDS FL)))
	       (FLAVOR-NAME FL)))
      FLAVOR-NAME NIL KEYWORD))

; Function to map over all components of a specified flavor.  We must do the
;  DEPENDS-ON's to all levels first, then the INCLUDES's at all levels and
;  what they depend on.
; Note that it does the specified flavor itself as well as all its components.
; Note well: if there are included flavors, this does not do them in the
;  right order.  Also note well: if there are multiple paths to a component,
;  it will be done more than once.
; RECURSION-STATE is 0 except when recursively calling itself.
; ERROR-P is T if not-yet-defflavored flavors are to be complained about,
;  NIL if they are to be ignored.  This exists to get rid of certain
;  bootstrapping problems.
; RETURN-FIRST-NON-NIL is T if the iteration should terminate as soon
;  as FUNCTION returns a non-null result.
; At each stage FUNCTION is applied to the flavor (not the name), the
;  STATE, and any ARGS.  STATE is updated to whatever the function returns.
; The final STATE is the final result of this function.
; RECURSION-STATE is:
;  0	top-level
;  1	first-pass over just depends-on's
;  6  	second-pass, this flavor reached via depends-on's so don't do it again
;  2	second-pass, this flavor reached via includes's so do it.
(DEFUN MAP-OVER-COMPONENT-FLAVORS (RECURSION-STATE ERROR-P RETURN-FIRST-NON-NIL
				   FUNCTION FLAVOR-NAME STATE &REST ARGS)
  (PROG MAP-OVER-COMPONENT-FLAVORS (FL)
    (COND ((OR ERROR-P (GET FLAVOR-NAME 'FLAVOR))
	   (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
	   ;; First do this flavor, unless this is the second pass and it shouldn't be done
	   (OR (BIT-TEST 4 RECURSION-STATE)
	       (SETQ STATE (LEXPR-FUNCALL FUNCTION FL STATE ARGS)))
	   ;; After each call to the function, see if we're supposed to be done now
	   (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
		(RETURN-FROM MAP-OVER-COMPONENT-FLAVORS))
	   ;; Now do the depends-on's.
	   (DOLIST (COMPONENT-FLAVOR (FLAVOR-DEPENDS-ON FL))
	     (SETQ STATE (LEXPR-FUNCALL #'MAP-OVER-COMPONENT-FLAVORS
					    (IF (ZEROP RECURSION-STATE) 1 RECURSION-STATE)
					    ERROR-P RETURN-FIRST-NON-NIL
					    FUNCTION COMPONENT-FLAVOR STATE ARGS))
	     (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
		  (RETURN-FROM MAP-OVER-COMPONENT-FLAVORS)))
	   ;; Unless this is the first pass, do the includes.
	   (OR (BIT-TEST 1 RECURSION-STATE)
	       (DOLIST (COMPONENT-FLAVOR (FLAVOR-INCLUDES FL))
		 (SETQ STATE (LEXPR-FUNCALL #'MAP-OVER-COMPONENT-FLAVORS
						2 ERROR-P RETURN-FIRST-NON-NIL
						FUNCTION COMPONENT-FLAVOR STATE ARGS))
		 (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
		      (RETURN-FROM MAP-OVER-COMPONENT-FLAVORS))))
	   ;; If this is the top-level, run the second pass on its depends-on's
	   ;; which doesn't do them but does do what they include.
	   (OR (NOT (ZEROP RECURSION-STATE))
	       (DOLIST (COMPONENT-FLAVOR (FLAVOR-DEPENDS-ON FL))
		 (SETQ STATE (LEXPR-FUNCALL #'MAP-OVER-COMPONENT-FLAVORS
					    6 ERROR-P RETURN-FIRST-NON-NIL
					    FUNCTION COMPONENT-FLAVOR STATE ARGS))
		 (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
		      (RETURN-FROM MAP-OVER-COMPONENT-FLAVORS)))))))
  STATE)

;Call this when a flavor has been changed, it updates that flavor's compiled
; information and that of any that depend on it.
;If a compilation is in progress the compilations performed
; will get output as part of that compilation.
;SINGLE-OPERATION is NIL to do all operations, or the name of an operation
; which needs incremental compilation.
;USE-OLD-COMBINED-METHODS can be NIL to force regeneration of all combined methods.
; This is used if a wrapper has changed or there was a bug in the method-combining routine.
;DO-DEPENDENTS controls whether flavors that depend on this one are also compiled.
(DEFUN RECOMPILE-FLAVOR (FLAVOR-NAME
		         &OPTIONAL (SINGLE-OPERATION NIL) (*USE-OLD-COMBINED-METHODS* T)
				   (DO-DEPENDENTS T)
			 &AUX FL)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  ;; Only update the method combination if it has been done before, else doesn't matter
  (COND ((FLAVOR-SELECT-METHOD FL)
	 (OR (FLAVOR-DEPENDS-ON-ALL FL)
	     (COMPOSE-FLAVOR-COMBINATION FL))
	 (COMPOSE-METHOD-COMBINATION FL SINGLE-OPERATION)))
  (IF DO-DEPENDENTS
      (LET ((FDEFINE-FILE-PATHNAME NIL))	;Don't give warnings for combined methods
	(DOLIST (FN (FLAVOR-DEPENDED-ON-BY-ALL FL))
	  (RECOMPILE-FLAVOR FN SINGLE-OPERATION *USE-OLD-COMBINED-METHODS* NIL)))))

;Make a list of all flavors that depend on this one, not including this flavor itself.
;This is a list of the names, not the defstructs.
(DEFUN FLAVOR-DEPENDED-ON-BY-ALL (FL &OPTIONAL (LIST-SO-FAR NIL) &AUX FFL)
  (DOLIST (FN (FLAVOR-DEPENDED-ON-BY FL))
    (OR (MEMQ FN LIST-SO-FAR)
	(NOT (SETQ FFL (GET FN 'FLAVOR)))
	(SETQ LIST-SO-FAR (FLAVOR-DEPENDED-ON-BY-ALL FFL (CONS FN LIST-SO-FAR)))))
  LIST-SO-FAR)

;This function takes care of flavor-combination.  It sets up the list
;of all component flavors, in appropriate order, and the list of all
;instance variables.  It generally needs to be called only once for a
;flavor, and must be called before method-combination can be dealt with.
(DEFUN COMPOSE-FLAVOR-COMBINATION (FL &AUX FLS VARS ORDS REQS SIZE
				           (DEFAULT-CONS-AREA *FLAVOR-AREA*))
  ;; Make list of all component flavors' names.
  ;; This list is in outermost-first order.
  ;; Would be nice for this not to have to search to all levels, but for
  ;; the moment that is hard, so I won't do it.
  ;; Included-flavors are hairy: if not otherwise in the list of components, they
  ;; are stuck in after the rightmost component that includes them, along with
  ;; any components of their own not otherwise in the list.
  (SETF (FLAVOR-DEPENDS-ON-ALL FL)
	(SETQ FLS (COPYLIST (COMPOSE-FLAVOR-INCLUSION (FLAVOR-NAME FL) T)
			    PERMANENT-STORAGE-AREA)))
  ;; Vanilla-flavor may have been put in by magic, so maintain the dependencies
  ;; in case new methods get added to it later.
  (LET ((VAN (GET 'VANILLA-FLAVOR 'FLAVOR))
	(FLAV (FLAVOR-NAME FL)))
    (AND (NOT (NULL VAN))
	 (NEQ FLAV 'VANILLA-FLAVOR)
	 (MEMQ 'VANILLA-FLAVOR FLS)
	 (NOT (MEMQ FLAV (FLAVOR-DEPENDED-ON-BY VAN)))
	 (PUSH FLAV (FLAVOR-DEPENDED-ON-BY VAN))))
  ;; Compute what the instance variables will be, and in what order.
  ;; Also collect the required but not present instance variables, which go onto the
  ;; ADDITIONAL-SPECIAL-VARIABLES property.  The instance variables of the
  ;; :REQUIRED-FLAVORS work the same way. 
  (DOLIST (F FLS)
    (SETQ F (GET F 'FLAVOR))
    (DOLIST (V (FLAVOR-LOCAL-INSTANCE-VARIABLES F))
      (OR (ATOM V) (SETQ V (CAR V)))
      (OR (MEMQ V VARS) (PUSH V VARS)))
    (DOLIST (V (GET (LOCF (FLAVOR-PLIST F)) ':REQUIRED-INSTANCE-VARIABLES))
      (OR (MEMQ V VARS) (MEMQ V REQS) (PUSH V REQS)))
    (DOLIST (FF (GET (LOCF (FLAVOR-PLIST F)) ':REQUIRED-FLAVORS))
      (COND ((AND (NOT (MEMQ FF FLS)) (SETQ FF (GET FF 'FLAVOR)))
	     (OR (FLAVOR-DEPENDS-ON-ALL FF) (COMPOSE-FLAVOR-COMBINATION FF))
	     (DOLIST (V (FLAVOR-ALL-INSTANCE-VARIABLES FF))
	       (OR (MEMQ V VARS) (MEMQ V REQS) (PUSH V REQS)))
	     (DOLIST (V (GET (LOCF (FLAVOR-PLIST FF)) 'ADDITIONAL-SPECIAL-VARIABLES))
	       (OR (MEMQ V VARS) (MEMQ V REQS) (PUSH V REQS))))))
    (LET ((ORD (GET (LOCF (FLAVOR-PLIST F)) ':ORDERED-INSTANCE-VARIABLES)))
      ;; Merge into existing order requirement.  Shorter of the two must be
      ;; a prefix of the longer, and we take the longer.
      (DO ((L1 ORD (CDR L1))
	   (L2 ORDS (CDR L2)))
	  (NIL)
	(COND ((NULL L1) (RETURN NIL))
	      ((NULL L2) (RETURN (SETQ ORDS ORD)))
	      ((NEQ (CAR L1) (CAR L2))
	       (FERROR NIL ":ORDERED-INSTANCE-VARIABLES conflict, ~S vs ~S"
		           (CAR L1) (CAR L2)))))))
  ;; This NREVERSE makes it compatible with the old code.  There is no other reason for it.
  (SETQ VARS (NREVERSE VARS))
  ;; Apply ordering requirement by moving those variables to the front.
  (DOLIST (V ORDS)
    (OR (MEMQ V VARS)
	(FERROR NIL "Flavor ~S lacks instance variable ~S which has an order requirement"
		(FLAVOR-NAME FL) V))
    (SETQ VARS (DELQ V VARS)))
  (SETQ VARS (APPEND ORDS VARS))
  (SETF (FLAVOR-ALL-INSTANCE-VARIABLES FL) (COPYLIST VARS PERMANENT-STORAGE-AREA))
  ;; Tell microcode about the instance variables
  (SETF (FLAVOR-BINDINGS FL)
	(LET ((B (MAKE-LIST (LENGTH VARS) ':AREA PERMANENT-STORAGE-AREA)))
	  (DO ((V VARS (CDR V))		;This way rather than MAPCAR for CDR-coding
	       (L B (CDR L)))
	      ((NULL V) B)
	    (RPLACA L (VALUE-CELL-LOCATION (CAR V))))))
  ;; Instance size must be at least 2 or microcode blows out - fix some day?
  (SETQ SIZE (MAX (1+ (LENGTH VARS)) 2))
  (AND (FLAVOR-INSTANCE-SIZE FL)
       ( (FLAVOR-INSTANCE-SIZE FL) SIZE)
       (FORMAT ERROR-OUTPUT "~&Warning: changing the size of an instance of ~S from ~S to ~S
This may cause you problems.~%"		;* This should perhaps do something about it *
		 (FLAVOR-NAME FL) (FLAVOR-INSTANCE-SIZE FL) SIZE))
  (SETF (FLAVOR-INSTANCE-SIZE FL) SIZE)
  ;; If there are any instance variables required but not present, save them
  ;; so that they can be declared special in methods.
  (DOLIST (V VARS)
    (SETQ REQS (DELQ V REQS)))
  (AND REQS (PUTPROP (LOCF (FLAVOR-PLIST FL))
		     (COPYLIST REQS PERMANENT-STORAGE-AREA)
		     'ADDITIONAL-SPECIAL-VARIABLES))
  NIL)

(DEFUN COMPOSE-FLAVOR-INCLUSION (FLAVOR ERROR-P)
  (MULTIPLE-VALUE-BIND (FLS ADDITIONS) (COMPOSE-FLAVOR-INCLUSION-1 FLAVOR NIL ERROR-P)
    ;; The new additions may themselves imply more components
    (DO L ADDITIONS (CDR L) (NULL L)
      (LET ((MORE-FLS (COMPOSE-FLAVOR-INCLUSION-1 (CAR L) FLS ERROR-P)))
	(DOLIST (F MORE-FLS)
	  ;; This hair inserts F before (after) the thing that indirectly included it
	  ;; and then puts that next on ADDITIONS so it gets composed also
	  (LET ((LL (MEMQ (CAR L) FLS)))
	    (RPLACA (RPLACD LL (CONS (CAR LL) (CDR LL))) F)
	    (RPLACD L (CONS F (CDR L)))))))
    ;; Now attach vanilla-flavor if desired
    (OR (LOOP FOR FLAVOR IN FLS
	      THEREIS (GET (LOCF (FLAVOR-PLIST (GET FLAVOR 'FLAVOR))) ':NO-VANILLA-FLAVOR))
	(PUSH 'VANILLA-FLAVOR FLS))
    (NREVERSE FLS)))

(LOCAL-DECLARE ((SPECIAL OTHER-COMPONENTS))
(DEFUN COMPOSE-FLAVOR-INCLUSION-1 (FLAVOR OTHER-COMPONENTS ERROR-P)
  ;; First, make a backwards list of all the normal (non-included) components
  (LET ((FLS (MAP-OVER-COMPONENT-FLAVORS 1 ERROR-P NIL
					 #'(LAMBDA (FL LIST)
					     (SETQ FL (FLAVOR-NAME FL))
					     (OR (MEMQ FL LIST)
						 (MEMQ FL OTHER-COMPONENTS)
						 (PUSH FL LIST))
					     LIST)
					 FLAVOR NIL))
	(ADDITIONS NIL))
    ;; If there are any inclusions that aren't in the list, plug
    ;; them in right after (before in backwards list) their last (first) includer
    (DO L FLS (CDR L) (NULL L)
      (DOLIST (FL (FLAVOR-INCLUDES (GET (CAR L) 'FLAVOR)))
	(OR (MEMQ FL FLS)
	    (MEMQ FL OTHER-COMPONENTS)
	    (PUSH (CAR (RPLACA (RPLACD L (CONS (CAR L) (CDR L))) FL)) ADDITIONS))))
    (OR (MEMQ FLAVOR FLS)
	(SETQ FLS (NCONC FLS
			 (NREVERSE
			   (LOOP FOR FL IN (FLAVOR-INCLUDES (GET FLAVOR 'FLAVOR))
				 UNLESS (OR (MEMQ FL FLS) (MEMQ FL OTHER-COMPONENTS))
				   COLLECT FL
				   AND DO (PUSH FL ADDITIONS))))))
    (VALUES FLS ADDITIONS))))

;Once the flavor-combination stuff has been done, do the method-combination stuff.
;The above function usually only gets called once, but this function gets called
;when a new method is added.
;Specify SINGLE-OPERATION to do this for just one operation, for incremental update.
;NOTE WELL: If a meth is in the method-table at all, it is considered to be defined
; for purposes of compose-method-combination.  Thus merely putprop'ing a method,
; or calling flavor-notice-method, will make the flavor think that method exists
; when it is next composed.  This is necessary to make compile-flavor-methods work.
; (Putprop must create the meth because loading does putprop before fdefine.)
(DEFUN COMPOSE-METHOD-COMBINATION (FL &OPTIONAL (SINGLE-OPERATION NIL)
				   &AUX TEM MAGIC-LIST DEFAULT-HANDLER ORDER
				        MSG ELEM HANDLERS SM FFL PL
					(DEFAULT-CONS-AREA *FLAVOR-AREA*))
  ;; Look through all the flavors depended upon and collect the following:
  ;; A list of all the operations handled and all the methods for each, called MAGIC-LIST.
  ;; The default handler for unknown operations.
  ;; The declared order of entries in the select-method alist.
  ;; Also generate any automatically-created methods not already present.
  ;; MAGIC-LIST is roughly the same format as the flavor-method-table, see its comments.
  ;; Each magic-list entry is (message comb-type comb-order (type function-spec...)...)
  (DO ((FFLS (FLAVOR-DEPENDS-ON-ALL FL) (CDR FFLS)))
      ((NULL FFLS))
    (SETQ FFL (GET (CAR FFLS) 'FLAVOR) PL (LOCF (FLAVOR-PLIST FFL)))
    (COND ((NOT SINGLE-OPERATION)
	   (OR DEFAULT-HANDLER (SETQ DEFAULT-HANDLER (GET PL ':DEFAULT-HANDLER)))
	   (AND (SETQ TEM (GET PL ':SELECT-METHOD-ORDER))
		(SETQ ORDER (NCONC ORDER (COPYLIST TEM))))))
    ;; Add data from flavor method-table to magic-list
    ;; But skip over combined methods, they are not relevant here
    (DOLIST (MTE (FLAVOR-METHOD-TABLE FFL))
      (SETQ MSG (CAR MTE))
      (COND ((OR (NOT SINGLE-OPERATION) (EQ MSG SINGLE-OPERATION))
	     ;; Well, we're supposed to concern ourselves with this operation
	     (SETQ ELEM (ASSQ MSG MAGIC-LIST))	;What we already know about it
	     (COND ((DOLIST (METH (CDDDR MTE))
		      (OR (EQ (METH-METHOD-TYPE METH) ':COMBINED) (RETURN T)))
		    ;; OK, this flavor really contributes to handling this operation
		    (OR ELEM (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) MAGIC-LIST))
		    ;; For each non-combined method for this operation, add it to the front
		    ;; of the magic-list element, thus they are in base-flavor-first order.
		    (DOLIST (METH (CDDDR MTE))
		      (LET ((TYPE (METH-METHOD-TYPE METH)))
			(COND ((EQ TYPE ':COMBINED))
			      ((NOT (SETQ TEM (ASSQ TYPE (CDDDR ELEM))))
			       (PUSH (LIST TYPE (METH-FUNCTION-SPEC METH)) (CDDDR ELEM)))
			      ;; Don't let the same method get in twice (how could it?)
			      ((NOT (MEMQ (METH-FUNCTION-SPEC METH) (CDR TEM)))
			       (PUSH (METH-FUNCTION-SPEC METH) (CDR TEM))))))))
	     ;; Pick up method-combination declarations
	     (AND (CADR MTE) (CADR ELEM)	;If both specify combination-type, check
		  (OR (NEQ (CADR MTE) (CADR ELEM)) (NEQ (CADDR MTE) (CADDR ELEM)))
		  (FERROR NIL
		      "Method-combination mismatch ~S-~S vs. ~S-~S, check your DEFFLAVOR's"
		      (CADR MTE) (CADDR MTE) (CADR ELEM) (CADDR ELEM)))
	     (COND ((CADR MTE)			;Save combination-type when specified
		    (OR ELEM (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) MAGIC-LIST))
		    (SETF (CADR ELEM) (CADR MTE))
		    (SETF (CADDR ELEM) (CADDR MTE)))) ))))
  ;; This NREVERSE tends to put base-flavor methods last
  (SETQ MAGIC-LIST (NREVERSE MAGIC-LIST))
  ;; Re-order the magic-list according to any declared required order
  (DOLIST (MSG (NREVERSE ORDER))
    (AND (SETQ TEM (ASSQ MSG MAGIC-LIST))
	 (SETQ MAGIC-LIST (CONS TEM (DELQ TEM MAGIC-LIST 1)))))
  ;; Map over the magic-list.  For each entry call the appropriate method-combining
  ;; routine, which will return a symbol which goes into the final select-method list.
  (DOLIST (MTE MAGIC-LIST)
    ;; Punt if there are no methods at all (just a method-combination declaration)
    (COND ((CDDDR MTE)
	   ;; Process the :DEFAULT methods; if there are any untyped methods the
	   ;; default methods go away, otherwise they become untyped methods.
	   (AND (SETQ TEM (ASSQ ':DEFAULT (CDDDR MTE)))
		(IF (ASSQ NIL (CDDDR MTE))
		    (SETF (CDDDR MTE) (DELQ TEM (CDDDR MTE)))
		    (RPLACA TEM NIL)))
	   (OR (SETQ TEM (GET (OR (CADR MTE) ':DAEMON) 'METHOD-COMBINATION))
	       (FERROR NIL "~S unknown method combination type for ~S operation"
		           (CADR MTE) (CAR MTE)))
	   (PUSH (FUNCALL TEM FL MTE) HANDLERS))
	  (T (SETQ MAGIC-LIST (DELQ MTE MAGIC-LIST 1)))))
  ;; Get back into declared order.  We now have a list of function specs for handlers.
  (SETQ HANDLERS (NREVERSE HANDLERS))
  (COND	(*JUST-COMPILING* )	;If just compiling, don't affect select-method
	(SINGLE-OPERATION
	  ;; If doing SINGLE-OPERATION, put it into the select-method list
	  (SETQ SM (%MAKE-POINTER DTP-LIST (FLAVOR-SELECT-METHOD FL)))
	  (SETQ ELEM (ASSQ-CAREFUL SINGLE-OPERATION SM))
	  (COND ((NULL HANDLERS)		;Deleting method
		 (COND (ELEM
			 (SETF (FLAVOR-SELECT-METHOD FL)
			       (%MAKE-POINTER DTP-SELECT-METHOD (DELQ ELEM SM 1)))
			 ;; This will have to be recomputed
			 (SETF (FLAVOR-WHICH-OPERATIONS FL) NIL))))
		(ELEM				;Replacing method
		 (STORE-HANDLER-EVCP ELEM (CAR HANDLERS)))
		(T (RPLACD (LAST SM)		;Adding method
			   (CONS-IN-AREA (CONS-HANDLER-EVCP SINGLE-OPERATION (CAR HANDLERS))
					 (CDR (LAST SM)) PERMANENT-STORAGE-AREA))
		   ;; This will have to be recomputed
		   (SETF (FLAVOR-WHICH-OPERATIONS FL) NIL))))
	(T
	  ;; Now cons up the select-method list.  CDR-code its top-level to save on memory.
	  (SETQ SM (MAKE-LIST (1+ (LENGTH MAGIC-LIST)) ':AREA PERMANENT-STORAGE-AREA))
	  (DO ((SM SM (CDR SM))
	       (HANDLERS HANDLERS (CDR HANDLERS))
	       (ML MAGIC-LIST (CDR ML)))
	      ((NULL ML)
	       ;; Final CDR is default handler
	       (RPLACA SM (OR DEFAULT-HANDLER 'FLAVOR-UNCLAIMED-MESSAGE))
	       (%P-DPB-OFFSET CDR-NORMAL %%Q-CDR-CODE SM -1)
	       (%P-DPB-OFFSET CDR-ERROR %%Q-CDR-CODE SM 0))
	    (RPLACA SM (CONS-HANDLER-EVCP (CAAR ML) (CAR HANDLERS))))
	  (SETF (FLAVOR-SELECT-METHOD FL) (%MAKE-POINTER DTP-SELECT-METHOD SM))
	  (SETF (FLAVOR-WHICH-OPERATIONS FL) NIL)))	;This will have to be recomputed
  (OR SINGLE-OPERATION
      ;; Make sure that the required variables and methods are present.
      (DO ((FFLS (FLAVOR-DEPENDS-ON-ALL FL) (CDR FFLS))
	   (MISSING-METHODS NIL)
	   (MISSING-INSTANCE-VARIABLES NIL)
	   (MISSING-FLAVORS NIL))
	  ((NULL FFLS)
	   (AND (OR MISSING-INSTANCE-VARIABLES MISSING-METHODS MISSING-FLAVORS)
		(FERROR NIL "Flavor ~S is missing ~
				~:[~2*~;instance variable~P ~{~S~^, ~} ~]~
				~:[~3*~;~:[~;and ~]method~P ~{~S~^, ~}~]~
				~:[~3*~;~:[~;and ~]component flavor~P ~{~S~^, ~}~]"
			(FLAVOR-NAME FL)
			MISSING-INSTANCE-VARIABLES
			(LENGTH MISSING-INSTANCE-VARIABLES)
			MISSING-INSTANCE-VARIABLES
			MISSING-METHODS
			MISSING-INSTANCE-VARIABLES
			(LENGTH MISSING-METHODS)
			MISSING-METHODS
			MISSING-FLAVORS
			(OR MISSING-INSTANCE-VARIABLES MISSING-METHODS)
			(LENGTH MISSING-FLAVORS)
			MISSING-FLAVORS)))
	(SETQ FFL (GET (CAR FFLS) 'FLAVOR) PL (LOCF (FLAVOR-PLIST FFL)))
	(DOLIST (REQM (GET PL ':REQUIRED-METHODS))
	  (OR (ASSQ REQM MAGIC-LIST)
	      (MEMQ REQM MISSING-METHODS)
	      (PUSH REQM MISSING-METHODS)))
	(DOLIST (REQV (GET PL ':REQUIRED-INSTANCE-VARIABLES))
	  (OR (MEMQ REQV (FLAVOR-ALL-INSTANCE-VARIABLES FL))
	      (MEMQ REQV MISSING-INSTANCE-VARIABLES)
	      (PUSH REQV MISSING-INSTANCE-VARIABLES)))
	(DOLIST (REQF (GET PL ':REQUIRED-FLAVORS))
	  (OR (MEMQ REQF (FLAVOR-DEPENDS-ON-ALL FL))
	      (MEMQ REQF MISSING-FLAVORS)
	      (PUSH REQF MISSING-FLAVORS)))))
  NIL)

;This is the default handler for flavors.
(DEFUN FLAVOR-UNCLAIMED-MESSAGE (&REST MESSAGE)
  (IF (FUNCALL-SELF ':OPERATION-HANDLED-P ':UNCLAIMED-MESSAGE)
      (LEXPR-FUNCALL-SELF ':UNCLAIMED-MESSAGE MESSAGE)
      (FERROR ':UNCLAIMED-MESSAGE "The object ~S received a ~S message, which went unclaimed.
The rest of the message was ~S~%" SELF (CAR MESSAGE) (CDR MESSAGE))))

;Cons up an a-list entry mapping the message keyword into an evcp to the handler function cell
(DEFUN CONS-HANDLER-EVCP (MESSAGE HANDLER)
  (STORE-HANDLER-EVCP (CONS-IN-AREA MESSAGE NIL PERMANENT-STORAGE-AREA) HANDLER))

;Store an external-value-cell-pointer to the function cell of the handler
;into the cdr of the cons (a-list element)
(DEFUN STORE-HANDLER-EVCP (LOC HANDLER)
  (SETQ HANDLER (FDEFINITION-LOCATION HANDLER))
  (LET ((P (%MAKE-POINTER-OFFSET DTP-LOCATIVE LOC 1)))
    (%P-STORE-TAG-AND-POINTER P (+ (LSH (%P-CDR-CODE P) 6) DTP-EXTERNAL-VALUE-CELL-POINTER)
				HANDLER))
  LOC)

;; Make the instance-variable getting and setting methods
(DEFUN COMPOSE-AUTOMATIC-METHODS (FL)
  (DOLIST (V (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL))
    (LET ((VV (CORRESPONDING-KEYWORD V)))
      (AND (OR *JUST-COMPILING* (NOT (FLAVOR-METHOD-EXISTS FL NIL VV)))
	   (LET ((LOCAL-DECLARATIONS (CONS `(SPECIAL ,V) LOCAL-DECLARATIONS))
		 (METH `(:METHOD ,(FLAVOR-NAME FL) ,VV)))
	     (FLAVOR-NOTICE-METHOD METH)
	     (COMPILE-AT-APPROPRIATE-TIME FL METH `(LAMBDA (IGNORE) ,V))))))
  (DOLIST (V (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL))
    (LET ((SV (INTERN1 (FORMAT NIL "SET-~A" V) PKG-USER-PACKAGE)))
      (AND (OR *JUST-COMPILING* (NOT (FLAVOR-METHOD-EXISTS FL NIL SV)))
	   (LET ((LOCAL-DECLARATIONS (CONS `(SPECIAL ,V) LOCAL-DECLARATIONS))
		 (METH `(:METHOD ,(FLAVOR-NAME FL) ,SV)))
	     (FLAVOR-NOTICE-METHOD METH)
	     (COMPILE-AT-APPROPRIATE-TIME FL METH
			  `(LAMBDA (IGNORE .NEWVALUE.) (SETQ ,V .NEWVALUE.))))))))

;INTERN but always return-array the print-name argument
(DEFUN INTERN1 (PNAME &OPTIONAL (PKG PACKAGE))
  (PROG1 (INTERN PNAME PKG)
	 (RETURN-ARRAY PNAME)))

;Given a symbol return the corresponding one in the keyword package
(DEFUN CORRESPONDING-KEYWORD (SYMBOL)
  (INTERN (GET-PNAME SYMBOL) PKG-USER-PACKAGE))

; Method-combination functions.  Found on the SI:METHOD-COMBINATION property
; of the combination-type.  These are passed the flavor structure, and the
; magic-list entry, and must return the function-spec for the handler
; to go into the select-method, defining any necessary functions.
; This function interprets combination-type-arg,
; which for many combination-types is either :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST.

; :DAEMON combination
; The primary method is the outermost untyped-method (or :DEFAULT).
; The :BEFORE methods are called base-flavor-last, the :AFTER methods are called
; base-flavor-first.  An important optimization is not to generate a combined-method
; if there is only a primary method.  You are allowed to omit the primary method
; if there are any daemons (I'm not convinced this is really a good idea) in which
; case the method's returned value will be NIL.
(DEFUN (:DAEMON METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:BEFORE :AFTER) T
						  ':BASE-FLAVOR-LAST)))
	(BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
					     ':BASE-FLAVOR-LAST))
	(AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
					    ':BASE-FLAVOR-FIRST))
	(WRAPPERS-P (ASSQ ':WRAPPER (CDDDR MAGIC-LIST-ENTRY))))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
	   (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS) PRIMARY-METHOD)
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	   `(PROGN 
	      ,@(MAPCAR #'(LAMBDA (X) `(LEXPR-FUNCALL #',X .DAEMON-CALLER-ARGS.))
			BEFORE-METHODS)
	      ,(IF AFTER-METHODS
		   ;; Kludge to return a few multiple values
		   `(PROG (.VAL1. .VAL2. .VAL3.)
		       ,(AND PRIMARY-METHOD
			     `(MULTIPLE-VALUE (.VAL1. .VAL2. .VAL3.)
				(LEXPR-FUNCALL #',PRIMARY-METHOD .DAEMON-CALLER-ARGS.)))
		       ,@(MAPCAR #'(LAMBDA (X) `(LEXPR-FUNCALL #',X .DAEMON-CALLER-ARGS.))
				 AFTER-METHODS)
		       (RETURN .VAL1. .VAL2. .VAL3.))
		   ;; No :AFTER methods, hair not required
		   ;; You are allowed to not have a primary method
		   (AND PRIMARY-METHOD
			`(LEXPR-FUNCALL #',PRIMARY-METHOD .DAEMON-CALLER-ARGS.))))))))

; :DAEMON-WITH-OVERRIDE combination
; This is the same as :DAEMON (the default), except that :OVERRIDE type methods
; are combined with the :BEFORE-primary-:AFTER methods in an OR.  This allows
; overriding of the main methods function.  For example, a combined method as follows
; might be generated: (OR (FOO-OVERRIDE-BAR-METHOD) (PROGN (FOO-BEFORE-BAR-METHOD)))
(DEFUN (:DAEMON-WITH-OVERRIDE METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL
						  '(:BEFORE :AFTER :OVERRIDE) T
						  ':BASE-FLAVOR-LAST)))
	(BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
					     ':BASE-FLAVOR-LAST))
	(AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
					    ':BASE-FLAVOR-FIRST))
	(WRAPPERS-P (ASSQ ':WRAPPER (CDDDR MAGIC-LIST-ENTRY)))
	(OVERRIDE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY
					       ':OVERRIDE T T ':BASE-FLAVOR-LAST)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
	   (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS)
	     (NULL OVERRIDE-METHODS)
	     PRIMARY-METHOD)
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	  `(OR ,@(MAPCAR #'(LAMBDA (X) `(LEXPR-FUNCALL #',X .DAEMON-CALLER-ARGS.))
			 OVERRIDE-METHODS)
	     (PROGN 
	      ,@(MAPCAR #'(LAMBDA (X) `(LEXPR-FUNCALL #',X .DAEMON-CALLER-ARGS.))
			BEFORE-METHODS)
	      ,(IF AFTER-METHODS
		   ;; Kludge to return a few multiple values
		   `(PROG (.VAL1. .VAL2. .VAL3.)
		       ,(AND PRIMARY-METHOD
			     `(MULTIPLE-VALUE (.VAL1. .VAL2. .VAL3.)
				(LEXPR-FUNCALL #',PRIMARY-METHOD .DAEMON-CALLER-ARGS.)))
		       ,@(MAPCAR #'(LAMBDA (X) `(LEXPR-FUNCALL #',X .DAEMON-CALLER-ARGS.))
				 AFTER-METHODS)
		       (RETURN .VAL1. .VAL2. .VAL3.))
		   ;; No :AFTER methods, hair not required
		   ;; You are allowed to not have a primary method
		   (AND PRIMARY-METHOD
			`(LEXPR-FUNCALL #',PRIMARY-METHOD .DAEMON-CALLER-ARGS.)))))))))

; :LIST combination
; No typed-methods allowed.  Returns a list of the results of all the methods.
; There will always be a combined-method, even if only one method to be called.
(DEFUN (:LIST METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
      (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	    (CONS 'LIST (MAPCAR #'(LAMBDA (M) `(LEXPR-FUNCALL #',M .DAEMON-CALLER-ARGS.))
				(GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL NIL NIL NIL))))))

; :INVERSE-LIST combination
; No typed-methods allowed.  Apply each method to an element of the list.  Given
; the result of a :LIST-combined method with the same ordering, and corresponding
; method definitions, the result that emerged from each component flavor gets handed
; back to that same flavor.  The combined-method returns no particular value.
(DEFUN (:INVERSE-LIST METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
      (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	 `(LET ((.FOO. (CADR .DAEMON-CALLER-ARGS.)))
	    . ,(DO ((ML (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL NIL NIL NIL) (CDR ML))
		    (R NIL))
		   ((NULL ML) (NREVERSE R))
		 (PUSH `(FUNCALL #',(CAR ML) (CAR .DAEMON-CALLER-ARGS.) (CAR .FOO.)) R)
		 (AND (CDR ML) (PUSH '(SETQ .FOO. (CDR .FOO.)) R)))))))

; :PROGN combination
; :AND combination
; :OR combination
; These just call all the untyped methods, inside the indicated special form.
; As an optimization, if there is only one method it is simply called.
; Should there also be such winning combinations as :+, :*, :MAX, :MIN, etc??
; Also there should be hair where methods with an extra keyword in them
; get to act as conditionals controlling which other methods get called,
; if anyone can ever specify exactly what this means.
(DEFPROP :PROGN SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :AND SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :OR SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)

(DEFUN SIMPLE-METHOD-COMBINATION (FL MAGIC-LIST-ENTRY)
  (LET ((METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL NIL NIL NIL))
	(WRAPPERS-P (ASSQ ':WRAPPER (CDDDR MAGIC-LIST-ENTRY))))
    (OR (AND (NOT WRAPPERS-P) (NULL (CDR METHODS)) (CAR METHODS))
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	   (CONS (CADR MAGIC-LIST-ENTRY)
		 (MAPCAR #'(LAMBDA (M) `(LEXPR-FUNCALL #',M .DAEMON-CALLER-ARGS.))
			 METHODS))))))

; :PASS-ON combination
; The values from the individual methods are the arguments to the next one;
; the values from the last method are the values returned by the combined
; method.  Format is (:METHOD-COMBINATION (:PASS-ON (ORDERING . ARGLIST)) . OPERATION-NAMES)
; ORDERING is :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST.  ARGLIST can have &AUX and &OPTIONAL.

(DEFUN (:PASS-ON METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL NIL NIL (CAADDR MAGIC-LIST-ENTRY)))
	(ARGLIST (CDADDR MAGIC-LIST-ENTRY))
	ARGS REST-ARG-P)
    (DO ((L ARGLIST (CDR L))
	 (ARG)
	 (NL NIL))
	((NULL L)
	 (SETQ ARGS (NREVERSE NL)))
      (SETQ ARG (CAR L))
      (AND (LISTP ARG)
	   (SETQ ARG (CAR ARG)))
      (COND ((EQ ARG '&REST)
	     (SETQ REST-ARG-P T))
	    ((EQ ARG '&AUX))
	    (T
	     (PUSH ARG NL))))      
    (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	  `(DESTRUCTURING-BIND ,(CONS '.OPERATION. ARGLIST) SI:.DAEMON-CALLER-ARGS.
	     . ,(DO ((METHS METHODS (CDR METHS))
		     (LIST NIL)
		     (METH))
		    ((NULL METHS)
		     (NREVERSE LIST))
		  (SETQ METH `(,(IF REST-ARG-P 'LEXPR-FUNCALL 'FUNCALL)
			       #',(CAR METHS) .OPERATION. . ,ARGS))
		  (AND (CDR METHS)
		       (SETQ METH (IF (NULL (CDR ARGS))
				      `(SETQ ,(CAR ARGS) ,METH)
				      `(MULTIPLE-VALUE ,ARGS ,METH))))
		  (PUSH METH LIST)))))))

; This function does most of the analysis of the magic-list-entry needed by
; method-combination functions, including most error checking.
(DEFUN GET-CERTAIN-METHODS (MAGIC-LIST-ENTRY METHOD-TYPE OTHER-METHODS-ALLOWED NO-METHODS-OK
			    ORDERING-DECLARATION &AUX (METHODS NIL))
  "Perform analysis needed by method-combination functions.
   Returns a list of the method symbols for METHOD-TYPE extracted from MAGIC-LIST-ENTRY.
   This value is shared with the data structure, don't bash it.
   OTHER-METHODS-ALLOWED is a list of method types not to complain about (T = allow all).
   NO-METHODS-OK = NIL means to complain if the returned value would be NIL.
   ORDERING-DECLARATION is :BASE-FLAVOR-FIRST, :BASE-FLAVOR-LAST, or NIL meaning
     take one of those symbols from the MAGIC-LIST-ENTRY."
  ;; Find the methods of the desired type, and barf at any extraneous methods
  (DOLIST (X (CDDDR MAGIC-LIST-ENTRY))
    (COND ((EQ (CAR X) METHOD-TYPE) (SETQ METHODS (CDR X)))
	  ((EQ (CAR X) ':WRAPPER) )		;Wrappers ignored at this level
	  ((OR (EQ OTHER-METHODS-ALLOWED T) (MEMQ (CAR X) OTHER-METHODS-ALLOWED)) )
	  (T (FERROR NIL "~S ~S method(s) illegal when using :~A method-combination"
		         (CAR X) (CAR MAGIC-LIST-ENTRY) (CADR MAGIC-LIST-ENTRY)))))
  ;; Complain if no methods supplied
  (AND (NULL METHODS) (NOT NO-METHODS-OK)
       (FERROR NIL "No ~S ~S method(s) supplied to :~A method-combination"
	           METHOD-TYPE (CAR MAGIC-LIST-ENTRY) (CADR MAGIC-LIST-ENTRY)))
  ;; Get methods into proper order.  Don't use NREVERSE!
  (SELECTQ (OR ORDERING-DECLARATION (SETQ ORDERING-DECLARATION (CADDR MAGIC-LIST-ENTRY)))
    (:BASE-FLAVOR-FIRST )
    (:BASE-FLAVOR-LAST (SETQ METHODS (REVERSE METHODS)))
    (OTHERWISE (FERROR NIL "~S invalid method combination order;
 must be :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST"
		           ORDERING-DECLARATION)))
  METHODS)

;; It is up to the caller to decide that a combined-method is called for at all.
;; If one is, this function decides whether it already exists OK or needs
;; to be recompiled.  Returns the symbol for the combined method if it is
;; still valid, otherwise returns NIL.
;; Always canonicalizes the magic-list-entry, since it will be needed
;; canonicalized later.
(DEFUN HAVE-COMBINED-METHOD (FL MAGIC-LIST-ENTRY
			     &AUX OPERATION-NAME CMS MTE OLD-MLE OLD-CMS TEM)
  ;; Canonicalize the magic-list-entry so can compare with EQUAL
  (SETF (CDDDR MAGIC-LIST-ENTRY)		;Canonicalize before comparing
	(SORTCAR (CDDDR MAGIC-LIST-ENTRY) #'STRING-LESSP))	;Sort by method-type
  (SETQ OPERATION-NAME (CAR MAGIC-LIST-ENTRY))
  ;; Get the :COMBINED method function spec for this flavor.  Note that if a suitable
  ;; one can be inherited, we will do so.
  ;; *USE-OLD-COMBINED-METHODS* controls whether we reuse an existing one for this
  ;; flavor; if we inherit one it will always be up-to-date already.
  ;; If all OK, return the function spec, else return NIL if new combined method must be made.
  (OR (AND *USE-OLD-COMBINED-METHODS*		;See if we already have one ourselves
	   (SETQ MTE (ASSQ OPERATION-NAME (FLAVOR-METHOD-TABLE FL)))
	   (SETQ OLD-CMS (SETQ CMS (METH-FUNCTION-SPEC (METH-LOOKUP ':COMBINED (CDDDR MTE)))))
	   (FDEFINEDP CMS)
	   (EQUAL MAGIC-LIST-ENTRY
		  (SETQ OLD-MLE (FUNCTION-SPEC-GET CMS 'COMBINED-METHOD-DERIVATION)))
	   CMS)
      ;; See if we can inherit one in either the current or future (being-compiled) world
      (DOLIST (FFL (CDR (FLAVOR-DEPENDS-ON-ALL FL)))  ;CDR = not self!
	(AND (SETQ MTE (ASSQ OPERATION-NAME (FLAVOR-METHOD-TABLE (GET FFL 'FLAVOR))))
	     (SETQ CMS (METH-FUNCTION-SPEC (METH-LOOKUP ':COMBINED (CDDDR MTE))))
	     (OR (FDEFINEDP CMS) *JUST-COMPILING*)
	     (EQUAL MAGIC-LIST-ENTRY
		    (SETQ TEM
			  (OR (AND *JUST-COMPILING*
				  (FUNCTION-SPEC-GET CMS 'FUTURE-COMBINED-METHOD-DERIVATION))
			      (FUNCTION-SPEC-GET CMS 'COMBINED-METHOD-DERIVATION))))
	     (RETURN CMS))
	;Save first combined-method seen for tracing, it's the one we would
	;have been most likely to inherit
	(OR OLD-CMS (NULL CMS) (NULL TEM)
	    (SETQ OLD-CMS CMS OLD-MLE TEM)))
      ;; Have to make a new combined method.  Trace if desired, but return NIL in any case.
      (PROGN
	(COND (*FLAVOR-COMPILE-TRACE*
	       (FORMAT *FLAVOR-COMPILE-TRACE*
		       "~&~S's ~S combined method needs to be recompiled~%to come from "
		       (FLAVOR-NAME FL) OPERATION-NAME)
	       (PRINT-COMBINED-METHOD-DERIVATION MAGIC-LIST-ENTRY *FLAVOR-COMPILE-TRACE*)
	       (COND (OLD-CMS
		      (FORMAT *FLAVOR-COMPILE-TRACE*
			      "~%rather than using ~S which comes from " OLD-CMS)
		      (PRINT-COMBINED-METHOD-DERIVATION OLD-MLE *FLAVOR-COMPILE-TRACE*))
		     ((NOT *USE-OLD-COMBINED-METHODS*)
		      (FORMAT *FLAVOR-COMPILE-TRACE* "~%because of forced recompilation.")))))
	NIL)))


(DEFUN PRINT-COMBINED-METHOD-DERIVATION (MLE STREAM)
  (LOOP FOR (TYPE . FUNCTION-SPECS) IN (CDDDR MLE)
	DO (LOOP FOR FUNCTION-SPEC IN FUNCTION-SPECS DO (FORMAT STREAM "~S " FUNCTION-SPEC)))
  (IF (OR (CADR MLE) (CADDR MLE))
      (FORMAT STREAM "with method-combination ~S ~S" (CADR MLE) (CADDR MLE))))

;; This function creates a combined-method, and returns the appropriate function spec.
;; Its main job in life is to take care of wrappers.  Note the combined method
;; always takes a single &REST argument named .DAEMON-CALLER-ARGS.
;; FORM is a single form to be used as the body.
(DEFUN MAKE-COMBINED-METHOD (FL MAGIC-LIST-ENTRY FORM &AUX FSPEC)
  ;; Get the function spec which will name the combined-method
  (SETQ FSPEC `(:METHOD ,(FLAVOR-NAME FL) :COMBINED ,(CAR MAGIC-LIST-ENTRY)))
  ;; Put the wrappers around the form.  The base-flavor wrapper goes on the inside.
  ;; Here we just put the macro-names.  The macros will be expanded by the compiler.
  (DO ((WRAPPERS (CDR (ASSQ ':WRAPPER (CDDDR MAGIC-LIST-ENTRY))) (CDR WRAPPERS)))
      ((NULL WRAPPERS))
    (OR (AND (FDEFINEDP (CAR WRAPPERS))
	     (LET ((DEF (FDEFINITION (CAR WRAPPERS))))
	       (OR (AND (LISTP DEF) (EQ (CAR DEF) 'MACRO))
		   ;--- temporary code so I can test things in the kludge environment
		   (AND (SYMBOLP DEF)
			(EQ (CAR (FSYMEVAL DEF)) 'MACRO)))))
	(FERROR NIL "~S supposed to be a wrapper macro, but missing!" (CAR WRAPPERS)))
    (SETQ FORM `(MACROCALL #',(CAR WRAPPERS) .DAEMON-CALLER-ARGS. ,FORM)))
  ;; Remember that it's going to be there, for HAVE-COMBINED-METHOD
  (FLAVOR-NOTICE-METHOD FSPEC)
  (IF *JUST-COMPILING*
      (FUNCTION-SPEC-PUTPROP FSPEC MAGIC-LIST-ENTRY 'FUTURE-COMBINED-METHOD-DERIVATION))
  ;; Compile the function.  It will be inserted into the flavor's tables either
  ;; now or when the QFASL file is loaded.
  ;; Declare the instance variables special in case wrappers use them
  (LET ((LOCAL-DECLARATIONS (CONS (FLAVOR-SPECIAL-DECLARATION (FLAVOR-NAME FL))
				  LOCAL-DECLARATIONS)))
    (COMPILE-AT-APPROPRIATE-TIME
	FL
	FSPEC
	`(LAMBDA (&REST .DAEMON-CALLER-ARGS.)
	   ,FORM)
	`(FUNCTION-SPEC-PUTPROP ',FSPEC
				',MAGIC-LIST-ENTRY
				'COMBINED-METHOD-DERIVATION)))
  FSPEC)

;Sort of a macro version of funcall, for wrappers
(DEFMACRO MACROCALL (&REST X)
  (LET ((MACRO (EVAL (CAR X))))
    (IF (AND (LISTP MACRO) (EQ (CAR MACRO) 'MACRO))
	(FUNCALL (CDR MACRO) X)
	;--- Temporary code so I can test things in the kludge environment
	(IF (AND (SYMBOLP MACRO) (LISTP (FSYMEVAL MACRO)) (EQ (CAR (FSYMEVAL MACRO)) 'MACRO))
	    (FUNCALL (CDR (FSYMEVAL MACRO)) X)
	    (FERROR NIL "~S evaluated to ~S, which is not a macro" (CAR X) MACRO)))))


;Return the SPECIAL declaration for a flavor, suitable for use in methods.
;No error (returns NIL) if flavor not fully defined yet, although you may get a 
;declared-special warning from the compiler.
(DEFUN FLAVOR-SPECIAL-DECLARATION (FLAVOR-NAME &AUX FL)
  (AND (SETQ FL (GET FLAVOR-NAME 'FLAVOR))
       (COND ((FLAVOR-COMPONENTS-DEFINED-P FLAVOR-NAME)
	      (OR (FLAVOR-DEPENDS-ON-ALL FL) (LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
					       (COMPOSE-FLAVOR-COMBINATION FL)))
	      (LET ((VARS (FLAVOR-ALL-INSTANCE-VARIABLES FL))
		    (MORE-VARS (GET (LOCF (FLAVOR-PLIST FL)) 'ADDITIONAL-SPECIAL-VARIABLES)))
		(AND MORE-VARS (SETQ VARS (APPEND MORE-VARS VARS)))
		(CONS 'SPECIAL VARS)))
	     (T		;Try to get as many variables as we can.
	      (CONS 'SPECIAL
		    (APPEND (GET (LOCF (FLAVOR-PLIST FL)) 'ADDITIONAL-SPECIAL-VARIABLES)
			    (MAP-OVER-COMPONENT-FLAVORS 0 NIL NIL
			      #'(LAMBDA (FL VL)
				  (DOLIST (X (FLAVOR-LOCAL-INSTANCE-VARIABLES FL))
				    (OR (ATOM X) (SETQ X (CAR X)))
				    (OR (MEMQ X VL) (PUSH X VL)))
				  VL)
			      FLAVOR-NAME NIL)))))))

;This is a flavor which is automatically made a component of nearly all
;other flavors.  It provides some basic facilities such as PRINT
;and DESCRIBE.

(EVAL-WHEN (LOAD EVAL)	;Allow this file to compile if it isn't loaded
(DEFFLAVOR VANILLA-FLAVOR () ()
  :NO-VANILLA-FLAVOR  ;No instance variables, no other flavors
  (:DOCUMENTATION :MIXIN "The default base flavor.
This flavor provides the normal handlers for the :PRINT, :DESCRIBE, and :WHICH-OPERATIONS
operations.  Only esoteric hacks should give the :NO-VANILLA-FLAVOR option to DEFFLAVOR to
prevent this inclusion."))
)

(DEFMETHOD (VANILLA-FLAVOR :PRINT-SELF) (STREAM &REST IGNORE)
  (SI:PRINTING-RANDOM-OBJECT (SELF STREAM)
    (PRINC (TYPEP SELF) STREAM)))

(DEFMETHOD (VANILLA-FLAVOR :DESCRIBE) ()
  (FORMAT T "~&~S, an object of flavor ~S,~% has instance variable values:~%"
	    SELF (TYPEP SELF))
  (DO ((BINDINGS (%P-CONTENTS-OFFSET (%P-CONTENTS-AS-LOCATIVE-OFFSET SELF 0)
				     %INSTANCE-DESCRIPTOR-BINDINGS)
		 (CDR BINDINGS))
       (SYM)
       (I 1 (1+ I)))
      ((NULL BINDINGS))
    (SETQ SYM (%FIND-STRUCTURE-HEADER (CAR BINDINGS)))
    (FORMAT T "	~S:~27T " SYM)
    (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE SELF I) DTP-NULL)
	   (FORMAT T "unbound~%"))
	  (T (FORMAT T "~S~%" (%P-CONTENTS-OFFSET SELF I))))))

;The default response to :WHICH-OPERATIONS is a list of all operations
;handled.  The list is consed up just once.  It is computed by examination
;of the dtp-select-method table, since that has no duplications.
;This goes to some pains to produce a cdr-coded list, for fast MEMQ'ing.
(DEFMETHOD (VANILLA-FLAVOR :WHICH-OPERATIONS) ()
  (LET ((FL (%MAKE-POINTER DTP-ARRAY-POINTER (%P-CONTENTS-AS-LOCATIVE-OFFSET SELF 0))))
    (OR (FLAVOR-WHICH-OPERATIONS FL)
	(SETF (FLAVOR-WHICH-OPERATIONS FL)
	      (LET ((S-M (%MAKE-POINTER DTP-LIST (FLAVOR-SELECT-METHOD FL))))
		(LET ((W-O (MAKE-LIST (LENGTH S-M) ':AREA PERMANENT-STORAGE-AREA)))
		  (DO ((S-M S-M (CDR S-M))
		       (R W-O (CDR R)))
		      ((ATOM S-M) W-O)
		    (RPLACA R (CAAR S-M)))))))))

(DEFMETHOD (VANILLA-FLAVOR :OPERATION-HANDLED-P) (OP)
  (LET* ((FL (%MAKE-POINTER DTP-ARRAY-POINTER (%P-CONTENTS-AS-LOCATIVE-OFFSET SELF 0)))
	 (WO (OR (FLAVOR-WHICH-OPERATIONS FL) (FUNCALL-SELF ':WHICH-OPERATIONS))))
    (NOT (NOT (MEMQ OP WO)))))

(DEFMETHOD (VANILLA-FLAVOR :SEND-IF-HANDLES) (OP &REST TO-SEND)
  (LET* ((FL (%MAKE-POINTER DTP-ARRAY-POINTER (%P-CONTENTS-AS-LOCATIVE-OFFSET SELF 0)))
	 (WO (OR (FLAVOR-WHICH-OPERATIONS FL) (FUNCALL-SELF ':WHICH-OPERATIONS))))
    (AND (MEMQ OP WO)
	 (LEXPR-FUNCALL-SELF OP TO-SEND))))

;This is useful for debugging.  E.g. you can get a break with all the
;instance variables bound.  If we go to lexical closures, this method
;will have to bind the special variables with the same names explicitly.
(DEFMETHOD (VANILLA-FLAVOR :EVAL-INSIDE-YOURSELF) (FORM)
  (EVAL FORM))

(DEFMETHOD (VANILLA-FLAVOR :FUNCALL-INSIDE-YOURSELF) (FUNCTION &REST ARGS)
  (APPLY FUNCTION ARGS))

(DEFMETHOD (VANILLA-FLAVOR :GET-HANDLER-FOR) (OP)
  (GET-HANDLER-FOR SELF OP))

;;; This flavor is a useful mixin that provides messages for a property list protocol.

(DEFFLAVOR PROPERTY-LIST-MIXIN ((PROPERTY-LIST NIL)) ()
  :SETTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION :MIXIN "A mixin that provides property list messages."))

(DEFMETHOD (PROPERTY-LIST-MIXIN :GET) (INDICATOR)
  (GET (LOCF PROPERTY-LIST) INDICATOR))

(DEFMETHOD (PROPERTY-LIST-MIXIN :GETL) (INDICATOR-LIST)
  (GETL (LOCF PROPERTY-LIST) INDICATOR-LIST))

(DEFMETHOD (PROPERTY-LIST-MIXIN :PUTPROP) (PROPERTY INDICATOR)
  (PUTPROP (LOCF PROPERTY-LIST) PROPERTY INDICATOR))

(DEFMETHOD (PROPERTY-LIST-MIXIN :REMPROP) (INDICATOR)
  (REMPROP (LOCF PROPERTY-LIST) INDICATOR))

(DEFMETHOD (PROPERTY-LIST-MIXIN :PUSH-PROPERTY) (PROPERTY INDICATOR)
  (PUSH PROPERTY (GET (LOCF PROPERTY-LIST) INDICATOR)))

(DEFMETHOD (PROPERTY-LIST-MIXIN :PLIST) () PROPERTY-LIST)

(DEFUN GET-HANDLER-FOR (FUNCTION OPERATION &OPTIONAL (SUPERIORS-P T) &AUX TEM)
  "Given a functional object, return its subfunction to do the given operation or NIL.
   Returns NIL if it does not reduce to a select-method or if it does not handle that."
  (DO-NAMED GET-HANDLER-FOR () (NIL)	;Repeat until reduced to a select-method (if possible)
    (SELECT (%DATA-TYPE FUNCTION)
      (DTP-ARRAY-POINTER
       (AND (NAMED-STRUCTURE-P FUNCTION)	;This is a crock (why?)
	    (SETQ FUNCTION (GET (NAMED-STRUCTURE-SYMBOL FUNCTION) 'NAMED-STRUCTURE-INVOKE))))
      (DTP-SYMBOL
       (OR (FBOUNDP FUNCTION) (RETURN NIL))
       (SETQ FUNCTION (FSYMEVAL FUNCTION)))
      ((DTP-ENTITY DTP-CLOSURE)
       (SETQ FUNCTION (CAR (%MAKE-POINTER DTP-LIST FUNCTION))))
      (DTP-SELECT-METHOD
       (SETQ FUNCTION (%MAKE-POINTER DTP-LIST FUNCTION))
       (DO () (NIL)			;Iterate down select-method, then continue with tail
	 (COND ((SYMBOLP (CAR FUNCTION))		;One level subroutine call
		(AND SUPERIORS-P
		     (SETQ TEM (GET-HANDLER-FOR FUNCTION OPERATION NIL))
		     (RETURN-FROM GET-HANDLER-FOR TEM)))
	       ((IF (LISTP (CAAR FUNCTION)) (MEMQ OPERATION (CAAR FUNCTION))
		    (EQ OPERATION (CAAR FUNCTION)))
		(RETURN-FROM GET-HANDLER-FOR (CDAR FUNCTION))))
	 (SETQ FUNCTION (CDR FUNCTION))
	 (OR (LISTP FUNCTION) (RETURN NIL))))
      (DTP-INSTANCE
       (SETQ FUNCTION (%P-CONTENTS-OFFSET (%P-CONTENTS-AS-LOCATIVE-OFFSET FUNCTION 0)
					  %INSTANCE-DESCRIPTOR-FUNCTION)))
      (OTHERWISE
       (RETURN-FROM GET-HANDLER-FOR NIL)))))

;;; Get the function that would handle an operation for a flavor
(DEFUN GET-FLAVOR-HANDLER-FOR (FLAVOR-NAME OPERATION &OPTIONAL (SUPERIORS-P T) &AUX FL)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (OR (FLAVOR-SELECT-METHOD FL) (COMPOSE-METHOD-COMBINATION FL))
  (GET-HANDLER-FOR (FLAVOR-SELECT-METHOD FL) OPERATION SUPERIORS-P))

;; (:HANDLER flavor operation) refers to the function that is called when
;;   an object of flavor FLAVOR is sent the message OPERATION.
;; Storing into this changes the select-method for that specific flavor
;;  which should make it possible to trace and so forth.
(DEFPROP :HANDLER HANDLER-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN HANDLER-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((FLAVOR (SECOND FUNCTION-SPEC))
	(MESSAGE (THIRD FUNCTION-SPEC)))
    ;; Checking structure like :INTERNAL
    (AND (SYMBOLP FLAVOR)
	 (LET ((FL (GET FLAVOR 'FLAVOR)))
	   (OR FL (FERROR NIL "In the function spec ~S, ~S is not the name of a flavor"
			      FUNCTION-SPEC FLAVOR))
	   ;; Do any composition (compilation) of combined stuff, if not done already
	   (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
	   (OR (FLAVOR-SELECT-METHOD FL) (COMPOSE-METHOD-COMBINATION FL))
	   (LET ((LOC (DO ((L (%MAKE-POINTER DTP-LIST (FLAVOR-SELECT-METHOD FL)) (CDR L)))
			  ((ATOM L) NIL)
			(IF (EQ (CAAR L) MESSAGE)
			    (RETURN (CAR L))))))	;CDR of this is the method
	     (OR (NOT (NULL LOC))
		 (MEMQ FUNCTION '(VALIDATE-FUNCTION-SPEC FDEFINEDP))
		 (FERROR NIL "The flavor ~S does not handle the ~S message" FLAVOR MESSAGE))
	     (SELECTQ FUNCTION
	       (VALIDATE-FUNCTION-SPEC (AND (= (LENGTH FUNCTION-SPEC) 3)
					    (SYMBOLP MESSAGE)))
	       (FDEFINE (RPLACD LOC ARG1))
	       (FDEFINITION (CDR LOC))
	       (FDEFINEDP LOC)
	       (FDEFINITION-LOCATION LOC)
	       (FUNDEFINE (FERROR NIL "FUNDEFINE is not implemented for :HANDLER"))
	       (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))
	       ))))))


(DEFPROP %INSTANCE-REF ((%INSTANCE-REF INSTANCE INDEX)
			%INSTANCE-SET VAL INSTANCE INDEX) SETF)

(DEFPROP %INSTANCE-REF ((%INSTANCE-REF INSTANCE INDEX)
			%INSTANCE-LOC INSTANCE INDEX) LOCF)

;This is in LMWIN;COLD
;(DEFUN SYMEVAL-IN-INSTANCE (INSTANCE PTR &OPTIONAL NO-ERROR-P)
;  (CHECK-ARG INSTANCE (= (%DATA-TYPE INSTANCE) DTP-INSTANCE) "an instance")
;  (AND (SYMBOLP PTR) (SETQ PTR (VALUE-CELL-LOCATION PTR)))
;  (LET ((N (FIND-POSITION-IN-LIST PTR (%P-CONTENTS-OFFSET
;					(%P-CONTENTS-AS-LOCATIVE-OFFSET INSTANCE 0)
;					%INSTANCE-DESCRIPTOR-BINDINGS))))
;    (COND (N (%INSTANCE-REF INSTANCE (1+ N)))
;	  (NO-ERROR-P NIL)
;	  (T
;	   (FERROR NIL "The variable ~S is not an instance variable of ~S"
;		   (%FIND-STRUCTURE-HEADER PTR) INSTANCE)))))

(DEFPROP SYMEVAL-IN-INSTANCE ((SYMEVAL-IN-INSTANCE INSTANCE PTR)
			      SET-IN-INSTANCE INSTANCE PTR VAL) SETF)
(DEFUN SET-IN-INSTANCE (INSTANCE PTR VAL)
  (CHECK-ARG INSTANCE (= (%DATA-TYPE INSTANCE) DTP-INSTANCE) "an instance")
  (AND (SYMBOLP PTR) (SETQ PTR (VALUE-CELL-LOCATION PTR)))
  (LET ((N (FIND-POSITION-IN-LIST PTR (%P-CONTENTS-OFFSET
					(%P-CONTENTS-AS-LOCATIVE-OFFSET INSTANCE 0)
					%INSTANCE-DESCRIPTOR-BINDINGS))))
    (IF N
	(%INSTANCE-SET VAL INSTANCE (1+ N))
	(FERROR NIL "The variable ~S is not an instance variable of ~S"
		(%FIND-STRUCTURE-HEADER PTR) INSTANCE))))

(DEFPROP SYMEVAL-IN-INSTANCE ((SYMEVAL-IN-INSTANCE INSTANCE PTR)
			      LOCATE-IN-INSTANCE INSTANCE PTR) LOCF)
(DEFUN LOCATE-IN-INSTANCE (INSTANCE PTR)
  (CHECK-ARG INSTANCE (= (%DATA-TYPE INSTANCE) DTP-INSTANCE) "an instance")
  (AND (SYMBOLP PTR) (SETQ PTR (VALUE-CELL-LOCATION PTR)))
  (LET ((N (FIND-POSITION-IN-LIST PTR (%P-CONTENTS-OFFSET
					(%P-CONTENTS-AS-LOCATIVE-OFFSET INSTANCE 0)
					%INSTANCE-DESCRIPTOR-BINDINGS))))
    (IF N
	(%INSTANCE-LOC INSTANCE (1+ N))
	(FERROR NIL "The variable ~S is not an instance variable of ~S"
		(%FIND-STRUCTURE-HEADER PTR) INSTANCE))))

;Interface to the compiler.
;If called in *JUST-COMPILING* mode, during a QC-FILE, sends its output into the QFASL file.
;If called during a compilation to core, for instance from
;the editor c-t-C command, compiles to core as part of the compilation
;in progress (assuming you are in the top level macro-expanding part of the
;compiler rather than deep inside its guts).  If called at a random time,
;simply compiles to core.
;Note that if LOCAL-DECLARATIONS is bound when this is called it will be obeyed.
(DEFUN COMPILE-AT-APPROPRIATE-TIME (FL NAME LAMBDA-EXP &OPTIONAL FORM-TO-EVAL)
  ;; Switch to the appropriate package so gensyms get defined in that package and
  ;; and error messages about wrong package defining a function are avoided.  But
  ;; if compiling, don't mess with the package, so that symbols in the qfasl file
  ;; get interned in the proper place.
  (LET ((PACKAGE (IF COMPILER:QC-FILE-IN-PROGRESS PACKAGE
		     (FLAVOR-PACKAGE FL))))
    (IF COMPILER:QC-FILE-IN-PROGRESS
	;; This case if in QC-FILE or editor-compile
	(COMPILER:QC-TRANSLATE-FUNCTION
	   NAME LAMBDA-EXP 'COMPILER:MACRO-COMPILE
	   (IF (AND (NOT COMPILER:QC-FILE-LOAD-FLAG) *JUST-COMPILING*)
	       'COMPILER:QFASL 'COMPILER:COMPILE-TO-CORE))
	;; This case if not doing anything special
	(LET ((FDEFINE-FILE-PATHNAME NIL)
	      (INHIBIT-FDEFINE-WARNINGS T))
	  (PUSH NAME *FLAVOR-COMPILATIONS*)
	  (COMPILER:COMPILE NAME LAMBDA-EXP)))
    ;; Evaluate form now or send it over in the qfasl file
    (AND FORM-TO-EVAL
	 (IF (AND COMPILER:QC-FILE-IN-PROGRESS (NOT COMPILER:QC-FILE-LOAD-FLAG)
		  *JUST-COMPILING*)
	     (COMPILER:FASD-FORM FORM-TO-EVAL)
	     (EVAL FORM-TO-EVAL)))))

;This macro takes flavor names as "arguments".  It causes the compiler
;to include the appropriate methods in the qfasl file, provided all the
;component flavors are defined.
(DEFMACRO COMPILE-FLAVOR-METHODS (&REST FLAVOR-NAMES)
  `(PROGN 'COMPILE
     (EVAL-WHEN (COMPILE EVAL)
       . ,(MAPCAN #'(LAMBDA (FLAVOR-NAME)
		      (NCONC (AND (GET FLAVOR-NAME 'FLAVOR)
				  (NCONS `(PUTPROP (LOCF (FLAVOR-PLIST
							   (GET ',FLAVOR-NAME 'FLAVOR)))
						   T
						   'COMPILE-FLAVOR-METHODS)))
			     (NCONS `(COMPILE-FLAVOR-METHODS-1 ',FLAVOR-NAME))))
		  FLAVOR-NAMES))
     (EVAL-WHEN (LOAD EVAL)
       . ,(MAPCAR #'(LAMBDA (FLAVOR-NAME) `(COMPILE-FLAVOR-METHODS-2 ',FLAVOR-NAME))
		  FLAVOR-NAMES))))

;; Cause the combined-methods to get compiled.
(DEFUN COMPILE-FLAVOR-METHODS-1 (FLAVOR-NAME &AUX FL)
  (COND ((FLAVOR-COMPONENTS-DEFINED-P FLAVOR-NAME 'COMPILE-FLAVOR-METHODS)
	 (SETQ FL (GET FLAVOR-NAME 'FLAVOR))
	 (OR (FLAVOR-DEPENDS-ON-ALL FL)
	     (COMPOSE-FLAVOR-COMBINATION FL))
	 (LET ((*JUST-COMPILING* T)
	       (*USE-OLD-COMBINED-METHODS* NIL))
	   (COMPOSE-METHOD-COMBINATION FL NIL)))))

;; Do the composition now.  This should normally just generate data-structure
;; as the methods should already all have been compiled, unless something has changed.
(DEFUN COMPILE-FLAVOR-METHODS-2 (FLAVOR-NAME &AUX FL)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  (PUTPROP (LOCF (FLAVOR-PLIST FL)) (OR FDEFINE-FILE-PATHNAME T) 'COMPILE-FLAVOR-METHODS)
  (COND ((FLAVOR-COMPONENTS-DEFINED-P FLAVOR-NAME)
	 (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
	 (OR (FLAVOR-SELECT-METHOD FL) (COMPOSE-METHOD-COMBINATION FL))))
  FLAVOR-NAME)

;Returns T if all components of this flavor are defined
(DEFUN FLAVOR-COMPONENTS-DEFINED-P (FLAVOR-NAME &OPTIONAL COMPLAINT &AUX FL)
  (COND ((SETQ FL (GET FLAVOR-NAME 'FLAVOR))
	 (OR (NOT (NULL (FLAVOR-DEPENDS-ON-ALL FL)))	;Already composed, be fast
	     (AND (DO ((L (FLAVOR-DEPENDS-ON FL) (CDR L))) ((NULL L) T)
		    (OR (FLAVOR-COMPONENTS-DEFINED-P (CAR L)) (RETURN NIL)))
		  (DO ((L (FLAVOR-INCLUDES FL) (CDR L))) ((NULL L) T)
		    (OR (FLAVOR-COMPONENTS-DEFINED-P (CAR L)) (RETURN NIL))))))
	(COMPLAINT (FORMAT ERROR-OUTPUT "~&~A - ~S undefined flavor" COMPLAINT FLAVOR-NAME)
		   NIL)
	(T NIL)))
