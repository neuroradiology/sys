;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-
;;;  ** (c) Copyright 1981 Massachusetts Institute of Technology **

;;; Host objects:

;;; Host objects have two major uses.  One, they identify a "file
;;; computer" to use inside a pathname.  Two, they serve to canonicalize
;;; name and address mapping on networks.  A comment at the start of AI:
;;; LMIO; PATHNM > describes in more detail the pathname parsing
;;; interaction with host objects.

;;; The flavor SI:BASIC-HOST is included in host objects of all types.
;;; The flavor SI:HOST is for hosts which actually correspond to a
;;; particular machine on some network (as opposed to a logical host in
;;; the pathname sense).

;;; Interesting messages on all hosts
;;; :NAME - returns the whole name of the host
;;; :NAME-AS-FILE-COMPUTER - returns what should be printed before the : in pathnames
;;; :PATHNAME-HOST-NAMEP name - is name the name of this host before the : in a pathname?

;;; Interesting messages on network hosts
;;; :SHORT-NAME - returns the shortest nickname from the host table
;;; :SYSTEM-TYPE - returns the operating system type, a symbol in the keyword package, e.g.
;;;                :ITS, :LISPM
;;; :NETWORK-TYPEP network - is this host connected to network, a keyword, e.g. :CHAOS?
;;; :NETWORK-TYPE - returns the major network this host is connected to (i.e. prefers
;;;                :CHAOS)

;;; Relevant functions
;;; SI:PARSE-HOST thing - takes a string and returns a host object corresponding to the
;;;		          network host with that name.  NOT the function used with pathnames.
;;; SI:DEFINE-HOST name &rest options - the form written into host table files.  Defines
;;;					a new network host.  Special options include:
;;;	:SYSTEM-TYPE keyword
;;;	:HOST-NAMES list-of-nicknames
;;;				        All other options are taken as a network type and
;;;				        the value as a list of addresses.
;;;				        The function CHAOS:GENERATE-HOST-TABLE-1 knows how
;;;				        to make a set of these forms from the HOSTS2 CHAOS
;;;				        host table.
;;; SI:SET-HOST-FLAVOR-KEYWORDS flavor keywords
;;;	- sets the association between an operating system type and set of networks and the
;;;	  host object flavor.  CAR of keywords is the operating system, CDR the list of
;;;	  networks.  E.g. (SI:SET-HOST-FLAVOR-KEYWORDS ITS-CHAOS-HOST '(:ITS :CHAOS)).
;;; SI:COMPILE-HOST-FLAVOR-COMBINATION system-type &rest network-types
;;;	- special form that generates the right compile-flavor-methods for an automatic
;;;	  host flavor combination.
;;; SI:GET-HOST-FROM-ADDRESS address network - returns a host object for address on network.
;;;	E.g. (SI:GET-HOST-FROM-ADDRESS 2026 ':CHAOS) => #<MUMBLE MIT-AI>.

;;; Dynamic host flavor association
;;; Network hosts are instantiated as a flavor from SI:HOST-FLAVOR-ALIST.
;;; Entries on this alist are (flavor system-type . networks).
;;; To make an explicit association (i.e. include a special type of file computer), make
;;; the flavor combination yourself and call SI:SET-HOST-FLAVOR-KEYWORDS to put something
;;; on this alist.  This will take care of fixing up old host instances with those same
;;; keywords but the wrong flavor.
;;; If such an explicit entry if not found in the alist for a combination of system-type
;;; and network types, a flavor combination is made up at runtime.
;;; This is done by mixing (or (get system-type 'system-type-flavor)
;;;			       (get ':default 'system-type-flavor))
;;; with (get network-type 'network-type-flavor).  For example, when the
;;; first VAX is found on the chaosnet, HOST-VAX-MIXIN is combined with
;;; HOST-CHAOS-MIXIN to make VAX-CHAOS-HOST, which is instantiated for
;;; this host.
;;; SI:COMPILE-HOST-FLAVOR-COMBINATION should be used for premaking
;;; these automatic association as COMPILE-FLAVOR-METHODS for speed.

;;; Base flavor, for network and special hosts.
(DEFFLAVOR BASIC-HOST () ()
  (:REQUIRED-METHODS :NAME)
  (:METHOD-COMBINATION (:OR :BASE-FLAVOR-LAST :PATHNAME-HOST-NAMEP)
		       (:LIST :BASE-FLAVOR-FIRST :PEEK-FILE-SYSTEM-HEADER
						 :PEEK-FILE-SYSTEM)))

(DEFMETHOD (BASIC-HOST :INIT) (IGNORE) NIL)

;;; STRING of a host is its name
(DEFMETHOD (BASIC-HOST :STRING-FOR-PRINTING) ()
  (FUNCALL-SELF ':NAME))

(DEFMETHOD (BASIC-HOST :PRINT-SELF) (STREAM IGNORE SLASHIFY-P)
  (IF SLASHIFY-P
      (SI:PRINTING-RANDOM-OBJECT (SELF STREAM :NO-POINTER)
	(PRINC (TYPEP SELF) STREAM)
	(FUNCALL STREAM ':TYO #\SP)
	(PRINC (FUNCALL-SELF ':NAME) STREAM))
      (FUNCALL STREAM ':STRING-OUT (FUNCALL-SELF ':NAME))))

(DEFMETHOD (BASIC-HOST :SHORT-NAME) ()
  (FUNCALL-SELF ':NAME))

(DEFMETHOD (BASIC-HOST :NAME-AS-FILE-COMPUTER) ()
  (FUNCALL-SELF ':NAME))

(DEFMETHOD (BASIC-HOST :DEFAULT :PATHNAME-HOST-NAMEP) (NAME)
  (STRING-EQUAL NAME (FUNCALL-SELF ':NAME)))

(DEFMETHOD (BASIC-HOST :OPEN-STREAMS) () NIL)

(DEFMETHOD (BASIC-HOST :CLOSE-ALL-FILES) ()
  (LOOP FOR STREAM IN (FUNCALL-SELF ':OPEN-STREAMS)
	DO (FORMAT ERROR-OUTPUT "~%Closing ~S" STREAM)
	   (FUNCALL STREAM ':CLOSE ':ABORT)
	COLLECT STREAM))

(DEFMETHOD (BASIC-HOST :DEFAULT :PEEK-FILE-SYSTEM-HEADER) ()
  NIL)

(DEFMETHOD (BASIC-HOST :DEFAULT :PEEK-FILE-SYSTEM) ()
  NIL)

;;; General (multi-network) host table.
(DEFVAR HOST-ALIST NIL)

(DEFSTRUCT (HOST-ALIST-ELEM :LIST* (:CONC-NAME HOST-))
  NAME
  INSTANCE
  NAME-LIST
  SYSTEM-TYPE-INTERNAL
  SITE-NAME
  ADDRESSES)

;;; This is the function written out in the host table.
(DEFUN DEFINE-HOST (NAME &REST OPTIONS &AUX ELEM OLD-P)
  (IF (NULL (SETQ ELEM (ASSOC NAME HOST-ALIST)))
      (SETQ ELEM (MAKE-HOST-ALIST-ELEM NAME NAME))
      (SETQ OLD-P T)
      (SETF (HOST-ADDRESSES ELEM) NIL))
  (SETF (HOST-SITE-NAME ELEM) SITE-NAME)
  (LOOP FOR (OPTION VALUE) ON OPTIONS BY 'CDDR
	DO (SELECTQ OPTION
	     (:HOST-NAMES (SETF (HOST-NAME-LIST ELEM) VALUE))
	     (:SYSTEM-TYPE (SETF (HOST-SYSTEM-TYPE-INTERNAL ELEM) VALUE))
	     (OTHERWISE (PUTPROP (LOCF (HOST-ADDRESSES ELEM)) VALUE OPTION))))
  (IF (NOT OLD-P)
      (PUSH ELEM HOST-ALIST)
      (LET ((OLD-INSTANCE (HOST-INSTANCE ELEM)))
	(AND OLD-INSTANCE
	     (LET ((FLAVOR (COMPUTE-HOST-FLAVOR ELEM)))
	       (AND (NEQ FLAVOR (TYPEP OLD-INSTANCE))
		    (LET ((NEW-INSTANCE (MAKE-INSTANCE FLAVOR ':ALIST-ELEM ELEM)))
		      ;; If incorrect flavor, make new one now.
		      (STRUCTURE-FORWARD OLD-INSTANCE NEW-INSTANCE))))))))

;;; After new host table has been loaded, take any old hosts and forget about their
;;; addresses.  They stay around that way, if pointed to by pathnames, but aren't
;;; usable.
(DEFUN RESET-NON-SITE-HOSTS ()
  (LOOP FOR ELEM IN HOST-ALIST
	WHEN (NEQ (HOST-SITE-NAME ELEM) SITE-NAME)
	DO (SETF (HOST-ADDRESSES ELEM) NIL)))

;;; This should happen after the SITE initialization to load the host table which is
;;; in QMISC.
(ADD-INITIALIZATION "RESET-NON-SITE-HOSTS" '(RESET-NON-SITE-HOSTS) '(SITE NORMAL))

;;; These are hosts of the host table sort
(DEFFLAVOR HOST
	(ALIST-ELEM)
	(BASIC-HOST)
  (:INITABLE-INSTANCE-VARIABLES))

(DEFMETHOD (HOST :NAME) ()
  (HOST-NAME ALIST-ELEM))

(DEFMETHOD (HOST :HOST-NAMES) ()
  (HOST-NAME-LIST ALIST-ELEM))

(DEFMETHOD (HOST :PATHNAME-HOST-NAMEP) (NAME)
  (MEM #'STRING-EQUAL NAME (HOST-NAME-LIST ALIST-ELEM)))

(DEFMETHOD (HOST :SHORT-NAME) ()
  (OR (FIRST (HOST-NAME-LIST ALIST-ELEM))
      (HOST-NAME ALIST-ELEM)))

(DEFUN HOST-SHORT-NAME (HOST)
  (FUNCALL (PARSE-HOST HOST) ':SHORT-NAME))

(DEFMETHOD (HOST :NAME-AS-FILE-COMPUTER) ()
  (FUNCALL-SELF ':SHORT-NAME))

(DEFMETHOD (HOST :SYSTEM-TYPE) ()
  (HOST-SYSTEM-TYPE-INTERNAL ALIST-ELEM))

(DEFUN HOST-SYSTEM-TYPE (HOST)
  (FUNCALL (PARSE-HOST HOST) ':SYSTEM-TYPE))

(DEFMETHOD (HOST :NETWORK-TYPE) ()
  (CAR (HOST-ADDRESSES ALIST-ELEM)))

(DEFMETHOD (HOST :NETWORK-TYPEP) (TYPE)
  (NOT (NULL (GET (LOCF (HOST-ADDRESSES ALIST-ELEM)) TYPE))))

(DEFUN HOST-NETWORK-TYPE (HOST)
  (FUNCALL (PARSE-HOST HOST) ':NETWORK-TYPE))

(DEFMETHOD (HOST :SET-SITE) (NEW-SITE)
  (SETF (HOST-SITE-NAME ALIST-ELEM) NEW-SITE))

;;; List of (flavor system-type . network-types)
(DEFVAR HOST-FLAVOR-ALIST NIL)

(DEFUN SET-HOST-FLAVOR-KEYWORDS (FLAVOR KEYWORDS &AUX ELEM)
  (IF (NULL (SETQ ELEM (RASSOC KEYWORDS HOST-FLAVOR-ALIST)))
      (PUSH (CONS FLAVOR KEYWORDS) HOST-FLAVOR-ALIST)
      (LET ((OLD-FLAVOR (CAR ELEM)))
	(COND ((NEQ OLD-FLAVOR FLAVOR)
	       (SETF (CAR ELEM) FLAVOR)
	       ;; If a new flavor now handles this system/network combination, fix up all old
	       ;; instances.
	       (LOOP FOR HELEM IN HOST-ALIST
		     AS INSTANCE = (HOST-INSTANCE HELEM)
		     WHEN (TYPEP INSTANCE OLD-FLAVOR)
		     DO (STRUCTURE-FORWARD INSTANCE
					   (MAKE-INSTANCE FLAVOR ':ALIST-ELEM HELEM))))))))

(DEFUN COMPUTE-HOST-FLAVOR (ELEM &AUX KEYWORDS FLAVOR)
  (SETQ KEYWORDS (CONS (LET ((SYSTEM-TYPE (HOST-SYSTEM-TYPE-INTERNAL ELEM)))
			 (IF (NULL (GET SYSTEM-TYPE 'SYSTEM-TYPE-FLAVOR))
			     ':DEFAULT SYSTEM-TYPE))
		       (LOOP FOR NET IN (HOST-ADDRESSES ELEM) BY 'CDDR
			     WHEN (NOT (NULL (GET NET 'NETWORK-TYPE-FLAVOR)))
			     COLLECT NET)))
  (IF (SETQ FLAVOR (RASSOC KEYWORDS HOST-FLAVOR-ALIST))
      (CAR FLAVOR)
      (SETQ FLAVOR (COMPOSE-HOST-FLAVOR-NAME KEYWORDS))
      (SET-HOST-FLAVOR-KEYWORDS FLAVOR KEYWORDS)
      (LET ((FLAVORS `(,(GET (CAR KEYWORDS) 'SYSTEM-TYPE-FLAVOR)
		       ,@(LOOP FOR NET IN (CDR KEYWORDS)
			       COLLECT (GET NET 'NETWORK-TYPE-FLAVOR))
		       HOST)))
	(DEFFLAVOR1 FLAVOR NIL FLAVORS NIL))
      (COMPOSE-AUTOMATIC-METHODS (GET FLAVOR 'FLAVOR))
      FLAVOR))

(DEFUN COMPOSE-HOST-FLAVOR-NAME (KEYWORDS)
  (DO ((STRING (MAKE-ARRAY 20 ':TYPE 'ART-STRING ':LEADER-LIST '(0)))
       (L KEYWORDS (CDR L)))
      ((NULL L)
       (STRING-NCONC STRING "HOST")
       (INTERN STRING "SI"))
    (STRING-NCONC STRING (CAR L) #/-)))

;;; This is useful for causing flavor combinations to be saved out
(DEFMACRO COMPILE-HOST-FLAVOR-COMBINATION (SYSTEM-TYPE &REST NETWORK-TYPES)
  (LET ((FLAVORS `(,(GET SYSTEM-TYPE 'SYSTEM-TYPE-FLAVOR)
		   ,@(LOOP FOR NET IN NETWORK-TYPES
			   COLLECT (GET NET 'NETWORK-TYPE-FLAVOR))
		   HOST))
	(FLAVOR (COMPOSE-HOST-FLAVOR-NAME (CONS SYSTEM-TYPE NETWORK-TYPES))))
    `(PROGN 'COMPILE
       (DEFFLAVOR ,FLAVOR () ,FLAVORS)
       (SET-HOST-FLAVOR-KEYWORDS ',FLAVOR ',(CONS SYSTEM-TYPE NETWORK-TYPES))
       (COMPILE-FLAVOR-METHODS ,FLAVOR))))

;;; This can be set by the chaosnet or whatever.
(DEFVAR UNKNOWN-HOST-FUNCTION NIL)

(DEFUN PARSE-HOST (HOST &OPTIONAL NO-ERROR-P UNKNOWN-OK &AUX ELEM)
  (COND ((TYPEP HOST 'HOST) HOST)
	((AND (SETQ ELEM (LOOP FOR ELEM IN HOST-ALIST
			       WHEN (MEM #'STRING-EQUAL HOST (HOST-NAME-LIST ELEM))
			       RETURN ELEM))
	      (NOT (NULL (HOST-ADDRESSES ELEM))))
	 (GET-ALIST-ELEM-HOST ELEM))
	((AND UNKNOWN-OK UNKNOWN-HOST-FUNCTION)
	 (FUNCALL UNKNOWN-HOST-FUNCTION HOST)
	 (PARSE-HOST HOST NO-ERROR-P NIL))
	(NO-ERROR-P
	 NIL)
	(T
	 (FERROR NIL "~S is not a known host" HOST))))

(DEFUN GET-ALIST-ELEM-HOST (ELEM)
  (OR (HOST-INSTANCE ELEM)
      (LET ((INSTANCE (MAKE-INSTANCE (COMPUTE-HOST-FLAVOR ELEM) ':ALIST-ELEM ELEM)))
	(SETF (HOST-INSTANCE ELEM) INSTANCE)
	INSTANCE)))

(DEFUN GET-HOST-FROM-ADDRESS (ADDRESS NETWORK)
  (LOOP FOR ELEM IN SI:HOST-ALIST
	WHEN (MEMQ ADDRESS (GET (LOCF (SI:HOST-ADDRESSES ELEM)) NETWORK))
	RETURN (GET-ALIST-ELEM-HOST ELEM)))

(DEFUN MAKE-UNNAMED-HOST (SYSTEM-TYPE ADDRESSES &AUX ELEM)
  (SETQ ELEM (MAKE-HOST-ALIST-ELEM NAME "UNKNOWN"
				   SYSTEM-TYPE-INTERNAL SYSTEM-TYPE
				   SITE-NAME SITE-NAME
				   ADDRESSES ADDRESSES))
  (PUSH ELEM HOST-ALIST)
  (GET-ALIST-ELEM-HOST ELEM))

(DEFPROP :DEFAULT DEFAULT-SYSTEM-TYPE-MIXIN SYSTEM-TYPE-FLAVOR)

(DEFFLAVOR DEFAULT-SYSTEM-TYPE-MIXIN () ()
  (:INCLUDED-FLAVORS HOST))

;;; These flavors aren't used for anything yet.  But when someone needs tops-20 pathnames
;;; off the chaosnet, for instance, they will help.
(DEFPROP :ITS HOST-ITS-MIXIN SYSTEM-TYPE-FLAVOR)

(DEFFLAVOR HOST-ITS-MIXIN () ()
  (:INCLUDED-FLAVORS HOST))

(DEFPROP :TOPS-20 HOST-TOPS20-MIXIN SYSTEM-TYPE-FLAVOR)

(DEFFLAVOR HOST-TOPS20-MIXIN () ()
  (:INCLUDED-FLAVORS HOST))

(DEFPROP :TENEX HOST-TENEX-MIXIN SYSTEM-TYPE-FLAVOR)

(DEFFLAVOR HOST-TENEX-MIXIN () ()
  (:INCLUDED-FLAVORS HOST))

(DEFPROP :UNIX HOST-UNIX-MIXIN SYSTEM-TYPE-FLAVOR)

(DEFFLAVOR HOST-UNIX-MIXIN () ()
  (:INCLUDED-FLAVORS HOST))

(DEFPROP :MULTICS HOST-MULTICS-MIXIN SYSTEM-TYPE-FLAVOR)

(DEFFLAVOR HOST-MULTICS-MIXIN () ()
  (:INCLUDED-FLAVORS HOST))

(DEFPROP :VMS HOST-VMS-MIXIN SYSTEM-TYPE-FLAVOR)

(DEFFLAVOR HOST-VMS-MIXIN () ()
  (:INCLUDED-FLAVORS HOST))

(DEFPROP :LISPM HOST-LISPM-MIXIN SYSTEM-TYPE-FLAVOR)

(DEFFLAVOR HOST-LISPM-MIXIN () ()
  (:INCLUDED-FLAVORS HOST))

;;; LISPM Location stuff
(DEFVAR LOCAL-HOST)
(DEFVAR LOCAL-HOST-NAME)
(DEFVAR LOCAL-PRETTY-HOST-NAME)
(DEFVAR LOCAL-FINGER-LOCATION)
(DEFVAR LOCAL-FLOOR-LOCATION)
(DEFVAR ASSOCIATED-MACHINE "AI")

(DEFUN SET-LOCAL-HOST-VARIABLES (&AUX ELEM)
  (SETQ LOCAL-HOST-NAME (FUNCALL LOCAL-HOST ':NAME))
  (IF (SETQ ELEM (ASSOC LOCAL-HOST-NAME MACHINE-LOCATION-ALIST))
      (SETF `(LOCAL-HOST-NAME ,LOCAL-PRETTY-HOST-NAME ,LOCAL-FINGER-LOCATION
	      ,LOCAL-FLOOR-LOCATION ,ASSOCIATED-MACHINE)
	    ELEM)
      (SETQ LOCAL-PRETTY-HOST-NAME "Unknown"
	    LOCAL-FINGER-LOCATION "(Unknown)"
	    LOCAL-FLOOR-LOCATION '(UNKNOWN 0)))
  (SETQ ASSOCIATED-MACHINE (FS:GET-PATHNAME-HOST ASSOCIATED-MACHINE)))

(ADD-INITIALIZATION "SET-LOCAL-HOST-VARIABLES" '(SET-LOCAL-HOST-VARIABLES) '(WARM))
