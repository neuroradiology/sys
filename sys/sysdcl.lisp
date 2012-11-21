;;;-*- Mode:LISP; Package:SYSTEM-INTERNALS -*-
;;; Declarations for SYSTEM's initally loaded
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFSYSTEM SYSTEM
  (:NAME "System")
  (:SHORT-NAME "SYS")
  (:PATCHABLE "SYS: PATCH;")
  (:MODULE ALLDEFS ("SYS: SYS2; DEFMAC"		;These are defs files for whole system
		    "SYS: SYS2; LMMAC"
		    "SYS: SYS2; STRUCT"
		    "SYS: SYS2; SETF")
	   :PACKAGE SI)
  (:COMPONENT-SYSTEMS FONTS		;Before SYSTEM-INTERNALS because things use fonts
		      SYSTEM-INTERNALS
		      FORMAT		;a NO-OP if loading for first time
#-XEROX		      CHAOS		;likewise
#+XEROX		      ETHER
		      COMPILER
		      FILE-SYSTEM
		      QFASL-REL
		      TIME		;must be before TV
		      TV
		      PEEK
;		      SUPDUP
		      ZWEI
;		      FED
;		      COLOR
		      EH
		      CADR
;		      PRESS
		      MICRO-ASSEMBLER
		      MATH
		      HACKS
		      METER
		      SRCCOM
		      )
  (:COMPILE-LOAD ALLDEFS)
  (:DO-COMPONENTS (:FASLOAD ALLDEFS)))

(DEFSYSTEM SYSTEM-INTERNALS
  (:PACKAGE SYSTEM-INTERNALS)
  (:NICKNAMES "SI")
  (:MODULE DEFS ("SYS: SYS2; PRODEF"
		 "SYS: IO; RDDEFS"
		 "SYS: SYS2; SGDEFS"))
  (:MODULE METH "SYS: SYS2; METH")
  (:MODULE CLASS "SYS: SYS2; CLASS") 
  (:MODULE MAIN ("SYS: SYS2; STRING"
		 "SYS: SYS; QMISC"
		 "SYS: SYS; LTOP"
		 "SYS: WINDOW; COLD"
		 "SYS: IO; PRINT"
		 "SYS: IO; DEBUG"
		 "SYS: SYS; QEV"
		 "SYS: SYS; QFASL"
		 "SYS: IO; MINI"
;		 "SYS: NETWORK; TFTP-MINI"
		 "SYS: IO; QIO"
		 "SYS: SYS; QRAND"
		 "SYS: IO; READ"
		 "SYS: SYS; SGFCTN"
		 "SYS: SYS2; ADVISE"
		 "SYS: SYS2; BAND"
		 "SYS: SYS2; DEFSEL"
		 "SYS: IO; DISK"
		 "SYS: IO; DLEDIT"
		 "SYS: IO; DRIBBL"
		 "SYS: SYS2; ENCAPS"
		 "SYS: SYS2; FLAVOR"
		 "SYS: SYS2; GC"
		 "SYS: IO; GRIND"
		 "SYS: SYS2; HASH"
		 "SYS: SYS2; HOST"
		 "SYS: SITE; LMLOCS"
		 "SYS: SYS2; LOGIN"
		 "SYS: SYS2; LOOP"
		 "SYS: SYS2; MAKSYS"
		 "SYS: SYS2; NUMER"
		 "SYS: SYS; PACK4"
		 "SYS: SYS2; PATCH"
		 "SYS: SYS2; PLANE"
		 "SYS: SYS2; PROCES"
		 "SYS: SYS; QFCTNS"
		 "SYS: SYS2; QTRACE"
		 "SYS: SYS2; RAT"
		 "SYS: SYS2; SELEV"
		 "SYS: IO1; SERIAL"
		 "SYS: SITE; SITE"
		 "SYS: SYS; SORT"
		 "SYS: SYS2; STEP"
		 "SYS: IO; STREAM"
		 "SYS: SYS; SYSDCL"
		 "SYS: SYS2; UNFASL"
		 "SYS: IO; UNIBUS"
;		 "SYS: IO1; XGP"
		))
  (:MODULE RDTBL "SYS: IO; RDTBL")
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
  (:FASLOAD RDTBL)
  (:COMPILE-LOAD METH (:FASLOAD DEFS))
  (:COMPILE-LOAD CLASS (:FASLOAD DEFS) (:FASLOAD METH)))

(DEFSYSTEM FONTS
  (:PACKAGE FONTS)
  (:PATHNAME-DEFAULT "SYS: FONTS;")
  (:FASLOAD ("TVFONT" "CPTFONTB" "BIGFNT" "TINY" "5X5"
	     "MEDFNT" "MEDFNB" "METS" "METSI" "43VXMS"
	     "HL6" "HL7" "HL10" "HL10B" "HL12" "HL12I" "HL12B" "HL12BI"
	     "TR8" "TR8I" "TR8B" "TR10" "TR10I" "TR10B" "TR10BI"
	     "MOUSE" "SEARCH" "TOG" "ABACUS")))
#-XEROX
(DEFSYSTEM CHAOS
  (:PACKAGE CHAOS)
  (:MODULE NCP ("SYS: IO; CHSNCP" "SYS: IO; CHSAUX"))
  (:MODULE TEST "SYS: IO1; CHATST")
; (:MODULE EFTP "SYS: IO1; EFTP")
  (:COMPILE-LOAD (NCP TEST)) ; was EFTP
  (:COMPILE-LOAD (:GENERATE-HOST-TABLE (("SYS: CHAOS; HOSTS" "SYS: SITE; HSTTBL")))))

#+XEROX
(DEFSYSTEM ETHER
  (:PACKAGE ETHER)
  (:SHORT-NAME "ETH")
  (:PATHNAME-DEFAULT "SYS: ETHER;")
  (:PATCHABLE)
  (:MODULE DEFS "EDEFS")
  (:MODULE QCOM "UQCOM")
  (:MODULE MAIN ("MISC" "ETHER" "CHAT" "ROUTE" "PUP1" "RTP" "BSP" "FTP" "EDIR" "ENAME"
		 "EMAIL" "FILES" "SERVER"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD QCOM (:FASLOAD DEFS))
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS QCOM)))

(DEFSYSTEM TIME
  (:PACKAGE TIME)
  (:PATHNAME-DEFAULT "SYS: IO1;")
  (:COMPILE-LOAD ("TIME" "TIMPAR")))

(DEFSYSTEM SUPDUP
  (:PACKAGE SUPDUP)
  (:COMPILE-LOAD ("SYS: WINDOW; SUPDUP")))

(DEFSYSTEM PRESS
  (:PACKAGE PRESS)
  (:PATHNAME-DEFAULT "SYS: IO1;")
  (:MODULE RFONTW "RFONTW")
  (:MODULE PRESS "PRESS")
  (:MODULE FONTW "PRESS-FONTS; FONTS WIDTHS >")
  (:COMPILE-LOAD RFONTW)
  (:COMPILE-LOAD PRESS)
  (:LOAD-FONTS-WIDTHS FONTW (:FASLOAD RFONTW)))

(DEFSYSTEM FORMAT
  (:PACKAGE FORMAT)
  (:COMPILE-LOAD ("SYS: IO; FORMAT"
		  "SYS: IO1; FQUERY"
		  "SYS: IO1; OUTPUT")))

(DEFSYSTEM QFASL-REL
  (:PACKAGE QFASL-REL)
  (:PATHNAME-DEFAULT "SYS: IO1;")
  (:COMPILE-LOAD ("RELLD" "RELDMP")))

(DEFSYSTEM COMPILER
  (:PACKAGE COMPILER)
  (:MODULE DEFS ("SYS: SYS; MADEFS"
		 "SYS: SYS; QCDEFS"))
  (:MODULE MAIN ("SYS: SYS2; DISASS"
		 "SYS: SYS; MA"
		 "SYS: SYS; MAOPT"
		 "SYS: SYS; MC"
		 "SYS: SYS; MLAP"
		 "SYS: SYS2; QFASD"
		 "SYS: SYS2; QCFILE"
		 "SYS: SYS; QCP1"
		 "SYS: SYS; QCP2"
		 "SYS: SYS; QCOPT"
		 "SYS: SYS2; PEEP"
		 "SYS: SYS; QLF"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
  (:READFILE ("SYS: COLD; DEFMIC"
	      "SYS: SYS; DOCMIC"))
  (:FASLOAD ("SYS: SYS; UCINIT")))

(DEFSYSTEM COLOR
  (:PACKAGE COLOR)
  (:COMPILE-LOAD ("SYS: WINDOW; COLOR")))

(DEFSYSTEM ZWEI
  (:PACKAGE ZWEI)
  (:PATHNAME-DEFAULT "SYS: ZWEI;")
  (:MODULE DEFS ("DEFS"				;Structure definitions and declarations.
		 "MACROS"))			;Lisp macros used in the ZWEIs source.
  (:MODULE MAIN ("COMTAB"			;Functions regarding comtabs and command loop.
		 "DISPLA"			;Redisplay, and screen-related functions.
		 "FOR"				;Forward-this, forward-that functions.
		 "INDENT"			;Indention functions
		 "INSERT"			;Insertion and deletion, and related functions
		 "PRIMIT"			;Random primitives and utilities.
		 "FONT"				;Font hacking stuff
		 "KBDMAC"			;Keyboard macro stream
		 "SEARCH"			;Searching functions

		 "COMA"				;Vanilla commands.
		 "COMB"				;More vanilla commands.
		 "COMC"				;Yet more vanilla commands.
		 "COMD"				;Still more vanilla commands.
		 "COME"				;Even more vanilla commands.
		 "COMF"				;More and more vanilla commands
		 "COMG"				;And more vanilla commands
		 "COMS"				;Searching and replacing commands.
		 "DIRED"			;Directory editor.
		 "DOC"				;Self-documentation commands and functions.
		 "FASUPD"			;Update fasl file from core.
		 "FILES"			;File commands and utilities.
		 "LPARSE"			;Parsing lisp code.
		 "MODES"			;Major and minor mode functions and commands
		 "MOUSE"			;Mouse commands less screen interface
		 "PL1MOD"			;PL/I mode commands.
		 "SCREEN"			;Interface to screen system
		 "STREAM"			;Editor stream

		 "SECTIO"			;Some section specific command for ZMACS
		 "ZMACS"			;Multiple-buffer and file commands.
		 "SYS2; PATED"			;Patch commands for ZWEI

		 "ZYMURG"))			;Last file loaded, flavors and initializations
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS)))

(DEFSYSTEM FED
  (:PACKAGE FED)
  (:MODULE DEFS "SYS: IO1; FNTDEF")
  (:MODULE MAIN ("SYS: WINDOW; FED"
		 "SYS: IO1; FNTCNV"))
  (:READFILE DEFS)
  (:COMPILE-LOAD MAIN (:READFILE DEFS)))

(DEFSYSTEM CADR
  (:PACKAGE CADR)
  (:MODULE DEFS ("SYS: SYS; COMPAT"
		 "SYS: UCADR; LQFMAC"
		 "SYS: UCADR; CADLDB"
		 "SYS: UCADR; LCADMC"))
  (:MODULE MAIN ("SYS: CC; CC"
		 "SYS: CC; CCGSYL"
		 "SYS: UCADR; LCADRD"
		 "SYS: UCADR; DIAGS"
		 "SYS: UCADR; DMON"
		 "SYS: UCADR; LDBG"
		 "SYS: CC; ZERO"
		 "SYS: CC; CADLD"
		 "SYS: UCADR; QF"
		 "SYS: UCADR; CCDISK"
		 "SYS: CC; DCHECK"
		 "SYS: UCADR; PACKED"
		 "SYS: UCADR; CHPLOC"
		 "SYS: ZWEI; SALVAG"))
  (:COMPILE-LOAD DEFS)
  (:READFILE ("SYS: CC; CADREG"))
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS)))

(DEFSYSTEM MICRO-ASSEMBLER
  (:PACKAGE MICRO-ASSEMBLER)
  (:NICKNAMES "UA")
  (:MODULE DEFS "SYS: SYS; COMPAT")
  (:MODULE ASS "SYS: SYS; CADRLP")
  (:MODULE MAIN ("SYS: SYS; CDMP"
		 "SYS: UCADR; QWMCR"
		 "SYS: IO; FREAD"
		 "SYS: SYS2; USYMLD"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD ASS (:FASLOAD DEFS))
  (:READFILE ("SYS: COLD; QCOM"
	      "SYS: COLD; DEFMIC"
	      "SYS: SYS; CADSYM")
	     (:FASLOAD ASS))
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS)))

(DEFSYSTEM EH
  (:PACKAGE EH)
  (:PATHNAME-DEFAULT "SYS: WINDOW;")
  (:COMPILE-LOAD ("EH" "EHR" "EHC" "EHW")))

(DEFSYSTEM TV
  (:PACKAGE TV)
  (:PATHNAME-DEFAULT "SYS: WINDOW;")
  (:MODULE DEFS "TVDEFS")
  (:MODULE MAIN ("SCRMAN" "SHEET" "SHWARM" "BASWIN" "WHOLIN"
		 "MOUSE" "BASSTR" "STREAM" "MENU" "COMETH"
		 ;; The above must be loaded before any windows get created
		 "SYSMEN" "SCRED" "TYPWIN" "SCROLL" "TSCROL"
		 "FRAME" "CHOICE" "CSRPOS" "INSPCT"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS)))

(DEFSYSTEM PEEK
  (:PACKAGE TV)
  (:PATHNAME-DEFAULT "SYS: WINDOW;")
  (:MODULE MAIN "PEEK")
#-XEROX
  (:MODULE CHAOS "PEEKCH" :PACKAGE CHAOS)
#-XEROX
  (:MODULE FILE "PEEKFS" :PACKAGE FS)
  (:COMPILE-LOAD MAIN)
#-XEROX
  (:COMPILE-LOAD CHAOS)
#-XEROX
  (:COMPILE-LOAD FILE))

(DEFSYSTEM FILE-SYSTEM
  (:PACKAGE FILE-SYSTEM)
  (:NICKNAMES "FS")
  (:PATHNAME-DEFAULT "SYS: IO;")
  (:COMPILE-LOAD ("PATHNM" #-XEROX "QFILE")))

(DEFSYSTEM MATH
  (:PACKAGE MATH)
  (:COMPILE-LOAD ("SYS: SYS2; MATRIX")))

(DEFSYSTEM HACKS
  (:PACKAGE HACKS)
  (:PATHNAME-DEFAULT "SYS: DEMO;")
  (:MODULE DEFS "HAKDEF")
  (:MODULE MAIN ("ABACUS" "ALARM" "CAFE" "COLXOR" "CROCK" "DC" "DEUTSC" "DLWHAK"
		 "GEB" "HCEDIT" "MUNCH" "OHACKS" "ORGAN" "WORM"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
  (:FASLOAD ("TVBGAR" "WORMCH")))

(DEFSYSTEM SRCCOM
  (:PACKAGE SRCCOM)
  (:COMPILE-LOAD ("SYS: IO1; SRCCOM")))

(DEFSYSTEM METER
  (:PACKAGE METER)
  (:COMPILE-LOAD ("SYS: IO1; METER")))

;;; Systems not initially loaded, but done right afterwards

#+(OR MIT LMI)
(DEFSYSTEM REMOTE-FILE
  (:PATHNAME-DEFAULT "SYS: FILE;")
  (:PACKAGE FILE-SYSTEM)
  (:MODULE DEFS ("RMDEFS" "STREAM"))
  (:MODULE MAIN ("LMPARS" "FILE2;PATHNM" "REMOTE"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS)))

(DEFSYSTEM ZMAIL
  (:NAME "ZMail")
  (:PATHNAME-DEFAULT "SYS: ZMAIL;")
  (:SHORT-NAME "ZM")
  (:PATCHABLE)
  (:NOT-IN-DISK-LABEL)
  (:PACKAGE ZWEI)
  (:MODULE DEFS "DEFS")
  (:MODULE TV ("MULT" "BUTTON") :PACKAGE TV)
  (:MODULE MAIN ("TOP" "MFILES" "COMNDS" "MAIL" "WINDOW" "FILTER" "PROFIL" TV))
  (:MODULE COMETH "COMETH")
  (:MODULE PARSE "PARSE")
  (:MODULE RFC733 "RFC733")
  (:MODULE LEX733 "LEX733")
  (:MODULE FONTS "NARROW")
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN (:FASLOAD DEFS))
  (:COMPILE-LOAD PARSE)
  (:COMPILE-LOAD RFC733 (:FASLOAD PARSE))
; (:RTC-LOAD LEX733)		;Someday
  (:FASLOAD LEX733)
  (:FASLOAD FONTS)
  (:COMPILE-LOAD COMETH))

;;; These are the files in the cold load
(DEFCONST COLD-LOAD-FILE-LIST
	  '("SYS: FONTS; CPTFONT QFASL >"
	    "SYS: SYS; QRAND QFASL >"
	    "SYS: IO; QIO QFASL >"
;	    "SYS: IO; RDTBL QFASL >"	;done specially
	    "SYS: IO; READ QFASL >"
	    "SYS: IO; PRINT QFASL >"
	    "SYS: WINDOW; COLD QFASL >"
	    "SYS: IO; DEBUG QFASL >"
	    "SYS: SYS; SGFCTN QFASL >"
	    "SYS: SYS; QEV QFASL >"
	    "SYS: SYS; LTOP QFASL >"
	    "SYS: SYS; QFASL QFASL >"
	    "SYS: IO; MINI QFASL >"     
;	    "SYS: NETWORK; TFTP-MINI QFASL >"
	    "SYS: SYS; QFCTNS QFASL >"
	    "SYS: SYS2; STRING QFASL >"
	    ))

;;; These variables are looked at by the cold load generator, who takes
;;; the translated pathnames and dumps out prototype values into the new
;;; world with those strings suitable for use with MINI.
;;; They are then used before this file gets loaded.
(DEFCONST MINI-FILE-ALIST-LIST
	  '(LOAD-PACKAGES-FILE-ALIST-1 LOAD-PACKAGES-FILE-ALIST-2 GLOBAL-PACKAGE-FILE-ALIST
	    INNER-SYSTEM-FILE-ALIST
	    #-XEROX CHAOS-FILE-ALIST #+XEROX ETHER-FILE-ALIST
	    SITE-FILE-ALIST HOST-TABLE-FILE-ALIST))

(DEFCONST LOAD-PACKAGES-FILE-ALIST-1
	  '(("SYS: SYS; PACK4 QFASL >" "")))

(DEFCONST LOAD-PACKAGES-FILE-ALIST-2
	  '(("SYS: SYS; PKGDCL LISP >" "")
#+XEROX	    ("SYS: SYS; ETHPKG LISP >" "")
	    ))

(DEFCONST GLOBAL-PACKAGE-FILE-ALIST
	  '(("SYS: COLD; GLOBAL LISP >" "GLOBAL")
	    ("SYS: COLD; SYSTEM LISP >" "SYSTEM")))

(DEFCONST INNER-SYSTEM-FILE-ALIST
	  '(("SYS: SYS; QMISC QFASL >" "SI")
	    ("SYS: SYS; SORT QFASL >" "SI")	;Needed by FLAVOR
	    ("SYS: SYS2; DEFSEL QFASL >" "SI")	;Needed by FQUERY
	    ("SYS: IO; FORMAT QFASL >" "FORMAT")	;ditto
	    ("SYS: IO1; FQUERY QFASL >" "FORMAT")	;Needed by everything in sight
	    ("SYS: SYS2; FLAVOR QFASL >" "SI")	;Needed by PROCES
	    ("SYS: SYS2; PRODEF QFASL >" "SI")	;Definitions for PROCES
	    ("SYS: SYS2; PROCES QFASL >" "SI")
	    ("SYS: WINDOW; EH QFASL >" "EH")
	    ("SYS: WINDOW; EHR QFASL >" "EH")
	    ("SYS: WINDOW; EHC QFASL >" "EH")
	    ("SYS: SYS2; DISASS QFASL >" "COMPILER")	;EH calls subroutines in DISASS
	    ("SYS: IO; DISK QFASL >" "SI")
	    ("SYS: SYS2; LOGIN QFASL >" "SI")	;ditto
	    ("SYS: IO; RDDEFS QFASL >" "SI")	;Load this before trying to read any #\'s
	    ("SYS: SYS2; HOST QFASL >" "SI")
	    ("SYS: SYS2; HASH QFASL >" "SI")	;Needed by PATHNM
	    ("SYS: IO; STREAM QFASL >" "SI")	;Probably needed by any file system
	    ;; PATHNM must be the last file in this list.  It breaks things while cold loading
	    ;; that QLD knows how to fix after this alist is loaded.
	    ("SYS: IO; PATHNM QFASL >" "FS")
	    ))

(DEFCONST CHAOS-FILE-ALIST
	  '(("SYS: IO; CHSNCP QFASL >" "CHAOS")
	    ("SYS: IO; CHSAUX QFASL >" "CHAOS")
	    ("SYS: IO; QFILE QFASL >" "FS")
	    ))

(DEFVAR ETHER-FILE-ALIST)

(DEFCONST SITE-FILE-ALIST
	  '(("SYS: SITE; SITE QFASL >" "SI")
	    ))

(DEFCONST HOST-TABLE-FILE-ALIST
	  '(
	    ("SYS: SITE; HSTTBL QFASL >" "CHAOS")
	    ("SYS: SITE; LMLOCS QFASL >" "SI")
	    ))

(DEFUN RECOMPILE-WORLD (&REST KEYWORDS)
  (LEXPR-FUNCALL #'MAKE-SYSTEM 'SYSTEM ':COMPILE ':NOLOAD KEYWORDS))

(DEFUN LIST-OBSOLETE-FILES (&OPTIONAL (SYSTEM 'SYSTEM))
  (MAKE-SYSTEM SYSTEM ':PRINT-ONLY))
