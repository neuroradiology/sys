;;; -*- Mode:Lisp; Package:System-internals; Base:8 -*-
;;; ** (C) Copyright 1980, Massachusetts Institute of Technology
;;;    Enhancements (C) Copyright 1981, Symbolics, Inc.
;;; The Massachusetts Institute of Technology has acquired the rights from Symbolics
;;; to include the Software covered by the foregoing notice of copyright with its
;;; licenses of the Lisp Machine System **

;;; This file contains functions for interrupt-driven input from
;;; simple Unibus devices.  Note that the keyboard uses the same
;;; mechanism, but does not use these functions, so that this file
;;; need not be in the cold load and to avoid the expense of an
;;; extra wired page all the time for the keyboard buffer.

;;; SI:GET-UNIBUS-CHANNEL interrupt-vector CSR-address CSR-bits data-address n-data-words
;;;	&optional output-turnoff-unibus-address output-turnoff-data
;;;	n-data-words can be 1 or 2 (the number of 16-bit words at the data-address)
;;;	The two optional arguments are for output channels; they are the
;;;	arguments to a %UNIBUS-WRITE to be issued if the device interrupts and
;;;	the channel buffer is empty.
;;; SI:RETURN-UNIBUS-CHANNEL chan
;;; SI:READ-UNIBUS-CHANNEL chan -> returns 2 values in case n-data-words was 2
;;; SI:UNIBUS-CHANNEL-NOT-EMPTY chan -> T or NIL

;;; The UNIBUS-CHANNEL-QS in QCOM are known about by the microcode.
;;; They are offsets within a wired Unibus-channel data structure.
;;; This code stores such inside wired-down 1-page-long ART-32B arrays.
;;; These indices start at 1 to allow for the presence of an array header.

(DEFRESOURCE UNIBUS-CHANNEL ()
  :CONSTRUCTOR (MAKE-ARRAY (1- PAGE-SIZE) ':TYPE 'ART-32B ':AREA DISK-BUFFER-AREA))

(DEFUN GET-UNIBUS-CHANNEL (INTERRUPT-VECTOR CSR-ADDRESS CSR-BITS DATA-ADDRESS N-DATA-WORDS
			   &OPTIONAL OUTPUT-TURNOFF-UNIBUS-ADDRESS OUTPUT-TURNOFF-DATA
			   &AUX CHAN)
  (CHECK-ARG N-DATA-WORDS (OR (= N-DATA-WORDS 1) (= N-DATA-WORDS 2)) "1 or 2")
  (SETQ CHAN (ALLOCATE-RESOURCE 'UNIBUS-CHANNEL))
  (LOOP FOR I FROM 1 BELOW PAGE-SIZE		;Zero out the array, including tag bits
	DO (%P-DPB-OFFSET 0 %%Q-LOW-HALF CHAN I)
	   (%P-DPB-OFFSET 0 %%Q-HIGH-HALF CHAN I))
  (%P-DPB-OFFSET INTERRUPT-VECTOR %%Q-POINTER CHAN %UNIBUS-CHANNEL-VECTOR-ADDRESS)
  (%P-DPB-OFFSET (VIRTUAL-UNIBUS-ADDRESS CSR-ADDRESS) %%Q-POINTER
		 CHAN %UNIBUS-CHANNEL-CSR-ADDRESS)
  (%P-DPB-OFFSET CSR-BITS %%Q-POINTER CHAN %UNIBUS-CHANNEL-CSR-BITS)
  (%P-DPB-OFFSET (VIRTUAL-UNIBUS-ADDRESS DATA-ADDRESS) %%Q-POINTER
		 CHAN %UNIBUS-CHANNEL-DATA-ADDRESS)
  (AND (= N-DATA-WORDS 2)
       (%P-DPB-OFFSET 1 %%UNIBUS-CSR-TWO-DATA-REGISTERS CHAN %UNIBUS-CHANNEL-DATA-ADDRESS))
  (COND (OUTPUT-TURNOFF-UNIBUS-ADDRESS
	 (%P-DPB-OFFSET (VIRTUAL-UNIBUS-ADDRESS OUTPUT-TURNOFF-UNIBUS-ADDRESS) %%Q-POINTER
			CHAN %UNIBUS-CHANNEL-OUTPUT-TURNOFF-ADDRESS)
	 (%P-DPB-OFFSET OUTPUT-TURNOFF-DATA %%Q-POINTER
			CHAN %UNIBUS-CHANNEL-OUTPUT-TURNOFF-BITS)
	 (%P-DPB-OFFSET 1 2001 CHAN %UNIBUS-CHANNEL-CSR-BITS)	;Output flag
	 ;; Temporary error check, remove after system 79
	 (OR ( %MICROCODE-VERSION-NUMBER 840.)
	     (FERROR NIL "You need microcode 840 or later to use Unibus output channels"))))
  (LET ((BUFFER-START (+ (%POINTER CHAN) 20)) ;leave room for expansion
	(BUFFER-END (+ (%POINTER CHAN) PAGE-SIZE)))
    (%P-DPB-OFFSET BUFFER-START %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START)
    (%P-DPB-OFFSET BUFFER-END %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END)
    (%P-DPB-OFFSET BUFFER-START %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR)
    (%P-DPB-OFFSET BUFFER-START %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
  (WIRE-PAGE CHAN)
  (WITHOUT-INTERRUPTS
       (%P-DPB-OFFSET (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST)
		      %%Q-POINTER CHAN %UNIBUS-CHANNEL-LINK)
       (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST) CHAN))
  CHAN)

(DEFUN RETURN-UNIBUS-CHANNEL (CHAN)
  (AND CHAN
       (WITHOUT-INTERRUPTS
	    (DO ((X (%POINTER (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST))
		    (%P-LDB %%Q-POINTER (+ X %UNIBUS-CHANNEL-LINK)))
		 (P NIL X))
		((ZEROP X))
	      (COND ((= (%POINTER CHAN) X)
		     (COND ((NULL P)
			    (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST)
				   (%P-LDB %%Q-POINTER (+ X %UNIBUS-CHANNEL-LINK))))
			   ((%P-DPB (%P-LDB %%Q-POINTER (+ X %UNIBUS-CHANNEL-LINK))
				    %%Q-POINTER (+ P %UNIBUS-CHANNEL-LINK))))
		     (RETURN NIL))))
	    (UNWIRE-PAGE CHAN)
	    (DEALLOCATE-RESOURCE 'UNIBUS-CHANNEL CHAN)
	    NIL)))

(DEFUN UNIBUS-CHANNEL-NOT-EMPTY (CHAN)
  (CHECK-ARG CHAN ARRAYP "an array (a Unibus channel)")
  (NEQ (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR)
       (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR)))

;;; Get next word (pair) from an input channel, or NIL if empty
(DEFUN READ-UNIBUS-CHANNEL (CHAN)
  (DECLARE (RETURN-LIST FIRST-WORD SECOND-WORD))
  (AND (UNIBUS-CHANNEL-NOT-EMPTY CHAN)
       (LET* ((OUT-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
	      (VAL1 (%P-LDB 0020 OUT-PTR))
	      (VAL2 (%P-LDB 2020 OUT-PTR)))
	 (AND (= (SETQ OUT-PTR (1+ OUT-PTR))
		 (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END))
	      (SETQ OUT-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START)))
	 (%P-DPB-OFFSET OUT-PTR %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR)
	 (VALUES VAL1 VAL2))))

(DEFUN UNIBUS-CHANNEL-NOT-FULL (CHAN &OPTIONAL BUFFER-SIZE-LIMIT)
  (CHECK-ARG CHAN ARRAYP "an array (a Unibus channel)")
  (LET ((BUFFER-SIZE (- (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END)
			(%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START))))
    (< (\ (+ (- (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR)
		(%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
	     BUFFER-SIZE)
	  BUFFER-SIZE)
       (OR BUFFER-SIZE-LIMIT (1- BUFFER-SIZE)))))

;;; For an output channel, returns an empty contiguous range of the buffer
;;; Note that we must arrange for at least one free slot at all times, because
;;; a completely full buffer looks empty
(DEFUN UNIBUS-CHANNEL-SPACE-AVAILABLE (CHAN)
  (DECLARE (RETURN-LIST START-INDEX END-INDEX))
  (CHECK-ARG CHAN ARRAYP "an array (a Unibus channel)")
  (LET ((IN-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR))
	(OUT-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
	(START (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START))
	(END (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END)))	
    (IF (> OUT-PTR IN-PTR)
	(SETQ END (1- OUT-PTR))
	(IF (= OUT-PTR START)
	    (SETQ END (1- END))))
    (VALUES (- IN-PTR (%POINTER CHAN) 1) (- END (%POINTER CHAN) 1))))

;;; Advance input pointer for unibus channel
(DEFUN UNIBUS-CHANNEL-ADVANCE (CHAN NEW-INDEX)
  (CHECK-ARG CHAN ARRAYP "an array (a Unibus channel)")
  (LET* ((START (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START))
	 (END (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END))
	 (IN-PTR (+ NEW-INDEX (%POINTER CHAN) 1)))
    (OR ( START IN-PTR END) (FERROR NIL "Index lies outside of buffer"))
    (IF (= IN-PTR END) (SETQ IN-PTR START))
    (%P-DPB-OFFSET IN-PTR %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR)))
