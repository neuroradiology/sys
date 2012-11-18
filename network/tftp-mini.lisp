;;; -*- Mode: Lisp; Package: System-Internals; BASE: 8; Cold-load: T -*-

;;; Implementation of TFTP protocol suitable for use in a cold load.
;;; 
;;; It also knows the format of a packet non-symbolically.

(DECLARE (SPECIAL MINI-RECV-PKT MINI-PKT MINI-FILE-ID MINI-OPEN-P MINI-CH-IDX
		  MINI-UNRCHF MINI-PKT-NUMBER MINI-EOF-SEEN MINI-STARTED
		  MINI-LOCAL-PORT MINI-LOCAL-HOST
		  MINI-REMOTE-PORT MINI-REMOTE-HOST
		  MINI-REMOTE-ADDRESS MINI-LOCAL-ADDRESS
		  MINI-PKT-SIZE MINI-PLIST-RECEIVER-POINTER))

;This is the filename (a string) on which MINI-FASLOAD was called.
(DEFVAR MINI-FASLOAD-FILENAME)

(SETQ MINI-REMOTE-HOST "ren")
(SETQ MINI-REMOTE-ADDRESS 30052000006)		; REN
(SETQ MINI-LOCAL-ADDRESS 30052000030)		; CADR-1
(DEFVAR MINI-HIS-ETHERNET-ADDRESS (MAKE-ARRAY 6 ':TYPE ':ART-8B))
(DEFVAR MINI-MY-ETHERNET-ADDRESS (MAKE-ARRAY 6 ':TYPE ':ART-8B))

(defconst +ether-regs-base+ 376000)
(defconst +ether-descriptors-base+ 376400)

;;; Initialization, usually only called once.
(DEFUN MINI-INIT ()
  ;; Init lists microcode looks at
;  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-FREE-LIST) NIL)
;  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-RECEIVE-LIST) NIL)
;  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-TRANSMIT-LIST) NIL)
  ;; Fake up two packet buffers for the microcode, locations 1200-x through 1377
  ;; I.e. in the unused portions of SCRATCH-PAD-INIT-AREA
  (%P-STORE-TAG-AND-POINTER 1150 DTP-ARRAY-HEADER
			    (DPB 1 %%ARRAY-NUMBER-DIMENSIONS
				 (DPB 1056 %%ARRAY-INDEX-LENGTH-IF-SHORT
				      (DPB 1 %%ARRAY-LEADER-BIT
					   ART-8B))))
  (%P-STORE-TAG-AND-POINTER 1147 DTP-FIX #.(LENGTH ETHER-BUFFER-LEADER-QS))
  (%P-STORE-TAG-AND-POINTER (- 1147 1 #.(LENGTH ETHER-BUFFER-LEADER-QS))
			    DTP-HEADER
			    (DPB %HEADER-TYPE-ARRAY-LEADER %%HEADER-TYPE-FIELD
				 (+ 2 #.(LENGTH ETHER-BUFFER-LEADER-QS))))
  (SETQ MINI-RECV-PKT (%MAKE-POINTER DTP-ARRAY-POINTER 1150))
  ;; Need to fake up MINI-PKT too.
  (SETQ MINI-PKT (MAKE-ARRAY 200 ':TYPE ':ART-8B))
  (OR (BOUNDP 'MINI-LOCAL-PORT)
      (SETQ MINI-LOCAL-PORT 1024.))
  (SETQ MINI-OPEN-P NIL)
  (SETQ MINI-STARTED NIL)
  (ether-init))

;;; Get a connection to a file server
(DEFUN MINI-OPEN-CONNECTION (HOST PORT)
  (OR (BOUNDP 'MINI-PKT) (MINI-INIT))
  (SETQ MINI-LOCAL-PORT (1+ MINI-LOCAL-PORT))
  (AND (= MINI-LOCAL-PORT 200000) (SETQ MINI-LOCAL-PORT 1024.))
  (SETQ MINI-REMOTE-PORT PORT)
;  (get-his-ethernet-address)
  )

;;; Send an ACK
(DEFUN MINI-SEND-ACK ()
  ;(FORMAT DEBUG-STREAM "MINI-SEND-ACK: ~D~%" MINI-PKT-NUMBER)
  (ASET 0 MINI-PKT 52)
  (ASET 4 MINI-PKT 53)			; ACK

  (ASET (LDB 1010 MINI-PKT-NUMBER) MINI-PKT 54)
  (ASET (LDB 0010 MINI-PKT-NUMBER) MINI-PKT 55)

  (MINI-SEND-PKT 4)
  (SETQ MINI-PKT-NUMBER (1+ MINI-PKT-NUMBER)))

;;; Open a file for read
(DEFUN MINI-OPEN-FILE (FILENAME BINARY-P &AUX LEN MODE MODELEN)
  (SETQ MINI-UNRCHF NIL MINI-EOF-SEEN NIL MINI-PKT-NUMBER 1)
  (OR MINI-OPEN-P
      (MINI-OPEN-CONNECTION MINI-REMOTE-HOST 69.)) 
  (SETQ LEN (ARRAY-ACTIVE-LENGTH FILENAME))
  (SETQ MODE (IF BINARY-P "octet" "netascii"))
  (SETQ MODELEN (ARRAY-ACTIVE-LENGTH MODE))

  (ASET 0 MINI-PKT 52)
  (ASET 1 MINI-PKT 53)			; RRQ
  (COPY-ARRAY-PORTION (STRING-DOWNCASE FILENAME) 0 LEN MINI-PKT 54 (+ 54 LEN))
  (ASET 0 MINI-PKT (+ 54 LEN))
  (COPY-ARRAY-PORTION MODE 0 MODELEN MINI-PKT (+ 55 LEN) (+ 55 LEN MODELEN))
  (ASET 0 MINI-PKT (+ 55 LEN MODELEN))

  (MINI-SEND-PKT (+ LEN MODELEN 4))

  (OR MINI-STARTED
      (ether-start))

  ;; Before pathnames and time parsing is loaded, things are stored as strings.
  (SETQ MINI-FILE-ID (CONS FILENAME "11//06//12 17:30:00"))
  (MINI-NEXT-PKT T)
  (IF BINARY-P #'MINI-BINARY-STREAM #'MINI-ASCII-STREAM))

;; Doesn't use symbols for packet fields since not loaded yet
;; This sends a packet and doesn't return until it has cleared microcode.
;; You fill in the data part before calling, this fills in the header.
(DEFUN MINI-SEND-PKT (N-BYTES &AUX CHECKSUM)
  ;; UDP
  (ASET (LDB 1010 MINI-LOCAL-PORT) MINI-PKT 42)
  (ASET (LDB 0010 MINI-LOCAL-PORT) MINI-PKT 43)
  (ASET (LDB 1010 MINI-REMOTE-PORT) MINI-PKT 44)
  (ASET (LDB 0010 MINI-REMOTE-PORT) MINI-PKT 45)
  (ASET 0 MINI-PKT 46)
  (ASET (+ N-BYTES 10) MINI-PKT 47)
  (ASET 0 MINI-PKT 50)
  (ASET 0 MINI-PKT 51)

  ;; IP
  (ASET 105 MINI-PKT 16)		;v4 + header length
  (ASET 0 MINI-PKT 17)			; TOS
  (ASET 0 MINI-PKT 20)
  (ASET (+ N-BYTES 34) MINI-PKT 21)
  (ASET 0 MINI-PKT 22)
  (ASET 0 MINI-PKT 23)
  (ASET 0 MINI-PKT 24)
  (ASET 0 MINI-PKT 25)
  (ASET 100 MINI-PKT 26)		; TTL
  (ASET 21 MINI-PKT 27)			; UDP
  (ASET 0 MINI-PKT 30)
  (ASET 0 MINI-PKT 31)

  (ASET (LDB 3010 MINI-LOCAL-ADDRESS) MINI-PKT 32)
  (ASET (LDB 2010 MINI-LOCAL-ADDRESS) MINI-PKT 33)
  (ASET (LDB 1010 MINI-LOCAL-ADDRESS) MINI-PKT 34)
  (ASET (LDB 0010 MINI-LOCAL-ADDRESS) MINI-PKT 35)

  (ASET (LDB 3010 MINI-REMOTE-ADDRESS) MINI-PKT 36)
  (ASET (LDB 2010 MINI-REMOTE-ADDRESS) MINI-PKT 37)
  (ASET (LDB 1010 MINI-REMOTE-ADDRESS) MINI-PKT 40)
  (ASET (LDB 0010 MINI-REMOTE-ADDRESS) MINI-PKT 41)

  (SETQ CHECKSUM (LOGXOR 177777 (%IP-CHECKSUM MINI-PKT 0 16 24 NIL)))
  (ASET (LDB 1010 CHECKSUM) MINI-PKT 30)
  (ASET (LDB 0010 CHECKSUM) MINI-PKT 31)

  ;; Ethernet
  (COPY-ARRAY-PORTION MINI-HIS-ETHERNET-ADDRESS 0 6 MINI-PKT 0 6)
  (COPY-ARRAY-PORTION MINI-MY-ETHERNET-ADDRESS 0 6 MINI-PKT 6 14)
  (ASET 10 MINI-PKT 14)
  (ASET 0 MINI-PKT 15)

;  (STORE-ARRAY-LEADER NIL MINI-PKT %ETHER-LEADER-THREAD)
;  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-TRANSMIT-LIST) MINI-PKT)
;  (%ETHER-WAKEUP)
;  (DO ()	;Await completion of transmission
;      ((NULL (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-TRANSMIT-LIST))))
  ;; Disallow use of the packet by the receive side, flush any received packet that snuck in
;  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-FREE-LIST) NIL)
;  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-RECEIVE-LIST) NIL)

  (MINI-ETHER-SEND MINI-PKT (+ N-BYTES 52)))

;; Return opcode of next packet other than those that are no good.
;; If the arg is NIL, can return NIL if no packet arrives after a while.
;; If T, waits forever.  Return value is the opcode of the packet in MINI-PKT.
(DEFUN MINI-NEXT-PKT (MUST-RETURN-A-PACKET)
  (PROG (IRQ)
     TOP
    ;; Enable receive
    (%xbus-write (+ +ether-descriptors-base+ 200)
		 #.(logior (dpb 1 %%ether-desc-rx-empty 0)
			 (dpb 1 %%ether-desc-rx-wrap 0)
			 (dpb 1 %%ether-desc-rx-irq 0)))

    ;; Enable microcode to receive a packet
;    (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-FREE-LIST) NIL)
;    (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-RECEIVE-LIST) NIL)
;    (STORE-ARRAY-LEADER NIL MINI-PKT %ETHER-LEADER-THREAD)
;    (COPY-ARRAY-CONTENTS "" MINI-PKT)		;Fill with zero
;    (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-FREE-LIST) MINI-PKT)
;    (%ETHER-WAKEUP)
;    (DO ((N 2000. (1- N)))	;Give it time
;	((OR (ZEROP N) (SYSTEM-COMMUNICATION-AREA %SYS-COM-ETHER-RECEIVE-LIST))))

    MID
    (SETQ IRQ (%xbus-read (+ +ether-regs-base+ #.sys:%ether-int-source-offset)))
    (WHEN (ZEROP (LDB #.%%ether-int-rxb irq)) (GO MID))
    (%xbus-write (+ +ether-regs-base+ #.sys:%ether-int-source-offset) irq)

    (SETQ MINI-PKT-SIZE (LDB #.%%ether-desc-length (%XBUS-READ (+ +ETHER-DESCRIPTORS-BASE+ 200))))
;   (PRINT "MINI-NEXT-PKT: read " DEBUG-STREAM)
;   (PRINC MINI-PKT-SIZE DEBUG-STREAM)
    (SELECT (DPB (AREF MINI-RECV-PKT 14) 1010 (AREF MINI-RECV-PKT 15))
      (#x800				; IPv4
;      (FORMAT DEBUG-STREAM "MINI-NEXT-PKT: IPv4 ~D~%" (AREF MINI-RECV-PKT 27))
       (COND ((AND (= (AREF MINI-RECV-PKT 27) 21) ; UDP
		   (= (DPB (AREF MINI-RECV-PKT 44) 1010 (AREF MINI-RECV-PKT 45))
		      MINI-LOCAL-PORT)
		   (= (AREF MINI-RECV-PKT 53) 3)	; DATA
		   (= (DPB (AREF MINI-RECV-PKT 54) 1010 (AREF MINI-RECV-PKT 55))
		      MINI-PKT-NUMBER))
	      (WHEN (= MINI-PKT-NUMBER 1)
		    (SETQ MINI-REMOTE-PORT
			  (DPB (AREF MINI-RECV-PKT 42) 1010 (AREF MINI-RECV-PKT 43))))
	      (SETQ MINI-CH-IDX 56)
;	      (PRINT "MINI-NEXT-PKT: RETURNING" DEBUG-STREAM)
	      (RETURN T)))
       T)
      (#x806				; ARP
;      (FORMAT DEBUG-STREAM "MINI-NEXT-PKT: ARP~%")
       T))

    (GO TOP)))


;Stream which does only 16-bit TYI
(DEFUN MINI-BINARY-STREAM (OP &OPTIONAL ARG1)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:TYI))
    (:TYI (COND (MINI-UNRCHF
		 (PROG1 MINI-UNRCHF (SETQ MINI-UNRCHF NIL)))
		((< MINI-CH-IDX MINI-PKT-SIZE)
		 (PROG1 (DPB (AREF MINI-RECV-PKT (1+ MINI-CH-IDX))
			     1010 (AREF MINI-RECV-PKT MINI-CH-IDX))
			(SETQ MINI-CH-IDX (+ MINI-CH-IDX 2))))
		((< MINI-PKT-SIZE 1056)
		 (MINI-SEND-ACK)
		 (SETQ MINI-EOF-SEEN T)
		 (AND ARG1 (ERROR ARG1))
		 NIL)		;and tell caller
		(T ;Get another packet
		 (MINI-SEND-ACK)  ;Acknowledge packet just processed
		 (MINI-NEXT-PKT T)
		 (MINI-BINARY-STREAM ':TYI))))
    (:UNTYI (SETQ MINI-UNRCHF ARG1))
;   (:PATHNAME MINI-FASLOAD-FILENAME)
;   (:GENERIC-PATHNAME 'MINI-PLIST-RECEIVER)
;   (:INFO MINI-FILE-ID)
;   (:CLOSE (DO () (MINI-EOF-SEEN) (MINI-BINARY-STREAM ':TYI)))
    (OTHERWISE (MINI-BARF "Unknown stream operation" OP))))

(DEFUN MINI-ASCII-STREAM (OP &OPTIONAL ARG1)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:TYI :UNTYI))
    (:TYI (COND (MINI-UNRCHF
		 (PROG1 MINI-UNRCHF (SETQ MINI-UNRCHF NIL)))
		((< MINI-CH-IDX MINI-PKT-SIZE)
		 (LET ((CH (AREF MINI-RECV-PKT MINI-CH-IDX)))
		   (SETQ MINI-CH-IDX (1+ MINI-CH-IDX))
		   (SELECT CH
		     (10 #\BS)
		     (11 #\TAB)
		     (12 #\LINE)
		     (14 #\FF)
		     (15 #\NEWLINE)
		     (177 #\RUBOUT)
		     (OTHERWISE CH))))
		((< MINI-PKT-SIZE 1056)
		 (MINI-SEND-ACK)
		 (SETQ MINI-EOF-SEEN T)
		 (AND ARG1 (ERROR ARG1))
		 NIL)		;and tell caller
		(T ;Get another packet
		 (MINI-SEND-ACK)  ;Acknowledge packet just processed
		 (MINI-NEXT-PKT T)
		 (MINI-ASCII-STREAM ':TYI))))
    (:UNTYI (SETQ MINI-UNRCHF ARG1))
;   (:PATHNAME MINI-FASLOAD-FILENAME)
;   (:GENERIC-PATHNAME 'MINI-PLIST-RECEIVER)
;    (:INFO MINI-FILE-ID)
;   (:CLOSE (DO () (MINI-EOF-SEEN) (MINI-ASCII-STREAM ':TYI)))
    (OTHERWISE (MINI-BARF "Unknown stream operation" OP))))

(DEFUN MINI-BARF (&REST ARGS)
  (SETQ MINI-OPEN-P NIL) ;Force re-open of connection
  ;; If inside the cold load, this will be FERROR-COLD-LOAD, else make debugging easier
  (LEXPR-FUNCALL #'FERROR 'MINI-BARF ARGS))

;;; Higher-level stuff

;;; Load a file alist as setup by the cold load generator
(DEFUN MINI-LOAD-FILE-ALIST (ALIST)
  (LOOP FOR (FILE PACK QFASLP) IN ALIST
	DO (PRINT FILE)
	DO (FUNCALL (IF QFASLP #'MINI-FASLOAD #'MINI-READFILE) FILE PACK)))

(DECLARE (SPECIAL FASL-STREAM FASLOAD-FILE-PROPERTY-LIST-FLAG FASL-GROUP-DISPATCH
                  FASL-OPS FDEFINE-FILE-PATHNAME FASL-GENERIC-PATHNAME-PLIST
		  FASL-STREAM-BYPASS-P))

(DECLARE (SPECIAL ACCUMULATE-FASL-FORMS))

(DECLARE (SPECIAL *COLD-LOADED-FILE-PROPERTY-LISTS*))

;This kludge simulates the behavior of PROPERTY-LIST-MIXIN.
;It is used instead of the generic-pathname in fasloading and readfiling;
;it handles the same messages that generic-pathnames are typically sent.
(DEFUN MINI-PLIST-RECEIVER (OP &REST ARGS)
  (SELECTQ OP
    (:GET (GET MINI-PLIST-RECEIVER-POINTER (CAR ARGS)))
    (:GETL (GETL MINI-PLIST-RECEIVER-POINTER (CAR ARGS)))
    (:PUTPROP (PUTPROP MINI-PLIST-RECEIVER-POINTER (CAR ARGS) (CADR ARGS)))
    (:REMPROP (REMPROP MINI-PLIST-RECEIVER-POINTER (CAR ARGS)))
    (:PLIST (CAR MINI-PLIST-RECEIVER-POINTER))
    (:PUSH-PROPERTY (PUSH (CAR ARGS) (GET MINI-PLIST-RECEIVER-POINTER (CADR ARGS))))
    (OTHERWISE
     (PRINT "Bad op to MINI-PLIST-RECEIVER ")
     (PRINT OP)
     (%HALT))))

(DEFUN MINI-FASLOAD (FILE-NAME PKG
		     &AUX FASL-STREAM W1 W2 TEM TEM1
			  (FDEFINE-FILE-PATHNAME FILE-NAME) FASL-GENERIC-PATHNAME-PLIST
			  FASLOAD-FILE-PROPERTY-LIST-FLAG
			  (FASL-TABLE NIL) (FASL-STREAM-BYPASS-P NIL))

  ;; Set it up so that file properties get remembered for when there are pathnames
  (OR (SETQ TEM (ASSOC FILE-NAME *COLD-LOADED-FILE-PROPERTY-LISTS*))
      (PUSH (SETQ TEM (NCONS FILE-NAME)) *COLD-LOADED-FILE-PROPERTY-LISTS*))

  (SETQ FASL-GENERIC-PATHNAME-PLIST (LOCF TEM1))
  (SETQ MINI-PLIST-RECEIVER-POINTER TEM)
  
  (FASL-START)
  
  ;;Open the input stream in binary mode, and start by making sure
  ;;the file type in the first word is really SIXBIT/QFASL/.
  (SETQ FASL-STREAM (MINI-OPEN-FILE FILE-NAME T))
  (SETQ W1 (FUNCALL FASL-STREAM ':TYI)
	W2 (FUNCALL FASL-STREAM ':TYI))
  (COND ((AND (= W1 143150) (= W2 71660))	;If magic ID checks,
	 (LET ((PACKAGE (IF (FBOUNDP 'INTERN-LOCAL)	;If packages exist now 
			    (PKG-FIND-PACKAGE PKG)
			    NIL)))
	   ;; Read in the file property list in the wrong package list fasload does
	   (AND PACKAGE
		(= (LOGAND (FASL-NIBBLE-PEEK) %FASL-GROUP-TYPE) FASL-OP-FILE-PROPERTY-LIST)
		(FASL-FILE-PROPERTY-LIST))
	   ;; Call fasload to load it
	   (FASL-TOP-LEVEL)			;load it.
	   ;; Doesn't really read to EOF, must read rest to avoid getting out of phase
	   (DO () (MINI-EOF-SEEN)
	     (FUNCALL FASL-STREAM ':TYI))
	   ;; If package is NIL, will be fixed later
	   (SET-FILE-LOADED-ID 'MINI-PLIST-RECEIVER MINI-FILE-ID PACKAGE)))
	((FERROR NIL "~A is not a QFASL file" FILE-NAME)))	;Otherwise, barf out.
  FILE-NAME)

(DEFUN MINI-READFILE (FILE-NAME PKG &AUX (FDEFINE-FILE-PATHNAME FILE-NAME) TEM)
  (LET ((EOF '(()))
	(STANDARD-INPUT (MINI-OPEN-FILE FILE-NAME NIL))
	(PACKAGE (PKG-FIND-PACKAGE PKG)))
    (DO FORM (READ STANDARD-INPUT EOF) (READ STANDARD-INPUT EOF) (EQ FORM EOF)
	(EVAL FORM))
    (OR (SETQ TEM (ASSOC FILE-NAME *COLD-LOADED-FILE-PROPERTY-LISTS*))
	(PUSH (SETQ TEM (NCONS FILE-NAME)) *COLD-LOADED-FILE-PROPERTY-LISTS*))
    (SETQ MINI-PLIST-RECEIVER-POINTER TEM)
    (SET-FILE-LOADED-ID 'MINI-PLIST-RECEIVER MINI-FILE-ID PACKAGE)))

(DEFUN MINI-BOOT ()
  (SETQ MINI-OPEN-P NIL)
  (SETQ MINI-STARTED NIL)
  (MAKUNBOUND 'MINI-PKT)
  (MAKUNBOUND 'MINI-RECV-PKT))

(ADD-INITIALIZATION "MINI" '(MINI-BOOT) '(WARM FIRST))

;;; Ethernet device support


;; Send a packet
;;
;; Just using the first descriptor for now.

(defun mini-ether-send (pkt len &aux status)

  (setq status (logior (dpb 1 #.%%ether-desc-tx-ready 0)
		       (dpb 1 #.%%ether-desc-tx-wrap 0)
		       (dpb 1 #.%%ether-desc-tx-pad 0)
		       (dpb 1 #.%%ether-desc-tx-crc 0)
		       (dpb len #.%%ether-desc-length 0)))
  (%xbus-write (+ +ether-descriptors-base+ 1)
	       (%physical-address (+ (%pointer pkt) 1
				     (%p-ldb #.%%array-long-length-flag pkt))))
  (%xbus-write +ether-descriptors-base+ status))

(defun ether-start (&aux mode)
  (setq mode (logior (dpb 1 #.%%ether-mode-crc-enable 0)
		     (dpb 1 #.%%ether-mode-tx-enable 0)
		     (dpb 1 #.%%ether-mode-rx-enable 0)))
  (%xbus-write (+ +ether-regs-base+ #.sys:%ether-mode-offset) mode)
  (setq mini-started t))

(defun ether-init (&aux val)
  ;; Should pick this up from an EEPROM
  (aset #x00 MINI-MY-ETHERNET-ADDRESS 0)
  (aset #x01 MINI-MY-ETHERNET-ADDRESS 1)
  (aset #x02 MINI-MY-ETHERNET-ADDRESS 2)
  (aset #x03 MINI-MY-ETHERNET-ADDRESS 3)
  (aset #x04 MINI-MY-ETHERNET-ADDRESS 4)
  (aset #x06 MINI-MY-ETHERNET-ADDRESS 5)

  (setq val (aref MINI-MY-ETHERNET-ADDRESS 5))
  (setq val (dpb (aref MINI-MY-ETHERNET-ADDRESS 4) 1010 val))
  (setq val (dpb (aref MINI-MY-ETHERNET-ADDRESS 3) 2010 val))
  (setq val (dpb (aref MINI-MY-ETHERNET-ADDRESS 2) 3010 val))
  (%xbus-write (+ +ether-regs-base+ #.sys:%ether-mac-address0-offset) val)
  (setq val (aref MINI-MY-ETHERNET-ADDRESS 1))
  (setq val (dpb (aref MINI-MY-ETHERNET-ADDRESS 0) 1010 val))
  (%xbus-write (+ +ether-regs-base+ #.sys:%ether-mac-address1-offset) val)

  ;(fillarray MINI-BROADCAST-ADDRESS '(377 377 377 377 377 377))
  ;(fillarray MINI-HIS-ETHERNET-ADDRESS '(#x00 33 #x21 #x34 #x55 #x62))
  (aset #x00 MINI-HIS-ETHERNET-ADDRESS 0)
  (aset 33 MINI-HIS-ETHERNET-ADDRESS 1)
  (aset #x21 MINI-HIS-ETHERNET-ADDRESS 2)
  (aset #x34 MINI-HIS-ETHERNET-ADDRESS 3)
  (aset #x55 MINI-HIS-ETHERNET-ADDRESS 4)
  (aset #x62 MINI-HIS-ETHERNET-ADDRESS 5)

  (%xbus-write (+ +ether-descriptors-base+ 201)
	       (%physical-address (+ (%pointer MINI-RECV-PKT) 1
				     (%p-ldb #.%%array-long-length-flag MINI-RECV-PKT)))))


;(defun ether-pkt-available ()
;  (plusp (ldb %%ether-int-rxb
;	      (%xbus-read (+ +ether-regs-base+ sys:%ether-int-source-offset)))))

(defun %ip-checksum (array sum start count odd-flag)
  (let ((high-sum (logand 377 (ash sum -8)))
        (low-sum (logand 377 sum))
        (offset start)
	(end (+ start count)))
    (unless (zerop count)
      (prog ()
          (when odd-flag
            (go get-low-byte))
       top
          (incf high-sum (aref array offset))
          (incf offset)
       get-low-byte
          (unless (= offset end)
            (incf low-sum (aref array offset))
            (incf offset)
            (unless (= offset end)
              (go top)))))
    (incf high-sum (ash low-sum -8))
    (setq sum (dpb high-sum 1027 low-sum))
    (+ (ldb 0020 sum) (ldb 2020 sum))))


