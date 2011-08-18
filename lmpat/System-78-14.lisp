;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.14
;;; Reason: Install revised, faster RS232 serial I/O stream
;;; Written 12/12/81 23:19:54 by Moon,
;;; while running on Spaniel from band 2
;;; with System 78.9, ZMail 38.1, Experimental Tape 6.0, Experimental LMFS 20.9, Experimental Symbolics 8.3, Experimental Canon 9.0, microcode 840.


;--------------------------------------------------
; To system maintainers:
; This patch file requires microcode 840 and a one-wire ECO to the
; I/O board (wire from A12-8 to C20-1).  Without this ECO the machine
; will fail to do serial output after installing this patch.  With this
; ECO and without this patch, the machine will loop in the microcode if
; you try to use the serial I/O port.
;--------------------------------------------------

; From file PATCH.LISP DSK:<MOON> SCRC:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(SETQ UNIBUS-CHANNEL-QS '(
	%UNIBUS-CHANNEL-LINK			;Address of next or 0 to end list
	%UNIBUS-CHANNEL-VECTOR-ADDRESS		;Interrupt vector address of device
	%UNIBUS-CHANNEL-CSR-ADDRESS		;Virtual address of status register
	%UNIBUS-CHANNEL-CSR-BITS		;Bits which must be on in CSR
	%UNIBUS-CHANNEL-DATA-ADDRESS		;Virtual address of data register(s)
						;The %%Q-FLAG bit means there are 2 data regs
	%UNIBUS-CHANNEL-BUFFER-START		;Start address of buffer
	%UNIBUS-CHANNEL-BUFFER-END		;End address+1 of buffer
	%UNIBUS-CHANNEL-BUFFER-IN-PTR		;Address of next word to store
						;The flag bit enables seq breaks per channel.
	%UNIBUS-CHANNEL-BUFFER-OUT-PTR		;Address of next word to extract
  ;**this last does not really exist now.  It should be carried thru on the next cold load.
  ;  It is required for the non-local unibus hack to work in general, altho we can get along
  ;  without it for the time being since the keyboard is always interrupt enabled.**
	%UNIBUS-CHANNEL-INTERRUPT-ENABLE-BITS ;Bit(s) in CSR which enable interrupts.
	%UNIBUS-CHANNEL-OUTPUT-TURNOFF-ADDRESS	;Address to write to shut down output channel
	%UNIBUS-CHANNEL-OUTPUT-TURNOFF-BITS))	;Value to write into that address

(ASSIGN-VALUES-INIT-DELTA UNIBUS-CHANNEL-QS 0 1 1)

(GLOBALIZE '%UNIBUS-CHANNEL-OUTPUT-TURNOFF-ADDRESS 'SYSTEM)
(GLOBALIZE '%UNIBUS-CHANNEL-OUTPUT-TURNOFF-BITS 'SYSTEM)
(DEFPROP %UNIBUS-CHANNEL-OUTPUT-TURNOFF-ADDRESS T COMPILER:SYSTEM-CONSTANT)
(DEFPROP %UNIBUS-CHANNEL-OUTPUT-TURNOFF-BITS T COMPILER:SYSTEM-CONSTANT)
)


;These files have been changed almost totally, just reload the QFASLs
(LOAD "SYS:IO;UNIBUS")
(LOAD "SYS:IO1;SERIAL")
