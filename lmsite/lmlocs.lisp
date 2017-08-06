;;; -*- Mode: Lisp; Package: SI; Base:8 -*-

;;; This file contains information on the physical location of the various lisp machines.

;;; Format is
;;; (name machine-name finger-location (building floor) associated-machine)

;;; Each machine is the only source of knowledge of where it is; all programs 
;;; which print machine locations ask each machine for its location.
;;; To change a machine's location knowledge:
;;; (1) Edit this file to correctly list the affected machine's location.
;;; (2) Do (SI:RECOMPILE-SITE-FILES) on any LISPM
;;; (3) Cold-boot the affected machine.
;;; (4) Do (SI:UPDATE-SITE-CONFIGURATION-INFO)
;;; To make this knowledge permanent, do (DISK-SAVE "LODn") immediately after
;;; (SI:UPDATE-SITE-CONFIGURATION-INFO) file.  This will add the new location
;;; knowledge to the band.  See the LispM Manual, section 24.8,
;;; for more information on saving and restoring bands.

(DEFCONST MACHINE-LOCATION-ALIST '(

("SERVER" "server" "location" (MAIN 1) "server")

("CADR" "cadr" "location" (MAIN 2) "server")

))
