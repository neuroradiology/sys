;-*- Mode:LISP; Package:USER; Base:8. -*-
;;; This file defines bogus systems to describe all that ought be distributed,
;;; and says which systems should be distributed.  This file is expected to be
;;; found in sys:distribution;distribution-list lisp > by the program
;;; distribution-tape.lisp. 

;;;Nobody except the person doing the dumping should load this thing, and he//she
;;;will get it loaded automatically.  Those other than those at Symbolics will have
;;;to change the "scrc" in the TOPS20 logical-host definition.

;;; BSG 12/31/81

(si:define-simple-transformation :uload ignore nil ("uload") nil)

(si:define-simple-transformation :midas ignore nil
				 ("mid") nil)

(si:define-simple-transformation :failasm ignore nil
				 ("fai") nil)

(fs:add-logical-pathname-host "tops20" (fs:get-pathname-host "scrc") '(("chaos" "<chaos>")))


(defsystem tops20-utilities
  (:pathname-default "tops20:chaos;")
  (:module distribution-tape "dtape" "chatap")
  (:module chaos-user-ftp "cftp")
  (:module chaos-file-job ("file" "fildef"))
  (:module chaos-host-table "chahtb")
  (:module chaos-symbols "chasym")
  (:midas (distribution-tape chaos-file-job chaos-host-table chaos-symbols))
  (:failasm chaos-user-ftp))


(defsystem distributed-utilities
  (:pathname-default "sys:sys;")
  (:package system)
  (:module copy-tools ("distribution;lmccop" "distribution;copy"))
  (:module build-utilities
	   ("pkgdcl" "qdefs" "recom" "sys2;global" "sys2;system" "sys2;let" "sys2;macros"))
  (:module micro-tools
	   ("io1;as8748" "io1;cdrive" "io1;promp" "ucadr;praid" "sys2;micdoc"
	    "io1;ctest" "io1;ukbd"))
  (:module memd-uload  "ubin;memd uload")
  (:module mini "tops20:chaos;mini mid")
  (:uload memd-uload)
  (:compile copy-tools)
  (:compile build-utilities)
  (:compile micro-tools)
  (:midas mini))

(setq distribution:*distribution-standard-systems*
	      '("system"
		"tops20-utilities"
		"distributed-utilities"
		"zmail"
		"lmfs"
		"symbolics"			;includes "distribution"
		"tape"
		("cold" "sys:sys;coldpk lisp")
		("cube" "pointer:>cube>cubpkg.lisp")
	      ))

;;;Daedalus is another issue.  If you want to dump it or anything else,
;;; simply load the system declaration and tell the dumper.

(setq distribution:*loaded-distribution-list* t)
