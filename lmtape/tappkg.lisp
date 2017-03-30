;-*-package:user;  mode:lisp; lowercase:t-*-

(package-declare tape system 500. ()
	(myrefname global tape))

(defsystem tape
  (:name "Tape")
  (:not-in-disk-label)
  (:pathname-default "sys: lmtape;")
  (:patchable)
  (:not-in-disk-label)
  (:package tape)
  (:module phys-tape-handler "tape")
  (:module tape-stream "tapestr")
  (:module band-transfer "tape-band")
  (:module tapex "tapex")
  (:module diagnostic "tape-test")
  (:compile-load phys-tape-handler)
  (:compile-load tape-stream)
  (:compile-load band-transfer)
  (:compile-load tapex)
  (:compile diagnostic))	;Not normally loaded, but kept up to date with compilation






