;-*- Mode:LISP; Package:USER; Base:8. -*-



(package-declare distribution system 4096. ()
		 (myrefname global distribution))

(defsystem distribution
  (:pathname-default "sys:distribution;")
  (:package distribution)
  (:module dist-band ("dist") :package system-internals)
  (:module embedded-length ("embedded-length-stream") :package system-internals)
  (:module writer ("distribution-dump"))
  (:module reader ("distribution-load"))
  (:module dummy-defs ("distribution-list"))

  (:compile-load embedded-length)
  (:compile-load dist-band)
  (:compile-load writer)
  (:compile-load reader (:fasload writer))
  (:skip :readfile dummy-defs))
