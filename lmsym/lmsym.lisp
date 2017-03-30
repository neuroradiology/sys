;-*- Mode:LISP; Package:USER; Lowercase:T -*-

;;; Enhancements to the standard LISPM system by Symbolics, Inc.
;;; (c) Copyright Symbolics, Inc., 1981

(defsystem lmsym
  (:name "Symbolics")
  (:pathname-default "sys: lmsym;")
  (:patchable)
  (:not-in-disk-label)
  (:component-systems lmsym-fed distribution))

(defsystem distribution
  (:pathname-default "sys: distribution;")
  (:compile-load ("dist")))

(defsystem lmsym-fed
  (:pathname-default "sys: lmsym;")
  (:module fed ("fed"))
  (:compile-load fed))
