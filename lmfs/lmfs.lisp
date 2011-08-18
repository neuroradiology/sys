;-*-package:user;  mode:lisp; lowercase:t-*-

(package-declare lmfs system 4096. ()
		 (shadow send send-self)
		 (myrefname global lmfs))

(format t "~&If you're loading this, you probably want to (make-system 'tape)
before (make-system 'lmfs).")



(load "sys: lmtape; tappkg lisp >")

(defsystem lmfs
  (:pathname-default "sys: lmfs;")
  (:patchable)
  (:package lmfs)
  (:bug-reports)
  (:module 1st-level-macros ("defsetf" "defstorage"))
  (:module fs-dcls "fs-dcls")
  (:module buffer-macros "bufmacs")
  (:module fs-programs
	   ("buffers" "files" "records" "vols" "dirs"
	    "plist" "paths" "fsstr" "commands" "diskio" "fs-interim"))
  (:module lmfs-user "fs-user" :package fs)
  (:module icings ("salvage" "server" "backup"))
  (:module fsedit "fsedit" :package tv)
  (:module actedit "actedit")
  (:module fsmaint "fsmaint")
  (:module zmail ("zmail; lmfs" "zmail; lmfs-server") :package zwei)
  (:module peek "peek" :package tv)
  (:compile-load 1st-level-macros)
  (:compile-load fs-dcls (:fasload 1st-level-macros))
  (:compile-load buffer-macros (:fasload 1st-level-macros fs-dcls))
  (:compile-load fs-programs (:fasload 1st-level-macros fs-dcls buffer-macros))
  (:compile-load lmfs-user)
  (:compile-load icings (:fasload 1st-level-macros fs-dcls buffer-macros fs-programs))
  (:compile-load fsedit)
  (:compile-load actedit (:fasload fs-dcls fs-programs fsedit))
  (:compile-load fsmaint (:fasload fs-dcls fs-programs fsedit actedit))
  (:compile-load zmail)
  (:compile-load peek))
