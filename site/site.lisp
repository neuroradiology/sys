;;;-*- Mode: LISP; Package: SYSTEM-INTERNALS; Base: 8 -*-

;; (DEFCONST SYS-HOST-DIRECTORY-TRANSLATIONS
;;   '(("CHAOS" ">chaos>")
;;     ("DISTRIBUTION" ">distribution>")
;;     ("FONTS" ">lmfonts>")
;;     ("IO" ">lmio>")
;;     ("IO1" ">lmio1>")
;;     ("LMFS" ">lmfs>")
;;     ("LMSYM" ">lmsym>")
;;     ("PATCH" ">lmpat>")
;;     ("SITE" ">site>")
;;     ("SYS" ">lispm>")
;;     ("SYS2" ">lispm2>")
;;     ("WINDOW" ">lmwin>")
;;     ("ZMAIL" ">zmail>")
;;     ("ZWEI" ">zwei>")
;;     ))

(DEFCONST SYS-HOST-DIRECTORY-TRANSLATIONS
  '(("CC" "//tree//cc//")
    ("COLD" "//tree//cold//")
    ("CHAOS" "//tree//chaos//")
    ("DEMO" "//tree//demo//")
    ("DISTRIBUTION" "//tree//distribution//")
    ("FONTS" "//tree//fonts//")
    ("IO" "//tree//io//")
    ("IO1" "//tree//io1//")
    ("LMFS" "//tree//lmfs//")
    ("NETWORK" "//tree//network//")
    ("PATCH" "//tree//patch//")
    ("SITE" "//tree//site//")
    ("SYS" "//tree//sys//")
    ("SYS2" "//tree//sys2//")
    ("UBIN" "//tree//ubin//")
    ("UCADR" "//tree//ucadr//")
    ("WINDOW" "//tree//window//")
    ("ZMAIL" "//tree//zmail//")
    ("ZWEI" "//tree//zwei//")
    ))

;;; Here is the DEFSITE special form.

(DEFSITE :DISTRIBUTION
  (:CHAOS-FILE-SERVER-HOSTS '("server"))
  (:CHAOS-HOST-TABLE-SERVER-HOSTS '("server"))
  (:CHAOS-MAIL-SERVER-HOSTS '("server"))
  (:CHAOS-TAPE-SERVER-HOSTS '("server"))
  (:CHAOS-TIME-SERVER-HOSTS '("server"))
  (:CHAOS T)
  (:DEFAULT-MAIL-MODE ':CHAOS)
  (:ESC-F-ARG-ALIST '(
		      ;; TERMINAL F lists the users on the default file
		      ;; host (usually the VAX, but see the Lisp Machine
		      ;; Manual's description of the LOGIN function,
		      ;; p. 506).
		      (NIL . :LOGIN)
		      ;; TERMINAL 1 F lists the LM-2 users.
		      (1 . :LOCAL-LISP-MACHINES)
		      ;; TERMINAL 2 F
		      (2 . ("server"))
		      ;; TERMINAL 0 F prompts for which users to list.
		      (0 . :READ)))

  (:HOST-FOR-BUG-REPORTS "server")
  (:LISPM-FILE-SERVERS T)
  (:LOCAL-MAIL-HOSTS '("server"))
  (:SITE-PRETTY-NAME "local")
  (:SYS-HOST "server")
  (:SYS-DIRECTORY-TRANSLATIONS SYS-HOST-DIRECTORY-TRANSLATIONS)
  (:TIMEZONE 5)
  ;; End of DEFSITE.
  )

;;; This is the end of the site file.
