;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.16
;;; Reason: Finish 78.13
;;; Written 12/15/81 14:35:06 by MMcM,
;;; while running on Lisp Machine Five from band 5
;;; with System 78.5, ZMail 38.0, microcode 836.



; From file ZFIX
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))


(DOLIST (E *EDITORS-WHOSE-MODES-TO-RESET*)
  (LET* ((MLW (SYMEVAL-IN-INSTANCE E '*MODE-LINE-WINDOW*))
	 (MBW (FUNCALL MLW ':SEARCH-MINI-BUFFER-WINDOW)))
    (TV:BLINKER-SET-VISIBILITY (WINDOW-POINT-BLINKER MBW) NIL)
    (SETF (WINDOW-POINT-BLINKER MBW) (TV:MAKE-BLINKER (WINDOW-SHEET MBW)))))

)
