;;; Lisp Machine mail reader -*- Mode:LISP; Package:ZWEI -*- ;;; Mail file support for local file system;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **;;; ** (c) Enhancements copyright 1981 Symbolics, Inc. **(DEFMETHOD (LMFS:LMFS-PATHNAME :MAIL-FILE-FORMAT-COMPUTER) (IGNORE)  'BABYL-MAIL-FILE)(DEFMETHOD (LMFS:LMFS-PATHNAME :POSSIBLE-RMAIL-FILES) ()  (LIST (FUNCALL-SELF ':NEW-PATHNAME ':NAME "BABYL" ':TYPE "TEXT" ':VERSION ':NEWEST)))(DEFMETHOD (LMFS:LMFS-PATHNAME :POSSIBLE-MAIL-FILE-FLAVORS) ()  '(BABYL-MAIL-FILE))(DEFMETHOD (LMFS:LMFS-PATHNAME :NEW-MAIL-PATHNAME) ()  (FUNCALL-SELF ':NEW-PATHNAME ':NAME "MAIL" ':TYPE "TEXT" ':VERSION ':NEWEST))(DEFMETHOD (LMFS:LMFS-PATHNAME :ZMAIL-TEMP-FILE-NAME) ()  "_ZMAIL_")(DEFMETHOD (LMFS:LMFS-PATHNAME :NEW-MAIL-FILE-FLAVOR) ()  'ITS-NEW-MAIL-FILE)