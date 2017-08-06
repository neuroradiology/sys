;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.17
;;; Reason: Bad scoping in server-dataproc-hack-directory.
;;; Written 12/22/81 16:49:50 by BSG,
;;; while running on Spaniel from band 1
;;; with System 78.26, ZMail 38.4, Symbolics 8.7, Tape 6.2, LMFS 21.15, Canon 9.3, microcode 841.



; From file server.LISP >LMFS POINTER:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun server-dataproc-hack-directory
       (data handle &aux ok (conn (server-dataproc-comm-conn data)))
  (trap-lossage (nil "Directory lister toplevel")
     (let* ((stream (chaos:stream conn))
	    (arg (server-dataproc-comm-arg data))
	    (string (car arg))
	    (opts (cdr arg))
	    (path (lmfs-parse-for-server string)))
       (if (stringp path)
	   (send-data-async-lossage
	     conn (format nil "STX F Error parsing path: ~A" string) handle)
	   (let ((dirlist (funcall path ':directory-list
				   (cons ':noerror opts))))
	     (if (stringp dirlist)
		 (send-data-async-lossage conn dirlist handle)
		 (progn
		   (server-dirlist-single (cdar dirlist) nil stream)
		   (dolist (file (cdr dirlist))
		     (server-dirlist-single (cdr file) (car file) stream))
		   (funcall stream ':tyo #\CR)
		   (setq ok t)))))
       (funcall stream ':force-output)
       (if ok (chaos:send-pkt conn (chaos:get-pkt) chaos:eof-op)))
    (send-data-async-lossage conn "System error during dir list processing" handle)))

)
