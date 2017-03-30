;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.13
;;; Reason: distribution: double-indirect and 16-bit and clear-eof bugs.
;;; Written 1/11/82 16:45:31 by File Server,
;;; while running on Pointer from band 3
;;; with System 78.47, ZMail 38.5, Symbolics 8.12, Tape 6.5, LMFS 21.33, Canon 9.11, microcode 841.



; From file distribution-dump.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

(defun distribution-16-bit-copy (from to)
  (using-resource (indirect distribution-8bit)
    (do (8len) (())
      (multiple-value-bind (buf start lastx)
	  (funcall from ':read-input-buffer)
	(if (null buf) (return nil))
	(cond 
	  ((prog1 (array-indexed-p buf)		;assume 16..
		  (setq 8len (* 2 (- lastx start))))
	   (funcall from ':advance-input-buffer start)
	   (return (loop as c = (funcall from ':tyi)
			 when (null c) return nil
			 do
			 (funcall to ':tyo (ldb #O0008 c))
			 (funcall to ':tyo (ldb #O0808 c)))))

	  (t
	   (si:change-indirect-array indirect 'art-8b (list 8len) buf (* 2 start))
	   (funcall to ':string-out indirect 0 8len)))
	(funcall from ':discard-current-input-buffer)))))

)

; From file distribution-dump.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

(defun wdsf-help (path report fetch case)
  (format t "~&N = Nothing; skip this, make no attempt to dump any version of ~A.~@
	       T = Try again to get ~A (after you've attempted to remedy the problem).~@
               D = List all files matching ~A.~@
	       ~:[~*~*~;L = Use ~A, the latest version of ~A.~@
               ~]E = Prompt for a new pathname to be written as ~A.~2%"
	  path report (funcall path ':new-pathname ':type ':wild ':version ':wild)
	  (eq case ':specific-not-found-but-there-are-some) fetch path path)) 

)

; From file distribution-load.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

(defun distribution-16-bit-reverse-copy (from to &aux 16len)
  (using-resource (indirect distribution-16bit)
    (do () (())
      (multiple-value-bind (buf start lastx)
	  (funcall from ':read-input-buffer)
	(cond ((null buf) (return nil))
	      ((or (oddp start) (oddp (- lastx start)))	;hard to believe, but just in case,
	       (funcall from ':advance-input-buffer start)
	       (return (loop as c1 = (funcall from ':tyi)
			     when (null c1) return nil
			     as c2 = (funcall from ':tyi)
			     do
			     (funcall to ':tyo (dpb c2 #O1010 c1)))))
	      ;; King of elegance.  Double indirect arrays don't work, so....
	      ((prog1
		 (array-indexed-p buf)		;assume 16..
		 (setq 16len (// (- lastx start) 2)))
	       (let ((disp-to (%p-contents-offset buf 1))
		     (disp-off (%p-contents-offset buf 3)))
		 (or (eq (array-type disp-to) 'art-16b)
		     (ferror nil "Odd array indirected to - ~S" disp-to))
		 (let ((rstart (// start 2))
		       (roff (// disp-off 2)))
		   (funcall to ':string-out disp-to (+ rstart roff) (+ rstart roff 16len)))))
	      (t 
	       (si:change-indirect-array indirect 'art-16b (list 16len) buf (// start 2))
	       (funcall to ':string-out indirect 0 16len)))
	(funcall from ':discard-current-input-buffer)))))

)

; From file distribution-load.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

(defun distribution-load-font-file (stream story)
  (or (distribution-load-path
	stream
	(funcall (fs:parse-pathname "sys:fonts;foo qfasl >") ':new-name
		 (get story ':font))
	story)
      (distribution-clear-to-eof stream))
  (funcall stream ':clear-eof))

)

; From file distribution-load.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

(defun distribution-load-ucode-file (stream story)
  (or
    (distribution-load-path
      stream
      (funcall (if (get story ':microcode-source)
		   (fs:parse-pathname "sys:ucadr;ucadr")
	       (fs:parse-pathname "sys:ubin;ucadr"))
	       ':new-type-and-version
	       (cond ((get story ':microcode-source) "LISP")
		     ((get story ':microcode-object) "MCR")
		     ((get story ':microcode-symbols) "SYM")
		     ((get story ':microcode-table) "TBL"))
	       (get story ':microcode-version))
      story)
    (distribution-clear-to-eof stream))
  (funcall stream ':clear-eof))

)

