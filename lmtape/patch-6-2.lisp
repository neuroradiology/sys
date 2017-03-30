;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Tape version 6.2
;;; Reason: Fix io-tape-to-array to return disk request blocks
;;; Written 12/18/81 14:45:33 by File Server,
;;; while running on Pointer from band 1
;;; with System 78.20, ZMail 38.3, Experimental Symbolics 8.5, Experimental Tape 6.1, Experimental LMFS 21.14, Canon 9.1, microcode 840.



; From file TAPE.LISP DSK:<LMTAPE> SCRC:
#8R TAPE:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "TAPE")))

(defun io-tape-to-array (command array drive from to nostat)
  (if (null to) (setq to (array-length array)))
  (let ((art (car (arraydims array))))
    (if (eq art 'art-string) (setq art 'art-8b))
    (if (not (memq art '(art-8b art-16b)))
	(ferror nil "io-tape-to-array: array type of ~S not art-8b or art-16b" array))
    (let* ((datalen (- to from))
	   (npages (// (lmfs:round-up-to-boundary
			 sys:page-size
			 (let ((bpw (if (eq art 'art-8b) 2 4)))
			   (// (lmfs:round-up-to-boundary bpw datalen) bpw)))
		       sys:page-size))
	   (rqb (si:get-disk-rqb npages))
	   (8ary (if (eq art 'art-8b)		;should unwp...
		     (make-array datalen ':type 'art-8b ':displaced-to (si:rqb-buffer rqb)
				 ':displaced-index-offset
				 (%p-contents-offset (si:rqb-buffer rqb) 3))))
	   (copyarray (if (eq art 'art-8b) 8ary (si:rqb-buffer rqb))))
      (selectq command
	(:read   (fillarray (si:rqb-buffer rqb) '(0)))
	(:write  (copy-array-portion array from to copyarray 0 datalen)))      
      (si:wire-disk-rqb rqb)
      (tape-command
	drive
	command
	(rqb-to-data-virtual-address rqb)
	(if (eq art 'art-8b) datalen (* 2 datalen)))
      (tape-wait-op-complete)
      (or nostat (tstat))
      (if (eq command ':read)
	  (copy-array-portion copyarray 0 datalen array from to))
      (if 8ary (return-array 8ary))
      (si:unwire-disk-rqb rqb)
      (si:return-disk-rqb rqb))))

)

