;-*- Mode:LISP; Package:TAPE; Lowercase:T -*-
 
;;; All this stuff is entirely specific to the Western Peripherals
;;; TC-131 Controller interfaced on the Lispm UNIBUS.

(defvar *UNIBUS-TO-XBUS-UNIBUS-MAP-LOC* #O766140)
(defvar *UNIBUS-TO-XBUS-VALID* #O100000)
(defvar *UNIBUS-TO-XBUS-WRITE-PERMIT* #O040000)
(defvar *UNIBUS-TO-XBUS-LDB-PPSS* 1016)
(defvar *UNIBUS-TO-XBUS-BASE* #O140000)


(defvar *TAPE-STATUS-REGISTER* #O772520)
(defvar *TAPE-COMMAND-REGISTER* #O772522)
(defvar *TAPE-MTCMA* #O772526)
(defvar *TAPE-MTBRC* #O772524)


;;; Intended as user-callable, mainly for debugging.


(defun tstat ()
  (print-tape-status-register (%unibus-read *TAPE-STATUS-REGISTER*))
  (print-tape-command-register (%unibus-read *TAPE-COMMAND-REGISTER*))
  (format t "~& Address register (@ ~O): ~O" *TAPE-MTCMA* (%unibus-read *TAPE-MTCMA*))
  (format t "~& Count register (@ ~O): ~O" *TAPE-MTBRC* (%unibus-read *TAPE-MTBRC*)))

(defun print-tape-status-register (value)
  (tstat-print value
	       *TAPE-STATUS-REGISTER*
	       "Tape status register"
	       '((:not "Tape Unit Not Ready")
		 "Rewinding"
		 "Write Lock"
		 "Tape Slowing Down"
		 (:field 1 "9 Track" "7 Track")
		 "BOT"
		 (:field 1 "Unit not selected" "Unit selected")
		 "NXM error"
		 "Bad tape error"
		 "Record length error"
		 "End of Tape"
		 "Bus Grant Late"
		 "Parity error"
		 "Odd length record error"
		 "EOF"
		 "Illegal command")))

(defun print-tape-command-register (value)
  (tstat-print value
	       *TAPE-COMMAND-REGISTER*
	       "Command Register"
	       '("Go (s//b 0)"
		 (:field 3 "Set Off line" "Read" "Write" "Write EOF"
			   "Space Forward" "Space Reverse" "Write ERG" "Rewind")
		 (:fixnum 2 "UBX")
		 "Interrupt enable"
		 (:not "Controller Busy")
		 (:fixnum 3 "Unit")
		 (:field 1 "Odd Parity Mode" "Even Parity Mode")
		 "Power Clear (s//b 0)"
		 nil
		 (:field 1 "1600 BPI" "800 BPI")
		 "Error")))

(defun tstat-print (reg address name specs)
  (format t "~&~A (@ ~O):~%   ~O" name address reg)
  (dolist (spec specs)
    (setq reg
	  (lsh reg
	       (- (cond ((null spec) 1)
			((atom spec)
			 (if (bit-test 1 reg)
			     (format t ", ~A" spec))
			 1)
			((eq (car spec) ':fixnum)
			 (format t ", ~A=~D" (third spec) (load-byte reg 0 (second spec)))
			 (second spec))
			((eq (car spec) ':not)
			 (if (not (bit-test 1 reg))
			     (format t ", ~A" (cadr spec)))
			 1)
			((eq (car spec) ':field)
			 (let ((width (second spec))
			       (names (cddr spec)))
			   (format t ", ~A"
				   (nth (load-byte reg 0 width) names))
			   width))
			(t (ferror nil "Quid, me vexari?"))))))))


;;; Basically for debugging.

(defun tcmd (cmd &optional (drive 0))
  (%unibus-write *TAPE-COMMAND-REGISTER*
		 (logior
		   1				;go
		   (lsh
		     (cond ((numberp cmd) cmd)
			   (t (do ((i 0 (1+ i))
				   (x '("Off line" "Read" "Write" "Write EOF"
					"Space Forward" "Space Reverse" "Write ERG" "Rewind")
				      (cdr x)))
				  ((null x)
				   (ferror nil "Invalid command ~A" cmd))
				(if (string-equal (car x) cmd)
				    (return i)))))
		     1)
		   (lsh drive 8)))
  (tstat))



;;;These three guys are used for low-level, from-Lisp-listener debugging.

(defun read-tape-to-array (array &optional (drive 0) (from 0) to nostat)
  (io-tape-to-array ':read array drive from to nostat))


(defun write-tape-from-array (array &optional (drive 0) (from 0) to nostat)
  (io-tape-to-array ':write array drive from to nostat))

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
      (si:unwire-disk-rqb rqb))))



;;; These guys are used by both the debugging fcns above, and the real
;;; magtape stream in TAPESTR.

(defun rqb-to-data-virtual-address (rqb)
  (let ((long-array-flag (%p-ldb sys:%%array-long-length-flag rqb))
	(offset (%p-contents-offset (si:rqb-buffer rqb) 3)))
    (+ (%pointer rqb) (// offset 2) long-array-flag 1)))

(defun tape-command
       (drive cmd &optional virt-addr byte-count &aux (offset 0))
  (cond ((not (null virt-addr))
	 (setq offset (* (logand virt-addr (1- sys:page-size)) 4))
	 (loop for count from (- offset) by (* 4 sys:page-size)
	       and ubaddr from (+ 16. *UNIBUS-TO-XBUS-UNIBUS-MAP-LOC*) by 2
	       and mapnumber from 8 by 1
	       and vaddr from virt-addr by sys:page-size
	       until ( count byte-count)
	       when ( mapnumber 16.)
		 do (ferror nil "Unibus map capacity exceeded")
	       do
	       (%unibus-write ubaddr
			      (logior *UNIBUS-TO-XBUS-VALID*
				      (if (eq cmd ':read) *UNIBUS-TO-XBUS-WRITE-PERMIT* 0)
				      (ldb *UNIBUS-TO-XBUS-LDB-PPSS*
					   (sys:%physical-address vaddr))
				      )))))
  (let ((command (find-position-in-list cmd '(:off-line :read :write :write-eof
					      :space-forward :space-reverse
					      :write-erg :rewind))))
    (if (null command) (ferror nil "~S unknown tape command" cmd))
    (%unibus-write *TAPE-MTBRC* (- (or byte-count 2)))
    (%unibus-write *TAPE-MTCMA* (+ (* 8 1024.) *UNIBUS-TO-XBUS-BASE* offset))
    (%unibus-write *TAPE-COMMAND-REGISTER*
		   (logior 1
			   (lsh command 1)
			   (lsh drive 8)))))

(defun tape-status (drive)			;a discovery by BSG
  (%unibus-write *TAPE-COMMAND-REGISTER* (lsh drive 8.))	;NO "go" bit
  (%unibus-read *TAPE-STATUS-REGISTER*)		;let logic settle.
  (%unibus-read *TAPE-STATUS-REGISTER*))

;;; intended as a high-level interface
(defun drive-ready-p (drive)
  (not (tape-offlinep (tape-status drive))))

(defun drive-bot-p (drive)
  (tape-bot-p (tape-status drive)))
				   
(defun tape-wait-op-complete ()
  (process-wait "Tape I//O" #'tape-complete-p))

(defun tape-wait-rewind-complete (&optional drive)	;Defaults to last selected
  (if drive (%unibus-write *TAPE-COMMAND-REGISTER* (lsh drive 8.)))
  (process-wait "Rewind"
		#'(lambda () (not (bit-test 2 (%unibus-read *tape-status-register*))))))


;;; These guys isolate all controller status bit knowledge.
;;; [Except for the controller bit knowledge that's elsewhere.]

(defun tape-complete-p ()
  (bit-test 1_7 (%unibus-read *TAPE-COMMAND-REGISTER*)))

(defun tape-error-p ()
  (bit-test 1_15. (%unibus-read *TAPE-COMMAND-REGISTER*)))

(defun tape-errstat ()
  (%unibus-read *TAPE-STATUS-REGISTER*))

(defun tape-rewindingp (err)
  (bit-test 1_1. err))

(defun tape-bot-p (stat)
  (bit-test 1_5. stat))

(defun tape-offlinep (err)
  (not (bit-test 1_0. err)))
  
(defun tape-eofstatp (err)
  (bit-test 1_14. err))

(defun tape-eotstatp (err)
  (bit-test 1_10. err))

(defun tape-errprint (err)
  (print-tape-status-register err))

(defun tape-byte-residue ()
  (let ((val (%unibus-read *TAPE-MTBRC*)))
    (if (zerop val) val
	(- 1_16. val))))

;;; corrects deficiency in Lisp machine.
(defun rewind (s) (funcall s ':rewind))

;;; debugging, long-arm
(defun rw0 () (tape-command 0 ':rewind)
       (tape-wait-op-complete))

;;; another Lispm deficiency corrected.
(defun print-unibus-map ()
  (dotimes (i 16.)
    (let ((word (%unibus-read (+ *UNIBUS-TO-XBUS-UNIBUS-MAP-LOC* (* 2 i)))))
      (format t "~&~o ~4T ~o ~:[INVALID~;VALID~] ~:[READ-ONLY~;WRITE-PERMIT~] ~o"
	      i
	      (+ (* i 4 sys:page-size) *UNIBUS-TO-XBUS-BASE*)
	      (bit-test *UNIBUS-TO-XBUS-VALID* word)
	      (bit-test *UNIBUS-TO-XBUS-WRITE-PERMIT* word)
	      (* sys:page-size (ldb 13. word))))))


;;TOOL


(defvar inspect-tape-array nil)
(setq inspect-tape-array nil)			;People will load this over old ones...

(defun scan-tape () (inspect-tape ':brief))

(defun inspect-tape (&optional brief)
  (if (not (drive-bot-p 0))
      (if (y-or-n-p "Rewind? ")
	  (progn
	    (rw0)
	    (tape-wait-rewind-complete))))
  (if (null inspect-tape-array)
      (setq inspect-tape-array  (make-array 4096. ':type 'art-string)))
  (do () (())
    (fillarray inspect-tape-array '(0))
    (or brief (funcall terminal-io ':clear-screen))
    (read-tape-to-array inspect-tape-array 0 0 4096. t)
    (if (and (tape-error-p)
	     (not (tape-eofstatp (tape-errstat))))
	(tstat))
    (cond ((tape-eofstatp (tape-errstat))
	   (format t "~&At EOF."))
	  (t
	   (let ((bytes (- 4096. (tape-byte-residue))))
	     (format t "~&Read ~D bytes." bytes)
	     (let ((sv inspect-tape-array) (n (// (+ bytes 3) 4)))
	       (if brief (setq n 0))
	       (dotimes (i n)
		 (if (funcall terminal-io ':listen) (return nil))
		 (let ((a (* 4 i)))
		   (let ((a1 (aref sv (+ a 3)))
			 (a2 (aref sv (+ a 2)))
			 (a3 (aref sv (+ a 1)))
			 (a4 (aref sv (+ a 0))))
		     (format t "~%~4O F: ~12O H: ~6O ~6O B: ~3O ~3O ~3O ~3O C: ~C~C~C~C"
			     i
			     (+ a4 (lsh (+ a3 (lsh (+ a2 (lsh a1 8)) 8)) 8))
			     (+ a2 (lsh a1 8))
			     (+ a4 (lsh a3 8))
			     a1 a2 a3 a4
			     a4 a3 a2 a1))))))))
    (funcall terminal-io ':fresh-line)
    (if brief
	(if (funcall terminal-io ':listen)
	    (let ((c (tyi)))
	      (or (equal c #\SP)
		  (funcall terminal-io ':untyi c))
	      (return nil)))
	(let ((x (tyi)))
	  (if (memq x '(#/F #/f))
	      (progn
		(tape-command 0 ':space-forward nil 0)
		(tape-wait-op-complete)))))))
