;-*- Mode:LISP; Package:TAPE -*-

(defun tapex-query (question options)
  (do () (())
    (format t "~&~A " question)
    (let ((c (tyi)))
      (let ((won
	      (dolist (opt options)
		(if (memq c (cdr opt)) (return (car opt))))))
	(if won (return won))
	(tv:beep)))))

(defun tapex ()
  (selectq
    (tapex-query "Read, Write, or List (r, w or l) ?"
		 '((:read #/r #/R) (:write #/w #/W) (:list #/l #/L)))
    (:read  (tapex-reader))
    (:list  (tapex-lister))
    (:write (tapex-writer))))


(defmacro with-open-tape-stream ((name . tape-options) &body body)
  `(let ((/.tape-stream-finished-normally/. nil)
	 (,name nil))
     (unwind-protect
       (prog1
	 (progn
	   (setq ,name (open-tape . ,tape-options))
	   . ,body)
	 (setq /.tape-stream-finished-normally/. t))
       (if stream
	   (if /.tape-stream-finished-normally/.
	       (tapex-close ,name)
	       (funcall ,name ':close ':abort))))))

(defun tapex-close (stream)
  (let ((wop (funcall stream ':which-operations)))
    (cond ((and (memq ':bot-p wop) (memq ':rewind wop) (memq ':eof wop)
		(not (funcall stream ':bot-p))
		(y-or-n-p "Rewind? "))
	   (if (eq (funcall stream ':mode) ':write)
	       (funcall stream ':eof))
	   (funcall stream ':rewind)))
    (funcall stream ':close)))



(defconst *TAPEX-CHARS* (string-append #O15 #O14 #O11 #O12 #O200))

(defun tapex-reader ()
  (with-open-tape-stream (stream 0 ':record-length (* 4 1024.))
    (do () (())
      (let ((path (make-array '100. ':type 'art-string ':leader-length 1)))
	(store-array-leader 0 path 0)
	
	(let ((c (funcall stream ':tyi)))
	  (if (null c) (return nil))
	  (array-push-extend path c))
	(do ((c (tyi stream) (tyi stream))) ((= c 200)) 
	  (array-push-extend path c))
	(funcall stream ':clear-input)
	(format t "~&File ~A. New path? (CR to skip) " path)
	(let ((pathstring (readline)))
	  (if (string-search-not-char #\SP pathstring)
	      (let ((newpath (fs:parse-pathname pathstring)))
		(with-open-file (outstr newpath '(:write))
		  (format t "~&Creating ~A." (funcall outstr ':truename))
		  (do () (())
		    (multiple-value-bind (buf curx remaining)
			(funcall stream ':get-input-buffer)
		      (if (null buf) (return nil))
		      (let ((interestx
			      (string-search-set *TAPEX-CHARS*
						 buf curx (+ curx remaining))))
			(if (null interestx)
			    (progn
			      (if (not (zerop remaining))
				  (funcall
				    outstr ':string-out buf curx (+ curx remaining)))
			      (funcall stream ':advance-input-buffer))
			    ;; found one
			    (progn
			      (if (not (= curx interestx))
				  (funcall outstr ':string-out buf curx interestx))
			      (funcall stream ':advance-input-buffer interestx)))
			
			(let ((c (funcall stream ':tyi)))
			  (if (or (null c) (= c 200) (= c 0))
			      (progn
				(if (not (null c)) (funcall stream ':skip-file))
				(return)))
			  (selectq c
			    (#O15             ())	;throw away cr
			    (#O12             (funcall outstr ':tyo #\CR))
			    (#O14		  (funcall outstr ':tyo #\FORM))
			    (#O11		  (funcall outstr ':tyo #\TAB))
			    (t		  (funcall outstr ':tyo c)))))))))
	      (funcall stream ':skip-file)))))))

(defconst *INVERSE-TAPEX-CHARS* '(#\TAB #\CR #\FORM))

;;; Remember that final eof is written by close op, not us.
(defun tapex-writer ()
  (with-open-tape-stream (stream 0 ':mode ':write ':record-length (* 4 1024.)
				 ':pad-char #O200)
    (do () (())
      (format t "~&File? (CR to end) ")
      (let ((pathstring (readline)))
	(let ((newpath (fs:parse-pathname pathstring)))
	  (if (not (string-search-not-char #\SP pathstring))
	      (return nil))
	  (with-open-file (instr newpath)
	    (format t "~&Dumping ~A." (funcall instr ':truename))
	    (funcall stream ':string-out (funcall (funcall instr ':truename)
						  ':string-for-host))
	    (funcall stream ':tyo #O200)
	    (funcall stream ':force-output)	;end the record
	    (do () (())
	      (multiple-value-bind (buf curx remaining)
		  (funcall instr ':get-input-buffer)
		(if (null buf) (return nil))
		(let ((interestx
			(string-search-set *INVERSE-TAPEX-CHARS*
					   buf curx (+ curx remaining))))
		  (if (null interestx)
		      (progn
			(if (not (zerop remaining))
			    (funcall
			      stream ':string-out buf curx (+ curx remaining)))
			(funcall instr ':advance-input-buffer))
		      ;; found one
		      (progn
			(if (not (= curx interestx))
			    (funcall stream ':string-out buf curx interestx))
			(funcall instr ':advance-input-buffer interestx)))
		  
		  (let ((c (funcall instr ':tyi)))
		    (if (null c)
			(return))
		    (selectq c
		      (#\CR             (funcall stream ':tyo #O15)
		       (funcall stream ':tyo #O12))
		      (#\TAB		  (funcall stream ':tyo #O11))
		      (#\FORM           (funcall stream ':tyo #O14))
		      (t		  (funcall stream ':tyo c)))))))
	    (funcall stream ':tyo #O200)
	    (funcall stream ':eof)))))))


(defun tapex-lister ()
  (with-open-tape-stream (stream 0 ':record-length (* 4 1024.))
    (do ((path (make-array '100. ':type 'art-string ':leader-list '(0)))) (nil)
      (store-array-leader 0 path 0)		;have to do each time
      (let ((c (funcall stream ':tyi)))
	(if (null c) (return nil))
	(array-push-extend path c))
      (do ((c (tyi stream) (tyi stream))) ((= c 200)) 
	(array-push-extend path c))
      (funcall stream ':clear-input)
      (format t "~&~A" path)
      (funcall stream ':skip-file))))

(defun read-lmi-tape (&aux (eofob (ncons ':eof)))
  (with-open-tape-stream (stream 0 ':record-length (* 4 1024.))
    (do () (())
      (let ((what (let ((package (pkg-find-package "")))
		    (read stream eofob))))
	(cond ((eq what eofob)
	       (return nil))
	      ((neq what ':LMFL)
	       (ferror nil "Don't know what a ~S is." what))
	      (t
	       (let ((plist (let ((package (pkg-find-package "")))
			      (read stream)))
		     (path-default nil))
		 (display-lmi-plist plist)
		 (setq path-default (fs:make-pathname
				      ':host si:local-host
				      ':device ':unspecific
				      ':directory (if (get (locf plist) ':directory)
						      (string-append
							">"
							(get (locf plist) ':directory)))
				      ':name (get (locf plist) ':name)
				      ':type (get (locf plist) ':type)
				      ':version ':newest))
		 (format t "~&New path? (default ~A) (CR to skip) " path-default)
		 (let ((pathstring (readline)))
		   (if (not (string-search-not-char #\SP pathstring))
		       (funcall stream ':skip-file)
		       (let ((newpath 
			       (fs:merge-pathname-defaults
				 (fs:parse-pathname pathstring nil path-default)
				 path-default)))
			 (with-open-file (outstr newpath '(:write))
			   (format t "~&Creating ~A." (funcall outstr ':truename))
			   (funcall stream ':clear-input)
			   (do () (())
			     (multiple-value-bind (buf curx end)
				 (funcall stream ':get-input-buffer)
			       (if (null buf)
				   (return nil)	;AHA THERE WERE NO PADS!!!!
				   (setq end (+ end curx)))	;like this better
			       (let ((zerx (string-reverse-search-not-char
					     0 buf end curx)))
				 (cond ((= zerx (1- end))
					(funcall outstr ':string-out buf curx end)
					(funcall stream ':advance-input-buffer end))
				       (t (funcall outstr ':string-out buf curx (1+ zerx))
					  (funcall stream ':advance-input-buffer end)
					  (funcall stream ':skip-file)
					  (return nil)))))))))))))))))
		  
(defun display-lmi-plist (plist)
  (tv:doplist (plist prop ind)
    (format t "~&~A:~30t~A"
	    (zwei:pretty-command-name (string-append ind))	;he clobbers
	    (selectq ind
	      (:AUTHOR         prop)
	      (:CREATION-DATE  (time:print-universal-time prop nil))
	      ((:VERSION :BYTE-SIZE)
	                       (format nil "~D" prop))
	      (T               prop))))) 