;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for Symbolics version 8.9
;;; Reason: System-deleting bugs in GET-DISTRIBUTION-OPTIONS.
;;; Written 1/07/82 14:36:37 by BSG,
;;; while running on Basset from band 4
;;; with System 78.44, ZMail 38.5, Symbolics 8.8, Tape 6.5, LMFS 21.31, Canon 9.11, microcode 841.



; From file distribution-dump.lisp >distribution POINTER:
#8R DISTRIBUTION:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "DISTRIBUTION")))

(defun get-distribution-options (&aux (ucode-version sys:%microcode-version-number)
				 interact-pane
				 systems bands reel (drive 0) do-it
				   scroll-pane
				   &special	;love ya, chvv
				   (*dlcv-Global-dump-fonts* t)
				   (*dlcv-Global-comment* "")
				   (*dlcv-Global-ucode* nil))

  (using-resource (frame distribution-frame)
    (setq scroll-pane (funcall frame ':get-pane 'scroll-pane))
    (unwind-protect				;make sure these stack pointers go away..
      (progn
	(funcall
	  (funcall frame ':get-pane 'chvv-pane) ':set-stack-group sys:%current-stack-group)
	;;should setup menus for dumping too, but,.. leave for a few hours..
	(funcall scroll-pane
		 ':set-display-item
		 (distribution-dump-display-item (locf bands) (locf systems)
						 (locf reel) (locf drive)
						 (locf ucode-version)))
	(setq interact-pane (funcall frame ':get-pane 'interact-pane))
	(tv:window-call (frame :deactivate)
	 (tv:window-call (interact-pane :deactivate)
	  (let ((terminal-io interact-pane)
		(chvv (funcall frame ':get-pane 'chvv-pane)))
	    (*catch
	      'outer
	      (do () (())
		(*catch
		  'sys:command-level
		  (do ((noclear nil)) (())
		    (if noclear
			(setq noclear nil)
		      (funcall interact-pane ':clear-screen))	;should be small
		    (funcall scroll-pane ':redisplay)
		    ;;One day the activity system will fix all this, right?
		    (if (neq tv:selected-window interact-pane)
			(funcall interact-pane ':select))
		    (let ((object (funcall frame ':any-tyi)))
		      (cond
			((numberp object)	;let it go for now
			 (selectq (char-upcase object)
			   (#\END  (if (null reel)
				       (setq reel (get-reel-spec))
				     (cond ((and (null bands) (null systems)
						 (null *dlcv-Global-ucode*)
						 (null *dlcv-Global-dump-fonts*))
					    (format t "~&What do you want to dump?")
					    (setq noclear t))
					   ((and (equal *dlcv-Global-var-dump-device*
							"Connect")
						 (or (and
						       (zerop
							 (string-length *dlcv-Global-host*))
						       (progn
							 (format t
								 "~&You must specify a host.")
							 (setq noclear t)))
						     (and
						       (null (chaos:address-parse
							       *dlcv-Global-host*))
						       (progn
							 (format t "Bad host: ~A"
								 *dlcv-Global-host*)
							 (setq noclear t))))))
					   (t (setq do-it t)
					      (*throw 'outer 'do-it)))))
			   (#/S			;add system
			    
			    (setq systems (distribution-merge
					    systems (get-distribution-system-specs) ':add)))
			   
			   (#/B	    
			    (setq bands
				  (distribution-merge
				    bands (list (get-distribution-band-spec)) ':add)))
			   (#/R  (setq reel (get-reel-spec)))
			   (#/F  (setq *dlcv-Global-dump-fonts*
				       (not *dlcv-Global-dump-fonts*))
			         (funcall chvv
					  ':redisplay-variable '*dlcv-Global-dump-fonts*))
			   (#/U (funcall frame ':force-kbd-input ':ucode-mouse))
			   (#\SP)
			   (t (tv:beep))))
			((atom object)
			 (selectq object
			   (:reel-mouse  (setq reel (get-reel-spec)))
			   (:drive-mouse (setq drive (get-drive-spec)))
			   (:ucode-mouse (setq ucode-version (get-distribution-ucode-number))
					 (setq *dlcv-Global-ucode* t)
					 (funcall chvv ':redisplay-variable
						  '*dlcv-Global-ucode*))))
			(t (selectq (car object)
			     (:system-delete (setq systems (delete (cadr object) systems)))
			     (:band-delete   (setq bands (delete (cadr object) bands)))
			     ((:system-select :band-select) (tv:beep))
			     (:menu
			      (let ((val (funcall (fourth object) ':execute (second object))))
				(selectq val
				  (:do-it    (funcall frame ':force-kbd-input '#\end))
				  (:abort    (setq do-it nil)
					     (*throw 'outer ':abort))
				  (:ucode-version
				            (funcall frame ':force-kbd-input ':ucode-mouse))
				  (:reset-band-list (setq bands nil))
				  (:add-band (funcall frame ':force-kbd-input #/B))
				  (:delete-one-band
				   (format t "~&Mouse or type band to remove: ")
				   (let ((x (funcall frame ':any-tyi)))
				     (if (and (listp x)
					      (memq (car x) '(:band-select :band-delete)))
					 (setq bands (delete (cadr x) bands))
				       (funcall terminal-io ':untyi x)
				       (setq bands (delete (get-distribution-band-spec nil)
							   bands)))))
				  (:standard-systems
				   (setq systems (mapcar #'distribution-define-system
							 *distribution-standard-systems*)))
				  (:add-system (funcall frame ':force-kbd-input #/S))
				  (:reset-system-list  (setq systems nil))
				  (:delete-one-system
				   (format t "~&Mouse or type system to remove: ")
				   (let ((x (funcall frame ':any-tyi)))
				     (if (and (listp x)
					      (memq (car x) '(:system-select :system-delete)))
					 (setq systems (delete (cadr x) systems))
				       (funcall terminal-io ':untyi x)
				       (setq
					 systems
					 (delete (get-distribution-system-specs nil) 
						 systems)))))
				  (t (print val)))))
			     
			     (:variable-choice
			      (tv:choose-variable-values-process-message
				(second object) object))
			     (t (print object)))))))))))))

	
	)
      (funcall scroll-pane ':set-display-item nil))
    (and do-it
	 (values systems bands *dlcv-Global-comment*
		 (distribution-tape-stream
		   ':tape-p (cond ((equal *dlcv-Global-var-dump-device* "local") t)
				  ((equal *dlcv-Global-var-dump-device* "connect") ':connect)
				  (t nil))
		   ':host *dlcv-Global-host*
		   ':direction  ':output
		   ':unit drive
		   ':reel reel)
		 reel (and *dlcv-Global-ucode* ucode-version)
		 *dlcv-Global-dump-fonts*))))

)

