;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.5
;;; Reason: Print-disk-label-any in FSMaint.
;;; Written 12/16/81 10:30:59 by BSG,
;;; while running on Basset from band 1
;;; with System 78.16, ZMail 38.2, Experimental Symbolics 8.3, Experimental Tape 6.1, Experimental LMFS 21.3, Canon 9.0, microcode 840.



; From file FSMAINT.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

;;; This function is for the purposes of patch files so that the latter can manhandle
;;; extant fsmaint frames.
(defun find-fsmaint-frame (&optional pane)
  (or
    (dolist (inf (funcall tv:main-screen ':inferiors))
      (if (typep inf 'fsmaint-frame)
	  (if (null pane)
	      (return inf)
	    (return (or (funcall inf ':get-pane pane)
			(ferror nil "Can't find pane ~S of fsmaint frame" pane))))))
    (ferror nil "Can't find fsmaint frame")))

)

; From file FSMAINT.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defconst fsmaint-global-menu-alist
	  '(("Tree edit root"
	     :buttons
	     ((nil :value :tree-edit-root)
	      (nil :value :tree-edit-homedir-root)
	      (nil :value :tree-edit-any-root))
	     :documentation
	     "Tree edit whole file system. L: Local  M: Homedir sys R: Any host")
	    ("Tree edit any" :value :tree-edit-any
	     :documentation "Invoke the File System editor on a directory to be prompted for.")
	    ("Tree edit Homedir"
	     :buttons
	     ((nil :value :tree-edit-homedir)
	      (nil :value :tree-edit-local-homedir)
	      (nil :value :beep))
	     :documentation
	     "L: Invoke the File System editor on your home directory  M: local homedir")
	    ("Maintenance" :value :wizard
	     :documentation "Invoke highly specialized operations")


	    ("Incremental Dump" :value :incremental-dump
	     :documentation "Start an incremental backup dump")
	    ("Complete Dump" :value :complete-dump
	     :documentation "Start a complete backup dump")
	    ("Salvage" :value :salvage
	     :documentation "Invoke the partition salvager on the file partition")
	    ("Retrieve" :value :retrieve
	     :documentation "Retrieve selected files from backup tape")

	    ("Print Disk Label"
	     :buttons
	     ((nil :value :print-disk-label)
	      (nil :value :print-disk-label-any)
	      (nil :value :beep))
	     :documentation
	     "L: Print disk label from drive 0.  M: Prompt for drive number or host")
	    ("Print Loaded Band" :value :print-loaded-band
	     :documentation "Print info about the running system")
	    ("Flush Free Buffer" :value :flush-free-buffer
	     :documentation "Update free record buffer to disk")
	    ("Free records" :value :free-records
	     :documentation "Print information about the number of free records in file partition")


	    ("Lisp Window" :value :lisp-window
	     :documentation "Bring up a Lisp interaction pane")
	    ("Flush Typeout" :value :flush-typeout
	     :documentation "Clear typeout from the lower pane")
	    ("HELP" :value :help
	     :documentation "More information about this menu and its options")
	    ("QUIT" :value :quit
	     :documentation "Exit the File Sytem Maintenance program")))

)

; From file FSMAINT.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defmethod (fsmaint-frame :execute) (key)
  (selectq key
    (:lisp-window         (or (memq  state '(:lisp :wizard))
			      (if lmfs-wizardp
				  (funcall-self ':ensure-state ':wizard)
				  (funcall-self ':ensure-state ':lisp))))
    (:actedit             (funcall afse-pane ':set-hierarchy *the-root* *the-partition*)
			  (funcall-self ':ensure-state ':afse))
    (:tree-edit-root      (funcall-self ':hiered-root si:local-host))
    (:tree-edit-homedir-root (funcall-self ':hiered-root
					   (funcall (fs:user-homedir) ':host)))
    (:tree-edit-any-root   (let ((what
				  (zwei:typein-line-readline-near-window
				    ':mouse "Name of host to edit")))
			    (if (not (memq what '(nil t)))
				(let ((parsed (si:parse-host (string-trim " " what) t)))
				  (if (null parsed)
				      (format t "~&Invalid host: ~A" what)
				      (funcall-self ':hiered-root parsed))))))
    (:tree-edit-any       (if
			    (funcall hiered-pane ':demand-tree-edit
				     (tv:tree-edit-read-local-path nil
				       "Full directory path to edit"))
			    (funcall-self ':ensure-state ':hiered)))
    (:tree-edit-homedir   (if
			    (funcall hiered-pane ':demand-tree-edit (fs:user-homedir))
			    (funcall-self ':ensure-state ':hiered)))
    (:tree-edit-local-homedir
                          (if (funcall hiered-pane ':demand-tree-edit
				       (fs:user-homedir si:local-host))
			      (funcall-self ':ensure-state ':hiered)))
    (:complete-dump       (lmfs:backup-dumper ':dump-type ':complete))
    (:incremental-dump    (lmfs:backup-dumper ':dump-type ':incremental))
    (:print-disk-label    (print-disk-label))
    (:print-disk-label-any  (let ((what
				    (zwei:typein-line-readline-near-window
				      ':mouse "Host name or drive number")))
			      (cond ((memq what '(nil t)))	;punt
				    ((or (string-equal (setq what  (string-trim " " what))
						       "cc")
					 (si:parse-host what t)
					 (let ((num (parse-number what)))
					   (and num (setq what num))))
				     (print-disk-label what))
				    (t (format t "~&Not a known host or number: ~A" what)))))
    (:print-loaded-band   (print-loaded-band))
    (:initialize          (if (eq state ':hiered) (funcall-self ':ensure-state ':wizard))
			  (fsmaint-fs-initialize))
    (:flush-free-buffer   (lmfs:with-fs-locked (lmfs:shutwarm)))
    (:free-records        (lmfs:with-fs-locked
			    (lmfs:free-record-status)))
    (:salvage             (funcall-self ':ensure-state ':lisp)
			  (let ((orphs (yes-or-no-p
					 "Do you wish orphan files to be sought and repatriated?
 (Answering /"yes/" will substantially increase salvage time)")))
			    (lmfs:with-fs-locked
			      (lmfs:bitsalv-part lmfs:*the-partition* orphs))))

    (:wizard		  (funcall-self ':ensure-state ':lisp)
			  (if (or lmfs-wizardp
				  (yes-or-no-p
				    "These operations can potentially damage the file system.
Do you really know what you are doing and
take responsibility for what you are about to do? "))
			      (progn
				(setq lmfs-wizardp t)
				(funcall-self ':ensure-state ':wizard))))
    (:close-all-files     (fs:close-all-files))
    (:reload              (funcall-self ':ensure-state ':lisp)
			  (lmfs:reloader))
    (:help		  (fsmaint-help-spiel))
    (:flush-typeout       (selectq state
			    (:hiered
			     (funcall (funcall hiered-pane ':typeout-window) ':make-complete))
			    (:afse
			     (funcall (funcall afse-pane ':typeout-window) ':make-complete))
			    (t (funcall lisp-pane ':clear-screen))))
    (:quit		  (funcall-self ':bury))
    (t                    (tv:beep)))		;unimplementeds beep
  (if (memq state '(:afse :hiered))
      (tv:tree-edit-end-typeout)
      (funcall terminal-io ':fresh-line)))
(funcall (find-fsmaint-frame 'command-menu-window) ':set-item-list fsmaint-global-menu-alist)
)
