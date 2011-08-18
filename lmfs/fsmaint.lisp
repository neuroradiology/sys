;-*- Mode:LISP; Package:LMFS-*-

(defflavor fsmaint-interaction-pane ()
	   (tv:dont-select-with-mouse-mixin	;THIS IS A ***CROCK***
	    tv:pane-mixin tv:preemptable-read-any-tyi-mixin
	    tv:autoexposing-more-mixin tv:window)
  (:DOCUMENTATION :COMBINATION "FSMAINT Lisp pane when needed."))


(defflavor fsmaint-frame
	   ((terminal-io terminal-io)
						;IS BOUND HEREIN
	    global-menu				;global options menu
	    lisp-pane				;when needed
	    wizard-menu				;when needed
	    hiered-pane				;pane for hierarchy editor
	    afse-pane				;pane for active fs edit
	    state)				;what's in lower window
  (tv:process-mixin tv:any-tyi-mixin tv:stream-mixin
		    tv:bordered-constraint-frame-with-shared-io-buffer
		    tv:select-mixin)
  (:default-init-plist :save-bits ':delayed)
  (:gettable-instance-variables state)
  (:documentation :special-purpose "File System Maintenance frame"))

(defmethod (fsmaint-frame :name-for-selection) ()
  (if (eq state ':random)
      "File System"
    (format nil "File System (~A)"(cdr (assq state
					     '((:hiered . "FS Edit")
					       (:afse . "FS Data analyze")
					       (:wizard . "Wizard Ops")
					       (:lisp . "Lisp")))))))

(defflavor fsmaint-hiered-pane ()
	   (tv:pane-mixin
	    tv:dont-select-with-mouse-mixin
	    tv:mousable-tree-scroll-mixin tv:basic-tree-scroll))

(defflavor fsmaint-afse-pane ()
	   (tv:pane-mixin
	    tv:dont-select-with-mouse-mixin
	    afse-mixin tv:basic-tree-scroll))

(defmethod (fsmaint-hiered-pane :edit-object) (tree)
  (funcall tree ':edit self))

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

	    ("Print Disk Label" :value :print-disk-label
	     :documentation "Print the disk label on drive 0")
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

(defconst fsmaint-wizard-menu-alist
	  '(("Initialize" :value :initialize
	     :documentation "Initialize the file partition")
	    ("Close All Files" :value :close-all-files
	     :documentation "Close all files, local and otherwise")
	    ("Reload" :value :reload
	     :documentation "Reload the file system from backup tapes")
	    ("Active structure edit" :value :actedit
	     :documentation "Edit the active file system data structure")))

(defmethod (fsmaint-frame :before :init) (&rest ignore)
  (setq tv:selected-pane 'lisp-window
	tv:process (or tv:process
		       '(fsmaint-top-level :special-pdl-size 4000
					   :regular-pdl-size 10000))
	tv:panes `((lisp-window fsmaint-interaction-pane
				:label (:string "Lisp Interaction Window"
						:font FONTS:TR12I))
		   (command-menu-window tv:command-menu-pane :item-list
					,fsmaint-global-menu-alist
					:label (:string "File System Operations"
						:font FONTS:TR12I))
		   (afse-window fsmaint-afse-pane
				:label (:string "Active FS Structure Edit"
						:font FONTS:TR12I))
		   (hiered-window fsmaint-hiered-pane
				  :label (:string "File System Editor"
						  :font FONTS:TR12I))
		   (wizard-window tv:command-menu-pane :item-list
				  ,fsmaint-wizard-menu-alist
				  :label (:string "Maintenance Operations"
					  :font FONTS:TR12I)))
	tv:constraints
                 `((:lisp
		     . ((command-menu-window lisp-window)
			((command-menu-window :ask :pane-size))
			((lisp-window :even))))
		   (:wizard
		     . ((command-menu-window wizard-window lisp-window)
			((command-menu-window :ask :pane-size)
			 (wizard-window :ask :pane-size))
			((lisp-window :even))))
		   (:hiered
		     . ((command-menu-window hiered-window)
			((command-menu-window :ask :pane-size))
			((hiered-window :even))))
		   (:afse
		     . ((command-menu-window afse-window)
			((command-menu-window :ask :pane-size))
			((afse-window :even)))))))
  
(defmethod (fsmaint-frame :after :init) (&rest ignore)
  (funcall-self ':set-configuration ':lisp)
  (setq state ':random)
  (setq global-menu (funcall-self ':get-pane 'command-menu-window))
  (setq wizard-menu (funcall-self ':get-pane 'wizard-window))
  (setq hiered-pane (funcall-self ':get-pane 'hiered-window))
  (setq afse-pane (funcall-self ':get-pane 'afse-window))
  (setq lisp-pane (funcall-self ':get-pane 'lisp-window)))


(defun fsmaint-top-level (window)		;terminal io bound as instance var...
  (funcall window ':fsmaint-top-level))

(defmethod (fsmaint-frame :fsmaint-top-level) ()
  (funcall-self ':set-configuration ':lisp)
  (do ((*)(**)(***)(+)(++)(+++)) (())
    (*catch
      'sys:command-level
      (funcall-self ':select-selected-pane)	; in case abort
      (if (not (memq state '(:hiered :afse)))
	  (funcall terminal-io ':fresh-line))
      (do () (())
	(if (memq state '(:afse :hiered))
	    (funcall-self ':interpret-input (funcall-self ':any-tyi))
	    (multiple-value-bind (special-char sexp)
		(eh:window-command-loop-read t)
	      (if special-char
		  (funcall-self ':interpret-input special-char)
		  (let ((ans (eval sexp)))
		    (funcall terminal-io ':fresh-line)
		    (prin1 ans)
		    (psetq *** **   ** *  * ans +++ ++  ++ + + sexp)
		    (funcall terminal-io ':fresh-line)))))))
    (funcall-self ':refresh)))

;;;This serves to deexpose the typeout pane if it was exposed at abort time.
;;; Lest we do this, there is no reason why it SHOULD go away

(defmethod (fsmaint-frame :select-selected-pane) ()
  (let ((tow (funcall afse-pane ':typeout-window)))
    (if (eq tv:selected-window tow)
	(funcall tow ':deexpose)))
  (let ((tow (funcall hiered-pane ':typeout-window)))
    (if (eq tv:selected-window tow)
	(funcall tow ':deexpose))))

;;; This is kind of silly. There should be some organized way of doing this.
;;; Probbly when we put this thing starting-up at load time, when the
;;; defresource problem is grokked.  If you don't do this, and SYS F
;;; before login, the system
;;; will wedge unfiguroutably trying to notify you to log in during window
;;; creation trying to get the font.  And suppose the host is down, anyway?
;;; Silly price to pay for italic font...

(funcall tv:default-screen ':parse-font-descriptor 'fonts:tr12i)


(defmethod (fsmaint-frame :interpret-input) (input)
  (let ((pane (funcall-self ':selected-pane)))
    (if (listp input)
	(selectq (first input)
	  (tv:tree-mouse (funcall pane ':tree-interpret-blip input))
	  (:menu	       (funcall-self ':execute (funcall (fourth input)
								':execute (second input))))))
    (tv:sheet-force-access (pane)
      (if (and (memq state '(:hiered :afse))
	       (not (eq pane lisp-pane)))	;profoundly silly, but happens during trans.
	  (funcall pane ':redisplay)))))

(tv:add-to-system-menu-programs-column "FS Maintenance" '(tv:select-or-create-window-of-flavor
						     'fsmaint-frame)
				       "The File System Maintenance program")

(add-initialization "FSM exl" '(push '(#/F
				       fsmaint-frame
				       "File system maintenance"
				       t) tv:*system-keys*)
		    '(:once))

(defvar lmfs-wizardp nil)

(defmethod (fsmaint-frame :after :set-configuration) (config &aux sel-pane)
  (setq state config)
  (cond ((boundp 'lisp-pane)
	 (selectq state
	   (:hiered     (setq sel-pane hiered-pane
			      terminal-io (funcall hiered-pane ':typeout-window)))
	   (:afse       (setq sel-pane afse-pane
			      terminal-io (funcall afse-pane ':typeout-window)))
	   (t           (setq sel-pane lisp-pane terminal-io lisp-pane)))
	 (funcall-self ':select-pane sel-pane))))

(defmethod (fsmaint-frame :ensure-state) (new-state &optional no-select)
  (cond ((not (eq new-state state))
	 (funcall-self ':set-configuration new-state)
	 (or no-select (funcall-self ':select)))))

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

(defmethod (fsmaint-frame :hiered-root) (host)
  (let ((hd-path (fs:user-homedir host)))
    (funcall hiered-pane ':set-tree
	     (make-instance 'tv:tree-list-root-topnode ':sample-path hd-path))
    (funcall-self ':ensure-state ':hiered)))

(defmethod (fsmaint-hiered-pane :demand-tree-edit) (path)
  (setq path (fsmaint-dir-verify path))
  (and path
       (progn
	 (funcall-self ':set-tree (make-instance 'tv:root-directory ':dir-in-dir-form path))
	 t)))					;indicate success

(defun fsmaint-dir-verify (path)
  (and path
       (progn
	 (if (not (memq (funcall path ':name) '(:wild :unspecific nil)))
	     (setq path (funcall path ':pathname-as-directory)))
	 (let* ((foopath
		  (funcall path ':new-pathname ':name "Ibblegribble" ':type
			   "Fribbledibble" ':version 1))
		(props (fs:file-properties foopath nil)))
	   (cond ((not (stringp props)) path)	;!!!
		 ((member (fs:file-process-error props nil "" t) '("DNF" "NSD"))
		  (format t "~& No such directory: ~A" path)
		  nil)
		 (t path))))))
			      
(defun fsmaint-help-spiel ()
  (princ "This is the file system maintenance program.  Choose an operation
from the menu at top.  There are special operations available to wizards,
which can be obtained by clicking on the /"Maintenance/" item.  This
will cause another menu to appear.  The lowest pane is either a lisp
interaction pane, to which forms may be typed, or an instance of the
File System Editor, as appropriate to the last operation selected.
When in the File System editor, you may mouse upon either the upper
menu or the files and directories.  There is wholine documentation
for each of the menu items, as well as in the File System Editor."))


;;; Fear and trembling implemented here.
;;;

(defun fsmaint-fs-initialize ()
  (format t "~&This is ~A." si:disk-pack-name)
  (if
    (and (if (si:find-disk-partition "FILE")
	     t
	     (progn
	       (format t "~&There is no FILE partition on this disk.")
	       nil))
	 (progn
	   (if (and (boundp 'lmfs:*the-root*) lmfs:*the-root*)
	       nil
	       (lmfs:fstart "FILE"))
	   (if lmfs:*the-root*
	       (yes-or-no-p
		 (format t "~&~%There appears to be an operative file system on ~A with
~D director~:@P inferior to its root.  Do you wish to destroy it? " si:disk-pack-name
			 (lmfs:with-fs-locked (length (lmfs:simple-list-directory
							lmfs:*the-root*)))))
	       t))
	 (yes-or-no-p
	   (format t
		   "~&~%Proceeding with this operation will completely and thoroughly destroy
the contents of the FILE partition, irretrievably and irreversibly.  There is no going
back once you have answered /"yes/" to this question.  Do you still wish to create an
empty file system on the FILE partition of ~A? " si:disk-pack-name)))
    (progn
      (format t "~&~%Proceeding with initialization of FILE partition on ~A."
	      si:disk-pack-name)
      (lmfs:with-fs-locked
	(lmfs:fs-file-part-init "FILE")
	(lmfs:create-root))
      (format t "~&Initialization complete."))
    (format t "~&Will not initialize FILE partition of ~A." si:disk-pack-name)))

(tv:make-window 'fsmaint-frame ':activate-p t)

(compile-flavor-methods fsmaint-interaction-pane fsmaint-hiered-pane
			fsmaint-afse-pane fsmaint-frame)
