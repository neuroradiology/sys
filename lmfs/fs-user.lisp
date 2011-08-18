;-*- Mode:LISP; Package:FILE-SYSTEM-*-

;; Up to SYS 75

(defvar *local-server-via-net* nil)
(defflavor remote-lmfs-pathname () (lmfs:lmfs-pathname fs:chaos-pathname))

;;;host-lisp-mixin is defined for us in HOST >

(defflavor file-host-lispm-mixin () (file-host-mixin))

(defmethod (file-host-lispm-mixin :pathname-flavor) ()
  (cond ((neq self si:local-host) 'remote-lmfs-pathname)
	(*local-server-via-net* 'remote-lmfs-pathname)
	(t 'lmfs:local-lmfs-pathname)))

(defmethod (file-host-lispm-mixin :max-data-connections) () 99.)

;;; This is what sets si:local-host ----
(defmethod (file-host-lispm-mixin :after :set-site) (&rest ignore)
  ;; This is necessary since the initialization that adds this host comes much later.
  (setq fs:*chaos-file-hosts* (delq self fs:*chaos-file-hosts*))
  (if (memq chaos:my-address (funcall-self ':chaos-addresses))
      (setq si:local-host self)))

(defmethod (file-host-lispm-mixin :pathname-host-namep) (name)
  (and (string-equal name lmfs:*LOCAL-FS-HOST*)
       (eq self si:local-host)))

(defmethod (file-host-lispm-mixin :hsname-information) (ignore str idx)
  (let ((pathname
	  (if (null idx)
	      (fs:make-pathname ':host self ':device ':unspecific
				':directory (string-append lmfs:*path-delimiter* user-id)
				':name nil ':type nil ':version nil)
	      (fs:parse-pathname
		(substring str (setq idx (1+ idx)) (string-search-char #\CR str idx))
		self))))
    pathname user-personal-name user-group-affiliation user-personal-name-first-name-first))

(defflavor lispm-chaos-host () (si:host-lispm-mixin chaos:host-chaos-mixin
				file-host-lispm-mixin si:host))

(si:set-host-flavor-keywords 'lispm-chaos-host '(:lispm :chaos))

;;; This has been pretty much busted by the general concept of System 74.
(defun local-server-via-net (&optional (whichway t))
  (setq *local-server-via-net* whichway))

(defun establish-lispm-servers ()
  (dolist (server si:machine-location-alist)
    (fs:add-chaosnet-file-computer (car server)))
  (local-server-via-net nil))

(defun establish-local-lm-host ()
  (fs:add-chaosnet-file-computer si:local-host))

(add-initialization "LM File Servers" '(establish-lispm-servers) '(:site))
		    

(add-initialization "Local LM Host" '(establish-local-lm-host) '(:cold))

(compile-flavor-methods remote-lmfs-pathname lispm-chaos-host)
