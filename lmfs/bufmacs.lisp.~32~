;;; <LMFS>BUFMACS.LSP;1  1-Apr-81 15:43:17, Edit by BSG -*-package:lmfs;mode:LISP-*-

;;; Copyright (c) Symbolics, Inc., 1981


;;; Buffers contain records, not blocks.
;;; These macros encase code that wants to address data in buffers.
;;; In general, they provide for reference-counting the buffers, and
;;; dereferencing (?!) them when done.  The user has to explicitly
;;; upreference things he doesnt want vanished at the end of the
;;; macros.  The buffers never get reused while referencecounted.
;;; They will, however, be added to a free pool, and preferenced for
;;; replacement within the same file if the refcount goes zero.  When
;;; an attempt is made to address data in a file, such a buffer will
;;; be sought.

;;; Modification strategy: good-p is set appropriately when you
;;; start inconsistentizing a buffer.  It is considered an error
;;; to dereference (to zero) a buffer with good-p not on.  You
;;; have to have written it out. Anything you write out gets declared
;;; good by context and implication.  Perhaps good-p is simply ^buphm?
;;; We'll see what happens.


#M (eval-when (compile eval)
     (load 'fs-dcls))


#M (defmacro fs-copy-array-portion rest `(simvector-copy-array-portion . ,rest))
#Q (defmacro fs-copy-array-portion rest `(copy-array-portion . ,rest))

#M (defmacro defunp (spec argl . body)
     `(defun ,spec ,argl
	(prog nil
	      ,@(reverse (cdr (reverse body)))
	      (return ,(car (last body))))))

#M (and (status feature complr)
	(*expr get-general-file-addressor
	       set-bufmod-from-addressor
	       upreference-addressor
	       downreference-addressor
	       upreference-file-buffer
	       downreference-file-buffer))



(defmacro protect-buffer-addressor ((var val) &body body)
  `(let ((,var ,val))					;val c/b nil
     (unwind-protect
       (progn . ,body)
       (and ,var (downreference-addressor ,var)))))

(defmacro protect-buffer-addressor-modifying ((var) &body body)
  `(let ((,var nil))
     (unwind-protect
       (progn
	 (set-bufmod-from-addressor ,var)
	 . ,body)
       (and ,var (downreference-addressor ,var)))))

(defmacro with-filedata-addressibility ((filedesc word-addr var buf) &body body)
  (bufadrs-macros filedesc word-addr var buf body 'file nil))


(defmacro with-filedata-addressibility-modifying ((filedesc word-addr var buf) &body body)
  (bufadrs-macros filedesc word-addr var buf body 'file t))

(defmacro with-fileheader-addressibility ((filedesc word-addr var buf) &body body)
  (bufadrs-macros filedesc word-addr var buf body 'header nil))

(defmacro with-fileheader-addressibility-modifying ((filedesc word-addr var buf) &body body)
  (bufadrs-macros filedesc word-addr var buf body 'header t))


(defun bufadrs-macros (filedesc word-addr var buf body file-p mod-p)
  (if (not (symbolp var))
      (ferror nil "Invalid syntax for with-fileheader-addressibility-modifying: ~S not a var" var))

  buf							;obsolescent
    
  `(protect-buffer-addressor (,var)
     (setq ,var (get-general-file-addressor ,(eq file-p 'header)
					    ,filedesc ,word-addr))
     ,@ (and mod-p `((set-bufmod-from-addressor ,var)))
     . ,body))

