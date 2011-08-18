;;; <LMFS>DEFSTORAGE.LSP;108 28-May-81 09:44:11, Edit by BSG -*-Mode:LISP;package:lmfs-*-
;;; <LMFS>DEFSTORAGE.LSP;90 25-Mar-81 20:08:06, Edit by BSG -*-LISP-*-
;;;<LMFS>DEFSTORAGE.LSP;51 20-Mar-81 17:02:06, Edit by BSG -*-LISP-*-
;;;<LMFS>DEFSTORAGE.LSP;1 16-Mar-81 13:23:20, Edit by BSG -*-LISP-*-
;;;

;;; A PL/I like storage-laying-out facility
;;; Kind of a half-breed of defstorage and ifd's "->" ..


;;; Copyright (c) Symbolics, Inc., 1981



;;; 3/23 version 59 - uses indirect 8bit arrays for everything.

;;; Yet to be done --
;;;   addr
;;;   settle once-only issue
;;;   variable precision specs

#M(declare (macros t)					;maclisp baloney
	   (special ***mret-value*** error-output)
	   (*lexpr values))

#M (eval-when (compile eval)(or (boundp 'error-output) (setq error-output t)))

#Q (defun array-string-get (array start end)
     (let ((answer (make-array (- end start) ':type 'art-string)))
       (copy-array-portion array start end answer 0 (- end start))
       answer))

#Q (defmacro substring-store (val array start end+1 len)
     `(copy-array-portion ,val 0 ,len ,array ,start ,end+1))

(defconst *DFST-AREF* '#Q aref #M sim-vector-aref)
(defconst *DFST-ASET* '#Q aset #M sim-vector-aset)
(defconst *DEFAULT-DEFSTORAGE-TYPE* 'fixnum)
(defconst *DEFAULT-CHAR-LENGTH* 4)
(defconst *BITS-PER-WORD* 32.)
(defconst *BITS-PER-CHAR* 8.)
(defconst *CHARS-PER-WORD* 4.)
(defconst *BYTES-PER-WORD* 4.)
(defconst *DEFAULT-FIXNUM-BYTES* 4.)
(defconst *BITS-PER-BYTE* 8.)
(defconst *DEFAULT-BIT-LENGTH* 1)

(defvar defstorage-subscripter-stack nil)

;;; Storage-layout facility

(defmacro defstorage (namespec . components)
  (defstorage1 (if (atom namespec)
		 namespec
		 (car namespec))
	 (cons nil				;DISEMBODIED PLIST
	       (if (atom namespec)
		   nil
		   (cdr namespec)))
	 components))

;;; Work-distributor for defstorage.  Returns a progn 'compile
;;; of all macros to be defined, and appropriate evalwhens.

(defun defstorage1 (structure-name plist components)
  (putprop structure-name (list plist components) 'defstorage-def)
  (let ((word 0) (bit 0) (to-be-compiled nil)
		 (defstorage-subscripter-stack nil))
    (multiple-value (word bit to-be-compiled)
      (defstorage-include structure-name components word bit))
    `(progn
       'compile
       (eval-when (compile load eval)
	 (putprop ',structure-name
		  ,(// (+ bit -1 *BITS-PER-BYTE* (* word *BITS-PER-WORD*))
		      *BITS-PER-BYTE*)
		  'structure-size-in-bytes))
       (defmacro ,(intern (format nil "~S-SIZE-IN-BITS" structure-name)) ()
	 `(progn ,',(+ bit (* word *BITS-PER-WORD*))))
       (defmacro ,(intern (format nil "~S-SIZE-IN-WORDS" structure-name)) ()
	 `(progn ,',(defstorage-align 'word word bit)))	;counting on 1st val.
       . ,to-be-compiled)))

;;; Compile  a given structure, perhaps as itself, or perhaps
;;; as part of another structure.

(defun defstorage-include (structure-name components word bit)
  (let ((to-be-compiled nil))
    (dolist (component components)
      (if (atom component)
	  (setq component `(,component ,*DEFAULT-DEFSTORAGE-TYPE*)))
      (let ((component-name (car component))
	    (component-type (or (cadr component) *DEFAULT-DEFSTORAGE-TYPE*))
	    (component-spec (cddr component))
	    (temp-tbc))
	(multiple-value (word bit temp-tbc)
	  (defstorage-process-element structure-name component-name
	    component-type component-spec word bit))
	(setq to-be-compiled (nconc to-be-compiled temp-tbc))))
    (values word bit to-be-compiled)))


;;; Compile one given element.  Handle accessor naming, inclusion
;;; of other structures, inclusion, and included-being.

(defun defstorage-process-element (structure name type spec word bit)
  (let ((handler (get type 'defstorage-processor))
	(to-be-compiled nil)
	(accessor (intern (format nil "~A-~A" structure name))))

    (if (null handler)
	(let ((sdef (get type 'defstorage-def)))
	  (if (null sdef)
	      (format error-output "~%defstorage: No data type handler for ~S: ~S" type accessor)
	      (let* ((components (second sdef))
		     (internal (first components)))
		(if (= (length components) 1)
		    (multiple-value (word bit to-be-compiled)
		      (defstorage-process-element structure name
			(cadr internal) (cddr internal) word bit))
		    (multiple-value (word bit to-be-compiled)
		      (defstorage-include accessor components word bit))))))
	(multiple-value (word bit to-be-compiled)
	  (funcall handler
		   (if (eq type 'array) name accessor)
		   structure spec word bit)))
    (if (and (>= (string-length name) 6)
	     (string-equal "ignore" (substring name 0 6)))
	(setq to-be-compiled nil))

    (values word bit to-be-compiled)))

;;;  Utilities of all handlers.


;;;  Align next allocation to some boundary.
;;;  Sort of a write-around to defstorage-add-units..

(defun defstorage-align (to-what word bit)
  (multiple-value-bind (w b)
     (defstorage-add-units 0 to-what word bit)
    (values w b)))

;;; Allocate storage by adding units.


(defun defstorage-add-units (how-many what word bit)
  (let* ((mods (if (numberp what)
		   what
		   (selectq what
		     (word	  *BITS-PER-WORD*)
		     ((byte char) *BITS-PER-CHAR*)
		     (bit	  1)
		     (t  (format error-output "~% defstorage: obscure units: ~S, words used." what)
			 *BITS-PER-WORD*))))
	 (gross-bits (* mods (+ how-many
				(// (+  (* word *BITS-PER-WORD*)
					bit mods -1)   ;align to mods, round up
				    mods)))))
    (values (//  gross-bits *BITS-PER-WORD*)		;word
	    (\   gross-bits *BITS-PER-WORD*))))		;bit



;;;
;;;	Actual handlers for different types
;;;

(defun (char-with-length defstorage-processor) (accessor ignore spec word bit)
  ignore
  (let ((length (or (car spec) *DEFAULT-CHAR-LENGTH*))
	(length-accessor (intern (format nil "~A-LENGTH" accessor)))
	(index-offset)
	(compare-accessor (intern (format nil "~A-COMPARE" accessor)))
	(ml-macro (intern (format nil "~A-MAX-LENGTH" accessor)))
	(to-be-compiled nil))

;;; Compile length-accessor recursively.

    (multiple-value  (word bit)
      (defstorage-align 16. word bit))

    (multiple-value  (word bit to-be-compiled)
      (defstorage-process-element
	accessor "LENGTH" 'fixnum-bytes '(2) word bit))

    (setq index-offset (// (+ bit (* word *BITS-PER-WORD*)) *BITS-PER-CHAR*))

    (push
     `(defmacro ,ml-macro () `(progn ,',length))
     to-be-compiled)

    (push
     `(defmacro ,compare-accessor (8array value)
	`(let ((val ,value)
	       (8ary ,8array))
	   (let ((datl (,',length-accessor 8ary)))
	     (and 
	      (= (string-length val) datl)
	      #Q(%string-equal 8ary ,',index-offset val 0 datl)
	      #M(array-string-compare 8ary ,',index-offset (+ datl ,',index-offset) val)))))
     to-be-compiled)

    (push
     `(defmacro ,accessor (8array)
	`(array-string-get
	     ,8array ,,index-offset (+ (,',length-accessor ,8array) ,,index-offset)))
     to-be-compiled)

    (push
     `(defsetf ,accessor (8array) v
	`(let* ((val ,v)
		(v-stringlen (string-length val)))
	   (substring-store val ,8array ,,index-offset (+ ,,index-offset  v-stringlen)
			    #Q v-stringlen)
	   (setf (,',length-accessor ,8array) v-stringlen)))
     to-be-compiled)

    (multiple-value  (word bit)
      (defstorage-add-units length 'char word bit))

    (values word bit to-be-compiled)))

(defun (char defstorage-processor)  (accessor ignore spec word bit)
  ignore
  (let ((length (or (car spec) *DEFAULT-CHAR-LENGTH*))
	(index-offset)
	(compare-accessor (intern (format nil "~A-COMPARE" accessor)))
	(to-be-compiled nil))

    (multiple-value (word bit)
      (defstorage-align 'byte word bit))

    (setq index-offset (// (+ bit (* word *BITS-PER-WORD*))
			   *BITS-PER-CHAR*))

    (push
     `(defmacro ,accessor (8array)
	`(array-string-get
	     ,8array ,,index-offset ,,(+ index-offset length)))
     to-be-compiled)

    (push
     `(defmacro ,compare-accessor (8array value)
	#Q`(%string-equal ,8array ,',index-offset ,value 0 ,',length)
	#M`(array-string-compare ,8array ,,index-offset (+ ,,length ,,index-offset) ,value))
     to-be-compiled)

    (push
     `(defsetf ,accessor (8array) v
	`(substring-store ,v ,8array ,,index-offset ,,(+ index-offset length) #Q ,',length))
     to-be-compiled)

    (multiple-value  (word bit)
      (defstorage-add-units length 'char word bit))

    (values word bit to-be-compiled)))

(defun (fixnum defstorage-processor)  (accessor ignore spec word bit)
  ignore spec
  (let ((to-be-compiled nil))
    (multiple-value  (word bit)
      (defstorage-align 'word word bit))

    (multiple-value (word bit to-be-compiled)
      (funcall (get 'fixnum-bytes 'defstorage-processor)
	       accessor nil '(4) word bit))

    (values word bit to-be-compiled)))

(defun (fixnum-bytes defstorage-processor) (accessor ignore spec word bit)
  ignore
  (let ((to-be-compiled nil)
	(length (or (car spec) *DEFAULT-FIXNUM-BYTES*)))
    (cond ((or (not (fixp length))
	       (< length 1)
	       (> length 8.))
	   (format error-output "~%Invalid fixnum length: ~S" length))
	    (t
	     (multiple-value  (word bit)
	       (defstorage-align 'byte word bit))
	     (let ((index-offset (// (+ bit (* word *BITS-PER-WORD*))
				     *BITS-PER-CHAR*)))
	       (do ((ixo index-offset (1+ ixo))
		    ;; make obj code slightly less gross.. 
		    (cixo (if (null defstorage-subscripter-stack)
			      index-offset
			      `variable-index-offset)
			  (if (numberp cixo)
			      (1+ ixo)
			      `(+ ,ctr variable-index-offset)))
		    (ppss #o0010 (+ #o1000 ppss))
		    (length length (1- length))
		    (ctr 1 (1+ ctr))
		    (ldbr nil)
		    (dpbr nil)
		    (vgen (if (not (null defstorage-subscripter-stack))
			      (if (zerop index-offset)
				  ``((variable-index-offset
				      ,,(car defstorage-subscripter-stack)))
				  ``((variable-index-offset
				      (+ ,',index-offset
					 ,,(car defstorage-subscripter-stack))))))))
		   ((= length 0)
		    (push
		     `(defmacro ,accessor (8ary . subs)
			subs				;if dont get use
			`(let ((8ary1 ,8ary)
			       ,@,vgen
			       ) ,',dpbr))
		     to-be-compiled)
		    (push
		     `(defsetf ,accessor (8ary . subs) v
			subs
			`(let ((8ary1 ,8ary)
			       ,@,vgen
			       (val ,v))
			   . ,',ldbr))
		     to-be-compiled))
		 (push `(,*DFST-ASET* (ldb ,ppss val) 8ary1 ,cixo) ldbr)
		 (setq dpbr
		       (if (null dpbr)
			   `(,*DFST-AREF* 8ary1 ,cixo)
			   `(dpb (,*DFST-AREF* 8ary1 ,cixo) ,ppss ,dpbr)))))))

    (multiple-value (word bit)
      (defstorage-add-units length 'byte word bit))

    (values word bit to-be-compiled)))



;;;needs major rework in light of v.59 paradigm


(defun (bit defstorage-processor)  (accessor ignore spec word bit)
  ignore
  (let ((to-be-compiled nil)
	(bit-length (or (car spec) *DEFAULT-BIT-LENGTH*)))
    (if (> (+ bit-length bit) *BITS-PER-WORD*)
	(multiple-value (word bit ignore)
	  (defstorage-process-element
	    accessor ignore 'mod '(word) word bit)))
    (let ((ppss (+ (lsh bit 6) bit-length)))

      (push
       `(defmacro ,accessor (ptr)
	  `(ldb (get-32-fixnum-32** ,ptr ,,word) ,,ppss))
       to-be-compiled)

      (push
       `(defsetf ,accessor (ptr) v
	  `(put-32-fixnum-32** ,ptr
			       (dpb v ,,ppss (,',accessor ,ptr))
			       ,,word))
       to-be-compiled))

      (multiple-value (word bit)
         (defstorage-add-units bit-length 'bit word bit))

      (values word bit to-be-compiled)))

(defun (flag defstorage-processor) (accessor ignore1 ignore2 word bit)
  ignore1 ignore2
  (let ((to-be-compiled nil)
	(xbit (\  bit 8.))
	(index-offset (// (+ bit (* word *BITS-PER-WORD*))
			  *BITS-PER-CHAR*)))

    (push
     `(defmacro ,accessor (8ary)
	`(bit-test ,,(ash 1 xbit)
		   (,',*DFST-AREF* ,8ary ,,index-offset)))
     to-be-compiled)

    (push
     `(defsetf ,accessor (8ary) v
	`(,',*DFST-ASET*
	  ,(let ((byteget `(,',*DFST-AREF* ,8ary ,,index-offset)))
	     (if (defstorage-conboolable* v)
		 (defstorage-conboole* v ,xbit byteget)
		 `(dpb (if ,v 1 0)
		       ,,(+ (lsh xbit 6) 1)
		       ,byteget)))
	  ,8ary
	  ,,index-offset))
     to-be-compiled)
    (multiple-value (word bit)
      (defstorage-add-units 1 'bit word bit))
    (values word bit to-be-compiled)))


(defun (array defstorage-processor) (accessor structure spec word bit &aux tbc ig1 ig2)
  (let* ((dims (car spec))
	 (type (cdr spec))
	 (eltsize (defstorage-get-array-element-size type)))
    (cond ((null eltsize)
	   (format error-output "~%defstorage: invalid//undefined array type: ~A ~S"
		   accessor type))
	  ((or (atom dims)
	       (dolist (x dims) (or (and (fixp x) (> x 0))
				    (eq x '*)
				    (return t))))
	   (format error-output "~%defstorage: invalid array dims: ~S - ~A" dims accessor))
	  ((memq '* (cdr dims))
	   (format error-output "%~defstorage: invalid context for boundless array: ~A" accessor))
	  (t
	   (multiple-value (word bit) (defstorage-align 'word word bit))

	   (let* ((defstorage-subscripter-stack
		    (cons (compute-defstorage-subscripter-stack eltsize dims)
			  defstorage-subscripter-stack)))
	     (multiple-value (ig1 ig2 tbc)
	       (defstorage-process-element structure accessor (car type) (cdr type) word bit))
	     (if (not (memq '* dims))
		 (multiple-value (word bit)
		   (defstorage-add-units
		     (* eltsize (apply '* dims)) 'byte word bit))))))
    (values word bit tbc)))


(defun defstorage-get-array-element-size (type)
  (cond ((equal type '(fixnum)) 4)
	((eq (car type) 'fixnum-bytes) (cadr type))
	((get (car type) 'structure-size-in-bytes))
	(t nil)))

(defun compute-defstorage-subscripter-stack (eltsize dims)
  (let ((inner (do ((dims (cdr dims) (cdr dims))
		    (answer '(pop subs)
			    ``(+ (* ,,answer ,',(car dims)) ,(pop subs))))
		   ((null dims) answer))))
    (let* ((tem (if (= eltsize 1) inner ``(* ,,inner ,',eltsize))))	;somday
      (if defstorage-subscripter-stack
	  ``(+ ,,(car defstorage-subscripter-stack) ,,tem)
	  tem))))

(defun (mod defstorage-processor)  (accessor ignore spec word bit)
   accessor
   (if (memq (car spec) '(word bit byte char))
       (multiple-value (word bit)
	 (defstorage-align (car spec) word bit))
       (format error-output "~%defstorage: invalid mod spec: ~S" spec))	   
   (values word bit nil))

(defun (being defstorage-processor)  (accessor sname spec word bit)
  accessor
  (let ((to-be-compiled nil)
	(temp-tbc)
	(type (car spec))
	(specs (cdr spec)))

    (if (atom type)
	(setq spec nil)
	(psetq type (car type) spec (cdr type)))

    (dolist (member specs)
      (multiple-value (word bit temp-tbc)
	(defstorage-process-element sname member type spec word bit))

      (setq to-be-compiled (nconc to-be-compiled temp-tbc)))

    (values word bit to-be-compiled)))

;;;----------------------------------------------------------------------


;;; macro time, not macro-macro time fcns

(defun defstorage-conboolable* (v)
  (cond ((memq v '(t nil)) t)
	((atom v) nil)
	((atom (cdr v)) nil)
	((memq (cadr v) '(t nil)) t)
	(t nil)))

(defun defstorage-conboole* (val bitno wordgetter)
  (let ((mask (ash 1 bitno))
	(tornil (if (atom val)
		    val
		    (cadr val)))
	(boole 1))
    (if tornil
	(setq boole 7)
	(setq mask (boole 6 -1 mask)))
    `(boole ,boole ,mask ,wordgetter)))

;;;----------------------------------------------------------------------


;;;  A TOOL

(defun defstorage-display (sname)
  (let ((def (get sname 'defstorage-def)))
    (if (null def) (ferror nil "Not a known structure: ~S" sname))
    (defstorage-display-recurse sname def))
  (defstorage-display-sym1
    (get (intern (format nil "~A-~A" sname "SIZE-IN-WORDS")) 'macro)
    (subst nil nil '((x)))
    ()
    "size in words"
    sname))

(defun defstorage-display-recurse (sname def)
  (let ((components (second def)))
    (dolist (component components)
      (let* ((name (first component))
	     (accessor (intern (format nil "~A-~A" sname name))))
	(if (getl accessor `(macro setf))
	    (defstorage-display-sym accessor)
	    (let ((type (second component)))
	      (if (eq type 'being)
		  (progn
		    (setq type (let ((type (third component)))
				 (if (atom type) (list type) type)))
		    (defstorage-display-recurse
		      sname
		      `(,(first def)
			,(mapcar #'(lambda (x) `(,x . ,type)) (cdddr component)))))
		  (if (string-equal (substring name 0 6) "ignore")
		      nil
		      (let ((internal-def
			     (or (get (second component) 'defstorage-def)
				 (ferror nil "No determinable info: ~S" component))))
			(defstorage-display-recurse accessor internal-def))))))))))

(defun defstorage-display-sym (sym)
  (defstorage-display-sym1
    (get sym 'macro)
    (subst nil nil '((x array-of-8s)))
    '(array-of-8s)
    "get" sym)
  (defstorage-display-sym1
    (get sym 'setf)
    (subst nil nil '((x array-of-8s) value))
    '(array-of-8s value)
    "set" sym))

(defun defstorage-display-sym1 (fcn args argl what sym)
  (format t "~%~%~A: ~A" sym what)
  (let ((random (intern (format nil "*-~A-~A" what sym))))
    (eval
     `(defun ,random ,argl ,(apply fcn args)))
    (eval `(grindef ,random))))




;;;----------------------------------------------------------------------

(sstatus feature defstorage)
