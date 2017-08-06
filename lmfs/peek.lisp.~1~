;;;-*- Mode:LISP; Package:TV; Base:8 -*-

(DEFMETHOD (FS:FILE-HOST-LISPM-MIXIN :PEEK-FILE-SYSTEM) ()
  (AND (EQ SELF SI:LOCAL-HOST)
       (LIST ()
	 (SCROLL-PARSE-ITEM "  Host local")
	 (SCROLL-MAINTAIN-LIST #'(LAMBDA ()
				   (FUNCALL WHO-LINE-FILE-STATE-SHEET ':OPEN-STREAMS))
			       #'(LAMBDA (STREAM)
				   (FUNCALL STREAM ':PEEK-FILE-SYSTEM 4))
			       NIL
			       #'(LAMBDA (LIST)
				   (DO ((L LIST (CDR L)))
				       ((NULL L) (VALUES NIL NIL T))
				     (AND (TYPEP (CAR L) 'LMFS:LMFS-OPENING-MIXIN)
					  (RETURN (VALUES (CAR L) (CDR L) (NULL (CDR L)))))))
			       ))))
