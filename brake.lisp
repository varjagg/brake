(in-package #:brake)

(defclass brake-record ()
  ((state :initform -1
	  :accessor state
	  :type integer)
   (enabled-p :initform t
	      :accessor enabled-p)
   (tracing-p :initform nil
	      :accessor tracing-p)
   (brake-points :accessor brake-points
		 :initform '())))

(defparameter *brake-records* (make-hash-table))

(defun add-brake-record (tag step)
  (check-type step (integer 0 *) "A positive integer")
  (let ((record (or (gethash tag *brake-records*)
		    (setf (gethash tag *brake-records*)
			  (make-instance 'brake-record)))))
    (unless (find step (brake-points record))
      (setf (brake-points record)
	    (merge 'list (list step)
		   (brake-points record)
		   #'<)))
    record))

(defun get-record (tag)
  (gethash tag *brake-records*))

(defmacro brake (&optional tag-or-sexp step sexp)
  (check-type step (or (satisfies null) (integer 0 *)) "An integer >= 0")
  (let ((result (gensym "BRK-RES"))
	(record (gensym "BRK"))
	(prev-state (gensym "BRK"))
	(tail (gensym "BRK"))
	(subtail (gensym "BRK")))
    (when (and tag-or-sexp (listp tag-or-sexp))
      (setf sexp tag-or-sexp))
    `(let ((,result (multiple-value-list ,sexp)))
       ,(if tag-or-sexp
	    (if (keywordp tag-or-sexp)
		(progn
		  (add-brake-record tag-or-sexp step)
		  `(let ((,record (get-record ,tag-or-sexp)))
		     (unless ,record
		       (error "No record found for breakpoint with tag ~a" ,tag-or-sexp))
		     (when (enabled-p ,record)
		       (let* ((,prev-state (state ,record))
			      (,tail (member (state ,record) (brake-points ,record)))
			      (,subtail (member ,step ,tail)))
			 ;; right after current
			 (unwind-protect
			      (when (or (and (= (state ,record) -1)
					     (= ,step (first (brake-points ,record))))
					(= ,prev-state ,step)
					(and (cdr ,tail) (eql (cdr ,tail) ,subtail)))
				(when (tracing-p ,record)
				  (format *trace-output* "~&~vaTag ~s/~d ~:[~;values ~s~]"
					  (position ,step (brake-points ,record))
					  #\Space
					  ,tag-or-sexp ,step (car ,result) ,result))
				(break "Breaking at tag ~s step ~d" ,tag-or-sexp ,step)
				(setf (state ,record) ,step))
			   ;; reset state if user aborts from BREAK or after last break
			   (unless (or (minusp ,prev-state)
				       (and (eql (state ,record) ,step)
					    (cdr ,subtail)))
			     (setf (state ,record) -1)))))))
		'(break))
	    '(break))
       (values-list ,result))))

(defmacro brake-when (condition &optional tag-or-sexp step sexp)
  `(if (and (if (keywordp ,tag-or-sexp)
			   (enabled-p (get-record ,tag-or-sexp))
			   t)
	    ,condition)
       (brake ,tag-or-sexp ,step ,sexp)
       ,(if (and tag-or-sexp (listp tag-or-sexp))
	   tag-or-sexp
	   sexp)))

(defmacro break-when (&rest args)
  `(brake-when ,@args))

(defmacro mark (tag step &optional sexp)
  (check-type step (integer 0 *) "An integer >= 0")
  (let ((result (gensym "BRK-RES"))
	(record (gensym "BRK"))
	(prev-state (gensym "BRK"))
	(tail (gensym "BRK"))
	(subtail (gensym "BRK")))
    `(let ((,result (multiple-value-list ,sexp)))
       ,(progn
	  (add-brake-record tag step)
	  `(let ((,record (get-record ,tag)))
	     (unless ,record
	       (error "No record found for mark with tag ~a" ,tag))
	     (when (enabled-p ,record)
	       (let* ((,prev-state (state ,record))
		      (,tail (member (state ,record) (brake-points ,record)))
		      (,subtail (member ,step ,tail)))
		 ;; right after current
		 (when (or (and (= (state ,record) -1)
				(= ,step (first (brake-points ,record))))
			   (= ,prev-state ,step)
			   (and (cdr ,tail) (eql (cdr ,tail) ,subtail)))
		   (when (tracing-p ,record)
		     (format *trace-output* "~&~vaTag ~s/~d ~:[~;values ~s~]"
			     (position ,step (brake-points ,record))
			     #\Space
			     ,tag ,step (car ,result) ,result))
		   (setf (state ,record) ,step))
		 (unless (or (minusp ,prev-state)
			     (cdr ,subtail))
		   (setf (state ,record) -1))))))
       (values-list ,result))))

(defmacro mark-when (condition &optional tag-or-sexp step sexp)
  `(if (and (if (keywordp ,tag-or-sexp)
		(enabled-p (get-record ,tag-or-sexp))
		t)
	    ,condition)
       (mark ,tag-or-sexp ,step ,sexp)
       ,(if (and tag-or-sexp (listp tag-or-sexp))
	    tag-or-sexp
	    sexp)))

(defmacro operate-brake (tag &rest parameter-pairs)
  (let* ((record (gensym))
	 (body (loop for (slot value) in parameter-pairs
		     appending (list (list slot record) value))))
    `(let ((,record (gethash ,tag *brake-records*)))
       (if ,record
	   (setf ,@body)
	   (warn "No record of breakpoints with tag ~a" ,tag)))))

(defmacro operate-brakes (tags &rest operation-expressions)
  `(progn
     (dolist (tag ,tags)
       (check-type tag keyword "A keyword"))
     (dolist (tag ,tags)
       (operate-brake tag ,@operation-expressions))))

(defun brake-disable (&rest tags)
  (unless tags
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (push k tags))
	     *brake-records*))
  (operate-brakes tags (enabled-p nil)))

(defun brake-enable (&rest tags)
  (unless tags
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (push k tags))
	     *brake-records*))
  (operate-brakes tags (enabled-p t)))

(defun brake-reset (tag)
  (operate-brake tag (enabled-p t) (tracing-p nil) (state -1)))

(defun brake-trace (tag &rest tags)
  (operate-brakes (cons tag tags) (tracing-p t))
  t)

(defun brake-untrace (&rest tags)
  (unless tags
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (push k tags))
	     *brake-records*))
  (operate-brakes tags (tracing-p nil)))

(defun clear-brake-points ()
  (clrhash *brake-records*))

(defun clear-brake-tag (tag)
  (remhash tag *brake-records*))

(defun report-brakes ()
  (maphash #'(lambda (tag record)
	       (format t "~&Tag ~s is ~:[DISABLED~;ENABLED~]~:[~;, traced,~] with ~d defined step~:p, current state is ~:[~d~;initial~]~%"
		       tag (enabled-p record) (tracing-p record) (length (brake-points record)) (minusp (state record)) (state record)))
	   *brake-records*))
