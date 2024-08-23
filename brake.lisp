(in-package #:brake)

(defclass brake-record ()
  ((state :initform -1
	  :accessor state
	  :type integer)
   (enabled-p :initform t
	      :accessor enabled-p)
   (brake-points :accessor brake-points
		 :initform '())))

(defparameter *brake-records* (make-hash-table))

(defmethod add-brake-record (tag step)
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

(defmacro brake (&optional tag-or-sexp step sexp)
  (check-type step (or (satisfies null) (integer 0 *)) "An integer >= 0")
  (let ((result (gensym "BRK-RES"))
	(record (gensym "BRK"))
	(tail (gensym "BRK"))
	(subtail (gensym "BRK")))
    (when (and tag-or-sexp (listp tag-or-sexp))
      (setf sexp tag-or-sexp))
    `(let ((,result (progn ,sexp)))
       ,(if tag-or-sexp
	    (if (keywordp tag-or-sexp)
		(progn
		  (add-brake-record tag-or-sexp step)
		  `(let ((,record (gethash ,tag-or-sexp ,*brake-records*)))
		     (unless ,record
		       (error "No record found for breakpoing with tag ~a" ,tag-or-sexp))
		     (when (enabled-p ,record)
		       (let* ((,tail (member (state ,record) (brake-points ,record)))
			      (,subtail (member ,step ,tail)))
			 ;; right after current
			 (unwind-protect
			      (when (eql (cdr ,tail) ,subtail)
				(break "Breaking at tag ~s step ~d" ,tag-or-sexp ,step)
				(setf (state ,record) ,step))
			   ;; reset state if user aborts from BREAK or after last break
			   (unless (and (eql (state ,record) ,step)
					(print (cdr ,subtail)))
			     (setf (state ,record) -1)))))))
		'(break))
	    '(break))
       ,result)))

(defmacro brake-when (predicate &optional tag step sexp)
  )

(defmacro break-when (&rest args)
  `(brake-when ,@args))

(defun brake-disable (tag)
  (let ((record (gethash tag *brake-records*)))
    (if record
	(setf (enabled-p record) nil)
	(warn "No record of breakpoints with tag ~a" tag))))

(defun brake-enable (tag)
  (let ((record (gethash tag *brake-records*)))
    (if record
	(setf (enabled-p record) t)
	(warn "No record of breakpoints with tag ~a" tag))))

(defun reset-brake-points ()
  (clrhash *brake-records*))
