(in-package #:brake)

(defclass brake-record ()
  ((state :initform 0
	  :accessor state
	  :type integer)
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
		     (let* ((,tail (member (state ,record) (brake-points ,record)))
			    (,subtail (member ,step ,tail)))
		       ;; right after current
		       (unwind-protect
			    (when (eql (cdr ,tail) ,subtail)
			      (break)
			      (setf (state ,record) ,step))
			 ;; reset state if user aborts from BREAK or after last break
			 (unless (and (eql (state ,record) ,step)
				      (print (cdr ,subtail)))
			   (setf (state ,record) 0))))))
		'(break))
	    '(break))
       ,result)))

(defmacro brake-when (predicate &optional tag step sexp)
  )

(defmacro break-when (&rest args)
  `(brake-when ,@args))

(defun brake-disable (tag)
  )

(defun brake-enable (tag)
  )

(defun reset-brake-points ()
  (clrhash *brake-records*))
