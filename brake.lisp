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
    (setf (brake-points record) (merge 'list (list step)
				       (brake-points record)
				       #'<))))

(defmacro brake (&optional tag-or-body step &body body)
  (let ((result (gensym "BRK")))
    (when (and tag-or-body (listp tag-or-body))
      (setf body tag-or-body))
    `(let ((,result (progn ,@body)))
       ,(if tag-or-body
	    (error "Not implemented")
	    '(break))
       ,result)))

(defmacro brake-when (test &optional tag step &body body)
  )

(defmacro break-when (&rest args)
  `(brake-when ,@args))

(defun brake-disable (tag)
  )

(defun brake-enable (tag)
  )

(defun reset-brake-points ()
  (clrhash *brake-records*))
