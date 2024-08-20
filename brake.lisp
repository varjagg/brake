(in-package #:brake)

(defparameter *brake-points* (make-hash-table))

(defmacro brake (&optional tag step &body body)
  (let ((result (gensym "BRK")))
    `(let ((,result (progn ,@body)))
       ,(if tag
	    (error "Not implemented")
	    '(break))
       ,result)))

(defmacro brake-if (test &optional tag step &body body)
  )

(defmacro break-if (&rest args)
  `(brake-if ,@args))

(defun brake-disable (tag)
  )

(defun brake-enable (tag)
  )
