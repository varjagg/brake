(in-package #:breac)

(defparameter *breac-points* (make-hash-table))

(defmacro breac (&optional tag step &body body)
  (let ((result (gensym "BREAC")))
    `(let ((,result (progn ,@body)))
       ,(if tag
	    (error "Not implemented")
	    '(break))
       ,result)))

(defmacro breac-if (test &optional tag step &body body)
  )

(defmacro break-if (&rest args)
  `(breac-if ,@args))

(defun breac-disable (tag)
  )

(defun breac-enable (tag)
  )
