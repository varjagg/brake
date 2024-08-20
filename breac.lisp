(in-package #:breac)

(defparameter *breac-points* '())

(defmacro breac (&optional tag step &body body)
  (let ((result (gensym)))
    `(let ((,result ,body))
       ,(if tag
	    (error "Not implemented")
	    '(break))
       ,result)))

(defmacro breac-if (test &optional tag step &body body)
  )

(defmacro break-if (&rest args)
  `(breac-if ,@args))
