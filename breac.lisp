(in-package #:breac)

(defparameter *breac-points* '())

(defmacro breac (&optional tag step &body body)
  )

(defmacro breac-if (test &optional tag step &body body)
  )

(defmacro break-if (&rest args)
  `(breac-if ,@args))
