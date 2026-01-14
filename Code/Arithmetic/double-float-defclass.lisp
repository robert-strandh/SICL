(cl:in-package #:sicl-arithmetic)

(defclass double-float (float standard-object)
  ())

(defun double-float-p (object)
  (typep object 'double-float))
