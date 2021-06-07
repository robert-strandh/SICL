(cl:in-package #:sicl-arithmetic)

(defclass double-float (float)
  ())

(defun double-float-p (object)
  (typep object 'double-float))
