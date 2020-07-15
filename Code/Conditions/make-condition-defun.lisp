(cl:in-package #:sicl-conditions)

(defun make-condition (type &rest args)
  (apply #'make-instance type args))
