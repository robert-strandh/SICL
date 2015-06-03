(cl:in-package #:sicl-clos)

(defun ensure-class (&rest arguments)
  (apply #'ensure-class-using-class-null arguments))
