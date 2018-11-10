(cl:in-package #:sicl-clos)

(defun ensure-method-on-generic-function (&rest arguments)
  (apply #'ensure-method arguments))
