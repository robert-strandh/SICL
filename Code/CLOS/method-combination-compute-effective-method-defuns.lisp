(cl:in-package #:sicl-clos)

(defun method-combination-compute-effective-method
    (method-combination methods generic-function)
  (declare (ignore method-combination))
  (compute-effective-method-default generic-function methods))
