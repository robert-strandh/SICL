(cl:in-package #:sicl-clos)

(defun method-combination-compute-effective-method-default
    (method-combination methods)
  (declare (ignore method-combination))
  (compute-effective-method-default methods))
