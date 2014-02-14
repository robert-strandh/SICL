(cl:in-package #:sicl-clos)

(defun compute-effective-method (generic-function method-combination methods)
  (declare (ignore generic-function method-combination))
  (compute-effective-method-default methods))
