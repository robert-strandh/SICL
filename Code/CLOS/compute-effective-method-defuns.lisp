(cl:in-package #:sicl-clos)

(defun compute-effective-method (generic-function method-combination methods)
  (declare (ignore method-combination))
  (compute-effective-method-default generic-function methods))
