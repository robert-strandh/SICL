(cl:in-package #:sicl-clos)

(defun compute-effective-method (generic-function method-combination methods)
  (compute-effective-method-default generic-function
				    method-combination
				    methods))
