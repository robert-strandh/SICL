(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'compile))

(defun compile (dummy lambda-expression)
  (declare (ignore dummy lambda-expression))
  (allocate-built-in-instance (find-bridge-class 'function)))
