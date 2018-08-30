(cl:in-package #:sicl-clos)

(defun make-method-for-generic-function (generic-function specializers keys)
  (apply #'make-instance
         (generic-function-method-class generic-function)
         :specializers specializers
         keys))
