(cl:in-package #:sicl-clos)

(defun make-method-lambda
    (generic-function method lambda-expression environment)
  (make-method-lambda-default
   generic-function method lambda-expression environment))
