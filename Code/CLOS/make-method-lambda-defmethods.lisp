(cl:in-package #:sicl-clos)

(defmethod make-method-lambda
    ((generic-function standard-generic-function)
     (method standard-method)
     lambda-expression
     environment)
  (make-method-lambda-default
   generic-function method lambda-expression environment))
