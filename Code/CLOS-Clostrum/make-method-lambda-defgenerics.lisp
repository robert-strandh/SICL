(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/make-method-lambda.html
(defgeneric make-method-lambda
    (generic-function method lambda-expression environment))
