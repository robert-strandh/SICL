(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-effective-method.html
(defgeneric compute-effective-method
    (generic-function method-combination methods))
