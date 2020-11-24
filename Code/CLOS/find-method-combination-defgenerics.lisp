(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/find-method-combination.html
(defgeneric find-method-combination
    (generic-function method-combination-type-name method-combination-options))
