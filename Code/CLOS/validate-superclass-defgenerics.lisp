(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/validate-superclass.html
(defgeneric validate-superclass (class superclass)
  (:argument-precedence-order superclass class))
