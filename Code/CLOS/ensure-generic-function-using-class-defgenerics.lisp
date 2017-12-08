(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/ensure-generic-function-using-class.html
(defgeneric ensure-generic-function-using-class
    (generic-function
     function-name
     &key
       argument-precedence-order
       declarations
       documentation
       generic-function-class
       lambda-list
       method-class
       method-combination
       name
       environment
     &allow-other-keys))
