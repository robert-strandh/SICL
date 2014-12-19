(cl:in-package #:sicl-clos)

;;; For a list of specified readers of these metaobjects, see
;;; see
;;; http://metamodular.com/CLOS-MOP/readers-for-generic-function-metaobjects.html

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-name.html
(defgeneric generic-function-name (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-lambda-list.html
(defgeneric generic-function-lambda-list (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-argument-precedence-order.html
(defgeneric generic-function-argument-precedence-order (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-declarations.html
(defgeneric generic-function-declarations (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-method-class.html
(defgeneric generic-function-method-class (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-method-combination.html
(defgeneric generic-function-method-combination (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-methods.html
(defgeneric generic-function-methods (generic-function))
