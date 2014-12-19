(cl:in-package #:sicl-clos)

;;; Readers for method metaobjects.
;;;
;;; For a list of specified readers of these metaobjects, see
;;; http://metamodular.com/CLOS-MOP/readers-for-method-metaobjects.html

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-function.html
(defgeneric method-function (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-generic-function.html
(defgeneric method-generic-function (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-lambda-list.html
(defgeneric method-lambda-list (method))


;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-specializers.html
(defgeneric method-specializers (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-qualifiers.html
(defgeneric method-qualifiers (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/accessor-method-slot-definition.html
(defgeneric accessor-method-slot-definition (accessor-method))
