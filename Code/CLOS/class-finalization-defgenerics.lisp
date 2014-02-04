(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/finalize-inheritance.html
(defgeneric finalize-inheritance (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-class-precedence-list.html
(defgeneric compute-class-precedence-list (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-default-initargs.html
(defgeneric compute-default-initargs (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-effective-slot-definition.html
(defgeneric compute-effective-slot-definition
    (class name direct-slot-definitions))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-slots.html
(defgeneric compute-slots (class))
