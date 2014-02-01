(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/direct-slot-definition-class.html
(defgeneric direct-slot-definition-class (class &rest initargs))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/effective-slot-definition-class.html
(defgeneric effective-slot-definition-class (class &rest initargs))
