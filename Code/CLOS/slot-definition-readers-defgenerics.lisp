(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-writers.html
(defgeneric slot-definition-writers (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-location.html
(defgeneric slot-definition-location (slot-definition))
