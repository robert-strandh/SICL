(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-direct-subclass.html
(defgeneric add-direct-subclass (superclass subclass))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-direct-subclass.html
(defgeneric remove-direct-subclass (superclass subclass))
