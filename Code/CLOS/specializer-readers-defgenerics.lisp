(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/specializer-direct-generic-functions.html
(defgeneric specializer-direct-generic-functions (specializer))

(defgeneric specializer-direct-methods (specializer))

(defgeneric eql-specializer-object (eql-specializer))
