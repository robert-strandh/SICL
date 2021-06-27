(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/direct-slot-definition-class.html
(defgeneric direct-slot-definition-class (class &rest initargs))

(defmethod direct-slot-definition-class
    ((class regular-class) &rest initargs)
  (declare (ignore class initargs))
  (find-class 'standard-direct-slot-definition))
