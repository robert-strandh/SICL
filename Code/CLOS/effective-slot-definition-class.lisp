(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/effective-slot-definition-class.html
(defgeneric effective-slot-definition-class (class &rest initargs))

(defmethod effective-slot-definition-class
    ((class regular-class) &rest initargs)
  (declare (ignore class initargs))
  (find-class 'standard-effective-slot-definition))
