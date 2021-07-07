(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/setf-class-name.html
(defgeneric (setf class-name) (new-name class))

(defmethod (setf class-name) (new-name (class class))
  (reinitialize-instance class :name new-name))
