(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/setf-generic-function-name.html
(defgeneric (setf generic-function-name) (new-name generic-function))

(defmethod (setf generic-function-name)
    (new-name (generic-function generic-function))
  (reinitialize-instance generic-function :name new-name))
