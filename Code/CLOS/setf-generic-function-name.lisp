(cl:in-package #:sicl-clos)

;;; Contrary to appearance, this function is not a slot writer.
;;; Instead, according to the AMOP, this function should call
;;; REINITIALIZE-INSTANCE with three arguments: CLASS, :NAME, and
;;; NEW-NAME.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/setf-generic-function-name.html
(defgeneric (setf generic-function-name) (new-name generic-function))

(defmethod (setf generic-function-name)
    (new-name (generic-function generic-function))
  (reinitialize-instance generic-function :name new-name))
