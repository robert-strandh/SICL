(cl:in-package #:sicl-clos)

;;; Contrary to appearance, this function is not a slot writer.
;;; Instead, according to the AMOP, this function should call
;;; REINITIALIZE-INSTANCE with three arguments: CLASS, :NAME, and
;;; NEW-NAME.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/setf-class-name.html
(defgeneric (setf class-name) (new-name class))

(defmethod (setf class-name) (new-name (class class))
  (reinitialize-instance class :name new-name))
