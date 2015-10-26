(cl:in-package #:sicl-clos)

;;; For the specification of this function, see
;;; http://metamodular.com/CLOS-MOP/standard-instance-access.html
(defun standard-instance-access (instance location)
  (general-instance-access instance location))

(defun (setf standard-instance-access) (new-value instance location)
  (setf (general-instance-access instance location) new-value))
