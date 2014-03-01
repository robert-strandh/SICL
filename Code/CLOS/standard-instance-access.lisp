(cl:in-package #:sicl-clos)

;;; For the specification of this function, see
;;; http://metamodular.com/CLOS-MOP/standard-instance-access.html
(defun standard-instance-access (instance location)
  (slot-contents (heap-instance-slots instance) location))

(defun (setf standard-instance-access) (new-value instance location)
  (setf (slot-contents (heap-instance-slots instance) location) new-value))
