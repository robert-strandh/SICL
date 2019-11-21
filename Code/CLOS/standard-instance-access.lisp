(cl:in-package #:sicl-clos)

;;; For the specification of this function, see
;;; http://metamodular.com/CLOS-MOP/standard-instance-access.html
(defun standard-instance-access (instance location)
  (cleavir-primop:nook-read instance location))

(defun (setf standard-instance-access) (new-value instance location)
  (cleavir-primop:nook-write instance location new-value)
  new-value)
