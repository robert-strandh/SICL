(cl:in-package #:sicl-clos)

(defun slot-boundp-using-index (general-instance index)
  (not (eq (cleavir-primop:nook-read general-instance index)
	   +unbound-slot-value+)))

(defun slot-makunbound-using-index (instance index)
  (cleavir-primop:nook-write instance index +unbound-slot-value+)
  nil)
