(cl:in-package #:sicl-clos)

(defun slot-boundp-using-index (general-instance index)
  (eq (general-instance-access general-instance index)
      +unbound-slot-value+))

(defun slot-makunbound-using-index (general-instance index)
  (setf (general-instance-access general-instance index)
	+unbound-slot-value+))
