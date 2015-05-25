(cl:in-package #:sicl-clos)

(defun direct-slot-definition-class (&rest arguments)
  (declare (ignore arguments))
  (find-class 'standard-direct-slot-definition))
