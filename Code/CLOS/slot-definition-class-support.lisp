(cl:in-package #:sicl-clos)

(defun direct-slot-definition-class-default (class &rest initargs)
  (declare (ignore initargs))
  *standard-direct-slot-definition*)

(defun effective-slot-definition-class-default (class &rest initargs)
  (declare (ignore initargs))
  *standard-effective-slot-definition*)
