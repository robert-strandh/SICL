(cl:in-package #:sicl-clos)

(defun direct-slot-definition-class-default (class &rest initargs)
  (declare (ignore class initargs))
  (find-class 'standard-direct-slot-definition))
