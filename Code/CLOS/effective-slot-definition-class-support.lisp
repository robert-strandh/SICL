(cl:in-package #:sicl-clos)

(defun effective-slot-definition-class-default (class &rest initargs)
  (declare (ignore class initargs))
  *standard-effective-slot-definition*)
