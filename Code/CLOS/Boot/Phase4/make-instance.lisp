(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'make-instance))

(defun make-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (find-target-class class)))
  (apply #'make-instance-default class initargs))
