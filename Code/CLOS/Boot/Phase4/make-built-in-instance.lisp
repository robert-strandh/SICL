(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'make-built-in-instance))

(defun make-built-in-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (find-bridge-class class)))
  (let ((instance (apply #'make-built-in-instance-default class initargs)))
    (patch-built-in-instance instance)
    instance))


