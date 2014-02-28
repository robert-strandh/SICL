(cl:in-package #:sicl-clos)

(defun make-built-in-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (find-bridge-class class)))
  (apply #'make-built-in-instance-default class initargs))

