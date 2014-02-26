(cl:in-package #:sicl-clos)

(defun make-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (find-bridge-class class)))
  (apply #'make-instance-default class initargs))
