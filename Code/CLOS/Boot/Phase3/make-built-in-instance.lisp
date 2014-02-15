(cl:in-package #:sicl-clos)

(defun make-built-in-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (cdr (assoc class *bridge-classes*))))
  (apply #'make-built-in-instance-default class initargs))

