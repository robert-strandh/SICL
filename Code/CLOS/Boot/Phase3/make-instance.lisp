(cl:in-package #:sicl-clos)

(defun make-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (cdr (assoc class *bridge-classes*))))
  (apply #'make-instance-default class initargs))
