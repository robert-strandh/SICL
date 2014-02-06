(cl:in-package #:sicl-clos)

(defun allocate-instance (class &rest initargs)
  (apply #'allocate-instance-default class initargs))
