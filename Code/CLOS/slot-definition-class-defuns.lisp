(cl:in-package #:sicl-clos)

(defun direct-slot-definition-class (class &rest initargs)
  (apply #'direct-slot-definition-class-default class initargs))

(defun effective-slot-definition-class (class &rest initargs)
  (apply #'effective-slot-definition-class-default class initargs))
