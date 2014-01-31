(cl:in-package #:sicl-clos)

(defun reader-method-class (class direct-slot &rest initargs)
  (apply #'reader-method-class-default class direct-slot initargs))

(defun writer-method-class (class direct-slot &rest initargs)
  (apply #'writer-method-class-default class direct-slot initargs))
