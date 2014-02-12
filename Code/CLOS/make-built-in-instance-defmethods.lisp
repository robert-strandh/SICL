(cl:in-package #:sicl-clos)

(defmethod make-built-in-instance ((class symbol) &rest initargs)
  (apply #'make-built-in-instance (find-class class) initargs))

(defmethod make-built-in-instance ((class built-in-clas) &rest initargs)
  (apply #'make-built-in-instance-default class initargs))
