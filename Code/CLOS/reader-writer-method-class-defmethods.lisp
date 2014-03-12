(cl:in-package #:sicl-clos)

(defmethod reader-method-class
    ((class regular-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (apply #'reader-method-class-default class direct-slot initargs))

(defmethod writer-method-class
    ((class regular-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (apply #'writer-method-class-default class direct-slot initargs))
