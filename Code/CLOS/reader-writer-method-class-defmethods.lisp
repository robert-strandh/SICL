(cl:in-package #:sicl-clos)

(defmethod reader-method-class
    ((class standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (apply #'reader-method-class-default class direct-slot initargs))

(defmethod reader-method-class
    ((class funcallable-standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (apply #'reader-method-class-default class direct-slot initargs))

(defmethod writer-method-class
    ((class standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (apply #'writer-method-class-default class direct-slot initargs))

(defmethod writer-method-class
    ((class funcallable-standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (apply #'writer-method-class-default class direct-slot initargs))
