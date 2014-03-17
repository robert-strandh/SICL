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

;;; These methods should be removed when the system is in production.

(defmethod reader-method-class
    ((class built-in-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (apply #'reader-method-class-default class direct-slot initargs))

(defmethod writer-method-class
    ((class built-in-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (apply #'writer-method-class-default class direct-slot initargs))
