(cl:in-package #:sicl-clos)

(defmethod effective-slot-definition-class
    ((class regular-class) &rest initargs)
  (apply #'effective-slot-definition-class-default class initargs))

;;; The following method should be removed once the system is up and
;;; running.

(defmethod effective-slot-definition-class
    ((class built-in-class) &rest initargs)
  (apply #'effective-slot-definition-class-default class initargs))
