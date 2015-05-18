(cl:in-package #:sicl-clos)

(defmethod direct-slot-definition-class
    ((class regular-class) &rest initargs)
  (apply #'direct-slot-definition-class-default class initargs))

;;; The following method should be removed once the system is up and
;;; running.

(defmethod direct-slot-definition-class
    ((class built-in-class) &rest initargs)
  (apply #'direct-slot-definition-class-default class initargs))
