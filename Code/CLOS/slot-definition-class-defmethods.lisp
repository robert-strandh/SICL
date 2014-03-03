(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  DIRECT-SLOT-DEFINITION-CLASS and EFFECTIVE-SLOT-DEFINITION-CLASS.

(defmethod direct-slot-definition-class
    ((class regular-class) &rest initargs)
  (apply #'direct-slot-definition-class-default class initargs))

(defmethod effective-slot-definition-class
    ((class regular-class) &rest initargs)
  (apply #'effective-slot-definition-class-default class initargs))
