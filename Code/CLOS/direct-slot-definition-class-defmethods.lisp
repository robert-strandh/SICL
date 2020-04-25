(cl:in-package #:sicl-clos)

;;; We extract the body of the default methods into a separate
;;; function in a separate file.  During bootstrapping, the generic
;;; function need to be tied to and present in an environment En, but
;;; the classes need to be looked up in the environment En-1.  We
;;; accomplish this effect by having
;;; DIRECT-SLOT-DEFINITION-CLASS-DEFAULT tied to En-1 and present in
;;; En.

(defmethod direct-slot-definition-class
    ((class regular-class) &rest initargs)
  (apply #'direct-slot-definition-class-default class initargs))

;;; The following method should be removed once the system is up and
;;; running.

(defmethod direct-slot-definition-class
    ((class built-in-class) &rest initargs)
  (apply #'direct-slot-definition-class-default class initargs))
