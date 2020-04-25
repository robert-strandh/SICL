(cl:in-package #:sicl-clos)

;;; We extract the body of the default methods into separate functions
;;; in a separate file.  During bootstrapping, the generic functions
;;; need to be tied to and present in an environment En, but the
;;; classes need to be looked up in the environment En-1.  We
;;; accomplish this effect by having READER-METHOD-CLASS-DEFAULT and
;;; WRITER-METHOD-CLASS-DEFAULT tied to En-1 and present in En.

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
