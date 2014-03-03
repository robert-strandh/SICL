(cl:in-package #:sicl-clos)

(cl:defclass bridge-generic-function
    (#+sbcl sb-pcl:funcallable-standard-object
     sicl-boot-phase1:standard-generic-function)
  ()
  (:metaclass #+sbcl sb-pcl:funcallable-standard-class))

(cl:defmethod cl:print-object ((object bridge-generic-function) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s" (cl:slot-value object '%name))))
