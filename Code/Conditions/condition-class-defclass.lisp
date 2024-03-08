(cl:in-package #:sicl-conditions)

(defclass condition-class (clostrophilia:regular-class)
  ())

(defgeneric clostrophilia:reader-method-class
    (class direct-slot-definition &rest initargs))

(defgeneric clostrophilia:writer-method-class
    (class direct-slot-definition &rest initargs))

(defmethod clostrophilia:reader-method-class
    ((class condition-class)
     (direct-slot-definition clostrophilia:standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore initargs))
  (find-class 'clostrophilia:standard-reader-method))

(defmethod clostrophilia:writer-method-class
    ((class condition-class)
     (direct-slot-definition clostrophilia:standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore initargs))
  (find-class 'clostrophilia:standard-writer-method))
