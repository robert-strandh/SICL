(cl:in-package #:sicl-clos)

(defmethod default-superclasses ((class class))
  '())

(defmethod default-superclasses ((class standard-class))
  (find-class 'standard-object))

(defmethod default-superclasses ((class funcallable-standard-class))
  (find-class 'funcallable-standard-object))
