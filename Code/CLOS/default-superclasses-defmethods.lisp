(cl:in-package #:sicl-clos)

(defmethod default-superclasses ((class class))
  '())

(defmethod default-superclasses ((class standard-class))
  (list (find-superclass 'standard-object)))

(defmethod default-superclasses ((class funcallable-standard-class))
  (list (find-superclass 'funcallable-standard-object)))
