(cl:in-package #:sicl-boot-phase1)

(defclass environment (sicl-simple-environment:simple-environment)
  ((%compilation-environment :initarg :compilation-environment
			     :reader compilation-environment)))

(defmethod initialize-instance :after
    ((environment environment) &key compilation-environment)
  (fill-environment compilation-environment environment))
