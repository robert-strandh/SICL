(cl:in-package #:sicl-boot-phase1)

(defclass environment (sicl-simple-environment:simple-environment)
  ((%compilation-environment
    :initarg :compilation-environment
    :initform (make-instance 'sicl-extrinsic-environment:environment)
    :reader compilation-environment)))

(defmethod initialize-instance :after ((environment environment) &key)
  (fill-environment environment))
