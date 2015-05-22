(cl:in-package #:sicl-boot-phase2)

(defclass environment (sicl-extrinsic-environment:environment)
  ((%compilation-environment
    :initarg :compilation-environment
    :initform (make-instance 'sicl-extrinsic-environment:environment))
   (%phase1-environment
    :initarg :phase1-environment
    :initform (make-instance 'sicl-boot-phase1:environment)
    :reader phase1-environment)))

(defmethod initialize-instance :after ((environment environment) &key)
  (fill-environment environment))
