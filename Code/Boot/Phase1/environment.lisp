(cl:in-package #:sicl-boot-phase1)

;;; We define a subclass of the normal extrinsic environment.  This
;;; version of the extrinsic environment is used as compilation
;;; environment in phase 1 of bootstrapping.  By creating a special
;;; version of it, we can define an :AFTER method on
;;; INITIALIZE-INSTANCE where we can modify it to correspond to the
;;; needs of bootstrapping.
(defclass compilation-environment (sicl-extrinsic-environment:environment)
  ())

(defmethod initialize-instance :after
    ((environment compilation-environment) &key)
  nil)

(defun customize (compilation-environment run-time-environment)
  (declare (ignore compilation-environment run-time-environment))
  nil)

(defclass environment (sicl-extrinsic-environment:environment)
  ((%compilation-environment
    :initarg :compilation-environment
    :initform (make-instance 'compilation-environment)
    :reader compilation-environment)))

(defmethod initialize-instance :after ((environment environment) &key)
  (customize (compilation-environment environment) environment)
  (fill-environment environment))
