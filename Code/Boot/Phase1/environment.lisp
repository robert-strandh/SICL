(cl:in-package #:sicl-boot-phase1)

;;; We define a subclass of the normal extrinsic environment.  This
;;; version of the extrinsic environment is used as compilation
;;; environment in phase 1 of bootstrapping.  By creating a special
;;; version of it, we can define auxiliary methods on the generic
;;; function INITIALIZE-INSTANCE in which we can modify it to
;;; correspond to the needs of bootstrapping.
(defclass compilation-environment (sicl-extrinsic-environment:environment)
  ())

(defmethod initialize-instance :around
    ((environment compilation-environment) &key)
  (format t "Initializing phase 1 compilation environment~%")
  (call-next-method)
  (format t "Finished initializing phase 1 compilation environment~%"))

(defun customize (compilation-environment run-time-environment)
  (declare (ignore compilation-environment run-time-environment))
  nil)

(defclass environment (sicl-extrinsic-environment:environment)
  ((%compilation-environment
    :initarg :compilation-environment
    :initform (make-instance 'compilation-environment)
    :reader compilation-environment)))

(defmethod initialize-instance :around ((environment environment) &key)
  (call-next-method)
  (customize (compilation-environment environment) environment)
  (fill-environment environment))
