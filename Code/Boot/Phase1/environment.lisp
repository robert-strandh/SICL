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
  ;; For the benefit of the macro DEFMETHOD, we define the function
  ;; SICL-CLOS:CLASS-PROTOTYPE to be the function CLASS-PROTOTYPE of
  ;; the host.
  (setf (sicl-genv:fdefinition 'sicl-clos:class-prototype environment)
	#'closer-mop:class-prototype))

(defclass environment (sicl-extrinsic-environment:environment)
  ((%compilation-environment
    :initarg :compilation-environment
    :initform (make-instance 'compilation-environment)
    :reader compilation-environment)))

(defmethod initialize-instance :after ((environment environment) &key)
  (fill-environment environment))
