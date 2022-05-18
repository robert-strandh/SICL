(cl:in-package #:sicl-boot-fill-target-environment)

(defclass client (sicl-client:sicl)
  (;; This slot contains a target environment represented as an ersatz
   ;; object.
   (%target-environment
    :initarg :target-environment
    :reader target-environment)
   ;; This slot contains the target function ENV:FDEFINITION, We can
   ;; pass the target environment in the previous slot to this
   ;; function and get access to any function in the target
   ;; environment.
   (%target-fdefinition
    :initarg :target-fdefinition
    :reader target-fdefinition)))

(defmethod env:fdefinition
    ((client client) (environment sicl-boot:header) function-name)
  (funcall (target-fdefinition client)
           client environment function-name))
