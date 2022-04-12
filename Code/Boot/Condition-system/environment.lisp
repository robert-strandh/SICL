(cl:in-package #:sicl-boot-condition-system)

(defclass environment (sicl-boot:environment)
  ((%base :initarg :base :reader base)))

(defclass client (sicl-boot:client)
  ())

(defmethod env:fboundp ((client client) (environment environment) function-name)
  (or (call-next-method)
      (env:fboundp client (base environment) function-name)))
