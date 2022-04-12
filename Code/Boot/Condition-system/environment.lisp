(cl:in-package #:sicl-boot-condition-system)

(defclass environment (sicl-boot:environment)
  ((%base :initarg :base :reader base)))

(defclass client (sicl-boot:client)
  ())
