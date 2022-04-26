(cl:in-package #:sicl-boot-sequence-functions)

(defclass environment (sicl-boot:environment)
  ())

(defclass client (sicl-boot:client)
  ((%environment :initarg :environment :reader environment)))
