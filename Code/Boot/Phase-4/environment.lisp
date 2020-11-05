(cl:in-package #:sicl-boot-phase-4)

(defclass environment (sicl-boot:environment)
  ())

(defclass client (sicl-boot:client)
  ((%e4 :initarg :e4 :reader e4)))
