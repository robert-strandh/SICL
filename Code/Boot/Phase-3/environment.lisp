(cl:in-package #:sicl-boot-phase-3)

(defclass environment (sicl-boot:environment)
  ())

(defclass client (sicl-boot:client)
  ((%e3 :initarg :e3 :reader e3)))
