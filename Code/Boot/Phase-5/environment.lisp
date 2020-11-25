(cl:in-package #:sicl-boot-phase-5)

(defclass environment (sicl-boot:environment)
  ())

(defclass client (sicl-boot:client)
  ((%e5 :initarg :e5 :reader e5)))
