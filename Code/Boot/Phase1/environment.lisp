(cl:in-package #:sicl-boot-phase1)

(defclass environment (sicl-simple-environment:simple-environment)
  ((%compilation-environment :initarg :compilation-environment
			     :reader compilation-environment)))
