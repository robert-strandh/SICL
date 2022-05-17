(cl:in-package #:sicl-boot-fill-target-environment)

(defclass client (sicl-client:sicl)
  ((%target-environment
    :initarg :target-environment
    :reader target-environment)))
