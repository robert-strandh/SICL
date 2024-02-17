(cl:in-package #:sicl-new-boot)

(defclass environment (sicl-environment:run-time-environment)
  ((%name :initarg :name :reader name)))
