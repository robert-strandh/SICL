(cl:in-package #:sicl-boot)

(defclass environment (sicl-extrinsic-environment:environment)
  ()
  (:default-initargs :client (make-instance 'client)))

(defmethod trucler-reference:global-environment ((environment environment))
  environment)
