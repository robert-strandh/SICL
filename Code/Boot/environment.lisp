(cl:in-package #:sicl-boot)

(defclass environment (sicl-extrinsic-environment:environment)
  ())

(defmethod trucler-reference:global-environment ((environment environment))
  environment)
