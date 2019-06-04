(cl:in-package #:sicl-boot)

(defclass environment (sicl-alternative-extrinsic-environment:environment)
  ())

(defmethod trucler-reference:global-environment ((environment environment))
  environment)
