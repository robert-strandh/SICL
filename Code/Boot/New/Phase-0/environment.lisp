(cl:in-package #:sicl-boot-phase-0)

(defclass environment (sicl-alternative-extrinsic-environment:environment)
  ())

(defmethod trucler-reference:global-environment ((environment environment))
  environment)
