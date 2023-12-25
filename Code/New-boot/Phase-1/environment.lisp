(cl:in-package #:sicl-new-boot-phase-1)

(defclass environment (common-boot:environment)
  ())

(defun create-environment (client)
  (let ((environment (cb:create-environment)))
    (change-class (trucler:global-environment client environment)
                  'sicl-environment:run-time-environment)
    environment))
