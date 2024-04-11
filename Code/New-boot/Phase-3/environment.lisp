(cl:in-package #:sicl-new-boot-phase-3)

(defun create-environment (client)
  (let ((environment (cb:create-environment)))
    (change-class (trucler:global-environment client environment)
                  'sb:environment
                  :name :E3)
    environment))
