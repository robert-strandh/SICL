(cl:in-package #:sicl-new-boot)

(defclass client (trucler-reference:client)
  ())

(defun create-environment ()
  (let ((environment (cb:create-environment)))
    environment))
