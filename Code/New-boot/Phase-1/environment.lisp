(cl:in-package #:sicl-new-boot)

(defclass client (cb:macro-transforming-client)
  ())

(defun create-environment ()
  (let ((environment (cb:create-environment)))
    environment))
