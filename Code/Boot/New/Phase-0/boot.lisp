(cl:in-package #:sicl-boot-phase-0)

(defun boot ()
  (let ((environment (make-instance 'environment))
        (client (make-instance 'client)))
    (fill-environment client environment)
    (compile-files client environment)))
