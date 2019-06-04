(cl:in-package #:sicl-boot-phase-0)

(defun boot (boot)
  (let ((environment (make-instance 'environment))
        (client (make-instance 'client)))
    (setf (sicl-boot:e0 boot) environment)
    (fill-environment client environment)
    (compile-files client environment)))
