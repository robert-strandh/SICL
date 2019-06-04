(cl:in-package #:sicl-boot-phase-0)

(defun boot (boot)
  (format *trace-output* "Start phase 0~%")
  (let ((environment (make-instance 'environment))
        (client (make-instance 'client)))
    (setf (sicl-boot:e0 boot) environment)
    (fill-environment client environment)
    (compile-files client environment)))
