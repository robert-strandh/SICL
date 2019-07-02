(cl:in-package #:sicl-boot-phase-0)

(defun boot (boot)
  (format *trace-output* "Start phase 0~%")
  (with-accessors ((e0 sicl-boot:e0)) boot
    (change-class e0 'environment)
    (let ((client (make-instance 'client)))
      (fill-environment client e0)
      (compile-files client e0))))
