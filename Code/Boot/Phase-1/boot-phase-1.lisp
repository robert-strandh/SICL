(cl:in-package #:sicl-boot-phase-1)

(defun boot-phase-1 (boot)
  (format *trace-output* "Start of phase 1~%")
  (with-accessors ((e1 sicl-boot:e1) (e2 sicl-boot:e2)) boot
    (change-class e1 'environment)
    (define-accessor-generic-functions e2)
    (define-mop-classes-phase1 boot)))
