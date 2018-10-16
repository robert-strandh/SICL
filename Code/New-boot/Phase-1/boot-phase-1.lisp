(cl:in-package #:sicl-new-boot-phase-1)

(defclass environment (sicl-new-boot:environment)
  ())

(defun boot-phase-1 (boot)
  (format *trace-output* "Start of phase 1~%")
  (with-accessors ((e1 sicl-new-boot:e1) (e2 sicl-new-boot:e2)) boot
    (change-class e1 'environment)
    (import-functions-from-host '(format) e1)
    (setf (sicl-genv:special-variable '*trace-output* e1 t) *trace-output*)
    (load-accessor-defgenerics e2)
    (enable-defclass-in-e1 boot)
    (create-mop-classes-phase1 boot)))
