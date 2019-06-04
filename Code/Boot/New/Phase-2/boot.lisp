(cl:in-package #:sicl-boot-phase-2)

(defun boot (boot)
  (with-accessors ((e2 sicl-boot:e2) (e3 sicl-boot:e3)) boot
    (change-class e2 'environment)
    (enable-defgeneric e3)))
