(cl:in-package #:sicl-boot-phase-2)

(defun boot (boot)
  (format *trace-output* "Start phase 2~%")
  (with-accessors ((e2 sicl-boot:e2) (e3 sicl-boot:e3)) boot
    (change-class e2 'environment)
    (set-up-environments boot)
    (enable-defgeneric e3)
    (load-accessor-defgenerics e3)
    (enable-defclass boot)
    (sicl-boot:create-mop-classes #'sicl-boot:load-fasl e2)))
    ;; (load-mop-defclasses boot)))
