(cl:in-package #:sicl-boot-phase-4)

(defun boot (boot)
  (format *trace-output* "Start phase 4~%")
  (with-accessors ((e0 sicl-boot:e0)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (change-class e3 'environment)
    (sicl-boot:create-accessor-defgenerics e5)
    (sicl-boot:create-mop-classes e5)))

