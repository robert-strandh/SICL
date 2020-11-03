(cl:in-package #:sicl-boot-phase-6)

(defun boot (boot)
  (format *trace-output* "Start phase 6~%")
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (enable-object-creation e5)
    (copy-classes e4 e5)))
