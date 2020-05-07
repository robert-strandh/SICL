(cl:in-package #:sicl-boot-phase-7)

(defun boot (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    (load-make-instance e5)
    (satiate-generic-functions e3 e4 e5)
    (patch-classes e4 e5)
    (patch-generic-functions e3 e4 e5)
    (move-functions e5 e6)))
