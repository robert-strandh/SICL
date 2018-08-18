(cl:in-package #:sicl-new-boot-phase-2)

(defun boot-phase-1 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (list e1 e2 e3)))
