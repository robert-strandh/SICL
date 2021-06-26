(cl:in-package #:sicl-boot-phase-2)

(defun prepare-next-phase (boot)
  (with-accessors ((e1 sicl-boot:e1)
                   (e2 sicl-boot:e2)
                   (e3 sicl-boot:e3))
      boot
    ))
