(cl:in-package #:sicl-new-boot-phase-3)

(defun create-mop-classes (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4))
      boot
    (import-functions-from-host '(sicl-genv:typep) e3)
    nil))
