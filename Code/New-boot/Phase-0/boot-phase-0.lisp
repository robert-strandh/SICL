(cl:in-package #:sicl-new-boot-phase-0)

(defun boot-phase-0 (boot)
  (with-accessors ((e0 sicl-new-boot:e0) (e1 sicl-new-boot:e1)) boot
    (import-package-from-host 'sicl-clos e0)))
