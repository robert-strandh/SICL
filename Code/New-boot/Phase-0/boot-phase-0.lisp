(cl:in-package #:sicl-new-boot-phase-0)

(defun import-from-host (boot)
  (with-accessors ((e0 sicl-new-boot:e0)) boot
    (import-package-from-host 'sicl-clos e0)
    (import-functions-from-host
     '(sicl-genv:find-class)
     e0)))

(defun boot-phase-0 (boot)
  (import-from-host boot)
  (with-accessors ((e0 sicl-new-boot:e0) (e1 sicl-new-boot:e1)) boot
    nil))
