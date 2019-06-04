(cl:in-package #:sicl-boot-phase-2)

(defun set-up-environments (boot)
  (with-accessors ((e3 sicl-boot:e3)) boot
    (sicl-boot:import-function-from-host 'funcall e3)
    (sicl-boot:import-function-from-host '(setf sicl-genv:function-lambda-list) e3)
    (sicl-boot:import-function-from-host '(setf sicl-genv:function-type) e3)
    (sicl-hir-to-cl:fill-environment e3)
    (setf (sicl-genv:fdefinition 'sicl-genv:global-environment e3)
          (constantly e3))))
