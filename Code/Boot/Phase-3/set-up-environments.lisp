(cl:in-package #:sicl-boot-phase-3)

(defun set-up-environments (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (import-function-from-host 'funcall e4)
    (import-function-from-host '(setf sicl-genv:function-lambda-list) e4)
    (import-function-from-host '(setf sicl-genv:function-type) e4)
    (sicl-hir-interpreter:fill-environment e4)
    (import-function-from-host 'list e3)
    (import-function-from-host 'null e3)
    (import-function-from-host 'append e3)
    (import-function-from-host '(setf sicl-genv:special-variable) e3)))
