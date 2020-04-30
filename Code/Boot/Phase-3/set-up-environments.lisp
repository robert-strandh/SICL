(cl:in-package #:sicl-boot-phase-3)

(defun set-up-environments (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (sicl-hir-interpreter:fill-environment e4)
    (import-functions-from-host
     '()
     e2)
    (import-functions-from-host
     '((setf sicl-genv:special-variable)
       list null append)
     e3)
    (import-functions-from-host
     '(funcall
       (setf sicl-genv:function-lambda-list)
       (setf sicl-genv:function-type))
     e4)))
