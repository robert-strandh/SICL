(cl:in-package #:sicl-boot-phase-2)

(defun set-up-environments (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3))
      boot
    (sicl-hir-evaluator:fill-environment e2)
    (sicl-hir-evaluator:fill-environment e3)
    (sicl-boot:import-functions-from-host
     '(funcall cons cadr cddr cdddr list error)
     e2)
    (sicl-boot:import-functions-from-host
     '((setf sicl-genv:function-lambda-list)
       (setf sicl-genv:function-type)
       funcall)
     e3)))
