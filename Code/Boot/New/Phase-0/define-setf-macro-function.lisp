(cl:in-package #:sicl-boot-phase-0)

(defun define-setf-macro-function (environment)
  (setf (sicl-genv:fdefinition '(setf sicl-genv:macro-function) environment)
        #'(setf sicl-genv:macro-function)))
