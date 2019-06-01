(cl:in-package #:sicl-boot-phase-0)

(defun compile-files (client environment)
  (flet ((cf (name)
           (compile-file client name environment)))
    (cf "CLOS/t-defclass.lisp")))
