(cl:in-package #:sicl-minimal-extrinsic-environment)

(defun define-find-package (environment)
  (setf (sicl-genv:fdefinition 'find-package environment)
        (lambda (name) (sicl-genv:find-package name environment))))
