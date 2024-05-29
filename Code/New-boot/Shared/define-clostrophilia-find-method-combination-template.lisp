(cl:in-package #:sicl-new-boot)

(eval-when (:compile-toplevel) (enable-parcl-symbols client))

(defun define-clostrophilia-find-method-combination-template
    (client environment)
  (setf (clo:fdefinition
         client environment @clostrophilia:find-method-combination-template)
        (lambda (client name)
          (sicl-environment:find-method-combination-template
           client environment name))))
