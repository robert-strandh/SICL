(cl:in-package #:sicl-new-boot)

(defun define-environment-functions (client global-environment)
  (setf (clostrum:fdefinition
         client global-environment '(setf fdefinition))
        (lambda (definition name)
          (setf (clostrum:fdefinition client global-environment name)
                definition)))
  (setf (clostrum:fdefinition
         client global-environment '(setf find-class))
        (lambda (class name)
          (setf (clostrum:find-class client global-environment name)
                class))))
