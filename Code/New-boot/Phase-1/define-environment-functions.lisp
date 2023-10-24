(cl:in-package #:sicl-new-boot-phase-1)

(defun define-environment-functions (client global-environment)
  (setf (clostrum:fdefinition
         client global-environment '(setf fdefinition))
        (lambda (definition name)
          (setf (clostrum:fdefinition client global-environment name)
                definition)))
  (setf (clostrum:fdefinition
         client global-environment 'find-class)
        (lambda (name)
          (clostrum:find-class client global-environment name)))
  (setf (clostrum:fdefinition
         client global-environment '(setf macro-function))
        (lambda (definition name)
          (setf (clostrum:macro-function client global-environment name)
                definition)))
  (setf (clostrum:fdefinition
         client global-environment '(setf find-class))
        (lambda (class name)
          (setf (clostrum:find-class client global-environment name)
                class)))
  (setf (clostrum:fdefinition
         client global-environment '(setf symbol-value))
        (lambda (value name)
          (setf (clostrum:symbol-value client global-environment name)
                value))))
