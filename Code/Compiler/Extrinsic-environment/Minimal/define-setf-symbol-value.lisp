(cl:in-package #:sicl-minimal-extrinsic-environment)

(defun define-setf-symbol-value (environment)
  (setf (sicl-genv:fdefinition '(setf symbol-value) environment) 
        #'(setf symbol-value)))

(defun define-symbol-value (environment)
  (setf (sicl-genv:fdefinition 'symbol-value environment) #'symbol-value))
