(cl:in-package #:sicl-data-and-control-flow)

(let* ((environment (env:global-environment))
       (setf-fdefinition (fdefinition '(setf env:fdefinition))))
  (defun (setf fdefinition) (new-definition function-name)
    (funcall setf-fdefinition new-definition
             sicl-client:*client* environment function-name)
    new-definition))
