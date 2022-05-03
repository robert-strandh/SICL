(cl:in-package #:sicl-data-and-control-flow)

(let* ((environment (env:global-environment))
       (client (env:client environment))
       (setf-fdefinition (fdefinition '(setf env:fdefinition))))
  (defun (setf fdefinition) (new-definition function-name)
    (funcall setf-fdefinition new-definition client environment function-name)
    new-definition))
