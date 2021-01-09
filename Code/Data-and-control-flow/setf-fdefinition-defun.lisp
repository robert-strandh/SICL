(cl:in-package #:sicl-data-and-control-flow)

(let* ((environment (sicl-environment:global-environment))
       (client (sicl-environment:client environment))
       (setf-fdefinition (fdefinition '(setf sicl-environment:fdefinition))))
  (defun (setf fdefinition) (new-definition function-name)
    (funcall setf-fdefinition new-definition client environment function-name)
    new-definition))
