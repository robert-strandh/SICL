(cl:in-package #:sicl-data-and-control-flow)

(let* ((environment (sicl-environment:global-environment))
       (client (sicl-environment:client environment))
       (fdefinition (fdefinition 'sicl-environment:fdefinition)))
  (defun fdefinition (function-name)
    ;; We call IDENTITY because we want only the first value returned
    ;; by the environment function.
    (identity (funcall fdefinition client environment function-name))))
