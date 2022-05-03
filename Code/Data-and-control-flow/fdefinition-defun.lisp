(cl:in-package #:sicl-data-and-control-flow)

(let* ((environment (env:global-environment))
       (client (env:client environment))
       (fdefinition (fdefinition 'env:fdefinition)))
  (defun fdefinition (function-name)
    ;; We call IDENTITY because we want only the first value returned
    ;; by the environment function.
    (identity (funcall fdefinition client environment function-name))))
