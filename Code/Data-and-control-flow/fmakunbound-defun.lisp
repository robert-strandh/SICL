(cl:in-package #:sicl-data-and-control-flow)

(let* ((environment (sicl-environment:global-environment))
       (client (sicl-environment:client environment))
       (fmakunbound (fdefinition 'sicl-environment:fmakunbound)))
  (defun fmakunbound (function-name)
    (funcall fmakunbound client environment function-name)
    function-name))
