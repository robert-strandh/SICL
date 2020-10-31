(in-package #:sicl-data-and-control-flow)

(defun get-setf-expansion
    (place &optional (environment (sicl-environment:global-environment)))
  (let* ((global-env (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-env)))
    (sicl-environment:get-setf-expansion client environment place)))
