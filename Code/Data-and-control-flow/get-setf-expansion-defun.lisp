(in-package #:sicl-data-and-control-flow)

(defun get-setf-expansion
    (place &optional (environment (env:global-environment)))
  (let* ((global-env (env:global-environment environment))
         (client (env:client global-env)))
    (env:get-setf-expansion client environment place)))
