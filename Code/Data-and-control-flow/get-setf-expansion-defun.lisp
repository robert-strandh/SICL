(in-package #:sicl-data-and-control-flow)

(defun get-setf-expansion
    (place &optional (environment (env:global-environment)))
  (let* ((global-env (env:global-environment environment)))
    (env:get-setf-expansion sicl-client:*client* environment place)))
