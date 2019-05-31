(in-package #:sicl-data-and-control-flow)

(defun get-setf-expansion (place &optional environment)
  (let ((global-env (trucler:global-environment environment)))
    (sicl-global-environment:get-setf-expansion place global-env)))
