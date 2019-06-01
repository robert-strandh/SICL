(in-package #:sicl-data-and-control-flow)

(defun get-setf-expansion (place &optional environment)
  (let ((global-env (if (null environment)
                        (sicl-genv:global-environment)
                        (trucler-reference:global-environment environment))))
    (sicl-global-environment:get-setf-expansion place global-env)))
