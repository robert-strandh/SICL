(in-package #:sicl-data-and-control-flow)

(defun get-setf-expansion (place &optional environment)
  (let ((global-env (cleavir-env:global-environment environment)))
    (sicl-global-environment:get-setf-expansion place global-env)))
