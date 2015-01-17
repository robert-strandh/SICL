(cl:in-package #:sicl-environment)

(defun fboundp (function-name environment)
  (assert (not (null environment)))
  (let ((global-env (cleavir-env:global-environment environment)))
    (sicl-global-environment:fboundp function-name global-env)))
