(cl:in-package #:sicl-environment)

(defun symbol-function (symbol environment)
  (assert (not (null environment)))
  (let ((global-env (cleavir-env:global-environment environment)))
    (sicl-global-environment:fdefinition symbol global-env)))
