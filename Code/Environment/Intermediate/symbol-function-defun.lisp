(cl:in-package #:sicl-environment)

(defun symbol-function (symbol environment)
  (assert (not (null environment)))
  (let ((global-env (trucler-reference:global-environment environment)))
    (sicl-global-environment:fdefinition symbol global-env)))
