(cl:in-package #:sicl-environment)

(defun fboundp (function-name environment)
  (assert (not (null environment)))
  (let ((global-env (trucler-reference:global-environment environment)))
    (sicl-global-environment:fboundp function-name global-env)))
