(cl:in-package #:sicl-environment)

(defun fboundp (function-name environment)
  (assert (not (null environment)))
  (let ((global-env (trucler:global-environment environment)))
    (sicl-global-environment:fboundp function-name global-env)))
