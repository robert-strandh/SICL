(cl:in-package #:sicl-environment)

(defun fdefinition (function-name environment)
  (assert (not (null environment)))
  (let ((global-env (trucler:global-environment environment)))
    (sicl-global-environment:fdefinition function-name global-env)))
