(cl:in-package #:sicl-environment)

(defun find-class (class-name environment)
  (assert (not (null environment)))
  (let ((global-env (trucler:global-environment environment)))
    (sicl-global-environment:find-class class-name global-env)))
