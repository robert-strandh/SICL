(cl:in-package #:sicl-global-environment)

(defmacro defun (&environment env name lambda-list &body body)
  (defun-expander env name lambda-list body))
