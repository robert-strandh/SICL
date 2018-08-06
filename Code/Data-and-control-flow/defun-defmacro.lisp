(cl:in-package #:sicl-data-and-control-flow)

(defmacro defun (&environment env name lambda-list &body body)
  (defun-expander env name lambda-list body))
