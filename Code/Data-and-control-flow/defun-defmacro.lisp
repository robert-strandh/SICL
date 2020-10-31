(cl:in-package #:sicl-data-and-control-flow)

(defmacro defun (&environment environment name lambda-list &body body)
  (defun-expander name lambda-list body environment))
