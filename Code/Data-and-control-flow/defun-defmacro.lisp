(cl:in-package #:sicl-data-and-control-flow)

(defmacro defun (name lambda-list &body body)
  (defun-expander name lambda-list body))
