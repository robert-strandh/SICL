(cl:in-package #:sicl-data-and-control-flow)

(defmacro destructuring-bind (lambda-list expression &body body)
  (destructuring-bind-expander lambda-list expression body))
