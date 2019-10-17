(cl:in-package #:sicl-data-and-control-flow)

(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))
