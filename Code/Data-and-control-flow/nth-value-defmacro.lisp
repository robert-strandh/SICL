(cl:in-package #:sicl-data-and-control-flow)

(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))
