(cl:in-package #:sicl-iteration)

(defmacro dotimes ((var count-form &optional result-form) &body body)
  (dotimes-expander var count-form result-form body))
