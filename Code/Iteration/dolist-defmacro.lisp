(cl:in-package #:sicl-iteration)

(defmacro dolist ((var list-form &optional result-form) &body body)
  (dolist-expander var list-form result-form body))
