(cl:in-package #:target-data-and-control-flow)

(defmacro when (test &body body)
  `(if ,test (progn ,@body) nil))
