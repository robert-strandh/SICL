(cl:in-package #:sicl-data-and-control-flow)

(defun constantly (object)
  (lambda (&rest arguments)
    (declare (ignore arguments))
    object))
