(cl:in-package #:sicl-clos)

(defun find-class (&rest arguments)
  (apply #'cl:find-class arguments))
