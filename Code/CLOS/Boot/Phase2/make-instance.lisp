(cl:in-package #:sicl-clos)

(defun make-instance (&rest arguments)
  (apply #'cl:make-instance arguments))
