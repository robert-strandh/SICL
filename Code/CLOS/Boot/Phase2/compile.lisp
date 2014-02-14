(cl:in-package #:sicl-clos)

(defun compile (&rest arguments)
  (apply #'cl:compile arguments))
