(cl:in-package #:sicl-conditions)

(defun cerror (continue-string datum &rest arguments)
  (with-simple-restart
      (continue "~A" (apply #'format nil continue-string arguments))
    (apply #'error datum arguments))
  nil)
