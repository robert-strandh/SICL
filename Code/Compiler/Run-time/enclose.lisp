(cl:in-package #:sicl-run-time)

(defun enclose (entry-point)
  (make-instance 'sicl-clos:simple-function
    :entry-point entry-point))
