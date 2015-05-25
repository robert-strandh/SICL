(cl:in-package #:sicl-clos)

(defun reader-method-class (&rest arguments)
  (declare (ignore arguments))
  (find-class 'standard-reader-method))
