(cl:in-package #:sicl-clos)

(defun writer-method-class (&rest arguments)
  (declare (ignore arguments))
  (find-class 'standard-writer-method))
