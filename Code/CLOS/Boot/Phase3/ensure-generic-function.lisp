(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'ensure-generic-function))

(defun ensure-generic-function (function-name &rest args)
  (declare (ignore args))
  (find-target-generic-function function-name))
