(cl:in-package #:sicl-clos)

(defun ensure-generic-function (function-name &rest args)
  (declare (ignore args))
  (find-bridge-generic-function function-name))
