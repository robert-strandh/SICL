(cl:in-package #:sicl-clos)

(defun set-funcallable-instance-function (function generic-function)
  (setf (discriminating-function generic-function) function))
