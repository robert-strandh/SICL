(cl:in-package #:sicl-clos)

(defun set-funcallable-instance-function (generic-function function)
  (setf (discriminating-function generic-function) function))
