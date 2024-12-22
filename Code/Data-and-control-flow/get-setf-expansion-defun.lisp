(in-package #:sicl-data-and-control-flow)

(defun get-setf-expansion
    (place &optional (environment (env:*environment*)))
  (env:get-setf-expansion env:*client* environment place))
