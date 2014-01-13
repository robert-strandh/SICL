(cl:in-package #:sicl-clos)

(defun ensure-generic-function (name &rest keys)
  ;; FIXME: check if it names a non-generic function or macro already. 
  (let ((generic-function (find-generic-function name)))
    (apply #'ensure-generic-function-using-class
	   generic-function name keys)))
