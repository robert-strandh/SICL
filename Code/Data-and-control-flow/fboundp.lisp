(cl:in-package #:sicl-data-and-control-flow)

(defun fboundp (function-name)
  (sicl-env:fboundp function-name
		    sicl-env:*global-environment*))
