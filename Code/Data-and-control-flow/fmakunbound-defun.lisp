(cl:in-package #:sicl-data-and-control-flow)

(defun fmakunbound (function-name)
  (sicl-env:fmakunbound function-name
			sicl-env:*global-environment*))
