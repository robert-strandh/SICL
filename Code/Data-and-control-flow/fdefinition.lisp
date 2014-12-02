(cl:in-package #:sicl-data-and-control-flow)

(defun fdefinition (function-name)
  (sicl-env:fdefinition function-name
			sicl-env:*global-environment*))
