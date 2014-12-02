(cl:in-package #:sicl-data-and-control-flow)

(defun (setf fdefinition) (new-definition function-name)
  (setf (sicl-env:fdefinition function-name
			      sicl-env:*global-environment*)
	new-definition))
