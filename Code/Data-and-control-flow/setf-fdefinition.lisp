(cl:in-package #:sicl-data-and-control-flow)

(defun (setf fdefinition) (new-definition function-name)
  (setf (sicl-env:fdefinition function-name *global-environment*)
	new-definition))
