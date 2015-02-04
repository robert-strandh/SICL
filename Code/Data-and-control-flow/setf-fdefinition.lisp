(cl:in-package #:sicl-data-and-control-flow)

(defun (setf fdefinition) (new-definition function-name)
  (setf (sicl-global-environment:fdefinition
	 function-name
	 sicl-global-environment:*global-environment*)
	new-definition))
