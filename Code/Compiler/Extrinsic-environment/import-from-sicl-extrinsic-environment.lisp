(cl:in-package #:sicl-extrinsic-environment)

(defun import-from-sicl-extrinsic-environment (environment)
  (setf (sicl-env:fdefinition 'sicl-extrinsic-environment:symbol-value
			      environment)
	(fdefinition 'sicl-extrinsic-environment:symbol-value))
  (setf (sicl-env:fdefinition '(setf sicl-extrinsic-environment:symbol-value)
			      environment)
	(fdefinition '(setf sicl-extrinsic-environment:symbol-value))))
