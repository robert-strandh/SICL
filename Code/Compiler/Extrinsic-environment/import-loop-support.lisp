(cl:in-package #:sicl-extrinsic-environment)

(defun import-loop-support (environment)
  (setf (sicl-env:fdefinition 'sicl-loop::expand-body environment)
	(fdefinition 'sicl-loop::expand-body)))
