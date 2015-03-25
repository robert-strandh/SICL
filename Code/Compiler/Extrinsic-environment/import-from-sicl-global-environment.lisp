(cl:in-package #:sicl-extrinsic-environment)

(defun import-from-sicl-global-environment (environment)
  (setf (sicl-env:special-variable 'sicl-env:*global-environment* environment t)
	environment))
