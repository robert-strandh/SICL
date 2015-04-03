(cl:in-package #:sicl-extrinsic-environment)

(defun import-from-sicl-global-environment (environment)
  ;; This variable is deprecated.  Uses of it should be replaced by
  ;; uses of the constant sicl-env:+global-environment+
  (setf (sicl-env:special-variable 'sicl-env:*global-environment* environment t)
	environment)
  (setf (sicl-env:constant-variable 'sicl-env:+global-environment+ environment)
	environment))
