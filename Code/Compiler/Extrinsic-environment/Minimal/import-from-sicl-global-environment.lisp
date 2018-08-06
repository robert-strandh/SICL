(cl:in-package #:sicl-minimal-extrinsic-environment)

(defun import-from-sicl-global-environment (environment)
  ;; This variable is deprecated.  Uses of it should be replaced by
  ;; uses of the constant sicl-genv:+global-environment+
  (setf (sicl-genv:special-variable 'sicl-env:*global-environment* environment t)
	environment)
  (setf (sicl-genv:constant-variable 'sicl-env:+global-environment+ environment)
	environment)
  (setf (sicl-genv:fdefinition 'sicl-env:global-environment environment)
	(lambda () environment)))
