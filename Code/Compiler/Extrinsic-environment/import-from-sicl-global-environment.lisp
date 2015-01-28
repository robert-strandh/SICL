(cl:in-package #:sicl-extrinsic-environment)

;;; Add every global environment function into the environment.
(defun import-from-sicl-global-environment (environment)
  (loop for symbol being each external-symbol in '#:sicl-global-environment
	when (fboundp symbol)
	  do (setf (sicl-env:fdefinition symbol environment)
		   (fdefinition symbol))
	when (fboundp `(setf ,symbol))
	  do (setf (sicl-env:fdefinition `(setf ,symbol) environment)
		   (fdefinition `(setf ,symbol)))))
