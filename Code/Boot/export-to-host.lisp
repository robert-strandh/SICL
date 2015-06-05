(cl:in-package #:sicl-boot)

(defun export-to-host (function-name from-environment)
  (setf (fdefinition function-name)
	(sicl-genv:fdefinition function-name from-environment)))
