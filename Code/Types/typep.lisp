(cl:in-package #:cl-type)

(defun typep (object type-specifier)
  (generic-typep object
		 type-specifier
		 (load-time-value (sicl-genv:global-environment))))
