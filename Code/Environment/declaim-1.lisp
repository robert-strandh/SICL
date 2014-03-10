(cl:in-package #:sicl-global-environment)

(defmacro declaim (&rest declaration-specifiers)
  `(eval-when (:compile-toplevel)
     (progn ,@(mapcar #'(lambda (declaration-specifier)
			  `(proclaim ',declaration-specifier))
		      declaration-specifiers))))
