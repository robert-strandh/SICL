(cl:in-package #:sicl-compiler-environment)

(defmacro declaim (&rest declaration-specifiers)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    `(progn ,@(mapcar #'(lambda (declaration-specifier)
			  `(proclaim ',declaration-specifier))
		      declaration-specifiers))))
