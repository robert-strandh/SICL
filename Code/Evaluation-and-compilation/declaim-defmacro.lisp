(cl:in-package #:sicl-evaluation-and-compilation)

(defmacro declaim (&rest declaration-specifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; can't use loop for bootstrap reasons
     ,@(mapcar (lambda (decl) `(proclaim ',decl))
	       declaration-specifiers)))
