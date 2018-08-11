(cl:in-package #:sicl-evaluation-and-compilation)

(defun declaim-expander (declaration-specifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for declaration-specifier in declaration-specifiers
             collect `(proclaim ',declaration-specifier))))
