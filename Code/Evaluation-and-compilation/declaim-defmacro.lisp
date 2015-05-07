(cl:in-package #:sicl-evaluation-and-compilation)

(defmacro declaim (&rest declaration-specifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (proclaim ',declaration-specifiers)))
