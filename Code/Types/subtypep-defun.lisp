(cl:in-package #:sicl-type)

(defun subtypep (type-specifier-1 type-specifier-2
                 &optional (environment *environment*))
  (ctype:subctypep (ctype:specifier-ctype type-specifier-1 environment)
                   (ctype:specifier-ctype type-specifier-2 environment)))
