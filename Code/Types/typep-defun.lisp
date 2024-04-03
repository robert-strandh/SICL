(cl:in-package #:sicl-type)

(defun typep (object type-specifier
              &optional (environment sicl-environment:*environment*))
  (ctype:ctypep object (ctype:specifier-ctype type-specifier environment)))
