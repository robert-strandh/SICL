(cl:in-package #:sicl-type)

(defun typep (object type-specifier
              &optional (environment sicl-environment:*environment*))
  (ctypep object (specifier-ctype type-specifier environment)))
