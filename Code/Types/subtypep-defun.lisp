(cl:in-package #:sicl-type)

(let ((global-environment (sicl-environment:global-environment)))
  (defun subtypep (type-specifier-1 type-specifier-2
                   &optional (environment global-environment))
    (ctype:subctypep (ctype:specifier-ctype type-specifier-1 environment)
                     (ctype:specifier-ctype type-specifier-2 environment))))
