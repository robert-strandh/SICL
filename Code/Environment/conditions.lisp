(cl:in-package #:sicl-standard-environment-functions)

(define-condition variables-must-be-proper-list (program-error)
  ((%variables :initarg :variables :reader variables)))

(define-condition variable-must-be-symbol (program-error)
  ((%variable :initarg :variable :reader variable)))

(define-condition malformed-type-specifier (program-error)
  ((%type-specifier :initarg :type-specifier :reader type-specifier)))
