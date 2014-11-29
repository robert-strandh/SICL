(cl:in-package #:sicl-standard-environment-functions)

(define-condition no-such-class (error)
  ((%name :initarg :name :reader name)))

(define-condition odd-number-of-arguments-to-setf (program-error)
  ((%form :initarg :form :reader form)))

(define-condition variables-must-be-proper-list (program-error)
  ((%variables :initarg :variables :reader variables)))
