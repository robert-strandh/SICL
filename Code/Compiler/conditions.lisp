(cl:in-package #:sicl-compiler)

(define-condition compile-time-warning (warning)
  ((%source-location
    :initform nil
    :initarg :source-location
    :reader source-location)))

(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))
