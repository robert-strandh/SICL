(cl:in-package #:sicl-string)

(define-condition bag-is-dotted-list (type-error)
  ())

(define-condition bag-is-circular-list (type-error)
  ())

(define-condition bag-contains-non-character (type-error)
  ())
