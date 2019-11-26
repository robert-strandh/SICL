(cl:in-package #:sicl-array)

(define-condition argument-to-array-displacement-must-be-an-array
    (type-error acclimation:condition)
  ())

(define-condition argument-to-array-element-type-must-be-an-array
    (type-error acclimation:condition)
  ())

(define-condition argument-to-array-row-major-index-must-be-an-array
    (type-error acclimation:condition)
  ())
