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

(define-condition number-of-indices-must-equal-array-rank
    (error acclimation:condition)
  ((%indices :initarg :indices :reader indices)
   (%array :initarg :array :reader given-array)))

(define-condition index-must-be-non-negative-and-less-than-dimension
    (type-error acclimation:condition)
  ((%array :initarg :array :reader given-array)
   (%index-number :initarg :index-number :reader index-number)))
