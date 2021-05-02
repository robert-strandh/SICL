(cl:in-package #:sicl-array)

(define-condition object-must-be-an-array
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

(define-condition row-major-index-must-be-non-negative-and-less-than-total-size
    (type-error acclimation:condition)
  ((%array :initarg :array :reader given-array)))

(define-condition incompatible-type-for-new-element
    (type-error acclimation:condition)
  ((%array :initarg :array :reader given-array)))
