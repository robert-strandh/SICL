(cl:in-package #:sicl-loop)

(define-condition loop-runtime-error (error acclimation:condition)
  ())

(define-condition sum-argument-must-be-number (type-error loop-runtime-error)
  ())

(define-condition max-argument-must-be-real (type-error loop-runtime-error)
  ())

(define-condition min-argument-must-be-real (type-error loop-runtime-error)
  ())

(define-condition value-must-be-list (type-error loop-runtime-error)
  ())
