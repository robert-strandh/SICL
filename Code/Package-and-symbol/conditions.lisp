(in-package #:sicl-package)

(define-condition package-error
    (error acclimation:condition)
  ())

(define-condition not-a-package-designator
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type '(or package character string symbol)))
