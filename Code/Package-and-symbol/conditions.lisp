(in-package #:sicl-package)

(define-condition package-error
    (error acclimation:condition)
  ())

(define-condition not-a-package-designator
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type '(or package character string symbol)))

(define-condition nicknames-must-be-proper-list
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type 'list))

(define-condition use-list-must-be-proper-list
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type 'list))

(define-condition symbol-conflict
    (package-error acclimation:condition)
  ((%conflicting-symbols
    :initarg :conflicting-symbols
    :reader conflicting-symbols)))
