(in-package #:sicl-package)

(define-condition not-a-package-designator
    (type-error acclimation:condition)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A package designator was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type '(or package character string symbol)))

(define-condition nicknames-must-be-proper-list
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type 'list))

(define-condition use-list-must-be-proper-list
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type 'list))

(define-condition package-error
    (error acclimation:condition)
  ((%package :initarg :package :reader package-error-package)))

(define-condition symbol-conflict
    (package-error acclimation:condition)
  ((%conflicting-symbols
    :initarg :conflicting-symbols
    :reader conflicting-symbols)))

(define-condition symbol-is-not-accessible
    (package-error acclimation:condition)
  ((%symbol :initarg :symbol :reader inaccessible-symbol)))
