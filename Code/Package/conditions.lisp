(in-package #:sicl-package)

(define-condition not-a-package-designator (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A package designator was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type '(or package character string symbol)))

(define-condition nicknames-must-be-proper-list (type-error)
  ()
  (:default-initargs :expected-type 'list))

(define-condition use-list-must-be-proper-list (type-error)
  ()
  (:default-initargs :expected-type 'list))

(define-condition package-error (error)
  ((%package :initarg :package :reader package-error-package)))

(define-condition symbol-conflict (package-error)
  ((%conflicting-symbols
    :initarg :conflicting-symbols
    :reader conflicting-symbols)))

(define-condition symbol-is-not-accessible (package-error)
  ((%symbol :initarg :symbol :reader inaccessible-symbol)))
