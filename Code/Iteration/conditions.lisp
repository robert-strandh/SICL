(cl:in-package #:sicl-iteration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Externally visible conditions

(define-condition malformed-binding-var
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type 'symbol))

;;; This condition is used by functions and macros that require
;;; some argument to be a list (a cons or nil).
(define-condition malformed-list-form
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some argument to be a nonnegative integer.
(define-condition malformed-count-form
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type '(integer 0)))

;;; This condition is used by functions and macros that require
;;; some list to be a proper list.
(define-condition malformed-body
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type 'list))

(define-condition malformed-variable-clauses
    (type-error acclimation:condition)
  ()
  (:default-initargs :expected-type 'list))

(define-condition malformed-variable-clause
    (program-error acclimation:condition)
  ((%found :initarg :found :reader found)))

(define-condition malformed-end-test
    (program-error acclimation:condition)
  ((%found :initarg :found :reader found)))
