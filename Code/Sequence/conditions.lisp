(cl:in-package #:sicl-sequence)

(define-condition must-be-nonnegative-integer (type-error)
  ()
  (:default-initargs :expected-type '(integer 0)))

(define-condition must-be-cons (type-error)
  ()
  (:default-initargs :expected-type 'cons))

(define-condition must-be-sequence (type-error)
  ()
  (:default-initargs :expected-type 'sequence))

(define-condition must-be-function-designator (type-error)
  ()
  (:default-initargs :expected-type '(or function symbol)))

(define-condition must-be-list (type-error)
  ()
  (:default-initargs :expected-type 'list))

(define-condition must-be-proper-list (type-error)
  ()
  (:default-initargs :expected-type 'list))

(define-condition must-be-recognizable-subtype-of-sequence (type-error)
  ()
  (:default-initargs :expected-type 'sequence-type-specifier))

(define-condition must-be-recognizable-subtype-of-vector
    (must-be-recognizable-subtype-of-sequence)
  ())

(define-condition must-be-result-type (type-error)
  ())

;;; Index Handling

(define-condition invalid-sequence-index (type-error)
  ((%in-sequence :initarg :in-sequence :reader in-sequence)))

(define-condition invalid-bounding-index (invalid-sequence-index)
  ())

(define-condition invalid-start-index (invalid-bounding-index)
  ())

(define-condition invalid-end-index (invalid-bounding-index)
  ())

(define-condition end-less-than-start (invalid-bounding-index)
  ((%start-index :initarg :start-index :reader start-index)))

;;; Test and Test-Not Handling

(define-condition both-test-and-test-not-given (error)
  ())

(define-condition warn-both-test-and-test-not-given (warning)
  ())
