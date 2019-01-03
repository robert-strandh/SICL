(cl:in-package #:sicl-sequence)

;;; This condition is used to mix into other conditions that
;;; will report the construct (function, macro, etc) in which
;;; the condition was signaled.
(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))

;;; This condition is used by functions an macros that require
;;; some argument to be a nonnegative integer.
(define-condition must-be-nonnegative-integer (type-error name-mixin)
  ()
  (:default-initargs :expected-type '(integer 0)))

;;; This condition is used by functions and macros that require
;;; some argument to be a cons cell.
(define-condition must-be-cons (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'cons))

;;; This condition is used by functions and macros that require
;;; some argument to be a sequence.
(define-condition must-be-sequence (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'sequence))

;;; This condition is used by functions and macros that require
;;; some argument to be a list (a cons or nil).
(define-condition must-be-list (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some list to be a proper list.
(define-condition must-be-proper-list (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions that take :test and :test-not
;;; keyword arguments, and is signaled when both of those are given.
(define-condition both-test-and-test-not-given (error name-mixin)
  ())

;;; This condition is used by macros and compiler macrosthat detect
;;; that there is both a :test and a :test-not, and that detection is
;;; done at macro-expansion time.
(define-condition warn-both-test-and-test-not-given (warning name-mixin)
  ())

(define-condition invalid-sequence-index-type (type-error name-mixin)
  ())

(define-condition invalid-start-index-type (invalid-sequence-index-type)
  ())

(define-condition invalid-end-index-type (invalid-sequence-index-type)
  ())

;;; This is the base class of conditions that need to report
;;; some problem relative to a particular sequence.
(define-condition invalid-sequence-index (type-error name-mixin)
  ((%in-sequence :initarg :in-sequence :reader in-sequence)))

;;; This is the base class of conditions that need to report
;;; some bounding index to be out of bounds.
(define-condition invalid-bounding-index (invalid-sequence-index)
  ())

;;; This condition is used to indicate in invalid start index
;;; of some sequence, as given by the :start keyword.
(define-condition invalid-start-index (invalid-bounding-index)
  ())

;;; This condition is used to indicate in invalid end index
;;; of some sequence, as given by the :end keyword.
(define-condition invalid-end-index (invalid-bounding-index)
  ())

;;; This condition is used to indicate that, although both
;;; the start and the end indexes are valid bounding indexes
;;; separately, the end index is smaller than the start index.
;;; We reuse the datum in the type-error for the start index,
;;; and add a slot for the end index.
(define-condition end-less-than-start (invalid-bounding-index)
  ((end-index :initarg :end-index :reader end-index)))
