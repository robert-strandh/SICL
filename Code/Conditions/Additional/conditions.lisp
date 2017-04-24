(in-package #:sicl-additional-conditions)

;;;; Copyright (c) 2008, 2009, 2010, 2012, 2015
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

(define-condition sicl-warning (acclimation:condition warning) ())
(define-condition sicl-style-warning (acclimation:condition style-warning) ())
(define-condition sicl-error (acclimation:condition  error) ())
(define-condition sicl-type-error (acclimation:condition type-error) ())
(define-condition sicl-cell-error (acclimation:condition cell-error) ())
(define-condition sicl-unbound-variable (acclimation:condition unbound-variable) ())
(define-condition sicl-undefined-function (acclimation:condition undefined-function) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Runtime conditions. 

;;; This condition is used by functions that take :test and :test-not
;;; keyword arguments, and is signaled when both of those are given.
(define-condition both-test-and-test-not-given (sicl-error)
  ())

;;; This condition is used by the map* family functions when no lists
;;; were given, since those functions require at least one list
;;; argument.
(define-condition at-least-one-list-required (sicl-error)
  ())

;;; This condition is used by the list* function when no arguments
;;; were given, since that function requires at least one argument.
(define-condition at-least-one-argument-required (sicl-error)
  ())

;;; This condition is used by the pairlis function when 
;;; the two lists are not of the same length.
(define-condition lists-must-have-the-same-length (sicl-error)
  ((%list1 :initarg :list1 :reader list1)
   (%list2 :initarg :list2 :reader list2)))

;;; This condition is used by macros that detect that there
;;; is both a :test and a :test-not, and that detection is
;;; done at macro-expansion time. 
(define-condition warn-both-test-and-test-not-given (sicl-warning)
  ())

;;; This condition is used by sequence functions when a list has been
;;; given as the sequence, but the list has been found not to be a
;;; proper list.
(define-condition list-as-sequence-must-be-proper (sicl-type-error)
  ())

;;; Provide :initarg :SEQUENCE and a :reader SEQUENCE
(define-condition sequence-error-mixin ()
  ((%sequence :initarg :sequence :reader sequence)))

;;; This condition is used by sequence functions when the bounding
;;; indexes are invalid as a pair, but separately valid, typically,
;;; when START is greater than END.
(define-condition invalid-bouding-indexes (sicl-type-error sequence-error-mixin)
  ())

;;; This condition is used by sequence functions when the START
;;; bounding indexes is invalid.  This can be because it is not an
;;; integer, or because it is less than 0 or greater than the length
;;; of the sequence.
(define-condition invalid-start-index (sicl-type-error sequence-error-mixin)
  ())

;;; This condition is used by sequence functions when the END bounding
;;; indexes is invalid.  This can be because it is neither an integer
;;; nor NIL, or because it is less than 0 or greater than the length
;;; of the sequence.
(define-condition invalid-end-index (sicl-type-error sequence-error-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile time conditions. 

(define-condition sicl-program-error (acclimation:condition program-error)
  ())

(define-condition sicl-syntax-error (sicl-program-error)
  ((%code :initarg :code :reader code)))

(define-condition sicl-program-warning (acclimation:condition warning)
  ((%code :initarg :code :reader code)))

(define-condition sicl-program-style-warning (acclimation:condition style-warning)
  ((%code :initarg :code :reader code)))


(define-condition form-must-be-proper-list (sicl-syntax-error)
  ())

(define-condition block-tag-must-be-symbol (sicl-syntax-error)
  ())

(define-condition body-must-be-proper-list (sicl-syntax-error)
  ())

(define-condition multiple-documentation-strings-in-body (sicl-syntax-error)
  ())

(define-condition documentation-string-not-allowed-in-body (sicl-syntax-error)
  ())

(define-condition declarations-not-allowed-in-body (sicl-syntax-error)
  ())

(define-condition declaration-follows-form-in-body  (sicl-syntax-error)
  ())

(define-condition form-too-short (sicl-syntax-error)
  ((%min-length :initarg :min-length :reader min-length)))

(define-condition form-too-long (sicl-syntax-error)
  ((%max-length :initarg :max-length :reader max-length)))

(define-condition unknown-eval-when-situation (sicl-syntax-error)
  ())

(define-condition go-tag-must-be-symbol-or-integer (sicl-syntax-error)
  ())

(define-condition setq-must-have-even-number-arguments (sicl-syntax-error)
  ())

(define-condition setq-variable-must-be-symbol (sicl-syntax-error)
  ())

(define-condition tagbody-element-must-be-symbol-integer-or-compound-form
    (sicl-syntax-error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Argument mismatch conditions.

(define-condition argument-mismatch (sicl-program-error)
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%arguments :initarg :arguments :reader arguments)))

(define-condition too-few-arguments (argument-mismatch)
  ())

(define-condition too-many-arguments (argument-mismatch)
  ())

(define-condition unrecognized-keyword-argument (argument-mismatch)
  ((%keyword-argument
    :initarg :keyword
    :reader keyword-argument)))

(define-condition invalid-keyword-argument (argument-mismatch)
  ((%keyword-argument
    :initarg :keyword
    :reader keyword-argument)))

(define-condition odd-number-of-keyword-arguments (argument-mismatch)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLOS/MOP-related conditions. 

(define-condition no-such-class-name (sicl-type-error)
  ()
  (:default-initargs :type 'symbol))

(define-condition must-be-class-or-nil (sicl-type-error)
  ()
  (:default-initargs :type '(or class null)))

(define-condition superclass-list-must-be-proper-list
    (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-slots-list (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-slot-spec (sicl-type-error)
  ()
  (:default-initargs :type '(or symbol list)))

(define-condition illegal-slot-name (sicl-type-error)
  ()
  (:default-initargs :type 'symbol))

(define-condition slot-options-must-be-even (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition slot-option-name-must-be-symbol (sicl-type-error)
  ()
  (:default-initargs :type 'symbol))

(define-condition multiple-initform-options-not-permitted (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition multiple-documentation-options-not-permitted (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition multiple-allocation-options-not-permitted (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition multiple-type-options-not-permitted (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition slot-documentation-option-must-be-string (sicl-type-error)
  ()
  (:default-initargs :type 'string))

(define-condition class-option-must-be-non-empty-list (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition class-option-name-must-be-symbol (sicl-type-error)
  ()
  (:default-initargs :type 'symbol))

;;; FIXME: This doesn't feel like a type-error
(define-condition duplicate-class-option-not-allowed (sicl-type-error)
  ()
  (:default-initargs :type 'symbol))

(define-condition malformed-documentation-option (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-metaclass-option (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-default-initargs-option (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition default-initargs-option-once (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition documentation-option-once (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition metaclass-option-once (sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition unknown-class-option (sicl-type-error)
  ()
  (:default-initargs :type 'list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Warnings

(define-condition empty-body (sicl-style-warning)
  ())

(define-condition numeric-catch-tag (sicl-warning)
  ())

(define-condition deprecated-eval-when-situation (sicl-style-warning)
  ())

(define-condition load-time-value-read-only-p-not-evaluated
    (sicl-style-warning)
  ())

