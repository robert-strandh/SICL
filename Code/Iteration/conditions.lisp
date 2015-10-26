(cl:in-package #:sicl-iteration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Externally visible conditions

;;; This condition is used to mix into other conditions that
;;; will report the construct (function, macro, etc) in which 
;;; the condition was signaled. 
(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))

(define-condition malformed-binding-var
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'symbol))

;;; This condition is used by functions and macros that require
;;; some argument to be a list (a cons or nil).
(define-condition malformed-list-form
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require 
;;; some argument to be a nonnegative integer. 
(define-condition malformed-count-form
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type '(integer 0)))

;;; This condition is used by functions and macros that require
;;; some list to be a proper list.  
(define-condition malformed-body
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'list))

(define-condition malformed-variable-clauses
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'list))

(define-condition malformed-variable-clause
    (program-error name-mixin cleavir-i18n:condition)
  ((%found :initarg :found :reader found)))

(define-condition malformed-end-test
    (program-error name-mixin cleavir-i18n:condition)
  ((%found :initarg :found :reader found)))
