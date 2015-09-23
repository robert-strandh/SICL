(cl:in-package :sicl-cons-high)

;;;; Copyright (c) 2008 - 2013
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

;;;; This file is part of the cons-high module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file cons-high.text for a description of the module.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Externally visible conditions

;;; This condition is used to mix into other conditions that
;;; will report the construct (function, macro, etc) in which 
;;; the condition was signaled. 
(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))

;;; This condition is used by functions an macros that require 
;;; some argument to be a nonnegative integer. 
(define-condition must-be-nonnegative-integer
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type '(integer 0)))

;;; This condition is used by functions and macros that require
;;; some argument to be a cons cell.
(define-condition must-be-cons
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'cons))

;;; This condition is used by functions and macros that require
;;; some argument to be a list (a cons or nil).
(define-condition must-be-list
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some list to be a proper list.  
(define-condition must-be-proper-list
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some list to be either a proper list or a circular list.
(define-condition must-be-proper-or-circular-list
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require some
;;; list to be either a proper list or a dotted list (but not a
;;; circular list). 
(define-condition must-be-proper-or-dotted-list
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some argument to be a propterty list.
(define-condition must-be-property-list
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some argument to be a association list.
(define-condition must-be-association-list
    (type-error name-mixin cleavir-i18n:condition)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions that take :test and :test-not
;;; keyword arguments, and is signaled when both of those are given.
(define-condition both-test-and-test-not-given
    (error name-mixin cleavir-i18n:condition)
  ())

;;; This condition is used by the map* family functions when no lists
;;; were given, since those functions require at least one list
;;; argument.
(define-condition at-least-one-list-required
    (error name-mixin cleavir-i18n:condition)
  ())

;;; This condition is used by the list* function when no arguments
;;; were given, since that function requires at least one argument.
(define-condition at-least-one-argument-required
    (error name-mixin cleavir-i18n:condition)
  ())

;;; This condition is used by the pairlis function when 
;;; the two lists are not of the same length.
(define-condition lists-must-have-the-same-length
    (error name-mixin cleavir-i18n:condition)
  ((%list1 :initarg :list1 :reader list1)
   (%list2 :initarg :list2 :reader list2)))

;;; This condition is used by setf expanders for c*r when an
;;; object must be a cons cell but something else was found
(define-condition setf-c*r-must-be-cons (must-be-cons)
  ((%original-tree :initarg :original-tree :reader original-tree)
   (%access-string :initarg :access-string :reader access-string)))

;;; This condition is used by the setf expander for nth when an
;;; object must be a cons cell but something else was found
(define-condition setf-nth-must-be-cons (must-be-cons)
  ((%original-tree :initarg :original-tree :reader original-tree)
   (%cons-cell-count :initarg :cons-cell-count :reader cons-cell-count)))

;;; This condition is used by macros that detect that there
;;; is both a :test and a :test-not, and that detection is
;;; done at macro-expansion time. 
(define-condition warn-both-test-and-test-not-given (warning name-mixin)
  ())

(define-condition expected-list-with-at-least-n-elements
    (error cleavir-i18n:condition)
  ((%found :initarg :found :reader found)
   (%at-least :initarg :at-least :reader at-least)))
