(in-package #:sicl-additional-conditions)

;;;; Copyright (c) 2008, 2009, 2010, 2012
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

;;; This condition is used to mix into other conditions that
;;; will report the construct (function, macro, etc) in which 
;;; the condition was signaled. 
(define-condition signaler-mixin ()
  ((%signaler :initform nil :initarg :signaler :reader signaler)))

(define-condition sicl-condition (signaler-mixin condition) ())
(define-condition sicl-warning (sicl-condition warning) ())
(define-condition sicl-style-warning (sicl-condition style-warning) ())
(define-condition sicl-error (sicl-condition  error) ())
(define-condition sicl-type-error (sicl-condition type-error) ())
(define-condition sicl-cell-error (sicl-condition cell-error) ())
(define-condition sicl-unbound-variable (sicl-condition unbound-variable) ())
(define-condition sicl-undefined-function (sicl-condition undefined-function) ())

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile time conditions. 

(define-condition sicl-program-error (sicl-condition program-error)
  ((%code :initarg :code :reader code)))

(define-condition sicl-program-warning (sicl-condition warning)
  ((%code :initarg :code :reader code)))

(define-condition sicl-program-style-warning (sicl-condition style-warning)
  ((%code :initarg :code :reader code)))


(define-condition form-must-be-proper-list (sicl-program-error)
  ())

(define-condition block-tag-must-be-symbol (sicl-program-error)
  ())

(define-condition body-must-be-proper-list (sicl-program-error)
  ())

(define-condition multiple-documentation-strings-in-body (sicl-program-error)
  ())

(define-condition documentation-string-not-allowed-in-body (sicl-program-error)
  ())

(define-condition declarations-not-allowed-in-body (sicl-program-error)
  ())

(define-condition declaration-follows-form-in-body  (sicl-program-error)
  ())

(define-condition form-too-short (sicl-program-error)
  ((%min-length :initarg :min-length :reader min-length)))

(define-condition form-too-long (sicl-program-error)
  ((%max-length :initarg :max-length :reader max-length)))

(define-condition unknown-eval-when-situation (sicl-program-error)
  ())

(define-condition go-tag-must-be-symbol-or-integer (sicl-program-error)
  ())

(define-condition setq-must-have-even-number-arguments (sicl-program-error)
  ())

(define-condition setq-variable-must-be-symbol (sicl-program-error)
  ())

(define-condition tagbody-element-must-be-symbol-integer-or-compound-form
    (sicl-program-error)
  ())

(define-condition lambda-list-must-not-be-circular (sicl-program-error)
  ())

(define-condition lambda-list-must-be-proper-list (sicl-program-error)
  ())

(define-condition lambda-list-keyword-not-allowed (sicl-program-error)
  ((%keyword :initarg :keyword :reader lambda-list-keyword)))

(define-condition lambda-list-keyword-not-allowed-in-dotted-lambda-list
    (sicl-program-error)
  ((%keyword :initarg :keyword :reader lambda-list-keyword)))

(define-condition multiple-occurrences-of-lambda-list-keyword
    (sicl-program-error)
  ((%keyword :initarg :keyword :reader lambda-list-keyword)))

(define-condition incorrect-keyword-order (sicl-program-error)
  ((%keyword1 :initarg :keyword1 :reader lambda-list-keyword1)
   (%keyword2 :initarg :keyword2 :reader lambda-list-keyword2)))

(define-condition both-rest-and-body-occur-in-lambda-list (sicl-program-error)
  ())

(define-condition whole-must-appear-first (sicl-program-error)
  ())

(define-condition whole-must-be-followed-by-variable (sicl-program-error)
  ())

(define-condition environment-must-be-followed-by-variable (sicl-program-error)
  ())

(define-condition environment-can-appear-at-most-once (sicl-program-error)
  ())

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

(define-condition suspect-lambda-list-keyword (sicl-style-warning)
  ((%keyword :initarg :keyword :reader lambda-list-keyword)))
