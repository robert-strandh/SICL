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
(define-condition sicl-error (acclimation:condition error) ())
(define-condition sicl-type-error (acclimation:condition type-error) ())
(define-condition sicl-cell-error (acclimation:condition cell-error) ())
(define-condition sicl-undefined-function (acclimation:condition undefined-function) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Runtime conditions.

;;; This condition is used by functions that take :test and :test-not
;;; keyword arguments, and is signaled when both of those are given.
(define-condition both-test-and-test-not-given (sicl-error)
  ())

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Argument mismatch conditions.

(define-condition argument-mismatch (sicl-program-error)
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%arguments :initarg :arguments :reader arguments)))
