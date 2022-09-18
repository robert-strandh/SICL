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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Runtime conditions.

;;; Provide :initarg :SEQUENCE and a :reader SEQUENCE
(define-condition sequence-error-mixin ()
  ((%sequence :initarg :sequence :reader sequence)))

;;; This condition is used by sequence functions when the bounding
;;; indexes are invalid as a pair, but separately valid, typically,
;;; when START is greater than END.
(define-condition invalid-bouding-indexes (sicl-type-error sequence-error-mixin)
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
