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
