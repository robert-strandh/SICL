;;;; Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013, 2014
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

(cl:in-package #:sicl-loop)

(defgeneric report-loop-condition (condition stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions for parsing
;;;
;;; FIXME: Remove condition reporters from the DEFINE-CONDITION forms
;;; and put them in a separate (language-specific) file.  

(define-condition loop-parse-error (parse-error) ())

;;; Root class for loop parse errors that report something that was
;;; found, but should not be there.
(define-condition loop-parse-error-found (parse-error)
  ((%found :initarg :found :reader found)))

(define-condition expected-var-spec-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-var-spec-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-simple-var-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-simple-var-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-type-spec-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-type-spec-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-compound-form-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-compound-form-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-form-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-symbol-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-symbol-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-keyword-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-for/as-subclause-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-symbol-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-each/the-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-each/the-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-hash-or-package-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-hash-or-package-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-in/of-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-in/of-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-hash-key-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-hash-value-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition expected-hash-key-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-hash-value-but-found (loop-parse-error-found)
  ()
  (:report report-loop-condition))

(define-condition expected-preposition-but-end (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition too-many-prepositions-from-one-group (loop-parse-error)
  ()
  (:report report-loop-condition))

(define-condition conflicting-stepping-directions (loop-parse-error)
  ()
  (:report report-loop-condition))
