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

;;;; This file is part of the conditionals module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file conditionals.text for a description of the module.

;;; This implementation also does not use the format function, and
;;; instead uses print and princ for error reporting.  This makes it
;;; possible for format to use the conditional constructs define here.

(in-package #:sicl-conditionals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at macro-expansion time

;;; This condition is used to mix into other conditions that
;;; will report the construct (function, macro, etc) in which 
;;; the condition was signaled. 
(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))

(define-condition malformed-body
    (program-error name-mixin cleavir-i18n:condition)
  ((%body :initarg :body :reader body)))
     
(define-condition malformed-cond-clauses
    (program-error name-mixin cleavir-i18n:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-cond-clause
    (program-error name-mixin cleavir-i18n:condition)
  ((%clause :initarg :clause :reader clause)))
     
(define-condition malformed-case-clauses
    (program-error name-mixin cleavir-i18n:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-case-clause
    (program-error name-mixin cleavir-i18n:condition)
  ((%clause :initarg :clause :reader clause)))
     
(define-condition otherwise-clause-not-last
    (program-error name-mixin cleavir-i18n:condition)
  ((%clauses :initarg :clauses :reader clauses)))

(define-condition malformed-keys
    (program-error name-mixin cleavir-i18n:condition)
  ((%keys :initarg :keys :reader keys)))
     
(define-condition malformed-typecase-clauses
    (program-error name-mixin cleavir-i18n:condition)
  ((%clauses :initarg :clauses :reader clauses)))
     
(define-condition malformed-typecase-clause
    (program-error name-mixin cleavir-i18n:condition)
  ((%clause :initarg :clause :reader clause)))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at runtime

(define-condition ecase-type-error
    (type-error name-mixin cleavir-i18n:condition)
  ())

(define-condition ccase-type-error
    (type-error name-mixin cleavir-i18n:condition)
  ())

(define-condition etypecase-type-error
    (type-error name-mixin cleavir-i18n:condition)
  ())

(define-condition ctypecase-type-error
    (type-error name-mixin cleavir-i18n:condition)
  ())

