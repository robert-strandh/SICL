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

;;;; The terminology used here is that of the BNF grammar in the
;;;; dictionary description of the loop macro in the HyperSpec.  It is
;;;; not the same as the terminology used in the section 6.1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Common classes

;;; The base class of all clauses
(defclass clause () ())

;;; Mixin for clauses that accept `and'
(defclass subclauses-mixin ()
  ((%subclauses :initarg :subclauses :reader subclauses)))

;;; Mixin for clauses and subclauses that take
;;; a var-spec and a type-spec
(defclass var-and-type-spec-mixin ()
  ((%var-spec :initarg :var-spec :accessor var-spec)
   (%type-spec :initarg :type-spec :accessor type-spec)))

;;; Mixin for clauses that take a list of compound forms
(defclass compound-forms-mixin ()
  ((%forms :initarg :forms :reader forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for variable clauses
;;;
;;; The HyperSpec defines varible-clause like this:
;;;
;;;    variable-clause::= with-clause | initial-final | for-as-clause 
;;; 
;;; and we follow this example.  The reason the HyperSpec defines
;;; variable-clause is because the loop body is a name-clause followed
;;; by zero or more variable-clauses followed by zero or more
;;; main-clauses, and initial-final is one possibility both for
;;; variable-clause and main-clause.  This means that an
;;; initially-clause or a finally-clause can appear anywhere after a
;;; name-clause.

(defclass variable-clause-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that make the loop return a value.

(defclass loop-return-clause-mixin () ())
