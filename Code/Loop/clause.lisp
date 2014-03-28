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
   (%type-spec :initarg :type-spec :accessor type-spec)
   (%bindings :initform nil :accessor bindings)
   (%types :initform nil :accessor types)
   (%ignorables :initform nil :accessor ignorables)))

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
;;; Mixin for main clauses 
;;;
;;; The HyperSpec defines main-clause like this:
;;;
;;;    main-clause::= unconditional | accumulation | conditional |
;;;                   termination-test | initial-final              
;;; 
;;; The reason the HyperSpec defines main-clause is because the loop
;;; body is a name-clause followed by zero or more variable-clauses
;;; followed by zero or more main-clauses, and initial-final is one
;;; possibility both for variable-clause and main-clause.  This means
;;; that an initially-clause or a finally-clause can appear anywhere
;;; after a name-clause.

(defclass main-clause-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause NAME

(defclass name-clause (clause)
  ((%name :initarg :name :reader name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause WITH

(defclass with-clause (clause subclauses-mixin variable-clause-mixin) ())

(defclass with-subclause (var-and-type-spec-mixin)
  (;; The initialization form, or NIL if there is no form.
   (%form :initarg :form :reader form)
   ;; TRUE if and only if there is an explicit initialization form
   ;; present.
   (%form-present-p :initarg :form-present-p :reader form-present-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS

(defclass for/as-clause (clause subclause-mixin variable-clause-mixin) ())

(defclass for/as-subclause (var-and-type-spec-mixin)
  (;; The value of this slot is a list of bindings of the form
   ;; (<variable> <form>) where <variable> is a either the loop
   ;; variable associated with this subclause, or a symbol created by
   ;; GENSYM and <form> depends on the origin of the binding.
   (%bindings :initarg :bindings :reader bindings)
   ;; The value of this slot is either NIL, meaning that there is no
   ;; termination condition for this subclause, or a form to be
   ;; evaluated before the iteration of the loop starts.
   (%termination :initarg :termination :reader termination)
   (%step :initarg :step :reader step)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS arithmetic.

(defclass for/as-arithmetic-subclause (for/as-subclause)
  ())

;;; Every preposition has a form associated with it. 
(defclass preposition ()
  ((%form :initarg :form :reader form)))

;;; Some prepositions also have a variable associated with them that
;;; will hold the value of the form. 
(defclass with-var-mixin ()
  ((%var :initform (gensym) :reader var)))

(defclass preposition-first-group (preposition) ())
(defclass preposition-second-group (preposition with-var-mixin) ())
(defclass preposition-third-group (preposition with-var-mixin) ())

(defclass preposition-from (preposition-first-group) ())
(defclass preposition-downfrom (preposition-first-group) ())
(defclass preposition-upfrom (preposition-first-group) ())

(defclass preposition-to (preposition-second-group) ())
(defclass preposition-downto (preposition-second-group) ())
(defclass preposition-upto (preposition-second-group) ())
(defclass preposition-below (preposition-second-group) ())
(defclass preposition-above (preposition-second-group) ())

(defclass preposition-by (preposition-third-group) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS IN/ON

(defclass for/as-in-on-list-subclause (for/as-subclause)
  ())

(defclass for/as-in-list-subclause (for/as-in-on-list-subclause) ())

(defclass for/as-on-list-subclause (for/as-in-on-list-subclause) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS ACROSS

(defclass for/as-across-subclause (for/as-subclause) 
  ((%array-form :initarg :array-form :reader array-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR/AS = THEN

(defclass for/as-equals-then-subclause (for/as-subclause)
  ((%form1 :initarg :form1 :reader form1)
   (%form2 :initarg :form2 :reader form2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS hash

(defclass for/as-hash-subclause (for/as-subclause)
  ((%hash-table-form :initarg :hash-table-form :reader hash-table-form)
   (%other-var :initarg :other-var :reader other-var)))

(defclass for/as-hash-key-subclause (for/as-hash-subclause) ())

(defclass for/as-hash-value-subclause (for/as-hash-subclause) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS package

(defclass for/as-package-subclause (for/as-subclause)
  ((%package-form :initarg :package-form :reader package-form)))

(defclass for/as-package-symbols-subcause (for/as-package-subclause) ())

(defclass for/as-package-present-symbols-subcause (for/as-package-subclause) ())

(defclass for/as-package-external-symbols-subcause (for/as-package-subclause) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause INITIALLY

(defclass initially-clause
    (clause variable-clause-mixin main-clause-mixin compound-forms-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FINALLY

(defclass finally-clause
    (clause variable-clause-mixin main-clause-mixin compound-forms-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause DO

(defclass do-clause
    (clause main-clause-mixin compound-forms-mixin) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause RETURN

(defclass return-clause (clause main-clause-mixin)
  ((%form :initarg :form :reader form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses for accumulation

(defclass accumulation-clause (clause main-clause-mixin)
  ((%form :initform nil :initarg :form :accessor form)
   (%into-var :initform nil :initarg :into-var :accessor into-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses for list accumulation

(defclass list-accumulation-clause (accumulation-clause)
  ((%into-tail-var :initform nil :initarg :into-tail-var :accessor into-tail-var)))

(defclass collect-clause (list-accumulation-clause) ())
(defclass append-clause (list-accumulation-clause) ())
(defclass nconc-clause (list-accumulation-clause) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses for numeric accumulation.

(defclass numeric-accumulation-clause (accumulation-clause)
  ((%type-spec :initform t :initarg :type-spec :accessor type-spec)))

(defclass count-clause (numeric-accumulation-clause) ())
(defclass sum-clause (numeric-accumulation-clause) ())
(defclass maximize-clause (numeric-accumulation-clause) ())
(defclass minimize-clause (numeric-accumulation-clause) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditional clauses.

(defclass conditional-clause (clause main-clause)
  ((%test-form :initarg :test-form :reader test-form)
   (%then-clauses :initarg :then-clauses :reader then-clauses)
   (%else-clauses :initarg :else-clauses :reader else-clauses)))

(defclass if/when-clause (conditional-clause) ())

(defclass unless-clause (conditional-clause) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses for termination test.

(defclass termination-test (clause main-clause-mixin)
  ((%form :initarg :form :reader form)))

(defclass while-clause (termination-test) ())
(defclass until-clause (termination-test) ())
(defclass repeat-clause (termination-test var-and-type-spec-mixin) ())
(defclass always-clause (termination-test) ())
(defclass never-clause (termination-test) ())
(defclass thereis-clause (termination-test) ())

