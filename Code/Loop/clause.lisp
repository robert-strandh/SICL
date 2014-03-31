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
;;; Clause NAME-CLAUSE.
;;;
;;; A NAME-CLAUSE is a clause that gives a name to the loop.  It
;;; translates to a block name, so that RETURN-FROM can be used to
;;; exit the loop.  By default, the name of the loop is nil.
;;;
;;; The name-clause is optional, and if present, must be the first one
;;; in the body.  The syntax is:
;;;
;;;    NAMED name
;;;
;;; where name is a symbol.

(defclass name-clause (clause)
  ((%name :initarg :name :reader name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-CLAUSE.
;;;
;;; The HyperSpec says that a FOR-AS-CLAUSE has the following syntax:
;;;
;;;    for-as-clause ::= {for | as} for-as-subclause {and for-as-subclause}* 
;;;    for-as-subclause::= for-as-arithmetic | for-as-in-list | 
;;;                        for-as-on-list | for-as-equals-then | 
;;;                        for-as-across | for-as-hash | for-as-package 
;;;
;;; For the purpose of specialization, we need different names for the
;;; main clauses as well as for the subclauses, so we alter this
;;; grammar a bit and define it like this instead:
;;;
;;;    for-as-clause::= 
;;;      for-as-arithmetic-clause | for-as-in-list-clause | 
;;;      for-as-on-list-clause | for-as-equals-then-clause | 
;;;      for-as-across-clause | for-as-hash-clause | for-as-package-clause
;;;    
;;;    for-as-arithmetic-clause ::=
;;;      {for | as} for-as-arithmetic {and for-as-subclause}* 
;;;    
;;;    for-as-in-list-clause ::=
;;;      {for | as} for-as-in-list {and for-as-subclause}* 
;;;    
;;;    for-as-on-list-clause ::=
;;;      {for | as} for-as-on-list {and for-as-subclause}* 
;;;    
;;;    for-as-equals-then-clause ::=
;;;      {for | as} for-as-equals-then {and for-as-subclause}* 
;;;    
;;;    for-as-across-clause ::=
;;;      {for | as} for-as-across {and for-as-subclause}* 
;;;
;;;    for-as-hash-clause ::=
;;;      {for | as} for-as-hash {and for-as-subclause}* 
;;;
;;;    for-as-package-clause ::=
;;;      {for | as} for-as-package {and for-as-subclause}* 

(defclass for-as-clause (clause subclause-mixin variable-clause-mixin) ())

(defclass for-as-subclause (var-and-type-spec-mixin)
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
;;; Clause FOR-AS-ARITHMETIC.

(defclass for-as-arithmetic-clause (for-as-clause) ())

(defclass for-as-arithmetic (for-as-subclause) ())

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
;;; Clauses FOR-AS-IN-LIST FOR-AS-ON-LIST
;;;
;;; The clause FOR-AS-IN-ON-LIST does not exist in the HyperSpec.  We
;;; use it as a common superclass.

(defclass for-as-in-on-list-clause (for-as-clause) ())

(defclass for-as-in-on-list (for-as-subclause) ())

(defclass for-as-in-list (for-as-in-on-list) ())

(defclass for-as-on-list (for-as-in-on-list) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR-AS-ACROSS-CLAUSE and FOR-AS-ACROSS

(defclass for-as-across-clause (for-as-clause) ())

(defclass for-as-across (for-as-subclause) 
  ((%array-form :initarg :array-form :reader array-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR-AS-EQUALS-THEN-CLAUSE and FOR-AS-EQUALS-THEN

(defclass for-as-equals-then-clause (for-as-clause) ())

(defclass for-as-equals-then (for-as-subclause)
  ((%form1 :initarg :form1 :reader form1)
   (%form2 :initarg :form2 :reader form2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR-AS-HASH-CLAUSE and FOR-AS-HASH

(defclass for-as-hash-clause (for-as-clause) ())

(defclass for-as-hash (for-as-subclause)
  ((%hash-table-form :initarg :hash-table-form :reader hash-table-form)
   (%other-var :initarg :other-var :reader other-var)))

(defclass for-as-hash-key (for-as-hash) ())

(defclass for-as-hash-value (for-as-hash) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR-AS-PACKAGE-CLAUSE and FOR-AS-PACKAGE

(defclass for-as-package-clause (for-as-clause) ())

(defclass for-as-package (for-as-subclause)
  ((%package-form :initarg :package-form :reader package-form)))

(defclass for-as-package-symbols (for-as-package) ())

(defclass for-as-package-present-symbols (for-as-package) ())

(defclass for-as-package-external-symbols (for-as-package) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accumulation clauses

(defclass accumulation-clause (clause main-clause-mixin)
  ())

(defclass accumulate-it-clause (accumulation-clause)
  ())

(defclass accumulate-form-clause (accumulation-clause)
  ((%form :initform nil :initarg :form :accessor form)))

(defclass accumulate-it-into-clause (accumulate-it-clause)
  ((%into-var :initform nil :initarg :into-var :accessor into-var)))

(defclass accumulate-form-into-clause (accumulate-form-clause)
  ((%into-var :initform nil :initarg :into-var :accessor into-var)))

(defclass list-accumulation-mixin () ())

(defclass numeric-accumulation-mixin () ())

