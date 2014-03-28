;;;; Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;; 
;;;; Copyright (c) 2010, 2011
;;;;
;;;;     Matthieu Villeneuve (matthieu.villeneuve@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the LOOP module of the SICL project.
;;;; See the file SICL.text for a description of the project. 

;;;; The LOOP module provides a portable implementation of the LOOP
;;;; macro as described in the Common Lisp HyperSpec.

;;;; This implementation does not use any iteration construct.
;;;; Depending on the use case for this module, using iteration
;;;; constructs to implement LOOP may or may not be a problem.
;;;;
;;;; If this module is used as part of a new implementation (the
;;;; TARGET) to be bootstrapped from an existing conforming Common
;;;; Lisp implementation (the HOST), then any iteration construct
;;;; could be used, including LOOP, simply because the host
;;;; implementation of these constructs would be used to cross-compile
;;;; target files using LOOP.
;;;;
;;;; On the other hand, if this module is to be added to an existing
;;;; Common Lisp implementation, perhaps in order to obtain a better
;;;; implementation of LOOP, or perhaps because it is a partial
;;;; implementation that does not yet have a LOOP macro, then it is
;;;; important that this module not use LOOP itself.  We decide to go
;;;; one step further by also not using any other iteration
;;;; constructs, so that those iteration constructs could be
;;;; implemented by using the definition of LOOP in this module.

(cl:in-package #:sicl-loop)

(declaim (optimize (debug 3)))

;;; The terminology used here is that of the BNF grammar in the
;;; dictionary description of the loop macro in the HyperSpec.  It is
;;; not the same as the terminology used in the section 6.1. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities

;;; Loop keywords are symbols, but they are not recognized by symbol
;;; identity as is usually the case, but instead by their names.  The
;;; HyperSpec doesn't say what function to use for comparing the
;;; names.  We assume string= here, meaning that the names are case 
;;; sensitive. 

(defun symbol-equal (symbol1 symbol2)
  (and (symbolp symbol1)
       (string= symbol1 symbol2)))

;;; A parser is a function that takes a list of arbitrary Lisp objects
;;; and returns two values, the result of the parse and the remainder
;;; of the list that needs to be parsed.  The second return value is
;;; identical to the list of Lisp objects that it received as an
;;; argument if and only if the parser failed to parse the list of
;;; objects.  In that case, the first return value is not specified.

;;; Returns a list of clauses parsed by repeated invocations of the
;;; parser.
(defun parse-sequence (body parser)
  (multiple-value-bind (clause rest1)
      (funcall parser body)
    (if (eq rest1 body)
	;; Failed parsing the body according to the parser we
	;; received.  Indicate failure by returning the empty list and
	;; the same body as we recieved as argument.
	(values '() body)
	;; We succeeded parsing the body according to the parser we
	;; received.  Parse the remainder of the body recursively, and
	;; return a list of the results returned by the parser we
	;; received.
	(multiple-value-bind (clauses rest2)
	    (parse-sequence rest1 parser)
	  (values (cons clause clauses) rest2)))))

;;; Try alternative parsers and return the result of the first one
;;; that succeeds.  If none of the parsers succeeds, then indicate
;;; failure by returning nil and the body unchanged.
(defun parse-alternative (body &rest parsers)
  (if (null parsers)
      (values nil body)
      (multiple-value-bind (clause rest)
	  (funcall (car parsers) body)
	(if (not (eq body rest))
	    (values clause rest)
	    (apply #'parse-alternative body (rest parsers))))))

;;; An elementary parser is one that takes a list of tokens as an
;;; argument, checks that the first element of that list is one of a
;;; set of symbols given and if so, executes some code and returns the
;;; values of that execution, and if not returns two values: nil and
;;; the list unchanged.
;;;
;;; This macro helps avoid duplicated boilerplate code by automating
;;; the check for whether the first element of the list is in the set.
(defmacro define-elementary-parser (name body-var start-symbols &body body)
  `(defun ,name (,body-var)
     (when (or (null ,body-var)
	       (and ,@(mapcar (lambda (start-symbol)
				`(not (symbol-equal (car ,body-var)
						    ',start-symbol)))
			      start-symbols)))
       (return-from ,name (values nil ,body-var)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a d-var-spec and a type-spec

;;; A d-var-spec is a is a destructuring variable specifier: 
;;; 
;;;    d-var-spec ::= simple-var | nil | (d-var-spec . d-var-spec)
;;;
;;; where simple-var is a symbol (a name of a variable). 
;;;
;;; The syntax of a type-spec is:
;;;
;;;    type-spec ::= simple-type-spec | destructured-type-spec
;;;    simple-type-spec ::= fixnum | float | t | nil
;;;    destructured-type-spec ::= of-type d-type-spec
;;;    d-type-spec ::= type-specifier | (d-type-spec . d-type-spec)

;;; Parse a d-var-spec.
(defun parse-d-var-spec (body)
  (if (null body)
      (error 'expected-var-spec-but-end)
      (values (car body) (cdr body))))

;;; Parse a type-spec
(defun parse-type-spec (body)
  (cond ((null body)
	 (values nil body))
	((member (car body) '(fixnum float t nil))
	 (values (car body) (cdr body)))
	((symbol-equal (car body) '#:of-type)
	 (if (null (cdr body))
	     (error 'expected-type-spec-but-end)
	     (values (cadr body) (cddr body))))
	(t
	 (values nil body))))

;;; Parse a d-var-spec followed by a type-spec
(defun parse-var-and-type-spec (body)
  (multiple-value-bind (d-var-spec rest1)
      (parse-d-var-spec body)
    (multiple-value-bind (type-spec rest2)
	(parse-type-spec rest1)
      (values d-var-spec type-spec rest2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse any form

(defun parse-form (body)
  (if (null body)
      (error 'expected-form-but-end)
      (values (car body) (cdr body))))

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

;;; Mixin for variable clauses
(defclass variable-clause-mixin () ())

;;; Mixin for main clauses 
(defclass main-clause-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a name-clause
;;;
;;; A name-clause is a clause that gives a name to the loop.  It
;;; translates to a block name, so that return-from can be used to
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

(define-elementary-parser parse-name-clause body (#:named)
  (cond ((null (cdr body))
	 (error 'expected-symbol-but-end))
	((not (symbolp (cadr body)))
	 (error 'expected-symbol-but-found
		:found (cadr body)))
	(t
	 (values (make-instance 'name-clause
		   :name (cadr body))
		 (cddr body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a with-clause
;;;
;;; A with-clause allows the creation of local variables.  It is
;;; executed once. 
;;;
;;; The syntax of a with-clause is:
;;;
;;;    with-clause ::= WITH var1 [type-spec] [= form1] 
;;;                    {AND var2 [type-spec] [= form2]}*
;;;
;;; where var1 and var2 are destructuring variable specifiers
;;; (d-var-spec) allowing multiple local variables to be created in a
;;; single with-clause by destructuring the value of the corresponding
;;; form.
;;;
;;; When there are several consecutive with-claues, the execution is
;;; done sequentially, so that variables created in one with-clause
;;; can be used in the forms of subsequent with-clauses.  If parallel
;;; creation of variables is wanted, then the with-clause can be
;;; followed by one or more and-clauses. 
;;;
;;; The (destructuring) type specifier is optional.  If no type
;;; specifier is given, it is as if t was given. 
;;;
;;; The initialization form is optional.  If there is a corresponding
;;; type specifier for a variable, but no initialization form, then
;;; the variable is initialized to a value that is appropriate for the
;;; type.  In particular, for the type t the value is nil, for the
;;; type number, the value is 0, and for the type float, the value is
;;; 0.0.  

(defclass with-clause (clause subclauses-mixin variable-clause-mixin) ())

(defclass with-subclause (var-and-type-spec-mixin)
  (;; The initialization form, or NIL if there is no form.
   (%form :initarg :form :reader form)
   ;; TRUE if and only if there is an explicit initialization form
   ;; present.
   (%form-present-p :initarg :form-present-p :reader form-present-p)))

(define-elementary-parser parse-and-with-subclause body (#:and)
  (multiple-value-bind (var type-spec rest1)
      (parse-var-and-type-spec (cdr body))
    (if (or (null rest1)
	    (not (symbol-equal (car rest1) '#:=)))
	(values
	 (make-instance 'with-subclause
	   :var-spec var
	   :type-spec type-spec
	   :form nil
	   :form-present-p nil)
	 rest1)
	;; Else, there is a `='
	(multiple-value-bind (form rest2)
	    (parse-form (cdr rest1))
	  (values
	   (make-instance 'with-subclause
	     :var-spec var
	     :type-spec type-spec
	     :form form
	     :form-present-p t)
	   rest2)))))

(defun parse-with-subclauses (body)
  (parse-sequence body #'parse-and-with-subclause))

(define-elementary-parser parse-with-clause body (#:with)
  (multiple-value-bind (subclauses rest)
      (parse-with-subclauses (cons 'and (cdr body)))
    (values (make-instance 'with-clause
	      :subclauses subclauses)
	    rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS
;;;
;;; A for/as clause has the following syntax:
;;;
;;;    for-as-clause ::= {for | as} for-as-subclause {and for-as-subclause}* 
;;;    for-as-subclause::= for-as-arithmetic | for-as-in-list | 
;;;                        for-as-on-list | for-as-equals-then | 
;;;                        for-as-across | for-as-hash | for-as-package 

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

;;; Parse a FOR/AS subclause starting with AND.  In order to give a
;;; reasonable error message, we parse the VAR and the TYPE-SPEC
;;; first.  We then try each FOR/AS parser in sequence passing them
;;; the original list as opposed to the remaining list after parsing
;;; VAR and TYPE-SPEC.  If all of them fail, there must be something
;;; unacceptable following the VAR and the TYPE-SPEC.  By passing the
;;; original list, we give each FOR/AS parser access to the VAR and
;;; the TYPE-SPEC.  The downside of this method is that we parse the
;;; VAR and the TYPE-SPEC once in each individual FOR/AS parser.
(define-elementary-parser parse-and-for/as-subclause body (#:and)
  ;; All for/as clauses start with a variable and an optional type
  ;; spec.  Start by parsing them.
  (multiple-value-bind (var type-spec rest1)
      (parse-var-and-type-spec (cdr body))
    (if (null rest1)
	;; After parsing the variable and the optional type-spec, we
	;; are now at the end of the list, which is incorrect syntax. 
	(error 'expected-for/as-subclause-but-end)
	;; Try each FOR/AS parser until one succeeds.  We pass each
	;; parser the original list (with the initial AND removed) so
	;; they have access to the variable and the type-spec.
	(multiple-value-bind (subclause rest2)
	    (parse-alternative (cdr body)
			       #'parse-for/as-in
			       #'parse-for/as-on
			       #'parse-for/as-equals-then
			       #'parse-for/as-across
			       #'parse-for/as-hash/package
			       #'parse-for/as-arithmetic)
	  (if (eq rest2 body)
	      (error 'expected-for/as-subclause-but-found
		     :found (car rest1))
	      (values subclause rest2))))))

(define-elementary-parser parse-for/as-clause body (#:for #:as)
  (multiple-value-bind (subclauses rest)
      (parse-sequence (cons 'and (cdr body))
		      #'parse-and-for/as-subclause)
    (values (make-instance 'for/as-clause
	      :subclauses subclauses)
	    rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS arithmetic.
;;;
;;;    for-as-arithmetic::= var [type-spec] for-as-arithmetic-subclause 
;;;    for-as-arithmetic-subclause::= arithmetic-up | arithmetic-downto | 
;;;                                   arithmetic-downfrom 
;;;    arithmetic-up::= [[{from | upfrom} form1 | 
;;;                       {to | upto | below} form2 | 
;;;                       by form3]]+ 
;;;    arithmetic-downto::= [[{{from form1}} | 
;;;                           {{{downto | above} form2}} |
;;;                           by form3]] 
;;;    arithmetic-downfrom::= [[{{downfrom form1}} | 
;;;                             {to | downto | above} form2 |
;;;                             by form3]] 
;;;
;;; This grammar is an approximation because there are additional
;;; constraints that can not be expressed as a context-free grammar.
;;; From section 6.1.2.1.1 in the HyperSpec, we learn that a clause is
;;; introduced by one of the following prepositions: FROM, DOWNFROM,
;;; UPFROM, TO, DOWNTO, UPTO, BELOW, ABOVE, and BY.  These
;;; prepositions are divided into three groups as follows:
;;;
;;;    Group 1: FROM, DOWNFROM, UPFROM
;;;             Prepositions in this group indicate initial value of the 
;;;             loop variable.  If no preposition from this group is present,
;;;             then the initial value is 0.
;;;
;;;    Group 2: TO, DOWNTO, UPTO, BELOW, ABOVE
;;;             Prepositions in this group indicate iteration termination 
;;;             in the form of a VALUE (the value of the form) and a
;;;             comparison operator (<=, >=, <, >).  If no preposition from
;;;             this group is present, then no test for termination is made.
;;;
;;;    Group 3: BY
;;;             This preposition indicates a step increment.  The value
;;;             must be a positive number.  If this preposition is not
;;;             present, the default step increment is 1. 
;;;
;;; From section 6.1.2.1.1 we also learn that at least one preposition
;;; must be used, and at most one from each group is allowed in a
;;; single subclause.  As far as syntax is concerned, the order in
;;; which the prepositions appear is not important.  However, the
;;; order influences the result because evaluation is left-to-right as
;;; usual.
;;;
;;; As the grammar indicates, not every combination of prepositions is
;;; allowed.  In particular, stepping direction is either INCREMENTAL
;;; or DECREMENTAL, and some prepositions determine this direction.
;;; It is then now allowed to have two prepositions that indicate
;;; opposite stepping directions.  The prepositions FROM, TO, and BY
;;; are neutral when it comes to stepping direction, and if only
;;; neutral prepositions are used, then the stepping direction is
;;; incremental by default.  The prepositions UPFROM, UPTO, and BELOW
;;; indicate that the stepping direction is incremental.  The
;;; prepositions DOWNFROM, downto, and ABOVE indicate that the
;;; stepping direction is decremental.

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

(define-elementary-parser parse-preposition-from body (#:from)
  (values (make-instance 'preposition-from :form (cadr body))
	  (cddr body)))

(define-elementary-parser parse-preposition-downfrom body (#:downfrom)
  (values (make-instance 'preposition-downfrom :form (cadr body))
	  (cddr body)))

(define-elementary-parser parse-preposition-upfrom body (#:upfrom)
  (values (make-instance 'preposition-upfrom :form (cadr body))
	  (cddr body)))

(define-elementary-parser parse-preposition-to body (#:to)
  (values (make-instance 'preposition-to :form (cadr body))
	  (cddr body)))

(define-elementary-parser parse-preposition-downto body (#:downto)
  (values (make-instance 'preposition-downto :form (cadr body))
	  (cddr body)))

(define-elementary-parser parse-preposition-upto body (#:upto)
  (values (make-instance 'preposition-upto :form (cadr body))
	  (cddr body)))

(define-elementary-parser parse-preposition-below body (#:below)
  (values (make-instance 'preposition-below :form (cadr body))
	  (cddr body)))

(define-elementary-parser parse-preposition-above body (#:above)
  (values (make-instance 'preposition-above :form (cadr body))
	  (cddr body)))

(define-elementary-parser parse-preposition-by body (#:by)
  (values (make-instance 'preposition-by :form (cadr body))
	  (cddr body)))

(defun parse-preposition (body)
  (parse-alternative body
		     #'parse-preposition-from
		     #'parse-preposition-downfrom
		     #'parse-preposition-upfrom
		     #'parse-preposition-to
		     #'parse-preposition-upto
		     #'parse-preposition-below
		     #'parse-preposition-above
		     #'parse-preposition-by))

;;; In order to give pertinent error messages in case the various
;;; constraints are not respected, we first attempt to parse any
;;; number of prepositions that occur.  Only later do we check the
;;; constraints on those prepositions. 
(defun parse-prepositions (body)
  (parse-sequence body #'parse-preposition))

;;; Generate a binding from a preposition.  If the preposition is in
;;; the first group, the LOOP-VAR becomes the variable in the binding.
;;; Otherwise the binding itself contains the variable. 
(defun generate-binding (loop-var preposition)
  (if (typep preposition 'preposition-first-group)
      `(,(if (typep preposition 'preposition-first-group)
	     loop-var
	     (var preposition))
	,(form preposition))))

;;; For a list of prepositions, already checked for errors, return
;;; TRUE if and only if they imply that stepping is decremental. 
(defun decremental-stepping-p (prepositions)
  (or (member-if (lambda (x) (typep x 'preposition-downfrom)) prepositions)
      (member-if (lambda (x) (typep x 'preposition-downto)) prepositions)
      (member-if (lambda (x) (typep x 'preposition-above)) prepositions)))

(defun make-for/as-arithmetic-subclause (loop-var prepositions)
  (let ((g1 (find-if (lambda (p) (typep preposition 'preposition-first-group))
		     prepositions))
	(g2 (find-if (lambda (p) (typep preposition 'preposition-second-group))
		     prepositions))
	(g3 (find-if (lambda (p) (typep preposition 'preposition-third-group))
		     prepositions))
	(bindings (mapcar (lambda (preposition)
			    (generate-binding loop-var preposition))
			  prepositions)))
    (if (decremental-stepping-p prepositions)
	(make-instance 'for/as-arithmetic-subclause
	  :bindings bindings
	  :termination (etypecase g2
			 (null
			  nil)
			 ((to downto)
			  `(when (< ,loop-var ,(var g2))
			     (loop-finish)))
			 (above
			  `(when (<= ,loop-var ,(var g2))
			     (loop-finish))))
	  :step `(decf ,loop-var ,(if (null g3) 1 (var g3))))
	(make-instance 'for/as-arithmetic-subclause
	  :bindings bindings
	  :termination (etypecase g2
			 (null
			  nil)
			 ((to upto)
			  `(when (> ,loop-var ,(var g2))
			     (loop-finish)))
			 (below
			  `(when (>= ,loop-var ,(var g2))
			     (loop-finish))))
	  :step `(incff ,loop-var ,(if (null g3) 1 (var g3)))))))
  

;;; Parse a FOR/AS arithmetic subclause.  The list we are given starts
;;; with the variable and the optional type-spec.
(defun parse-for/as-arithmetic (body)
  (multiple-value-bind (var type-spec rest1)
      (parse-var-and-type-spec body)
    ;; FIXME: handle type-spec.
    (declare (ignore type-spec))
    (multiple-value-bind (prepositions rest2)
	(parse-prepositions rest1)
      ;; check the prepositions
      (cond ((null prepositions)
	     (error 'expected-preposition-but-end))
	    ((or (< 1 (count 'preposition-first-group
			     prepositions
			     :test (lambda (x y) (typep y x))))
		 (< 1 (count 'preposition-second-group
			     prepositions
			     :test (lambda (x y) (typep y x))))
		 (< 1 (count 'preposition-third-group
			     prepositions
			     :test (lambda (x y) (typep y x)))))
	     (error 'too-many-prepositions-from-one-group))
	    ((or (and (member-if (lambda (x) (typep x 'preposition-downfrom))
				 prepositions)
		      (or (member-if (lambda (x) (typep x 'preposition-below))
				     prepositions)
			  (member-if (lambda (x) (typep x 'preposition-upto))
				     prepositions)))
		 (and (member-if (lambda (x) (typep x 'preposition-upfrom))
				 prepositions)
		      (or (member-if (lambda (x) (typep x 'preposition-above))
				     prepositions)
			  (member-if (lambda (x) (typep x 'preposition-downto))
				     prepositions))))
	     (error 'conflicting-stepping-directions))
	    (t (values (make-for/as-arithmetic-subclause var prepositions)
		       rest2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS IN/ON
;;;
;;;    for-as-in-list::= var [type-spec] in form1 [by step-fun] 
;;;    for-as-on-list::= var [type-spec] on form1 [by step-fun] 

(defclass for/as-in-on-list-subclause (for/as-subclause)
  ())

(defclass for/as-in-list-subclause (for/as-in-on-list-subclause) ())

(defclass for/as-on-list-subclause (for/as-in-on-list-subclause) ())

;;; Recall that a d-var-spec is either a simple variable (i.e., a
;;; symbol), NIL, or a CONS of two d-var-spec.  This function takes
;;; the non-NIL symbols and replace each one with the result of
;;; calling GENSYM.  It returns two values, the first value is the
;;; modified d-var-spec, and the second value is an alist where the
;;; CAR of each element is the original symbol in the d-var-spec and
;;; the CDR is the corresponding GENSYMed symbol that replaced it. 
(defun fresh-variables (d-var-spec)
  (let ((dictionary '()))
    (labels ((traverse (tree)
	       (cond ((null tree) nil)
		     ((symbolp tree)
		      (let ((new (gensym)))
			(push (cons tree new) dictionary)
			new))
		     ((consp tree)
		      (cons (traverse (car tree)) (traverse (cdr tree))))
		     (t
		      ;; FIXME: replace with a specific condition
		      (error "not a valid d-var-spec ~s" tree)))))
      (values (traverse d-var-spec) dictionary))))

;;; This function is called when we have already
;;; seen `for/as var in/on'.
(defun parse-for/as-in/on (body class-name)
  (multiple-value-bind (form rest1)
      (parse-form body)
    (if (or (null rest1)
	    (not (symbol-equal (car rest1) '#:by)))
	(values (make-instance class-name
		  :list-form form
		  :step-fun-form #'cdr)
		rest1)
	(multiple-value-bind (step-fun-form rest2)
	    (parse-form (cdr rest1))
	  (values (make-instance class-name
		    :list-form form
		    :step-fun-form step-fun-form)
		  rest2)))))

;;; Parse a FOR/AS IN/ON subclause.  The list we are given starts
;;; with the variable and the optional type-spec.
(defun parse-for/as-in/on (body)
  (multiple-value-bind (var type-spec rest1)
      (parse-var-and-type-spec body)
    ;; FIXME: handle type-spec.
    (declare (ignore type-spec))
    (cond ((symbol-equal (car rest1) '#:in)
	   (parse-for/as-in/on (cdr body) 'for/as-in-list-subclause var))
	  ((symbol-equal (car rest1) '#:on)
	   (parse-for/as-in/on (cdr body) 'for/as-on-list-subclause var))
	  (t
	   (values nil body)))))
	  
(defun parse-for/as-in (body)
  (multiple-value-bind (var type-spec rest1)
      (parse-var-and-type-spec body)
    ;; FIXME: handle type-spec.
    (declare (ignore type-spec))
    (if (symbol-equal (car rest1) '#:in)
	(
  

(define-elementary-parser parse-for/as-in body (#:in)
  (parse-for/as-in/on (cdr body) 'for/as-in-list-subclause))

(define-elementary-parser parse-for/as-on body (#:on)
  (parse-for/as-in/on (cdr body) 'for/as-on-list-subclause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS ACROSS
;;;
;;;    for-as-across::= var [type-spec] across vector 

(defclass for/as-across-subclause (for/as-subclause) 
  ((%array-form :initarg :array-form :reader array-form)))

(define-elementary-parser parse-for/as-across body (#:across)
  (multiple-value-bind (form rest)
      (parse-form body)
    (values (make-instance 'for/as-across-subclause
	      :array-form form)
	    rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR/AS = THEN
;;;
;;;    for-as-equals-then::= var [type-spec] = form1 [then form2] 

(defclass for/as-equals-then-subclause (for/as-subclause)
  ((%form1 :initarg :form1 :reader form1)
   (%form2 :initarg :form2 :reader form2)))

(define-elementary-parser parse-for/as-equals-then body (#:=)
  (multiple-value-bind (form1 rest1)
      (parse-form (cdr body))
    (if (or (null rest1)
	    (not (symbol-equal (car rest1) '#:then)))
	(values (make-instance 'for/as-equals-then-subclause
		  :form1 form1
		  :form2 form1)
		rest1)
	(multiple-value-bind (form2 rest2)
	    (parse-form (cdr rest1))
	  (values (make-instance 'for/as-equals-then-subclause
		    :form1 form1
		    :form2 form2)
		  rest2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS hash
;;;
;;;    for-as-hash::= var [type-spec] being {each | the}  
;;;               {{hash-key | hash-keys} {in | of} hash-table  
;;;                [using (hash-value other-var)] |  
;;;                {hash-value | hash-values} {in | of} hash-table  
;;;                [using (hash-key other-var)]} 

(defclass for/as-hash-subclause (for/as-subclause)
  ((%hash-table-form :initarg :hash-table-form :reader hash-table-form)
   (%other-var :initarg :other-var :reader other-var)))

(defclass for/as-hash-key-subclause (for/as-hash-subclause) ())

(defclass for/as-hash-value-subclause (for/as-hash-subclause) ())

(define-elementary-parser parse-for/as-hash-keys body (#:hash-key #:hash-keys)
  (pop body)
  (let ((hash-table-form nil))
    (cond ((null body)
	   (error 'expected-in/of-but-end))
	  ((not (or (symbol-equal (car body) '#:in)
		    (symbol-equal (car body) '#:of)))
	   (error 'expected-in/of-but-found
		  :found (car body)))
	  ((null (progn (pop body) body))
	   (error 'expected-form-but-end))
	  (t
	   (setf hash-table-form (car body))
	   (pop body)
	   (cond ((or (null body)
		      (not (symbol-equal (car body) '#:using)))
		  (values (make-instance 'for/as-hash-key-subclause
			    :hash-table-form hash-table-form
			    :other-var nil)
			  body))
		 ((null (cdr body))
		  (error 'expected-hash-value-but-end))
		 (t
		  (let ((using-arg (car body)))
		    (if (or (not (consp using-arg))
			    (not (symbol-equal (car using-arg) '#:has-value))
			    (not (consp (cdr using-arg)))
			    (not (null (cddr using-arg)))
			    (not (symbolp (cadr using-arg))))
			(error 'expected-hash-value-but-found
			       :found using-arg)
			(values (make-instance 'for/as-hash-key-subclause
				  :hash-table-form hash-table-form
				  :other-var (cadr using-arg))
				(cdr body))))))))))

(define-elementary-parser parse-for/as-hash-values body (#:hash-value #:hash-values)
  (pop body)
  (let ((hash-table-form nil))
    (cond ((null body)
	   (error 'expected-in/of-but-end))
	  ((not (or (symbol-equal (car body) '#:in)
		    (symbol-equal (car body) '#:of)))
	   (error 'expected-in/of-but-found
		  :found (car body)))
	  ((null (progn (pop body) body))
	   (error 'expected-form-but-end))
	  (t
	   (setf hash-table-form (car body))
	   (pop body)
	   (cond ((or (null body)
		      (not (symbol-equal (car body) '#:using)))
		  (values (make-instance 'for-as-hash-value-subclause
			    :hash-table-form hash-table-form
			    :other-var nil)
			  body))
		 ((null (cdr body))
		  (error 'expected-hash-key-but-end))
		 (t
		  (let ((using-arg (car body)))
		    (if (or (not (consp using-arg))
			    (not (symbol-equal (car using-arg) '#:has-key))
			    (not (consp (cdr using-arg)))
			    (not (null (cddr using-arg)))
			    (not (symbolp (cadr using-arg))))
			(error 'expected-hash-key-but-found
			       :found using-arg)
			(values (make-instance 'for/as-hash-value-subclause
				  :hash-table-form hash-table-form
				  :other-var (cadr using-arg))
				(cdr body))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR/AS package
;;;
;;;    for-as-package::= var [type-spec] being {each | the}  
;;;                      {symbol | symbols | 
;;;                      present-symbol | present-symbols | 
;;;                      external-symbol | external-symbols} 
;;;                      [{in | of} package] 

(defclass for/as-package-subclause (for/as-subclause)
  ((%package-form :initarg :package-form :reader package-form)))

(defclass for/as-package-symbols-subcause (for/as-package-subclause) ())

(defclass for/as-package-present-symbols-subcause (for/as-package-subclause) ())

(defclass for/as-package-external-symbols-subcause (for/as-package-subclause) ())

;;; This function is called when we have already seen `for/as var
;;; being each' and then either symbol, symbols, present-symbol,
;;; present-symbols, external-symbol, or external-symbols.
(defun parse-for/as-package (body class-name)
  (cond ((null body)
	 (error 'expected-in/of-but-end))
	((not (or (symbol-equal (car body) '#:in)
		  (symbol-equal (car body) '#:of)))
	 (error 'expected-in/of-but-found
		:found (car body)))
	((null (cddr body))
	 (error 'expected-form-but-end))
	(t
	 (let ((form (caddr body)))
	   (values (make-instance class-name
		     :package-form form)
		   (cdddr body))))))

(define-elementary-parser parse-for/as-package-symbols body (#:symbol #:symbols)
  (parse-for/as-package (cdr body) 'for/as-package-symbols-subcause))

(define-elementary-parser parse-for/as-package-present-symbols body (#:present-symbol #:present-symbols)
  (parse-for/as-package (cdr body) 'for/as-package-present-symbols-subcause))

(define-elementary-parser parse-for/as-package-external-symbols body (#:external-symbol #:external-symbols)
  (parse-for/as-package (cdr body) 'for/as-package-external-symbols-subcause))

(define-elementary-parser parse-for/as-hash/package body (#:being)
  (cond ((null (cdr body))
	 (error 'expected-each/the-but-end))
	((not (or (symbol-equal (cadr body) '#:each)
		  (symbol-equal (cadr body) '#:the)))
	 (error 'expected-each/the-but-found
		:found (cadr body)))
	((null (cddr body))
	 (error 'expected-hash-or-package-but-end))
	(t
	 (parse-alternative (cddr body)
			    #'parse-for/as-hash-keys
			    #'parse-for/as-hash-values
			    #'parse-for/as-package-symbols
			    #'parse-for/as-package-present-symbols
			    #'parse-for/as-package-external-symbols
			    (lambda (body)
			      (error 'expected-hash-or-package-but-found
				     :found (car body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse compound forms

(defun parse-compound-forms (body)
  (if (or (null body) (not (consp (car body))))
      (values '() body)
      (multiple-value-bind (compound-forms rest)
	  (parse-compound-forms (cdr body))
	(values (cons (car body) compound-forms) rest))))

(defun parse-nonempty-compound-forms (body)
  (if (null body)
      (error 'expected-compound-form-but-end)
      (multiple-value-bind (compound-forms rest)
	  (parse-compound-forms body)
	(if (null compound-forms)
	    (error 'expected-compound-form-but-found
		   :found (car body))
	    (values compound-forms rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse initially-clause

(defclass initially-clause
    (clause variable-clause-mixin main-clause-mixin compound-forms-mixin)
  ())

;;; An initially clause does not exist as a separate grammar item in
;;; the HyperSpec, but it is here.  The syntax is:
;;;
;;;    initial ::= initially compound-form+

(define-elementary-parser parse-initially-clause body (#:initially)
  (multiple-value-bind (compound-forms rest)
      (parse-nonempty-compound-forms (cdr body))
    (values (make-instance 'initially-clause :forms compound-forms)
	    rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse finally-clause

(defclass finally-clause
    (clause variable-clause-mixin main-clause-mixin compound-forms-mixin)
  ())

;;; A finally clause does not exist as a separate grammar item in the
;;; HyperSpec, but it is here.  The syntax is:
;;;
;;;    final ::= finally compound-form+

(define-elementary-parser parse-finally-clause body (#:finally)
  (multiple-value-bind (compound-forms rest)
      (parse-nonempty-compound-forms (cdr body))
    (values (make-instance 'finally-clause :forms compound-forms)
	    rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse initial or final

;;; There is no particular reason to regroup these two, other than the
;;; fact that the HyperSpec does, and the HyperSpec probably does it
;;; only because they both take 1 or more compound forms after the
;;; loop keyword.  We do not take advantage of this fact for code
;;; factoring, though.
;;;
;;; The syntax is according to the HyperSpec is:
;;;
;;;    initial-final ::= initially compound-form+ | finally compound-form+ 
;;;
;;; but here it would be more like:
;;;
;;;    initial-final ::= initial | final
;;;    initial ::= initially compund-form+
;;;    final ::= finally compound-form+

(defun parse-initial-final (body)
  (parse-alternative body
		     #'parse-initially-clause
		     #'parse-finally-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a do-clause

;;; The HyperSpec does not have an explicit do-clause, but we do here,
;;; and we define the syntax to be:
;;;
;;;    do-clause ::= {do | doing} compund-form+

(defclass do-clause
    (clause main-clause-mixin compound-forms-mixin) ())

(define-elementary-parser parse-do-clause body (#:do #:doing)
  (multiple-value-bind (compound-forms rest)
      (parse-nonempty-compound-forms (cdr body))
    (values (make-instance 'do-clause :forms compound-forms)
	    rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a return-clause

;;; The HyperSpec does not have an explicit return-clause, but we do here,
;;; and we define the syntax to be:
;;;
;;;    return-clause ::= return {form | it}

(defclass return-clause (clause main-clause-mixin)
  ((%form :initarg :form :reader form)))

(define-elementary-parser parse-return-clause body (#:return)
  (multiple-value-bind (form rest)
      (parse-form (cdr body))
    (values (make-instance 'return-clause :form form)
          rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse unconditional

;;; An unconditional is defined in the HyperSpec like this:
;;;
;;;    unconditional::= {do | doing} compound-form+ | return {form | it} 
;;;
;;; but here it would be more like:
;;;
;;;    unconditional::= do-clause | return-clause

(defun parse-unconditional (body)
  (parse-alternative body
		     #'parse-do-clause
		     #'parse-return-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accumulation clauses

(defclass accumulation-clause (clause main-clause-mixin)
  ((%form :initform nil :initarg :form :accessor form)
   (%into-var :initform nil :initarg :into-var :accessor into-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse list accumulation clauses

(defclass list-accumulation-clause (accumulation-clause)
  ((%into-tail-var :initform nil :initarg :into-tail-var :accessor into-tail-var)))

(defclass collect-clause (list-accumulation-clause) ())
(defclass append-clause (list-accumulation-clause) ())
(defclass nconc-clause (list-accumulation-clause) ())

;;; This function is called when we have seen 
;;; one of collect(ing), append(ing), nconc(ing).
(defun parse-list-accumulation-clause (body class-name)
  (if (null body)
      (error 'expected-form-but-end)
      (let ((result (make-instance class-name
		      :form (car body)
		      :into-tail-var (gensym "LIST-TAIL-"))))
        (if (and (not (null (cdr body)))
                 (symbol-equal (cadr body) '#:into))
            (cond ((null (cddr body))
                   (error 'expected-simple-var-but-end))
                  ((not (symbolp (caddr body)))
                   (error 'expected-simple-var-but-found :found (caddr body)))
                  (t
                   (setf (into-var result) (caddr body))
                   (values result (cdddr body))))
            (values result (cdr body))))))

(define-elementary-parser parse-collect body (#:collect #:collecting)
  (parse-list-accumulation-clause (cdr body) 'collect-clause))

(define-elementary-parser parse-append body (#:append #:appending)
  (parse-list-accumulation-clause (cdr body) 'append-clause))

(define-elementary-parser parse-nconc body (#:nconc #:nconcing)
  (parse-list-accumulation-clause (cdr body) 'nconc-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse numeric accumulation

(defclass numeric-accumulation-clause (accumulation-clause)
  ((%type-spec :initform t :initarg :type-spec :accessor type-spec)))

(defclass count-clause (numeric-accumulation-clause) ())
(defclass sum-clause (numeric-accumulation-clause) ())
(defclass maximize-clause (numeric-accumulation-clause) ())
(defclass minimize-clause (numeric-accumulation-clause) ())

;;; This function is called when we have seen 
;;; one of count(ing), sum(ming), maximiz(e)(ing), minimiz(e)(ing)
(defun parse-numeric-accumulation-clause (body class-name)
  (if (null body)
      (error 'expected-form-but-end)
      (let ((result (make-instance class-name
		      :form (car body))))
	(if (and (not (null (cdr body)))
		 (symbol-equal (cadr body) '#:into))
	    (cond ((null (cddr body))
		   (error 'expected-simple-var-but-end))
		  ((not (symbolp (caddr body)))
		   (error 'expected-simple-var-but-found
			  :found (caddr body)))
		  (t
		   (setf (into-var result) (caddr body))
		   (multiple-value-bind (type-spec rest)
		       (parse-type-spec (cdddr body))
		     (setf (type-spec result) type-spec)
		     (values result rest))))
	    (values result (cdr body))))))

(define-elementary-parser parse-count-clause body (#:count #:counting)
  (parse-numeric-accumulation-clause (cdr body) 'count-clause))

(define-elementary-parser parse-sum-clause body (#:sum #:summing)
  (parse-numeric-accumulation-clause (cdr body) 'sum-clause))

(define-elementary-parser parse-maximize-clause body (#:maximize #:maximizing)
  (parse-numeric-accumulation-clause (cdr body) 'maximize-clause))

(define-elementary-parser parse-minimize-clause body (#:minimize #:minimizing)
  (parse-numeric-accumulation-clause (cdr body) 'minimize-clause))

(defun parse-numeric-accumulation (body)
  (parse-alternative body
		     #'parse-count-clause
		     #'parse-sum-clause
		     #'parse-maximize-clause
		     #'parse-minimize-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse accumulation

(defun parse-accumulation (body)
  (parse-alternative body
		     #'parse-collect
                     #'parse-append
                     #'parse-nconc
		     #'parse-numeric-accumulation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse conditional

;;; The HyperSpec defines conditional in a way similar to this:
;;;
;;;    conditional ::= if/when | unless
;;;
;;; except that it does not introduce the names if/when and unless.  We find it
;;; practical to do so so that we can give a name to each individual parser.

(defclass conditional-clause (clause main-clause)
  ((%test-form :initarg :test-form :reader test-form)
   (%then-clauses :initarg :then-clauses :reader then-clauses)
   (%else-clauses :initarg :else-clauses :reader else-clauses)))

(defclass if/when-clause (conditional-clause) ())

(defclass unless-clause (conditional-clause) ())

(define-elementary-parser parse-and-selectable-clause body (#:and)
  (parse-alternative (cdr body)
		     #'parse-unconditional
		     #'parse-accumulation
		     #'parse-conditional))

(defun parse-selectable-clauses (body)
  (parse-sequence (cons 'and body) #'parse-and-selectable-clause))

;;; This function is called when we have 
;;; already seen if, when, or unless
(defun parse-conditional-remaining (body class-name)
  (multiple-value-bind (form rest1)
      (parse-form body)
    (declare (ignore rest1))
    (multiple-value-bind (then-clauses rest2)
	(parse-selectable-clauses body)
      (cond ((or (null rest2)
		 (and (not (symbol-equal (car rest2) '#:else))
		      (not (symbol-equal (car rest2) '#:end))))
	     (values (make-instance class-name
		       :form form
		       :then-clauses then-clauses
		       :else-clauses nil)
		     rest2))
	    ((symbol-equal (car rest2) '#:end)
	     (values (make-instance class-name
		       :form form
		       :then-clauses then-clauses
		       :else-clauses nil)
		     (cdr rest2)))
	    (t
	     ;; we have an else
	     (multiple-value-bind (else-clauses rest3)
		 (parse-selectable-clauses (cdr rest2))
	       (values (make-instance class-name
			 :form form
			 :then-clauses then-clauses
			 :else-clauses else-clauses)
		       (if (symbol-equal (car rest3) ':#end)
			   (cdr rest3)
			   rest3))))))))

(define-elementary-parser parse-if/when body (#:if #:when)
  (parse-conditional-remaining body 'if/when-clause))

(define-elementary-parser parse-unless body (#:if)
  (parse-conditional-remaining body 'unless-clause))

(defun parse-conditional (body)
  (parse-alternative body
		     #'parse-if/when
		     #'parse-unless))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse termination test

;;; The HyperSpec defines termination-test in a way similar to this:
;;;
;;;    termination-test ::= while-clause | until-clause | repeat-clause
;;;                         always-clause | never-clause | thereis-clause
;;; 
;;; except that it does not introduce the names while-clause,
;;; until-clause, repeat-clause, always-clause, never-clause and
;;; thereis-clause .  We find it practical to do so so that we can
;;; give a name to each individual parser.

(defclass termination-test (clause main-clause-mixin)
  ((%form :initarg :form :reader form)))

(defclass while-clause (termination-test) ())
(defclass until-clause (termination-test) ())
(defclass repeat-clause (termination-test var-and-type-spec-mixin) ())
(defclass always-clause (termination-test) ())
(defclass never-clause (termination-test) ())
(defclass thereis-clause (termination-test) ())

(defun parse-termination-test-remaining (body class-name)
  (multiple-value-bind (form rest)
      (parse-form body)
    (values (make-instance class-name :form form)
	    rest)))

(define-elementary-parser parse-while-clause body (#:while)
  (parse-termination-test-remaining (cdr body) 'while-clause))

(define-elementary-parser parse-until-clause body (#:until)
  (parse-termination-test-remaining (cdr body) 'until-clause))

(define-elementary-parser parse-repeat-clause body (#:repeat)
  (multiple-value-bind (clause rest)                        
      (parse-termination-test-remaining (cdr body) 'repeat-clause)
    (setf (var-spec clause) (gensym "REPEAT-"))
    (values clause rest)))

(define-elementary-parser parse-always-clause body (#:always)
  (parse-termination-test-remaining (cdr body) 'always-clause))

(define-elementary-parser parse-never-clause body (#:never)
  (parse-termination-test-remaining (cdr body) 'never-clause))

(define-elementary-parser parse-thereis-clause body (#:thereis)
  (parse-termination-test-remaining (cdr body) 'thereis-clause))

(defun parse-termination-test (body)
  (parse-alternative body
		     #'parse-while-clause
                     #'parse-until-clause
                     #'parse-repeat-clause
                     #'parse-always-clause
                     #'parse-never-clause
                     #'parse-thereis-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loop parser

;;; At this level, we do not impose any order between the clauses, and
;;; we just store all the cluses as we find them.  Later, we verify
;;; that the order is correct.
;;;
;;; The reason for doing it that way is so that it is common that
;;; programmers get the order between clauses wrong, so in that case
;;; we want to be able to generate an appropriate error message. 

(defclass loop-body ()
  ((%clauses :initform '() :initarg :clauses :accessor clauses)
   (%accumulation-variable :initform nil :accessor accumulation-variable)
   (%accumulation-list-tail :initform nil :accessor accumulation-list-tail)
   (%accumulation-type :initform nil :accessor accumulation-type)))

(defun parse-any-clause (body)
  (parse-alternative body
		     #'parse-name-clause
		     ;; Treat initially and finally separately
		     #'parse-initial-final
		     ;; Variable clauses except initial and final
		     #'parse-with-clause
		     #'parse-for/as-clause
		     ;; Main clauses except initial and final
		     #'parse-unconditional
		     #'parse-conditional
		     #'parse-accumulation
		     #'parse-termination-test))

(defun parse-loop-body (body)
  (multiple-value-bind (clauses rest)
      (parse-sequence body #'parse-any-clause)
    (if (not (null rest))
	(error 'expected-keyword-but-found
	       :found (car rest))
	(make-instance 'loop-body :clauses clauses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions for syntactic and semantic analysis

;;; The root of all syntax errors.
(define-condition loop-syntax-error (program-error) ())

;;; This condition is signaled when a name-clause is found
;;; and the first clause is not a name-clause. 
(define-condition name-clause-not-first (loop-syntax-error) ())

;;; This condition is signaled when the first clause is a name-clause
;;; but there are other name-clauses. 
(define-condition multiple-name-clauses (loop-syntax-error) ())

;;; This condition is signaled when a variable-clause (other than an
;;; initially-clause or a finally-clause) appears after a main-clause
;;; (other than an initially-clause or a finally-clause).  
(define-condition invalid-clause-order (loop-syntax-error) ())

;;; The root of all semantic errors.
(define-condition loop-semantic-error (program-error) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntactic and semantic analysis

(defun verify-clause-order (loop-body)
  (let ((clauses (clauses loop-body)))
    ;; Check that if there is a name-clause, the first one is in
    ;; position 0.  For now, we do not care if there is more than one
    ;; name-clause.
    (let ((name-clause-position
	   (position-if (lambda (clause) (typep clause 'name-clause))
			clauses)))
      (when (and (not (null name-clause-position)) (plusp name-clause-position))
	(error 'name-clause-not-first)))
    ;; Check that, if the first clause is a name-clause, there is not
    ;; a second name-clause.  We have already tested the case when
    ;; there is a name-clause other than in the first position.
    (when (typep (car clauses) 'name-clause)
      (let ((second-name-clause
	     (find-if (lambda (clause) (typep clause 'name-clause))
		      (cdr clauses))))
	(when (not (null second-name-clause))
	  (error 'multiple-name-clauses))))
    ;; Check that there is not a variable-clause (other than an
    ;; initially-clause or a finally-clause) following a name clause
    ;; (other than an initially-clause or a finally-clause). 
    (let ((last-variable-clause-position
	   (position-if (lambda (clause)
			  (and (typep clause 'variable-clause-mixin)
			       (not (typep clause 'initially-clause))
			       (not (typep clause 'finally-clause))))
			clauses
			:from-end t))
	  (first-main-clause-position
	   (position-if (lambda (clause)
			  (and (typep clause 'main-clause-mixin)
			       (not (typep clause 'initially-clause))
			       (not (typep clause 'finally-clause))))
			clauses)))
      (when (and (not (null last-variable-clause-position))
		 (not (null first-main-clause-position))
		 (> last-variable-clause-position first-main-clause-position))
	(error 'invalid-clause-order)))))

(defun destructure-variables (d-var-spec root)
  (let ((bindings '())
	(ignorables '()))
    (labels ((destructure-aux (d-var-spec root)
	       (cond ((null d-var-spec)
		      nil)
		     ((symbolp d-var-spec)
		      (push `(,d-var-spec ,root) bindings))
		     ((not (consp d-var-spec))
		      (error 'expected-var-spec-but-found
			     :found d-var-spec))
		     (t
		      (let ((head (gensym))
			    (tail (gensym)))
			(push head ignorables)
			(push tail ignorables)
			(push `(,head (car ,root)) bindings)
			(push `(,tail (cdr ,root)) bindings)
			(destructure-aux (car d-var-spec) head)
			(destructure-aux (cdr d-var-spec) tail))))))
      (destructure-aux d-var-spec root)
      (values (nreverse bindings) (nreverse ignorables)))))

;;; Extract variables
(defun extract-variables (d-var-spec d-type-spec)
  (let ((result '()))
    (labels ((extract-aux (d-var-spec d-type-spec)
	       (cond ((null d-var-spec)
		      nil)
		     ((symbolp d-var-spec)
		      (push (list d-var-spec (or d-type-spec t)) result))
		     ((symbolp d-type-spec)
		      (if (not (consp d-var-spec))
			  (error 'expected-var-spec-but-found
				 :found d-var-spec)
			  (progn (extract-aux (car d-var-spec) d-type-spec)
				 (extract-aux (cdr d-var-spec) d-type-spec))))
		     ((not (consp d-var-spec))
		      (error 'expected-var-spec-but-found
			     :found d-var-spec))
		     ((not (consp d-type-spec))
		      (error 'expected-type-spec-but-found
			     :found d-type-spec))
		     (t
		      (extract-aux (car d-var-spec) (car d-type-spec))
		      (extract-aux (cdr d-var-spec) (cdr d-type-spec))))))
      (extract-aux d-var-spec d-type-spec)
      result)))

;;; The HyperSpec defines main-clause like this:
;;;
;;;    main-clause::= unconditional | accumulation | conditional |
;;;                   termination-test | initial-final              
;;; 
;;; The reason the HyperSpec defines main-clause is because the loop
;;; body is a name-clause followed by zero or more variable-clauses
;;; followed by zero or more main-clauses, and initia-final is one
;;; possibility both for variable-clause and main-clause.  This means
;;; that an initially-clause or a finally-clause can appear anywhere
;;; after a name-clause.

;;; The HyperSpec defines varible-clause like this:
;;;
;;;    variable-clause::= with-clause | initial-final | for-as-clause 
;;; 
;;; and we follow this example.  The reason the HyperSpec defines
;;; variable-clause is because the loop body is a name-clause
;;; followed by zero or more variable-clauses followed by zero or more
;;; main-clauses, and initia-final is one possibility both for
;;; variable-clause and main-clause.  This means that an
;;; initially-clause or a finally-clause can appear anywhere after a
;;; name-clause.


;;; The HyperSpec defines the syntax of the loop macro like this:
;;;
;;;    loop [name-clause] {variable-clause}* {main-clause}*
;;;
;;; meaning that there is an optional name-clause followed by zero or
;;; more variable-clauses, followed by zero or more main clauses.  One
;;; might think from this that a main-clause cannot precede a
;;; variable-clause, but this is not true, because initially-clauses
;;; and finally-clauses are possibilites for both variable-clause and
;;; main-clause, and it is indeed possible for an initally-clause or a
;;; finally-clause to precede another variable-clause or to follow
;;; another main-clause.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code generation

;;;;;
(defun progn-or-single-form (forms)
  (if (> (length forms) 1)
      `(progn ,@forms)
      (first forms)))

(defgeneric generate-bindings (clause))

(defmethod generate-bindings (clause)
  nil)

(defmethod generate-bindings ((clause repeat-clause))
  `((,(var-spec clause) ,(form clause))))

(defmethod generate-bindings ((clause with-clause))
  (mapcar (lambda (subclause)
            `(,(var-spec subclause) ,(if (form-present-p subclause)
                                         (form subclause)
                                         nil)))
          (subclauses clause)))

(defmethod generate-bindings ((clause list-accumulation-clause))
  (if (null (into-var clause))
      nil
      `((,(into-var clause) nil)
        (,(into-tail-var clause) nil))))

(defmethod generate-bindings ((clause numeric-accumulation-clause))
  (if (null (into-var clause))
      nil
      `((,(into-var clause) 0))))

(defgeneric generate-prologue (clause))

(defmethod generate-prologue (clause)
  nil)

(defmethod generate-prologue ((clause initially-clause))
  (progn-or-single-form (forms clause)))

(defgeneric generate-epilogue (clause))

(defmethod generate-epilogue (clause)
  nil)

(defmethod generate-epilogue ((clause finally-clause))
  (progn-or-single-form (forms clause)))

(defgeneric generate-termination-check (clause))

(defmethod generate-termination-check (clause)
  nil)

(defmethod generate-termination-check ((clause while-clause))
  `(unless ,(form clause)
     (go end)))

(defmethod generate-termination-check ((clause until-clause))
  `(when ,(form clause)
     (go end)))

(defmethod generate-termination-check ((clause repeat-clause))
  (let ((repeat-counter (var-spec clause)))
    `(if (plusp ,repeat-counter)
         (decf ,repeat-counter)
         (go end))))

(defvar *body*)

(defgeneric generate-main-code (clause))

(defmethod generate-main-code (clause)
  nil)

(defmethod generate-main-code ((clause do-clause))
  `(progn ,@(forms clause)))

(defmethod generate-main-code ((clause collect-clause))
  (let ((var (or (into-var clause)
                 (accumulation-variable *body*)))
        (tail (if (into-var clause)
                  (into-tail-var clause)
                  (accumulation-list-tail *body*)))
        (cons (gensym)))
    `(let ((,cons (cons ,(form clause) nil)))
       (if (null ,var)
           (setf ,var ,cons
                 ,tail ,cons)
           (setf (cdr ,tail) ,cons
                 ,tail ,cons)))))

(defmethod generate-main-code ((clause sum-clause))
  (let ((var (or (into-var clause)
                 (accumulation-variable *body*))))
    `(incf ,var ,(form clause))))

(defun generate-body (body)
  (let ((clauses (clauses body))
        (*body* body))
    `(tagbody
        ,@(remove nil (mapcar #'generate-prologue clauses))
      again
        ,@(remove nil (mapcar #'generate-termination-check clauses))
        ,@(remove nil (mapcar #'generate-main-code clauses))
        (go again)
      end
        ,@(remove nil (mapcar #'generate-epilogue clauses)))))

(defun generate-accumulation-bindings-and-body (body)
  (let ((body-form (generate-body body)))
    (case (accumulation-type body)
      (:list
       `(let ((,(accumulation-variable body) nil)
              (,(accumulation-list-tail body) nil))
              ,body-form))
      (:numeric
       `(let ((,(accumulation-variable body) 0))
          ,body-form))
      ((nil)
       body-form))))

(defun generate-bindings-and-body (binding-clauses body)
  (if (endp binding-clauses)
      (generate-accumulation-bindings-and-body body)
      (let ((bindings-first (generate-bindings (first binding-clauses)))
            (bindings-rest-and-body (generate-bindings-and-body
                                     (rest binding-clauses)
                                     body)))
        (if (null bindings-first)
            bindings-rest-and-body
            `(let (,@bindings-first)
               ,bindings-rest-and-body)))))

(defun initialize-accumulation (clauses body)
  (unless (endp clauses)
    (let ((clause (first clauses)))
      (when (and (typep clause 'accumulation-clause)
                 (null (into-var clause)))
        (let ((clause-accumulation-type
               (cond ((typep clause 'list-accumulation-clause)
                      :list)
                     ((typep clause 'numeric-accumulation-clause)
                      :numeric))))
          (cond ((null (accumulation-type body))
                 (setf (accumulation-variable body) (gensym)
                       (accumulation-list-tail body) (gensym)
                       (accumulation-type body) clause-accumulation-type))
                ((not (eq (accumulation-type body) clause-accumulation-type))
                 (error "Conflicting accumulation types")))))
      (initialize-accumulation (rest clauses) body))))
;;;;;
