;;; This code is in the public domain.
;;;
;;; The preliminary name for this project is SICL, which doesn't stand
;;; for anything in particular.  Pronounce it like "sickle".
;;;
;;; The purpose of this code is to provide a totally portable
;;; implementation of some high-level functionality of the Common Lisp
;;; language, so that implementors of Common Lisp systems can
;;; integrate it as it is into their systems, without having to
;;; implement and maintain a specific version of it. 
;;;
;;; Author: Robert Strandh (strandh@labri.fr)
;;; Date: 2008
;;;
;;; A portable implementation of the LOOP macro.
;;; This implementation does not use any iteration construct. 

;;; Ultimately, this form should be moved to a central place, such as
;;; packages.lisp.
(defpackage #:sicl-loop
    (:use #:common-lisp)
  (:shadow #:loop))

(in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions for parsing

(define-condition loop-parse-error (parse-error) ())

(define-condition loop-parse-error-found (parse-error)
  ((%found :initarg :found :reader found)))

(define-condition expected-var-spec-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected a variable specification but reached ~
              the end of the loop body"))))

(define-condition expected-var-spec-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected a variable specification but found: ~s"
	     (found condition)))))

(define-condition expected-simple-var-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected a simple variable but reached ~
              the end of the loop body"))))

(define-condition expected-simple-var-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected a simple variable but found: ~s"
	     (found condition)))))

(define-condition expected-type-spec-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected a variable specification but reached ~
              the end of the loop body"))))

(define-condition expected-type-spec-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected a type specification but found: ~s"
	     (found condition)))))

(define-condition expected-compound-form-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected a compound form but reached ~
              the end of the loop body"))))

(define-condition expected-compound-form-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected a compound form but found: ~s"
	     (found condition)))))

(define-condition expected-form-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected a form but reached ~
              the end of the loop body"))))

(define-condition expected-symbol-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected a symbol but reached ~
              the end of the loop body"))))

(define-condition expected-symbol-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected a symbol but found: ~s"
	     (found condition)))))

(define-condition expected-keyword-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected a loop keyword, but found: ~s"
	     (found condition)))))

(define-condition expected-for/as-subclause-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected a loop keyword indicating a for/as ~
              subclause, but reached the end of the loop body"))))

(define-condition expected-symbol-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected a loop keyword indicating a for/as ~
              subclause, but found: ~s"
	     (found condition)))))

(define-condition expected-each/the-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected the loop keyword each/the, ~
              but reached the end of the loop body"))))

(define-condition expected-each/the-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected the loop keyword each/the, but found: ~s"
	     (found condition)))))

(define-condition expected-hash-or-package-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected a loop keyword indicating a for/as-hash, ~
              but reached the end of the loop body"))))

(define-condition expected-hash-or-package-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected a loop keyword indicating a for/as-hash ~
              or a for/as-package subclause, but found: ~s"
	     (found condition)))))

(define-condition expected-in/of-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected the loop keyword in/or, ~
              but reached the end of the loop body"))))

(define-condition expected-in/of-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected the loop keyword in/or, but found: ~s"
	     (found condition)))))

(define-condition expected-hash-key-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected (hash-key other-var), ~
              but reached the end of the loop body"))))

(define-condition expected-hash-value-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected (hash-value other-var), ~
              but reached the end of the loop body"))))

(define-condition expected-hash-key-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected (hash-key other-var), but found: ~s"
	     (found condition)))))

(define-condition expected-hash-value-but-found (loop-parse-error-found)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected (hash-value other-var), but found: ~s"
	     (found condition)))))

(define-condition expected-preposition-but-end (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
	     "Expected a for/as preposition, ~
              but reached the end of the loop body"))))

(define-condition too-many-prepositions-from-one-group (loop-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Expected (hash-value other-var), but found: ~s"
	     (found condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities

(defun symbol-equal (symbol1 symbol2)
  (and (symbolp symbol1)
       (string= symbol1 symbol2)))

;;; Returns a list of clauses parsed by repeated 
;;; invocations of the parser.
(defun parse-sequence (body parser)
  (multiple-value-bind (clause rest1)
      (funcall parser body)
    (if (eq rest1 body)
	(values '() body)
	(multiple-value-bind (clauses rest2)
	    (parse-sequence rest1 parser)
	  (values (cons clause clauses) rest2)))))

;;; Try alternative parsers and return the result
;;; of the first one that succeeds.  If none of
;;; the parsers succeeds, then return nil and 
;;; the body unchanged.
(defun parse-alternative (body &rest parsers)
  (if (null parsers)
      (values nil body)
      (multiple-value-bind (clause rest)
	  (funcall (car parsers) body)
	(if (not (eq body rest))
	    (values clause rest)
	    (apply #'parse-alternative body (rest parsers))))))

(defmacro define-elementary-parser (name body-var start-symbols &body body)
  `(defun ,name (,body-var)
     (when (or (null ,body-var)
	       (and ,@(mapcar (lambda (start-symbol)
				`(not (symbol-equal (car ,body-var) ',start-symbol)))
			      start-symbols)))
       (return-from ,name (values nil ,body-var)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a d-var-spec and a type spec

;;; Parse a d-var-spec
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

;;; Return two values, the first clause of the body, and the remaining
;;; body to be parsed.  The body does not include the loop keyword
;;; which must have been removed before this function is called.

(defgeneric parse-body (body keyword))

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
;;; Clause NAMED

(defclass named-clause (clause)
  ((%name :initarg :name :reader name)))

(define-elementary-parser parse-name-clause body (#:named)
  (cond ((null (cdr body))
	 (error 'expected-symbol-but-end))
	((not (symbolp (cadr body)))
	 (error 'expected-symbol-but-found
		:found (cadr body)))
	(t
	 (values (make-instance 'named-clause
		   :name (cadr body))
		 (cddr body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a with-clause

(defclass with-clause (clause subclauses-mixin variable-clause-mixin)
  ((%form :initarg :form :reader form)
   (%form-present :initarg :form-present :reader form-present)))

(defclass with-subclause (var-and-type-spec-mixin) ())

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
	   :form-present nil)
	 rest1)
	;; Else, there is a `='
	(multiple-value-bind (form rest2)
	    (parse-form (cdr rest1))
	  (make-instance 'with-subclause
	    :var-spec var
	    :type-spec type-spec
	    :form form
	    :form-present t)
	  rest2))))

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

(defclass for/as-clause (clause subclause-mixin variable-clause-mixin) ())

(defclass for/as-subclause (var-and-type-spec-mixin) ())

(defclass for/as-arithmetic-subclause (for/as-subclause)
  ((%prepositions :initarg :prepositions :reader prepositions)))

(defclass for/as-arithmetic-up-subclause (for/as-arithmetic-subclause) ())

(defclass for/as-arithmetic-downto-subclause (for/as-arithmetic-subclause) ())

(defclass for/as-arithmetic-downfrom-subclause (for/as-arithmetic-subclause) ())

(defclass preposition ()
  ((%form :initarg :form :reader form)))

(defclass preposition-first-group (preposition) ())
(defclass preposition-second-group (preposition) ())
(defclass preposition-third-group (preposition) ())

(defclass preposition-from (preposition-first-group) ())
(defclass preposition-downfrom (preposition-first-group) ())
(defclass preposition-upfrom (preposition-first-group) ())

(defclass preposition-to (preposition-second-group) ())
(defclass preposition-downto (preposition-second-group) ())
(defclass preposition-upto (preposition-second-group) ())
(defclass preposition-below (preposition-second-group) ())
(defclass preposition-above (preposition-second-group) ())

(defclass preposition-by (preposition-third-group) ())

(defclass for/as-in-on-list-subclause (for/as-subclause)
  ((%list-form :initarg :list-form :reader list-form)
   (%step-fun-form :initarg :step-fun-form :reader step-fun-form)))

(defclass for/as-in-list-subclause (for/as-in-on-list-subclause) ())

(defclass for/as-on-list-subclause (for/as-in-on-list-subclause) ())

(defclass for/as-equals-then-subclause (for/as-subclause)
  ((%form1 :initarg :form1 :reader form1)
   (%form2 :initarg :form2 :reader form2)))

(defclass for/as-across-subclause (for/as-subclause) 
  ((%array-form :initarg :array-form :reader array-form)))

(defclass for/as-hash-subclause (for/as-subclause)
  ((%hash-table-form :initarg :hash-table-form :reader hash-table-form)
   (%other-var :initarg :other-var :reader other-var)))

(defclass for/as-hash-key-subclause (for/as-hash-subclause) ())

(defclass for/as-hash-value-subclause (for/as-hash-subclause) ())

(defclass for/as-package-subclause (for/as-subclause)
  ((%package-form :initarg :package-form :reader package-form)))

(defclass for/as-package-symbols-subcause (for/as-package-subclause) ())

(defclass for/as-package-present-symbols-subcause (for/as-package-subclause) ())

(defclass for/as-package-external-symbols-subcause (for/as-package-subclause) ())

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

(define-elementary-parser parse-for/as-in body (#:in)
  (parse-for/as-in/on (cdr body) 'for/as-in-list-subclause))

(define-elementary-parser parse-for/as-on body (#:n)
  (parse-for/as-in/on (cdr body) 'for/as-on-list-subclause))

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

(define-elementary-parser parse-for/as-across body (#:across)
  (multiple-value-bind (form rest)
      (parse-form body)
    (values (make-instance 'for/as-across-subclause
	      :array-form form)
	    rest)))

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
		  (values (make-instanc 'for/as-hash-key-subclause
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
	  ((not (or (symbol-equal (car body) #:in)
		    (symbol-equal (car body) #:of)))
	   (error 'expected-in/of-but-found
		  :found (car body)))
	  ((null (progn (pop body) body))
	   (error 'expected-form-but-end))
	  (t
	   (setf hash-table-form (car body))
	   (pop body)
	   (cond ((or (null body)
		      (not (symbol-equal (car body) '#:using)))
		  (values (make-instanc 'for-as-hash-value-subclause
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

(defun parse-prepositions (body)
  (parse-sequence body #'parse-preposition))

(defun parse-for/as-arithmetic (body)
  (multiple-value-bind (prepositions rest)
      (parse-preposition body)
    (declare (ignore rest))
    ;; check the prepositions
    (cond ((null prepositions)
	   (error 'expected-preposition-but-end))
	  ((or (plusp (count 'preposition-first-group
			     prepositions
			     :test (lambda (x y) (typep y x))))
	       (plusp (count 'preposition-second-group
			     prepositions
			     :test (lambda (x y) (typep y x))))
	       (plusp (count 'preposition-third-group
			     prepositions
			     :test (lambda (x y) (typep y x)))))
	       
	   (error 'too-many-prepositions-from-one-group))
	  (t (values (make-instance 'for/as-arithmetic-subclause
		       :prepositions prepositions)
		     body)))))

(define-elementary-parser parse-and-for/as-subclause body (#:and)
  (multiple-value-bind (var type-spec rest1)
      ;; All for/as clauses start with a variable and an
      ;; optional type spec.  Start by parsing them.
      (parse-var-and-type-spec (cdr body))
    (if (null rest1)
	(error 'expected-for/as-subclause-but-end)
	(multiple-value-bind (subclause rest2)
	    (parse-alternative rest1
			       #'parse-for/as-in
			       #'parse-for/as-on
			       #'parse-for/as-equals-then
			       #'parse-for/as-across
			       #'parse-for/as-hash/package
			       #'parse-for/as-arithmetic)
	  (if (eq rest2 rest1)
	      (error 'expected-for/as-subclause-but-found
		     :found (car rest1))
	      (setf (var-spec subclause) var
		    (type-spec subclause type-spec)))
	  (values subclause rest2)))))

(define-elementary-parser parse-for/as-clause body (#:for #:as)
  (multiple-value-bind (subclauses rest)
      (parse-sequence (cons 'and (cdr body))
		      #'parse-and-for-/as-subclause)
    (values (make-instance 'for/as-clause
	      :subclauses subclauses)
	    rest)))

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

(define-elementary-parser parse-finally-clause body (#:finally)
  (multiple-value-bind (compound-forms rest)
      (parse-nonempty-compound-forms (cdr body))
    (values (make-instance 'finally-clause :forms compound-forms)
	    rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse initial or final

(defun parse-initial-final (body)
  (parse-alternative body
		     #'parse-initially-clause
		     #'parse-finally-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a variable-clause

(defun parse-variable-clause (body)
  (parse-alternative body
		     #'parse-with-clause
		     #'parse-initial-final
		     #'parse-for/as-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a do-clause

(defclass do-clause
    (clause main-clause-mixin compound-forms-mixin) ())

(define-elementary-parser parse-do body (#:do)
  (multiple-value-bind (compound-forms rest)
      (parse-nonempty-compound-forms (cdr body))
    (values (make-instance 'do-clause :forms compound-forms)
	    rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a return-clause

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

(defclass list-accumulation-clause (accumulation-clause) ())

(defclass collect-clause (list-accumulation-clause) ())
(defclass append-clause (list-accumulation-clause) ())
(defclass nconc-clause (list-accumulation-clause) ())

;;; This function is called when we have seen 
;;; one of collect(ing), append(ing), nconc(ing).
(defun parse-list-accumulation-clause (body class-name)
  (if (null body)
      (error 'expected-form-but-end)
      (let ((result (make-instance 'class-name
		      :form (car body))))
	(if (and (not (null (cdr body)))
		 (symbol-equal (cadr body) '#:into))
	    (cond ((null (cddr body))
		   (error 'expected-simple-var-but-end))
		  ((not (symbolp (caddr body)))
		   (error 'expected-simple-var-but-found
			  :found (caddr body)))
		  (t
		   (setf (into-var result)
			 (caddr body))
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
      (let ((result (make-instance 'class-name
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
		     #'parse-list-accumulation-clause
		     #'parse-numeric-accumulation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse conditional

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
(defun parse-conditional (body class-name)
  (multiple-value-bind (form rest1)
      (parse-form body)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse termination test

(defclass termination-test (clause main-clause-mixin)
  ((%form :initarg :form :reader form)))

(defclass while-clause (termination-test) ())
(defclass until-clause (termination-test) ())
(defclass repeat-clause (termination-test) ())
(defclass always-clause (termination-test) ())
(defclass never-clause (termination-test) ())
(defclass thereis-clause (termination-test) ())

(defun parse-termination-test (body class-name)
  (multiple-value-bind (form rest)
      (parse-form body)
    (values (make-instance class-name :form form)
	    rest)))

(define-elementary-parser parse-while-clause body (#:while)
  (parse-termination-test (cdr body) 'while-clause))

(define-elementary-parser parse-until-clause body (#:until)
  (parse-termination-test (cdr body) 'until-clause))

(define-elementary-parser parse-repeat-clause body (#:repeat)
  (parse-termination-test (cdr body) 'repeat-clause))

(define-elementary-parser parse-always-clause body (#:always)
  (parse-termination-test (cdr body) 'always-clause))

(define-elementary-parser parse-never-clause body (#:never)
  (parse-termination-test (cdr body) 'never-clause))

(define-elementary-parser parse-thereis-clause body (#:thereis)
  (parse-termination-test (cdr body) 'thereis-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a main-clause

(defun parse-main-clause (body)
  (parse-alternative body
		     #'parse-unconditional
		     #'parse-accumulation
		     #'parse-conditional
		     #'parse-termination-test
		     #'parse-initial-final))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loop parser

(defclass loop-body ()
  ((%name-clause :initarg :name-clause :reader name-clause)
   (%variable-clauses :initarg :variable-clauses :reader variable-clauses)
   (%main-clauses :initarg :main-clauses :reader main-clauses)))

(defun parse-loop-body (body)
  (multiple-value-bind (name-clause rest1)
      (parse-name-clause body)
    (multiple-value-bind (variable-clauses rest2)
	(parse-sequence rest1 #'parse-variable-clause)
      (multiple-value-bind (main-clauses rest3)
	  (parse-sequence rest2 #'parse-main-clause)
	(cond ((not (null rest3))
	       (error 'expected-keyword-but-found
		      :found (car rest3)))
	      (t (make-instance 'loop-body
                   :name-clause name-clause
                   :variable-clauses variable-clauses
                   :main-clauses main-clauses)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions for syntactic and semantic analysis

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntactic and semantic analysis

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

