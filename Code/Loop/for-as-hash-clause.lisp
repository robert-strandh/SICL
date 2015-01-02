;;;; Copyright (c) 2014
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clauses FOR-AS-HASH-CLAUSE and FOR-AS-HASH

(defclass for-as-hash-clause (for-as-clause) ())

(defclass for-as-hash (for-as-subclause)
  ((%hash-table-form :initarg :hash-table-form :reader hash-table-form)
   (%hash-table-var :initform (gensym) :reader hash-table-var)
   (%temp-entry-p-var :initform (gensym) :reader temp-entry-p-var)
   (%temp-key-var :initform (gensym) :reader temp-key-var)
   (%temp-value-var :initform (gensym) :reader temp-value-var)
   (%other-var-spec :initarg :other-var-spec :reader other-var-spec)))

(defclass for-as-hash-key (for-as-hash) ())

(defclass for-as-hash-value (for-as-hash) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers

(define-parser each-the-parser
  (alternative (keyword-parser 'each)
	       (keyword-parser 'the)))

(define-parser hash-key-parser
  (alternative (keyword-parser 'hash-key)
	       (keyword-parser 'hash-keys)))

(define-parser hash-value-parser
  (alternative (keyword-parser 'hash-value)
	       (keyword-parser 'hash-values)))

(define-parser in-of-parser
  (alternative (keyword-parser 'in)
	       (keyword-parser 'of)))

(define-parser hash-value-other-parser
  (singleton #'second
	     (lambda (token)
	       (and (consp token)
		    (consp (cdr token))
		    (null (cddr token))
		    (symbolp (car token))
		    (equal (symbol-name (car token)) (string '#:hash-value))))))

(define-parser hash-key-other-parser
  (singleton #'second
	     (lambda (token)
	       (and (consp token)
		    (consp (cdr token))
		    (null (cddr token))
		    (symbolp (car token))
		    (equal (symbol-name (car token)) (string '#:hash-key))))))

(define-parser hash-key-using-parser
  (consecutive (lambda (var-spec
			type-spec
			being
			each
			hash-key
			of
			hash-table-form
			using)
		 (declare (ignore being each hash-key of))
		 (make-instance 'for-as-hash-key
		   :var-spec var-spec
		   :type-spec type-spec
		   :hash-table-form hash-table-form
		   :other-var-spec using))
	       (singleton #'identity (constantly t))
	       'optional-type-spec-parser
	       (keyword-parser 'being)
	       'each-the-parser
	       'hash-key-parser
	       'in-of-parser
	       (singleton #'identity (constantly t))
	       'hash-value-other-parser))

(define-parser hash-key-parser
  (consecutive (lambda (var-spec
			type-spec
			being
			each
			hash-key
			of
			hash-table-form)
		 (declare (ignore being each hash-key of))
		 (make-instance 'for-as-hash-key
		   :var-spec var-spec
		   :type-spec type-spec
		   :hash-table-form hash-table-form
		   :other-var-spec nil))
	       (singleton #'identity (constantly t))
	       'optional-type-spec-parser
	       (keyword-parser 'being)
	       'each-the-parser
	       'hash-key-parser
	       'in-of-parser
	       (singleton #'identity (constantly t))))

(define-parser hash-value-using-parser
  (consecutive (lambda (var-spec
			type-spec
			being
			each
			hash-key
			of
			hash-table-form
			using)
		 (declare (ignore being each hash-key of))
		 (make-instance 'for-as-hash-key
		   :var-spec var-spec
		   :type-spec type-spec
		   :hash-table-form hash-table-form
		   :other-var-spec using))
	       (singleton #'identity (constantly t))
	       'optional-type-spec-parser
	       (keyword-parser 'being)
	       'each-the-parser
	       'hash-value-parser
	       'in-of-parser
	       (singleton #'identity (constantly t))
	       'hash-key-other-parser))

(define-parser hash-value-parser
  (consecutive (lambda (var-spec
			type-spec
			being
			each
			hash-key
			of
			hash-table-form)
		 (declare (ignore being each hash-key of))
		 (make-instance 'for-as-hash-key
		   :var-spec var-spec
		   :type-spec type-spec
		   :hash-table-form hash-table-form
		   :other-var-spec nil))
	       (singleton #'identity (constantly t))
	       'optional-type-spec-parser
	       (keyword-parser 'being)
	       'each-the-parser
	       'hash-value-parser
	       'in-of-parser
	       (singleton #'identity (constantly t))))

(define-parser for-as-hash-parser
  (alternative 'hash-key-using-parser
	       'hash-key-parser
	       'hash-value-using-parser
	       'hash-value-parser))

(add-for-as-subclause-parser 'for-as-hash-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-hash))
  `((,(hash-table-var clause) ,(hash-table-form clause))
