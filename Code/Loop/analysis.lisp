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
    ;; initial-clause or a final-clause) following a name clause
    ;; (other than an initial-clause or a final-clause).
    (let ((last-variable-clause-position
	   (position-if (lambda (clause)
			  (and (typep clause 'variable-clause-mixin)
			       (not (typep clause 'initial-clause))
			       (not (typep clause 'final-clause))))
			clauses
			:from-end t))
	  (first-main-clause-position
	   (position-if (lambda (clause)
			  (and (typep clause 'main-clause-mixin)
			       (not (typep clause 'initial-clause))
			       (not (typep clause 'final-clause))))
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

