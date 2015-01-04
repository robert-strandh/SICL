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

(defun verify-clause-order (clauses)
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
  ;; Check that there is not a variable-clausefollowing a main clause.
  ;; Recall that we diverge from the BNF grammar in the HyperSpec so
  ;; that INITIALLY and FINALLY are neither main clauses nor variable
  ;; clauses.
  (let ((last-variable-clause-position
	  (position-if (lambda (clause)
			 (typep clause 'variable-clause))
		       clauses
		       :from-end t))
	(first-main-clause-position
	  (position-if (lambda (clause)
			 (typep clause 'main-clause))
		       clauses)))
    (when (and (not (null last-variable-clause-position))
	       (not (null first-main-clause-position))
	       (> last-variable-clause-position first-main-clause-position))
      (error 'invalid-clause-order))))

;;; FIXME: Add more analyses.
(defun analyze-clauses (clauses)
  (verify-clause-order clauses))
