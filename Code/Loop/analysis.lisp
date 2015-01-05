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

;;; Check that if there is a name-clause, the last one is in position
;;; zero.
(defun check-name-clause-position (clauses)
  (let ((name-clause-position
	  (position-if (lambda (clause) (typep clause 'name-clause)) clauses
		       :from-end t)))
    (when (and (not (null name-clause-position)) (plusp name-clause-position))
      (error 'name-clause-not-first))))

;;; Check that there is not a variable-clause following a main clause.
;;; Recall that we diverge from the BNF grammar in the HyperSpec so
;;; that INITIALLY and FINALLY are neither main clauses nor variable
;;; clauses.
(defun check-order-variable-clause-main-clause (clauses)
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

(defun verify-clause-order (clauses)
  (check-name-clause-position clauses)
  (check-order-variable-clause-main-clause clauses))

(defun check-variable-uniqueness (clauses)
  (let* ((variables (reduce #'append (mapcar #'bound-variables clauses)
			    :from-end t))
	 (unique-variables (remove-duplicates variables :test #'eq)))
    (unless (= (length variables)
	       (length unique-variables))
      (loop for var in unique-variables
	    do (when (> (count var variables :test #'eq) 1)
		 (error 'multiple-variable-occurrences
			:bound-variable var))))))

;;; FIXME: Add more analyses.
(defun analyze-clauses (clauses)
  (verify-clause-order clauses)
  (check-variable-uniqueness clauses))
