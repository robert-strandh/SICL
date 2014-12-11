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

(in-package #:sicl-loop)

(defgeneric declarations (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric prologue (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric termination (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric body (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric step (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric epilogue (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defun expand-clauses (all-clauses)
  (labels ((do-bindings (clauses)
	     (if (null clauses)
		 `(progn ,@(mapcar #'prologue all-clauses)
			 (tagbody
			  start
			    (progn ,@(mapcar #'termination all-clauses))
			    (progn ,@(mapcar #'body all-clauses))
			    (progn ,@(mapcar #'step all-clauses))
			    (go start)
			    (progn ,@(mapcar #'epilogue all-clauses))
			  end))
		 `(let* ,(bindings (car clauses))
		    (declare ,@(declarations (car clauses)))
		    ,(do-bindings (cdr clauses))))))
    (do-bindings all-clauses)))
