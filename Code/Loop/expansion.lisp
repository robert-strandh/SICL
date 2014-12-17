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

(defgeneric initial-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric final-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric bindings (clause)
  (:method (clause)
    (append (initial-bindings clause) (final-bindings clause))))

(defgeneric prologue (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
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
  (let ((end-tag (gensym)))
    (labels ((do-bindings (clauses)
	       (if (null clauses)
		   `(progn ,@(mapcar (lambda (clause) (prologue clause end-tag))
				     all-clauses)
			   (tagbody
			    start
			      (progn ,@(mapcar #'termination all-clauses))
			      (progn ,@(mapcar #'body all-clauses))
			      (progn ,@(mapcar #'step all-clauses))
			      (go start)
			    ,end-tag
			      (progn ,@(mapcar #'epilogue all-clauses))))
		   `(let* ,(bindings (car clauses))
		      (declare ,@(declarations (car clauses)))
		      ,(do-bindings (cdr clauses))))))
      (do-bindings all-clauses))))

(defun expand-body (loop-body)
  (let ((clauses (parse-loop-body loop-body)))
    (analyse-clauses clauses)
    `(block ,(if (typep (car clauses) 'name-clause)
		 (name (car clauses))
		 nil)
       ,(expand-clauses clauses))))
