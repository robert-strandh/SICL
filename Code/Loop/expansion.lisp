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

(defgeneric prologue-form (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

(defgeneric termination-form (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

(defgeneric body-form (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

(defgeneric step-form (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric epilogue (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defun expand-clauses (all-clauses)
  (let ((start-tag (gensym))
	(end-tag (gensym)))
    (labels
	((do-bindings (clauses)
	   (if (null clauses)
	       `(tagbody
		   (progn ,@(mapcar (lambda (clause)
				      (prologue-form clause end-tag))
				    all-clauses))
		   ,start-tag
		   (progn ,@(mapcar (lambda (clause)
				      (body-form clause end-tag))
				    all-clauses))
		   (progn ,@(mapcar (lambda (clause)
				      (termination-form clause end-tag))
				    all-clauses))
		   (progn ,@(mapcar #'step-form all-clauses))
		   (go ,start-tag)
		   ,end-tag
		   (progn ,@(mapcar #'epilogue all-clauses)))
	       `(let* ,(bindings (car clauses))
		  (declare ,@(declarations (car clauses)))
		  ,(do-bindings (cdr clauses))))))
      (do-bindings all-clauses))))

(defvar *loop-name*)

(defvar *accumulation-variable*)

(defvar *list-tail-accumulation-variable*)

(defun expand-body (loop-body)
  (if (every #'consp loop-body)
      (let ((tag (gensym)))
	`(block nil
	   (tagbody
	      ,tag
	      ,@loop-body
	      (go ,tag))))
      (let ((clauses (parse-loop-body loop-body)))
	(analyse-clauses clauses)
	(let* ((name (if (typep (car clauses) 'name-clause)
			 (name (car clauses))
			 nil))
	       (*loop-name* name)
	       (*accumulation-variable* (gensym))
	       (*list-tail-accumulation-variable* (gensym)))
	  `(block ,name
	     ,(expand-clauses clauses))))))
