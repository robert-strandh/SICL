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

;;; Determine the initial value for the main loop accumulation
;;; variable.  If there is a default SUM or a default COUNT clause,
;;; then the initial value is 0.  Otherwise it is NIL.
(defun accumulation-initial-value (clauses)
  (if (find-if (lambda (clause)
		 (or (typep clause 'count-form-clause)
		     (typep clause 'sum-form-clause)))
	       clauses)
      0
      nil))

(defun prologue-body-epilogue (clauses)
  (let ((start-tag (gensym))
	(end-tag (gensym)))
    `(tagbody
	(progn ,@(mapcar (lambda (clause)
			   (prologue-form clause end-tag))
			 clauses))
	,start-tag
	(progn ,@(mapcar (lambda (clause)
			   (body-form clause end-tag))
			 clauses))
	(progn ,@(mapcar (lambda (clause)
			   (termination-form clause end-tag))
			 clauses))
	(progn ,@(mapcar #'step-form clauses))
	(go ,start-tag)
	,end-tag
	(progn ,@(mapcar #'epilogue clauses)
	       (return-from ,*loop-name*
		 ,*accumulation-variable*)))))

(defun do-clauses (all-clauses remaining-clauses)
  (if (null remaining-clauses)
      (prologue-body-epilogue all-clauses)
      `(let* ,(bindings (car remaining-clauses))
	 (declare ,@(declarations (car remaining-clauses)))
	 ,(do-clauses all-clauses (cdr remaining-clauses)))))

(defun expand-clauses (all-clauses)
  `(let ((,*accumulation-variable*
	   ,(accumulation-initial-value all-clauses))
	 (,*list-tail-accumulation-variable* nil))
     (declare (ignorable ,*list-tail-accumulation-variable*))
     ,(do-clauses all-clauses all-clauses)))

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
