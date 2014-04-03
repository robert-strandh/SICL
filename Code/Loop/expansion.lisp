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

(defgeneric bindings (clause))

(defmethod bindings (clause)
  (declare (ignore clause))
  '())

(defgeneric prologue (clause))

(defmethod prologue (clause)
  (declare (ignore clause))
  '())

(defgeneric prologue (clause))

(defmethod prologue (clause)
  (declare (ignore clause))
  '())

(defgeneric termination (clause))

(defmethod termination (clause)
  (declare (ignore clause))
  '())

(defgeneric body (clause))

(defmethod body (clause)
  (declare (ignore clause))
  '())

(defgeneric step (clause))

(defmethod step (clause)
  (declare (ignore clause))
  '())

(defgeneric epilogue (clause))

(defmethod epilogue (clause)
  (declare (ignore clause))
  '())

(defun expand-clauses (all-clauses)
  (labels ((do-bindings (clauses)
	     (if (null clauses)
		 `(progn ,@(mapcar #'prologue all-clauses)
			 (tagbody
			  start
			    ,@(mapcar #'termination all-clauses)
			    ,@(mapcar #'body all-clauses)
			    ,@(reduce #'append
				      (mapcar #'step all-clauses)
				      :from-end t)
			    ,@(mapcar #'epilogue all-clauses)
			    (go start)
			  end))
		 `(let ,(bindings (car clauses))
		    (declare ,@(declarations (car clauses)))
		    ,(do-bindings (cdr clauses))))))
    (do-bindings all-clauses)))
