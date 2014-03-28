;;;; Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;; 
;;;; Copyright (c) 2010, 2011
;;;;
;;;;     Matthieu Villeneuve (matthieu.villeneuve@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the LOOP module of the SICL project.
;;;; See the file SICL.text for a description of the project. 

;;;; The LOOP module provides a portable implementation of the LOOP
;;;; macro as described in the Common Lisp HyperSpec.

;;;; This implementation does not use any iteration construct.
;;;; Depending on the use case for this module, using iteration
;;;; constructs to implement LOOP may or may not be a problem.
;;;;
;;;; If this module is used as part of a new implementation (the
;;;; TARGET) to be bootstrapped from an existing conforming Common
;;;; Lisp implementation (the HOST), then any iteration construct
;;;; could be used, including LOOP, simply because the host
;;;; implementation of these constructs would be used to cross-compile
;;;; target files using LOOP.
;;;;
;;;; On the other hand, if this module is to be added to an existing
;;;; Common Lisp implementation, perhaps in order to obtain a better
;;;; implementation of LOOP, or perhaps because it is a partial
;;;; implementation that does not yet have a LOOP macro, then it is
;;;; important that this module not use LOOP itself.  We decide to go
;;;; one step further by also not using any other iteration
;;;; constructs, so that those iteration constructs could be
;;;; implemented by using the definition of LOOP in this module.

(cl:in-package #:sicl-loop)

(declaim (optimize (debug 3)))

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
    ;; initially-clause or a finally-clause) following a name clause
    ;; (other than an initially-clause or a finally-clause). 
    (let ((last-variable-clause-position
	   (position-if (lambda (clause)
			  (and (typep clause 'variable-clause-mixin)
			       (not (typep clause 'initially-clause))
			       (not (typep clause 'finally-clause))))
			clauses
			:from-end t))
	  (first-main-clause-position
	   (position-if (lambda (clause)
			  (and (typep clause 'main-clause-mixin)
			       (not (typep clause 'initially-clause))
			       (not (typep clause 'finally-clause))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code generation

;;;;;
(defun progn-or-single-form (forms)
  (if (> (length forms) 1)
      `(progn ,@forms)
      (first forms)))

(defgeneric generate-bindings (clause))

(defmethod generate-bindings (clause)
  nil)

(defmethod generate-bindings ((clause repeat-clause))
  `((,(var-spec clause) ,(form clause))))

(defmethod generate-bindings ((clause with-clause))
  (mapcar (lambda (subclause)
            `(,(var-spec subclause) ,(if (form-present-p subclause)
                                         (form subclause)
                                         nil)))
          (subclauses clause)))

(defmethod generate-bindings ((clause list-accumulation-clause))
  (if (null (into-var clause))
      nil
      `((,(into-var clause) nil)
        (,(into-tail-var clause) nil))))

(defmethod generate-bindings ((clause numeric-accumulation-clause))
  (if (null (into-var clause))
      nil
      `((,(into-var clause) 0))))

(defgeneric generate-prologue (clause))

(defmethod generate-prologue (clause)
  nil)

(defmethod generate-prologue ((clause initially-clause))
  (progn-or-single-form (forms clause)))

(defgeneric generate-epilogue (clause))

(defmethod generate-epilogue (clause)
  nil)

(defmethod generate-epilogue ((clause finally-clause))
  (progn-or-single-form (forms clause)))

(defgeneric generate-termination-check (clause))

(defmethod generate-termination-check (clause)
  nil)

(defmethod generate-termination-check ((clause while-clause))
  `(unless ,(form clause)
     (go end)))

(defmethod generate-termination-check ((clause until-clause))
  `(when ,(form clause)
     (go end)))

(defmethod generate-termination-check ((clause repeat-clause))
  (let ((repeat-counter (var-spec clause)))
    `(if (plusp ,repeat-counter)
         (decf ,repeat-counter)
         (go end))))

(defvar *body*)

(defgeneric generate-main-code (clause))

(defmethod generate-main-code (clause)
  nil)

(defmethod generate-main-code ((clause do-clause))
  `(progn ,@(forms clause)))

(defmethod generate-main-code ((clause collect-clause))
  (let ((var (or (into-var clause)
                 (accumulation-variable *body*)))
        (tail (if (into-var clause)
                  (into-tail-var clause)
                  (accumulation-list-tail *body*)))
        (cons (gensym)))
    `(let ((,cons (cons ,(form clause) nil)))
       (if (null ,var)
           (setf ,var ,cons
                 ,tail ,cons)
           (setf (cdr ,tail) ,cons
                 ,tail ,cons)))))

(defmethod generate-main-code ((clause sum-clause))
  (let ((var (or (into-var clause)
                 (accumulation-variable *body*))))
    `(incf ,var ,(form clause))))

(defun generate-body (body)
  (let ((clauses (clauses body))
        (*body* body))
    `(tagbody
        ,@(remove nil (mapcar #'generate-prologue clauses))
      again
        ,@(remove nil (mapcar #'generate-termination-check clauses))
        ,@(remove nil (mapcar #'generate-main-code clauses))
        (go again)
      end
        ,@(remove nil (mapcar #'generate-epilogue clauses)))))

(defun generate-accumulation-bindings-and-body (body)
  (let ((body-form (generate-body body)))
    (case (accumulation-type body)
      (:list
       `(let ((,(accumulation-variable body) nil)
              (,(accumulation-list-tail body) nil))
              ,body-form))
      (:numeric
       `(let ((,(accumulation-variable body) 0))
          ,body-form))
      ((nil)
       body-form))))

(defun generate-bindings-and-body (binding-clauses body)
  (if (endp binding-clauses)
      (generate-accumulation-bindings-and-body body)
      (let ((bindings-first (generate-bindings (first binding-clauses)))
            (bindings-rest-and-body (generate-bindings-and-body
                                     (rest binding-clauses)
                                     body)))
        (if (null bindings-first)
            bindings-rest-and-body
            `(let (,@bindings-first)
               ,bindings-rest-and-body)))))

(defun initialize-accumulation (clauses body)
  (unless (endp clauses)
    (let ((clause (first clauses)))
      (when (and (typep clause 'accumulation-clause)
                 (null (into-var clause)))
        (let ((clause-accumulation-type
               (cond ((typep clause 'list-accumulation-clause)
                      :list)
                     ((typep clause 'numeric-accumulation-clause)
                      :numeric))))
          (cond ((null (accumulation-type body))
                 (setf (accumulation-variable body) (gensym)
                       (accumulation-list-tail body) (gensym)
                       (accumulation-type body) clause-accumulation-type))
                ((not (eq (accumulation-type body) clause-accumulation-type))
                 (error "Conflicting accumulation types")))))
      (initialize-accumulation (rest clauses) body))))
;;;;;
