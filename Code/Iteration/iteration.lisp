;;; This code is in the public domain.
;;;
;;; The preliminary name for this project is SICL, which doesn't stand
;;; for anything in particular.  Pronounce it like "sickle".
;;;
;;; The purpose of this code is to provide a totally portable
;;; implementation of some high-level functionality of the Common Lisp
;;; language, so that implementors of Common Lisp systems can
;;; integrate it as it is into their systems, without having to
;;; implement and maintain a specific version of it. 
;;;
;;; Author: Robert Strandh (strandh@labri.fr)
;;; Date: 2008
;;;
;;; A portable implementation of the Common Lisp
;;; iteration macros.  
;;; 
;;; This implementation does not use any iteration construct, nor any
;;; operations on sequences (other than the ones we define ourself
;;; here).  Implementations can therefore load this file very early on
;;; in the bootstrap process.  It allows for operations on sequences
;;; and the loop macro to be defined in terms of the macros defined
;;; here.
;;;
;;; This implementation also does not use the format function, and
;;; instead uses print and princ for error reporting.  This makes it
;;; possible for format to use iteration constructs defined here.

;;; Ultimately, this form should be moved to a central place, such as
;;; packages.lisp.
(defpackage #:sicl-iteration
  (:use #:common-lisp)
  ;; Shadow these for now.  Ultimately, import them with
  ;; the rest of the CL package. 
  (:shadow #:dolist #:dotimes #:do #:do*))

(in-package #:sicl-iteration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions

(define-condition malformed-binding-var (program-error)
  ((%found :initarg :found :reader found))
  (:report
   (lambda (condition stream)
     (princ "Expected a binding var in the form of" stream)
     (terpri stream)
     (princ "a symbol, but found: " stream)
     (print (found condition) stream))))

(define-condition malformed-list-form (program-error)
  ((%found :initarg :found :reader found))
  (:report
   (lambda (condition stream)
     (princ "Expected a list form in the form of" stream)
     (terpri stream)
     (princ "a list, but found: " stream)
     (print (found condition) stream))))


(define-condition malformed-count-form (program-error)
  ((%found :initarg :found :reader found))
  (:report
   (lambda (condition stream)
     (princ "Expected a count form in the form of" stream)
     (terpri stream)
     (princ "a number, but found: " stream)
     (print (found condition) stream))))

(define-condition malformed-body (program-error)
  ((%body :initarg :body :reader body))
  (:report
   (lambda (condition stream)
     (princ "Expected a body in the form of" stream)
     (terpri stream)
     (princ "a proper list, but found: " stream)
     (print (body condition) stream))))

(define-condition malformed-variable-clauses (program-error)
  ((%found :initarg :found :reader found))
  (:report
   (lambda (condition stream)
     (princ "Expected a proper list of variable clauses," stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (found condition) stream))))

(define-condition malformed-variable-clause (program-error)
  ((%found :initarg :found :reader found))
  (:report
   (lambda (condition stream)
     (princ "Expected a variable clause of the form" stream)
     (terpri stream)
     (princ "var, (var), (var init-form), or (var init-form step-form),"
	    stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (found condition) stream))))

(define-condition malformed-end-test (program-error)
  ((%found :initarg :found :reader found))
  (:report
   (lambda (condition stream)
     (princ "Expected an end test clause of the form" stream)
     (terpri stream)
     (princ "(end-test result-form*)," stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (found condition) stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities

;;; Split a body into declarations and forms.
(defun split-body (body &optional declarations)
  (if (or (null body)
	  (not (consp (car body)))
	  (not (eq (caar body) 'declare)))
      (values (nreverse declarations) body)
      (split-body (cdr body) (cons (car body) declarations))))

;;; Check that an object is a proper list
(defun proper-list-p (object)
  (or (null object)
      (and (consp object)
	   (proper-list-p (cdr object)))))

;;; For do and do* we need to map over the variable binding clauses.
;;; We therefore need mapcar or something similar.  But in order to
;;; avoid introducing a dependency on sequence operations, we define
;;; our own mapcar using only recursion.

(defun local-mapcar (function list)
  (if (null list)
      '()
      (cons (funcall function (car list))
	    (local-mapcar function (cdr list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros dolist and dotimes

;;; The spec says that the variable is bound to nil when the
;;; result-form is evaluated.  But we don't want the declarations to
;;; have to include nil as one of the values of var.  For that reason,
;;; there needs to be a different binding of the variable when the
;;; forms of the body are evaluated and when the result-form is
;;; evaluated.

;;; The spec says we have a choice between binding or assigning the
;;; variable in each iteration.  For dolist, choosing assignment gets
;;; complicated in the first iteration though, because we would have
;;; to come up with an initial value of the variable that is
;;; compatible with the declarations.  For that reason, we choose to
;;; bind it.

(defmacro dolist ((var list-form &optional result-form) &body body)
  ;; do some syntax checking
  (unless (symbolp var)
    (error 'malformed-binding-var :found var))
  (unless (or (listp list-form) (symbolp list-form))
    (error 'malformed-list-form :found list-form))
  (unless (proper-list-p body)
    (error 'malformed-body :body body))
  (multiple-value-bind (declarations forms)
      (split-body body)
    (let ((start-tag (gensym))
	  (end-tag (gensym))
	  (list-var (gensym)))
      `(let ((,list-var ,list-form))
	 (block nil
	   (tagbody
	      ,start-tag
	      (when (endp ,list-var)
		(go ,end-tag))
	      (let ((,var (car ,list-var)))
		,@declarations
		(tagbody ,@forms))
	      (pop ,list-var)
	      (go ,start-tag)
	      ,end-tag)
	   (let ((,var nil))
	     (declare (ignorable ,var))
	     ,result-form))))))

;;; For dotimes, we don't have the problem of initial value which is
;;; always 0, so we can bind the variable once for the entire loop
;;; body.

(defmacro dotimes ((var count-form &optional result-form) &body body)
  ;; do some syntax checking
  (unless (symbolp var)
    (error 'malformed-binding-var :found var))
  (unless (or (numberp count-form) (not (constantp count-form)))
    (error 'malformed-count-form :found count-form))
  (unless (proper-list-p body)
    (error 'malformed-body :body body))
  (multiple-value-bind (declarations forms)
      (split-body body)
    (let ((start-tag (gensym))
	  (end-tag (gensym))
	  (count-var (gensym)))
      `(let ((,count-var ,count-form)
	     (,var 0))
	 (declare (type integer ,var))
	 ,@declarations
	 (block nil
	   (tagbody
	      ,start-tag
	      (when (= ,var ,count-var)
		(go ,end-tag))
	      (tagbody ,@forms)
	      (incf ,var)
	      (go ,start-tag)
	      ,end-tag)
	   (let ((,var nil))
	     (declare (ignorable ,var))
	     ,result-form))))))

(defun check-variable-clauses (variable-clauses)
  (unless (proper-list-p variable-clauses)
    (error 'malformed-variable-clauses :found variable-clauses))
  (local-mapcar
   (lambda (clause)
     (unless (or (symbolp clause)
		 (and (consp clause)
		      (symbolp (car clause))
		      (or (null (cdr clause))
			  (null (cddr clause))
			  (null (cdddr clause)))))
       (error 'malformed-variable-clause
	      :found clause)))
   variable-clauses))

(defun extract-bindings (variable-clauses)
  (local-mapcar
   (lambda (clause)
     (cond ((symbolp clause) clause)
	   ((null (cdr clause)) (car clause))
	   (t (list (car clause) (cadr clause)))))
   variable-clauses))

(defun extract-updates (variable-clauses)
  (if (null variable-clauses) '()
      (let ((clause (car variable-clauses)))
	(if (and (consp clause)
		 (not (null (cddr clause))))
	    (list* (car clause)
		   (caddr clause)
		   (extract-updates (cdr variable-clauses)))
	    (extract-updates (cdr variable-clauses))))))

(macrolet ((define-do (name assignment-type)
             (multiple-value-bind (let-type setq-type)
                 (ecase assignment-type
                   (:sequential (values 'let* 'setq))
                   (:parallel (values 'let 'psetq)))
               `(defmacro ,name (variable-clauses end-test
                                 &body body)
                  ;; do some syntax checking
                  (check-variable-clauses variable-clauses)
                  (unless (proper-list-p body)
                    (error 'malformed-body :body body))
                  (unless (and (proper-list-p end-test)
                               (not (null end-test)))
                    (error 'malformed-end-test :found end-test))
                  (multiple-value-bind (declarations forms)
                      (split-body body)
                    (let ((start-tag (gensym)))
                      `(block nil
                         (,',let-type ,(extract-bindings
                                        variable-clauses)
                           ,@declarations
                           (tagbody
                              ,start-tag
                              (when ,(car end-test)
                                (return
                                  (progn ,@(cdr end-test))))
                              ,@forms
                              (,',setq-type ,@(extract-updates
                                               variable-clauses))
                              (go ,start-tag))))))))))
  (define-do do :parallel)
  (define-do do* :sequential))
