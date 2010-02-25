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
;;; conditional macros
;;; 
;;; This implementation does not use any iteration construct, nor any
;;; operations on sequences (other than the ones we define ourself
;;; here).  Implementations can therefore load this file very early on
;;; in the bootstrap process.
;;;
;;; This implementation also does not use the format function, and
;;; instead uses print and princ for error reporting.  This makes it
;;; possible for format to use the conditional constructs define here.

;;; Ultimately, this form should be moved to a central place, such as
;;; packages.lisp.
(defpackage #:sicl-conditionals
    (:use #:common-lisp)
  ;; Shadow these for now.  Ultimately, import them with
  ;; the rest of the CL package. 
  (:shadow #:or #:and #:when #:unless #:cond)
  (:shadow #:case #:ccase #:ecase)
  (:shadow #:typecase #:ctypecase #:etypecase ))

(in-package #:sicl-conditionals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at macro-expansion time

(define-condition malformed-body (program-error)
  ((%body :initarg :body :reader body))
  (:report
   (lambda (condition stream)
     (princ "Expected a proper list of forms," stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (body condition) stream))))
     
(define-condition malformed-cond-clauses (program-error)
  ((%clauses :initarg :clauses :reader clauses))
  (:report
   (lambda (condition stream)
     (princ "Expected a proper list of cond clauses," stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (clauses condition) stream))))
     
(define-condition malformed-cond-clause (program-error)
  ((%clause :initarg :clause :reader clause))
  (:report
   (lambda (condition stream)
     (princ "Expected a cond clause of the form," stream)
     (terpri stream)
     (princ "(test-form form*)," stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (clause condition) stream))))
     
(define-condition malformed-case-clauses (program-error)
  ((%clauses :initarg :clauses :reader clauses))
  (:report
   (lambda (condition stream)
     (princ "Expected a proper list of case clauses," stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (clauses condition) stream))))
     
(define-condition malformed-case-clause (program-error)
  ((%clause :initarg :clause :reader clause))
  (:report
   (lambda (condition stream)
     (princ "Expected a case clause of the form," stream)
     (terpri stream)
     (princ "(keys form*)," stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (clause condition) stream))))
     
(define-condition otherwise-clause-not-last (program-error)
  ((%clauses :initarg :clauses :reader clauses))
  (:report
   (lambda (condition stream)
     (princ "The `otherwise' or `t' clause must be last in a case form," stream)
     (terpri stream)
     (princ "but but it was followed by: " stream)
     (print (clauses condition) stream))))

(define-condition malformed-keys (program-error)
  ((%keys :initarg :keys :reader keys))
  (:report
   (lambda (condition stream)
     (princ "Expected a designator for a list of keys," stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (keys condition) stream))))
     
(define-condition malformed-typecase-clauses (program-error)
  ((%clauses :initarg :clauses :reader clauses))
  (:report
   (lambda (condition stream)
     (princ "Expected a proper list of typecase clauses," stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (clauses condition) stream))))
     
(define-condition malformed-typecase-clause (program-error)
  ((%clause :initarg :clause :reader clause))
  (:report
   (lambda (condition stream)
     (princ "Expected a typecase clause of the form," stream)
     (terpri stream)
     (princ "(type form*)," stream)
     (terpri stream)
     (princ "but found: " stream)
     (print (clause condition) stream))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at runtime

(define-condition ecase-type-error (type-error)
  ()
  (:report
   (lambda (condition stream)
     (princ "No key matched in ecase expression." stream)
     (terpri stream)
     (princ "Offending datum: " stream)
     (print (type-error-datum condition) stream)
     (princ "Offending datum: " stream)
     (print (type-error-expected-type condition) stream))))

(define-condition ccase-type-error (type-error)
  ()
  (:report
   (lambda (condition stream)
     (princ "No key matched in ccase expression." stream)
     (terpri stream)
     (princ "Offending datum: " stream)
     (print (type-error-datum condition) stream)
     (princ "Offending datum: " stream)
     (print (type-error-expected-type condition) stream))))

(define-condition etypecase-type-error (type-error)
  ()
  (:report
   (lambda (condition stream)
     (princ "No key matched in etypecase expression." stream)
     (terpri stream)
     (princ "Offending datum: " stream)
     (print (type-error-datum condition) stream)
     (princ "Offending datum: " stream)
     (print (type-error-expected-type condition) stream))))

(define-condition ctypecase-type-error (type-error)
  ()
  (:report
   (lambda (condition stream)
     (princ "No key matched in ctypecase expression." stream)
     (terpri stream)
     (princ "Offending datum: " stream)
     (print (type-error-datum condition) stream)
     (princ "Offending datum: " stream)
     (print (type-error-expected-type condition) stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities

;;; Check that an object is a proper list.
;;; We could have used the AND macro here, but
;;; we would have had to move this utility then,
;;; so we expand the AND macro manually.
(eval-when (:compile-toplevel :load-toplevel)

  (defun proper-list-p (object)
    (if (null object)
	t
	(if (consp object)
	    (proper-list-p (cdr object))
	    nil)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the macros

(defmacro or (&rest forms)
  (if (null forms)
      nil
      (if (not (consp forms))
	  (error 'malformed-body :body forms)
	  (if (null (cdr forms))
	      (car forms)
	      (let ((temp-var (gensym)))
		`(let ((,temp-var ,(car forms)))
		   (if ,temp-var
		       ,temp-var
		       (or ,@(cdr forms)))))))))

(defmacro and (&rest forms)
  (if (null forms)
      t
      (if (not (consp forms))
	  (error 'malformed-body :body forms)
	  (if (null (cdr forms))
	      (car forms)
	      `(if ,(car forms)
		   (and ,@(cdr forms))
		   nil)))))

(defmacro when (form &body body)
  (if (not (proper-list-p body))
      (error 'malformed-body :body body)
      `(if ,form
	   (progn ,@body)
	   nil)))

(defmacro unless (form &body body)
  (if (not (proper-list-p body))
      (error 'malformed-body :body body)
      `(if ,form
	   nil
	   (progn ,@body))))

(defmacro cond (&rest clauses)
  (if (not (proper-list-p clauses))
      (error 'malformed-cond-clauses :clauses clauses)
      (if (null clauses)
	  nil
	  (let ((clause (car clauses)))
	    (if (not (and (proper-list-p clause)
			  (not (null clause))))
		(error 'malformed-cond-clause
		       :clause clause)
		(if (null (cdr clause))
		    `(or ,(car clause)
			 (cond ,@(cdr clauses)))
		    `(if ,(car clause)
			 (progn ,@(cdr clause))
			 (cond ,@(cdr clauses)))))))))

;;; Take a list of keys (known to be a proper list), and the name of a
;;; variable, and produce a list of forms (eql <variable> key).
(defun eql-ify (keys variable)
  (if (null keys)
      '()
      (cons `(eql ,variable ,(car keys))
	    (eql-ify (cdr keys) variable))))

;;; This function turns a list of CASE clauses into nested IFs.  It
;;; checks that the list of clauses is a proper list and that each
;;; clause is also a proper list.  It also checks that, if there is an
;;; otherwise clause, it is the last one.
(defun expand-case-clauses (clauses variable)
  (if (null clauses)
      'nil
      (if (not (consp clauses))
	  (error 'malformed-case-clauses
		 :clauses clauses)
	  (let ((clause (car clauses)))
	    (unless (and (proper-list-p clause)
			 (not (null clause)))
	      (error 'malformed-case-clause
		     :clause clause))
	    (if (or (eq (car clause) 'otherwise)
		    (eq (car clause) t))
		(if (null (cdr clauses))
		    `(progn ,@(cdr clause))
		    (error 'otherwise-clause-not-last
			   :clauses (cdr clauses)))
		;; it is a normal clause
		(let ((keys (car clause))
		      (forms (cdr clause)))
		  (if (and (atom keys)
			   (not (null keys)))
		      `(if (eql ,variable ,keys)
			   (progn ,@forms)
			   ,(expand-case-clauses (cdr clauses) variable))
		      (if (not (proper-list-p keys))
			  (error 'malformed-keys
				 :keys keys)
			  `(if (or ,@(eql-ify keys variable))
			       (progn ,@forms)
			       ,(expand-case-clauses (cdr clauses) variable))))))))))

(defmacro case (keyform &rest clauses)
  (let ((variable (gensym)))
    `(let ((,variable ,keyform))
       ,(expand-case-clauses clauses variable))))

;;; Collect a list of all the keys for ecase or ccase 
;;; to be used as the `exptected type' in error reporting.
(defun collect-e/ccase-keys (clauses)
  (if (null clauses)
      nil
      (append 
       (let ((keys (caar clauses)))
	 (if (and (atom keys)
		  (not (null keys)))
	     (list keys)
	     (if (not (proper-list-p keys))
		 (error 'malformed-keys
			:keys keys)
		 keys)))
       (collect-e/ccase-keys (cdr clauses)))))

;;; Expand a list of clauses for ECASE or CCASE.  We turn the clauses
;;; into nested IFs, where the innermost form (final) depends on
;;; whether we use ecase or ccase.  We check that the list of clauses
;;; is a proper list, and that each clause is a proper list.
(defun expand-e/ccase-clauses (clauses variable final)
  (if (null clauses)
      final
      (if (not (consp clauses))
	  (error 'malformed-case-clauses
		 :clauses clauses)
	  (let ((clause (car clauses)))
	    (unless (and (proper-list-p clause)
			 (not (null clause)))
	      (error 'malformed-case-clause
		     :clause clause))
	    (let ((keys (car clause))
		  (forms (cdr clause)))
	      (if (and (atom keys)
		       (not (null keys)))
		  `(if (eql ,variable ,keys)
		       (progn ,@forms)
		       ,(expand-e/ccase-clauses (cdr clauses) variable final))
		  `(if (or ,@(eql-ify keys variable))
		       (progn ,@forms)
		       ,(expand-e/ccase-clauses (cdr clauses) variable final))))))))

;;; For ECASE, the default is to signal a type error. 
(defmacro ecase (keyform &rest clauses)
  (let* ((variable (gensym))
	 (keys (collect-e/ccase-keys clauses))
	 (final `(error 'ecase-type-error
			:datum ,variable
			:expected-type '(member ,@keys))))
    `(let ((,variable ,keyform))
       ,(expand-e/ccase-clauses clauses variable final))))

;;; This function is does the same thing as
;;; (mapcar #'list vars vals), but since we are not
;;; using mapping functions here, we have to 
;;; implement it recursively. 
(defun compute-let*-bindings (vars vals)
  (if (null vars)
      '()
      (cons (list (car vars) (car vals))
	    (compute-let*-bindings (cdr vars) (cdr vals)))))

;;; For CCASE, the default is to signal a correctable
;;; error, allowing a new value to be stored in the
;;; place passed as argument to CCASE, using the restart
;;; STORE-VALUE.  We use GET-SETF-EXPANSION so as to
;;; avoid multiple evaluation of the subforms of the place, 
;;; even though the HyperSpec allows such multiple evaluation. 
(defmacro ccase (keyplace &rest clauses &environment env)
  (multiple-value-bind (vars vals store-vars writer-forms reader-forms)
      (get-setf-expansion keyplace env)
    (let* ((label (gensym))
	   (keys (collect-e/ccase-keys clauses))
	   (final `(restart-case (error 'ccase-type-error
					:datum ,(car store-vars)
					:expected-type '(member ,@keys))
				 (store-value (v)
					      :interactive
					      (lambda ()
						(format *query-io*
							"New value: ")
						(list (read *query-io*)))
					      :report "Supply a new value"
					      (setq ,(car store-vars) v)
					      ,writer-forms
					      (go ,label)))))
      `(let* ,(compute-let*-bindings vars vals)
	 (declare (ignorable ,@vars))
	 (multiple-value-bind ,store-vars ,reader-forms
	   (tagbody
	      ,label
	      ,(expand-e/ccase-clauses clauses (car store-vars) final)))))))

;;; Turn a list of TYPECASE clauses into nested IFs.  We check that
;;; the list of clauses is a proper list, that each clause is a proper
;;; list as well, and that, if there is an otherwise clause, it is the
;;; last one.
(defun expand-typecase-clauses (clauses variable)
  (if (null clauses)
      'nil
      (if (not (consp clauses))
	  (error 'malformed-typecase-clauses
		 :clauses clauses)
	  (let ((clause (car clauses)))
	    (unless (and (proper-list-p clause)
			 (not (null clause)))
	      (error 'malformed-typecase-clause
		     :clause clause))
	    (if (or (eq (car clause) 'otherwise)
		    (eq (car clause) t))
		(if (null (cdr clauses))
		    `(progn ,@(cdr clauses))
		    (error 'otherwise-clause-not-last
			   :clauses (cdr clauses)))
		;; it is a normal clause
		(let ((type (car clause))
		      (forms (cdr clause)))
		  `(if (typep ,variable ,type)
		       (progn ,@forms)
		       ,(expand-typecase-clauses (cdr clauses) variable))))))))

(defmacro typecase (keyform &rest clauses)
  (let ((variable (gensym)))
    `(let ((,variable ,keyform))
       ,(expand-typecase-clauses clauses variable))))

;;; Collect a list of all the types for etypecase or ctypecase 
;;; to be used as the `exptected type' in error reporting.
(defun collect-e/ctypecase-keys (clauses)
  (if (null clauses)
      nil
      (cons (caar clauses)
	    (collect-e/ctypecase-keys (cdr clauses)))))

;;; Turn a list of clauses for ETYPCASE or CTYPECASE into nested IFs.
;;; We check that the list of clauses is a proper list, and that each
;;; clause is a proper list.  The default case depends on whether we
;;; have a CCTYPECASE or an ETYPECASE, so we pass that as an argument
;;; (final).
(defun expand-e/ctypecase-clauses (clauses variable final)
  (if (null clauses)
      final
      (if (not (consp clauses))
	  (error 'malformed-typecase-clauses
		 :clauses clauses)
	  (let ((clause (car clauses)))
	    (unless (and (proper-list-p clause)
			 (not (null clause)))
	      (error 'malformed-typecase-clause
		     :clause clause))
	    (let ((type (car clause))
		  (forms (cdr clause)))
	      `(if (typep ,variable ,type)
		   (progn ,@forms)
		   ,(expand-e/ctypecase-clauses (cdr clauses) variable final)))))))

;;; As with ECASE, the default for ETYPECASE is to signal an error.
(defmacro etypecase (keyform &rest clauses)
  (let* ((variable (gensym))
	 (keys (collect-e/ctypecase-keys clauses))
	 (final `(error 'etypecase-type-error
			:datum ,variable
			:expected-type '(member ,@keys))))
    `(let ((,variable ,keyform))
       ,(expand-e/ctypecase-clauses clauses variable final))))

;;; As with CCASE, the default for CTYPECASE is is to signal a
;;; correctable error, and to allow the value to be altered by the
;;; STORE-VALUE restart.
(defmacro ctypecase (keyplace &rest clauses &environment env)
  (multiple-value-bind (vars vals store-vars writer-forms reader-forms)
      (get-setf-expansion keyplace env)
    (let* ((label (gensym))
	   (keys (collect-e/ctypecase-keys clauses))
	   (final `(restart-case (error 'ctypecase-type-error
					:datum ,(car store-vars)
					:expected-type '(member ,@keys))
				 (store-value (v)
					      :interactive
					      (lambda ()
						(format *query-io*
							"New value: ")
						(list (read *query-io*)))
					      :report "Supply a new value"
					      (setq ,(car store-vars) v)
					      ,writer-forms
					      (go ,label)))))
      `(let* ,(compute-let*-bindings vars vals)
	 (declare (ignorable ,@vars))
	 (multiple-value-bind ,store-vars ,reader-forms
	   (tagbody
	      ,label
	      ,(expand-e/ctypecase-clauses clauses (car store-vars) final)))))))
