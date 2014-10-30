;;;; Copyright (c) 2008 - 2013
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

;;;; This file is part of the conditionals module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file conditionals.text for a description of the module.

;;; This implementation also does not use the format function, and
;;; instead uses print and princ for error reporting.  This makes it
;;; possible for format to use the conditional constructs define here.

(in-package #:sicl-conditionals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros CASE, ECASE, CCASE.
;;;
;;; A normal CASE/ECASE/CCASE clause has the form (KEYS FORM*) where
;;; KEYS is a designator for a list of objects, except that for CASE,
;;; the symbols T and OTHERWISE may not be used as such.  Instead,
;;; when T or OTHERWISE are present in the CAR of a clause, then they
;;; do not designate a list of objects, and instead that clause is an
;;; otherwise-clause.  For ECASE and CCASE, T and OTHERWISE can be
;;; used as as designators for lists, and they then designate the
;;; singleton list containing itself. 
;;;
;;; In the glossary of the HyperSpec (under "list designator"), we
;;; learn that a list designator is ether a non-NIL atom, in which
;;; case the denoted list is the list containing that one atom, or
;;; else it is a proper list, and the denoted list is that list.  In
;;; particular, this means that if NIL (or equivalently `()') is used
;;; in the CAR of a CASE clause, then the denoted list is the empty
;;; list and NOT the list containing NIL.  Thus, to obtain the
;;; singleton list containing NIL, the user has to use `(NIL)'. 

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
		 :name 'case
		 :clauses clauses)
	  (let ((clause (car clauses)))
	    (unless (and (cleavir-code-utilities:proper-list-p clause)
			 (not (null clause)))
	      (error 'malformed-case-clause
		     :name 'case
		     :clause clause))
	    (if (or (eq (car clause) 'otherwise)
		    (eq (car clause) t))
		(if (null (cdr clauses))
		    `(progn ,@(cdr clause))
		    (error 'otherwise-clause-not-last
			   :name 'case
			   :clauses (cdr clauses)))
		;; it is a normal clause
		(let ((keys (car clause))
		      (forms (cdr clause)))
		  (if (and (atom keys)
			   (not (null keys)))
		      `(if (eql ,variable ,keys)
			   (progn ,@forms)
			   ,(expand-case-clauses (cdr clauses) variable))
		      (if (not (cleavir-code-utilities:proper-list-p keys))
			  (error 'malformed-keys
				 :name 'case
				 :keys keys)
			  `(if (or ,@(eql-ify keys variable))
			       (progn ,@forms)
			       ,(expand-case-clauses (cdr clauses) variable))))))))))

;;; Collect a list of all the keys for ecase or ccase 
;;; to be used as the `exptected type' in error reporting.
(defun collect-e/ccase-keys (clauses name)
  (if (null clauses)
      nil
      (append 
       (let ((keys (caar clauses)))
	 (if (and (atom keys)
		  (not (null keys)))
	     (list keys)
	     (if (not (cleavir-code-utilities:proper-list-p keys))
		 (error 'malformed-keys
			:name name
			:keys keys)
		 keys)))
       (collect-e/ccase-keys (cdr clauses) name))))

;;; Expand a list of clauses for ECASE or CCASE.  We turn the clauses
;;; into nested IFs, where the innermost form (final) depends on
;;; whether we use ecase or ccase.  We check that the list of clauses
;;; is a proper list, and that each clause is a proper list.
(defun expand-e/ccase-clauses (clauses variable final name)
  (if (null clauses)
      final
      (if (not (consp clauses))
	  (error 'malformed-case-clauses
		 :name name
		 :clauses clauses)
	  (let ((clause (car clauses)))
	    (unless (and (cleavir-code-utilities:proper-list-p clause)
			 (not (null clause)))
	      (error 'malformed-case-clause
		     :name name
		     :clause clause))
	    (let ((keys (car clause))
		  (forms (cdr clause)))
	      (if (and (atom keys)
		       (not (null keys)))
		  `(if (eql ,variable ,keys)
		       (progn ,@forms)
		       ,(expand-e/ccase-clauses (cdr clauses) variable final name))
		  `(if (or ,@(eql-ify keys variable))
		       (progn ,@forms)
		       ,(expand-e/ccase-clauses (cdr clauses) variable final name))))))))

;;; This function is does the same thing as
;;; (mapcar #'list vars vals), but since we are not
;;; using mapping functions here, we have to 
;;; implement it recursively. 
(defun compute-let*-bindings (vars vals)
  (if (null vars)
      '()
      (cons (list (car vars) (car vals))
	    (compute-let*-bindings (cdr vars) (cdr vals)))))

;;; Turn a list of TYPECASE clauses into nested IFs.  We check that
;;; the list of clauses is a proper list, that each clause is a proper
;;; list as well, and that, if there is an otherwise clause, it is the
;;; last one.
(defun expand-typecase-clauses (clauses variable)
  (if (null clauses)
      'nil
      (if (not (consp clauses))
	  (error 'malformed-typecase-clauses
		 :name 'typecase
		 :clauses clauses)
	  (let ((clause (car clauses)))
	    (unless (and (cleavir-code-utilities:proper-list-p clause)
			 (not (null clause)))
	      (error 'malformed-typecase-clause
		     :name 'typecase
		     :clause clause))
	    (if (or (eq (car clause) 'otherwise)
		    (eq (car clause) t))
		(if (null (cdr clauses))
		    `(progn ,@(cdr clauses))
		    (error 'otherwise-clause-not-last
			   :name 'typecase
			   :clauses (cdr clauses)))
		;; it is a normal clause
		(let ((type (car clause))
		      (forms (cdr clause)))
		  `(if (typep ,variable ,type)
		       (progn ,@forms)
		       ,(expand-typecase-clauses (cdr clauses) variable))))))))

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
(defun expand-e/ctypecase-clauses (clauses variable final name)
  (if (null clauses)
      final
      (if (not (consp clauses))
	  (error 'malformed-typecase-clauses
		 :name name
		 :clauses clauses)
	  (let ((clause (car clauses)))
	    (unless (and (cleavir-code-utilities:proper-list-p clause)
			 (not (null clause)))
	      (error 'malformed-typecase-clause
		     :name name
		     :clause clause))
	    (let ((type (car clause))
		  (forms (cdr clause)))
	      `(if (typep ,variable ,type)
		   (progn ,@forms)
		   ,(expand-e/ctypecase-clauses (cdr clauses) variable final name)))))))

