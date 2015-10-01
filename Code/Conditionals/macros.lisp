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

(cl:in-package #:sicl-conditionals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the macros.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro OR.

(defmacro or (&rest forms)
  (labels ((aux (forms)
	     (if (null (cdr forms))
		 (car forms)
		 (let ((temp-var (gensym)))
		   `(let ((,temp-var ,(car forms)))
		      (if ,temp-var
			  ,temp-var
			  ,(aux (cdr forms))))))))
    (if (null forms)
	nil
	(aux forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro AND.

(defmacro and (&rest forms)
  (labels ((aux (forms)
	     (if (null (cdr forms))
		 (car forms)
		 `(if ,(car forms)
		      ,(aux (cdr forms))
		      nil))))
    (if (null forms)
	t
	(aux forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro WHEN.

(defmacro when (test &body body)
  `(if ,test
       (progn ,@body)
       nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro UNLESS.

(defmacro unless (test &body body)
  `(if ,test
       nil
       (progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro COND.

(defmacro cond (&rest clauses)
  (labels ((aux (clauses)
	     (if (null clauses)
		 nil
		 (let ((clause (car clauses)))
		   (if (not (and (cleavir-code-utilities:proper-list-p clause)
				 (not (null clause))))
		       (error 'malformed-cond-clause
			      :name 'cond
			      :clause clause)
		       (if (null (cdr clause))
			   `(or ,(car clause)
				,(aux (cdr clauses)))
			   `(if ,(car clause)
				(progn ,@(cdr clause))
				,(aux (cdr clauses)))))))))
    (aux clauses)))

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

(defmacro case (keyform &rest clauses)
  (let ((variable (gensym)))
    `(let ((,variable ,keyform))
       ,(expand-case-clauses clauses variable))))

;;; For ECASE, the default is to signal a type error. 
(defmacro ecase (keyform &rest clauses)
  (let* ((variable (gensym))
	 (keys (collect-e/ccase-keys clauses 'ecase))
	 (final `(error 'ecase-type-error
			:name 'ecase
			:datum ,variable
			:expected-type '(member ,@keys))))
    `(let ((,variable ,keyform))
       ,(expand-e/ccase-clauses clauses variable final 'ecase))))

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
	   (keys (collect-e/ccase-keys clauses 'ccase))
	   (final `(restart-case (error 'ccase-type-error
					:name 'ccase
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
	      ,(expand-e/ccase-clauses clauses (car store-vars) final 'ccase)))))))
(defmacro typecase (keyform &rest clauses)
  (let ((variable (gensym)))
    `(let ((,variable ,keyform))
       ,(expand-typecase-clauses clauses variable))))

;;; As with ECASE, the default for ETYPECASE is to signal an error.
(defmacro etypecase (keyform &rest clauses)
  (let* ((variable (gensym))
	 (keys (collect-e/ctypecase-keys clauses))
	 (final `(error 'etypecase-type-error
			:name 'etypecase
			:datum ,variable
			:expected-type '(member ,@keys))))
    `(let ((,variable ,keyform))
       ,(expand-e/ctypecase-clauses clauses variable final 'etypecase))))

;;; As with CCASE, the default for CTYPECASE is is to signal a
;;; correctable error, and to allow the value to be altered by the
;;; STORE-VALUE restart.
(defmacro ctypecase (keyplace &rest clauses &environment env)
  (multiple-value-bind (vars vals store-vars writer-forms reader-forms)
      (get-setf-expansion keyplace env)
    (let* ((label (gensym))
	   (keys (collect-e/ctypecase-keys clauses))
	   (final `(restart-case (error 'ctypecase-type-error
					:name 'ctypecase
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
	      ,(expand-e/ctypecase-clauses clauses (car store-vars) final 'ctypecase)))))))

