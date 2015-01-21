(cl:in-package #:sicl-iteration)

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
;;; Author: Robert Strandh (robert.strandh@gmail.com)
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
  (binding-var-must-be-symbol 'dolist var)
  (list-form-must-be-list 'dolist list-form)
  (body-must-be-proper-list 'dolist body)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body body)
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
  (binding-var-must-be-symbol 'dotimes var)
  (count-form-must-be-nonnegative-integer 'dotimes count-form)
  (body-must-be-proper-list 'dotimes body)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body body)
    (let ((start-tag (gensym))
	  (end-tag (gensym))
	  (count-var (gensym)))
      `(let ((,count-var ,count-form)
	     (,var 0))
	 (declare (type unsigned-byte ,var))
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

(macrolet ((define-do (name assignment-type)
             (multiple-value-bind (let-type setq-type)
                 (ecase assignment-type
                   (:sequential (values 'let* 'setq))
                   (:parallel (values 'let 'psetq)))
               `(defmacro ,name (variable-clauses end-test
                                 &body body)
                  ;; do some syntax checking
                  (check-variable-clauses ',name variable-clauses)
                  (body-must-be-proper-list ',name body)
                  (unless (and (cleavir-code-utilities:proper-list-p end-test)
                               (not (null end-test)))
                    (error 'malformed-end-test
                           :name ',name
                           :found end-test))
                  (multiple-value-bind (declarations forms)
                      (cleavir-code-utilities:separate-ordinary-body body)
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
