(cl:in-package #:sicl-extrinsic-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.

(defun tie (untied arg-forms environment1 environment2)
  (let* ((args (loop for arg-form in arg-forms
		     collect (cleavir-env:eval
			      arg-form environment1 environment2)))
	 (result (apply untied args)))
    (if (typep result 'fun)
	(let ((tied (make-instance 'fun)))
	  (setf (untied tied) untied)
	  (setf (arg-forms tied) arg-forms)
	  (closer-mop:set-funcallable-instance-function tied result)
	  tied)
	result)))

(defparameter *caching* nil)

;;; This variable is part of an attempt at caching the result of
;;; loading files.  It is bound by SICL-EXTRINSIC-ENVIRONMENT:LOAD to
;;; the hash value (computed by SXHASH) of the form.  Here, we check
;;; whether this variable is bound, and if so, whether we have already
;;; compiled the form in question, in which case, we just return the
;;; result of that compilation.  Otherwise, we compile the form and
;;; cache the result.
(defparameter *form-being-compiled* nil)

;;; This variable holds an EQUAL hash table mapping hash values
;;; (computed by SXHASH) of forms to compiled functions.
(defparameter *compiled-form-cache* (make-hash-table :test #'eql))

(defmethod cleavir-env:eval (form environment1 (environment2 environment))
  (cond ((and (consp form)
	      (consp (cdr form))
	      (null (cddr form))
	      (eq (car form) 'quote))
	 (cadr form))
	((and (symbolp form)
	      (nth-value 1 (sicl-global-environment:constant-variable
			    form environment1)))
	 (nth-value 0 (sicl-global-environment:constant-variable
		       form environment1)))
	((and (atom form) (not (symbolp form)))
	 form)
	((and (consp form)
	      (consp (cdr form))
	      (consp (cddr form))
	      (null (cdddr form))
	      (eq (car form) 'sicl-global-environment:function-cell)
	      (eq (caddr form) 'sicl-global-environment:*global-environment*)
	      (consp (cadr form))
	      (consp (cdr (cadr form)))
	      (null (cddr (cadr form)))
	      (eq (car (cadr form)) 'quote))
	 (sicl-global-environment:function-cell (cadr (cadr form))
						environment2))
	((and (consp form)
	      (consp (cdr form))
	      (consp (cddr form))
	      (null (cdddr form))
	      (eq (car form) 'sicl-global-environment:function-cell)
	      (equal (caddr form) '(sicl-global-environment:global-environment))
	      (consp (cadr form))
	      (consp (cdr (cadr form)))
	      (null (cddr (cadr form)))
	      (eq (car (cadr form)) 'quote))
	 (sicl-global-environment:function-cell (cadr (cadr form))
						environment2))
	((and (consp form)
	      (consp (cdr form))
	      (consp (cddr form))
	      (null (cdddr form))
	      (eq (car form) 'sicl-global-environment:variable-cell)
	      (eq (caddr form) 'sicl-global-environment:*global-environment*)
	      (consp (cadr form))
	      (consp (cdr (cadr form)))
	      (null (cddr (cadr form)))
	      (eq (car (cadr form)) 'quote))
	 (sicl-global-environment:variable-cell (cadr (cadr form))
						environment2))
	((and (consp form)
	      (consp (cdr form))
	      (consp (cddr form))
	      (null (cdddr form))
	      (eq (car form) 'sicl-global-environment:variable-cell)
	      (equal (caddr form) '(sicl-global-environment:global-environment))
	      (consp (cadr form))
	      (consp (cdr (cadr form)))
	      (null (cddr (cadr form)))
	      (eq (car (cadr form)) 'quote))
	 (sicl-global-environment:variable-cell (cadr (cadr form))
						environment2))
	((eq form 'sicl-global-environment:*global-environment*)
	 environment2)
	(t
	 (let (fun-and-forms)
	   (let* ((cleavir-generate-ast:*compiler* 'cl:eval)
		  (ast (cleavir-generate-ast:generate-ast form environment1 nil))
		  (ast-bis (cleavir-ast-transformations:hoist-load-time-value ast))
		  (hir (cleavir-ast-to-hir:compile-toplevel ast-bis))
		  (ignore (cleavir-hir-transformations:eliminate-typeq hir))
		  (lambda-expr (translate hir environment2))
		  (fun (compile nil lambda-expr)))
	     (declare (ignore ignore))
	     (setf fun-and-forms (cons fun (cleavir-ir:forms hir))))
	   (tie (car fun-and-forms) (cdr fun-and-forms)
		environment1 environment2)))))
