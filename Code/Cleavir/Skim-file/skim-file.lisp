(cl:in-package #:cleavir-skim-file)

(defparameter *compile-time-too* nil)

(defgeneric skim-special (symbol form environment))

(defun skim-form (form environment)
  (if *compile-time-too*
      (cleavir-env:eval form environment environment)      
      (when (and (consp form) (symbolp (first form)))
	(let ((info (cleavir-env:function-info environment (first form))))
	  (when (typep info 'cleavir-env:special-operator-info)
	    (skim-special (first form) form environment))))))

(defun skim-file (filename environment)
  (with-open-file (stream filename :direction :input)
    (loop with eof-value = (list)
	  for top-level-form = (read stream nil eof-value)
	  until (eq top-level-form eof-value)
	  do (skim-form top-level-form environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenience functions for augmenting the environment with a bunch
;;; of declarations.
;;;
;;; FIXME: this code is identical to the code in the component
;;; Generate-AST/generate-ast.lisp.  Try to factor maybe.

(defun augment-environment-with-declaration (declaration environment)
  (destructuring-bind (head . rest) declaration
    (let ((new-env (case head
		     ;; (declaration
		     ;; (make-declaration-declaration-entry (car rest)))
		     (dynamic-extent
		      (if (consp (car rest))
			  (cleavir-env:add-function-dynamic-extent
			   environment (cadr (car rest)))
			  (cleavir-env:add-variable-dynamic-extent
			   environment (car rest))))
		     (ftype
		      (cleavir-env:add-function-type
		       environment (cadr rest) (car rest)))
		     ((ignore ignorable)
		      (if (consp (car rest))
			  (cleavir-env:add-function-ignore
			   environment (cadr (car rest)) head)
			  (cleavir-env:add-variable-ignore
			   environment (car rest) head)))
		     ((inline notinline)
		      (cleavir-env:add-inline
		       environment (car rest) head))
		     ;; (optimize
		     ;; (make-optimize-declaration-entry
		     ;; (car (car rest)) (cadr (car rest))))
		     ;; (special
		     ;; FIXME: is this right?
		     ;; (make-special-variable-entry (car rest)))
		     (type
		      (cleavir-env:add-function-type
		       environment (cadr rest) (car rest))))))
      new-env)))

(defun augment-environment-with-declarations (environment declarations)
  (let ((declaration-specifiers
	  (cleavir-code-utilities:canonicalize-declaration-specifiers
	   (reduce #'append (mapcar #'cdr declarations))))
	(new-env environment))
    (loop for spec in declaration-specifiers
	  do (setf new-env (augment-environment-with-declaration spec new-env)))
    new-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SKIM-SPECIAL.

(defmethod skim-special (symbol form env)
  (declare (ignore symbol form env)))

(defmethod skim-special
    ((symbol (eql 'locally)) form env)
  (multiple-value-bind (declarations body-forms)
      (cleavir-code-utilities:separate-ordinary-body (rest form))
    (let ((new-env (augment-environment-with-declarations
		    env declarations)))
      (loop for body-form in body-forms
	    do (skim-form body-form new-env)))))

(defmethod skim-special
    ((symbol (eql 'macrolet)) form env)
  (destructuring-bind (definitions &rest body) (rest form)
    (let ((new-env env))
      (loop for (name lambda-list . body) in definitions
	    for lambda-expr = (cleavir-code-utilities:parse-macro
			        name lambda-list body env)
	    for expander = (cleavir-env:eval lambda-expr env env)
	    do (setf new-env
		     (cleavir-env:add-local-macro new-env name expander)))
      (skim-form `(locally ,@body) new-env))))
