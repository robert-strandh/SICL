(in-package #:cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenience functions for augmenting the environment with a bunch
;;; of declarations.

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
;;; Converting code to an abstract syntax tree.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting ordinary Common Lisp code.

(defgeneric convert (form environment))

(defun convert-top-level-form (form)
  (convert `(function (lambda () ,form)) nil))

(defun convert-top-level-lamda-expression (lambda-expression)
  (unless (and (consp lambda-expression)
	       (eq (car lambda-expression) 'lambda))
    (error "argument must be a lambda expression"))
  (convert `(function ,lambda-expression) nil))

(defun convert-for-inlining (lambda-expression)
  (let* ((lambda-list (cadr lambda-expression))
	 (let-bindings (loop for var in lambda-list
			     for i from 0
			     collect `(,var (arg ,i)))))
    (let ((ast (convert `(let ,let-bindings ,@(cddr lambda-expression)) nil)))
      ;; The AST looks like this:
      ;; (progn (setq <a0> (arg 0)) (progn (setq <a1> (arg 1)) ....
      (loop for arg in lambda-list
	    collect (first (cleavir-ast:children (first (cleavir-ast:children ast))))
	      into lexical-asts
	    do (setf ast (second (cleavir-ast:children ast)))
	    finally (return (values lexical-asts ast))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a sequence of forms.

(defun convert-sequence (forms environment)
  (loop for form in forms
	collect (convert form environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form when the head of a compound form is a
;;; CONS.  Then the head must be a lambda expression.  We replace a
;;; call such as ((lambda (params) . body) . args) by (flet ((temp
;;; (params) . body)) (temp . args))
;;;
;;; FIXME: do some more error checking.
(defun convert-lambda-call (form env)
  (destructuring-bind ((lambda lambda-list &rest body) &rest args) form
    (declare (ignore lambda))
    (cleavir-ast:make-call-ast
     (convert-code lambda-list body env)
     (convert-sequence args env))))

(defun add-lambda-list-to-env (lambda-list env)
  (let ((rest lambda-list)
	(new-env env))
    (tagbody
     required
       (cond ((null rest) (go out))
	     ((eq (car rest) '&optional) (pop rest) (go optional))
	     ((eq (car rest) '&key) (pop rest) (go key))
	     (t (let* ((var (pop rest))
		       (var-ast (cleavir-ast:make-lexical-ast var)))
		  (setq new-env
			(cleavir-env:add-lexical-variable
			 new-env var var-ast)))
		(go required)))
     optional
       (cond ((null rest) (go out))
	     ((eq (car rest) '&key) (pop rest) (go key))
	     (t (let* ((var1 (first (first rest)))
		       (var1-ast (cleavir-ast:make-lexical-ast var1))
		       (var2 (second (first rest)))
		       (var2-ast (cleavir-ast:make-lexical-ast var2)))
		  (setq new-env
			(cleavir-env:add-lexical-variable
			 new-env var1 var1-ast))
		  (setq new-env
			(cleavir-env:add-lexical-variable
			 new-env var2 var2-ast)))
		(pop rest)
		(go optional)))
     key
       (cond ((or (null rest) (eq (car rest) '&allow-other-keys))
	      (go out))
	     (t (let* ((var1 (second (first rest)))
		       (var1-ast (cleavir-ast:make-lexical-ast var1))
		       (var2 (third (first rest)))
		       (var2-ast (cleavir-ast:make-lexical-ast var2)))
		  (setq new-env
			(cleavir-env:add-lexical-variable
			 new-env var1 var1-ast))
		  (setq new-env
			(cleavir-env:add-lexical-variable
			 new-env var2 var2-ast)))
		(pop rest)
		(go key)))
     out)
    new-env))

(defun var-to-lexical-identity (var env)
  (let ((info (cleavir-env:variable-info env var)))
    (cleavir-env:identity info)))

(defun build-ast-lambda-list (lambda-list env)
  (let ((rest lambda-list)
	(result nil))
    (tagbody
     required
       (cond ((null rest)
	      (go out))
	     ((eq (car rest) '&optional)
	      (push (pop rest) result)
	      (go optional))
	     ((eq (car rest) '&key)
	      (push (pop rest) result)
	      (go key))
	     (t (push (var-to-lexical-identity (pop rest) env) result)
		(go required)))
     optional
       (cond ((null rest)
	      (go out))
	     ((eq (car rest) '&key)
	      (push (pop rest) result)
	      (go key))
	     (t (push (list (var-to-lexical-identity (first (first rest)) env)
			    (var-to-lexical-identity (second (first rest)) env))
		      result)
		(pop rest)
		(go optional)))
     key
       (cond ((or (null rest) (eq (car rest) '&allow-other-keys))
	      (go out))
	     (t (push (list (first (first rest))
			    (var-to-lexical-identity (second (first rest)) env)
			    (var-to-lexical-identity (third (first rest)) env))
		      result)
		(pop rest)
		(go key)))
     out)
    (nreverse result)))

(defun convert-code (lambda-list body env)
  (let ((parsed-lambda-list
	  (cleavir-code-utilities:parse-ordinary-lambda-list lambda-list)))
    (multiple-value-bind (entry-lambda-list initforms)
	(cleavir-code-utilities:preprocess-lambda-list parsed-lambda-list)
      (let* ((new-env (add-lambda-list-to-env entry-lambda-list env))
	     (ast-lambda-list (build-ast-lambda-list entry-lambda-list new-env)))
	(cleavir-ast:make-function-ast
	 (convert `(progn ,@initforms ,@body) new-env)
	 ast-lambda-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT-FORM is called by CONVERT when the form is either a symbol
;;; or a CONS with a symbol in the CAR.  It is called with three
;;; arguments: 1. the form;  2. the environment INFO resulting from
;;; calling VARIABLE-INFO on the symbol or from calling FUNCTION-INFO
;;; on the CAR of the CONS;  3. the environment.

(defgeneric convert-form (form info environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT is responsible for converting a form to an abstract syntax
;;; tree.  It is only called when it is known that the form to be
;;; converted is not a top-level form.

(defmethod convert (form environment)
  (cond ((and (not (consp form)) (not (symbolp form)))
	 (cleavir-ast:make-constant-ast form))
	((and (symbolp form) (constantp form))
	 (cleavir-ast:make-constant-ast (symbol-value form)))
	((symbolp form)
	 (let ((info (cleavir-env:variable-info environment form)))
	   (convert-form form info environment)))
	((symbolp (car form))
	 (let ((info (cleavir-env:function-info environment (car form))))
	   (convert-form form info environment)))
	(t
	 (convert-lambda-call form environment))))

(defun generate-ast (form environment)
  (convert form environment))
