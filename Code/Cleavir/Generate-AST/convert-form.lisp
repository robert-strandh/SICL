(cl:in-package #:cleavir-generate-ast)

(defmethod convert-form (form (info cleavir-env:symbol-macro-info) env)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (lambda (form env)
			      (declare (ignore form env))
			      (cleavir-env:expansion info))
			    form
			    env)))
    (convert expansion env)))

(defmethod convert-form (form (info cleavir-env:constant-variable-info) env)
  (cleavir-ast:make-constant-ast (cleavir-env:value info)))

(defmethod convert-form (form (info cleavir-env:lexical-variable-info) env)
  (when (eq (cleavir-env:ignore info) 'ignore)
    (warn "Reference to a variable declared IGNORE"))
  (find-or-create-ast (cleavir-env:identity info)))

(defmethod convert-form (form (info cleavir-env:special-variable-info) env)
  (convert `(symbol-value ',(cleavir-env:name info)) env))

(defmethod convert-form (form (info cleavir-env:local-macro-info) env)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (cleavir-env:expander info)
			    form
			    env)))
    (convert expansion env)))

(defmethod convert-form (form (info cleavir-env:global-macro-info) env)
  (let ((compiler-macro (cleavir-env:compiler-macro info)))
    (if (null compiler-macro)
	;; There is no compiler macro, so we just apply the macro
	;; expander, and then convert the resulting form.
	(convert (funcall (coerce *macroexpand-hook* 'function)
			  (cleavir-env:expander info)
			  form
			  env)
		 env)
	;; There is a compiler macro, so we must see whether it will
	;; accept or decline.
	(let ((expanded-form (funcall (coerce *macroexpand-hook* 'function)
				      compiler-macro
				      form
				      env)))
	  (if (eq form expanded-form)
	      ;; If the two are EQ, this means that the compiler macro
	      ;; declined.  Then we appply the macro function, and
	      ;; then convert the resulting form, just like we did
	      ;; when there was no compiler macro present.
	      (convert (funcall (coerce *macroexpand-hook* 'function)
				(cleavir-env:expander info)
				expanded-form
				env)
		       env)
	      ;; If the two are not EQ, this means that the compiler
	      ;; macro replaced the original form with a new form.
	      ;; This new form must then again be converted without
	      ;; taking into account the real macro expander.
	      (convert expanded-form env))))))

(defmethod convert-form (form (info cleavir-env:global-function-info) env)
  (let ((compiler-macro (cleavir-env:compiler-macro info)))
    (if (null compiler-macro)
	;; There is no compiler macro.  Create a CALL-AST.
	(let ((function-ast (find-or-create-ast (cleavir-env:identity info)))
	      (argument-asts (convert-sequence (cdr form) env)))
	  (cleavir-ast:make-call-ast function-ast argument-asts))
	;; There is a compiler macro.  We must see whether it will
	;; accept or decline.
	(let ((expanded-form (funcall (coerce *macroexpand-hook* 'function)
				      compiler-macro
				      form
				      env)))
	  (if (eq form expanded-form)
	      ;; If the two are EQ, this means that the compiler macro
	      ;; declined.  We are left with function-call form.
	      ;; Create a CALL-AST, just as if there were no compiler
	      ;; macro present.
	      (let ((function-ast (find-or-create-ast (cleavir-env:identity info)))
		    (argument-asts (convert-sequence (cdr form) env)))
		(cleavir-ast:make-call-ast function-ast argument-asts))
	      ;; If the two are not EQ, this means that the compiler
	      ;; macro replaced the original form with a new form.
	      ;; This new form must then be converted.
	      (convert expanded-form env))))))
