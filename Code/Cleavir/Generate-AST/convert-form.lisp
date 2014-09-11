(cl:in-package #:cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a symbol macro.

(defmethod convert-form (form (info cleavir-env:symbol-macro-info) env)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (lambda (form env)
			      (declare (ignore form env))
			      (cleavir-env:expansion info))
			    form
			    env)))
    (convert expansion env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a constant variable.

(defmethod convert-form (form (info cleavir-env:constant-variable-info) env)
  (cleavir-ast:make-constant-ast (cleavir-env:value info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a lexical variable.

(defmethod convert-form (form (info cleavir-env:lexical-variable-info) env)
  (when (eq (cleavir-env:ignore info) 'ignore)
    (warn "Reference to a variable declared IGNORE"))
  (find-or-create-ast (cleavir-env:identity info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a special variable.
;;; We do this by generating a call to SYMBOL-VALUE.

(defmethod convert-form (form (info cleavir-env:special-variable-info) env)
  (convert `(symbol-value ',(cleavir-env:name info)) env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a local macro.
;;; A local macro can not have a compiler macro associated with it.

(defmethod convert-form (form (info cleavir-env:local-macro-info) env)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (cleavir-env:expander info)
			    form
			    env)))
    (convert expansion env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a global macro.
;;; A global macro can have a compiler macro associated with it.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a local function.
;;; A local function can not have a compiler macro associated with it.

(defmethod convert-form (form (info cleavir-env:local-function-info) env)
  (let ((function-ast (find-or-create-ast (cleavir-env:name info)))
	(argument-asts (convert-sequence (cdr form) env)))
    (cleavir-ast:make-call-ast function-ast argument-asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a global function.
;;; A global function can have  compiler macro associated with it.

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
	      (let ((function-ast (find-or-create-ast (cleavir-env:name info)))
		    (argument-asts (convert-sequence (cdr form) env)))
		(cleavir-ast:make-call-ast function-ast argument-asts))
	      ;; If the two are not EQ, this means that the compiler
	      ;; macro replaced the original form with a new form.
	      ;; This new form must then be converted.
	      (convert expanded-form env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a special form.

(defgeneric convert-special (head form environment))

(defmethod convert-form (form (info cleavir-env:special-operator-info) env)
  (convert-special (car form) form env))
