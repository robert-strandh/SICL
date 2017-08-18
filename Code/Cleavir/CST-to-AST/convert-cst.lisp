(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-cst
    (cst (info cleavir-env:special-operator-info) env system)
  (convert-special (car (cst:raw cst)) cst env system))

(defmethod convert-cst
    (cst (info cleavir-env:global-function-info) env system)
  ;; When we compile a call to a global function, it is possible that
  ;; we are in COMPILE-TIME-TOO mode.  In that case, we must first
  ;; evaluate the form.
  (when (and *current-form-is-top-level-p* *compile-time-too*)
    (cleavir-env:eval (cst:raw cst) env env))
  (let ((compiler-macro (cleavir-env:compiler-macro info))
	(notinline (eq 'notinline (cleavir-env:inline info)))
        (form (cst:raw cst)))
    (if (or notinline (null compiler-macro))
	;; There is no compiler macro.  Create the call.
	(make-call cst info env (cst:rest cst) system)
	;; There is a compiler macro.  We must see whether it will
	;; accept or decline.
	(let ((expanded-form (funcall (coerce *macroexpand-hook* 'function)
				      compiler-macro
				      form
				      env)))
	  (if (eq form expanded-form)
	      ;; If the two are EQ, this means that the compiler macro
	      ;; declined.  We are left with function-call form.
	      ;; Create the call, just as if there were no compiler
	      ;; macro present.
	      (make-call form info env (cdr form) system)
	      ;; If the two are not EQ, this means that the compiler
	      ;; macro replaced the original form with a new form.
	      ;; This new form must then be converted.
	      (convert expanded-form env system))))))
