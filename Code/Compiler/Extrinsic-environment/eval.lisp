(cl:in-package #:sicl-extrinsic-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.

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
	(t
	 (let* ((cleavir-generate-ast:*compiler* 'cl:eval)
		(ast (cleavir-generate-ast:generate-ast form environment1))
		(ast-bis (cleavir-ast-transformations:hoist-load-time-value ast))
		(hir (cleavir-ast-to-hir:compile-toplevel ast-bis))
		(lambda-expr (translate hir environment2))
		(fun (compile nil lambda-expr))
		(args (loop for arg in (cleavir-ir:forms hir)
			    collect (cleavir-env:eval
				     arg environment1 environment2))))
	   (apply fun args)))))
