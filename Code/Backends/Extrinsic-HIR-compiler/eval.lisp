(cl:in-package #:sicl-extrinsic-hir-compiler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.

(defun eval (form)
  (let* ((cleavir-generate-ast:*compiler* 'cl:eval)
	 (ast (cleavir-generate-ast:generate-ast form *environment*))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast))
	 (lambda-expr `(lambda ()
			 ,(translate hir *environment*)))
	 (fun1 (compile nil lambda-expr))
	 (fun2 (funcall fun1)))
    (funcall fun2)))

(defmethod cleavir-env:eval (form environment1 (environment2 environment))
  (let* ((cleavir-generate-ast:*compiler* 'cl:eval)
	 (ast (cleavir-generate-ast:generate-ast form environment1))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    (funcall
     (funcall
      (compile
       nil
       `(lambda ()
	  ,(translate hir environment2)))))))
