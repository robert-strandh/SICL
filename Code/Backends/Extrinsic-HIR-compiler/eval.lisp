(cl:in-package #:sicl-extrinsic-hir-compiler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.

(defparameter *fdefinition*
  (lambda (name) (sicl-env:fdefinition name *environment*)))

(defun eval (form)
  (let* ((cleavir-generate-ast:*compiler* 'cl:eval)
	 (ast (cleavir-generate-ast:generate-ast form *environment*))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    (funcall
     (funcall 
      (compile
       nil
       `(lambda ()
	  (let ((fdefinition *fdefinition*))
	    (declare (ignorable fdefinition))
	    ,(translate hir *environment*))))))))

(defmethod cleavir-env:eval (form environment1 (environment2 environment))
  (let* ((cleavir-generate-ast:*compiler* 'cl:eval)
	 (ast (cleavir-generate-ast:generate-ast form environment1))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    (funcall
     (funcall
      (compile
       nil
       `(lambda ()
	  (let ((fdefinition *fdefinition*))
	    (declare (ignorable fdefinition))
	    ,(translate hir environment2))))))))
